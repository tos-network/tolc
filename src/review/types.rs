use super::{ReviewError, ReviewResult};
use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub(crate) struct MemberTables {
    pub methods_arities: HashMap<String, Vec<usize>>,
    pub methods_varargs_min: HashMap<String, usize>,
    pub methods_signatures: HashMap<String, Vec<Vec<String>>>,
    pub methods_static: HashMap<String, bool>,
    pub fields_static: HashMap<String, bool>,
    pub fields_final: HashMap<String, bool>,
    pub ctors_arities: HashMap<String, Vec<usize>>, // keyed by type name (self)
    pub ctors_varargs_min: HashMap<String, usize>,
    pub ctors_signatures: HashMap<String, Vec<Vec<String>>>,
    pub type_param_count: usize,
    pub type_param_bounds: Vec<Vec<String>>, // simple names of bounds per type parameter
    pub super_name: Option<String>,
    pub interfaces: Vec<String>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct GlobalMemberIndex {
    pub by_type: HashMap<String, MemberTables>,
    pub imports: Vec<String>,
    pub package: Option<String>,
    pub wildcard_imports: Vec<String>,
    // Static imports
    pub static_explicit: HashMap<String, String>, // member name -> type name
    pub static_wildcard: Vec<String>,             // type names providing *
    pub enum_index: HashMap<String, HashMap<String, usize>>, // enum -> const -> ordinal
}

fn build_global_member_index(ast: &Ast) -> GlobalMemberIndex {
    let mut idx = GlobalMemberIndex { by_type: HashMap::new(), imports: Vec::new(), package: None, wildcard_imports: Vec::new(), static_explicit: HashMap::new(), static_wildcard: Vec::new(), enum_index: HashMap::new() };
    // record package
    idx.package = ast.package_decl.as_ref().map(|p| p.name.clone());
    // record simple imports
    for imp in &ast.imports {
        if imp.is_static {
            if imp.is_wildcard {
                // import static pkg.Type.*;
                idx.static_wildcard.push(imp.name.clone());
            } else {
                // import static pkg.Type.member;
                if let Some(dot) = imp.name.rfind('.') {
                    let type_name = imp.name[..dot].to_string();
                    let member = imp.name[dot + 1..].to_string();
                    idx.static_explicit.insert(member, type_name);
                }
            }
        } else if imp.is_wildcard {
            idx.wildcard_imports.push(imp.name.clone());
        } else {
            idx.imports.push(imp.name.clone());
        }
    }
    for td in &ast.type_decls {
        if let TypeDecl::Class(c) = td {
            let mut mt = MemberTables::default();
            mt.type_param_count = c.type_params.len();
            // record simple upper bounds for generic parameters
            if c.type_params.is_empty() {
                mt.type_param_bounds = Vec::new();
            } else {
                mt.type_param_bounds = c.type_params.iter().map(|tp| {
                    tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>()
                }).collect();
            }
            for member in &c.body {
                match member {
                    ClassMember::Field(f) => {
                        let is_static = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                        mt.fields_static.entry(f.name.clone()).or_insert(is_static);
                        let is_final = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Final));
                        mt.fields_final.entry(f.name.clone()).or_insert(is_final);
                    }
                    ClassMember::Method(m) => {
                        let arity = m.parameters.len();
                        mt.methods_arities.entry(m.name.clone()).or_default().push(arity);
                        if m.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                            mt.methods_varargs_min
                                .entry(m.name.clone())
                                .and_modify(|min| { *min = (*min).min(arity - 1); })
                                .or_insert(arity - 1);
                        }
                        let sig: Vec<String> = m.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
                        mt.methods_signatures.entry(m.name.clone()).or_default().push(sig);
                        let is_static = m.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                        mt.methods_static.entry(m.name.clone()).or_insert(is_static);
                    }
                    ClassMember::Constructor(cons) => {
                        let arity = cons.parameters.len();
                        mt.ctors_arities.entry(c.name.clone()).or_default().push(arity);
                        if cons.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                            mt.ctors_varargs_min
                                .entry(c.name.clone())
                                .and_modify(|min| { *min = (*min).min(arity - 1); })
                                .or_insert(arity - 1);
                        }
                        let sig: Vec<String> = cons.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
                        mt.ctors_signatures.entry(c.name.clone()).or_default().push(sig);
                    }
                    _ => {}
                }
            }
            // de-dup arities
            for v in mt.methods_arities.values_mut() { v.sort_unstable(); v.dedup(); }
            for v in mt.ctors_arities.values_mut() { v.sort_unstable(); v.dedup(); }
            // record immediate superclass simple name if present
            if let Some(ext) = &c.extends { mt.super_name = Some(ext.name.clone()); } else { mt.super_name = None; }
            // record implemented interfaces
            mt.interfaces = c.implements.iter().map(|t| t.name.clone()).collect();
            // insert by simple name
            idx.by_type.insert(c.name.clone(), mt.clone());
            // and fully qualified if package exists
            if let Some(pkg) = &idx.package {
                let fq = format!("{}.{}", pkg, c.name);
                idx.by_type.insert(fq, mt);
            } else {
                // also echo simple for completeness
                idx.by_type.insert(c.name.clone(), mt);
            }
        } else if let TypeDecl::Interface(i) = td {
            let mut mt = MemberTables::default();
            mt.type_param_count = i.type_params.len();
            mt.interfaces = i.extends.iter().map(|t| t.name.clone()).collect();
            idx.by_type.insert(i.name.clone(), mt.clone());
            if let Some(pkg) = &idx.package {
                let fq = format!("{}.{}", pkg, i.name);
                idx.by_type.insert(fq, mt);
            } else {
                idx.by_type.insert(i.name.clone(), mt);
            }
        } else if let TypeDecl::Enum(e) = td {
            // record enum constants ordinals
            let mut inner: HashMap<String, usize> = HashMap::new();
            for (i, c) in e.constants.iter().enumerate() { inner.insert(c.name.clone(), i); }
            idx.enum_index.insert(e.name.clone(), inner);
        }
    }
    idx
}

pub(crate) fn review_types(ast: &Ast) -> ReviewResult<()> {
    use std::collections::HashSet;
    let global_index = build_global_member_index(ast);
    let mut seen: HashSet<String> = HashSet::new();
    for td in &ast.type_decls {
        match td {
            TypeDecl::Class(c) => {
                if c.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(c.name.clone()) { return Err(ReviewError::DuplicateType(c.name.clone())); }
                super::class::review_class(c)?;
                super::fields::review_fields_of_class(c, &global_index)?;
                super::methods::review_methods_of_class(c, &global_index)?;
            }
            TypeDecl::Interface(i) => {
                if i.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(i.name.clone()) { return Err(ReviewError::DuplicateType(i.name.clone())); }
                super::interface::review_interface(i)?;
                super::fields::review_fields_of_interface(i, &global_index)?;
                super::methods::review_methods_of_interface(i)?;
            }
            TypeDecl::Enum(e) => {
                if e.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(e.name.clone()) { return Err(ReviewError::DuplicateType(e.name.clone())); }
                super::enums::review_enum(e)?;
            }
            TypeDecl::Annotation(a) => {
                if a.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(a.name.clone()) { return Err(ReviewError::DuplicateType(a.name.clone())); }
                super::annotation::review_annotation(a)?;
            }
        }
    }
    Ok(())
}

