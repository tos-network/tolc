use super::{ReviewError, ReviewResult};
use crate::ast::*;
use std::collections::HashMap;
use std::fs;
use walkdir::WalkDir;
use once_cell::sync::OnceCell;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Visibility { Private, Package, Protected, Public }

#[derive(Debug, Clone)]
pub(crate) struct MethodMeta {
    pub signature: Vec<String>,
    pub visibility: Visibility,
    pub is_static: bool,
    pub is_final: bool,
    pub is_abstract: bool,
    pub has_body: bool,
    pub return_type: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct MemberTables {
    pub methods_arities: HashMap<String, Vec<usize>>,
    pub methods_varargs_min: HashMap<String, usize>,
    pub methods_signatures: HashMap<String, Vec<Vec<String>>>,
    pub methods_static: HashMap<String, bool>,
    pub methods_throws: HashMap<String, Vec<(usize, Vec<String>)>>, // name -> list of (arity, throws)
    pub methods_meta: HashMap<String, Vec<MethodMeta>>, // name -> overload metas
    pub methods_throws_by_sig: HashMap<String, Vec<(Vec<String>, Vec<String>)>>, // name -> list of (param types, throws)
    pub fields_static: HashMap<String, bool>,
    pub fields_final: HashMap<String, bool>,
    pub fields_visibility: HashMap<String, Visibility>,
    pub ctors_arities: HashMap<String, Vec<usize>>, // keyed by type name (self)
    pub ctors_varargs_min: HashMap<String, usize>,
    pub ctors_signatures: HashMap<String, Vec<Vec<String>>>,
    pub ctors_throws: HashMap<String, Vec<(usize, Vec<String>)>>, // keyed by type name (self)
    pub ctors_throws_by_sig: HashMap<String, Vec<(Vec<String>, Vec<String>)>>, // keyed by type name (self)
    pub type_param_count: usize,
    pub type_param_bounds: Vec<Vec<String>>, // simple names of bounds per type parameter
    pub type_param_names: Vec<String>, // parallel to type_param_bounds
    pub type_param_name_to_bounds: HashMap<String, Vec<String>>, // convenience lookup
    pub super_name: Option<String>,
    pub interfaces: Vec<String>,
    pub package_name: Option<String>,
    pub is_interface: bool,
}
pub(crate) fn visibility_of(mods: &[Modifier]) -> Visibility {
    if mods.iter().any(|m| matches!(m, Modifier::Public)) { return Visibility::Public; }
    if mods.iter().any(|m| matches!(m, Modifier::Protected)) { return Visibility::Protected; }
    if mods.iter().any(|m| matches!(m, Modifier::Private)) { return Visibility::Private; }
    Visibility::Package
}

#[inline]
pub(crate) fn type_ref_signature_name(t: &TypeRef) -> String {
    let mut name = t.name.clone();
    for _ in 0..t.array_dims { name.push_str("[]"); }
    name
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

// Global cache for classpath index to avoid rebuilding per Java file
static CLASSPATH_INDEX_CACHE: OnceCell<(String, Arc<GlobalMemberIndex>)> = OnceCell::new();

fn get_or_build_classpath_index(current_ast: &Ast, classpath_dir: &str) -> Arc<GlobalMemberIndex> {
    if let Some((cached_cp, cached)) = CLASSPATH_INDEX_CACHE.get() {
        if cached_cp == classpath_dir {
            return Arc::clone(cached);
        }
    }
    let built = build_global_member_index_with_classpath(current_ast, classpath_dir);
    let arc = Arc::new(built);
    let _ = CLASSPATH_INDEX_CACHE.set((classpath_dir.to_string(), Arc::clone(&arc)));
    arc
}

pub(crate) fn build_global_member_index(ast: &Ast) -> GlobalMemberIndex {
    let mut idx = GlobalMemberIndex { by_type: HashMap::new(), imports: Vec::new(), package: None, wildcard_imports: Vec::new(), static_explicit: HashMap::new(), static_wildcard: Vec::new(), enum_index: HashMap::new() };
    // record package
    idx.package = ast.package_decl.as_ref().map(|p| p.name.clone());
    super::debug_log(format!("index package: {:?}", idx.package));
    // record simple imports
    for imp in &ast.imports {
        if imp.is_static {
            if imp.is_wildcard {
                // import static pkg.Type.*;
                idx.static_wildcard.push(imp.name.clone());
                super::debug_log(format!("import static *: {}", imp.name));
            } else {
                // import static pkg.Type.member;
                if let Some(dot) = imp.name.rfind('.') {
                    let type_name = imp.name[..dot].to_string();
                    let member = imp.name[dot + 1..].to_string();
                    idx.static_explicit.insert(member, type_name);
                    super::debug_log(format!("import static member: {}", imp.name));
                }
            }
        } else if imp.is_wildcard {
            idx.wildcard_imports.push(imp.name.clone());
            super::debug_log(format!("import *: {}", imp.name));
        } else {
            idx.imports.push(imp.name.clone());
            super::debug_log(format!("import: {}", imp.name));
        }
    }
    for td in &ast.type_decls {
        if let TypeDecl::Class(c) = td {
            let mut mt = MemberTables::default();
            mt.type_param_count = c.type_params.len();
            // record simple upper bounds for generic parameters
            if c.type_params.is_empty() {
                mt.type_param_bounds = Vec::new();
                mt.type_param_names = Vec::new();
                mt.type_param_name_to_bounds = HashMap::new();
            } else {
                mt.type_param_bounds = c.type_params.iter().map(|tp| {
                    tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>()
                }).collect();
                mt.type_param_names = c.type_params.iter().map(|tp| tp.name.clone()).collect();
                mt.type_param_name_to_bounds = c.type_params.iter().map(|tp| {
                    let v = tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>();
                    (tp.name.clone(), v)
                }).collect();
            }
            for member in &c.body {
                match member {
                    ClassMember::Field(f) => {
                        let is_static = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                        mt.fields_static.entry(f.name.clone()).or_insert(is_static);
                        let is_final = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Final));
                        mt.fields_final.entry(f.name.clone()).or_insert(is_final);
                        mt.fields_visibility.entry(f.name.clone()).or_insert(visibility_of(&f.modifiers));
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
                        let sig: Vec<String> = m.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                        mt.methods_signatures.entry(m.name.clone()).or_default().push(sig);
                        let is_static = m.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                        mt.methods_static.entry(m.name.clone()).or_insert(is_static);
                        // Map generic type-variable throws to erased upper bounds (best-effort)
                        let throws: Vec<String> = m.throws.iter().map(|t| {
                            let tn = &t.name;
                            // If resolves as a known type, keep as is
                            if idx.by_type.contains_key(tn) { return tn.clone(); }
                            // Method type parameter match
                            if let Some(tp) = m.type_params.iter().find(|tp| &tp.name == tn) {
                                if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                // No bound: default to Object
                                return "Object".to_string();
                            }
                            // Class type parameter match
                            if let Some(tp) = c.type_params.iter().find(|tp| &tp.name == tn) {
                                if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                return "Object".to_string();
                            }
                            // Unknown simple name: keep as is (compat)
                            tn.clone()
                        }).collect();
                        mt.methods_throws.entry(m.name.clone()).or_default().push((arity, throws));
                        let sig_keys: Vec<String> = m.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                        let throws_vec: Vec<String> = m.throws.iter().map(|t| {
                            let tn = &t.name;
                            if idx.by_type.contains_key(tn) { return tn.clone(); }
                            if let Some(tp) = m.type_params.iter().find(|tp| &tp.name == tn) {
                                if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                return "Object".to_string();
                            }
                            if let Some(tp) = c.type_params.iter().find(|tp| &tp.name == tn) {
                                if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                return "Object".to_string();
                            }
                            tn.clone()
                        }).collect();
                        mt.methods_throws_by_sig.entry(m.name.clone()).or_default().push((sig_keys.clone(), throws_vec));
                        let meta = MethodMeta {
                            signature: sig_keys,
                            visibility: visibility_of(&m.modifiers),
                            is_static,
                            is_final: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Final)),
                            is_abstract: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Abstract)) || m.body.is_none(),
                            has_body: m.body.is_some(),
                            return_type: m.return_type.as_ref().map(|t| t.name.clone()),
                        };
                        mt.methods_meta.entry(m.name.clone()).or_default().push(meta);
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
                        let sig: Vec<String> = cons.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                        mt.ctors_signatures.entry(c.name.clone()).or_default().push(sig);
                        let throws: Vec<String> = cons.throws.iter().map(|t| t.name.clone()).collect();
                        mt.ctors_throws.entry(c.name.clone()).or_default().push((arity, throws));
                        let ct_sig: Vec<String> = cons.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                        let ct_thr: Vec<String> = cons.throws.iter().map(|t| t.name.clone()).collect();
                        mt.ctors_throws_by_sig.entry(c.name.clone()).or_default().push((ct_sig, ct_thr));
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
            // record package name
            mt.package_name = idx.package.clone();
            mt.is_interface = false;
            // insert by simple name
            idx.by_type.insert(c.name.clone(), mt.clone());
            super::debug_log(format!("index class: {}", c.name));
            // and fully qualified if package exists
            if let Some(pkg) = &idx.package {
                let fq = format!("{}.{}", pkg, c.name);
                idx.by_type.insert(fq.clone(), mt);
                super::debug_log(format!("index class fq: {}", fq));
            } else {
                // also echo simple for completeness
                idx.by_type.insert(c.name.clone(), mt);
            }
            // Index nested types within this class by simple name for local resolution
            fn index_nested_in_class(idx: &mut GlobalMemberIndex, outer_pkg: &Option<String>, c: &ClassDecl) {
                for member in &c.body {
                    if let ClassMember::TypeDecl(TypeDecl::Class(nc)) = member {
                        let mut nmt = MemberTables::default();
                        nmt.type_param_count = nc.type_params.len();
                        nmt.package_name = outer_pkg.clone();
                        nmt.interfaces = nc.implements.iter().map(|t| t.name.clone()).collect();
                        // collect members
                        for m in &nc.body {
                            match m {
                                ClassMember::Field(f) => {
                                    let is_static = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                                    nmt.fields_static.entry(f.name.clone()).or_insert(is_static);
                                    let is_final = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Final));
                                    nmt.fields_final.entry(f.name.clone()).or_insert(is_final);
                                    nmt.fields_visibility.entry(f.name.clone()).or_insert(visibility_of(&f.modifiers));
                                }
                                ClassMember::Method(m2) => {
                                    let arity = m2.parameters.len();
                                    nmt.methods_arities.entry(m2.name.clone()).or_default().push(arity);
                                    if m2.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                                        nmt.methods_varargs_min
                                            .entry(m2.name.clone())
                                            .and_modify(|min| { *min = (*min).min(arity - 1); })
                                            .or_insert(arity - 1);
                                    }
                                    let sig: Vec<String> = m2.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                                    nmt.methods_signatures.entry(m2.name.clone()).or_default().push(sig);
                                    let is_static = m2.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                                    nmt.methods_static.entry(m2.name.clone()).or_insert(is_static);
                                }
                                ClassMember::Constructor(cons) => {
                                    let arity = cons.parameters.len();
                                    nmt.ctors_arities.entry(nc.name.clone()).or_default().push(arity);
                                    if cons.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                                        nmt.ctors_varargs_min
                                            .entry(nc.name.clone())
                                            .and_modify(|min| { *min = (*min).min(arity - 1); })
                                            .or_insert(arity - 1);
                                    }
                                    let sig: Vec<String> = cons.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                                    nmt.ctors_signatures.entry(nc.name.clone()).or_default().push(sig);
                                }
                                _ => {}
                            }
                        }
                        for v in nmt.methods_arities.values_mut() { v.sort_unstable(); v.dedup(); }
                        for v in nmt.ctors_arities.values_mut() { v.sort_unstable(); v.dedup(); }
                        idx.by_type.insert(nc.name.clone(), nmt.clone());
                        // recurse
                        index_nested_in_class(idx, outer_pkg, nc);
                    }
                }
            }
            let pkg_clone = idx.package.clone();
            index_nested_in_class(&mut idx, &pkg_clone, c);
        } else if let TypeDecl::Interface(i) = td {
            let mut mt = MemberTables::default();
            mt.type_param_count = i.type_params.len();
            mt.package_name = idx.package.clone();
            mt.interfaces = i.extends.iter().map(|t| t.name.clone()).collect();
            mt.is_interface = true;
            // interface type parameters (for bounds checks on type usage)
            if i.type_params.is_empty() {
                mt.type_param_bounds = Vec::new();
                mt.type_param_names = Vec::new();
                mt.type_param_name_to_bounds = HashMap::new();
            } else {
                mt.type_param_bounds = i.type_params.iter().map(|tp| {
                    tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>()
                }).collect();
                mt.type_param_names = i.type_params.iter().map(|tp| tp.name.clone()).collect();
                mt.type_param_name_to_bounds = i.type_params.iter().map(|tp| {
                    let v = tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>();
                    (tp.name.clone(), v)
                }).collect();
            }
            // record interface methods metas and throws by signature
            for member in &i.body {
                if let InterfaceMember::Method(m) = member {
                    // also populate arities/signatures/varargs/static for overload resolution
                    let arity = m.parameters.len();
                    mt.methods_arities.entry(m.name.clone()).or_default().push(arity);
                    if m.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                        mt.methods_varargs_min
                            .entry(m.name.clone())
                            .and_modify(|min| { *min = (*min).min(arity - 1); })
                            .or_insert(arity - 1);
                    }
                    let sig_full: Vec<String> = m.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                    mt.methods_signatures.entry(m.name.clone()).or_default().push(sig_full);
                    let is_static = m.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                    mt.methods_static.entry(m.name.clone()).or_insert(is_static);
                    let sig: Vec<String> = m.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                    // Map generic type-variable throws to erased upper bounds using method or interface bounds
                    let throws_vec: Vec<String> = m.throws.iter().map(|t| {
                        let tn = &t.name;
                        if idx.by_type.contains_key(tn) { return tn.clone(); }
                        if let Some(tp) = m.type_params.iter().find(|tp| &tp.name == tn) {
                            if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                            return "Object".to_string();
                        }
                        if let Some(tp) = i.type_params.iter().find(|tp| &tp.name == tn) {
                            if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                            return "Object".to_string();
                        }
                        tn.clone()
                    }).collect();
                    let meta = MethodMeta {
                        signature: sig.clone(),
                        visibility: visibility_of(&m.modifiers),
                        is_static: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Static)),
                        is_final: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Final)),
                        is_abstract: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Abstract)) || m.body.is_none(),
                        has_body: m.body.is_some(),
                        return_type: m.return_type.as_ref().map(|t| t.name.clone()),
                    };
                    mt.methods_meta.entry(m.name.clone()).or_default().push(meta);
                    mt.methods_throws_by_sig.entry(m.name.clone()).or_default().push((sig, throws_vec));
                }
            }
            for v in mt.methods_arities.values_mut() { v.sort_unstable(); v.dedup(); }
            idx.by_type.insert(i.name.clone(), mt.clone());
            super::debug_log(format!("index interface: {}", i.name));
            if let Some(pkg) = &idx.package {
                let fq = format!("{}.{}", pkg, i.name);
                idx.by_type.insert(fq.clone(), mt);
                super::debug_log(format!("index interface fq: {}", fq));
            } else {
                idx.by_type.insert(i.name.clone(), mt);
            }
        } else if let TypeDecl::Enum(e) = td {
            // record enum constants ordinals
            let mut inner: HashMap<String, usize> = HashMap::new();
            for (i, c) in e.constants.iter().enumerate() { inner.insert(c.name.clone(), i); }
            idx.enum_index.insert(e.name.clone(), inner);
            super::debug_log(format!("index enum: {}", e.name));
        }
    }
    idx
}

/// Build a classpath-wide index using all types found under `classpath_dir`, while
/// preserving the imports/package of the current compilation unit `current_ast`.
pub(crate) fn build_global_member_index_with_classpath(current_ast: &Ast, classpath_dir: &str) -> GlobalMemberIndex {
    let mut idx = GlobalMemberIndex { by_type: HashMap::new(), imports: Vec::new(), package: None, wildcard_imports: Vec::new(), static_explicit: HashMap::new(), static_wildcard: Vec::new(), enum_index: HashMap::new() };
    // Seed current CU package/imports for resolution rules that depend on them
    idx.package = current_ast.package_decl.as_ref().map(|p| p.name.clone());
    for imp in &current_ast.imports {
        if imp.is_static {
            if imp.is_wildcard {
                idx.static_wildcard.push(imp.name.clone());
            } else if let Some(dot) = imp.name.rfind('.') {
                let type_name = imp.name[..dot].to_string();
                let member = imp.name[dot + 1..].to_string();
                idx.static_explicit.insert(member, type_name);
            }
        } else if imp.is_wildcard {
            idx.wildcard_imports.push(imp.name.clone());
        } else {
            idx.imports.push(imp.name.clone());
        }
    }
    // Helper to index one AST's types using its own package
    fn index_types_from_ast_into(idx: &mut GlobalMemberIndex, ast: &Ast) {
        let local_pkg = ast.package_decl.as_ref().map(|p| p.name.clone());
        for td in &ast.type_decls {
            match td {
                TypeDecl::Class(c) => {
                    let mut mt = MemberTables::default();
                    mt.type_param_count = c.type_params.len();
                    if c.type_params.is_empty() {
                        mt.type_param_bounds = Vec::new();
                        mt.type_param_names = Vec::new();
                        mt.type_param_name_to_bounds = HashMap::new();
                    } else {
                        mt.type_param_bounds = c.type_params.iter().map(|tp| {
                            tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>()
                        }).collect();
                        mt.type_param_names = c.type_params.iter().map(|tp| tp.name.clone()).collect();
                        mt.type_param_name_to_bounds = c.type_params.iter().map(|tp| {
                            let v = tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>();
                            (tp.name.clone(), v)
                        }).collect();
                    }
                    for member in &c.body {
                        match member {
                            ClassMember::Field(f) => {
                        let is_static = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                                mt.fields_static.entry(f.name.clone()).or_insert(is_static);
                                let is_final = f.modifiers.iter().any(|mm| matches!(mm, Modifier::Final));
                                mt.fields_final.entry(f.name.clone()).or_insert(is_final);
                        mt.fields_visibility.entry(f.name.clone()).or_insert(visibility_of(&f.modifiers));
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
                                // Map generic type-variable throws to erased upper bounds (best-effort),
                                // mirroring the logic used in build_global_member_index
                                let throws: Vec<String> = m.throws.iter().map(|t| {
                                    let tn = &t.name;
                                    // If resolves as a known type, keep as is
                                    if idx.by_type.contains_key(tn) { return tn.clone(); }
                                    // Method type parameter match
                                    if let Some(tp) = m.type_params.iter().find(|tp| &tp.name == tn) {
                                        if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                        // No bound: default to Object
                                        return "Object".to_string();
                                    }
                                    // Class type parameter match
                                    if let Some(tp) = c.type_params.iter().find(|tp| &tp.name == tn) {
                                        if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                        return "Object".to_string();
                                    }
                                    // Unknown simple name: keep as is (compat)
                                    tn.clone()
                                }).collect();
                                mt.methods_throws.entry(m.name.clone()).or_default().push((arity, throws));
                                let sig_keys: Vec<String> = m.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
                                let throws_vec: Vec<String> = m.throws.iter().map(|t| {
                                    let tn = &t.name;
                                    if idx.by_type.contains_key(tn) { return tn.clone(); }
                                    if let Some(tp) = m.type_params.iter().find(|tp| &tp.name == tn) {
                                        if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                        return "Object".to_string();
                                    }
                                    if let Some(tp) = c.type_params.iter().find(|tp| &tp.name == tn) {
                                        if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                        return "Object".to_string();
                                    }
                                    tn.clone()
                                }).collect();
                                mt.methods_throws_by_sig.entry(m.name.clone()).or_default().push((sig_keys.clone(), throws_vec));
                                let meta = MethodMeta {
                                    signature: sig_keys,
                                    visibility: visibility_of(&m.modifiers),
                                    is_static,
                                    is_final: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Final)),
                                    is_abstract: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Abstract)) || m.body.is_none(),
                                    has_body: m.body.is_some(),
                                    return_type: m.return_type.as_ref().map(|t| t.name.clone()),
                                };
                                mt.methods_meta.entry(m.name.clone()).or_default().push(meta);
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
                                let throws: Vec<String> = cons.throws.iter().map(|t| t.name.clone()).collect();
                                mt.ctors_throws.entry(c.name.clone()).or_default().push((arity, throws));
                                let ct_sig: Vec<String> = cons.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
                        let ct_thr: Vec<String> = cons.throws.iter().map(|t| {
                            let tn = &t.name;
                            if idx.by_type.contains_key(tn) { return tn.clone(); }
                            if let Some(tp) = c.type_params.iter().find(|tp| &tp.name == tn) {
                                if let Some(b) = tp.bounds.first() { return b.name.clone(); }
                                return "Object".to_string();
                            }
                            tn.clone()
                        }).collect();
                                mt.ctors_throws_by_sig.entry(c.name.clone()).or_default().push((ct_sig, ct_thr));
                            }
                            _ => {}
                        }
                    }
                    for v in mt.methods_arities.values_mut() { v.sort_unstable(); v.dedup(); }
                    for v in mt.ctors_arities.values_mut() { v.sort_unstable(); v.dedup(); }
                    mt.super_name = c.extends.as_ref().map(|t| t.name.clone());
                    mt.interfaces = c.implements.iter().map(|t| t.name.clone()).collect();
                    mt.package_name = local_pkg.clone();
                    mt.is_interface = false;
                    // insert simple and fq
                    idx.by_type.insert(c.name.clone(), mt.clone());
                    if let Some(pkg) = &local_pkg {
                        let fq = format!("{}.{}", pkg, c.name);
                        idx.by_type.insert(fq, mt);
                    } else {
                        idx.by_type.insert(c.name.clone(), mt);
                    }
                }
                TypeDecl::Interface(i) => {
                    let mut mt = MemberTables::default();
                    mt.type_param_count = i.type_params.len();
                    mt.package_name = local_pkg.clone();
                    mt.interfaces = i.extends.iter().map(|t| t.name.clone()).collect();
                    mt.is_interface = true;
                    if i.type_params.is_empty() {
                        mt.type_param_bounds = Vec::new();
                        mt.type_param_names = Vec::new();
                        mt.type_param_name_to_bounds = HashMap::new();
                    } else {
                        mt.type_param_bounds = i.type_params.iter().map(|tp| {
                            tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>()
                        }).collect();
                        mt.type_param_names = i.type_params.iter().map(|tp| tp.name.clone()).collect();
                        mt.type_param_name_to_bounds = i.type_params.iter().map(|tp| {
                            let v = tp.bounds.iter().map(|b| b.name.clone()).collect::<Vec<_>>();
                            (tp.name.clone(), v)
                        }).collect();
                    }
                    for member in &i.body {
                        if let InterfaceMember::Method(m) = member {
                            let arity = m.parameters.len();
                            mt.methods_arities.entry(m.name.clone()).or_default().push(arity);
                            if m.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                                mt.methods_varargs_min
                                    .entry(m.name.clone())
                                    .and_modify(|min| { *min = (*min).min(arity - 1); })
                                    .or_insert(arity - 1);
                            }
                            let sig_full: Vec<String> = m.parameters.iter().map(|p| type_ref_signature_name(&p.type_ref)).collect();
                            mt.methods_signatures.entry(m.name.clone()).or_default().push(sig_full);
                            let is_static = m.modifiers.iter().any(|mm| matches!(mm, Modifier::Static));
                            mt.methods_static.entry(m.name.clone()).or_insert(is_static);
                            let sig: Vec<String> = m.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
                            let throws_vec: Vec<String> = m.throws.iter().map(|t| t.name.clone()).collect();
                            let meta = MethodMeta {
                                signature: sig.clone(),
                                visibility: visibility_of(&m.modifiers),
                                is_static: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Static)),
                                is_final: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Final)),
                                is_abstract: m.modifiers.iter().any(|mm| matches!(mm, Modifier::Abstract)),
                                has_body: m.body.is_some(),
                                return_type: m.return_type.as_ref().map(|t| t.name.clone()),
                            };
                            mt.methods_meta.entry(m.name.clone()).or_default().push(meta);
                            mt.methods_throws_by_sig.entry(m.name.clone()).or_default().push((sig, throws_vec));
                        }
                    }
                    for v in mt.methods_arities.values_mut() { v.sort_unstable(); v.dedup(); }
                    idx.by_type.insert(i.name.clone(), mt.clone());
                    if let Some(pkg) = &local_pkg {
                        let fq = format!("{}.{}", pkg, i.name);
                        idx.by_type.insert(fq, mt);
                    } else {
                        idx.by_type.insert(i.name.clone(), mt);
                    }
                }
                TypeDecl::Enum(e) => {
                    let mut inner: HashMap<String, usize> = HashMap::new();
                    for (i, c) in e.constants.iter().enumerate() { inner.insert(c.name.clone(), i); }
                    idx.enum_index.insert(e.name.clone(), inner);
                }
                _ => {}
            }
        }
    }
    // Bounded scan to avoid hangs on huge classpaths. Defaults:
    // - max files: 500 (override via TOLC_CLASSPATH_MAX_FILES)
    // - max file size: 256 KiB (override via TOLC_CLASSPATH_MAX_FILE_SIZE)
    let max_files: usize = std::env::var("TOLC_CLASSPATH_MAX_FILES").ok().and_then(|s| s.parse().ok()).unwrap_or(500);
    let max_size: u64 = std::env::var("TOLC_CLASSPATH_MAX_FILE_SIZE").ok().and_then(|s| s.parse().ok()).unwrap_or(256 * 1024);
    let mut processed: usize = 0;
    for entry in WalkDir::new(classpath_dir).into_iter().filter_map(Result::ok) {
        if processed >= max_files { break; }
        let path = entry.path();
        if entry.file_type().is_file() && path.extension().map(|e| e == "java").unwrap_or(false) {
            // Skip very large files
            if let Ok(meta) = std::fs::metadata(path) {
                if meta.len() > max_size { continue; }
            }
            if let Ok(source) = fs::read_to_string(path) {
                // Additional size check in case metadata is unavailable
                if source.len() as u64 > max_size { continue; }
                // Use lenient parse for classpath scan to avoid stalls on unsupported syntax
                if let Ok(ast) = crate::parser::parse_tol_lenient(&source) {
                    super::debug_log(format!("[classpath] indexed {}", path.display()));
                    index_types_from_ast_into(&mut idx, &ast);
                }
                processed += 1;
            }
        }
    }
    idx
}

pub(crate) fn resolve_type_in_index<'a>(global: &'a GlobalMemberIndex, name: &str) -> Option<&'a MemberTables> {
    if let Some(mt) = global.by_type.get(name) { return Some(mt); }
    for imp in &global.imports {
        if let Some(last) = imp.rsplit('.').next() {
            if last == name { if let Some(mt) = global.by_type.get(imp) { return Some(mt); } }
        }
    }
    for wi in &global.wildcard_imports {
        let fq = format!("{}.{name}", wi);
        if let Some(mt) = global.by_type.get(&fq) { return Some(mt); }
    }
    None
}

pub(crate) fn review_types(ast: &Ast) -> ReviewResult<()> {
    use std::collections::HashSet;
    let global_index = if let Ok(cp) = std::env::var("TOLC_CLASSPATH") {
        // Use cached classpath-wide index for performance
        get_or_build_classpath_index(ast, &cp)
    } else {
        Arc::new(build_global_member_index(ast))
    };
    // Recursively review nested type declarations within a class/interface body
    fn review_nested_in_class(c: &ClassDecl, global_index: &GlobalMemberIndex) -> ReviewResult<()> {
        for member in &c.body {
            if let ClassMember::TypeDecl(td) = member {
                match td {
                    TypeDecl::Class(nc) => {
                        super::class::review_class(nc)?;
                        super::fields::review_fields_of_class(nc, global_index)?;
                        super::methods::review_methods_of_class(nc, global_index)?;
                        review_nested_in_class(nc, global_index)?;
                    }
                    TypeDecl::Interface(ni) => {
                        super::interface::review_interface(ni)?;
                        super::fields::review_fields_of_interface(ni, global_index)?;
                        super::methods::review_methods_of_interface(ni)?;
                        review_nested_in_interface(ni, global_index)?;
                    }
                    TypeDecl::Enum(ne) => {
                        super::enums::review_enum(ne)?;
                    }
                    TypeDecl::Annotation(na) => {
                        super::annotation::review_annotation(na)?;
                    }
                }
            }
        }
        Ok(())
    }
    fn review_nested_in_interface(i: &InterfaceDecl, global_index: &GlobalMemberIndex) -> ReviewResult<()> {
        for member in &i.body {
            if let InterfaceMember::TypeDecl(td) = member {
                match td {
                    TypeDecl::Class(nc) => {
                        super::class::review_class(nc)?;
                        super::fields::review_fields_of_class(nc, global_index)?;
                        super::methods::review_methods_of_class(nc, global_index)?;
                        review_nested_in_class(nc, global_index)?;
                    }
                    TypeDecl::Interface(ni) => {
                        super::interface::review_interface(ni)?;
                        super::fields::review_fields_of_interface(ni, global_index)?;
                        super::methods::review_methods_of_interface(ni)?;
                        review_nested_in_interface(ni, global_index)?;
                    }
                    TypeDecl::Enum(ne) => {
                        super::enums::review_enum(ne)?;
                    }
                    TypeDecl::Annotation(na) => {
                        super::annotation::review_annotation(na)?;
                    }
                }
            }
        }
        Ok(())
    }
    let mut seen: HashSet<String> = HashSet::new();
    for td in &ast.type_decls {
        match td {
            TypeDecl::Class(c) => {
                if c.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(c.name.clone()) { return Err(ReviewError::DuplicateType(c.name.clone())); }
                super::class::review_class(c)?;
                super::fields::review_fields_of_class(c, &global_index)?;
                super::methods::review_methods_of_class(c, &global_index)?;
                review_nested_in_class(c, &global_index)?;
            }
            TypeDecl::Interface(i) => {
                if i.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(i.name.clone()) { return Err(ReviewError::DuplicateType(i.name.clone())); }
                super::interface::review_interface(i)?;
                super::fields::review_fields_of_interface(i, &global_index)?;
                super::methods::review_methods_of_interface(i)?;
                review_nested_in_interface(i, &global_index)?;
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

