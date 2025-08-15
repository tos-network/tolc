//! Stage A scaffolding for generics analysis (interfaces only, no implementation)

use crate::review::types::GlobalMemberIndex;
use crate::ast::{ClassDecl, MethodDecl, TypeRef};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReviewedType {
    Primitive(&'static str),
    Class { name: String, args: Vec<ReviewedType> },
    Array { of: Box<ReviewedType> },
    TypeVar { name: String, upper: Vec<ReviewedType>, lower: Option<Box<ReviewedType>> },
    Wildcard { extends: Option<Box<ReviewedType>>, super_: Option<Box<ReviewedType>> },
    Null,
    Object,
}

#[derive(Clone, Debug, Default)]
pub struct TypeEnv {
    pub class_tparams: std::collections::HashMap<String, (Vec<ReviewedType>, Option<ReviewedType>)>,
    pub method_tparams: std::collections::HashMap<String, (Vec<ReviewedType>, Option<ReviewedType>)>,
}

pub fn resolve_type_ref(tref: &TypeRef, env: &TypeEnv, _index: &GlobalMemberIndex) -> ReviewedType {
    // Arrays
    if tref.array_dims > 0 {
        return ReviewedType::Array { of: Box::new(resolve_type_ref(&TypeRef { name: tref.name.clone(), type_args: tref.type_args.clone(), annotations: Vec::new(), array_dims: tref.array_dims.saturating_sub(1), span: tref.span }, env, _index)) };
    }
    // Primitive
    match tref.name.as_str() {
        "boolean"|"char"|"byte"|"short"|"int"|"long"|"float"|"double" => {
            return ReviewedType::Primitive(Box::leak(tref.name.clone().into_boxed_str()))
        }
        _ => {}
    }
    // TypeArg conversion
    let mut args: Vec<ReviewedType> = Vec::new();
    for ta in &tref.type_args {
        match ta {
            crate::ast::TypeArg::Type(inner) => args.push(resolve_type_ref(inner, env, _index)),
            crate::ast::TypeArg::Wildcard(w) => {
                let ext = w.bound.as_ref().and_then(|(k, b)| if matches!(k, crate::ast::BoundKind::Extends) { Some(b) } else { None })
                    .map(|b| Box::new(resolve_type_ref(b, env, _index)));
                let sup = w.bound.as_ref().and_then(|(k, b)| if matches!(k, crate::ast::BoundKind::Super) { Some(b) } else { None })
                    .map(|b| Box::new(resolve_type_ref(b, env, _index)));
                args.push(ReviewedType::Wildcard { extends: ext, super_: sup });
            }
        }
    }
    // Type variable lookup
    if let Some((uppers, lower)) = env.method_tparams.get(&tref.name).or_else(|| env.class_tparams.get(&tref.name)) {
        return ReviewedType::TypeVar { name: tref.name.clone(), upper: uppers.clone(), lower: lower.clone().map(|b| Box::new(b)) };
    }
    // Class/Interface
    if tref.name == "java.lang.Object" || tref.name == "Object" {
        ReviewedType::Object
    } else {
        ReviewedType::Class { name: tref.name.clone(), args }
    }
}

pub fn capture(ty: &ReviewedType, _env: &TypeEnv) -> ReviewedType {
    match ty {
        ReviewedType::Class { name, args } => {
            let mut out: Vec<ReviewedType> = Vec::with_capacity(args.len());
            for a in args {
                match a {
                    ReviewedType::Wildcard { extends, super_ } => {
                        let upper = if let Some(u) = extends {
                            vec![*u.clone()]
                        } else {
                            vec![ReviewedType::Object]
                        };
                        let lower = super_.as_ref().map(|l| l.clone());
                        out.push(ReviewedType::TypeVar { name: fresh_cap_name(), upper, lower });
                    }
                    other => out.push(other.clone()),
                }
            }
            ReviewedType::Class { name: name.clone(), args: out }
        }
        _ => ty.clone(),
    }
}

pub fn is_assignable(src: &ReviewedType, dst: &ReviewedType, env: &TypeEnv, index: &GlobalMemberIndex) -> bool {
    if src == dst { return true; }
    match (src, dst) {
        (ReviewedType::Null, ReviewedType::Class{..}) | (ReviewedType::Null, ReviewedType::Array{..}) | (ReviewedType::Null, ReviewedType::Object) => true,
        (ReviewedType::Primitive(a), ReviewedType::Primitive(b)) => primitive_widening_allows(a, b),
        (ReviewedType::Array{of:a}, ReviewedType::Array{of:b}) => is_assignable(a, b, env, index),
        (ReviewedType::Array{..}, ReviewedType::Object) => true,
        (ReviewedType::Class{name: ns, args: as_}, ReviewedType::Class{name: nt, args: at}) => {
            if !is_nominal_subtype_name(ns, nt, index) { return false; }
            // Invariance for exact type arguments when both sides provide concrete args
            if !as_.is_empty() && !at.is_empty() {
                if as_.len() != at.len() { return false; }
                for (ai, bi) in as_.iter().zip(at) {
                    if !type_arg_compatible(ai, bi, env, index) { return false; }
                }
                return true;
            }
            // If destination is raw/unparameterized, allow nominal assignment
            if at.is_empty() { return true; }
            // If source is raw/unparameterized but destination has args, reject (unsafe)
            false
        }
        // TypeVar on dst: src must fit within dst bounds
        (s, ReviewedType::TypeVar{upper, lower, ..}) => {
            let up_ok = if upper.is_empty() { true } else { upper.iter().any(|u| is_assignable(s, u, env, index)) };
            let lo_ok = if let Some(l) = lower { is_assignable(l, s, env, index) } else { true };
            up_ok && lo_ok
        }
        // TypeVar on src: its uppers must be assignable to dst
        (ReviewedType::TypeVar{upper, lower, ..}, t) => {
            let up_ok = if upper.is_empty() { false } else { upper.iter().any(|u| is_assignable(u, t, env, index)) };
            let lo_ok = if let Some(l) = lower { is_assignable(t, l, env, index) } else { true };
            up_ok && lo_ok
        }
        _ => false,
    }
}

pub fn is_subtype(src: &ReviewedType, dst: &ReviewedType, env: &TypeEnv, index: &GlobalMemberIndex) -> bool {
    is_assignable(src, dst, env, index)
}

pub fn build_class_env(_class: &ClassDecl, _index: &GlobalMemberIndex) -> TypeEnv { TypeEnv::default() }

pub fn extend_with_method_env(base: &TypeEnv, _method: &MethodDecl, _index: &GlobalMemberIndex) -> TypeEnv { base.clone() }

pub fn describe_type(_t: &ReviewedType) -> String { String::from("<type>") }

fn primitive_widening_allows(a: &&str, b: &&str) -> bool {
    // Minimal table: exact only for now
    a == b
}

fn is_nominal_subtype_name(src: &str, dst: &str, index: &GlobalMemberIndex) -> bool {
    if src == dst { return true; }
    if let Some(mt) = index.by_type.get(src) {
        if let Some(sup) = &mt.super_name { 
            // Direct match
            if sup == dst { return true; }
            // Check if super_name is fully qualified and dst is simple name in same package
            if sup.contains('.') && !dst.contains('.') {
                if let Some(pkg) = &mt.package_name {
                    let expected_fq = format!("{}.{}", pkg, dst);
                    if sup == &expected_fq { return true; }
                }
            }
            // Recursive check
            if is_nominal_subtype_name(sup, dst, index) { return true; }
        }
        // Check implemented interfaces
        for itf in &mt.interfaces { 
            // Direct interface match
            if itf == dst { return true; }
            // Check if interface is fully qualified and dst is simple name in same package
            if itf.contains('.') && !dst.contains('.') {
                if let Some(pkg) = &mt.package_name {
                    let expected_fq = format!("{}.{}", pkg, dst);
                    if itf == &expected_fq { return true; }
                }
            }
            // Recursive interface check
            if is_nominal_subtype_name(itf, dst, index) { return true; }
        }
    }
    false
}

fn type_arg_compatible(src: &ReviewedType, dst: &ReviewedType, env: &TypeEnv, index: &GlobalMemberIndex) -> bool {
    match dst {
        ReviewedType::Wildcard{extends: Some(ub), super_: None} => is_assignable(src, ub, env, index),
        ReviewedType::Wildcard{extends: None, super_: Some(lb)} => is_assignable(lb, src, env, index),
        ReviewedType::Wildcard{extends: None, super_: None} => true,
        ReviewedType::TypeVar{upper, lower, ..} => {
            let up_ok = if upper.is_empty() { true } else { upper.iter().any(|u| is_assignable(src, u, env, index)) };
            let lo_ok = if let Some(l) = lower { is_assignable(l, src, env, index) } else { true };
            up_ok && lo_ok
        }
        other => src == other,
    }
}

fn fresh_cap_name() -> String {
    // simple unique id based on pointer address
    format!("CAP{}", fresh_id())
}

fn fresh_id() -> usize { use std::sync::atomic::{AtomicUsize, Ordering}; static ID: AtomicUsize = AtomicUsize::new(1); ID.fetch_add(1, Ordering::Relaxed) }


