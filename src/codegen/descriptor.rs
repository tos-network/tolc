//! Utilities to build method/field descriptors

use crate::ast::TypeRef;
use crate::common::consts::JAVA_LANG_SIMPLE_TYPES;
use crate::common::classpath;

pub fn type_to_descriptor(ty: &TypeRef) -> String {
    let mut desc = String::new();
    for _ in 0..ty.array_dims { desc.push('['); }
    
    match ty.name.as_str() {
        "int" => {
            desc.push_str("I");
            desc
        },
        "long" => {
            desc.push_str("J");
            desc
        },
        "float" => {
            desc.push_str("F");
            desc
        },
        "double" => {
            desc.push_str("D");
            desc
        },
        "boolean" => {
            desc.push_str("Z");
            desc
        },
        "char" => {
            desc.push_str("C");
            desc
        },
        "byte" => {
            desc.push_str("B");
            desc
        },
        "short" => {
            desc.push_str("S");
            desc
        },
        "void" => {
            desc.push_str("V");
            desc
        },
        _ => {
            let simple = ty.name.as_str();
            // Handle generic type variables (single uppercase letters) - erase to Object
            if simple.len() == 1 && simple.chars().next().unwrap().is_ascii_uppercase() {
                format!("{}Ljava/lang/Object;", desc)
            } else {
                // Prefer classpath mapping when given a simple name; otherwise use provided fqcn
                let internal = if !simple.contains('/') && !simple.contains('.') {
                    if let Some(mapped) = classpath::resolve_class_name(simple) {
                        mapped.to_string()
                    } else if JAVA_LANG_SIMPLE_TYPES.contains(&simple) {
                        format!("java/lang/{}", simple)
                    } else {
                        simple.replace('.', "/")
                    }
                } else {
                    ty.name.replace('.', "/")
                };
                format!("{}L{};", desc, internal)
            }
        },
    }
}

pub fn method_descriptor(params: &[TypeRef], ret: Option<&TypeRef>) -> String {
    let mut d = String::new();
    d.push('(');
    for p in params { d.push_str(&type_to_descriptor(p)); }
    d.push(')');
    if let Some(r) = ret { d.push_str(&type_to_descriptor(r)); } else { d.push('V'); }
    d
}




