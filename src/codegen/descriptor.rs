//! Utilities to build method/field descriptors

use crate::ast::TypeRef;
use crate::consts::JAVA_LANG_SIMPLE_TYPES;
use super::classpath;

pub fn type_to_descriptor(ty: &TypeRef) -> String {
    let mut desc = String::new();
    for _ in 0..ty.array_dims { desc.push('['); }
    let base = match ty.name.as_str() {
        "int" => "I",
        "long" => "J",
        "float" => "F",
        "double" => "D",
        "boolean" => "Z",
        "char" => "C",
        "byte" => "B",
        "short" => "S",
        "void" => "V",
        _ => {
            let simple = ty.name.as_str();
            
            // Handle generic type variables (single uppercase letters) - erase to Object
            if simple.len() == 1 && simple.chars().next().unwrap().is_ascii_uppercase() {
                return format!("{}Ljava/lang/Object;", desc);
            }
            
            // First try the classpath mapping for accurate resolution
            let mapped = if let Some(internal_name) = classpath::resolve_class_name(simple) {
                internal_name.to_string()
            } else if JAVA_LANG_SIMPLE_TYPES.contains(&simple) {
                // Fallback for java.lang types not in classpath
                format!("java/lang/{}", simple)
            } else {
                // Handle any package structure, including java.base.*
                ty.name.replace('.', "/")
            };
            return format!("{}L{};", desc, mapped);
        },
    };
    desc.push_str(base);
    desc
}

pub fn method_descriptor(params: &[TypeRef], ret: Option<&TypeRef>) -> String {
    let mut d = String::new();
    d.push('(');
    for p in params { d.push_str(&type_to_descriptor(p)); }
    d.push(')');
    if let Some(r) = ret { d.push_str(&type_to_descriptor(r)); } else { d.push('V'); }
    d
}




