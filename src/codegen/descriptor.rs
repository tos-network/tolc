//! Utilities to build method/field descriptors

use crate::ast::TypeRef;

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
            // Map common java.lang simple names to fully-qualified internal names
            let simple = ty.name.as_str();
            let mapped = match simple {
                "String" | "Object" | "Throwable" | "Cloneable" | "Serializable" | "Integer" | "Long" | "Float" | "Double" | "Boolean" | "Character" | "Short" | "Byte" | "Void" => {
                    format!("java/lang/{}", simple)
                }
                _ => ty.name.replace('.', "/"),
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


