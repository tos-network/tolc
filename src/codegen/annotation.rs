//! Annotation handling for Java bytecode generation

use crate::ast::*;
use crate::common::error::Result;
use super::class::ClassFile;
use crate::codegen::descriptor::type_to_descriptor;
use super::method::MethodInfo;
use super::flag::access_flags;

/// Generate bytecode for an annotation declaration
pub fn generate_annotation(annotation: &AnnotationDecl, class_file: &mut ClassFile) -> Result<()> {
    // Set annotation name and access flags
    let annotation_name = &annotation.name;
    let this_class_index = class_file.constant_pool.try_add_class(annotation_name)
        .map_err(|e| crate::common::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
    class_file.this_class = this_class_index;

    // Set access flags - annotations are always interfaces
    let mut access_flags = access_flags::ACC_INTERFACE | access_flags::ACC_ABSTRACT | access_flags::ACC_ANNOTATION;
    if annotation.modifiers.contains(&Modifier::Public) {
        access_flags |= access_flags::ACC_PUBLIC;
    }
    class_file.access_flags = access_flags;

    // Set superclass to java.lang.annotation.Annotation
    let super_class_index = class_file.constant_pool.try_add_class("java/lang/annotation/Annotation")
        .map_err(|e| crate::common::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
    class_file.super_class = super_class_index;

    // Generate annotation members
    for member in &annotation.body {
        generate_annotation_member(member, class_file)?;
    }

    Ok(())
}

/// Generate bytecode for an annotation member
fn generate_annotation_member(member: &AnnotationMember, class_file: &mut ClassFile) -> Result<()> {
    // Annotation members are implicitly public and abstract methods
    let method_flags = access_flags::ACC_PUBLIC | access_flags::ACC_ABSTRACT;

    let name_index = class_file.constant_pool.try_add_utf8(&member.name)
        .map_err(|e| crate::common::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
    let descriptor = type_to_descriptor(&member.type_ref);
    let descriptor_index = class_file.constant_pool.try_add_utf8(&descriptor)
        .map_err(|e| crate::common::error::Error::CodeGen { message: format!("const pool: {}", e) })?;

    let method_info = MethodInfo {
        access_flags: method_flags,
        name_index,
        descriptor_index,
        attributes: Vec::new(),
    };

    class_file.methods.push(method_info);
    Ok(())
}



/// Convert type reference to JVM type descriptor
fn _legacy_type_to_descriptor(type_ref: &TypeRef) -> String {
    // Handle array dimensions
    let mut descriptor = String::new();
    for _ in 0..type_ref.array_dims {
        descriptor.push('[');
    }
    
    // Handle base type
    match type_ref.name.as_str() {
        "boolean" => descriptor.push('Z'),
        "byte" => descriptor.push('B'),
        "char" => descriptor.push('C'),
        "double" => descriptor.push('D'),
        "float" => descriptor.push('F'),
        "int" => descriptor.push('I'),
        "long" => descriptor.push('J'),
        "short" => descriptor.push('S'),
        "void" => descriptor.push('V'),
        _ => {
            // Reference type
            descriptor.push('L');
            descriptor.push_str(&type_ref.name.replace('.', "/"));
            descriptor.push(';');
        }
    }
    
    descriptor
}
