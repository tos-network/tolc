//! Code generation module for Terminos Language
//! 
//! This module handles the conversion of AST nodes into Java bytecode (.class files).

pub mod advanced_optimizer;
pub mod annotation;
pub mod assignment_optimizer;
pub mod attribute;
pub mod bytecode;
pub mod cast_optimizer;
pub mod chain;
pub mod class;
pub mod complexity_analyzer;
pub mod cond_item;
pub mod conditional_optimizer;
pub mod gen_cond;
pub mod class_writer;
pub mod classpath;
pub mod constant_optimizer;
pub mod constpool;
pub mod defs;
pub mod descriptor;
pub mod error;
pub mod exception_optimizer;
pub mod field;
pub mod field_access_optimizer;
pub mod finalizer_optimizer;
pub mod flag;
pub mod frame;
pub mod increment_optimizer;
pub mod instruction_optimizer;
pub mod instruction_widening;
pub mod item_system;
pub mod item_manager;
pub mod enhanced_stack_map_emitter;
pub mod loop_optimizer;
pub mod method;
pub mod method_invocation_optimizer;
pub mod method_writer;
pub mod object_optimizer;
pub mod opcodes;
pub mod opcode_generator;
pub mod signature;
pub mod stack_map_optimizer;
pub mod string_optimizer;
pub mod string_buffer_optimizer;
pub mod switch_optimizer;
pub mod type_coercion_optimizer;
pub mod typed_index;
pub mod vec;
pub mod writer;


// Re-export commonly used types
pub use class::*;
pub use class_writer::*;
pub use constpool::{ConstantPool, Constant, ConstPoolError};
pub use error::{ClassGenerationError, CodeGenResult};
pub use method::*;
pub use method_writer::*;
pub use opcode_generator::*;

pub use vec::*;
pub use writer::*;

// Re-export specific types from bytecode and typed_index to avoid conflicts
pub use bytecode::{StackState, StackFrame, LocalSlot, LocalType, StackError, BytecodeBuilder, ExceptionTableEntry};
pub use typed_index::{
    ConstPoolIndex, RawConstPoolIndex, ConstPoolEntryInfo,
    ClassIndex, StringIndex, NameAndTypeIndex, FieldRefIndex, MethodRefIndex,
    InterfaceMethodRefIndex, MethodHandleIndex, MethodTypeIndex, DynamicIndex, InvokeDynamicIndex
};

// Re-export the class_file_to_bytes function for convenience
pub use writer::class_file_to_bytes;

use crate::ast::*;
use crate::error::Result;
use crate::config::Config;
use std::path::Path;
use std::collections::HashMap;

/// Generate Java bytecode from an AST
pub fn generate_bytecode(ast: &Ast, output_dir: &str, config: &Config) -> Result<()> {
    let output_path = Path::new(output_dir);
    
    // Ensure output directory exists
    std::fs::create_dir_all(output_path)?;
    
    // Build compilation-unit level annotation retention index
    let cu_retention = build_annotation_retention_index_from_cu(ast);

    // Generate bytecode for each type declaration
    for type_decl in &ast.type_decls {
        match type_decl {
            TypeDecl::Class(class) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                class_writer.set_all_types(ast.type_decls.clone());
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_class(class)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let class_file_path = output_path.join(format!("{}.class", class.name));
                let bytes = class_file_to_bytes(&class_file);
                std::fs::write(&class_file_path, bytes)?;
            }
            TypeDecl::Interface(interface) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                class_writer.set_all_types(ast.type_decls.clone());
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_interface(interface)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let class_file_path = output_path.join(format!("{}.class", interface.name));
                let bytes = class_file_to_bytes(&class_file);
                std::fs::write(&class_file_path, bytes)?;
            }
            TypeDecl::Enum(enum_decl) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_enum(enum_decl)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let class_file_path = output_path.join(format!("{}.class", enum_decl.name));
                let bytes = class_file_to_bytes(&class_file);
                std::fs::write(&class_file_path, bytes)?;
            }
            TypeDecl::Annotation(annotation) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                class_writer.generate_annotation(annotation)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let class_file_path = output_path.join(format!("{}.class", annotation.name));
                let bytes = class_file_to_bytes(&class_file);
                std::fs::write(&class_file_path, bytes)?;
            }
        }
    }
    
    Ok(())
}

/// Generate bytecode for a class declaration
/// Generate bytecode for an interface declaration
fn generate_interface_bytecode(interface: &InterfaceDecl, output_dir: &Path, config: &Config) -> Result<()> {
    let mut class_writer = ClassWriter::new_with_config(config.clone());
    // Retention index is now provided at CU level in generate_bytecode
    
    // Generate the interface bytecode
    class_writer.generate_interface(interface)?;
    
    // Get the generated class file and verify before writing
    let class_file = class_writer.get_class_file();
    crate::verify::verify(&class_file)
        .map_err(|e| crate::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
    let class_file_path = output_dir.join(format!("{}.class", interface.name));
    
    let bytes = class_file_to_bytes(&class_file);
    std::fs::write(&class_file_path, bytes)?;
    
    Ok(())
}

/// Generate bytecode for an enum declaration
fn generate_enum_bytecode(enum_decl: &EnumDecl, output_dir: &Path, config: &Config) -> Result<()> {
    let mut class_writer = ClassWriter::new_with_config(config.clone());
    class_writer.set_annotation_retention_index(HashMap::new());
    
    // Generate the enum bytecode
    class_writer.generate_enum(enum_decl)?;
    
    // Get the generated class file and verify before writing
    let class_file = class_writer.get_class_file();
    crate::verify::verify(&class_file)
        .map_err(|e| crate::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
    let class_file_path = output_dir.join(format!("{}.class", enum_decl.name));
    
    let bytes = class_file_to_bytes(&class_file);
    std::fs::write(&class_file_path, bytes)?;
    
    Ok(())
}

/// Generate bytecode for an annotation declaration
fn generate_annotation_bytecode(annotation: &AnnotationDecl, output_dir: &Path, config: &Config) -> Result<()> {
    let mut class_writer = ClassWriter::new_with_config(config.clone());
    class_writer.set_annotation_retention_index(HashMap::new());
    
    // Generate the annotation bytecode
    class_writer.generate_annotation(annotation)?;
    
    // Get the generated class file and verify before writing
    let class_file = class_writer.get_class_file();
    crate::verify::verify(&class_file)
        .map_err(|e| crate::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
    let class_file_path = output_dir.join(format!("{}.class", annotation.name));
    
    let bytes = class_file_to_bytes(&class_file);
    std::fs::write(&class_file_path, bytes)?;
    
    Ok(())
}

// Build a CU-local annotation retention index (simple impl: scan enclosing type's members and siblings)
fn build_annotation_retention_index_from_cu(ast: &Ast) -> HashMap<String, attribute::RetentionPolicy> {
    let mut map = HashMap::new();
    for td in &ast.type_decls {
        match td {
            TypeDecl::Annotation(a) => {
                if let Some(p) = infer_retention_from_annotation_decl(a) { map.insert(a.name.clone(), p); }
            }
            TypeDecl::Class(c) => {
                for m in &c.body { if let ClassMember::TypeDecl(TypeDecl::Annotation(a)) = m { if let Some(p) = infer_retention_from_annotation_decl(a) { map.insert(a.name.clone(), p); } } }
            }
            TypeDecl::Interface(i) => {
                for m in &i.body { if let InterfaceMember::TypeDecl(TypeDecl::Annotation(a)) = m { if let Some(p) = infer_retention_from_annotation_decl(a) { map.insert(a.name.clone(), p); } } }
            }
            _ => {}
        }
    }
    map
}

fn infer_retention_from_annotation_decl(a: &AnnotationDecl) -> Option<attribute::RetentionPolicy> {
    // Look for @Retention(...) among annotations attached to this annotation declaration
    for ann in &a.annotations {
        let simple = ann.name.rsplit('.').next().unwrap_or(ann.name.as_str());
        if simple == "Retention" || ann.name == "java.lang.annotation.Retention" {
            // parse value: either value=RUNTIME/CLASS or single Value expr
            for arg in &ann.arguments {
                match arg {
                    AnnotationArg::Named(_, expr) => { if let Some(pol) = map_expr_to_retention(expr) { return Some(pol); } }
                    AnnotationArg::Value(expr) => { if let Some(pol) = map_expr_to_retention(expr) { return Some(pol); } }
                }
            }
        }
    }
    None
}

fn map_expr_to_retention(e: &Expr) -> Option<attribute::RetentionPolicy> {
    use attribute::RetentionPolicy;
    match e {
        Expr::Identifier(id) => {
            let last = id.name.rsplit('.').next().unwrap_or(id.name.as_str());
            match last {
                "RUNTIME" => Some(RetentionPolicy::Runtime),
                "CLASS" => Some(RetentionPolicy::Class),
                _ => None,
            }
        }
        Expr::FieldAccess(fa) => {
            // e.g., RetentionPolicy.RUNTIME
            let qual = if let Some(t) = &fa.target { if let Expr::Identifier(id) = &**t { id.name.as_str() } else { "" } } else { "" };
            let name = fa.name.as_str();
            if qual.ends_with("RetentionPolicy") {
                match name {
                    "RUNTIME" => Some(RetentionPolicy::Runtime),
                    "CLASS" => Some(RetentionPolicy::Class),
                    _ => None,
                }
            } else { None }
        }
        _ => None,
    }
}

/// Helper function to convert modifiers to bytecode flags
fn modifiers_to_flags(modifiers: &[Modifier]) -> u16 {
    let mut flags = 0;
    
    for modifier in modifiers {
        match modifier {
            Modifier::Public => flags |= flag::access_flags::ACC_PUBLIC,
            Modifier::Private => flags |= flag::access_flags::ACC_PRIVATE,
            Modifier::Protected => flags |= flag::access_flags::ACC_PROTECTED,
            Modifier::Static => flags |= flag::access_flags::ACC_STATIC,
            Modifier::Final => flags |= flag::access_flags::ACC_FINAL,
            Modifier::Abstract => flags |= flag::access_flags::ACC_ABSTRACT,
            Modifier::Native => flags |= flag::access_flags::ACC_NATIVE,
            Modifier::Synchronized => flags |= flag::access_flags::ACC_SYNCHRONIZED,
            Modifier::Transient => flags |= flag::access_flags::ACC_TRANSIENT,
            Modifier::Volatile => flags |= flag::access_flags::ACC_VOLATILE,
            Modifier::Strictfp => flags |= flag::access_flags::ACC_STRICT,
            // No direct access flag for default methods
            Modifier::Default => {}
        }
    }
    
    flags
}
