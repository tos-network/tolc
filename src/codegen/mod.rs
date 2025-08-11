//! Code generation module for Terminos Language
//! 
//! This module handles the conversion of AST nodes into Java bytecode (.class files).

pub mod annotation;
pub mod attribute;
pub mod bytecode;
pub mod class;
pub mod class_writer;
pub mod constpool;
pub mod defs;
pub mod descriptor;
pub mod error;
pub mod field;
pub mod flag;
pub mod frame;
pub mod method;
pub mod method_writer;
pub mod opcodes;
pub mod opcodor;
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
pub use opcodor::*;

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

/// Generate Java bytecode from an AST
pub fn generate_bytecode(ast: &Ast, output_dir: &str, config: &Config) -> Result<()> {
    let output_path = Path::new(output_dir);
    
    // Ensure output directory exists
    std::fs::create_dir_all(output_path)?;
    
    // Generate bytecode for each type declaration
    for type_decl in &ast.type_decls {
        match type_decl {
            TypeDecl::Class(class) => {
                generate_class_bytecode(class, output_path, config)?;
            }
            TypeDecl::Interface(interface) => {
                generate_interface_bytecode(interface, output_path, config)?;
            }
            TypeDecl::Enum(enum_decl) => {
                generate_enum_bytecode(enum_decl, output_path, config)?;
            }
            TypeDecl::Annotation(annotation) => {
                generate_annotation_bytecode(annotation, output_path, config)?;
            }
        }
    }
    
    Ok(())
}

/// Generate bytecode for a class declaration
fn generate_class_bytecode(class: &ClassDecl, output_dir: &Path, config: &Config) -> Result<()> {
    let mut class_writer = ClassWriter::new_with_config(config.clone());
    
    // Generate the class bytecode using the existing method
    class_writer.generate_class(class)?;
    
    // Get the generated class file and write it
    let class_file = class_writer.get_class_file();
    let class_file_path = output_dir.join(format!("{}.class", class.name));
    
    // Write the class file bytes to disk
    let bytes = class_file_to_bytes(&class_file);
    std::fs::write(&class_file_path, bytes)?;
    
    Ok(())
}

/// Generate bytecode for an interface declaration
fn generate_interface_bytecode(interface: &InterfaceDecl, output_dir: &Path, config: &Config) -> Result<()> {
    let mut class_writer = ClassWriter::new_with_config(config.clone());
    
    // Generate the interface bytecode
    class_writer.generate_interface(interface)?;
    
    // Get the generated class file and write it
    let class_file = class_writer.get_class_file();
    let class_file_path = output_dir.join(format!("{}.class", interface.name));
    
    let bytes = class_file_to_bytes(&class_file);
    std::fs::write(&class_file_path, bytes)?;
    
    Ok(())
}

/// Generate bytecode for an enum declaration
fn generate_enum_bytecode(enum_decl: &EnumDecl, output_dir: &Path, config: &Config) -> Result<()> {
    let mut class_writer = ClassWriter::new_with_config(config.clone());
    
    // Generate the enum bytecode
    class_writer.generate_enum(enum_decl)?;
    
    // Get the generated class file and write it
    let class_file = class_writer.get_class_file();
    let class_file_path = output_dir.join(format!("{}.class", enum_decl.name));
    
    let bytes = class_file_to_bytes(&class_file);
    std::fs::write(&class_file_path, bytes)?;
    
    Ok(())
}

/// Generate bytecode for an annotation declaration
fn generate_annotation_bytecode(annotation: &AnnotationDecl, output_dir: &Path, config: &Config) -> Result<()> {
    let mut class_writer = ClassWriter::new_with_config(config.clone());
    
    // Generate the annotation bytecode
    class_writer.generate_annotation(annotation)?;
    
    // Get the generated class file and write it
    let class_file = class_writer.get_class_file();
    let class_file_path = output_dir.join(format!("{}.class", annotation.name));
    
    let bytes = class_file_to_bytes(&class_file);
    std::fs::write(&class_file_path, bytes)?;
    
    Ok(())
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
        }
    }
    
    flags
}
