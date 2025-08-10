//! Code generation module for Terminos Language
//! 
//! This module handles the conversion of AST nodes into Java bytecode (.class files).

mod bytecode;
mod class_writer;
mod method_writer;
mod constpool;
mod descriptor;
mod flag;
mod attribute;
mod class;
mod field;
mod method;
mod defs;
mod frame;
mod writer;

pub use bytecode::*;
pub use class_writer::*;
pub use method_writer::*;
pub use descriptor::*;
pub use flag::*;
pub use attribute::*;
pub use class::*;
pub use field::*;
pub use method::*;
pub use defs::*;
pub use frame::*;
pub use writer::*;

use crate::ast::*;
use crate::error::{Result, Error};
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
            Modifier::Public => flags |= access_flags::ACC_PUBLIC,
            Modifier::Private => flags |= access_flags::ACC_PRIVATE,
            Modifier::Protected => flags |= access_flags::ACC_PROTECTED,
            Modifier::Static => flags |= access_flags::ACC_STATIC,
            Modifier::Final => flags |= access_flags::ACC_FINAL,
            Modifier::Abstract => flags |= access_flags::ACC_ABSTRACT,
            Modifier::Native => flags |= access_flags::ACC_NATIVE,
            Modifier::Synchronized => flags |= access_flags::ACC_SYNCHRONIZED,
            Modifier::Transient => flags |= access_flags::ACC_TRANSIENT,
            Modifier::Volatile => flags |= access_flags::ACC_VOLATILE,
            Modifier::Strictfp => flags |= access_flags::ACC_STRICT,
            _ => {}
        }
    }
    
    flags
}
