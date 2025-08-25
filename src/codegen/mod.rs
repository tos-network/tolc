//! Code generation module for Terminos Language
//! 
//! This module handles the conversion of AST nodes into Java bytecode (.class files).
//! 
//! ## JavaC-Aligned Architecture
//! New modular architecture based on Oracle javac structure:
//! - gen.rs: Main bytecode generator (corresponds to javac Gen.java)
//! - gen_expr.rs: Expression generation  
//! - gen_stmt.rs: Statement generation
//! - gen_cond.rs: Condition handling (unified)
//!
//! ## Semantic Analysis Pipeline
//! 
//! This module also implements the semantic analysis phases used between
//! parsing and code generation. Each phase corresponds to a compilation stage:
//! 
//! - Enter: Symbol table construction and import resolution
//! - Attr: Type checking and method resolution 
//! - Flow: Definite assignment and reachability analysis
//! - TransTypes: Generic type erasure and bridge method generation
//! - Lower: Syntactic sugar desugaring (enhanced for loops, string concat, etc.)

// Semantic analysis pipeline modules (migrated from wash)
// pub mod enter_old;      // Old symbol table construction (backed up)
pub mod enter;          // Enhanced Enter phase with dynamic type resolution (main)
pub mod attr;           // Type checking and method resolution
pub mod flow;           // Definite assignment and reachability analysis
pub mod trans_types;    // Generic type erasure and bridge method generation
pub mod lower;          // Syntactic sugar desugaring

// New javac-aligned architecture
pub mod gen;          // Main bytecode generator (corresponds to javac Gen.java)  
// Backed up: pub mod gen_expr;     // Expression generation
// Backed up: pub mod gen_stmt;     // Statement generation
pub mod gen_visitor;  // JavaC-style visitor methods (100% Gen.java aligned)
// Backed up: pub mod gen_items;    // Legacy item system management
// Backed up: pub mod items;        // New JavaC Items system (100% aligned)
// Backed up: pub mod items_simple; // Simplified Items system for Phase 2
pub mod items;  // Bytecode Items system with direct Code manipulation
pub mod method_context; // Method-level context management
// Backed up: pub mod optimization_manager; // Unified optimization management
pub mod symtab;      // JavaC-aligned symbol table
pub mod types;       // JavaC-aligned type system  
pub mod type_inference; // Type inference and checking
pub mod unified_resolver; // Unified identifier resolution facade
pub mod array_type_info; // Enhanced array type representation
// Enhanced type resolution functionality moved to common::type_resolver
// Dynamic class loader moved to common::classloader

// Optimizer architecture
pub mod const_fold; // Constant folding operations
// Backed up: pub mod attr_optimizer;   // Attribute phase optimizations
// Backed up: pub mod lower_optimizer;  // Lower phase optimizations
// Backed up: pub mod optimizer_alignment; // Optimizer alignment verification

// Backed up: pub mod advanced_optimizer;
pub mod annotation;  // Needed by class_writer.rs  
// Backed up: pub mod assignment_optimizer;
pub mod attribute;
pub mod branch_optimizer;    // JavaC-aligned branch optimization
// Backed up: pub mod bytecode; // Still needed by some existing code
pub mod code;        // New JavaC-aligned code buffer
// Backed up: pub mod code_builder; // High-level builder using Code
pub mod chain;
pub mod cond_item;
pub mod class;
// Backed up: pub mod complexity_analyzer;
// Backed up: pub mod cond_item;
// Backed up: pub mod conditional_optimizer;
// Backed up: pub mod gen_cond;
pub mod class_writer;
// Backed up: pub mod constant_optimizer;
pub mod constpool;
pub mod defs;
pub mod descriptor;
pub mod error;
// Backed up: pub mod exception_optimizer;
pub mod field;
// Backed up: pub mod field_access_optimizer;
// Backed up: pub mod finalizer_optimizer;
pub mod flag;
pub mod frame;
// Backed up: pub mod increment_optimizer;
// Backed up: pub mod instruction_optimizer;
// Backed up: pub mod instruction_widening;
// Backed up: pub mod item_system;
// Backed up: pub mod item_manager;
// Backed up: pub mod enhanced_stack_map_emitter;
// Backed up: pub mod branch_marker;
// Backed up: pub mod code_compactor;
// Backed up: pub mod type_erasure;
// Backed up: pub mod type_erasure_integration;
// Backed up: pub mod fatcode_manager;
// Backed up: pub mod pending_jumps; // Needed by JavaC code.rs
// Backed up: pub mod fixed_pc_manager;
// Backed up: pub mod jsr_ret_optimizer;
// Backed up: pub mod enhanced_string_optimizer;
pub mod line_number_optimizer; // JavaC-aligned line number debug optimization
pub mod jump_chain_optimizer;  // Jump chain optimization
// Backed up: pub mod loop_optimizer;
pub mod method;
// Backed up: pub mod method_invocation_optimizer;
pub mod register_alloc; // Register allocation for local variables (JavaC alignment)
// Backed up: pub mod method_writer;
// Backed up: pub mod object_optimizer;
pub mod opcodes;
pub mod opcode_enum;
pub mod opcode_generator;
pub mod signature;
pub mod stack_map_optimizer;
// Backed up: pub mod string_optimizer;
// Backed up: pub mod string_buffer_optimizer;
// Backed up: pub mod switch_optimizer;
// Backed up: pub mod type_coercion_optimizer;
pub mod type_cast_optimizer;    // JavaC-aligned type cast optimization
pub mod array_access_optimizer; // JavaC-aligned array access optimization
pub mod typed_index;
pub mod vec;
pub mod writer;

// Performance optimization modules
pub mod constpool_optimized;
pub mod bytecode_optimized;
pub mod performance_monitor;



// Re-export commonly used types
pub use class::*;
pub use class_writer::*;
pub use constpool::{ConstantPool, Constant, ConstPoolError};
pub use error::{ClassGenerationError, CodeGenResult};
pub use method::*;
// Backed up: pub use method_writer::*;
pub use opcode_generator::*;

// Re-export optimization modules
pub use constpool_optimized::{OptimizedConstantPool, InternedString};
pub use bytecode_optimized::{OptimizedBytecodeBuffer, InstructionCache};
pub use performance_monitor::{PerformanceMetrics};

pub use vec::*;
pub use writer::*;

// Re-export specific types from bytecode and typed_index to avoid conflicts
// Backed up: pub use bytecode::{StackState, StackFrame, LocalSlot, LocalType, StackError, BytecodeBuilder, ExceptionTableEntry};
pub use code::{Code, State, Type as CodeType, StackMapFormat, ExceptionTableEntry as CodeExceptionTableEntry, LineNumberEntry, LocalVarEntry};
pub use typed_index::{
    ConstPoolIndex, RawConstPoolIndex, ConstPoolEntryInfo,
    ClassIndex, StringIndex, NameAndTypeIndex, FieldRefIndex, MethodRefIndex,
    InterfaceMethodRefIndex, MethodHandleIndex, MethodTypeIndex, DynamicIndex, InvokeDynamicIndex
};

// Re-export the class_file_to_bytes function for convenience
pub use writer::class_file_to_bytes;

use crate::ast::*;
use crate::common::error::Result;
use crate::common::config::Config;
use std::path::Path;
use std::collections::HashMap;

/// Generate Java bytecode from an AST with wash results
pub fn generate_bytecode_with_wash(
    ast: &Ast, 
    output_dir: &str, 
    config: &Config, 
    signatures: Option<&std::collections::HashMap<String, String>>,
    wash_type_info: Option<std::collections::HashMap<String, crate::codegen::attr::ResolvedType>>,
    wash_symbol_env: Option<crate::common::env::SymbolEnvironment>
) -> Result<()> {
    generate_bytecode_impl(ast, output_dir, config, signatures, wash_type_info, wash_symbol_env)
}

/// Generate Java bytecode from an AST (legacy interface)
pub fn generate_bytecode(ast: &Ast, output_dir: &str, config: &Config, signatures: Option<&std::collections::HashMap<String, String>>) -> Result<()> {
    generate_bytecode_impl(ast, output_dir, config, signatures, None, None)
}

/// Internal implementation for bytecode generation
fn generate_bytecode_impl(
    ast: &Ast, 
    output_dir: &str, 
    config: &Config, 
    signatures: Option<&std::collections::HashMap<String, String>>,
    wash_type_info: Option<std::collections::HashMap<String, crate::codegen::attr::ResolvedType>>,
    wash_symbol_env: Option<crate::common::env::SymbolEnvironment>
) -> Result<()> {
    let output_path = Path::new(output_dir);
    
    // Ensure output directory exists
    std::fs::create_dir_all(output_path)?;
    
    // Build compilation-unit level annotation retention index
    let cu_retention = build_annotation_retention_index_from_cu(ast);

    // Extract all type declarations (including inner classes)
    let (all_type_decls, inner_class_relationships) = extract_all_type_declarations(ast);

    // Generate bytecode for each type declaration
    for (type_decl, parent_class_name) in all_type_decls {
        match type_decl {
            TypeDecl::Class(class) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                class_writer.set_all_types(ast.type_decls.clone());
                if let Some(sigs) = signatures {
                    class_writer.set_generic_signatures(sigs);
                }
                // Set wash results if available
                if let (Some(type_info), Some(symbol_env)) = (&wash_type_info, &wash_symbol_env) {
                    class_writer.set_wash_results(type_info.clone(), symbol_env.clone());
                }
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                
                // Set inner class relationships for InnerClasses attribute generation
                class_writer.set_inner_class_relationships(&inner_class_relationships);
                
                // Set parent class name for inner class generation
                class_writer.set_parent_class_name(parent_class_name.clone());
                
                class_writer.generate_class(&class)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                
                // Determine the class file name (for inner classes: OuterClass$InnerClass.class)
                let class_file_name = if let Some(outer_class) = &parent_class_name {
                    format!("{}${}.class", outer_class, class.name)
                } else {
                    format!("{}.class", class.name)
                };
                let class_file_path = output_path.join(class_file_name);
                let bytes = class_file_to_bytes(&class_file);
                std::fs::write(&class_file_path, bytes)?;
            }
            TypeDecl::Interface(interface) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                class_writer.set_all_types(ast.type_decls.clone());
                if let Some(sigs) = signatures {
                    class_writer.set_generic_signatures(sigs);
                }
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_interface(&interface)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
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
                class_writer.generate_enum(&enum_decl)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let class_file_path = output_path.join(format!("{}.class", enum_decl.name));
                let bytes = class_file_to_bytes(&class_file);
                std::fs::write(&class_file_path, bytes)?;
            }
            TypeDecl::Annotation(annotation) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_annotation(&annotation)?;
                let class_file = class_writer.get_class_file();
                crate::verify::verify(&class_file)
                    .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let class_file_path = output_path.join(format!("{}.class", annotation.name));
                let bytes = class_file_to_bytes(&class_file);
                std::fs::write(&class_file_path, bytes)?;
            }
        }
    }
    
    Ok(())
}

/// Inner class relationship information
#[derive(Debug, Clone)]
pub struct InnerClassInfo {
    pub inner_class_name: String,
    pub outer_class_name: String,
    pub simple_name: String,
    pub access_flags: u16,
}

/// Extract all type declarations from an AST, including nested inner classes
/// Returns a tuple of (types, inner_class_relationships)
fn extract_all_type_declarations(ast: &Ast) -> (Vec<(TypeDecl, Option<String>)>, Vec<InnerClassInfo>) {
    let mut all_types = Vec::new();
    let mut inner_class_info = Vec::new();
    
    // Process top-level type declarations
    for type_decl in &ast.type_decls {
        all_types.push((type_decl.clone(), None));
        
        // Extract inner classes from this type declaration
        extract_inner_classes_from_type_decl(type_decl, None, &mut all_types, &mut inner_class_info);
    }
    
    (all_types, inner_class_info)
}

/// Recursively extract inner classes from a type declaration
fn extract_inner_classes_from_type_decl(
    type_decl: &TypeDecl, 
    parent_class_name: Option<String>, 
    all_types: &mut Vec<(TypeDecl, Option<String>)>,
    inner_class_info: &mut Vec<InnerClassInfo>
) {
    match type_decl {
        TypeDecl::Class(class) => {
            let current_class_name = if let Some(parent) = &parent_class_name {
                format!("{}${}", parent, class.name)
            } else {
                class.name.clone()
            };
            
            // Extract nested type declarations from class members
            for member in &class.body {
                if let crate::ast::ClassMember::TypeDecl(nested_type) = member {
                    let nested_class_name = match nested_type {
                        TypeDecl::Class(nested_class) => &nested_class.name,
                        TypeDecl::Interface(nested_interface) => &nested_interface.name,
                        TypeDecl::Enum(nested_enum) => &nested_enum.name,
                        TypeDecl::Annotation(nested_annotation) => &nested_annotation.name,
                    };
                    
                    // Calculate access flags based on modifiers
                    let access_flags = calculate_access_flags(nested_type);
                    
                    // Add inner class relationship info
                    if let Some(parent) = &parent_class_name {
                        // This is a nested inner class (e.g., OuterClass$InnerClass$NestedClass)
                        inner_class_info.push(InnerClassInfo {
                            inner_class_name: format!("{}${}", current_class_name, nested_class_name),
                            outer_class_name: current_class_name.clone(),
                            simple_name: nested_class_name.clone(),
                            access_flags,
                        });
                    } else {
                        // This is a direct inner class (e.g., OuterClass$InnerClass)
                        inner_class_info.push(InnerClassInfo {
                            inner_class_name: format!("{}${}", class.name, nested_class_name),
                            outer_class_name: class.name.clone(),
                            simple_name: nested_class_name.clone(),
                            access_flags,
                        });
                    }
                    
                    all_types.push((nested_type.clone(), Some(current_class_name.clone())));
                    extract_inner_classes_from_type_decl(nested_type, Some(current_class_name.clone()), all_types, inner_class_info);
                }
            }
        }
        TypeDecl::Interface(interface) => {
            let current_interface_name = if let Some(parent) = &parent_class_name {
                format!("{}${}", parent, interface.name)
            } else {
                interface.name.clone()
            };
            
            // Extract nested type declarations from interface members
            for member in &interface.body {
                if let crate::ast::InterfaceMember::TypeDecl(nested_type) = member {
                    all_types.push((nested_type.clone(), Some(current_interface_name.clone())));
                    extract_inner_classes_from_type_decl(nested_type, Some(current_interface_name.clone()), all_types, inner_class_info);
                }
            }
        }
        TypeDecl::Enum(enum_decl) => {
            let current_enum_name = if let Some(parent) = &parent_class_name {
                format!("{}${}", parent, enum_decl.name)
            } else {
                enum_decl.name.clone()
            };
            
            // Enums can also have nested type declarations
            for member in &enum_decl.body {
                if let crate::ast::ClassMember::TypeDecl(nested_type) = member {
                    all_types.push((nested_type.clone(), Some(current_enum_name.clone())));
                    extract_inner_classes_from_type_decl(nested_type, Some(current_enum_name.clone()), all_types, inner_class_info);
                }
            }
        }
        TypeDecl::Annotation(_) => {
            // Annotations typically don't have complex nested types for now
        }
    }
}

/// Calculate access flags for a type declaration based on its modifiers
fn calculate_access_flags(type_decl: &TypeDecl) -> u16 {
    let modifiers = match type_decl {
        TypeDecl::Class(class) => &class.modifiers,
        TypeDecl::Interface(interface) => &interface.modifiers,
        TypeDecl::Enum(enum_decl) => &enum_decl.modifiers,
        TypeDecl::Annotation(annotation) => &annotation.modifiers,
    };
    
    let mut flags = 0u16;
    
    for modifier in modifiers {
        match modifier {
            crate::ast::Modifier::Public => flags |= 0x0001, // ACC_PUBLIC
            crate::ast::Modifier::Private => flags |= 0x0002, // ACC_PRIVATE
            crate::ast::Modifier::Protected => flags |= 0x0004, // ACC_PROTECTED
            crate::ast::Modifier::Static => flags |= 0x0008, // ACC_STATIC
            crate::ast::Modifier::Final => flags |= 0x0010, // ACC_FINAL
            crate::ast::Modifier::Abstract => flags |= 0x0400, // ACC_ABSTRACT
            _ => {} // Other modifiers not relevant for inner class access flags
        }
    }
    
    // For interfaces, add ACC_INTERFACE flag
    if matches!(type_decl, TypeDecl::Interface(_)) {
        flags |= 0x0200; // ACC_INTERFACE
    }
    
    // For annotations, add ACC_ANNOTATION flag  
    if matches!(type_decl, TypeDecl::Annotation(_)) {
        flags |= 0x2000; // ACC_ANNOTATION
    }
    
    // For enums, add ACC_ENUM flag
    if matches!(type_decl, TypeDecl::Enum(_)) {
        flags |= 0x4000; // ACC_ENUM
    }
    
    flags
}

/// Generate Java bytecode from an AST and return as Vec<u8> (in-memory compilation)
/// Returns the bytecode of the first type declaration found
/// Generate bytecode in memory with wash results
pub fn generate_bytecode_inmemory_with_wash(
    ast: &Ast, 
    config: &Config, 
    signatures: Option<&std::collections::HashMap<String, String>>,
    wash_type_info: Option<std::collections::HashMap<String, crate::codegen::attr::ResolvedType>>,
    wash_symbol_env: Option<crate::common::env::SymbolEnvironment>
) -> Result<Vec<u8>> {
    generate_bytecode_inmemory_impl(ast, config, signatures, wash_type_info, wash_symbol_env)
}

/// Generate bytecode in memory (legacy interface)
pub fn generate_bytecode_inmemory(ast: &Ast, config: &Config, signatures: Option<&std::collections::HashMap<String, String>>) -> Result<Vec<u8>> {
    generate_bytecode_inmemory_impl(ast, config, signatures, None, None)
}

/// Internal implementation for in-memory bytecode generation
fn generate_bytecode_inmemory_impl(
    ast: &Ast, 
    config: &Config, 
    signatures: Option<&std::collections::HashMap<String, String>>,
    wash_type_info: Option<std::collections::HashMap<String, crate::codegen::attr::ResolvedType>>,
    wash_symbol_env: Option<crate::common::env::SymbolEnvironment>
) -> Result<Vec<u8>> {
    // Build compilation-unit level annotation retention index
    let cu_retention = build_annotation_retention_index_from_cu(ast);

    // Extract all type declarations (including inner classes) 
    let (all_type_decls, inner_class_relationships) = extract_all_type_declarations(ast);

    // Generate bytecode for the first type declaration
    for (type_decl, parent_class_name) in all_type_decls.iter().take(1) {
        match type_decl {
            TypeDecl::Class(class) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                class_writer.set_all_types(ast.type_decls.clone());
                if let Some(sigs) = signatures {
                    class_writer.set_generic_signatures(sigs);
                }
                // Set wash results if available
                if let (Some(type_info), Some(symbol_env)) = (&wash_type_info, &wash_symbol_env) {
                    class_writer.set_wash_results(type_info.clone(), symbol_env.clone());
                }
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                
                // Set inner class relationships for InnerClasses attribute generation
                class_writer.set_inner_class_relationships(&inner_class_relationships);
                
                // Set parent class name for inner class generation
                class_writer.set_parent_class_name(parent_class_name.clone());
                
                class_writer.generate_class(&class)?;
                let class_file = class_writer.get_class_file();
                // Temporarily disable verification for in-memory compilation
                // crate::verify::verify(&class_file)
                //     .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let bytes = class_file_to_bytes(&class_file);
                return Ok(bytes);
            }
            TypeDecl::Interface(interface) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                class_writer.set_all_types(ast.type_decls.clone());
                if let Some(sigs) = signatures {
                    class_writer.set_generic_signatures(sigs);
                }
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_interface(&interface)?;
                let class_file = class_writer.get_class_file();
                // Temporarily disable verification for in-memory compilation
                // crate::verify::verify(&class_file)
                //     .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let bytes = class_file_to_bytes(&class_file);
                return Ok(bytes);
            }
            TypeDecl::Enum(enum_decl) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_enum(&enum_decl)?;
                let class_file = class_writer.get_class_file();
                // Temporarily disable verification for in-memory compilation
                // crate::verify::verify(&class_file)
                //     .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let bytes = class_file_to_bytes(&class_file);
                return Ok(bytes);
            }
            TypeDecl::Annotation(annotation) => {
                let mut class_writer = ClassWriter::new_with_config(config.clone());
                class_writer.set_annotation_retention_index(cu_retention.clone());
                // Set package name if present in AST
                if let Some(ref package) = ast.package_decl {
                    class_writer.set_package_name(Some(&package.name));
                }
                class_writer.generate_annotation(&annotation)?;
                let class_file = class_writer.get_class_file();
                // Temporarily disable verification for in-memory compilation
                // crate::verify::verify(&class_file)
                //     .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
                let bytes = class_file_to_bytes(&class_file);
                return Ok(bytes);
            }
        }
    }
    
    // No type declarations found
    Err(crate::common::error::Error::CodeGen { 
        message: "No type declarations found to compile".to_string() 
    })
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
        .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
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
        .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
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
        .map_err(|e| crate::common::error::Error::CodeGen { message: format!("ClassFile verify failed: {}", e) })?;
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

/// Main semantic analysis pipeline that orchestrates all phases
/// Follows standard compilation flow: EnhancedEnter → Attr → Flow → TransTypes → Lower
pub struct SemanticAnalyzer {
    pub enter: enter::EnhancedEnter,
    pub attr: attr::Attr,
    pub flow: flow::Flow,
    pub trans_types: trans_types::TransTypes,
    pub lower: lower::Lower,
    pub register_alloc: register_alloc::RegisterAllocator, // Register allocation for local variables
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            enter: enter::EnhancedEnter::new("."),
            attr: attr::Attr::new(),
            flow: flow::Flow::new(),
            trans_types: trans_types::TransTypes::new(),
            lower: lower::Lower::new(),
            register_alloc: register_alloc::RegisterAllocator::new(),
        }
    }


    /// Create SemanticAnalyzer with custom ClasspathManager (legacy compatibility)
    pub fn new_with_manager(manager: &mut crate::common::manager::ClasspathManager) -> Result<Self> {
        let classpath_entries = manager.get_classpath_entries();
        let classpath = if classpath_entries.is_empty() {
            ".".to_string()
        } else {
            classpath_entries[0].to_string_lossy().to_string()
        };

        Ok(Self {
            enter: enter::EnhancedEnter::new(&classpath),
            attr: attr::Attr::new(),
            flow: flow::Flow::new(),
            trans_types: trans_types::TransTypes::new(),
            lower: lower::Lower::new(),
            register_alloc: register_alloc::RegisterAllocator::new(),
        })
    }
    
    /// Run complete semantic analysis pipeline on AST
    /// Returns semantically analyzed and transformed AST ready for code generation
    pub fn analyze(&mut self, mut ast: Ast) -> Result<Ast> {
        eprintln!("🔍 SEMANTIC: Starting semantic analysis pipeline");
        
        // Phase 1: Enter - Build symbol tables
        ast = self.enter.process(ast)?;
        let symbol_env = self.enter.get_symbol_environment();
        
        // Phase 2: Attr - Type checking and resolution (with symbols)
        ast = self.attr.process_with_symbols(ast, Some(symbol_env))?;
        
        // Phase 3: Flow - Definite assignment analysis (with symbols)
        ast = self.flow.process_with_symbols(ast, Some(symbol_env))?;
        
        // Phase 4: TransTypes - Generic type erasure (with type info)
        ast = self.trans_types.process_with_types(ast, &self.attr.get_type_information())?;
        
        // Phase 5: Lower - Desugar syntax (with type info)
        ast = self.lower.process_with_types(ast, &self.trans_types.get_erased_types())?;
        
        eprintln!("✅ SEMANTIC: Semantic analysis pipeline complete");
        Ok(ast)
    }
    
    /// Get generic signatures stored during TransTypes phase
    pub fn get_generic_signatures(&self) -> &std::collections::HashMap<String, String> {
        &self.trans_types.generic_signatures
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
