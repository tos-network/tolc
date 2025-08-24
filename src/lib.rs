//! Terminos Language Compiler (tolc)
//! 
//! A Java8-compatible compiler written in Rust that compiles Java source files to bytecode.
//! 
//! ## Architecture
//! 
//! The compiler follows a standard compilation pipeline:
//! 
//! - **parser**: Lexical analysis and parsing of Java source into AST
//! - **codegen**: Semantic analysis pipeline and code generation from AST to Java bytecode (.class files)
//! - **ast**: Abstract Syntax Tree representation of parsed code
//! - **bin**: Command-line interface (similar to javac)
//! 
//! ## Compilation Flow
//! 
//! ```text
//! Java Source → Parser → AST → Wash Pipeline → Code Generation → .class files
//!                         ↓
//!                    Enter → Attr → Flow → TransTypes → Lower
//! ```

pub mod ast;
pub mod parser;
pub mod codegen;
pub mod verify;
pub mod review;
pub mod common;

pub use common::{Config, Result, Error};

/// Compile Java source to bytecode without writing to files
/// 
/// This function compiles Java source code to bytecode and returns the generated
/// class data as a Vec<u8>. Useful for tests and in-memory compilation.
/// Java Source → Parser → AST → Wash Pipeline → Bytecode Generation → Vec<u8>
pub fn compile(source: &str, config: &Config) -> Result<Vec<u8>> {
    let mut manager = common::manager::ClasspathManager::new(".");
    compile_with_manager(source, config, &mut manager)
}

/// Compile Java source to bytecode with custom ClasspathManager
pub fn compile_with_manager(source: &str, config: &Config, manager: &mut common::manager::ClasspathManager) -> Result<Vec<u8>> {
    eprintln!("🔧 TOLC: Starting in-memory Java compilation");
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline (Wash)
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = codegen::SemanticAnalyzer::new_with_manager(manager)?;
    ast = semantic_analyzer.analyze(ast)?;
    eprintln!("✅ TOLC: Semantic analysis complete");
    
    // Phase 3: In-memory Bytecode Generation
    eprintln!("⚙️  TOLC: Phase 3 - In-memory bytecode generation with wash integration");
    let signatures = semantic_analyzer.get_generic_signatures();
    let type_info_raw = semantic_analyzer.attr.get_type_information();
    let symbol_env = semantic_analyzer.enter.get_symbol_environment().clone();
    
    // Convert HashMap<usize, ResolvedType> to HashMap<String, ResolvedType> for codegen
    let type_info: std::collections::HashMap<String, crate::codegen::attr::ResolvedType> = type_info_raw
        .iter()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect();
    
    let bytecode = codegen::generate_bytecode_inmemory_with_wash(&ast, config, Some(signatures), Some(type_info), Some(symbol_env))?;
    eprintln!("✅ TOLC: In-memory bytecode generation complete");
    
    eprintln!("🎉 TOLC: In-memory Java compilation finished successfully");
    Ok(bytecode)
}

/// Complete Java compilation pipeline following standard approach
/// 
/// This is the main entry point that orchestrates the entire compilation process:
/// Java Source → Parser → AST → Wash Pipeline → Code Generation → .class files
pub fn compile2file(source: &str, output_dir: &str, config: &Config) -> Result<()> {
    let mut manager = common::manager::ClasspathManager::new(".");
    compile2file_with_manager(source, output_dir, config, &mut manager)
}

/// Complete Java compilation pipeline with custom ClasspathManager
pub fn compile2file_with_manager(source: &str, output_dir: &str, config: &Config, manager: &mut common::manager::ClasspathManager) -> Result<()> {
    eprintln!("🔧 TOLC: Starting Java compilation pipeline");
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline (Wash)
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = codegen::SemanticAnalyzer::new_with_manager(manager)?;
    ast = semantic_analyzer.analyze(ast)?;
    eprintln!("✅ TOLC: Semantic analysis complete");
    
    // Phase 3: Code Generation
    eprintln!("⚙️  TOLC: Phase 3 - Bytecode generation with wash integration");
    let signatures = semantic_analyzer.get_generic_signatures();
    let type_info_raw = semantic_analyzer.attr.get_type_information();
    let symbol_env = semantic_analyzer.enter.get_symbol_environment().clone();
    
    // Get semantic type mapping (meaningful names) for codegen  
    let semantic_types = semantic_analyzer.attr.get_semantic_types();
    let type_info: std::collections::HashMap<String, crate::codegen::attr::ResolvedType> = semantic_types
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    
    // Debug: Print semantic type info being passed to codegen
    eprintln!("🔍 DEBUG: Semantic type info being passed to codegen ({} entries):", type_info.len());
    for (key, resolved_type) in &type_info {
        eprintln!("  '{}' -> {:?}", key, resolved_type);
    }
    
    codegen::generate_bytecode_with_wash(&ast, output_dir, config, Some(signatures), Some(type_info), Some(symbol_env))?;
    eprintln!("✅ TOLC: Bytecode generation complete");
    
    eprintln!("🎉 TOLC: Java compilation pipeline finished successfully");
    Ok(())
}

/// Compile a Java source file to bytecode
pub fn compile_file(input_path: &str, output_dir: &str, config: &Config) -> Result<()> {
    let mut manager = common::manager::ClasspathManager::new(".");
    compile_file_with_manager(input_path, output_dir, config, &mut manager)
}

/// Compile a Java source file to bytecode with custom ClasspathManager
pub fn compile_file_with_manager(input_path: &str, output_dir: &str, config: &Config, manager: &mut common::manager::ClasspathManager) -> Result<()> {
    eprintln!("📂 TOLC: Compiling file: {}", input_path);
    
    let source = std::fs::read_to_string(input_path)?;
    
    compile2file_with_manager(&source, output_dir, config, manager)
}

/// Compile multiple Java source files
pub fn compile_files(input_paths: &[String], output_dir: &str, config: &Config) -> Result<()> {
    let mut manager = common::manager::ClasspathManager::new(".");
    compile_files_with_manager(input_paths, output_dir, config, &mut manager)
}

/// Compile multiple Java source files with custom ClasspathManager
pub fn compile_files_with_manager(input_paths: &[String], output_dir: &str, config: &Config, manager: &mut common::manager::ClasspathManager) -> Result<()> {
    eprintln!("📁 TOLC: Compiling {} files", input_paths.len());
    
    for (index, input_path) in input_paths.iter().enumerate() {
        eprintln!("🔄 TOLC: [{}/{}] Compiling {}", index + 1, input_paths.len(), input_path);
        compile_file_with_manager(input_path, output_dir, config, manager)?;
    }
    
    eprintln!("🏁 TOLC: All files compiled successfully");
    Ok(())
}

/// Legacy compatibility function - compile .tol files
/// 
/// This maintains backward compatibility with existing .tol file compilation
/// while transitioning to full Java source support
pub fn compile_tol_file(input_path: &str, output_dir: &str, config: &Config) -> Result<()> {
    eprintln!("⚠️  TOLC: Legacy .tol compilation - use compile_file() for Java sources");
    
    let source = std::fs::read_to_string(input_path)?;
    let ast = parser::parse_tol(&source)?;
    
    // Skip wash pipeline for legacy .tol files for now
    codegen::generate_bytecode(&ast, output_dir, config, None)?;
    
    Ok(())
}

/// Legacy compatibility function - compile multiple .tol files
pub fn compile_tol_files(input_paths: &[String], output_dir: &str, config: &Config) -> Result<()> {
    for input_path in input_paths {
        compile_tol_file(input_path, output_dir, config)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_basic_compilation() {
        // Basic test to ensure the library compiles
        assert!(true);
    }

    #[test]
    fn test_complete_compilation_pipeline() {
        let source = r#"
package com.example;

public class TestCompilation {
    private int value;
    
    public TestCompilation(int value) {
        this.value = value;
    }
    
    public int getValue() {
        return value;
    }
    
    public void setValue(int value) {
        this.value = value;
    }
}
"#;
        
        let config = Config::default();
        
        // Test in-memory compilation with complete pipeline
        let result = compile(source, &config);
        assert!(result.is_ok(), "Complete pipeline compilation failed: {:?}", result.err());
        
        // Test bytecode is generated
        let bytecode = result.unwrap();
        assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
        assert!(bytecode.len() > 100, "Bytecode should be substantial (>100 bytes)");
        
        // Verify bytecode starts with Java class file magic number (0xCAFEBABE)
        assert_eq!(bytecode[0..4], [0xCA, 0xFE, 0xBA, 0xBE], "Bytecode should start with Java class file magic");
    }

    #[test]
    fn test_compile_with_custom_manager() {
        use tempfile::TempDir;
        use std::fs;
        
        // Create temporary directory with test files
        let temp_dir = TempDir::new().unwrap();
        let java_lang = temp_dir.path().join("java/lang");
        fs::create_dir_all(&java_lang).unwrap();
        fs::write(java_lang.join("Object.java"), "package java.lang; public class Object {}").unwrap();
        fs::write(java_lang.join("String.java"), "package java.lang; public class String {}").unwrap();
        
        let source = r#"
public class CustomTest {
    public static void main(String[] args) {
        System.out.println("Testing custom classpath");
    }
}
"#;

        let config = Config::default();
        let mut manager = common::manager::ClasspathManager::new(&temp_dir.path().to_string_lossy());
        
        // Test compilation with custom ClasspathManager
        let result = compile_with_manager(source, &config, &mut manager);
        assert!(result.is_ok(), "Custom manager compilation failed: {:?}", result.err());
        
        let bytecode = result.unwrap();
        assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
        assert_eq!(bytecode[0..4], [0xCA, 0xFE, 0xBA, 0xBE], "Bytecode should start with Java class file magic");
    }
}
