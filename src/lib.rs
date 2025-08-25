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

/// Get classpath from environment variable TOLC_CLASSPATH or default to current directory
/// 
/// This function checks for the TOLC_CLASSPATH environment variable first,
/// then falls back to "." (current directory) if not set.
fn get_classpath() -> String {
    std::env::var("TOLC_CLASSPATH").unwrap_or_else(|_| ".".to_string())
}

/// Compile Java source to bytecode without writing to files
/// 
/// This function compiles Java source code to bytecode and returns the generated
/// class data as a Vec<u8>. Useful for tests and in-memory compilation.
/// Uses JavaC-aligned classpath resolution (CLASSPATH env var or current directory).
/// Java Source → Parser → AST → Wash Pipeline → Bytecode Generation → Vec<u8>
pub fn compile(source: &str, config: &Config) -> Result<Vec<u8>> {
    eprintln!("🔧 TOLC: Starting in-memory Java compilation");
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline (uses internal ClassManager)
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = codegen::SemanticAnalyzer::new();
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

/// Compile Java source to bytecode with custom classpath
pub fn compile_with_classpath(source: &str, config: &Config, classpath: &str) -> Result<Vec<u8>> {
    eprintln!("🔧 TOLC: Starting in-memory Java compilation with classpath: {}", classpath);
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline with custom classpath
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = codegen::SemanticAnalyzer::new_with_classpath(classpath);
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
/// Uses JavaC-aligned classpath resolution (CLASSPATH env var or current directory).
/// Java Source → Parser → AST → Wash Pipeline → Code Generation → .class files
pub fn compile2file(source: &str, output_dir: &str, config: &Config) -> Result<()> {
    eprintln!("🔧 TOLC: Starting Java compilation pipeline");
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline (uses internal ClassManager)
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = codegen::SemanticAnalyzer::new();
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
    
    codegen::generate_bytecode_with_wash(&ast, output_dir, config, Some(signatures), Some(type_info), Some(symbol_env))?;
    eprintln!("✅ TOLC: Bytecode generation complete");
    
    eprintln!("🎉 TOLC: Java compilation pipeline finished successfully");
    Ok(())
}

/// Complete Java compilation pipeline with custom classpath
pub fn compile2file_with_classpath(source: &str, output_dir: &str, config: &Config, classpath: &str) -> Result<()> {
    eprintln!("🔧 TOLC: Starting Java compilation pipeline with classpath: {}", classpath);
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline with custom classpath
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = codegen::SemanticAnalyzer::new_with_classpath(classpath);
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
/// Uses JavaC-aligned classpath resolution (CLASSPATH env var or current directory).
pub fn compile_file(input_path: &str, output_dir: &str, config: &Config) -> Result<()> {
    eprintln!("📂 TOLC: Compiling file: {}", input_path);
    
    let source = std::fs::read_to_string(input_path)?;
    
    compile2file(&source, output_dir, config)
}

/// Compile a Java source file with custom classpath
pub fn compile_file_with_classpath(input_path: &str, output_dir: &str, config: &Config, classpath: &str) -> Result<()> {
    eprintln!("📂 TOLC: Compiling file: {} with classpath: {}", input_path, classpath);
    
    let source = std::fs::read_to_string(input_path)?;
    
    compile2file_with_classpath(&source, output_dir, config, classpath)
}

/// Compile multiple Java source files
/// Uses JavaC-aligned classpath resolution (CLASSPATH env var or current directory).
pub fn compile_files(input_paths: &[String], output_dir: &str, config: &Config) -> Result<()> {
    eprintln!("📁 TOLC: Compiling {} files", input_paths.len());
    
    for (index, input_path) in input_paths.iter().enumerate() {
        eprintln!("🔄 TOLC: [{}/{}] Compiling {}", index + 1, input_paths.len(), input_path);
        compile_file(input_path, output_dir, config)?;
    }
    
    eprintln!("🏁 TOLC: All files compiled successfully");
    Ok(())
}

/// Compile multiple Java source files with custom classpath
pub fn compile_files_with_classpath(input_paths: &[String], output_dir: &str, config: &Config, classpath: &str) -> Result<()> {
    eprintln!("📁 TOLC: Compiling {} files with classpath: {}", input_paths.len(), classpath);
    
    for (index, input_path) in input_paths.iter().enumerate() {
        eprintln!("🔄 TOLC: [{}/{}] Compiling {}", index + 1, input_paths.len(), input_path);
        compile_file_with_classpath(input_path, output_dir, config, classpath)?;
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

// === Backward Compatibility Functions ===
// These functions are deprecated but maintained for compatibility

#[deprecated(since = "0.2.0", note = "Use compile() instead - ClassManager is now internal")]
pub fn compile_with_manager(source: &str, config: &Config, _manager: &mut common::class_manager::ClassManager) -> Result<Vec<u8>> {
    eprintln!("⚠️  DEPRECATED: compile_with_manager() is deprecated. ClassManager is now internal to SemanticAnalyzer.");
    compile(source, config)
}

#[deprecated(since = "0.2.0", note = "Use compile2file() instead - ClassManager is now internal")]
pub fn compile2file_with_manager(source: &str, output_dir: &str, config: &Config, _manager: &mut common::class_manager::ClassManager) -> Result<()> {
    eprintln!("⚠️  DEPRECATED: compile2file_with_manager() is deprecated. ClassManager is now internal to SemanticAnalyzer.");
    compile2file(source, output_dir, config)
}

#[deprecated(since = "0.2.0", note = "Use compile_file() instead - ClassManager is now internal")]
pub fn compile_file_with_manager(input_path: &str, output_dir: &str, config: &Config, _manager: &mut common::class_manager::ClassManager) -> Result<()> {
    eprintln!("⚠️  DEPRECATED: compile_file_with_manager() is deprecated. ClassManager is now internal to SemanticAnalyzer.");
    compile_file(input_path, output_dir, config)
}

#[deprecated(since = "0.2.0", note = "Use compile_files() instead - ClassManager is now internal")]
pub fn compile_files_with_manager(input_paths: &[String], output_dir: &str, config: &Config, _manager: &mut common::class_manager::ClassManager) -> Result<()> {
    eprintln!("⚠️  DEPRECATED: compile_files_with_manager() is deprecated. ClassManager is now internal to SemanticAnalyzer.");
    compile_files(input_paths, output_dir, config)
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
    fn test_compile_with_custom_classpath() {
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
        let classpath = temp_dir.path().to_string_lossy();
        
        // Test compilation with custom classpath using new API
        let result = compile_with_classpath(source, &config, &classpath);
        assert!(result.is_ok(), "Custom classpath compilation failed: {:?}", result.err());
        
        let bytecode = result.unwrap();
        assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
        assert_eq!(bytecode[0..4], [0xCA, 0xFE, 0xBA, 0xBE], "Bytecode should start with Java class file magic");
    }

    #[test] 
    fn test_deprecated_functions() {
        let source = r#"
public class DeprecatedTest {
    public int getValue() {
        return 42;
    }
}
"#;
        let config = Config::default();
        
        // Test that deprecated functions still work but show warnings
        let mut dummy_manager = common::class_manager::ClassManager::new();
        
        // This should work but show deprecation warning
        #[allow(deprecated)]
        let result = compile_with_manager(source, &config, &mut dummy_manager);
        assert!(result.is_ok(), "Deprecated compile_with_manager failed: {:?}", result.err());
        
        let bytecode = result.unwrap();
        assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
        assert_eq!(bytecode[0..4], [0xCA, 0xFE, 0xBA, 0xBE], "Bytecode should start with Java class file magic");
    }
}
