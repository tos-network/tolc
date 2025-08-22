//! Terminos Language Compiler (tolc)
//! 
//! A Java8-compatible compiler written in Rust that compiles Java source files to bytecode.
//! 
//! ## Architecture
//! 
//! The compiler follows the JavaC compilation pipeline:
//! 
//! - **parser**: Lexical analysis and parsing of Java source into AST
//! - **wash**: Semantic analysis pipeline (Enter → Attr → Flow → TransTypes → Lower)
//! - **codegen**: Code generation from AST to Java bytecode (.class files)
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
pub mod wash;
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
    eprintln!("🔧 TOLC: Starting in-memory Java compilation");
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline (Wash)
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = wash::SemanticAnalyzer::new();
    ast = semantic_analyzer.analyze(ast)?;
    eprintln!("✅ TOLC: Semantic analysis complete");
    
    // Phase 3: In-memory Bytecode Generation
    eprintln!("⚙️  TOLC: Phase 3 - In-memory bytecode generation with wash integration");
    let signatures = semantic_analyzer.get_generic_signatures();
    let type_info_raw = semantic_analyzer.attr.get_type_information();
    let symbol_env = semantic_analyzer.enter.get_symbol_environment().clone();
    
    // Convert HashMap<usize, ResolvedType> to HashMap<String, ResolvedType> for codegen
    let type_info: std::collections::HashMap<String, crate::wash::attr::ResolvedType> = type_info_raw
        .iter()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect();
    
    let bytecode = codegen::generate_bytecode_inmemory_with_wash(&ast, config, Some(signatures), Some(type_info), Some(symbol_env))?;
    eprintln!("✅ TOLC: In-memory bytecode generation complete");
    
    eprintln!("🎉 TOLC: In-memory Java compilation finished successfully");
    Ok(bytecode)
}

/// Complete Java compilation pipeline following JavaC's approach
/// 
/// This is the main entry point that orchestrates the entire compilation process:
/// Java Source → Parser → AST → Wash Pipeline → Code Generation → .class files
pub fn compile2file(source: &str, output_dir: &str, config: &Config) -> Result<()> {
    eprintln!("🔧 TOLC: Starting Java compilation pipeline");
    
    // Phase 1: Lexical Analysis & Parsing
    eprintln!("📝 TOLC: Phase 1 - Parsing Java source");
    let mut ast = parser::parse_java(&source)?;
    eprintln!("✅ TOLC: Parsing complete - AST generated");
    
    // Phase 2: Semantic Analysis Pipeline (Wash)
    eprintln!("🧠 TOLC: Phase 2 - Semantic analysis pipeline");
    let mut semantic_analyzer = wash::SemanticAnalyzer::new();
    ast = semantic_analyzer.analyze(ast)?;
    eprintln!("✅ TOLC: Semantic analysis complete");
    
    // Phase 3: Code Generation
    eprintln!("⚙️  TOLC: Phase 3 - Bytecode generation with wash integration");
    let signatures = semantic_analyzer.get_generic_signatures();
    let type_info_raw = semantic_analyzer.attr.get_type_information();
    let symbol_env = semantic_analyzer.enter.get_symbol_environment().clone();
    
    // Convert HashMap<usize, ResolvedType> to HashMap<String, ResolvedType> for codegen
    let type_info: std::collections::HashMap<String, crate::wash::attr::ResolvedType> = type_info_raw
        .iter()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect();
    codegen::generate_bytecode_with_wash(&ast, output_dir, config, Some(signatures), Some(type_info), Some(symbol_env))?;
    eprintln!("✅ TOLC: Bytecode generation complete");
    
    eprintln!("🎉 TOLC: Java compilation pipeline finished successfully");
    Ok(())
}

/// Compile a Java source file to bytecode
pub fn compile_file(input_path: &str, output_dir: &str, config: &Config) -> Result<()> {
    eprintln!("📂 TOLC: Compiling file: {}", input_path);
    
    let source = std::fs::read_to_string(input_path)?;
    
    compile2file(&source, output_dir, config)
}

/// Compile multiple Java source files
pub fn compile_files(input_paths: &[String], output_dir: &str, config: &Config) -> Result<()> {
    eprintln!("📁 TOLC: Compiling {} files", input_paths.len());
    
    for (index, input_path) in input_paths.iter().enumerate() {
        eprintln!("🔄 TOLC: [{}/{}] Compiling {}", index + 1, input_paths.len(), input_path);
        compile_file(input_path, output_dir, config)?;
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


    #[test]
    fn test_basic_compilation() {
        // Basic test to ensure the library compiles
        assert!(true);
    }
}
