//! Terminos Language Compiler (tolc)
//! 
//! A Java8-compatible compiler written in Rust that compiles .tol files to Java bytecode.
//! 
//! ## Architecture
//! 
//! - **parser**: Lexical analysis and parsing of .tol files into AST
//! - **ast**: Abstract Syntax Tree representation of parsed code
//! - **codegen**: Code generation from AST to Java bytecode (.class files)
//! - **bin**: Command-line interface (similar to solc)

pub mod ast;
pub mod parser;
pub mod codegen;
pub mod error;
pub mod config;

pub use error::{Result, Error};
pub use config::Config;

/// Compile a .tol file to Java bytecode
pub fn compile_tol_file(input_path: &str, output_dir: &str, config: &Config) -> Result<()> {
    // Parse the .tol file
    let source = std::fs::read_to_string(input_path)?;
    let ast = parser::parse_tol(&source)?;
    
    // Generate Java bytecode
    codegen::generate_bytecode(&ast, output_dir, config)?;
    
    Ok(())
}

/// Compile multiple .tol files
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
