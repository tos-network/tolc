use thiserror::Error;

/// Result type for tolc operations
pub type Result<T> = std::result::Result<T, Error>;

/// Error types for the tolc compiler
#[derive(Error, Debug)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("Parse error at line {line}, column {column}: {message}")]
    Parse {
        line: usize,
        column: usize,
        message: String,
    },
    
    #[error("Lexical error: {message}")]
    Lexical { message: String },
    
    #[error("Semantic error: {message}")]
    Semantic { message: String },
    
    #[error("Code generation error: {message}")]
    CodeGen { message: String },
    
    #[error("Invalid Java version: {version}")]
    InvalidJavaVersion { version: String },
    
    #[error("Unsupported feature: {feature}")]
    UnsupportedFeature { feature: String },
    
    #[error("Configuration error: {message}")]
    Config { message: String },
    
    #[error("Internal compiler error: {message}")]
    Internal { message: String },
}

impl Error {
    /// Create a parse error with location information
    pub fn parse_error(line: usize, column: usize, message: impl Into<String>) -> Self {
        Self::Parse {
            line,
            column,
            message: message.into(),
        }
    }
    
    /// Create a lexical error
    pub fn lexical_error(message: impl Into<String>) -> Self {
        Self::Lexical { message: message.into() }
    }
    
    /// Create a semantic error
    pub fn semantic_error(message: impl Into<String>) -> Self {
        Self::Semantic { message: message.into() }
    }
    
    /// Create a code generation error
    pub fn codegen_error(message: impl Into<String>) -> Self {
        Self::CodeGen { message: message.into() }
    }
}

// Backed up: Convert StackError to Error
// Backed up: impl From<crate::codegen::bytecode::StackError> for Error {
//     fn from(err: crate::codegen::bytecode::StackError) -> Self {
//         Self::CodeGen { 
//             message: format!("bytecode stack error: {}", err) 
//         }
//     }
// }
