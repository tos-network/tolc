use crate::ast::Location;
use crate::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ParseError {
    /// Unexpected token encountered
    UnexpectedToken {
        expected: String,
        found: String,
        location: Location,
    },
    
    /// Unexpected end of input
    UnexpectedEndOfInput {
        expected: String,
        location: Location,
    },
    
    /// Invalid syntax
    InvalidSyntax {
        message: String,
        location: Location,
    },
    
    /// Lexical error
    LexicalError {
        message: String,
        location: Location,
    },
    
    /// Semantic error
    SemanticError {
        message: String,
        location: Location,
    },
    
    /// Multiple errors collected during parsing
    MultipleErrors {
        errors: Vec<ParseError>,
    },
}

impl ParseError {
    /// Create a new unexpected token error
    pub fn unexpected_token(expected: &str, found: &str, location: Location) -> Self {
        ParseError::UnexpectedToken {
            expected: expected.to_string(),
            found: found.to_string(),
            location,
        }
    }
    
    /// Create a new unexpected end of input error
    pub fn unexpected_end_of_input(expected: &str, location: Location) -> Self {
        ParseError::UnexpectedEndOfInput {
            expected: expected.to_string(),
            location,
        }
    }
    
    /// Create a new invalid syntax error
    pub fn invalid_syntax(message: &str, location: Location) -> Self {
        ParseError::InvalidSyntax {
            message: message.to_string(),
            location,
        }
    }
    
    /// Create a new lexical error
    pub fn lexical_error(message: &str, location: Location) -> Self {
        ParseError::LexicalError {
            message: message.to_string(),
            location,
        }
    }
    
    /// Create a new semantic error
    pub fn semantic_error(message: &str, location: Location) -> Self {
        ParseError::SemanticError {
            message: message.to_string(),
            location,
        }
    }
    
    /// Add context to an error
    pub fn with_context(self, context: &str) -> Self {
        match self {
            ParseError::UnexpectedToken { expected, found, location } => {
                ParseError::UnexpectedToken {
                    expected: format!("{} (context: {})", expected, context),
                    found,
                    location,
                }
            }
            ParseError::UnexpectedEndOfInput { expected, location } => {
                ParseError::UnexpectedEndOfInput {
                    expected: format!("{} (context: {})", expected, context),
                    location,
                }
            }
            ParseError::InvalidSyntax { message, location } => {
                ParseError::InvalidSyntax {
                    message: format!("{} (context: {})", message, context),
                    location,
                }
            }
            ParseError::LexicalError { message, location } => {
                ParseError::LexicalError {
                    message: format!("{} (context: {})", message, context),
                    location,
                }
            }
            ParseError::SemanticError { message, location } => {
                ParseError::SemanticError {
                    message: format!("{} (context: {})", message, context),
                    location,
                }
            }
            ParseError::MultipleErrors { errors } => {
                ParseError::MultipleErrors {
                    errors: errors.into_iter().map(|e| e.with_context(context)).collect(),
                }
            }
        }
    }
    
    /// Get the location of the error
    pub fn location(&self) -> Option<&Location> {
        match self {
            ParseError::UnexpectedToken { location, .. } => Some(location),
            ParseError::UnexpectedEndOfInput { location, .. } => Some(location),
            ParseError::InvalidSyntax { location, .. } => Some(location),
            ParseError::LexicalError { location, .. } => Some(location),
            ParseError::SemanticError { location, .. } => Some(location),
            ParseError::MultipleErrors { errors } => {
                errors.first().and_then(|e| e.location())
            }
        }
    }
    
    /// Check if this is a recoverable error
    pub fn is_recoverable(&self) -> bool {
        match self {
            ParseError::UnexpectedToken { .. } => true,
            ParseError::UnexpectedEndOfInput { .. } => false,
            ParseError::InvalidSyntax { .. } => true,
            ParseError::LexicalError { .. } => false,
            ParseError::SemanticError { .. } => true,
            ParseError::MultipleErrors { errors } => {
                errors.iter().all(|e| e.is_recoverable())
            }
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found, location } => {
                write!(f, "Parse error at {}:{}: expected {}, found {}", 
                    location.line, location.column, expected, found)
            }
            ParseError::UnexpectedEndOfInput { expected, location } => {
                write!(f, "Parse error at {}:{}: unexpected end of input, expected {}", 
                    location.line, location.column, expected)
            }
            ParseError::InvalidSyntax { message, location } => {
                write!(f, "Parse error at {}:{}: {}", 
                    location.line, location.column, message)
            }
            ParseError::LexicalError { message, location } => {
                write!(f, "Lexical error at {}:{}: {}", 
                    location.line, location.column, message)
            }
            ParseError::SemanticError { message, location } => {
                write!(f, "Semantic error at {}:{}: {}", 
                    location.line, location.column, message)
            }
            ParseError::MultipleErrors { errors } => {
                write!(f, "Multiple parse errors:\n")?;
                for error in errors {
                    write!(f, "  {}\n", error)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for ParseError {}

impl From<ParseError> for Error {
    fn from(parse_error: ParseError) -> Self {
        match parse_error {
            ParseError::UnexpectedToken { expected, found, location } => {
                Error::Parse {
                    line: location.line,
                    column: location.column,
                    message: format!("expected {}, found {}", expected, found),
                }
            }
            ParseError::UnexpectedEndOfInput { expected, location } => {
                Error::Parse {
                    line: location.line,
                    column: location.column,
                    message: format!("unexpected end of input, expected {}", expected),
                }
            }
            ParseError::InvalidSyntax { message, location } => {
                Error::Parse {
                    line: location.line,
                    column: location.column,
                    message,
                }
            }
            ParseError::LexicalError { message, location: _ } => {
                Error::Lexical { message }
            }
            ParseError::SemanticError { message, location: _ } => {
                Error::Semantic { message }
            }
            ParseError::MultipleErrors { errors } => {
                // Take the first error for conversion
                if let Some(first_error) = errors.first() {
                    first_error.clone().into()
                } else {
                    Error::Parse {
                        line: 0,
                        column: 0,
                        message: "multiple parse errors".to_string(),
                    }
                }
            }
        }
    }
}

/// Result type for parsing operations
pub type ParseResult<T> = Result<T, ParseError>;

/// Error recovery context
#[derive(Debug)]
pub struct ErrorRecovery {
    /// Whether to continue parsing after errors
    pub continue_on_error: bool,
    /// Maximum number of errors to collect before giving up
    pub max_errors: usize,
    /// Collected errors
    pub errors: Vec<ParseError>,
}

impl Default for ErrorRecovery {
    fn default() -> Self {
        Self {
            continue_on_error: true,
            max_errors: 100,
            errors: Vec::new(),
        }
    }
}

impl ErrorRecovery {
    /// Create a new error recovery context
    pub fn new(continue_on_error: bool, max_errors: usize) -> Self {
        Self {
            continue_on_error,
            max_errors,
            errors: Vec::new(),
        }
    }
    
    /// Add an error to the recovery context
    pub fn add_error(&mut self, error: ParseError) -> bool {
        if self.errors.len() >= self.max_errors {
            return false;
        }
        
        self.errors.push(error);
        true
    }
    
    /// Check if we should continue parsing
    pub fn should_continue(&self) -> bool {
        self.continue_on_error && self.errors.len() < self.max_errors
    }
    
    /// Get all collected errors
    pub fn into_errors(self) -> Vec<ParseError> {
        self.errors
    }
    
    /// Create a multiple errors result if there are errors
    pub fn into_result<T>(self, value: T) -> ParseResult<T> {
        if self.errors.is_empty() {
            Ok(value)
        } else if self.errors.len() == 1 {
            Err(self.errors.into_iter().next().expect("no parse errors collected"))
        } else {
            Err(ParseError::MultipleErrors { errors: self.errors })
        }
    }
}
