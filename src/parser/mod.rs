//! Parser module for Terminos Language
//! 
//! This module handles lexical analysis and parsing of .tol files into AST.

pub mod lexer;
pub mod parser;
pub mod error;
pub mod span;

pub use lexer::Lexer;
pub use parser::Parser;
pub use error::{ParseError, ErrorRecovery};
pub use span::{Span, Location, HasSpan};

use crate::ast::Ast;
use crate::error::Result;

/// Parse a .tol source file into an AST
pub fn parse_tol(source: &str) -> Result<Ast> {
    parser::parse(source)
}

/// Parse multiple .tol source files
pub fn parse_tol_files(sources: &[&str]) -> Result<Vec<Ast>> {
    let mut asts = Vec::new();
    for source in sources {
        asts.push(parse_tol(source)?);
    }
    Ok(asts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_class() {
        let source = r#"
package com.example;

public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
"#;
        
        let ast = parse_tol(source).expect("Failed to parse");
        assert!(ast.type_decls.len() >= 1);
    }

    #[test]
    fn test_parse_with_imports() {
        let source = r#"
package com.example;

import java.util.List;
import java.util.ArrayList;

public class TestClass {
    private List<String> items = new ArrayList<>();
}
"#;
        
        let ast = parse_tol(source).expect("Failed to parse");
        assert_eq!(ast.imports.len(), 2);
    }
}
