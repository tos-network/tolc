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
use crate::common::error::Result;

// Planned split modules (incremental migration)
// mod core;
// mod gates;
// mod package;
// mod imports;
// mod modifiers;
// mod annotations;
// mod members;
// mod types;
// mod generics;
// mod params;
// mod lookahead;
// pub mod statements {
//     pub mod r#mod; // dispatcher
//     pub mod block;
//     pub mod flow;
//     pub mod try_catch;
//     pub mod switch;
//     pub mod sync;
// }
// pub mod expressions {
//     pub mod r#mod; // parse_expression
//     pub mod assignment;
//     pub mod conditional;
//     pub mod logical;
//     pub mod bitwise;
//     pub mod equality;
//     pub mod relational;
//     pub mod shift;
//     pub mod additive;
//     pub mod multiplicative;
//     pub mod unary;
//     pub mod postfix;
//     pub mod primary;
//     pub mod new_expr;
// }

/// Parse Java source code into an AST
/// 
/// This is the main entry point for parsing Java source files (.java)
/// Compatible with standard Java syntax
pub fn parse_java(source: &str) -> Result<Ast> { 
    parser::parse(source) 
}

/// Parse a .tol source file into an AST (legacy support)
pub fn parse_tol(source: &str) -> Result<Ast> { parser::parse(source) }

/// Lenient parse: produce best-effort AST even if syntax errors occurred.
pub fn parse_tol_lenient(source: &str) -> Result<Ast> { Parser::new(source)?.parse_lenient() }

/// Lenient parse for Java source
pub fn parse_java_lenient(source: &str) -> Result<Ast> { Parser::new(source)?.parse_lenient() }

/// Parse and verify a .tol source file
pub fn parse_and_verify(source: &str) -> Result<Ast> {
    let ast = parser::parse(source)?;
    crate::review::review(&ast)
        .map_err(|e| crate::common::error::Error::Semantic { message: e.to_string() })?;
    Ok(ast)
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
