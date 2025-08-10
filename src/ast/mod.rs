//! Abstract Syntax Tree (AST) representation for Terminos Language
//! 
//! This module defines the AST nodes that represent parsed .tol code.

mod nodes;
mod visitor;
mod printer;

pub use nodes::*;
pub use visitor::*;
pub use printer::*;

use std::fmt;

/// Source location information
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Location {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self { line, column, offset }
    }
}

/// Span of source code (start and end locations)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }
    
    pub fn from_to(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start: Location::new(start_line, start_col, 0),
            end: Location::new(end_line, end_col, 0),
        }
    }
}

/// AST node trait that all AST nodes implement
pub trait AstNode {
    /// Get the source span of this node
    fn span(&self) -> Span;
    
    /// Accept a visitor
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output;
}

/// Main AST root node
#[derive(Debug, Clone)]
pub struct Ast {
    pub package_decl: Option<PackageDecl>,
    pub imports: Vec<ImportDecl>,
    pub type_decls: Vec<TypeDecl>,
    pub span: Span,
}

impl AstNode for Ast {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_ast(self)
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref package) = self.package_decl {
            writeln!(f, "{}", package)?;
        }
        
        for import in &self.imports {
            writeln!(f, "{}", import)?;
        }
        
        for type_decl in &self.type_decls {
            writeln!(f, "{}", type_decl)?;
        }
        
        Ok(())
    }
}
