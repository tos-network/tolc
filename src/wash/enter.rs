//! Enter phase - Symbol table construction and import resolution
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Enter` class.
//! This phase builds symbol tables for all compilation units, establishes
//! inheritance relationships, and handles import statements.

use crate::ast::Ast;
use crate::error::Result;
use std::collections::HashMap;

/// Symbol table entry for a class or interface
#[derive(Debug, Clone)]
pub struct ClassSymbol {
    pub name: String,
    pub fully_qualified_name: String,
    pub package_name: Option<String>,
    pub is_interface: bool,
    pub super_class: Option<String>,
    pub interfaces: Vec<String>,
    pub modifiers: Vec<String>,
}

/// Global symbol environment containing all symbols
#[derive(Debug, Default)]
pub struct SymbolEnvironment {
    /// Map from class name to ClassSymbol
    pub classes: HashMap<String, ClassSymbol>,
    /// Import statements resolved to fully qualified names
    pub imports: HashMap<String, String>,
    /// Current package being processed
    pub current_package: Option<String>,
}

/// Enter phase processor - corresponds to JavaC's Enter class
pub struct Enter {
    pub symbol_env: SymbolEnvironment,
}

impl Enter {
    pub fn new() -> Self {
        Self {
            symbol_env: SymbolEnvironment::default(),
        }
    }
    
    /// Process AST through Enter phase - build symbol tables
    /// Corresponds to JavaC's Enter.main() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        eprintln!("🔍 ENTER: Starting symbol table construction");
        
        // Process package declaration
        if let Some(ref package) = ast.package_decl {
            self.symbol_env.current_package = Some(package.name.clone());
            eprintln!("📦 ENTER: Package: {}", package.name);
        }
        
        // Process import declarations
        for import in &ast.imports {
            self.process_import(import)?;
        }
        
        // Process type declarations (classes, interfaces, enums)
        for type_decl in &ast.type_decls {
            self.process_type_decl(type_decl)?;
        }
        
        eprintln!("✅ ENTER: Symbol table construction complete");
        eprintln!("📊 ENTER: {} classes, {} imports", 
                 self.symbol_env.classes.len(), 
                 self.symbol_env.imports.len());
        
        Ok(ast)
    }
    
    /// Process import declaration
    fn process_import(&mut self, import: &crate::ast::ImportDecl) -> Result<()> {
        if import.is_static {
            eprintln!("⚠️  ENTER: Static imports not yet implemented: {}", import.name);
            return Ok(());
        }
        
        if import.is_wildcard {
            eprintln!("⚠️  ENTER: Wildcard imports not yet implemented: {}.*", import.name);
            return Ok(());
        }
        
        // Single type import
        let simple_name = import.name.split('.').last().unwrap_or(&import.name);
        self.symbol_env.imports.insert(simple_name.to_string(), import.name.clone());
        eprintln!("📥 ENTER: Import: {} -> {}", simple_name, import.name);
        
        Ok(())
    }
    
    /// Process type declaration (placeholder)
    fn process_type_decl(&mut self, _type_decl: &crate::ast::TypeDecl) -> Result<()> {
        // TODO: Process actual type declarations when AST structure is finalized
        eprintln!("🏗️  ENTER: Processing type declaration (placeholder)");
        Ok(())
    }
}

impl Default for Enter {
    fn default() -> Self {
        Self::new()
    }
}