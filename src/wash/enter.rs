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
    
    /// Process type declaration - corresponds to JavaC's visitClassDef
    fn process_type_decl(&mut self, type_decl: &crate::ast::TypeDecl) -> Result<()> {
        use crate::ast::TypeDecl;
        
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.process_class_decl(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                self.process_interface_decl(interface_decl)?;
            }
            TypeDecl::Enum(enum_decl) => {
                self.process_enum_decl(enum_decl)?;
            }
            TypeDecl::Annotation(annotation_decl) => {
                self.process_annotation_decl(annotation_decl)?;
            }
        }
        
        Ok(())
    }
    
    /// Process class declaration - create ClassSymbol and enter into symbol table
    fn process_class_decl(&mut self, class_decl: &crate::ast::ClassDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, class_decl.name)
        } else {
            class_decl.name.clone()
        };
        
        let super_class = class_decl.extends.as_ref().map(|t| t.name.clone());
        let interfaces: Vec<String> = class_decl.implements.iter()
            .map(|t| t.name.clone())
            .collect();
        
        let modifiers: Vec<String> = class_decl.modifiers.iter()
            .map(|m| format!("{:?}", m))
            .collect();
        
        let class_symbol = ClassSymbol {
            name: class_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: false,
            super_class,
            interfaces,
            modifiers,
        };
        
        self.symbol_env.classes.insert(class_decl.name.clone(), class_symbol);
        eprintln!("🏛️  ENTER: Class: {} -> {}", class_decl.name, fully_qualified_name);
        
        // Process nested types (classes, interfaces, etc.)
        for member in &class_decl.body {
            if let crate::ast::ClassMember::TypeDecl(type_decl) = member {
                self.process_type_decl(type_decl)?;
            }
        }
        
        Ok(())
    }
    
    /// Process interface declaration
    fn process_interface_decl(&mut self, interface_decl: &crate::ast::InterfaceDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, interface_decl.name)
        } else {
            interface_decl.name.clone()
        };
        
        let interfaces: Vec<String> = interface_decl.extends.iter()
            .map(|t| t.name.clone())
            .collect();
        
        let modifiers: Vec<String> = interface_decl.modifiers.iter()
            .map(|m| format!("{:?}", m))
            .collect();
        
        let class_symbol = ClassSymbol {
            name: interface_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: true,
            super_class: None, // Interfaces don't have super classes
            interfaces,
            modifiers,
        };
        
        self.symbol_env.classes.insert(interface_decl.name.clone(), class_symbol);
        eprintln!("🔌 ENTER: Interface: {} -> {}", interface_decl.name, fully_qualified_name);
        
        Ok(())
    }
    
    /// Process enum declaration
    fn process_enum_decl(&mut self, enum_decl: &crate::ast::EnumDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, enum_decl.name)
        } else {
            enum_decl.name.clone()
        };
        
        let interfaces: Vec<String> = enum_decl.implements.iter()
            .map(|t| t.name.clone())
            .collect();
        
        let modifiers: Vec<String> = enum_decl.modifiers.iter()
            .map(|m| format!("{:?}", m))
            .collect();
        
        let class_symbol = ClassSymbol {
            name: enum_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: false,
            super_class: Some("java.lang.Enum".to_string()), // All enums extend java.lang.Enum
            interfaces,
            modifiers,
        };
        
        self.symbol_env.classes.insert(enum_decl.name.clone(), class_symbol);
        eprintln!("🎯 ENTER: Enum: {} -> {}", enum_decl.name, fully_qualified_name);
        
        Ok(())
    }
    
    /// Process annotation declaration
    fn process_annotation_decl(&mut self, annotation_decl: &crate::ast::AnnotationDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, annotation_decl.name)
        } else {
            annotation_decl.name.clone()
        };
        
        let modifiers: Vec<String> = annotation_decl.modifiers.iter()
            .map(|m| format!("{:?}", m))
            .collect();
        
        let class_symbol = ClassSymbol {
            name: annotation_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: true, // Annotations are interfaces
            super_class: None,
            interfaces: vec!["java.lang.annotation.Annotation".to_string()],
            modifiers,
        };
        
        self.symbol_env.classes.insert(annotation_decl.name.clone(), class_symbol);
        eprintln!("📝 ENTER: Annotation: {} -> {}", annotation_decl.name, fully_qualified_name);
        
        Ok(())
    }
}

impl Default for Enter {
    fn default() -> Self {
        Self::new()
    }
}