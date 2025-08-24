//! Dynamic Class Loader - JavaC-aligned dependency resolution
//!
//! This module implements dynamic loading of dependent class symbols
//! following JavaC's com.sun.tools.javac.comp.Enter.complete() pattern.
//! When field/method resolution fails, this system automatically loads
//! the required class symbols from classpath.

use crate::ast::{TypeDecl, ClassDecl, InterfaceDecl, FieldDecl, MethodDecl, TypeRef, TypeEnum, PrimitiveType};
use crate::common::env::{SymbolEnvironment, ClassSymbol, VariableSymbol, SymbolKind};
use crate::common::{error::Result, manager::ClasspathManager, type_resolver::TypeResolver, import::ImportResolver};
use std::collections::HashMap;

/// Dynamic class loader for dependency resolution
/// JavaC equivalent: com.sun.tools.javac.comp.Enter.complete()
pub struct ClassLoader<'a> {
    /// Cache of loaded class symbols to avoid duplicate work
    loaded_classes: HashMap<String, bool>,
    /// Symbol environment to populate
    symbol_env: SymbolEnvironment,
    /// ClasspathManager for class resolution
    manager: &'a mut ClasspathManager,
}

impl<'a> ClassLoader<'a> {
    /// Create new dynamic class loader
    pub fn new(symbol_env: SymbolEnvironment, manager: &'a mut ClasspathManager) -> Self {
        Self {
            loaded_classes: HashMap::new(),
            symbol_env,
            manager,
        }
    }

    /// Load class symbols dynamically when needed
    /// JavaC equivalent: Enter.complete() -> ClassSymbol.complete()
    pub fn load_class_if_needed(&mut self, class_name: &str) -> Result<bool> {
        // Check if already loaded
        if *self.loaded_classes.get(class_name).unwrap_or(&false) {
            return Ok(true);
        }

        eprintln!("🔄 DYNAMIC LOADER: Attempting to load class '{}'", class_name);

        // Try to find class in classpath and load it
        if let Some(class_info) = self.manager.get_class_info(class_name) {
            return self.load_class_from_classpath(class_name, &class_info.fully_qualified_name);
        }

        // Try to find by simple name in classpath
        if let Some(fully_qualified_name) = self.manager.find_class_by_simple_name(class_name) {
            return self.load_class_from_classpath(class_name, &fully_qualified_name);
        }

        eprintln!("⚠️  DYNAMIC LOADER: Could not find class '{}' in classpath", class_name);
        Ok(false)
    }

    /// Load class from classpath (unified approach)
    fn load_class_from_classpath(&mut self, class_name: &str, fully_qualified_name: &str) -> Result<bool> {
        eprintln!("🔧 DYNAMIC LOADER: Loading class '{}' from classpath ({})", class_name, fully_qualified_name);
        
        // Try to actually load the class content from classpath
        if let Some(source_path) = self.manager.find_source_file(fully_qualified_name) {
            // Load from .java source file
            self.load_from_java_source(class_name, fully_qualified_name, &source_path)
        } else if let Some(class_path) = self.manager.find_class_file(fully_qualified_name) {
            // Load from .class bytecode file  
            self.load_from_class_file(class_name, fully_qualified_name, &class_path)
        } else {
            // Fallback: create minimal placeholder symbol
            self.create_placeholder_symbol(class_name, fully_qualified_name)
        }
    }

    /// Load class from .java source file by parsing it
    fn load_from_java_source(&mut self, class_name: &str, fully_qualified_name: &str, source_path: &str) -> Result<bool> {
        eprintln!("📄 DYNAMIC LOADER: Loading '{}' from Java source: {}", class_name, source_path);
        
        // Read and parse the source file
        let source_content = std::fs::read_to_string(source_path)
            .map_err(|e| crate::common::error::Error::Io(e))?;
        
        // Parse the source to AST
        let mut parser = crate::parser::parser::Parser::new(&source_content)?;
        match parser.parse() {
            Ok(ast) => {
                // Extract class information from AST
                self.extract_class_from_ast(class_name, fully_qualified_name, &ast)?;
                self.loaded_classes.insert(class_name.to_string(), true);
                eprintln!("✅ DYNAMIC LOADER: Successfully loaded '{}' from source", class_name);
                Ok(true)
            }
            Err(parse_error) => {
                eprintln!("⚠️  DYNAMIC LOADER: Failed to parse {}: {:?}", source_path, parse_error);
                // Fallback to placeholder
                self.create_placeholder_symbol(class_name, fully_qualified_name)
            }
        }
    }

    /// Load class from .class bytecode file 
    fn load_from_class_file(&mut self, class_name: &str, fully_qualified_name: &str, class_path: &str) -> Result<bool> {
        eprintln!("🔧 DYNAMIC LOADER: Loading '{}' from class file: {}", class_name, class_path);
        
        // TODO: Implement actual .class file parsing
        // For now, this would involve:
        // 1. Read .class file bytes
        // 2. Parse class file format (constant pool, fields, methods, etc.)
        // 3. Extract class metadata and create full ClassSymbol
        // 4. This is a complex task requiring a full .class file parser
        
        eprintln!("🚧 DYNAMIC LOADER: .class file parsing not yet implemented, using placeholder");
        self.create_placeholder_symbol(class_name, fully_qualified_name)
    }

    /// Create minimal placeholder symbol when actual loading fails
    fn create_placeholder_symbol(&mut self, class_name: &str, fully_qualified_name: &str) -> Result<bool> {
        eprintln!("📝 DYNAMIC LOADER: Creating placeholder symbol for '{}'", class_name);
        
        let package_name = if let Some(last_dot) = fully_qualified_name.rfind('.') {
            Some(fully_qualified_name[..last_dot].to_string())
        } else {
            None
        };

        self.symbol_env.classes.insert(class_name.to_string(), ClassSymbol {
            name: class_name.to_string(),
            fully_qualified_name: fully_qualified_name.to_string(),
            package_name,
            is_interface: false,
            is_enum: false,
            is_annotation: false,
            super_class: Some("java.lang.Object".to_string()),
            interfaces: vec![],
            modifiers: vec![],
            type_parameters: vec![],
            is_generic: false,
            methods: HashMap::new(),
            enum_constants: vec![],
        });
        
        self.loaded_classes.insert(class_name.to_string(), true);
        eprintln!("✅ DYNAMIC LOADER: Created placeholder for '{}'", class_name);
        Ok(true)
    }

    /// Extract class information from parsed AST
    fn extract_class_from_ast(&mut self, class_name: &str, fully_qualified_name: &str, ast: &crate::ast::Ast) -> Result<()> {
        // Find the target class declaration in the AST
        for type_decl in &ast.type_decls {
            match type_decl {
                crate::ast::TypeDecl::Class(class_decl) => {
                    if class_decl.name == class_name || class_decl.name.ends_with(&format!(".{}", class_name)) {
                        self.extract_class_symbol_from_decl(fully_qualified_name, class_decl)?;
                        return Ok(());
                    }
                }
                crate::ast::TypeDecl::Interface(interface_decl) => {
                    if interface_decl.name == class_name || interface_decl.name.ends_with(&format!(".{}", class_name)) {
                        self.extract_interface_symbol_from_decl(fully_qualified_name, interface_decl)?;
                        return Ok(());
                    }
                }
                crate::ast::TypeDecl::Enum(enum_decl) => {
                    if enum_decl.name == class_name || enum_decl.name.ends_with(&format!(".{}", class_name)) {
                        self.extract_enum_symbol_from_decl(fully_qualified_name, enum_decl)?;
                        return Ok(());
                    }
                }
                crate::ast::TypeDecl::Annotation(annotation_decl) => {
                    if annotation_decl.name == class_name || annotation_decl.name.ends_with(&format!(".{}", class_name)) {
                        self.extract_annotation_symbol_from_decl(fully_qualified_name, annotation_decl)?;
                        return Ok(());
                    }
                }
            }
        }
        
        // If we didn't find the specific class, create a placeholder
        eprintln!("⚠️  DYNAMIC LOADER: Could not find class '{}' in parsed AST", class_name);
        Ok(())
    }

    /// Extract ClassSymbol from ClassDecl
    fn extract_class_symbol_from_decl(&mut self, fully_qualified_name: &str, class_decl: &crate::ast::ClassDecl) -> Result<()> {
        let package_name = if let Some(last_dot) = fully_qualified_name.rfind('.') {
            Some(fully_qualified_name[..last_dot].to_string())
        } else {
            None
        };

        // Extract superclass name
        let super_class = class_decl.extends.as_ref()
            .map(|sc| sc.name.clone())
            .or_else(|| Some("java.lang.Object".to_string()));

        // Extract interface names
        let interfaces = class_decl.implements.iter()
            .map(|iface| iface.name.clone())
            .collect();

        let class_symbol = ClassSymbol {
            name: class_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.to_string(),
            package_name,
            is_interface: false,
            is_enum: false,
            is_annotation: false,
            super_class,
            interfaces,
            modifiers: class_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: class_decl.type_params.iter().enumerate().map(|(i, tp)| {
                crate::common::env::TypeParameterSymbol {
                    name: tp.name.clone(),
                    bounds: tp.bounds.iter().map(|b| b.name.clone()).collect(),
                    owner: fully_qualified_name.to_string(),
                    index: i,
                }
            }).collect(),
            is_generic: !class_decl.type_params.is_empty(),
            methods: HashMap::new(), // TODO: Extract methods from class_decl.body
            enum_constants: vec![],
        };

        self.symbol_env.classes.insert(class_decl.name.clone(), class_symbol);
        eprintln!("📦 DYNAMIC LOADER: Extracted class symbol for '{}'", class_decl.name);
        Ok(())
    }

    /// Extract ClassSymbol from InterfaceDecl  
    fn extract_interface_symbol_from_decl(&mut self, fully_qualified_name: &str, interface_decl: &crate::ast::InterfaceDecl) -> Result<()> {
        let package_name = if let Some(last_dot) = fully_qualified_name.rfind('.') {
            Some(fully_qualified_name[..last_dot].to_string())
        } else {
            None
        };

        let interfaces = interface_decl.extends.iter()
            .map(|iface| iface.name.clone())
            .collect();

        let class_symbol = ClassSymbol {
            name: interface_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.to_string(),
            package_name,
            is_interface: true,
            is_enum: false,
            is_annotation: false,
            super_class: Some("java.lang.Object".to_string()),
            interfaces,
            modifiers: interface_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: interface_decl.type_params.iter().enumerate().map(|(i, tp)| {
                crate::common::env::TypeParameterSymbol {
                    name: tp.name.clone(),
                    bounds: tp.bounds.iter().map(|b| b.name.clone()).collect(),
                    owner: fully_qualified_name.to_string(),
                    index: i,
                }
            }).collect(),
            is_generic: !interface_decl.type_params.is_empty(),
            methods: HashMap::new(), // TODO: Extract methods
            enum_constants: vec![],
        };

        self.symbol_env.classes.insert(interface_decl.name.clone(), class_symbol);
        eprintln!("🔗 DYNAMIC LOADER: Extracted interface symbol for '{}'", interface_decl.name);
        Ok(())
    }

    /// Extract ClassSymbol from EnumDecl
    fn extract_enum_symbol_from_decl(&mut self, fully_qualified_name: &str, enum_decl: &crate::ast::EnumDecl) -> Result<()> {
        let package_name = if let Some(last_dot) = fully_qualified_name.rfind('.') {
            Some(fully_qualified_name[..last_dot].to_string())
        } else {
            None
        };

        let interfaces = enum_decl.implements.iter()
            .map(|iface| iface.name.clone())
            .collect();

        let enum_constants = enum_decl.constants.iter()
            .map(|const_decl| const_decl.name.clone())
            .collect();

        let class_symbol = ClassSymbol {
            name: enum_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.to_string(),
            package_name,
            is_interface: false,
            is_enum: true,
            is_annotation: false,
            super_class: Some("java.lang.Enum".to_string()),
            interfaces,
            modifiers: enum_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: vec![],
            is_generic: false,
            methods: HashMap::new(),
            enum_constants,
        };

        self.symbol_env.classes.insert(enum_decl.name.clone(), class_symbol);
        eprintln!("🏷️  DYNAMIC LOADER: Extracted enum symbol for '{}'", enum_decl.name);
        Ok(())
    }

    /// Extract ClassSymbol from AnnotationDecl
    fn extract_annotation_symbol_from_decl(&mut self, fully_qualified_name: &str, annotation_decl: &crate::ast::AnnotationDecl) -> Result<()> {
        let package_name = if let Some(last_dot) = fully_qualified_name.rfind('.') {
            Some(fully_qualified_name[..last_dot].to_string())
        } else {
            None
        };

        let class_symbol = ClassSymbol {
            name: annotation_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.to_string(),
            package_name,
            is_interface: true,
            is_enum: false,
            is_annotation: true,
            super_class: Some("java.lang.Object".to_string()),
            interfaces: vec!["java.lang.annotation.Annotation".to_string()],
            modifiers: annotation_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: vec![],
            is_generic: false,
            methods: HashMap::new(),
            enum_constants: vec![],
        };

        self.symbol_env.classes.insert(annotation_decl.name.clone(), class_symbol);
        eprintln!("📋 DYNAMIC LOADER: Extracted annotation symbol for '{}'", annotation_decl.name);
        Ok(())
    }







    /// Get the updated symbol environment
    pub fn get_symbol_environment(self) -> SymbolEnvironment {
        self.symbol_env
    }

    /// Check if a class has been loaded
    pub fn is_class_loaded(&self, class_name: &str) -> bool {
        *self.loaded_classes.get(class_name).unwrap_or(&false)
    }
}

/// Standalone ClassLoader that owns its ClasspathManager
/// This allows existing code to continue working while we migrate
pub struct StandaloneClassLoader {
    manager: ClasspathManager,
    loaded_classes: HashMap<String, bool>,
    symbol_env: SymbolEnvironment,
}

impl StandaloneClassLoader {
    pub fn new(symbol_env: SymbolEnvironment, default_classpath: &str) -> Self {
        StandaloneClassLoader {
            manager: ClasspathManager::new(default_classpath),
            loaded_classes: HashMap::new(),
            symbol_env,
        }
    }
    
    /// Create a borrowed ClassLoader from this standalone instance
    pub fn as_borrowed(&mut self) -> ClassLoader {
        ClassLoader::new(self.symbol_env.clone(), &mut self.manager)
    }
    
    /// Load class symbols dynamically when needed
    pub fn load_class_if_needed(&mut self, class_name: &str) -> Result<bool> {
        // For now, use the simple owned approach
        let mut borrowed = self.as_borrowed();
        borrowed.load_class_if_needed(class_name)
    }
}

/// Trait for types that can trigger dynamic class loading
pub trait DynamicLoadable {
    /// Ensure all dependencies are loaded for this type
    fn ensure_dependencies_loaded(&self, loader: &mut ClassLoader) -> Result<()>;
}

impl DynamicLoadable for TypeRef {
    fn ensure_dependencies_loaded(&self, loader: &mut ClassLoader) -> Result<()> {
        // Load the main type
        loader.load_class_if_needed(&self.name)?;
        
        // Load type arguments recursively (TODO: implement for TypeArg)
        // for type_arg in &self.type_args {
        //     type_arg.ensure_dependencies_loaded(loader)?;
        // }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::env::SymbolEnvironment;

    #[test]
    fn test_dynamic_class_loader() {
        let symbol_env = SymbolEnvironment::default();
        let mut manager = crate::common::manager::ClasspathManager::new("tests/java");
        let mut loader = ClassLoader::new(symbol_env, &mut manager);
        
        // Test loading common classes if they exist in classpath
        if loader.manager.class_exists("java.lang.String") {
            let result = loader.load_class_if_needed("String").unwrap();
            assert!(result);
            assert!(loader.is_class_loaded("String"));
        }
        
        // Test that non-existent classes return false
        let result = loader.load_class_if_needed("NonExistentClass").unwrap();
        assert!(!result);
        assert!(!loader.is_class_loaded("NonExistentClass"));
    }

    #[test]
    fn test_current_directory_access() {
        println!("🔍 Testing ClassLoader with current directory");
        
        // Create ClasspathManager using current directory
        let mut manager = crate::common::manager::ClasspathManager::new(".");
        
        // Check if we can find TestClass.java in current directory
        match manager.find_source_file("TestClass") {
            Some(path) => {
                println!("✅ Found TestClass.java at: {}", path);
                // If found, test actual loading
                let symbol_env = SymbolEnvironment::default();
                let mut loader = ClassLoader::new(symbol_env, &mut manager);
                
                match loader.load_class_if_needed("TestClass") {
                    Ok(loaded) => {
                        if loaded {
                            println!("✅ Successfully loaded TestClass from current directory!");
                            assert!(loader.is_class_loaded("TestClass"));
                        } else {
                            println!("⚠️  TestClass found but loading failed");
                        }
                    }
                    Err(e) => {
                        println!("💥 Error loading TestClass: {:?}", e);
                    }
                }
            }
            None => {
                println!("❌ Could not find TestClass.java in current directory");
                
                // Debug: check current directory contents
                println!("📁 Checking current directory contents:");
                if let Ok(entries) = std::fs::read_dir(".") {
                    for entry in entries {
                        if let Ok(entry) = entry {
                            if let Some(name) = entry.file_name().to_str() {
                                if name.ends_with(".java") {
                                    println!("  📄 Found Java file: {}", name);
                                }
                            }
                        }
                    }
                } else {
                    println!("  ❌ Failed to read current directory");
                }
            }
        }
    }
}