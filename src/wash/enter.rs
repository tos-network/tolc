//! Enter phase - Symbol table construction and import resolution
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Enter` class.
//! This phase builds symbol tables for all compilation units, establishes
//! inheritance relationships, and handles import statements.

use crate::ast::{Ast, TypeDecl, ClassDecl, InterfaceDecl, TypeParam, TypeRef};
use crate::error::Result;
use std::collections::HashMap;

/// Type parameter symbol for generic types
#[derive(Debug, Clone)]
pub struct TypeParameterSymbol {
    pub name: String,
    pub bounds: Vec<String>,
    pub owner: String, // Class or method that declares this type parameter
    pub index: usize,  // Position in type parameter list
}

/// Method symbol for generic methods
#[derive(Debug, Clone)]
pub struct MethodSymbol {
    pub name: String,
    pub owner_class: String,
    pub type_parameters: Vec<TypeParameterSymbol>,
    pub parameter_types: Vec<String>,
    pub return_type: String,
    pub is_static: bool,
    pub is_generic: bool,
}

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
    /// Generic type parameters declared on this class
    pub type_parameters: Vec<TypeParameterSymbol>,
    /// Whether this class is generic
    pub is_generic: bool,
    /// Methods declared in this class (including generic methods)
    pub methods: HashMap<String, MethodSymbol>,
}

/// Global symbol environment containing all symbols
#[derive(Debug, Default, Clone)]
pub struct SymbolEnvironment {
    /// Map from class name to ClassSymbol
    pub classes: HashMap<String, ClassSymbol>,
    /// Import statements resolved to fully qualified names
    pub imports: HashMap<String, String>,
    /// Current package being processed
    pub current_package: Option<String>,
    /// Map from type parameter name to its symbol (scoped by owner)
    pub type_parameters: HashMap<String, TypeParameterSymbol>,
    /// Map from method signature to MethodSymbol
    pub methods: HashMap<String, MethodSymbol>,
    /// Generic instantiation cache (T -> String, T -> Integer, etc.)
    pub instantiation_cache: HashMap<String, Vec<String>>,
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
        eprintln!("📊 ENTER: {} classes, {} imports, {} type parameters", 
                 self.symbol_env.classes.len(), 
                 self.symbol_env.imports.len(),
                 self.symbol_env.type_parameters.len());
        
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
    fn process_type_decl(&mut self, type_decl: &TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.process_class_decl(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                self.process_interface_decl(interface_decl)?;
            }
            _ => {
                eprintln!("⚠️  ENTER: Type declaration not yet implemented");
            }
        }
        Ok(())
    }
    
    /// Process class declaration with generic support
    fn process_class_decl(&mut self, class_decl: &ClassDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, class_decl.name)
        } else {
            class_decl.name.clone()
        };
        
        eprintln!("🏛️  ENTER: Processing class: {}", fully_qualified_name);
        
        // Process type parameters
        let mut type_parameters = Vec::new();
        for (index, type_param) in class_decl.type_params.iter().enumerate() {
            let type_param_symbol = self.process_type_parameter(type_param, &class_decl.name, index)?;
            type_parameters.push(type_param_symbol);
        }
        
        // Determine if class is generic
        let is_generic = !class_decl.type_params.is_empty();
        
        if is_generic {
            eprintln!("🧬 ENTER: Generic class with {} type parameters", type_parameters.len());
        }
        
        // Process superclass
        let super_class = class_decl.extends.as_ref().map(|ext| self.resolve_type_name(ext));
        
        // Process interfaces
        let interfaces: Vec<String> = class_decl.implements.iter()
            .map(|iface| self.resolve_type_name(iface))
            .collect();
        
        // Process methods to collect generic method information
        let mut methods = HashMap::new();
        for member in &class_decl.body {
            if let crate::ast::ClassMember::Method(method_decl) = member {
                let method_symbol = self.process_method_decl(method_decl, &class_decl.name)?;
                let method_signature = format!("{}:{}", class_decl.name, method_decl.name);
                methods.insert(method_signature.clone(), method_symbol.clone());
                self.symbol_env.methods.insert(method_signature, method_symbol);
            }
        }
        
        // Create class symbol
        let class_symbol = ClassSymbol {
            name: class_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: false,
            super_class,
            interfaces,
            modifiers: class_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters,
            is_generic,
            methods,
        };
        
        // Store class symbol
        self.symbol_env.classes.insert(class_decl.name.clone(), class_symbol);
        
        eprintln!("✅ ENTER: Class {} processed", class_decl.name);
        Ok(())
    }
    
    /// Process interface declaration with generic support
    fn process_interface_decl(&mut self, interface_decl: &InterfaceDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, interface_decl.name)
        } else {
            interface_decl.name.clone()
        };
        
        eprintln!("🔌 ENTER: Processing interface: {}", fully_qualified_name);
        
        // Process type parameters
        let mut type_parameters = Vec::new();
        for (index, type_param) in interface_decl.type_params.iter().enumerate() {
            let type_param_symbol = self.process_type_parameter(type_param, &interface_decl.name, index)?;
            type_parameters.push(type_param_symbol);
        }
        
        let is_generic = !interface_decl.type_params.is_empty();
        
        // Process extended interfaces
        let interfaces: Vec<String> = interface_decl.extends.iter()
            .map(|iface| self.resolve_type_name(iface))
            .collect();
        
        // Create class symbol for interface
        let interface_symbol = ClassSymbol {
            name: interface_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: true,
            super_class: Some("java.lang.Object".to_string()),
            interfaces,
            modifiers: interface_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters,
            is_generic,
            methods: HashMap::new(),
        };
        
        self.symbol_env.classes.insert(interface_decl.name.clone(), interface_symbol);
        
        eprintln!("✅ ENTER: Interface {} processed", interface_decl.name);
        Ok(())
    }
    
    /// Process type parameter and create symbol
    fn process_type_parameter(&mut self, type_param: &TypeParam, owner: &str, index: usize) -> Result<TypeParameterSymbol> {
        let bounds: Vec<String> = type_param.bounds.iter()
            .map(|bound| self.resolve_type_name(bound))
            .collect();
        
        let symbol = TypeParameterSymbol {
            name: type_param.name.clone(),
            bounds,
            owner: owner.to_string(),
            index,
        };
        
        // Store in scoped map (owner:name -> symbol)
        let scoped_name = format!("{}:{}", owner, type_param.name);
        self.symbol_env.type_parameters.insert(scoped_name, symbol.clone());
        
        eprintln!("🧬 ENTER: Type parameter {} with {} bounds", type_param.name, symbol.bounds.len());
        
        Ok(symbol)
    }
    
    /// Process method declaration and create symbol
    fn process_method_decl(&mut self, method_decl: &crate::ast::MethodDecl, owner_class: &str) -> Result<MethodSymbol> {
        // Process method type parameters
        let mut type_parameters = Vec::new();
        for (index, type_param) in method_decl.type_params.iter().enumerate() {
            let method_owner = format!("{}#{}", owner_class, method_decl.name);
            let type_param_symbol = self.process_type_parameter(type_param, &method_owner, index)?;
            type_parameters.push(type_param_symbol);
        }
        
        let is_generic = !method_decl.type_params.is_empty();
        let is_static = method_decl.modifiers.iter()
            .any(|m| matches!(m, crate::ast::Modifier::Static));
        
        // Process parameter types
        let parameter_types: Vec<String> = method_decl.parameters.iter()
            .map(|param| self.resolve_type_name(&param.type_ref))
            .collect();
        
        // Process return type
        let return_type = method_decl.return_type.as_ref()
            .map(|rt| self.resolve_type_name(rt))
            .unwrap_or_else(|| "void".to_string());
        
        let method_symbol = MethodSymbol {
            name: method_decl.name.clone(),
            owner_class: owner_class.to_string(),
            type_parameters,
            parameter_types,
            return_type,
            is_static,
            is_generic,
        };
        
        if is_generic {
            eprintln!("🧬 ENTER: Generic method {} with {} type parameters", 
                     method_decl.name, method_symbol.type_parameters.len());
        }
        
        Ok(method_symbol)
    }
    
    /// Resolve type name from TypeRef to string representation
    fn resolve_type_name(&self, type_ref: &TypeRef) -> String {
        if type_ref.type_args.is_empty() {
            // Simple type
            type_ref.name.clone()
        } else {
            // Generic type with type arguments
            let args: Vec<String> = type_ref.type_args.iter()
                .map(|arg| self.resolve_type_arg(arg))
                .collect();
            format!("{}<{}>", type_ref.name, args.join(", "))
        }
    }
    
    /// Resolve type argument to string representation
    fn resolve_type_arg(&self, type_arg: &crate::ast::TypeArg) -> String {
        match type_arg {
            crate::ast::TypeArg::Type(type_ref) => self.resolve_type_name(type_ref),
            crate::ast::TypeArg::Wildcard(wildcard) => {
                match &wildcard.bound {
                    Some((crate::ast::BoundKind::Extends, bound)) => {
                        format!("? extends {}", self.resolve_type_name(bound))
                    }
                    Some((crate::ast::BoundKind::Super, bound)) => {
                        format!("? super {}", self.resolve_type_name(bound))
                    }
                    None => "?".to_string(),
                }
            }
        }
    }
    
    /// Get symbol environment for use by subsequent phases
    pub fn get_symbol_environment(&self) -> &SymbolEnvironment {
        &self.symbol_env
    }
}

impl Default for Enter {
    fn default() -> Self {
        Self::new()
    }
}