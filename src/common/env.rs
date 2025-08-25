//! Symbol Environment - JavaC-aligned symbol table system
//!
//! This module contains the core symbol table structures following JavaC's
//! symbol system design. These symbols track types, methods, variables, and
//! their relationships across the compilation process.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use crate::common::class_manager::ClassManager;

/// Symbol kinds matching JavaC's Kinds.java
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    /// Variables (local variables, parameters) - JavaC VAR kind
    Variable,
    /// Instance and static fields - JavaC VAR kind with class owner
    Field,
    /// Methods and constructors - JavaC MTH kind  
    Method,
    /// Types (classes, interfaces, enums) - JavaC TYP kind
    Type,
}

/// Variable symbol following JavaC's VarSymbol pattern
#[derive(Debug, Clone)]
pub struct VariableSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub owner: String,          // Method or class that declares this (e.g., "method:main", "class:MyClass")
    pub var_type: String,       // Resolved type name
    pub is_static: bool,        // For fields - whether it's static
    pub is_parameter: bool,     // For variables - whether it's a method parameter
    pub local_slot: Option<usize>, // For local variables - JVM local slot number
    pub modifiers: Vec<String>, // Access modifiers (public, private, etc.)
}

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

/// Symbol table entry for a class, interface, enum, or annotation
#[derive(Debug, Clone)]
pub struct ClassSymbol {
    pub name: String,
    pub fully_qualified_name: String,
    pub package_name: Option<String>,
    pub is_interface: bool,
    pub is_enum: bool,
    pub is_annotation: bool,
    pub super_class: Option<String>,
    pub interfaces: Vec<String>,
    pub modifiers: Vec<String>,
    /// Generic type parameters declared on this class
    pub type_parameters: Vec<TypeParameterSymbol>,
    /// Whether this class is generic
    pub is_generic: bool,
    /// Methods declared in this class (including generic methods)
    pub methods: HashMap<String, MethodSymbol>,
    /// Enum constants (only for enums)
    pub enum_constants: Vec<String>,
}

/// Global symbol environment containing all symbols
#[derive(Clone)]
pub struct SymbolEnvironment {
    /// Map from class name to ClassSymbol
    pub classes: HashMap<String, ClassSymbol>,
    /// Import statements resolved to fully qualified names (single-type imports)
    pub imports: HashMap<String, String>,
    /// Wildcard imports - list of package names for on-demand imports
    pub wildcard_imports: Vec<String>,
    /// Static imports - map from simple name to fully qualified name
    pub static_imports: HashMap<String, String>,
    /// Static wildcard imports - list of class names for static on-demand imports
    pub static_wildcard_imports: Vec<String>,
    /// Current package being processed
    pub current_package: Option<String>,
    /// Map from type parameter name to its symbol (scoped by owner)
    pub type_parameters: HashMap<String, TypeParameterSymbol>,
    /// Map from method signature to MethodSymbol
    pub methods: HashMap<String, MethodSymbol>,
    /// Variable symbols - local variables and parameters (scoped by owner method)
    /// Key format: "method:owner::varname" (e.g., "method:main::x", "method:foo::args")
    pub variables: HashMap<String, VariableSymbol>,
    /// Field symbols - instance and static fields (scoped by owner class)
    /// Key format: "class:owner::fieldname" (e.g., "class:MyClass::next", "class:System::out")
    pub fields: HashMap<String, VariableSymbol>,
    /// Generic instantiation cache (T -> String, T -> Integer, etc.)
    pub instantiation_cache: HashMap<String, Vec<String>>,
    /// Shared ClassManager for centralized classpath management
    /// This replaces multiple independent ClassManager instances across the compilation process
    pub classpath_manager: Option<Arc<Mutex<ClassManager>>>,
}

impl std::fmt::Debug for SymbolEnvironment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SymbolEnvironment")
            .field("classes", &self.classes)
            .field("imports", &self.imports)
            .field("wildcard_imports", &self.wildcard_imports)
            .field("static_imports", &self.static_imports)
            .field("static_wildcard_imports", &self.static_wildcard_imports)
            .field("current_package", &self.current_package)
            .field("type_parameters", &self.type_parameters)
            .field("methods", &self.methods)
            .field("variables", &self.variables)
            .field("fields", &self.fields)
            .field("instantiation_cache", &self.instantiation_cache)
            .field("classpath_manager", &"<ClassManager>") // Don't try to debug the ClassManager
            .finish()
    }
}

impl Default for SymbolEnvironment {
    fn default() -> Self {
        Self {
            classes: HashMap::new(),
            imports: HashMap::new(),
            wildcard_imports: Vec::new(),
            static_imports: HashMap::new(),
            static_wildcard_imports: Vec::new(),
            current_package: None,
            type_parameters: HashMap::new(),
            methods: HashMap::new(),
            variables: HashMap::new(),
            fields: HashMap::new(),
            instantiation_cache: HashMap::new(),
            classpath_manager: None,
        }
    }
}

impl SymbolEnvironment {
    /// Create SymbolEnvironment with shared ClassManager
    /// This ensures all type resolution operations use the same classpath instance
    pub fn with_classpath(classpath: &str) -> Self {
        let manager = Arc::new(Mutex::new(ClassManager::with_classpath(classpath)));
        
        Self {
            classes: HashMap::new(),
            imports: HashMap::new(),
            wildcard_imports: Vec::new(),
            static_imports: HashMap::new(),
            static_wildcard_imports: Vec::new(),
            current_package: None,
            type_parameters: HashMap::new(),
            methods: HashMap::new(),
            variables: HashMap::new(),
            fields: HashMap::new(),
            instantiation_cache: HashMap::new(),
            classpath_manager: Some(manager),
        }
    }
    
    /// Get reference to the shared ClassManager
    /// Returns None if no ClassManager was configured
    pub fn get_classpath_manager(&self) -> Option<Arc<Mutex<ClassManager>>> {
        self.classpath_manager.clone()
    }
    
    /// Create a new SymbolEnvironment with an existing ClassManager
    /// Useful for sharing the same manager across multiple compilation units
    pub fn with_shared_classpath_manager(manager: Arc<Mutex<ClassManager>>) -> Self {
        Self {
            classes: HashMap::new(),
            imports: HashMap::new(),
            wildcard_imports: Vec::new(),
            static_imports: HashMap::new(),
            static_wildcard_imports: Vec::new(),
            current_package: None,
            type_parameters: HashMap::new(),
            methods: HashMap::new(),
            variables: HashMap::new(),
            fields: HashMap::new(),
            instantiation_cache: HashMap::new(),
            classpath_manager: Some(manager),
        }
    }
    /// Resolve simple type name to fully qualified name
    /// Follows Java name resolution rules: single import -> current package -> wildcard import -> java.lang
    pub fn resolve_type(&self, simple_name: &str) -> Option<String> {
        // 1. Single-type import
        if let Some(qualified) = self.imports.get(simple_name) {
            eprintln!("🔍 RESOLVE: {} -> {} (single import)", simple_name, qualified);
            return Some(qualified.clone());
        }
        
        // 2. Current package classes
        if let Some(ref pkg) = self.current_package {
            let qualified = format!("{}.{}", pkg, simple_name);
            if self.classes.contains_key(&qualified) {
                eprintln!("🔍 RESOLVE: {} -> {} (current package)", simple_name, qualified);
                return Some(qualified);
            }
        }
        
        // 3. Type-import-on-demand (wildcard imports)
        for package in &self.wildcard_imports {
            let qualified = format!("{}.{}", package, simple_name);
            if self.classes.contains_key(&qualified) {
                eprintln!("🔍 RESOLVE: {} -> {} (wildcard import)", simple_name, qualified);
                return Some(qualified);
            }
        }
        
        // 4. java.lang package (implicit import)
        let java_lang_qualified = format!("java.lang.{}", simple_name);
        if self.classes.contains_key(&java_lang_qualified) || is_java_lang_type(simple_name) {
            eprintln!("🔍 RESOLVE: {} -> {} (java.lang)", simple_name, java_lang_qualified);
            return Some(java_lang_qualified);
        }
        
        // 5. Not found
        eprintln!("❌ RESOLVE: {} -> NOT FOUND", simple_name);
        None
    }
    
    /// Register a class symbol in the environment
    pub fn register_class(&mut self, class_name: String, symbol: ClassSymbol) {
        eprintln!("📝 REGISTER_CLASS: {} -> {}", class_name, symbol.fully_qualified_name);
        self.classes.insert(class_name, symbol);
    }
    
    /// Register a variable or field symbol
    pub fn register_variable(&mut self, key: String, symbol: VariableSymbol) {
        match symbol.kind {
            SymbolKind::Field => {
                eprintln!("📝 REGISTER_FIELD: {} ({})", key, symbol.var_type);
                self.fields.insert(key, symbol);
            }
            SymbolKind::Variable => {
                eprintln!("📝 REGISTER_VARIABLE: {} ({})", key, symbol.var_type);
                self.variables.insert(key, symbol);
            }
            _ => {
                eprintln!("⚠️  REGISTER_VARIABLE: Unexpected symbol kind {:?}", symbol.kind);
            }
        }
    }
    
    /// Register a method symbol
    pub fn register_method(&mut self, key: String, symbol: MethodSymbol) {
        eprintln!("📝 REGISTER_METHOD: {} -> {}({})", key, symbol.return_type, symbol.parameter_types.join(", "));
        self.methods.insert(key, symbol);
    }
    
    /// Look up a class by name
    pub fn lookup_class(&self, name: &str) -> Option<&ClassSymbol> {
        self.classes.get(name)
    }
    
    /// Look up a variable (includes both local variables and fields)
    pub fn lookup_variable(&self, key: &str) -> Option<&VariableSymbol> {
        self.variables.get(key).or_else(|| self.fields.get(key))
    }
    
    /// Look up a method by signature
    pub fn lookup_method(&self, key: &str) -> Option<&MethodSymbol> {
        self.methods.get(key)
    }
    
    /// Add single-type import
    pub fn add_import(&mut self, simple_name: String, qualified_name: String) {
        eprintln!("📥 IMPORT: {} -> {}", simple_name, qualified_name);
        self.imports.insert(simple_name, qualified_name);
    }
    
    /// Add wildcard import
    pub fn add_wildcard_import(&mut self, package: String) {
        eprintln!("📥 WILDCARD_IMPORT: {}.*", package);
        if !self.wildcard_imports.contains(&package) {
            self.wildcard_imports.push(package);
        }
    }
    
    /// Set current package
    pub fn set_current_package(&mut self, package: Option<String>) {
        if let Some(ref pkg) = package {
            eprintln!("📦 CURRENT_PACKAGE: {}", pkg);
        } else {
            eprintln!("📦 CURRENT_PACKAGE: <default>");
        }
        self.current_package = package;
    }
    
    /// Legacy method for compatibility - resolve identifier with context
    pub fn resolve_identifier(&self, name: &str, method_context: Option<&str>, class_context: &str) -> Option<VariableSymbol> {
        // Try method-scoped variables first
        if let Some(method_ctx) = method_context {
            let variable_key = format!("method:{}::{}", method_ctx, name);
            if let Some(variable_symbol) = self.variables.get(&variable_key) {
                return Some(variable_symbol.clone());
            }
        }
        
        // Try class fields
        let field_key = format!("class:{}::{}", class_context, name);
        if let Some(field_symbol) = self.fields.get(&field_key) {
            return Some(field_symbol.clone());
        }
        
        None
    }
    
    /// Legacy method for compatibility - add variable to symbol environment
    pub fn add_variable(&mut self, name: String, var_type: String, method_context: String, is_parameter: bool, local_slot: Option<usize>) {
        let variable_key = format!("method:{}::{}", method_context, name);
        let symbol = VariableSymbol {
            name: name.clone(),
            kind: SymbolKind::Variable,
            owner: format!("method:{}", method_context),
            var_type,
            is_static: false,
            is_parameter,
            local_slot,
            modifiers: vec![],
        };
        
        eprintln!("📝 ADD_VARIABLE: {} -> {} (slot: {:?})", variable_key, symbol.var_type, local_slot);
        self.variables.insert(variable_key, symbol);
    }
}

/// Check if a simple name is a known java.lang type
fn is_java_lang_type(simple_name: &str) -> bool {
    matches!(simple_name, 
        "Object" | "String" | "Class" | "System" | 
        "Integer" | "Long" | "Double" | "Float" | "Boolean" | "Byte" | "Short" | "Character" |
        "Number" | "Math" | "Thread" | "Exception" | "RuntimeException" |
        "IllegalArgumentException" | "NullPointerException" | "IndexOutOfBoundsException" |
        "Throwable" | "Error" | "OutOfMemoryError" | "StackOverflowError"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_environment_creation() {
        let env = SymbolEnvironment::default();
        assert!(env.classes.is_empty());
        assert!(env.imports.is_empty());
        assert!(env.wildcard_imports.is_empty());
    }

    #[test]
    fn test_class_registration() {
        let mut env = SymbolEnvironment::default();
        let class_symbol = ClassSymbol {
            name: "TestClass".to_string(),
            fully_qualified_name: "com.example.TestClass".to_string(),
            package_name: Some("com.example".to_string()),
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
        };
        
        env.register_class("com.example.TestClass".to_string(), class_symbol);
        assert!(env.classes.contains_key("com.example.TestClass"));
    }
    
    #[test]
    fn test_type_resolution() {
        let mut env = SymbolEnvironment::default();
        
        // Test java.lang resolution
        assert_eq!(env.resolve_type("String"), Some("java.lang.String".to_string()));
        assert_eq!(env.resolve_type("Object"), Some("java.lang.Object".to_string()));
        
        // Test single import
        env.add_import("List".to_string(), "java.util.List".to_string());
        assert_eq!(env.resolve_type("List"), Some("java.util.List".to_string()));
    }
}