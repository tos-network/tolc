//! Unified Type Resolution - JavaC aligned
//!
//! This module provides centralized type resolution, replacing the scattered
//! hardcoded type mappings throughout the codebase, now including enhanced
//! type resolution with JVM descriptor generation.

use crate::ast::TypeRef;
use crate::common::env::SymbolEnvironment;
use crate::common::class_manager::ClassManager;
use crate::common::import::ImportResolver;
use crate::common::consts::JAVA_LANG_SIMPLE_TYPES;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Strongly-typed type resolution result with JVM descriptor support
#[derive(Debug, Clone, PartialEq)]
pub enum TypeResolution {
    /// Primitive type with JVM descriptor
    Primitive {
        type_name: String,
        jvm_descriptor: String,
    },
    /// Reference type with full qualification
    Reference {
        class_name: String,
        jvm_descriptor: String,
        is_interface: bool,
    },
    /// Array type with proper dimension handling
    Array {
        element_type: Box<TypeResolution>,
        dimensions: usize,
        jvm_descriptor: String,
    },
    /// Generic type with type parameters
    Generic {
        base_type: Box<TypeResolution>,
        type_args: Vec<TypeResolution>,
        raw_jvm_descriptor: String, // Erased type descriptor
    },
    /// Void type
    Void,
    /// Unresolved type (error case)
    Unresolved {
        type_name: String,
        reason: String,
    },
}

impl TypeResolution {
    /// Get the JVM descriptor for this type
    pub fn get_jvm_descriptor(&self) -> &str {
        match self {
            TypeResolution::Primitive { jvm_descriptor, .. } => jvm_descriptor,
            TypeResolution::Reference { jvm_descriptor, .. } => jvm_descriptor,
            TypeResolution::Array { jvm_descriptor, .. } => jvm_descriptor,
            TypeResolution::Generic { raw_jvm_descriptor, .. } => raw_jvm_descriptor,
            TypeResolution::Void => "V",
            TypeResolution::Unresolved { .. } => "Ljava/lang/Object;", // Default fallback
        }
    }
    
    /// Get the human-readable type name
    pub fn get_type_name(&self) -> String {
        match self {
            TypeResolution::Primitive { type_name, .. } => type_name.clone(),
            TypeResolution::Reference { class_name, .. } => class_name.clone(),
            TypeResolution::Array { element_type, dimensions, .. } => {
                format!("{}{}", element_type.get_type_name(), "[]".repeat(*dimensions))
            },
            TypeResolution::Generic { base_type, type_args, .. } => {
                let args: Vec<String> = type_args.iter()
                    .map(|arg| arg.get_type_name())
                    .collect();
                format!("{}<{}>", base_type.get_type_name(), args.join(", "))
            },
            TypeResolution::Void => "void".to_string(),
            TypeResolution::Unresolved { type_name, .. } => type_name.clone(),
        }
    }
    
    /// Check if this type is assignable to another type
    pub fn is_assignable_to(&self, other: &TypeResolution) -> bool {
        match (self, other) {
            (TypeResolution::Primitive { type_name: s, .. }, TypeResolution::Primitive { type_name: t, .. }) => {
                s == t || self.is_primitive_widening_convertible(s, t)
            },
            (TypeResolution::Reference { class_name: s, .. }, TypeResolution::Reference { class_name: t, .. }) => {
                s == t || s == "null" // null is assignable to any reference type
            },
            (TypeResolution::Array { .. }, TypeResolution::Reference { class_name, .. }) => {
                class_name == "java.lang.Object" || class_name == "java.io.Serializable" || class_name == "java.lang.Cloneable"
            },
            _ => false,
        }
    }
    
    fn is_primitive_widening_convertible(&self, from: &str, to: &str) -> bool {
        match (from, to) {
            ("byte", "short" | "int" | "long" | "float" | "double") => true,
            ("short", "int" | "long" | "float" | "double") => true,
            ("char", "int" | "long" | "float" | "double") => true,
            ("int", "long" | "float" | "double") => true,
            ("long", "float" | "double") => true,
            ("float", "double") => true,
            _ => false,
        }
    }
}

pub struct TypeResolver<'a> {
    manager: &'a mut ClassManager,
    builtin_types: BuiltinTypeRegistry,
    resolution_cache: HashMap<String, Option<String>>, // simple_name -> fully_qualified_name
    type_resolution_cache: HashMap<String, TypeResolution>, // TypeRef cache
    symbol_env: Option<SymbolEnvironment>,
}

/// Owned TypeResolver that manages its own ClassManager
/// Use this for temporary type resolution that doesn't need to share the manager
pub struct OwnedTypeResolver {
    /// Reference to shared ClassManager via SymbolEnvironment
    /// This replaces the owned manager to enable cache sharing across compilation units
    symbol_env: Option<Arc<SymbolEnvironment>>,
    builtin_types: BuiltinTypeRegistry,
    resolution_cache: HashMap<String, Option<String>>,
    type_resolution_cache: HashMap<String, TypeResolution>,
}

impl<'a> TypeResolver<'a> {
    pub fn new(manager: &'a mut ClassManager) -> Self {
        TypeResolver {
            manager,
            builtin_types: BuiltinTypeRegistry::new(),
            resolution_cache: HashMap::new(),
            type_resolution_cache: HashMap::new(),
            symbol_env: None,
        }
    }
    
    /// Create with symbol environment for enhanced resolution
    pub fn with_symbol_environment(manager: &'a mut ClassManager, symbol_env: SymbolEnvironment) -> Self {
        TypeResolver {
            manager,
            builtin_types: BuiltinTypeRegistry::new(),
            resolution_cache: HashMap::new(),
            type_resolution_cache: HashMap::new(),
            symbol_env: Some(symbol_env),
        }
    }
    
    /// Resolve a simple type name to fully qualified name using import context
    pub fn resolve_type_name(
        &mut self, 
        simple_name: &str, 
        import_resolver: &mut ImportResolver
    ) -> Option<String> {
        // Check cache first
        let cache_key = format!("{}::{}", import_resolver.get_current_package(), simple_name);
        if let Some(cached_result) = self.resolution_cache.get(&cache_key) {
            return cached_result.clone();
        }
        
        let result = self.resolve_type_name_internal(simple_name, import_resolver);
        
        // Cache result
        self.resolution_cache.insert(cache_key, result.clone());
        
        if let Some(ref resolved) = result {
            eprintln!("✅ TYPE_RESOLVER: '{}' -> '{}'", simple_name, resolved);
        } else {
            eprintln!("⚠️  TYPE_RESOLVER: Failed to resolve '{}'", simple_name);
        }
        
        result
    }
    
    fn resolve_type_name_internal(
        &mut self, 
        simple_name: &str, 
        import_resolver: &mut ImportResolver
    ) -> Option<String> {
        // 1. Check if it's a primitive type
        if self.builtin_types.is_primitive(simple_name) {
            return Some(simple_name.to_string()); // Primitives are not qualified
        }
        
        // 2. Check if it's a builtin reference type
        if let Some(builtin_fqn) = self.builtin_types.get_builtin_reference_type(simple_name) {
            return Some(builtin_fqn);
        }
        
        // 3. Use import resolver to check imports
        if let Some(resolved) = import_resolver.resolve_class_name(simple_name) {
            return Some(resolved);
        }
        
        // 4. Check if it's in java.lang (implicit import)
        if JAVA_LANG_SIMPLE_TYPES.contains(&simple_name) {
            let candidate = format!("java.lang.{}", simple_name);
            if self.manager.class_exists(&candidate) {
                return Some(candidate);
            }
        }
        
        // 5. Final fallback - assume it's in the current package or unqualified
        None
    }
    
    /// Convert a simple name to internal JVM format (dots to slashes)
    pub fn resolve_to_internal_name(
        &mut self, 
        simple_name: &str, 
        import_resolver: &mut ImportResolver
    ) -> String {
        if let Some(fully_qualified) = self.resolve_type_name(simple_name, import_resolver) {
            fully_qualified.replace('.', "/")
        } else {
            // Fallback - use as-is but convert to internal format
            simple_name.replace('.', "/")
        }
    }
    
    /// Check if a type exists in the classpath
    pub fn type_exists(&mut self, fully_qualified_name: &str) -> bool {
        self.builtin_types.is_builtin(fully_qualified_name) || 
        self.manager.class_exists(fully_qualified_name)
    }
    
    /// Get the classpath manager for external use
    pub fn get_manager(&mut self) -> &mut ClassManager {
        &mut self.manager
    }
    
    /// Resolve TypeRef to strong TypeResolution with JVM descriptor
    /// This is the main enhanced resolution function integrated from enhanced_type_resolution.rs
    pub fn resolve_type_ref(&mut self, type_ref: &TypeRef) -> TypeResolution {
        // Check cache first
        let cache_key = self.create_cache_key(type_ref);
        if let Some(cached) = self.type_resolution_cache.get(&cache_key) {
            return cached.clone();
        }
        
        let resolution = self.resolve_type_ref_internal(type_ref);
        
        // Cache the result
        self.type_resolution_cache.insert(cache_key, resolution.clone());
        
        resolution
    }
    
    /// Resolve a simple type name to TypeResolution
    pub fn resolve_simple_type(&mut self, type_name: &str) -> TypeResolution {
        self.resolve_simple_type_internal(type_name)
    }
    
    /// Internal type resolution
    fn resolve_type_ref_internal(&mut self, type_ref: &TypeRef) -> TypeResolution {
        // Handle arrays
        if type_ref.array_dims > 0 {
            return self.resolve_array_type(type_ref);
        }
        
        // Handle generics
        if !type_ref.type_args.is_empty() {
            return self.resolve_generic_type(type_ref);
        }
        
        // Handle simple types (primitives and references)
        self.resolve_simple_type_internal(&type_ref.name)
    }
    
    fn resolve_simple_type_internal(&mut self, type_name: &str) -> TypeResolution {
        // Check primitives first
        if let Some(descriptor) = self.get_primitive_descriptor(type_name) {
            return TypeResolution::Primitive {
                type_name: type_name.to_string(),
                jvm_descriptor: descriptor,
            };
        }
        
        // Handle void
        if type_name == "void" {
            return TypeResolution::Void;
        }
        
        // Handle reference types
        let class_name = self.resolve_reference_class_name(type_name);
        let jvm_descriptor = format!("L{};", class_name.replace('.', "/"));
        
        TypeResolution::Reference {
            class_name,
            jvm_descriptor,
            is_interface: false, // TODO: Could be enhanced to detect interfaces
        }
    }
    
    fn resolve_array_type(&mut self, type_ref: &TypeRef) -> TypeResolution {
        let element_type_ref = TypeRef {
            name: type_ref.name.clone(),
            type_args: type_ref.type_args.clone(),
            annotations: vec![],
            array_dims: 0, // Remove array dimensions for element type
            span: type_ref.span.clone(),
        };
        
        let element_resolution = self.resolve_type_ref_internal(&element_type_ref);
        let element_descriptor = element_resolution.get_jvm_descriptor();
        let jvm_descriptor = format!("{}{}", "[".repeat(type_ref.array_dims), element_descriptor);
        
        TypeResolution::Array {
            element_type: Box::new(element_resolution),
            dimensions: type_ref.array_dims,
            jvm_descriptor,
        }
    }
    
    fn resolve_generic_type(&mut self, type_ref: &TypeRef) -> TypeResolution {
        let base_resolution = self.resolve_simple_type_internal(&type_ref.name);
        let mut type_args = Vec::new();
        
        for type_arg in &type_ref.type_args {
            // TODO: Implement proper type argument resolution
            // For now, create a simple TypeRef from type argument
            let arg_type_ref = TypeRef {
                name: "Object".to_string(), // Simplified
                type_args: vec![],
                annotations: vec![],
                array_dims: 0,
                span: type_ref.span.clone(),
            };
            type_args.push(self.resolve_type_ref_internal(&arg_type_ref));
        }
        
        TypeResolution::Generic {
            base_type: Box::new(base_resolution.clone()),
            type_args,
            raw_jvm_descriptor: base_resolution.get_jvm_descriptor().to_string(),
        }
    }
    
    fn resolve_reference_class_name(&mut self, simple_name: &str) -> String {
        // Check builtin reference types
        if let Some(builtin_fqn) = self.builtin_types.get_builtin_reference_type(simple_name) {
            return builtin_fqn;
        }
        
        // Check if it's in java.lang
        if JAVA_LANG_SIMPLE_TYPES.contains(&simple_name) {
            return format!("java.lang.{}", simple_name);
        }
        
        // Default: assume it's a simple class name
        simple_name.to_string()
    }
    
    fn get_primitive_descriptor(&self, type_name: &str) -> Option<String> {
        match type_name {
            "boolean" => Some("Z".to_string()),
            "byte" => Some("B".to_string()),
            "char" => Some("C".to_string()),
            "short" => Some("S".to_string()),
            "int" => Some("I".to_string()),
            "long" => Some("J".to_string()),
            "float" => Some("F".to_string()),
            "double" => Some("D".to_string()),
            _ => None,
        }
    }
    
    fn create_cache_key(&self, type_ref: &TypeRef) -> String {
        format!(
            "{}[{}]<{}>",
            type_ref.name,
            type_ref.array_dims,
            type_ref.type_args.len()
        )
    }
}

/// Backwards compatibility: TypeResolver that owns its ClassManager
/// This allows existing code to continue working while we migrate
pub struct StandaloneTypeResolver {
    manager: ClassManager,
}

impl StandaloneTypeResolver {
    pub fn new(default_classpath: &str) -> Self {
        StandaloneTypeResolver {
            manager: ClassManager::with_classpath(default_classpath),
        }
    }
    
    /// Create a borrowed TypeResolver from this standalone instance
    pub fn as_borrowed(&mut self) -> TypeResolver {
        TypeResolver::new(&mut self.manager)
    }
    
    /// Create with symbol environment
    pub fn with_symbol_environment(default_classpath: &str, symbol_env: SymbolEnvironment) -> Self {
        // For now, ignore the symbol_env in this simple implementation
        Self::new(default_classpath)
    }
    
    /// Get mutable reference to manager 
    pub fn get_manager(&mut self) -> &mut ClassManager {
        &mut self.manager
    }
    
    /// Resolve type name using internal resolver with borrowed ImportResolver
    pub fn resolve_type_name(&mut self, simple_name: &str, import_resolver: &mut ImportResolver) -> Option<String> {
        let mut borrowed = self.as_borrowed();
        borrowed.resolve_type_name(simple_name, import_resolver)
    }
    
    /// Resolve type name using StandaloneImportResolver
    pub fn resolve_type_name_standalone(&mut self, simple_name: &str, import_resolver: &mut crate::common::import::StandaloneImportResolver) -> Option<String> {
        let mut borrowed_import = import_resolver.as_borrowed();
        let mut borrowed_type = self.as_borrowed();
        borrowed_type.resolve_type_name(simple_name, &mut borrowed_import)
    }
}

impl OwnedTypeResolver {
    /// Create OwnedTypeResolver with shared SymbolEnvironment
    /// This is the preferred method as it enables shared caching
    pub fn with_symbol_env(symbol_env: Arc<SymbolEnvironment>) -> Self {
        OwnedTypeResolver {
            symbol_env: Some(symbol_env),
            builtin_types: BuiltinTypeRegistry::new(),
            resolution_cache: HashMap::new(),
            type_resolution_cache: HashMap::new(),
        }
    }
    
    /// Legacy constructor for backwards compatibility
    /// Creates a standalone resolver with its own ClassManager (not recommended for new code)
    pub fn new(default_classpath: &str) -> Self {
        let symbol_env = Arc::new(SymbolEnvironment::with_classpath(default_classpath));
        Self::with_symbol_env(symbol_env)
    }
    
    /// Simple type name resolution without import context
    /// Now uses the shared ClassManager from SymbolEnvironment
    pub fn resolve_type_name_simple(&mut self, simple_name: &str) -> Option<String> {
        // Check builtin types first
        if let Some(builtin_fqn) = self.builtin_types.get_builtin_reference_type(simple_name) {
            return Some(builtin_fqn);
        }
        
        // Try classpath manager's common packages via SymbolEnvironment
        if let Some(ref symbol_env) = self.symbol_env {
            if let Some(manager_arc) = symbol_env.get_classpath_manager() {
                if let Ok(mut manager) = manager_arc.lock() {
                    return manager.find_class_by_simple_name(simple_name);
                }
            }
        }
        
        None
    }
    
    /// Get reference to the shared ClassManager
    /// Useful for operations that need direct manager access
    pub fn get_classpath_manager(&self) -> Option<Arc<Mutex<ClassManager>>> {
        self.symbol_env.as_ref().and_then(|env| env.get_classpath_manager())
    }
}

/// Registry of built-in Java types
pub struct BuiltinTypeRegistry {
    primitive_types: HashMap<String, String>,
    reference_types: HashMap<String, String>,
}

impl BuiltinTypeRegistry {
    pub fn new() -> Self {
        let mut primitive_types = HashMap::new();
        primitive_types.insert("boolean".to_string(), "boolean".to_string());
        primitive_types.insert("byte".to_string(), "byte".to_string());
        primitive_types.insert("char".to_string(), "char".to_string());
        primitive_types.insert("short".to_string(), "short".to_string());
        primitive_types.insert("int".to_string(), "int".to_string());
        primitive_types.insert("long".to_string(), "long".to_string());
        primitive_types.insert("float".to_string(), "float".to_string());
        primitive_types.insert("double".to_string(), "double".to_string());
        primitive_types.insert("void".to_string(), "void".to_string());
        
        let mut reference_types = HashMap::new();
        // Core java.lang types that are commonly used without import
        reference_types.insert("String".to_string(), "java.lang.String".to_string());
        reference_types.insert("Object".to_string(), "java.lang.Object".to_string());
        reference_types.insert("Class".to_string(), "java.lang.Class".to_string());
        reference_types.insert("Integer".to_string(), "java.lang.Integer".to_string());
        reference_types.insert("Long".to_string(), "java.lang.Long".to_string());
        reference_types.insert("Double".to_string(), "java.lang.Double".to_string());
        reference_types.insert("Float".to_string(), "java.lang.Float".to_string());
        reference_types.insert("Boolean".to_string(), "java.lang.Boolean".to_string());
        reference_types.insert("Character".to_string(), "java.lang.Character".to_string());
        reference_types.insert("Byte".to_string(), "java.lang.Byte".to_string());
        reference_types.insert("Short".to_string(), "java.lang.Short".to_string());
        
        // Common exception types
        reference_types.insert("Exception".to_string(), "java.lang.Exception".to_string());
        reference_types.insert("RuntimeException".to_string(), "java.lang.RuntimeException".to_string());
        reference_types.insert("IllegalArgumentException".to_string(), "java.lang.IllegalArgumentException".to_string());
        reference_types.insert("NullPointerException".to_string(), "java.lang.NullPointerException".to_string());
        reference_types.insert("UnsupportedOperationException".to_string(), "java.lang.UnsupportedOperationException".to_string());
        
        BuiltinTypeRegistry {
            primitive_types,
            reference_types,
        }
    }
    
    pub fn is_primitive(&self, type_name: &str) -> bool {
        self.primitive_types.contains_key(type_name)
    }
    
    pub fn is_builtin(&self, type_name: &str) -> bool {
        self.primitive_types.contains_key(type_name) || 
        self.reference_types.values().any(|v| v == type_name)
    }
    
    pub fn get_builtin_reference_type(&self, simple_name: &str) -> Option<String> {
        self.reference_types.get(simple_name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::import::ImportResolver;
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_builtin_types() {
        let registry = BuiltinTypeRegistry::new();
        
        // Test primitive types
        assert!(registry.is_primitive("int"));
        assert!(registry.is_primitive("boolean"));
        assert!(!registry.is_primitive("String"));
        
        // Test builtin reference types
        assert_eq!(registry.get_builtin_reference_type("String"), Some("java.lang.String".to_string()));
        assert_eq!(registry.get_builtin_reference_type("Object"), Some("java.lang.Object".to_string()));
        assert_eq!(registry.get_builtin_reference_type("NonExistent"), None);
    }
    
    #[test]
    fn test_type_resolution() {
        // Create test classpath
        let temp_dir = TempDir::new().unwrap();
        let java_util = temp_dir.path().join("java/util");
        fs::create_dir_all(&java_util).unwrap();
        fs::write(java_util.join("List.java"), "package java.util; public interface List {}").unwrap();
        
        let mut standalone_type_resolver = StandaloneTypeResolver::new(&temp_dir.path().to_string_lossy());
        let mut standalone_import_resolver = crate::common::import::StandaloneImportResolver::new(&temp_dir.path().to_string_lossy());
        
        let mut type_resolver = standalone_type_resolver.as_borrowed();
        let mut import_resolver = standalone_import_resolver.as_borrowed();
        
        // Test primitive resolution
        assert_eq!(type_resolver.resolve_type_name("int", &mut import_resolver), Some("int".to_string()));
        
        // Test builtin reference type resolution
        assert_eq!(type_resolver.resolve_type_name("String", &mut import_resolver), Some("java.lang.String".to_string()));
        
        // Test internal name conversion
        assert_eq!(type_resolver.resolve_to_internal_name("String", &mut import_resolver), "java/lang/String");
    }
}