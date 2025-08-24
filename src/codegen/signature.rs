//! JVM Signature attribute generation for generic types
//! 
//! This module handles the conversion of AST generic type information into JVM signature strings
//! that are stored in the Signature attribute of .class files.
//! 
//! Based on javac's SignatureGenerator implementation in Types.java and ClassWriter.java

use crate::ast::*;
use crate::common::consts::JAVA_LANG_SIMPLE_TYPES;
use crate::common::type_resolver::TypeResolver;
use crate::common::import::ImportResolver;
use std::collections::HashMap;

/// Global type name resolver that maps simple type names to their fully qualified names
/// This should be populated by scanning the classpath before signature generation
pub struct TypeNameResolver {
    type_mapping: HashMap<String, String>,
}

impl TypeNameResolver {
    /// Create a new type name resolver
    pub fn new() -> Self {
        Self {
            type_mapping: HashMap::new(),
        }
    }

    /// Add a type mapping from simple name to fully qualified name
    pub fn add_type_mapping(&mut self, simple_name: &str, qualified_name: &str) {
        self.type_mapping.insert(simple_name.to_string(), qualified_name.to_string());
    }

    /// Resolve a simple type name to its fully qualified name
    pub fn resolve_type_name(&self, simple_name: &str) -> Option<&String> {
        self.type_mapping.get(simple_name)
    }

    /// Build a default type name resolver with common Java types
    /// In a real implementation, this would be populated by scanning the classpath
    pub fn with_default_mappings() -> Self {
        let mut resolver = Self::new();
        
        // Add common Java types - these would normally come from classpath scanning
        resolver.add_type_mapping("AbstractCollection", "java.util.AbstractCollection");
        resolver.add_type_mapping("AbstractList", "java.util.AbstractList");
        resolver.add_type_mapping("AbstractSet", "java.util.AbstractSet");
        resolver.add_type_mapping("Set", "java.util.Set");
        resolver.add_type_mapping("Collection", "java.util.Collection");
        resolver.add_type_mapping("List", "java.util.List");
        resolver.add_type_mapping("ListIterator", "java.util.ListIterator");
        resolver.add_type_mapping("Map", "java.util.Map");
        resolver.add_type_mapping("Iterator", "java.util.Iterator");
        resolver.add_type_mapping("Iterable", "java.lang.Iterable");
        resolver.add_type_mapping("Serializable", "java.io.Serializable");
        resolver.add_type_mapping("Cloneable", "java.lang.Cloneable");
        
        resolver
    }
}

/// Generate a JVM signature string for a type reference
pub fn type_ref_to_signature(type_ref: &TypeRef, package_name: Option<&str>, current_class_name: Option<&str>, type_resolver: &TypeNameResolver) -> String {
    let mut signature = String::new();
    
    // Add array dimensions FIRST for ALL types (before the type itself)
    // This is crucial for JVM signature format: [LType<Args>; not LType<Args>;[
    if type_ref.array_dims > 0 {
        for _ in 0..type_ref.array_dims {
            signature.push('[');
        }
    }
    
    // Check if this is a type variable (generic parameter)
    // Type variables are typically single uppercase letters like T, S, E, K, V
    if type_ref.name.len() == 1 && type_ref.name.chars().next().unwrap().is_uppercase() {
        // Type variable: use TT; format (e.g., T -> TT;)
        signature.push_str(&format!("T{};", type_ref.name));
    } else {
        // Regular type: use normal signature format
        signature.push_str(&resolve_internal_name_for_signature(&type_ref.name, package_name, current_class_name, type_resolver));
    }
    
    // Add type arguments if present (after the base type)
    if !type_ref.type_args.is_empty() {
        // Remove the trailing semicolon from the base type
        signature.pop(); // Remove ';'
        
        // Add type arguments with angle brackets
        signature.push('<');
        for type_arg in &type_ref.type_args {
            match type_arg {
                TypeArg::Type(t) => {
                    // Check if this is a type variable (generic parameter)
                    // Type variables are typically single uppercase letters like T, S, E, K, V
                    if t.name.len() == 1 && t.name.chars().next().unwrap().is_uppercase() {
                        // Type variable: use TT; format (e.g., T -> TT;)
                        signature.push_str(&format!("T{};", t.name));
                    } else {
                        // Regular type: use normal signature format
                        signature.push_str(&type_ref_to_signature(t, package_name, current_class_name, type_resolver));
                    }
                }
                TypeArg::Wildcard(w) => {
                    if let Some((bound_kind, bound_type)) = &w.bound {
                        match bound_kind {
                            BoundKind::Extends => {
                                signature.push_str("+");
                                signature.push_str(&type_ref_to_signature(bound_type, package_name, current_class_name, type_resolver));
                            }
                            BoundKind::Super => {
                                signature.push_str("-");
                                signature.push_str(&type_ref_to_signature(bound_type, package_name, current_class_name, type_resolver));
                            }
                        }
                    } else {
                        signature.push('*'); // Unbounded wildcard
                    }
                }
            }
        }
        signature.push('>');
        
        // Add the final semicolon
        signature.push(';');
    }
    
    // NOTE: Array dimensions should be handled BEFORE the type, not after
    // The logic above (lines 64-68) already handles array dimensions correctly for type variables
    // For regular types with type arguments, array dimensions should be prepended to the signature
    // This section previously had incorrect logic that added '[' at the end instead of the beginning
    
    signature
}

/// Generate a JVM signature string for a type parameter
pub fn type_param_to_signature(type_param: &TypeParam, package_name: Option<&str>, current_class_name: Option<&str>, type_resolver: &TypeNameResolver) -> String {
    let mut signature = type_param.name.clone();
    
    if !type_param.bounds.is_empty() {
        // For type parameters with bounds, we need to handle the first bound specially
        // The first bound determines if it's a class or interface
        let first_bound = &type_param.bounds[0];
        let first_bound_name = &first_bound.name;
        
        // Check if first bound is an interface (java.lang.Object is treated specially)
        let is_first_interface = first_bound_name != "Object" && 
                                !JAVA_LANG_SIMPLE_TYPES.contains(&first_bound_name.as_str());
        
        if is_first_interface {
            signature.push(':');
        }
        
        // Add all bounds
        for bound in &type_param.bounds {
            signature.push(':');
            signature.push_str(&type_ref_to_signature(bound, package_name, current_class_name, type_resolver));
        }
    } else {
        // For unbounded type parameters, add default bound :Ljava/lang/Object;
        signature.push_str(":Ljava/lang/Object;");
    }
    
    signature
}

/// Generate a JVM signature string for a method
pub fn method_to_signature(method: &MethodDecl, package_name: Option<&str>, current_class_name: Option<&str>, type_resolver: &TypeNameResolver) -> String {
    let mut signature = String::new();
    
    // Add type parameters if present
    if !method.type_params.is_empty() {
        signature.push('<');
        for type_param in &method.type_params {
            signature.push_str(&type_param_to_signature(type_param, package_name, current_class_name, type_resolver));
        }
        signature.push('>');
    }
    
    // Add parameter types
    signature.push('(');
    for param in &method.parameters {
        signature.push_str(&type_ref_to_signature(&param.type_ref, package_name, current_class_name, type_resolver));
    }
    signature.push(')');
    
    // Add return type
    if let Some(return_type) = &method.return_type {
        signature.push_str(&type_ref_to_signature(return_type, package_name, current_class_name, type_resolver));
    } else {
        signature.push('V'); // void
    }
    
    signature
}

/// Generate a JVM signature string for a class
/// Format: <T:Ljava/lang/Object;>LSuperClass<TT;>;LInterface1<TT;>;LInterface2<TT;>;
pub fn class_to_signature(class: &ClassDecl, package_name: Option<&str>, current_class_name: Option<&str>, type_resolver: &TypeNameResolver) -> String {
    let mut signature = String::new();
    
    // Add type parameters if present
    if !class.type_params.is_empty() {
        signature.push('<');
        for type_param in &class.type_params {
            signature.push_str(&type_param_to_signature(type_param, package_name, current_class_name, type_resolver));
        }
        signature.push('>');
    }
    
    // Add superclass
    if let Some(extends) = &class.extends {
        signature.push_str(&type_ref_to_signature(extends, package_name, current_class_name, type_resolver));
    } else {
        signature.push_str("Ljava/lang/Object;");
    }
    
    // Add implemented interfaces
    for interface in &class.implements {
        signature.push_str(&type_ref_to_signature(interface, package_name, current_class_name, type_resolver));
    }
    
    signature
}

/// Generate a JVM signature string for an interface
/// Format: <T:Ljava/lang/Object;>Ljava/lang/Object;LInterface1;LInterface2;
/// Note: Interfaces DO include Ljava/lang/Object; as superclass in signatures (JavaC alignment)
pub fn interface_to_signature(interface: &InterfaceDecl, package_name: Option<&str>, current_class_name: Option<&str>, type_resolver: &TypeNameResolver) -> String {
    let mut signature = String::new();
    
    // Add type parameters if present
    if !interface.type_params.is_empty() {
        signature.push('<');
        for type_param in &interface.type_params {
            signature.push_str(&type_param_to_signature(type_param, package_name, current_class_name, type_resolver));
        }
        signature.push('>');
    }
    
    // All interfaces implicitly extend java.lang.Object in their signature 
    // This is required by JavaC alignment even though interfaces conceptually don't inherit from Object
    signature.push_str("Ljava/lang/Object;");
    
    // Add explicitly extended interfaces
    for extends in &interface.extends {
        signature.push_str(&type_ref_to_signature(extends, package_name, current_class_name, type_resolver));
    }
    
    signature
}

/// Resolve a type name to its internal JVM representation for signature generation
fn resolve_internal_name_for_signature(raw: &str, package_name: Option<&str>, current_class_name: Option<&str>, type_resolver: &TypeNameResolver) -> String {
    // Handle primitive types
    match raw {
        "int" => return "I".to_string(),
        "long" => return "J".to_string(),
        "float" => return "F".to_string(),
        "double" => return "D".to_string(),
        "boolean" => return "Z".to_string(),
        "char" => return "C".to_string(),
        "byte" => return "B".to_string(),
        "short" => return "S".to_string(),
        "void" => return "V".to_string(),
        _ => {}
    }
    
    // First try the provided TypeNameResolver for import resolution
    if let Some(fully_qualified) = type_resolver.resolve_type_name(raw) {
        return format!("L{};", fully_qualified.replace('.', "/"));
    }
    
    // Handle java.lang types (fallback)
    if JAVA_LANG_SIMPLE_TYPES.contains(&raw) {
        return format!("Ljava/lang/{};", raw);
    }
    
    // Handle already qualified names
    if raw.contains('/') || raw.contains('.') {
        return format!("L{};", raw.replace('.', "/"));
    }
    
    // Handle inner class references
    if let Some(current_class) = current_class_name {
        // Handle specific inner class references based on context
        if raw == "Entry" && current_class.ends_with("/Map") {
            // Map.Entry is a well-known inner interface
            return format!("L{}${};", current_class, raw);
        }
        // Add other specific inner class patterns as needed
    }
    
    // Try to resolve using the type resolver again
    if let Some(qualified_name) = type_resolver.resolve_type_name(raw) {
        return format!("L{};", qualified_name.replace('.', "/"));
    }
    
    // Handle unqualified names - add package prefix if available
    if let Some(pkg) = package_name {
        if !pkg.is_empty() {
            return format!("L{}/{};", pkg.replace('.', "/"), raw);
        }
    }
    
    // Default case - assume it's in the current package
    format!("L{};", raw)
}
