//! Enhanced Type Descriptor Resolution with Strong Type Support
//!
//! This module provides JavaC-aligned type resolution with strong typing,
//! proper generic handling, and comprehensive JVM descriptor generation.

use crate::ast::{TypeRef, TypeEnum, ReferenceType, PrimitiveType};
use crate::codegen::enter::SymbolEnvironment;
use crate::codegen::array_type_info::{ArrayTypeInfo, ArrayAwareTypeResolver};
use std::collections::HashMap;

/// Strongly-typed type resolution result
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
    
    /// Check if this type is assignable from another type (JavaC isAssignable)
    pub fn is_assignable_from(&self, other: &TypeResolution) -> bool {
        match (self, other) {
            // Same types are always assignable
            (a, b) if a == b => true,
            
            // Primitive widening conversions
            (TypeResolution::Primitive { type_name: to, .. }, 
             TypeResolution::Primitive { type_name: from, .. }) => {
                Self::is_primitive_widening_conversion(from, to)
            },
            
            // Object is assignable from any reference type
            (TypeResolution::Reference { class_name, .. }, TypeResolution::Reference { .. }) 
                if class_name == "java.lang.Object" => true,
            
            // Array covariance for reference types
            (TypeResolution::Array { element_type: to_elem, dimensions: to_dims, .. },
             TypeResolution::Array { element_type: from_elem, dimensions: from_dims, .. }) 
                if to_dims == from_dims => {
                to_elem.is_assignable_from(from_elem)
            },
            
            // Other cases require more complex inheritance checking
            _ => false,
        }
    }
    
    /// Check if primitive widening conversion is allowed
    fn is_primitive_widening_conversion(from: &str, to: &str) -> bool {
        match (from, to) {
            // byte can be widened to short, int, long, float, double
            ("byte", "short" | "int" | "long" | "float" | "double") => true,
            // short can be widened to int, long, float, double  
            ("short", "int" | "long" | "float" | "double") => true,
            // char can be widened to int, long, float, double
            ("char", "int" | "long" | "float" | "double") => true,
            // int can be widened to long, float, double
            ("int", "long" | "float" | "double") => true,
            // long can be widened to float, double
            ("long", "float" | "double") => true,
            // float can be widened to double
            ("float", "double") => true,
            _ => false,
        }
    }
    
    /// Get the size in JVM stack slots (1 for most types, 2 for long/double)
    pub fn get_stack_size(&self) -> u16 {
        match self {
            TypeResolution::Primitive { type_name, .. } => {
                match type_name.as_str() {
                    "long" | "double" => 2,
                    _ => 1,
                }
            },
            _ => 1, // References are always 1 slot
        }
    }
}

/// Enhanced type resolver with strong typing and JavaC alignment
pub struct EnhancedTypeResolver {
    /// Symbol environment for type resolution
    symbol_env: Option<SymbolEnvironment>,
    /// Type cache for performance
    type_cache: HashMap<String, TypeResolution>,
    /// Known interface types for is_interface flag
    interface_types: HashMap<String, bool>,
}

impl EnhancedTypeResolver {
    /// Create new enhanced type resolver
    pub fn new() -> Self {
        Self {
            symbol_env: None,
            type_cache: HashMap::new(),
            interface_types: HashMap::new(),
        }
    }
    
    /// Create with symbol environment
    pub fn with_symbol_environment(symbol_env: SymbolEnvironment) -> Self {
        Self {
            symbol_env: Some(symbol_env),
            type_cache: HashMap::new(),
            interface_types: HashMap::new(),
        }
    }
    
    /// Resolve TypeRef to strong TypeResolution
    pub fn resolve_type_ref(&mut self, type_ref: &TypeRef) -> TypeResolution {
        // Check cache first
        let cache_key = self.create_cache_key(type_ref);
        if let Some(cached) = self.type_cache.get(&cache_key) {
            return cached.clone();
        }
        
        let resolution = self.resolve_type_ref_internal(type_ref);
        
        // Cache the result
        self.type_cache.insert(cache_key, resolution.clone());
        
        resolution
    }
    
    /// Internal type resolution logic
    fn resolve_type_ref_internal(&mut self, type_ref: &TypeRef) -> TypeResolution {
        // Handle array types
        if type_ref.array_dims > 0 {
            return self.resolve_array_type(type_ref);
        }
        
        // Handle generic types
        if !type_ref.type_args.is_empty() {
            return self.resolve_generic_type(type_ref);
        }
        
        // Handle simple types
        self.resolve_simple_type(&type_ref.name)
    }
    
    /// Resolve array type with proper dimension handling
    fn resolve_array_type(&mut self, type_ref: &TypeRef) -> TypeResolution {
        let element_type_ref = TypeRef {
            name: type_ref.name.clone(),
            type_args: type_ref.type_args.clone(),
            annotations: vec![], // Arrays don't inherit element annotations
            array_dims: 0, // Remove array dimensions for element resolution
            span: type_ref.span.clone(),
        };
        
        let element_resolution = Box::new(self.resolve_type_ref_internal(&element_type_ref));
        let array_info = ArrayTypeInfo::new(type_ref.name.clone(), type_ref.array_dims);
        
        TypeResolution::Array {
            element_type: element_resolution,
            dimensions: type_ref.array_dims,
            jvm_descriptor: array_info.get_jvm_descriptor().to_string(),
        }
    }
    
    /// Resolve generic type with type arguments
    fn resolve_generic_type(&mut self, type_ref: &TypeRef) -> TypeResolution {
        let base_resolution = Box::new(self.resolve_simple_type(&type_ref.name));
        let type_args: Vec<TypeResolution> = type_ref.type_args.iter()
            .map(|arg| match arg {
                crate::ast::TypeArg::Type(type_ref) => self.resolve_type_ref(type_ref),
                crate::ast::TypeArg::Wildcard(_wildcard) => {
                    // For wildcards, use Object as the erased type
                    TypeResolution::Reference {
                        class_name: "java/lang/Object".to_string(),
                        jvm_descriptor: "Ljava/lang/Object;".to_string(),
                        is_interface: false,
                    }
                }
            })
            .collect();
        
        // For JVM, generics are erased, so use raw type descriptor
        let raw_descriptor = base_resolution.get_jvm_descriptor().to_string();
        
        TypeResolution::Generic {
            base_type: base_resolution,
            type_args,
            raw_jvm_descriptor: raw_descriptor,
        }
    }
    
    /// Resolve simple (non-array, non-generic) type
    fn resolve_simple_type(&mut self, type_name: &str) -> TypeResolution {
        // Check primitive types first
        if let Some(resolution) = self.resolve_primitive_type(type_name) {
            return resolution;
        }
        
        // Special case for void
        if type_name == "void" {
            return TypeResolution::Void;
        }
        
        // JavaC alignment: Handle generic type parameters (single uppercase letters)
        // Generic type variables like T, K, V should be erased to their bounds (Object if unbounded)
        if type_name.len() == 1 && type_name.chars().next().unwrap().is_ascii_uppercase() {
            return TypeResolution::Reference {
                class_name: "java.lang.Object".to_string(),
                jvm_descriptor: "Ljava/lang/Object;".to_string(),
                is_interface: false,
            };
        }
        
        // Resolve reference type
        self.resolve_reference_type(type_name)
    }
    
    /// Resolve primitive type
    fn resolve_primitive_type(&self, type_name: &str) -> Option<TypeResolution> {
        let (name, descriptor) = match type_name {
            "int" => ("int", "I"),
            "long" => ("long", "J"),
            "float" => ("float", "F"),
            "double" => ("double", "D"),
            "boolean" => ("boolean", "Z"),
            "byte" => ("byte", "B"),
            "char" => ("char", "C"),
            "short" => ("short", "S"),
            _ => return None,
        };
        
        Some(TypeResolution::Primitive {
            type_name: name.to_string(),
            jvm_descriptor: descriptor.to_string(),
        })
    }
    
    /// Resolve reference type (class/interface)
    fn resolve_reference_type(&mut self, type_name: &str) -> TypeResolution {
        // Try to resolve through symbol environment
        if let Some(ref symbol_env) = self.symbol_env {
            if let Some(resolved_name) = symbol_env.resolve_type(type_name) {
                let is_interface = self.is_interface_type(&resolved_name);
                let class_name = resolved_name.replace('/', ".");
                let jvm_descriptor = format!("L{};", resolved_name);
                
                return TypeResolution::Reference {
                    class_name,
                    jvm_descriptor,
                    is_interface,
                };
            }
        }
        
        // Fallback: assume it's a class and use as-is
        let class_name = type_name.to_string();
        let jvm_name = type_name.replace('.', "/");
        let jvm_descriptor = format!("L{};", jvm_name);
        let is_interface = self.is_interface_type(type_name);
        
        TypeResolution::Reference {
            class_name,
            jvm_descriptor,
            is_interface,
        }
    }
    
    /// Check if a type is an interface (simplified heuristic)
    fn is_interface_type(&self, type_name: &str) -> bool {
        // Check cache first
        if let Some(&is_interface) = self.interface_types.get(type_name) {
            return is_interface;
        }
        
        // Simple heuristics (in full implementation, would query symbol environment)
        let is_interface = type_name.contains("Interface") || 
                          type_name.starts_with("java.util.function.") ||
                          type_name == "java.util.List" ||
                          type_name == "java.util.Map";
        
        is_interface
    }
    
    /// Create cache key for TypeRef
    fn create_cache_key(&self, type_ref: &TypeRef) -> String {
        if type_ref.type_args.is_empty() {
            format!("{}[{}]", type_ref.name, type_ref.array_dims)
        } else {
            let args: Vec<String> = type_ref.type_args.iter()
                .map(|arg| match arg {
                    crate::ast::TypeArg::Type(type_ref) => self.create_cache_key(type_ref),
                    crate::ast::TypeArg::Wildcard(_) => "?".to_string(),
                })
                .collect();
            format!("{}<{}>[{}]", type_ref.name, args.join(","), type_ref.array_dims)
        }
    }
    
    /// Convert TypeEnum to TypeResolution for compatibility
    pub fn from_type_enum(&mut self, type_enum: &TypeEnum) -> TypeResolution {
        match type_enum {
            TypeEnum::Primitive(prim) => {
                let type_name = match prim {
                    PrimitiveType::Int => "int",
                    PrimitiveType::Long => "long",
                    PrimitiveType::Float => "float",
                    PrimitiveType::Double => "double",
                    PrimitiveType::Boolean => "boolean",
                    PrimitiveType::Byte => "byte",
                    PrimitiveType::Char => "char",
                    PrimitiveType::Short => "short",
                    // Note: PrimitiveType::Void doesn't exist - void is a separate TypeEnum::Void variant
                };
                self.resolve_primitive_type(type_name).unwrap()
            },
            TypeEnum::Reference(ref_type) => {
                match ref_type {
                    ReferenceType::Class(class_name) => {
                        self.resolve_reference_type(class_name)
                    },
                    ReferenceType::Array(element_ref) => {
                        // Convert TypeRef to TypeResolution
                        let element_resolution = Box::new(self.from_type_enum(
                            &TypeEnum::Reference(ReferenceType::Class(element_ref.name.clone()))
                        ));
                        let array_info = ArrayTypeInfo::new(element_ref.name.clone(), element_ref.array_dims + 1);
                        
                        TypeResolution::Array {
                            element_type: element_resolution,
                            dimensions: 1,
                            jvm_descriptor: array_info.get_jvm_descriptor().to_string(),
                        }
                    },
                    ReferenceType::Interface(interface_name) => {
                        // Interfaces are handled the same as classes in JVM bytecode
                        self.resolve_reference_type(interface_name)
                    },
                }
            },
            // Note: TypeEnum::Array doesn't exist - arrays are represented as Reference(ReferenceType::Array)
            // This case is handled above in the Reference match arm
            TypeEnum::Void => TypeResolution::Void,
            _ => {
                TypeResolution::Unresolved {
                    type_name: "unknown".to_string(),
                    reason: "Unsupported TypeEnum variant".to_string(),
                }
            }
        }
    }
    
    /// Clear the type cache
    pub fn clear_cache(&mut self) {
        self.type_cache.clear();
    }
    
    /// Update symbol environment
    pub fn set_symbol_environment(&mut self, symbol_env: SymbolEnvironment) {
        self.symbol_env = Some(symbol_env);
        self.clear_cache(); // Clear cache when environment changes
    }
}

impl Default for EnhancedTypeResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location};
    
    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }
    
    #[test]
    fn test_primitive_type_resolution() {
        let mut resolver = EnhancedTypeResolver::new();
        let resolution = resolver.resolve_simple_type("int");
        
        match resolution {
            TypeResolution::Primitive { type_name, jvm_descriptor } => {
                assert_eq!(type_name, "int");
                assert_eq!(jvm_descriptor, "I");
            },
            _ => panic!("Expected primitive type resolution"),
        }
    }
    
    #[test]
    fn test_array_type_resolution() {
        let mut resolver = EnhancedTypeResolver::new();
        let type_ref = TypeRef {
            name: "int".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 1,
            span: create_span(),
        };
        
        let resolution = resolver.resolve_type_ref(&type_ref);
        match resolution {
            TypeResolution::Array { dimensions, jvm_descriptor, .. } => {
                assert_eq!(dimensions, 1);
                assert_eq!(jvm_descriptor, "[I");
            },
            _ => panic!("Expected array type resolution"),
        }
    }
    
    #[test]
    fn test_reference_type_resolution() {
        let mut resolver = EnhancedTypeResolver::new();
        let resolution = resolver.resolve_simple_type("java.lang.String");
        
        match resolution {
            TypeResolution::Reference { class_name, jvm_descriptor, .. } => {
                assert_eq!(class_name, "java.lang.String");
                assert_eq!(jvm_descriptor, "Ljava/lang/String;");
            },
            _ => panic!("Expected reference type resolution"),
        }
    }
    
    #[test]
    fn test_type_assignability() {
        let mut resolver = EnhancedTypeResolver::new();
        let int_type = resolver.resolve_simple_type("int");
        let long_type = resolver.resolve_simple_type("long");
        
        // int can be widened to long
        assert!(long_type.is_assignable_from(&int_type));
        // but long cannot be narrowed to int without explicit cast
        assert!(!int_type.is_assignable_from(&long_type));
    }
    
    #[test]
    fn test_stack_size() {
        let mut resolver = EnhancedTypeResolver::new();
        let int_type = resolver.resolve_simple_type("int");
        let long_type = resolver.resolve_simple_type("long");
        let string_type = resolver.resolve_simple_type("java.lang.String");
        
        assert_eq!(int_type.get_stack_size(), 1);
        assert_eq!(long_type.get_stack_size(), 2);
        assert_eq!(string_type.get_stack_size(), 1);
    }
}