//! Enhanced Array Type Information for JavaC-aligned type representation
//!
//! This module provides improved array type handling that correctly generates
//! JVM descriptors and maintains compatibility with JavaC's type system.

use crate::ast::TypeRef;
use std::fmt;

/// Enhanced array type representation that properly handles JVM descriptors
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayTypeInfo {
    /// Base element type (without array dimensions)
    pub element_type: String,
    /// Number of array dimensions (1 for [], 2 for [][], etc.)
    pub dimensions: usize,
    /// Pre-computed JVM descriptor for efficiency
    pub jvm_descriptor: String,
    /// Whether this is a primitive array
    pub is_primitive: bool,
}

impl ArrayTypeInfo {
    /// Create ArrayTypeInfo from TypeRef following JavaC patterns
    pub fn from_type_ref(type_ref: &TypeRef) -> Self {
        let dimensions = type_ref.array_dims;
        let element_type = type_ref.name.clone();
        let is_primitive = Self::is_primitive_type(&element_type);
        let jvm_descriptor = Self::compute_jvm_descriptor(&element_type, dimensions);
        
        Self {
            element_type,
            dimensions,
            jvm_descriptor,
            is_primitive,
        }
    }
    
    /// Create ArrayTypeInfo directly from type name and dimensions
    pub fn new(element_type: String, dimensions: usize) -> Self {
        let is_primitive = Self::is_primitive_type(&element_type);
        let jvm_descriptor = Self::compute_jvm_descriptor(&element_type, dimensions);
        
        Self {
            element_type,
            dimensions,
            jvm_descriptor,
            is_primitive,
        }
    }
    
    /// Check if a type name represents a primitive type
    fn is_primitive_type(type_name: &str) -> bool {
        matches!(type_name, 
            "int" | "long" | "float" | "double" | "boolean" | 
            "byte" | "char" | "short" | "void"
        )
    }
    
    /// Compute JVM descriptor for array type following JVM specification
    fn compute_jvm_descriptor(element_type: &str, dimensions: usize) -> String {
        let element_descriptor = Self::element_to_descriptor(element_type);
        let array_prefix = "[".repeat(dimensions);
        format!("{}{}", array_prefix, element_descriptor)
    }
    
    /// Convert element type name to JVM descriptor
    fn element_to_descriptor(element_type: &str) -> String {
        match element_type {
            // Primitive types
            "int" => "I",
            "long" => "J",
            "float" => "F",
            "double" => "D",
            "boolean" => "Z",
            "byte" => "B",
            "char" => "C",
            "short" => "S",
            "void" => "V",
            // Reference types
            _ => {
                // JavaC alignment: Handle generic type parameters (single uppercase letters)
                // Generic type variables like T, K, V should be erased to their bounds (Object if unbounded)
                if element_type.len() == 1 && element_type.chars().next().unwrap().is_ascii_uppercase() {
                    return "Ljava/lang/Object;".to_string();
                }
                
                // Handle class names: java.lang.String -> Ljava/lang/String;
                let class_name = element_type.replace('.', "/");
                return format!("L{};", class_name);
            }
        }.to_string()
    }
    
    /// Get the JVM descriptor for this array type
    pub fn get_jvm_descriptor(&self) -> &str {
        &self.jvm_descriptor
    }
    
    /// Get the element type name
    pub fn get_element_type(&self) -> &str {
        &self.element_type
    }
    
    /// Get the number of dimensions
    pub fn get_dimensions(&self) -> usize {
        self.dimensions
    }
    
    /// Check if this is a primitive array
    pub fn is_primitive_array(&self) -> bool {
        self.is_primitive
    }
    
    /// Get the component type (array with one less dimension)
    /// For example, int[][] -> int[], int[] -> int
    pub fn get_component_type(&self) -> Option<ArrayTypeInfo> {
        if self.dimensions > 1 {
            Some(ArrayTypeInfo::new(self.element_type.clone(), self.dimensions - 1))
        } else if self.dimensions == 1 {
            // Return element type info (not array)
            None // Could return element type info here if needed
        } else {
            None
        }
    }
    
    /// Create array type with additional dimension
    /// For example, int -> int[], int[] -> int[][]
    pub fn add_dimension(&self) -> ArrayTypeInfo {
        ArrayTypeInfo::new(self.element_type.clone(), self.dimensions + 1)
    }
    
    /// Parse JVM descriptor to create ArrayTypeInfo
    /// For example, "[I" -> int[], "[[Ljava/lang/String;" -> String[][]
    pub fn from_jvm_descriptor(descriptor: &str) -> Option<ArrayTypeInfo> {
        if !descriptor.starts_with('[') {
            return None; // Not an array
        }
        
        let mut dimensions = 0;
        let mut chars = descriptor.chars();
        
        // Count array dimensions
        while let Some(ch) = chars.next() {
            if ch == '[' {
                dimensions += 1;
            } else {
                let element_descriptor = format!("{}{}", ch, chars.collect::<String>());
                let element_type = Self::descriptor_to_element_type(&element_descriptor)?;
                return Some(ArrayTypeInfo::new(element_type, dimensions));
            }
        }
        
        None
    }
    
    /// Convert JVM descriptor to element type name
    fn descriptor_to_element_type(descriptor: &str) -> Option<String> {
        match descriptor {
            "I" => Some("int".to_string()),
            "J" => Some("long".to_string()),
            "F" => Some("float".to_string()),
            "D" => Some("double".to_string()),
            "Z" => Some("boolean".to_string()),
            "B" => Some("byte".to_string()),
            "C" => Some("char".to_string()),
            "S" => Some("short".to_string()),
            "V" => Some("void".to_string()),
            desc if desc.starts_with('L') && desc.ends_with(';') => {
                // Reference type: Ljava/lang/String; -> java.lang.String
                let class_name = &desc[1..desc.len()-1].replace('/', ".");
                Some(class_name.to_string())
            }
            _ => None,
        }
    }
    
    /// Convert to human-readable Java syntax
    /// For example, "[I" -> "int[]", "[[Ljava/lang/String;" -> "String[][]"
    pub fn to_java_syntax(&self) -> String {
        let brackets = "[]".repeat(self.dimensions);
        format!("{}{}", self.element_type, brackets)
    }
}

impl fmt::Display for ArrayTypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_java_syntax())
    }
}

/// Enhanced type resolver that properly handles array types
pub struct ArrayAwareTypeResolver;

impl ArrayAwareTypeResolver {
    /// Resolve TypeRef to proper JVM descriptor with array support
    pub fn resolve_type_ref(type_ref: &TypeRef) -> String {
        if type_ref.array_dims > 0 {
            let array_info = ArrayTypeInfo::from_type_ref(type_ref);
            array_info.get_jvm_descriptor().to_string()
        } else {
            // Non-array type
            Self::resolve_element_type(&type_ref.name)
        }
    }
    
    /// Resolve element type to appropriate representation
    fn resolve_element_type(type_name: &str) -> String {
        match type_name {
            // Primitive types remain as names for non-array context
            "int" | "long" | "float" | "double" | "boolean" | 
            "byte" | "char" | "short" | "void" => type_name.to_string(),
            
            // Reference types - use fully qualified names
            _ => {
                // For now, return as-is. In full implementation, would resolve through imports
                type_name.to_string()
            }
        }
    }
    
    /// Create proper array type identifier for symbol table
    /// This ensures arrays are stored with proper JVM descriptors instead of malformed names
    pub fn create_array_type_identifier(element_type: &str, dimensions: usize) -> String {
        let array_info = ArrayTypeInfo::new(element_type.to_string(), dimensions);
        array_info.get_jvm_descriptor().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{TypeRef, Span, Location};
    
    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }
    
    #[test]
    fn test_primitive_array_descriptor() {
        let array_info = ArrayTypeInfo::new("int".to_string(), 1);
        assert_eq!(array_info.get_jvm_descriptor(), "[I");
        assert!(array_info.is_primitive_array());
        assert_eq!(array_info.to_java_syntax(), "int[]");
    }
    
    #[test]
    fn test_multi_dimensional_array() {
        let array_info = ArrayTypeInfo::new("int".to_string(), 2);
        assert_eq!(array_info.get_jvm_descriptor(), "[[I");
        assert_eq!(array_info.to_java_syntax(), "int[][]");
    }
    
    #[test]
    fn test_reference_array() {
        let array_info = ArrayTypeInfo::new("java.lang.String".to_string(), 1);
        assert_eq!(array_info.get_jvm_descriptor(), "[Ljava/lang/String;");
        assert!(!array_info.is_primitive_array());
        assert_eq!(array_info.to_java_syntax(), "java.lang.String[]");
    }
    
    #[test]
    fn test_from_type_ref() {
        let type_ref = TypeRef {
            name: "int".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 1,
            span: create_span(),
        };
        
        let array_info = ArrayTypeInfo::from_type_ref(&type_ref);
        assert_eq!(array_info.get_jvm_descriptor(), "[I");
    }
    
    #[test]
    fn test_from_jvm_descriptor() {
        let array_info = ArrayTypeInfo::from_jvm_descriptor("[I").unwrap();
        assert_eq!(array_info.get_element_type(), "int");
        assert_eq!(array_info.get_dimensions(), 1);
        assert_eq!(array_info.get_jvm_descriptor(), "[I");
    }
    
    #[test]
    fn test_complex_array_descriptor() {
        let array_info = ArrayTypeInfo::from_jvm_descriptor("[[Ljava/lang/String;").unwrap();
        assert_eq!(array_info.get_element_type(), "java.lang.String");
        assert_eq!(array_info.get_dimensions(), 2);
        assert_eq!(array_info.to_java_syntax(), "java.lang.String[][]");
    }
    
    #[test]
    fn test_array_aware_resolver() {
        let type_ref = TypeRef {
            name: "int".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 1,
            span: create_span(),
        };
        
        let resolved = ArrayAwareTypeResolver::resolve_type_ref(&type_ref);
        assert_eq!(resolved, "[I");
    }
    
    #[test]
    fn test_array_type_identifier() {
        let identifier = ArrayAwareTypeResolver::create_array_type_identifier("int", 1);
        assert_eq!(identifier, "[I");
        
        let identifier2 = ArrayAwareTypeResolver::create_array_type_identifier("java.lang.String", 2);
        assert_eq!(identifier2, "[[Ljava/lang/String;");
    }
}