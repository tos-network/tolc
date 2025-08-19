/// Intelligent cast optimization (javac-style)
/// Only generates checkcast when necessary based on type hierarchy

use crate::ast::TypeRef;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeHierarchy {
    /// Maps class names to their superclass
    superclasses: HashMap<String, String>,
    /// Maps class names to their implemented interfaces
    interfaces: HashMap<String, Vec<String>>,
    /// Cache for subtype relationships
    subtype_cache: HashMap<(String, String), bool>,
}

impl TypeHierarchy {
    pub fn new() -> Self {
        let mut hierarchy = TypeHierarchy {
            superclasses: HashMap::new(),
            interfaces: HashMap::new(),
            subtype_cache: HashMap::new(),
        };
        
        // Initialize with basic Java type hierarchy
        hierarchy.add_basic_java_types();
        hierarchy
    }
    
    fn add_basic_java_types(&mut self) {
        // Object is the root of all reference types
        self.superclasses.insert("java/lang/Object".to_string(), "".to_string());
        
        // Common Java classes
        self.superclasses.insert("java/lang/String".to_string(), "java/lang/Object".to_string());
        self.superclasses.insert("java/lang/Number".to_string(), "java/lang/Object".to_string());
        self.superclasses.insert("java/lang/Integer".to_string(), "java/lang/Number".to_string());
        self.superclasses.insert("java/lang/Long".to_string(), "java/lang/Number".to_string());
        self.superclasses.insert("java/lang/Float".to_string(), "java/lang/Number".to_string());
        self.superclasses.insert("java/lang/Double".to_string(), "java/lang/Number".to_string());
        
        // Collections
        self.superclasses.insert("java/util/AbstractCollection".to_string(), "java/lang/Object".to_string());
        self.superclasses.insert("java/util/AbstractList".to_string(), "java/util/AbstractCollection".to_string());
        self.superclasses.insert("java/util/ArrayList".to_string(), "java/util/AbstractList".to_string());
        self.superclasses.insert("java/util/LinkedList".to_string(), "java/util/AbstractList".to_string());
        
        // Interfaces
        self.interfaces.insert("java/util/List".to_string(), vec!["java/util/Collection".to_string()]);
        self.interfaces.insert("java/util/Collection".to_string(), vec!["java/lang/Iterable".to_string()]);
        self.interfaces.insert("java/util/ArrayList".to_string(), vec!["java/util/List".to_string()]);
        self.interfaces.insert("java/util/LinkedList".to_string(), vec!["java/util/List".to_string(), "java/util/Deque".to_string()]);
    }
    
    pub fn add_class(&mut self, class_name: String, superclass: Option<String>, interfaces: Vec<String>) {
        if let Some(super_name) = superclass {
            self.superclasses.insert(class_name.clone(), super_name);
        }
        if !interfaces.is_empty() {
            self.interfaces.insert(class_name, interfaces);
        }
    }
    
    /// Check if source_type is a subtype of target_type (javac algorithm)
    pub fn is_subtype(&mut self, source_type: &str, target_type: &str) -> bool {
        // Same type
        if source_type == target_type {
            return true;
        }
        
        // Check cache first
        let cache_key = (source_type.to_string(), target_type.to_string());
        if let Some(&cached_result) = self.subtype_cache.get(&cache_key) {
            return cached_result;
        }
        
        let result = self.is_subtype_uncached(source_type, target_type);
        self.subtype_cache.insert(cache_key, result);
        result
    }
    
    fn is_subtype_uncached(&self, source_type: &str, target_type: &str) -> bool {
        // Object is supertype of all reference types
        if target_type == "java/lang/Object" && !self.is_primitive_type(source_type) {
            return true;
        }
        
        // Check superclass chain
        if self.is_superclass(source_type, target_type) {
            return true;
        }
        
        // Check interface implementation
        if self.implements_interface(source_type, target_type) {
            return true;
        }
        
        false
    }
    
    fn is_superclass(&self, source_type: &str, target_type: &str) -> bool {
        let mut current = source_type;
        let mut visited = std::collections::HashSet::new();
        
        while let Some(superclass) = self.superclasses.get(current) {
            if visited.contains(current) {
                break; // Avoid infinite loops
            }
            visited.insert(current);
            
            if superclass == target_type {
                return true;
            }
            if superclass.is_empty() {
                break;
            }
            current = superclass;
        }
        
        false
    }
    
    fn implements_interface(&self, source_type: &str, target_interface: &str) -> bool {
        // Check direct interfaces
        if let Some(interfaces) = self.interfaces.get(source_type) {
            if interfaces.contains(&target_interface.to_string()) {
                return true;
            }
            
            // Check interface hierarchy
            for interface in interfaces {
                if self.implements_interface(interface, target_interface) {
                    return true;
                }
            }
        }
        
        // Check superclass interfaces
        if let Some(superclass) = self.superclasses.get(source_type) {
            if !superclass.is_empty() && self.implements_interface(superclass, target_interface) {
                return true;
            }
        }
        
        false
    }
    
    fn is_primitive_type(&self, type_name: &str) -> bool {
        matches!(type_name, "I" | "J" | "F" | "D" | "Z" | "B" | "C" | "S" | "V")
    }
    
    /// Convert TypeRef to internal type name
    pub fn type_ref_to_internal(&self, type_ref: &TypeRef) -> String {
        if type_ref.array_dims > 0 {
            // Array type
            let mut result = "[".repeat(type_ref.array_dims as usize);
            if self.is_primitive_type(&type_ref.name) {
                result.push_str(&type_ref.name);
            } else {
                result.push('L');
                result.push_str(&type_ref.name.replace('.', "/"));
                result.push(';');
            }
            result
        } else if self.is_primitive_type(&type_ref.name) {
            type_ref.name.clone()
        } else {
            type_ref.name.replace('.', "/")
        }
    }
}

pub struct CastOptimizer {
    type_hierarchy: TypeHierarchy,
}

impl CastOptimizer {
    pub fn new() -> Self {
        CastOptimizer {
            type_hierarchy: TypeHierarchy::new(),
        }
    }
    
    pub fn with_hierarchy(type_hierarchy: TypeHierarchy) -> Self {
        CastOptimizer { type_hierarchy }
    }
    
    /// Determine if checkcast is needed (javac algorithm)
    pub fn needs_checkcast(&mut self, source_type: &TypeRef, target_type: &TypeRef) -> bool {
        // No cast needed for primitive types (handled by coercion)
        if self.is_primitive_type_ref(source_type) || self.is_primitive_type_ref(target_type) {
            return false;
        }
        
        let source_internal = self.type_hierarchy.type_ref_to_internal(source_type);
        let target_internal = self.type_hierarchy.type_ref_to_internal(target_type);
        
        // No cast needed if target is a supertype of source
        !self.type_hierarchy.is_subtype(&source_internal, &target_internal)
    }
    
    /// Special handling for null literals (javac pattern)
    pub fn needs_checkcast_for_null(&self, target_type: &TypeRef) -> bool {
        // Null needs checkcast for multi-dimensional arrays
        target_type.array_dims > 1
    }
    
    fn is_primitive_type_ref(&self, type_ref: &TypeRef) -> bool {
        type_ref.array_dims == 0 && self.type_hierarchy.is_primitive_type(&type_ref.name)
    }
    
    pub fn add_class_to_hierarchy(&mut self, class_name: String, superclass: Option<String>, interfaces: Vec<String>) {
        self.type_hierarchy.add_class(class_name, superclass, interfaces);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_subtype_relationships() {
        let mut hierarchy = TypeHierarchy::new();
        
        // String is subtype of Object
        assert!(hierarchy.is_subtype("java/lang/String", "java/lang/Object"));
        
        // Integer is subtype of Number and Object
        assert!(hierarchy.is_subtype("java/lang/Integer", "java/lang/Number"));
        assert!(hierarchy.is_subtype("java/lang/Integer", "java/lang/Object"));
        
        // ArrayList is subtype of AbstractList and Object
        assert!(hierarchy.is_subtype("java/util/ArrayList", "java/util/AbstractList"));
        assert!(hierarchy.is_subtype("java/util/ArrayList", "java/lang/Object"));
    }
    
    #[test]
    fn test_interface_implementation() {
        let mut hierarchy = TypeHierarchy::new();
        
        // ArrayList implements List
        assert!(hierarchy.is_subtype("java/util/ArrayList", "java/util/List"));
        
        // LinkedList implements List and Deque
        assert!(hierarchy.is_subtype("java/util/LinkedList", "java/util/List"));
        assert!(hierarchy.is_subtype("java/util/LinkedList", "java/util/Deque"));
    }
    
    #[test]
    fn test_cast_optimization() {
        let mut optimizer = CastOptimizer::new();
        
        let span = crate::ast::Span::from_to(0, 0, 0, 0);
        let string_type = TypeRef { name: "java.lang.String".to_string(), array_dims: 0, type_args: Vec::new(), annotations: Vec::new(), span };
        let object_type = TypeRef { name: "java.lang.Object".to_string(), array_dims: 0, type_args: Vec::new(), annotations: Vec::new(), span };
        let integer_type = TypeRef { name: "java.lang.Integer".to_string(), array_dims: 0, type_args: Vec::new(), annotations: Vec::new(), span };
        
        // String to Object - no cast needed (upcast)
        assert!(!optimizer.needs_checkcast(&string_type, &object_type));
        
        // Object to String - cast needed (downcast)
        assert!(optimizer.needs_checkcast(&object_type, &string_type));
        
        // String to Integer - cast needed (unrelated types)
        assert!(optimizer.needs_checkcast(&string_type, &integer_type));
    }
    
    #[test]
    fn test_null_cast_optimization() {
        let optimizer = CastOptimizer::new();
        
        let span = crate::ast::Span::from_to(0, 0, 0, 0);
        let single_array = TypeRef { name: "java.lang.String".to_string(), array_dims: 1, type_args: Vec::new(), annotations: Vec::new(), span };
        let multi_array = TypeRef { name: "java.lang.String".to_string(), array_dims: 2, type_args: Vec::new(), annotations: Vec::new(), span };
        
        // Single-dimensional array - no cast needed for null
        assert!(!optimizer.needs_checkcast_for_null(&single_array));
        
        // Multi-dimensional array - cast needed for null
        assert!(optimizer.needs_checkcast_for_null(&multi_array));
    }
}
