//! Java Generic Type Erasure Implementation
//! 
//! This module implements Java's type erasure mechanism, which converts generic types
//! to their raw types at compile time. This is essential for Java bytecode generation
//! as the JVM does not have native support for generics.

use crate::ast::*;
use crate::review::generics::{ReviewedType, TypeEnv};
use crate::error::{Error, Result};
use std::collections::{HashMap, HashSet};

/// Type erasure processor for Java generics
/// 
/// Handles the conversion of generic types to their erased forms according to
/// Java Language Specification rules for type erasure.
pub struct TypeErasureProcessor {
    /// Cache of erased types to avoid recomputation
    erasure_cache: HashMap<String, String>,
    /// Set of bridge methods that need to be generated
    bridge_methods: HashMap<String, Vec<BridgeMethod>>,
    /// Type parameter bounds information
    type_bounds: HashMap<String, Vec<String>>,
    /// Current class context for type parameter resolution
    current_class_context: Option<String>,
}

/// Bridge method information for generic method overrides
#[derive(Debug, Clone)]
pub struct BridgeMethod {
    /// Original method name
    pub name: String,
    /// Erased parameter types
    pub erased_params: Vec<String>,
    /// Erased return type
    pub erased_return: String,
    /// Target method with concrete types
    pub target_method: String,
    /// Target parameter types
    pub target_params: Vec<String>,
    /// Target return type
    pub target_return: String,
    /// Method flags (public, protected, etc.)
    pub flags: u16,
}

/// Type erasure result containing erased type and additional information
#[derive(Debug, Clone)]
pub struct ErasureResult {
    /// The erased type name
    pub erased_type: String,
    /// Whether this was a generic type that got erased
    pub was_generic: bool,
    /// Original generic type arguments (if any)
    pub original_args: Vec<String>,
    /// Bridge methods needed for this erasure
    pub bridge_methods: Vec<BridgeMethod>,
}

impl TypeErasureProcessor {
    /// Create a new type erasure processor
    pub fn new() -> Self {
        Self {
            erasure_cache: HashMap::new(),
            bridge_methods: HashMap::new(),
            type_bounds: HashMap::new(),
            current_class_context: None,
        }
    }

    /// Set the current class context for type parameter resolution
    pub fn set_class_context(&mut self, class_name: &str) {
        self.current_class_context = Some(class_name.to_string());
    }

    /// Erase a generic type according to Java type erasure rules
    /// 
    /// # Type Erasure Rules (JLS §4.6):
    /// 1. The erasure of a parameterized type G<T1,...,Tn> is |G|
    /// 2. The erasure of a nested type T.C is |T|.C
    /// 3. The erasure of an array type T[] is |T|[]
    /// 4. The erasure of a type variable is the erasure of its leftmost bound
    /// 5. The erasure of every other type is the type itself
    pub fn erase_type(&mut self, type_ref: &TypeRef, env: &TypeEnv) -> Result<ErasureResult> {
        let type_key = self.create_type_key(type_ref);
        
        // Check cache first
        if let Some(cached) = self.erasure_cache.get(&type_key) {
            return Ok(ErasureResult {
                erased_type: cached.clone(),
                was_generic: !type_ref.type_args.is_empty(),
                original_args: type_ref.type_args.iter()
                    .filter_map(|arg| match arg {
                        TypeArg::Type(t) => Some(t.name.clone()),
                        _ => None,
                    })
                    .collect(),
                bridge_methods: Vec::new(),
            });
        }

        let result = self.perform_erasure(type_ref, env)?;
        
        // Cache the result
        self.erasure_cache.insert(type_key, result.erased_type.clone());
        
        Ok(result)
    }

    /// Perform the actual type erasure
    fn perform_erasure(&mut self, type_ref: &TypeRef, env: &TypeEnv) -> Result<ErasureResult> {
        // Handle array types
        if type_ref.array_dims > 0 {
            let element_type = TypeRef {
                name: type_ref.name.clone(),
                type_args: type_ref.type_args.clone(),
                annotations: Vec::new(),
                array_dims: 0,
                span: type_ref.span,
            };
            
            let erased_element = self.perform_erasure(&element_type, env)?;
            let array_suffix = "[]".repeat(type_ref.array_dims as usize);
            
            return Ok(ErasureResult {
                erased_type: format!("{}{}", erased_element.erased_type, array_suffix),
                was_generic: erased_element.was_generic,
                original_args: erased_element.original_args,
                bridge_methods: erased_element.bridge_methods,
            });
        }

        // Handle primitive types (no erasure needed)
        if self.is_primitive_type(&type_ref.name) {
            return Ok(ErasureResult {
                erased_type: type_ref.name.clone(),
                was_generic: false,
                original_args: Vec::new(),
                bridge_methods: Vec::new(),
            });
        }

        // Handle type variables
        if self.is_type_variable(&type_ref.name, env) {
            return self.erase_type_variable(&type_ref.name, env);
        }
        
        // Handle single-letter type names (likely type variables)
        if type_ref.name.len() == 1 && type_ref.name.chars().next().unwrap().is_uppercase() {
            return Ok(ErasureResult {
                erased_type: "java.lang.Object".to_string(),
                was_generic: true,
                original_args: vec![type_ref.name.clone()],
                bridge_methods: Vec::new(),
            });
        }

        // Handle parameterized types
        if !type_ref.type_args.is_empty() {
            return self.erase_parameterized_type(type_ref, env);
        }

        // Handle raw types (no type arguments)
        Ok(ErasureResult {
            erased_type: self.normalize_class_name(&type_ref.name),
            was_generic: false,
            original_args: Vec::new(),
            bridge_methods: Vec::new(),
        })
    }

    /// Erase a type variable to its leftmost bound
    fn erase_type_variable(&mut self, var_name: &str, env: &TypeEnv) -> Result<ErasureResult> {
        // Look up the type variable in the environment
        if let Some((bounds, _)) = env.method_tparams.get(var_name)
            .or_else(|| env.class_tparams.get(var_name)) {
            
            if !bounds.is_empty() {
                // Use the leftmost bound (first bound)
                let leftmost_bound = &bounds[0];
                let bound_name = self.extract_type_name_from_reviewed_type(leftmost_bound);
                
                return Ok(ErasureResult {
                    erased_type: bound_name,
                    was_generic: true,
                    original_args: vec![var_name.to_string()],
                    bridge_methods: Vec::new(),
                });
            }
        }

        // If no bounds found, erase to Object
        Ok(ErasureResult {
            erased_type: "java.lang.Object".to_string(),
            was_generic: true,
            original_args: vec![var_name.to_string()],
            bridge_methods: Vec::new(),
        })
    }

    /// Erase a parameterized type by removing type arguments
    fn erase_parameterized_type(&mut self, type_ref: &TypeRef, _env: &TypeEnv) -> Result<ErasureResult> {
        let original_args: Vec<String> = type_ref.type_args.iter()
            .filter_map(|arg| match arg {
                TypeArg::Type(t) => Some(t.name.clone()),
                TypeArg::Wildcard(_) => Some("?".to_string()),
            })
            .collect();

        Ok(ErasureResult {
            erased_type: self.normalize_class_name(&type_ref.name),
            was_generic: true,
            original_args,
            bridge_methods: Vec::new(),
        })
    }

    /// Generate bridge methods for generic method overrides
    /// 
    /// Bridge methods are synthetic methods generated by the compiler to preserve
    /// type safety when generic methods are overridden with different type parameters.
    pub fn generate_bridge_methods(&mut self, class: &ClassDecl, env: &TypeEnv) -> Result<Vec<BridgeMethod>> {
        let mut bridges = Vec::new();
        
        // Extract methods from class body
        for member in &class.body {
            if let ClassMember::Method(method) = member {
                if let Some(method_bridges) = self.analyze_method_for_bridges(method, class, env)? {
                    bridges.extend(method_bridges);
                }
            }
        }

        // Store bridge methods for this class
        if !bridges.is_empty() {
            self.bridge_methods.insert(class.name.clone(), bridges.clone());
        }

        Ok(bridges)
    }

    /// Analyze a method to determine if bridge methods are needed
    fn analyze_method_for_bridges(&mut self, method: &MethodDecl, class: &ClassDecl, env: &TypeEnv) -> Result<Option<Vec<BridgeMethod>>> {
        // Bridge methods are needed when:
        // 1. The method overrides a generic method from a superclass/interface
        // 2. The method has different erased signatures than the overridden method
        
        if method.name == "<init>" || method.name == "<clinit>" {
            return Ok(None); // No bridge methods for constructors
        }

        let mut bridges = Vec::new();

        // Check if this method overrides any generic methods
        if let Some(overridden_methods) = self.find_overridden_generic_methods(method, class)? {
            for overridden in overridden_methods {
                if let Some(bridge) = self.create_bridge_method(method, &overridden, env)? {
                    bridges.push(bridge);
                }
            }
        }

        if bridges.is_empty() {
            Ok(None)
        } else {
            Ok(Some(bridges))
        }
    }

    /// Find generic methods that this method overrides
    fn find_overridden_generic_methods(&self, method: &MethodDecl, class: &ClassDecl) -> Result<Option<Vec<MethodSignature>>> {
        let mut overridden = Vec::new();

        // Check superclass methods
        if let Some(super_name) = &class.extends {
            if let Some(super_methods) = self.get_class_generic_methods(&super_name.name) {
                for super_method in super_methods {
                    if super_method.name == method.name && 
                       self.methods_have_compatible_signatures(method, &super_method) {
                        overridden.push(super_method);
                    }
                }
            }
        }

        // Check interface methods
        for interface in &class.implements {
            if let Some(interface_methods) = self.get_class_generic_methods(&interface.name) {
                for interface_method in interface_methods {
                    if interface_method.name == method.name && 
                       self.methods_have_compatible_signatures(method, &interface_method) {
                        overridden.push(interface_method);
                    }
                }
            }
        }

        if overridden.is_empty() {
            Ok(None)
        } else {
            Ok(Some(overridden))
        }
    }

    /// Create a bridge method if needed
    fn create_bridge_method(&mut self, concrete_method: &MethodDecl, generic_method: &MethodSignature, env: &TypeEnv) -> Result<Option<BridgeMethod>> {
        // Erase both method signatures
        let concrete_erased = self.erase_method_signature(concrete_method, env)?;
        let generic_erased = self.erase_generic_method_signature(generic_method, env)?;

        // If erased signatures are different, we need a bridge method
        if concrete_erased != generic_erased {
            let bridge = BridgeMethod {
                name: concrete_method.name.clone(),
                erased_params: generic_erased.params,
                erased_return: generic_erased.return_type,
                target_method: concrete_method.name.clone(),
                target_params: concrete_erased.params,
                target_return: concrete_erased.return_type,
                flags: self.calculate_bridge_method_flags(concrete_method),
            };

            Ok(Some(bridge))
        } else {
            Ok(None)
        }
    }

    /// Erase a method signature
    fn erase_method_signature(&mut self, method: &MethodDecl, env: &TypeEnv) -> Result<ErasedMethodSignature> {
        let mut erased_params = Vec::new();
        
        for param in &method.parameters {
            let erased = self.erase_type(&param.type_ref, env)?;
            erased_params.push(erased.erased_type);
        }

        let erased_return = if let Some(return_type) = &method.return_type {
            self.erase_type(return_type, env)?.erased_type
        } else {
            "void".to_string()
        };

        Ok(ErasedMethodSignature {
            params: erased_params,
            return_type: erased_return,
        })
    }

    /// Erase a generic method signature
    fn erase_generic_method_signature(&mut self, method: &MethodSignature, env: &TypeEnv) -> Result<ErasedMethodSignature> {
        // This is a simplified implementation - in a full compiler,
        // we would need to properly handle the generic method's type parameters
        Ok(ErasedMethodSignature {
            params: method.param_types.clone(),
            return_type: method.return_type.clone(),
        })
    }

    /// Calculate flags for bridge methods
    fn calculate_bridge_method_flags(&self, original_method: &MethodDecl) -> u16 {
        let mut flags = 0u16;
        
        // Bridge methods are always synthetic and bridge
        flags |= 0x1000; // ACC_SYNTHETIC
        flags |= 0x0040; // ACC_BRIDGE
        
        // Copy access modifiers from original method
        for modifier in &original_method.modifiers {
            match modifier {
                Modifier::Public => flags |= 0x0001,
                Modifier::Protected => flags |= 0x0004,
                Modifier::Private => flags |= 0x0002,
                Modifier::Static => flags |= 0x0008,
                Modifier::Final => flags |= 0x0010,
                _ => {} // Other modifiers don't apply to bridge methods
            }
        }

        flags
    }

    /// Utility methods
    
    fn create_type_key(&self, type_ref: &TypeRef) -> String {
        if type_ref.type_args.is_empty() {
            format!("{}[{}]", type_ref.name, type_ref.array_dims)
        } else {
            let args: Vec<String> = type_ref.type_args.iter()
                .map(|arg| match arg {
                    TypeArg::Type(t) => t.name.clone(),
                    TypeArg::Wildcard(_) => "?".to_string(),
                })
                .collect();
            format!("{}[{}]<{}>", type_ref.name, type_ref.array_dims, args.join(","))
        }
    }

    fn is_primitive_type(&self, type_name: &str) -> bool {
        matches!(type_name, 
            "boolean" | "char" | "byte" | "short" | 
            "int" | "long" | "float" | "double" | "void"
        )
    }

    fn is_type_variable(&self, type_name: &str, env: &TypeEnv) -> bool {
        env.method_tparams.contains_key(type_name) || 
        env.class_tparams.contains_key(type_name)
    }

    fn normalize_class_name(&self, name: &str) -> String {
        // Convert simple names to fully qualified names where appropriate
        match name {
            "Object" => "java.lang.Object".to_string(),
            "String" => "java.lang.String".to_string(),
            "Class" => "java.lang.Class".to_string(),
            _ => name.to_string(),
        }
    }

    fn extract_type_name_from_reviewed_type(&self, reviewed_type: &ReviewedType) -> String {
        match reviewed_type {
            ReviewedType::Primitive(name) => name.to_string(),
            ReviewedType::Class { name, .. } => name.clone(),
            ReviewedType::Array { of } => {
                format!("{}[]", self.extract_type_name_from_reviewed_type(of))
            }
            ReviewedType::TypeVar { name, .. } => name.clone(),
            ReviewedType::Object => "java.lang.Object".to_string(),
            ReviewedType::Null => "java.lang.Object".to_string(),
            ReviewedType::Wildcard { extends: Some(bound), .. } => {
                self.extract_type_name_from_reviewed_type(bound)
            }
            ReviewedType::Wildcard { .. } => "java.lang.Object".to_string(),
        }
    }

    fn get_class_generic_methods(&self, _class_name: &str) -> Option<Vec<MethodSignature>> {
        // This would be implemented to look up generic methods from the class hierarchy
        // For now, return None as this requires more complex type system integration
        None
    }

    fn methods_have_compatible_signatures(&self, _method1: &MethodDecl, _method2: &MethodSignature) -> bool {
        // This would check if two methods have compatible signatures for overriding
        // For now, return false as this requires more complex signature analysis
        false
    }
}

/// Simplified method signature for erasure analysis
#[derive(Debug, Clone)]
struct MethodSignature {
    name: String,
    param_types: Vec<String>,
    return_type: String,
}

/// Erased method signature
#[derive(Debug, Clone, PartialEq)]
struct ErasedMethodSignature {
    params: Vec<String>,
    return_type: String,
}

/// Type erasure utilities
impl TypeErasureProcessor {
    /// Erase all generic types in a class declaration
    pub fn erase_class(&mut self, class: &mut ClassDecl, env: &TypeEnv) -> Result<()> {
        self.set_class_context(&class.name);

        // Erase superclass type arguments
        if let Some(extends) = &mut class.extends {
            let erased = self.erase_type(extends, env)?;
            extends.type_args.clear(); // Remove type arguments
            extends.name = erased.erased_type;
        }

        // Erase interface type arguments
        for interface in &mut class.implements {
            let erased = self.erase_type(interface, env)?;
            interface.type_args.clear(); // Remove type arguments
            interface.name = erased.erased_type;
        }

        // Erase field and method types from class body
        for member in &mut class.body {
            match member {
                ClassMember::Field(field) => {
                    let erased = self.erase_type(&field.type_ref, env)?;
                    field.type_ref.type_args.clear(); // Remove type arguments
                    field.type_ref.name = erased.erased_type;
                }
                ClassMember::Method(method) => {
                    self.erase_method(method, env)?;
                }
                _ => {} // Skip other member types
            }
        }

        // Generate bridge methods
        let bridge_methods = self.generate_bridge_methods(class, env)?;
        
        // Add bridge methods to the class (this would need to be implemented
        // in the actual AST structure to support synthetic methods)
        if !bridge_methods.is_empty() {
            eprintln!("Generated {} bridge methods for class {}", bridge_methods.len(), class.name);
        }

        Ok(())
    }

    /// Erase generic types in a method declaration
    pub fn erase_method(&mut self, method: &mut MethodDecl, env: &TypeEnv) -> Result<()> {
        // Erase parameter types
        for param in &mut method.parameters {
            let erased = self.erase_type(&param.type_ref, env)?;
            param.type_ref.type_args.clear(); // Remove type arguments
            param.type_ref.name = erased.erased_type;
        }

        // Erase return type
        if let Some(return_type) = &mut method.return_type {
            let erased = self.erase_type(return_type, env)?;
            return_type.type_args.clear(); // Remove type arguments
            return_type.name = erased.erased_type;
        }

        // Clear type parameters (they don't exist in bytecode)
        method.type_params.clear();

        Ok(())
    }

    /// Get all bridge methods for a class
    pub fn get_bridge_methods(&self, class_name: &str) -> Option<&Vec<BridgeMethod>> {
        self.bridge_methods.get(class_name)
    }

    /// Clear the erasure cache
    pub fn clear_cache(&mut self) {
        self.erasure_cache.clear();
    }

    /// Get erasure statistics
    pub fn get_statistics(&self) -> ErasureStatistics {
        ErasureStatistics {
            cached_erasures: self.erasure_cache.len(),
            bridge_methods_generated: self.bridge_methods.values().map(|v| v.len()).sum(),
            classes_processed: self.bridge_methods.len(),
        }
    }
}

/// Statistics about type erasure processing
#[derive(Debug, Clone)]
pub struct ErasureStatistics {
    pub cached_erasures: usize,
    pub bridge_methods_generated: usize,
    pub classes_processed: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location};

    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }

    fn create_type_ref(name: &str, type_args: Vec<TypeArg>) -> TypeRef {
        TypeRef {
            name: name.to_string(),
            type_args,
            annotations: Vec::new(),
            array_dims: 0,
            span: create_span(),
        }
    }

    #[test]
    fn test_primitive_type_erasure() {
        let mut processor = TypeErasureProcessor::new();
        let env = TypeEnv::default();

        let int_type = create_type_ref("int", vec![]);
        let result = processor.erase_type(&int_type, &env).unwrap();

        assert_eq!(result.erased_type, "int");
        assert!(!result.was_generic);
        assert!(result.original_args.is_empty());
    }

    #[test]
    fn test_parameterized_type_erasure() {
        let mut processor = TypeErasureProcessor::new();
        let env = TypeEnv::default();

        let string_arg = TypeArg::Type(create_type_ref("String", vec![]));
        let list_type = create_type_ref("java.util.List", vec![string_arg]);
        let result = processor.erase_type(&list_type, &env).unwrap();

        assert_eq!(result.erased_type, "java.util.List");
        assert!(result.was_generic);
        assert_eq!(result.original_args, vec!["String"]);
    }

    #[test]
    fn test_array_type_erasure() {
        let mut processor = TypeErasureProcessor::new();
        let env = TypeEnv::default();

        let string_arg = TypeArg::Type(create_type_ref("String", vec![]));
        let mut list_type = create_type_ref("java.util.List", vec![string_arg]);
        list_type.array_dims = 2; // List<String>[][]

        let result = processor.erase_type(&list_type, &env).unwrap();

        assert_eq!(result.erased_type, "java.util.List[][]");
        assert!(result.was_generic);
    }

    #[test]
    fn test_type_variable_erasure_to_object() {
        let mut processor = TypeErasureProcessor::new();
        let env = TypeEnv::default();

        let type_var = create_type_ref("T", vec![]);
        let result = processor.erase_type(&type_var, &env).unwrap();

        assert_eq!(result.erased_type, "java.lang.Object");
        assert!(result.was_generic);
        assert_eq!(result.original_args, vec!["T"]);
    }

    #[test]
    fn test_erasure_caching() {
        let mut processor = TypeErasureProcessor::new();
        let env = TypeEnv::default();

        let string_arg = TypeArg::Type(create_type_ref("String", vec![]));
        let list_type = create_type_ref("java.util.List", vec![string_arg.clone()]);

        // First erasure
        let result1 = processor.erase_type(&list_type, &env).unwrap();
        
        // Second erasure (should use cache)
        let list_type2 = create_type_ref("java.util.List", vec![string_arg]);
        let result2 = processor.erase_type(&list_type2, &env).unwrap();

        assert_eq!(result1.erased_type, result2.erased_type);
        assert_eq!(processor.erasure_cache.len(), 1);
    }

    #[test]
    fn test_statistics() {
        let mut processor = TypeErasureProcessor::new();
        let env = TypeEnv::default();

        let string_arg = TypeArg::Type(create_type_ref("String", vec![]));
        let list_type = create_type_ref("java.util.List", vec![string_arg]);
        let _ = processor.erase_type(&list_type, &env).unwrap();

        let stats = processor.get_statistics();
        assert_eq!(stats.cached_erasures, 1);
    }
}
