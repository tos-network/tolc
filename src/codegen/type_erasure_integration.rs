//! Type Erasure Integration for tolc Compiler
//! 
//! This module integrates type erasure processing into the compilation pipeline,
//! handling the erasure of generic types across entire compilation units.

use crate::ast::*;
use crate::codegen::type_erasure::{TypeErasureProcessor, BridgeMethod, ErasureStatistics};
use crate::review::generics::TypeEnv;
// Removed GlobalMemberIndex dependency as it's private
use crate::error::{Error, Result};
use std::collections::HashMap;

/// Type erasure integration manager
/// 
/// Coordinates type erasure across an entire compilation unit,
/// managing the erasure of classes, interfaces, and their members.
pub struct TypeErasureIntegration {
    /// The type erasure processor
    processor: TypeErasureProcessor,
    /// Map of class names to their bridge methods
    class_bridge_methods: HashMap<String, Vec<BridgeMethod>>,
    /// Statistics about the erasure process
    statistics: ErasureStatistics,
    /// Whether to generate detailed erasure reports
    verbose_reporting: bool,
}

impl TypeErasureIntegration {
    /// Create a new type erasure integration manager
    pub fn new() -> Self {
        Self {
            processor: TypeErasureProcessor::new(),
            class_bridge_methods: HashMap::new(),
            statistics: ErasureStatistics {
                cached_erasures: 0,
                bridge_methods_generated: 0,
                classes_processed: 0,
            },
            verbose_reporting: false,
        }
    }

    /// Enable or disable verbose reporting
    pub fn set_verbose_reporting(&mut self, verbose: bool) {
        self.verbose_reporting = verbose;
    }

    /// Process type erasure for an entire AST
    pub fn process_ast(&mut self, ast: &mut Ast) -> Result<()> {
        if self.verbose_reporting {
            println!("🔄 Starting type erasure processing for compilation unit");
        }

        // Create a global type environment and index
        let mut global_env = TypeEnv::default();
        let global_index = self.build_global_index(ast)?;

        // Process each type declaration
        for type_decl in &mut ast.type_decls {
            match type_decl {
                TypeDecl::Class(class) => {
                    self.process_class(class, &mut global_env)?;
                }
                TypeDecl::Interface(interface) => {
                    self.process_interface(interface, &mut global_env)?;
                }
                TypeDecl::Enum(enum_decl) => {
                    self.process_enum(enum_decl, &mut global_env)?;
                }
                TypeDecl::Annotation(annotation) => {
                    self.process_annotation(annotation, &mut global_env)?;
                }
            }
        }

        // Update statistics
        self.statistics = self.processor.get_statistics();

        if self.verbose_reporting {
            self.print_erasure_report();
        }

        Ok(())
    }

    /// Process type erasure for a class
    fn process_class(&mut self, class: &mut ClassDecl, env: &mut TypeEnv) -> Result<()> {
        if self.verbose_reporting {
            println!("🔄 Processing class: {}", class.name);
        }

        // Set the class context in the processor
        self.processor.set_class_context(&class.name);

        // Build class-specific type environment
        let class_env = self.build_class_environment(class, env)?;

        // Erase the class
        self.processor.erase_class(class, &class_env)?;

        // Generate bridge methods
        let bridge_methods = self.processor.generate_bridge_methods(class, &class_env)?;
        if !bridge_methods.is_empty() {
            self.class_bridge_methods.insert(class.name.clone(), bridge_methods.clone());
            
            if self.verbose_reporting {
                println!("  ✅ Generated {} bridge methods for class {}", bridge_methods.len(), class.name);
            }
        }

        // Process nested classes (if any exist in class body)
        for member in &mut class.body {
            if let ClassMember::TypeDecl(TypeDecl::Class(nested_class)) = member {
                self.process_class(nested_class, env)?;
            }
        }

        Ok(())
    }

    /// Process type erasure for an interface
    fn process_interface(&mut self, interface: &mut InterfaceDecl, env: &mut TypeEnv) -> Result<()> {
        if self.verbose_reporting {
            println!("🔄 Processing interface: {}", interface.name);
        }

        // Set the interface context in the processor
        self.processor.set_class_context(&interface.name);

        // Build interface-specific type environment
        let interface_env = self.build_interface_environment(interface, env)?;

        // Erase interface types (similar to class erasure)
        self.erase_interface_types(interface, &interface_env)?;

        Ok(())
    }

    /// Process type erasure for an enum
    fn process_enum(&mut self, enum_decl: &mut EnumDecl, env: &mut TypeEnv) -> Result<()> {
        if self.verbose_reporting {
            println!("🔄 Processing enum: {}", enum_decl.name);
        }

        // Set the enum context in the processor
        self.processor.set_class_context(&enum_decl.name);

        // Erase enum types (enums can have generic methods)
        self.erase_enum_types(enum_decl, env)?;

        Ok(())
    }

    /// Process type erasure for an annotation
    fn process_annotation(&mut self, annotation: &mut AnnotationDecl, _env: &mut TypeEnv) -> Result<()> {
        if self.verbose_reporting {
            println!("🔄 Processing annotation: {}", annotation.name);
        }

        // Annotations typically don't have complex generic types to erase
        // but we process them for completeness
        Ok(())
    }

    /// Build a global type index for the compilation unit
    fn build_global_index(&self, ast: &Ast) -> Result<()> {
        // This is a simplified implementation
        // In a full compiler, this would build a comprehensive index of all types
        
        for type_decl in &ast.type_decls {
            match type_decl {
                TypeDecl::Class(class) => {
                    // Add class to index
                    // This would include superclass, interfaces, methods, fields, etc.
                    if self.verbose_reporting {
                        println!("  📝 Indexing class: {}", class.name);
                    }
                }
                TypeDecl::Interface(interface) => {
                    if self.verbose_reporting {
                        println!("  📝 Indexing interface: {}", interface.name);
                    }
                }
                TypeDecl::Enum(enum_decl) => {
                    if self.verbose_reporting {
                        println!("  📝 Indexing enum: {}", enum_decl.name);
                    }
                }
                TypeDecl::Annotation(annotation) => {
                    if self.verbose_reporting {
                        println!("  📝 Indexing annotation: {}", annotation.name);
                    }
                }
            }
        }

        Ok(())
    }

    /// Build a class-specific type environment
    fn build_class_environment(&self, class: &ClassDecl, base_env: &TypeEnv) -> Result<TypeEnv> {
        let mut env = base_env.clone();

        // Add class type parameters to the environment
        for type_param in &class.type_params {
            // Simplified implementation - just add empty bounds for now
            // In a full implementation, we would properly resolve the bounds
            let bounds = Vec::new();
            
            env.class_tparams.insert(type_param.name.clone(), (bounds, None));
        }

        Ok(env)
    }

    /// Build an interface-specific type environment
    fn build_interface_environment(&self, interface: &InterfaceDecl, base_env: &TypeEnv) -> Result<TypeEnv> {
        let mut env = base_env.clone();

        // Add interface type parameters to the environment
        for type_param in &interface.type_params {
            // Simplified implementation - just add empty bounds for now
            let bounds = Vec::new();
            
            env.class_tparams.insert(type_param.name.clone(), (bounds, None));
        }

        Ok(env)
    }

    /// Erase types in an interface declaration
    fn erase_interface_types(&mut self, interface: &mut InterfaceDecl, env: &TypeEnv) -> Result<()> {
        // Erase extends clause
        for extends in &mut interface.extends {
            let erased = self.processor.erase_type(extends, env)?;
            extends.type_args.clear();
            extends.name = erased.erased_type;
        }

        // Erase method signatures from interface body
        for member in &mut interface.body {
            if let InterfaceMember::Method(method) = member {
                self.erase_method_signature(method, env)?;
            }
        }

        Ok(())
    }

    /// Erase types in an enum declaration
    fn erase_enum_types(&mut self, enum_decl: &mut EnumDecl, env: &TypeEnv) -> Result<()> {
        // Erase implements clause
        for implements in &mut enum_decl.implements {
            let erased = self.processor.erase_type(implements, env)?;
            implements.type_args.clear();
            implements.name = erased.erased_type;
        }

        // Erase method signatures from enum body (enums can have methods)
        for member in &mut enum_decl.body {
            if let ClassMember::Method(method) = member {
                self.erase_method_signature(method, env)?;
            }
        }

        Ok(())
    }

    /// Erase types in a method signature
    fn erase_method_signature(&mut self, method: &mut MethodDecl, env: &TypeEnv) -> Result<()> {
        // Erase parameter types
        for param in &mut method.parameters {
            let erased = self.processor.erase_type(&param.type_ref, env)?;
            param.type_ref.type_args.clear();
            param.type_ref.name = erased.erased_type;
        }

        // Erase return type
        if let Some(return_type) = &mut method.return_type {
            let erased = self.processor.erase_type(return_type, env)?;
            return_type.type_args.clear();
            return_type.name = erased.erased_type;
        }

        // Clear type parameters (they don't exist in bytecode)
        method.type_params.clear();

        Ok(())
    }

    /// Print a detailed erasure report
    fn print_erasure_report(&self) {
        println!("\n📊 Type Erasure Report");
        println!("======================");
        println!("Classes processed: {}", self.statistics.classes_processed);
        println!("Cached erasures: {}", self.statistics.cached_erasures);
        println!("Bridge methods generated: {}", self.statistics.bridge_methods_generated);
        
        if !self.class_bridge_methods.is_empty() {
            println!("\n🌉 Bridge Methods by Class:");
            for (class_name, bridge_methods) in &self.class_bridge_methods {
                println!("  {}: {} bridge methods", class_name, bridge_methods.len());
                for bridge in bridge_methods {
                    println!("    - {} -> {}", bridge.name, bridge.target_method);
                }
            }
        }
        
        println!("======================\n");
    }

    /// Get bridge methods for a specific class
    pub fn get_bridge_methods_for_class(&self, class_name: &str) -> Option<&Vec<BridgeMethod>> {
        self.class_bridge_methods.get(class_name)
    }

    /// Get erasure statistics
    pub fn get_statistics(&self) -> &ErasureStatistics {
        &self.statistics
    }

    /// Clear all cached data
    pub fn clear_cache(&mut self) {
        self.processor.clear_cache();
        self.class_bridge_methods.clear();
    }
}

/// Utility functions for type erasure integration
impl TypeErasureIntegration {
    /// Check if a type reference needs erasure
    pub fn needs_erasure(type_ref: &TypeRef) -> bool {
        // A type needs erasure if:
        // 1. It has type arguments (parameterized type)
        // 2. It's a type variable
        // 3. It contains wildcards
        !type_ref.type_args.is_empty() || 
        type_ref.name.len() == 1 || // Single letter (likely type variable)
        type_ref.type_args.iter().any(|arg| matches!(arg, TypeArg::Wildcard(_)))
    }

    /// Get the erased form of a type name
    pub fn get_erased_type_name(type_ref: &TypeRef) -> String {
        // Simple erasure: remove type arguments and return raw type
        if type_ref.array_dims > 0 {
            format!("{}{}", type_ref.name, "[]".repeat(type_ref.array_dims as usize))
        } else {
            type_ref.name.clone()
        }
    }

    /// Check if two method signatures are bridge-compatible
    pub fn are_bridge_compatible(method1: &MethodDecl, method2: &MethodDecl) -> bool {
        // Methods are bridge-compatible if they have the same name
        // and compatible parameter counts
        method1.name == method2.name && method1.parameters.len() == method2.parameters.len()
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
    fn test_needs_erasure() {
        let simple_type = TypeRef {
            name: "String".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: create_span(),
        };
        assert!(!TypeErasureIntegration::needs_erasure(&simple_type));

        let generic_type = TypeRef {
            name: "List".to_string(),
            type_args: vec![TypeArg::Type(simple_type.clone())],
            annotations: vec![],
            array_dims: 0,
            span: create_span(),
        };
        assert!(TypeErasureIntegration::needs_erasure(&generic_type));

        let type_var = TypeRef {
            name: "T".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: create_span(),
        };
        assert!(TypeErasureIntegration::needs_erasure(&type_var));
    }

    #[test]
    fn test_get_erased_type_name() {
        let simple_type = TypeRef {
            name: "String".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: create_span(),
        };
        assert_eq!(TypeErasureIntegration::get_erased_type_name(&simple_type), "String");

        let array_type = TypeRef {
            name: "String".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 2,
            span: create_span(),
        };
        assert_eq!(TypeErasureIntegration::get_erased_type_name(&array_type), "String[][]");
    }

    #[test]
    fn test_integration_creation() {
        let integration = TypeErasureIntegration::new();
        let stats = integration.get_statistics();
        assert_eq!(stats.classes_processed, 0);
        assert_eq!(stats.cached_erasures, 0);
        assert_eq!(stats.bridge_methods_generated, 0);
    }
}
