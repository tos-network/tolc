//! Compilation Context - Unified compilation state management
//!
//! This module provides the central CompilationContext that manages:
//! - ClassManager for dynamic class/method/field lookup
//! - TypeResolver for type inference and validation
//! - ImportResolver for import statement processing
//! - Shared state across all compilation phases
//! - Error collection and reporting

use std::collections::HashMap;
use crate::ast::TypeEnum;
use crate::common::error::{Result, Error};
use crate::common::class_manager::{ClassManager, ClassInfo, MethodInfo, FieldInfo};
// Note: TypeResolver and ImportResolver will be integrated later
use crate::common::env::SymbolEnvironment;
use crate::Config;

/// Compilation phase tracking
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompilePhase {
    Parse,
    Enter,
    Attr,
    Flow,
    TransTypes,
    Lower,
    CodeGen,
}

/// Unified compilation context shared across all phases
#[derive(Debug)]
pub struct CompilationContext {
    /// Core class management system
    pub class_manager: ClassManager,
    
    /// Compilation configuration
    pub config: Config,
    
    /// Current compilation phase
    pub current_phase: CompilePhase,
    
    /// Accumulated compilation errors
    pub errors: Vec<Error>,
    
    /// Symbol environment (shared across phases)
    pub symbol_env: Option<SymbolEnvironment>,
    
    /// Type information from Attr phase
    pub type_info: Option<HashMap<String, TypeEnum>>,
    
    /// Flow analysis results
    pub flow_analysis: Option<HashMap<String, bool>>, // variable -> definitely_assigned
}

impl CompilationContext {
    /// Create new compilation context with classpath
    pub fn new(classpath: &str, config: Config) -> Self {
        eprintln!("🏗️  CONTEXT: Initializing CompilationContext with classpath: {}", classpath);
        
        // For explicit classpath, use with_classpath method
        let mut class_manager = ClassManager::with_classpath(classpath);
        
        // Preload common packages for better performance
        if let Err(e) = class_manager.preload_common_packages() {
            eprintln!("⚠️  CONTEXT: Failed to preload common packages: {}", e);
        }
        
        Self {
            class_manager,
            config,
            current_phase: CompilePhase::Parse,
            errors: Vec::new(),
            symbol_env: None,
            type_info: None,
            flow_analysis: None,
        }
    }
    
    /// Set current compilation phase
    pub fn set_phase(&mut self, phase: CompilePhase) {
        eprintln!("📍 CONTEXT: Entering phase {:?}", phase);
        self.current_phase = phase;
    }
    
    /// Add compilation error
    pub fn add_error(&mut self, error: Error) {
        eprintln!("❌ CONTEXT: Error in phase {:?}: {}", self.current_phase, error);
        self.errors.push(error);
    }
    
    /// Check if there are compilation errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    /// Get all compilation errors
    pub fn get_errors(&self) -> &[Error] {
        &self.errors
    }
    
    /// Clear all errors
    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }
    
    // === Unified Class Resolution Interface ===
    
    /// Resolve class by name (handles both simple and qualified names)
    pub fn resolve_class(&mut self, name: &str) -> Option<ClassInfo> {
        if let Some(class_info) = self.class_manager.get_class_info(name) {
            return Some(class_info);
        }
        
        // Try simple name resolution if not found
        if !name.contains('.') {
            if let Some(qualified_name) = self.class_manager.find_class_by_simple_name(name) {
                return self.class_manager.get_class_info(&qualified_name);
            }
        }
        
        self.add_error(Error::Other { 
            message: format!("Class not found: {}", name) 
        });
        None
    }
    
    /// Resolve method by class, method name and argument types
    pub fn resolve_method(&mut self, class_name: &str, method_name: &str, args: &[TypeEnum]) -> Option<MethodInfo> {
        // Convert TypeEnum to String for ClassManager
        let param_types: Vec<String> = args.iter().map(|t| self.type_enum_to_string(t)).collect();
        
        if let Some(method_info) = self.class_manager.find_method(class_name, method_name, &param_types) {
            return Some(method_info);
        }
        
        self.add_error(Error::Other { 
            message: format!("Method not found: {}#{}", class_name, method_name) 
        });
        None
    }
    
    /// Resolve field by class and field name
    pub fn resolve_field(&mut self, class_name: &str, field_name: &str) -> Option<FieldInfo> {
        if let Some(field_info) = self.class_manager.find_field(class_name, field_name) {
            return Some(field_info);
        }
        
        self.add_error(Error::Other { 
            message: format!("Field not found: {}.{}", class_name, field_name) 
        });
        None
    }
    
    /// Resolve type name (handles simple names like "String" -> "java.lang.String")
    pub fn resolve_type_name(&mut self, type_name: &str) -> String {
        // If already qualified, return as-is
        if type_name.contains('.') {
            return type_name.to_string();
        }
        
        // Try simple name resolution
        if let Some(qualified_name) = self.class_manager.find_class_by_simple_name(type_name) {
            return qualified_name;
        }
        
        // Default to original name
        type_name.to_string()
    }
    
    /// Check type compatibility (subclass relationship)
    pub fn check_type_compatibility(&mut self, from_type: &str, to_type: &str) -> bool {
        if from_type == to_type {
            return true;
        }
        
        // Check if from_type is a subclass of to_type
        self.class_manager.is_subclass_of(from_type, to_type)
    }
    
    /// Generate method signature for bytecode generation
    pub fn get_method_signature(&mut self, class_name: &str, method_name: &str, args: &[TypeEnum]) -> Option<String> {
        if let Some(method_info) = self.resolve_method(class_name, method_name, args) {
            // Generate JVM method descriptor
            let param_descriptors: Vec<String> = method_info.parameters.iter()
                .map(|param_type| self.type_to_descriptor(param_type))
                .collect();
            let return_descriptor = self.type_to_descriptor(&method_info.return_type);
            
            Some(format!("({}){}",
                param_descriptors.join(""),
                return_descriptor
            ))
        } else {
            None
        }
    }
    
    /// Validate class hierarchy (check for circular inheritance)
    pub fn validate_class_hierarchy(&mut self, class_name: &str) -> Result<()> {
        // TODO: Implement circular inheritance detection
        if self.class_manager.get_class_info(class_name).is_some() {
            Ok(())
        } else {
            Err(Error::Other { 
                message: format!("Invalid class hierarchy for: {}", class_name) 
            })
        }
    }
    
    /// Get all dependencies for a class
    pub fn get_all_dependencies(&mut self, class_name: &str) -> Vec<String> {
        let mut dependencies = Vec::new();
        
        if let Some(class_info) = self.class_manager.get_class_info(class_name) {
            // Add parent class
            if let Some(ref parent) = class_info.parent_class {
                dependencies.push(parent.clone());
            }
            
            // Add interfaces
            dependencies.extend(class_info.interfaces.clone());
            
            // TODO: Add field types and method parameter/return types
        }
        
        dependencies
    }
    
    // === State Management ===
    
    /// Set symbol environment from Enter phase
    pub fn set_symbol_env(&mut self, env: SymbolEnvironment) {
        eprintln!("📊 CONTEXT: Setting symbol environment with {} classes", 
                 env.classes.len());
        self.symbol_env = Some(env);
    }
    
    /// Get symbol environment
    pub fn get_symbol_env(&self) -> Option<&SymbolEnvironment> {
        self.symbol_env.as_ref()
    }
    
    /// Set type information from Attr phase
    pub fn set_type_info(&mut self, info: HashMap<String, TypeEnum>) {
        eprintln!("📊 CONTEXT: Setting type information with {} entries", info.len());
        self.type_info = Some(info);
    }
    
    /// Get type information
    pub fn get_type_info(&self) -> Option<&HashMap<String, TypeEnum>> {
        self.type_info.as_ref()
    }
    
    /// Set flow analysis results
    pub fn set_flow_analysis(&mut self, analysis: HashMap<String, bool>) {
        eprintln!("📊 CONTEXT: Setting flow analysis with {} variables", analysis.len());
        self.flow_analysis = Some(analysis);
    }
    
    /// Get flow analysis results
    pub fn get_flow_analysis(&self) -> Option<&HashMap<String, bool>> {
        self.flow_analysis.as_ref()
    }
    
    // === Helper Methods ===
    
    /// Convert TypeEnum to String for ClassManager interface
    fn type_enum_to_string(&self, type_enum: &TypeEnum) -> String {
        match type_enum {
            TypeEnum::Primitive(prim) => format!("{:?}", prim).to_lowercase(),
            TypeEnum::Reference(ref_type) => format!("{:?}", ref_type), // Temporary fix
            TypeEnum::Void => "void".to_string(),
        }
    }
    
    /// Convert type string to JVM descriptor
    fn type_to_descriptor(&self, type_str: &str) -> String {
        match type_str {
            "void" => "V".to_string(),
            "boolean" => "Z".to_string(),
            "byte" => "B".to_string(),
            "char" => "C".to_string(),
            "short" => "S".to_string(),
            "int" => "I".to_string(),
            "long" => "J".to_string(),
            "float" => "F".to_string(),
            "double" => "D".to_string(),
            _ => {
                // Reference type
                if type_str.ends_with("[]") {
                    format!("[{}", self.type_to_descriptor(&type_str[..type_str.len()-2]))
                } else {
                    format!("L{};", type_str.replace('.', "/"))
                }
            }
        }
    }
}

impl Clone for CompilationContext {
    fn clone(&self) -> Self {
        // Note: We create a new ClassManager instead of cloning to avoid cache duplication
        let classpath = if self.class_manager.get_classpath_entries().is_empty() {
            ".".to_string()
        } else {
            self.class_manager.get_classpath_entries()[0].to_string_lossy().to_string()
        };
        
        let mut new_context = Self::new(&classpath, self.config.clone());
        
        new_context.current_phase = self.current_phase;
        // Note: Errors are not cloned (they don't implement Clone due to io::Error)
        // new_context.errors = self.errors.clone(); // Would fail - errors don't implement Clone
        new_context.errors = Vec::new(); // Start with empty error list
        new_context.symbol_env = self.symbol_env.clone();
        new_context.type_info = self.type_info.clone();
        new_context.flow_analysis = self.flow_analysis.clone();
        
        new_context
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_compilation_context_creation() {
        let config = Config::default();
        let mut context = CompilationContext::new("tests/", config);
        
        assert_eq!(context.current_phase, CompilePhase::Parse);
        assert!(!context.has_errors());
        
        // Test class resolution
        let system_class = context.resolve_class("java.lang.System");
        assert!(system_class.is_some());
        
        // Test simple name resolution
        let string_qualified = context.resolve_type_name("String");
        assert_eq!(string_qualified, "java.lang.String");
    }
    
    #[test]
    fn test_error_handling() {
        let config = Config::default();
        let mut context = CompilationContext::new("tests/", config);
        
        // Try to resolve non-existent class
        let result = context.resolve_class("com.nonexistent.Class");
        assert!(result.is_none());
        assert!(context.has_errors());
        
        let errors = context.get_errors();
        assert_eq!(errors.len(), 1);
    }
    
    #[test]
    fn test_phase_management() {
        let config = Config::default();
        let mut context = CompilationContext::new("tests/", config);
        
        context.set_phase(CompilePhase::Enter);
        assert_eq!(context.current_phase, CompilePhase::Enter);
        
        context.set_phase(CompilePhase::CodeGen);
        assert_eq!(context.current_phase, CompilePhase::CodeGen);
    }
}