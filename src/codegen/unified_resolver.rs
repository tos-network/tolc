//! UnifiedResolver - JavaC-aligned identifier resolution facade
//!
//! This module provides a unified interface for identifier resolution that builds
//! upon the existing SymbolEnvironment system, following JavaC's Resolve patterns.

use crate::common::env::SymbolEnvironment;
use crate::codegen::gen::{IdentifierResolution, ResolutionContext};
use crate::ast::{TypeRef, MethodDecl, ClassDecl};
use crate::common::error::Result;
use std::collections::HashMap;

/// UnifiedResolver - facade over SymbolEnvironment for JavaC-aligned resolution
/// This provides a clean, consistent interface that wraps our existing wash/enter system
pub struct UnifiedResolver {
    /// Reference to the underlying symbol environment
    symbol_env: SymbolEnvironment,
    
    /// Type resolution cache for performance
    type_cache: HashMap<String, String>,
    
    /// Method resolution cache
    method_cache: HashMap<String, String>,
    
    /// Current resolution context for debugging
    current_context: ResolutionContext,
}

impl UnifiedResolver {
    /// Create new resolver with existing symbol environment
    /// JavaC equivalent: Resolve(EnterContext env)
    pub fn new(symbol_env: SymbolEnvironment) -> Self {
        Self {
            symbol_env,
            type_cache: HashMap::new(),
            method_cache: HashMap::new(),
            current_context: ResolutionContext::NotFound,
        }
    }
    
    /// Main identifier resolution entry point - JavaC: resolveIdent
    /// This is the primary method that applications should use for identifier resolution
    pub fn resolve_identifier(&mut self, identifier: &str, context_class: Option<&str>, context_method: Option<&str>) -> Option<IdentifierResolution> {
        // Try type cache first for performance
        if let Some(cached_type) = self.type_cache.get(identifier) {
            self.current_context = ResolutionContext::Cached;
            return Some(IdentifierResolution {
                identifier: identifier.to_string(),
                resolved_type: cached_type.clone(),
                resolution_context: ResolutionContext::Cached,
                scope_depth: 0,
            });
        }
        
        // Delegate to symbol environment
        if let Some(resolved_type) = self.symbol_env.resolve_type(identifier) {
            // Cache the result for future lookups
            self.type_cache.insert(identifier.to_string(), resolved_type.clone());
            self.current_context = ResolutionContext::SymbolEnvironment;
            
            return Some(IdentifierResolution {
                identifier: identifier.to_string(),
                resolved_type,
                resolution_context: ResolutionContext::SymbolEnvironment,
                scope_depth: 0,
            });
        }
        
        // Try method resolution if we have context
        if let (Some(class_name), Some(method_name)) = (context_class, context_method) {
            let method_key = format!("{}#{}", class_name, method_name);
            if let Some(resolved) = self.resolve_method_context(identifier, &method_key) {
                return Some(resolved);
            }
        }
        
        // Try class field resolution
        if let Some(class_name) = context_class {
            if let Some(resolved) = self.resolve_class_field(identifier, class_name) {
                return Some(resolved);
            }
        }
        
        self.current_context = ResolutionContext::NotFound;
        None
    }
    
    /// Resolve method-scoped identifiers (parameters, local variables)
    /// JavaC equivalent: Resolve.findVar in method scope
    fn resolve_method_context(&mut self, identifier: &str, method_key: &str) -> Option<IdentifierResolution> {
        // Check method cache first
        let cache_key = format!("{}::{}", method_key, identifier);
        if let Some(cached_type) = self.method_cache.get(&cache_key) {
            return Some(IdentifierResolution {
                identifier: identifier.to_string(),
                resolved_type: cached_type.clone(),
                resolution_context: ResolutionContext::Parameter,
                scope_depth: 1,
            });
        }
        
        // Look up the variable directly in the SymbolEnvironment's variables HashMap
        // Use the same key format as Enter phase: "method:method_key::identifier"
        let variable_key = format!("method:{}::{}", method_key, identifier);
        if let Some(variable_symbol) = self.symbol_env.variables.get(&variable_key) {
            // Cache the result for future lookups
            self.method_cache.insert(cache_key, variable_symbol.var_type.clone());
            
            let resolution_context = if variable_symbol.is_parameter {
                ResolutionContext::Parameter
            } else {
                ResolutionContext::LocalVariable
            };
            
            return Some(IdentifierResolution {
                identifier: identifier.to_string(),
                resolved_type: variable_symbol.var_type.clone(),
                resolution_context,
                scope_depth: variable_symbol.local_slot.unwrap_or(1) as u16,
            });
        }
        
        None
    }
    
    /// Resolve class field identifiers
    /// JavaC equivalent: Resolve.findVar in class scope
    fn resolve_class_field(&mut self, identifier: &str, class_name: &str) -> Option<IdentifierResolution> {
        // Use the same field key format as Enter phase: "class:{}::{}" 
        let field_key = format!("class:{}::{}", class_name, identifier);
        
        // Look up field directly in the fields HashMap (not via resolve_type)
        if let Some(field_symbol) = self.symbol_env.fields.get(&field_key) {
            return Some(IdentifierResolution {
                identifier: identifier.to_string(),
                resolved_type: field_symbol.var_type.clone(),
                resolution_context: ResolutionContext::SymbolEnvironment,
                scope_depth: 0,
            });
        }
        
        None
    }
    
    /// Resolve type names to descriptors using integrated type resolution
    /// JavaC equivalent: Types.erasure() + descriptor generation
    pub fn resolve_type_descriptor(&mut self, type_name: &str) -> Option<String> {
        use crate::common::type_resolver::TypeResolver;
        use crate::ast::{TypeRef, Span, Location};
        
        // Create a simple TypeRef for the type name
        let type_ref = TypeRef {
            name: type_name.to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        };
        
        // Use integrated type resolver
        let mut manager = crate::common::manager::ClasspathManager::new("tests/java");
        let mut type_resolver = crate::common::type_resolver::TypeResolver::with_symbol_environment(&mut manager, self.symbol_env.clone());
        
        let resolution = type_resolver.resolve_type_ref(&type_ref);
        Some(resolution.get_jvm_descriptor().to_string())
    }
    
    /// Resolve method signatures using enhanced type resolution
    /// JavaC equivalent: Resolve.resolveMethod
    pub fn resolve_method_signature(&mut self, class_name: &str, method_name: &str, param_types: &[TypeRef]) -> Option<String> {
        use crate::common::type_resolver::TypeResolver;
        
        let method_key = format!("{}#{}", class_name, method_name);
        
        if let Some(method_symbol) = self.symbol_env.methods.get(&method_key) {
            // Build signature from MethodSymbol
            let mut descriptor = String::from("(");
            for param_type in &method_symbol.parameter_types {
                descriptor.push_str(param_type);
            }
            descriptor.push(')');
            descriptor.push_str(&method_symbol.return_type);
            return Some(descriptor);
        }
        
        // Use integrated type resolver for accurate parameter type resolution
        let mut manager = crate::common::manager::ClasspathManager::new("tests/java");
        let mut type_resolver = crate::common::type_resolver::TypeResolver::with_symbol_environment(&mut manager, self.symbol_env.clone());
        
        let mut descriptor = String::from("(");
        for param_type in param_types {
            let resolution = type_resolver.resolve_type_ref(param_type);
            descriptor.push_str(resolution.get_jvm_descriptor());
        }
        descriptor.push_str(")V"); // Default void return
        
        // Cache for future use - create a MethodSymbol
        let method_symbol = crate::common::env::MethodSymbol {
            name: method_name.to_string(),
            owner_class: class_name.to_string(),
            type_parameters: vec![],
            parameter_types: param_types.iter()
                .map(|param| type_resolver.resolve_type_ref(param).get_jvm_descriptor().to_string())
                .collect(),
            return_type: "V".to_string(), // Default void return
            is_static: false,
            is_generic: false,
        };
        self.symbol_env.methods.insert(method_key, method_symbol);
        
        Some(descriptor)
    }
    
    /// Clear caches (useful for incremental compilation)
    pub fn clear_caches(&mut self) {
        self.type_cache.clear();
        self.method_cache.clear();
    }
    
    /// Get current resolution context for debugging
    pub fn get_current_context(&self) -> &ResolutionContext {
        &self.current_context
    }
    
    /// Check if a type is resolvable
    /// JavaC equivalent: Resolve.isAccessible
    pub fn can_resolve_type(&mut self, type_name: &str) -> bool {
        self.resolve_type_descriptor(type_name).is_some()
    }
    
    /// Get underlying symbol environment (for advanced usage)
    pub fn get_symbol_environment(&self) -> &SymbolEnvironment {
        &self.symbol_env
    }
    
    /// Update symbol environment (for dynamic changes)
    pub fn update_symbol_environment(&mut self, new_env: SymbolEnvironment) {
        self.symbol_env = new_env;
        self.clear_caches(); // Clear caches when environment changes
    }
}

impl Default for UnifiedResolver {
    fn default() -> Self {
        Self::new(SymbolEnvironment::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::env::SymbolEnvironment;
    
    #[test]
    fn test_unified_resolver_creation() {
        let symbol_env = SymbolEnvironment::default();
        let resolver = UnifiedResolver::new(symbol_env);
        
        assert_eq!(resolver.type_cache.len(), 0);
        assert_eq!(resolver.method_cache.len(), 0);
        assert_eq!(*resolver.get_current_context(), ResolutionContext::NotFound);
    }
    
    #[test]
    fn test_primitive_type_resolution() {
        let mut resolver = UnifiedResolver::default();
        
        assert_eq!(resolver.resolve_type_descriptor("int"), Some("I".to_string()));
        assert_eq!(resolver.resolve_type_descriptor("boolean"), Some("Z".to_string()));
        assert_eq!(resolver.resolve_type_descriptor("void"), Some("V".to_string()));
    }
    
    #[test]
    fn test_cache_functionality() {
        let mut resolver = UnifiedResolver::default();
        
        // Add something to cache
        resolver.type_cache.insert("TestType".to_string(), "LTestType;".to_string());
        assert_eq!(resolver.type_cache.len(), 1);
        
        // Clear caches
        resolver.clear_caches();
        assert_eq!(resolver.type_cache.len(), 0);
    }
}