//! TransTypes phase - Generic type erasure and bridge method generation
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.TransTypes` class.
//! This phase removes generic type information (type erasure) and generates
//! bridge methods to maintain binary compatibility.

use crate::ast::*;
use crate::common::error::Result;
use std::collections::HashMap;

/// TransTypes phase processor - corresponds to JavaC's TransTypes class
pub struct TransTypes {
    /// Store generic signatures before erasure for later bytecode generation
    pub generic_signatures: HashMap<String, String>,
    /// Map from method names to their bridge methods
    pub bridge_methods: HashMap<String, Vec<BridgeMethod>>,
    /// Map from erased method signatures to original overridden methods
    pub overridden_methods: HashMap<String, String>,
    /// Map from generic types to their erased forms
    pub erased_types: HashMap<String, String>,
}

/// Bridge method information - corresponds to JavaC's bridge method generation
#[derive(Debug, Clone)]
pub struct BridgeMethod {
    pub name: String,
    pub erased_parameters: Vec<String>,
    pub erased_return_type: String,
    pub original_method: String,
    pub is_synthetic: bool,
}

impl TransTypes {
    pub fn new() -> Self {
        Self {
            generic_signatures: HashMap::new(),
            bridge_methods: HashMap::new(),
            overridden_methods: HashMap::new(),
            erased_types: HashMap::new(),
        }
    }
    
    /// Process AST through TransTypes phase - type erasure
    /// Corresponds to JavaC's TransTypes.translateTopLevelClass() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        self.process_with_types(ast, &std::collections::HashMap::new())
    }
    
    /// Process AST with type information from Attr phase
    pub fn process_with_types(&mut self, mut ast: Ast, type_info: &std::collections::HashMap<usize, crate::codegen::attr::ResolvedType>) -> Result<Ast> {
        eprintln!("🔍 TRANS_TYPES: Starting type erasure");
        eprintln!("📊 TRANS_TYPES: Processing {} type expressions", type_info.len());
        
        // First, perform type erasure on the ResolvedType information
        for (expr_id, resolved_type) in type_info {
            let erased_type = self.erase_type(resolved_type);
            self.erased_types.insert(expr_id.to_string(), erased_type);
        }
        
        // Store generic signature information for each type declaration before erasure
        // This information will be used later during bytecode generation
        for type_decl in &mut ast.type_decls {
            self.store_generic_signatures(type_decl)?;
        }
        
        // Generate bridge methods before erasure
        for type_decl in &ast.type_decls {
            self.generate_bridge_methods(type_decl)?;
        }
        
        // Then apply type erasure to all type declarations
        for type_decl in &mut ast.type_decls {
            self.erase_type_declaration(type_decl)?;
        }
        
        eprintln!("✅ TRANS_TYPES: Type erasure complete");
        eprintln!("📊 TRANS_TYPES: {} generic signatures, {} bridge methods", 
                 self.generic_signatures.len(),
                 self.bridge_methods.len());
        
        Ok(ast)
    }
    
    /// Store generic signature information before type erasure
    fn store_generic_signatures(&mut self, declaration: &TypeDecl) -> Result<()> {
        match declaration {
            TypeDecl::Class(class_decl) => {
                if !class_decl.type_params.is_empty() || 
                   class_decl.extends.as_ref().map_or(false, |e| !e.type_args.is_empty()) ||
                   class_decl.implements.iter().any(|i| !i.type_args.is_empty()) {
                    
                    let type_resolver = crate::codegen::signature::TypeNameResolver::with_default_mappings();
                    let signature = crate::codegen::signature::class_to_signature(
                        class_decl, 
                        None, // package_name - TODO: pass actual package
                        Some(&class_decl.name), 
                        &type_resolver
                    );
                    
                    self.generic_signatures.insert(format!("class:{}", class_decl.name), signature);
                    eprintln!("🔧 TRANS_TYPES: Stored class signature for '{}'", class_decl.name);
                }
                
                // Store method signatures
                for member in &class_decl.body {
                    if let ClassMember::Method(method) = member {
                        if self.method_needs_signature(method) {
                            let type_resolver = crate::codegen::signature::TypeNameResolver::with_default_mappings();
                            let signature = crate::codegen::signature::method_to_signature(
                                method, 
                                None, // package_name
                                Some(&class_decl.name), 
                                &type_resolver
                            );
                            
                            self.generic_signatures.insert(format!("method:{}:{}", class_decl.name, method.name), signature);
                            eprintln!("🔧 TRANS_TYPES: Stored method signature for '{}.{}'", class_decl.name, method.name);
                        }
                    }
                }
                
                // Store constructor signatures
                for member in &class_decl.body {
                    if let ClassMember::Constructor(constructor) = member {
                        if self.constructor_needs_signature(constructor) {
                            let type_resolver = crate::codegen::signature::TypeNameResolver::with_default_mappings();
                            let signature = self.constructor_to_signature(constructor, Some(&class_decl.name), &type_resolver);
                            
                            // Include descriptor to distinguish constructor overloads
                            let descriptor = self.constructor_to_descriptor(constructor);
                            let key = format!("method:{}:<init>:{}", class_decl.name, descriptor);
                            self.generic_signatures.insert(key, signature);
                            eprintln!("🔧 TRANS_TYPES: Stored constructor signature for '{}'", class_decl.name);
                        }
                    }
                }
            }
            TypeDecl::Interface(interface_decl) => {
                if !interface_decl.type_params.is_empty() ||
                   interface_decl.extends.iter().any(|i| !i.type_args.is_empty()) {
                    
                    let type_resolver = crate::codegen::signature::TypeNameResolver::with_default_mappings();
                    let signature = crate::codegen::signature::interface_to_signature(
                        interface_decl, 
                        None, // package_name
                        Some(&interface_decl.name), 
                        &type_resolver
                    );
                    
                    self.generic_signatures.insert(format!("interface:{}", interface_decl.name), signature);
                    eprintln!("🔧 TRANS_TYPES: Stored interface signature for '{}'", interface_decl.name);
                }
                
                // Store method signatures for interface methods
                for member in &interface_decl.body {
                    if let InterfaceMember::Method(method) = member {
                        if self.method_needs_signature(method) {
                            let type_resolver = crate::codegen::signature::TypeNameResolver::with_default_mappings();
                            let signature = crate::codegen::signature::method_to_signature(
                                method, 
                                None, // package_name
                                Some(&interface_decl.name), 
                                &type_resolver
                            );
                            
                            self.generic_signatures.insert(format!("method:{}:{}", interface_decl.name, method.name), signature);
                            eprintln!("🔧 TRANS_TYPES: Stored interface method signature for '{}.{}'", interface_decl.name, method.name);
                        }
                    }
                }
            }
            TypeDecl::Enum(_) => {
                // Enums typically don't have complex generic signatures
            }
            TypeDecl::Annotation(_) => {
                // Annotations typically don't have complex generic signatures
            }
        }
        Ok(())
    }
    
    /// Check if a method needs a signature attribute
    /// A method needs a signature if it:
    /// 1. Has its own type parameters
    /// 2. Uses type variables in its parameters or return type
    /// 3. Has type arguments in parameters or return type
    fn method_needs_signature(&self, method: &MethodDecl) -> bool {
        // Check if method has its own type parameters
        if !method.type_params.is_empty() {
            return true;
        }
        
        // Check if return type uses type variables or has type arguments
        if let Some(return_type) = &method.return_type {
            if self.type_ref_uses_generics(return_type) {
                return true;
            }
        }
        
        // Check if any parameter uses type variables or has type arguments
        for param in &method.parameters {
            if self.type_ref_uses_generics(&param.type_ref) {
                return true;
            }
        }
        
        // Check if any exception type uses generics
        for exception in &method.throws {
            if self.type_ref_uses_generics(exception) {
                return true;
            }
        }
        
        false
    }
    
    /// Check if a constructor needs a signature attribute
    fn constructor_needs_signature(&self, constructor: &ConstructorDecl) -> bool {
        // Check if any parameter uses type variables or has type arguments
        for param in &constructor.parameters {
            if self.type_ref_uses_generics(&param.type_ref) {
                return true;
            }
        }
        
        // Check if any exception type uses generics
        for exception in &constructor.throws {
            if self.type_ref_uses_generics(exception) {
                return true;
            }
        }
        
        false
    }
    
    /// Check if a TypeRef uses generics (type variables or type arguments)
    fn type_ref_uses_generics(&self, type_ref: &TypeRef) -> bool {
        // Check if it's a type variable (single uppercase letter)
        if type_ref.name.len() == 1 && type_ref.name.chars().next().unwrap().is_uppercase() {
            return true;
        }
        
        // Check if it has type arguments
        if !type_ref.type_args.is_empty() {
            return true;
        }
        
        false
    }
    
    /// Generate bridge methods for a type declaration - corresponds to JavaC's bridge method generation
    fn generate_bridge_methods(&mut self, declaration: &TypeDecl) -> Result<()> {
        match declaration {
            TypeDecl::Class(class_decl) => {
                self.generate_class_bridge_methods(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                self.generate_interface_bridge_methods(interface_decl)?;
            }
            _ => {} // Enums and annotations don't typically need bridge methods
        }
        Ok(())
    }
    
    /// Generate bridge methods for a class declaration
    fn generate_class_bridge_methods(&mut self, class_decl: &ClassDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Generating bridge methods for class '{}'", class_decl.name);
        
        let mut bridges = Vec::new();
        
        // Analyze each method to see if it needs bridge methods
        for member in &class_decl.body {
            if let ClassMember::Method(method) = member {
                if self.method_needs_bridge(method) {
                    let bridge = self.create_bridge_method(method, &class_decl.name);
                    bridges.push(bridge);
                    eprintln!("🌉 TRANS_TYPES: Created bridge method for '{}.{}'", class_decl.name, method.name);
                }
            }
        }
        
        if !bridges.is_empty() {
            self.bridge_methods.insert(class_decl.name.clone(), bridges);
        }
        
        Ok(())
    }
    
    /// Generate bridge methods for an interface declaration
    fn generate_interface_bridge_methods(&mut self, interface_decl: &InterfaceDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Generating bridge methods for interface '{}'", interface_decl.name);
        
        let mut bridges = Vec::new();
        
        // Analyze each method to see if it needs bridge methods
        for member in &interface_decl.body {
            if let InterfaceMember::Method(method) = member {
                if self.method_needs_bridge(method) {
                    let bridge = self.create_bridge_method(method, &interface_decl.name);
                    bridges.push(bridge);
                    eprintln!("🌉 TRANS_TYPES: Created bridge method for '{}.{}'", interface_decl.name, method.name);
                }
            }
        }
        
        if !bridges.is_empty() {
            self.bridge_methods.insert(interface_decl.name.clone(), bridges);
        }
        
        Ok(())
    }
    
    /// Check if a method needs a bridge method
    /// Bridge methods are needed when:
    /// 1. Method has generic parameters or return type that will be erased
    /// 2. Method overrides a generic method from superclass/interface
    /// 3. Method implements a generic interface method
    /// NOTE: Interface methods themselves do NOT need bridge methods - only implementations do
    fn method_needs_bridge(&self, method: &MethodDecl) -> bool {
        // Interface methods (abstract methods) never need bridge methods
        // Bridge methods are only needed in concrete implementations
        if method.body.is_none() {
            return false;
        }
        
        // Check if method has generic return type
        if let Some(return_type) = &method.return_type {
            if self.type_ref_uses_generics(return_type) {
                return true;
            }
        }
        
        // Check if method has generic parameters
        for param in &method.parameters {
            if self.type_ref_uses_generics(&param.type_ref) {
                return true;
            }
        }
        
        // TODO: In a full implementation, we would also check:
        // - If method overrides a generic method from superclass
        // - If method implements a generic interface method
        // - If method's signature would change after erasure
        
        false
    }
    
    /// Create a bridge method for the given method
    fn create_bridge_method(&self, method: &MethodDecl, class_name: &str) -> BridgeMethod {
        let mut erased_parameters = Vec::new();
        for param in &method.parameters {
            erased_parameters.push(self.erase_type_ref_to_string(&param.type_ref));
        }
        
        let erased_return_type = method.return_type.as_ref()
            .map(|rt| self.erase_type_ref_to_string(rt))
            .unwrap_or_else(|| "V".to_string());
        
        BridgeMethod {
            name: method.name.clone(),
            erased_parameters,
            erased_return_type,
            original_method: format!("{}.{}", class_name, method.name),
            is_synthetic: true,
        }
    }
    
    /// Erase a type reference to its string representation (for bridge method signatures)
    fn erase_type_ref_to_string(&self, type_ref: &TypeRef) -> String {
        if self.is_type_parameter(&type_ref.name) {
            "Ljava/lang/Object;".to_string()
        } else {
            // Convert type name to JVM signature format
            match type_ref.name.as_str() {
                "boolean" => "Z".to_string(),
                "byte" => "B".to_string(),
                "char" => "C".to_string(),
                "short" => "S".to_string(),
                "int" => "I".to_string(),
                "long" => "J".to_string(),
                "float" => "F".to_string(),
                "double" => "D".to_string(),
                "void" => "V".to_string(),
                _ => format!("L{};", type_ref.name.replace('.', "/")),
            }
        }
    }
    
    /// Generate signature for constructor (similar to method but no return type and no method type params)
    fn constructor_to_signature(&self, constructor: &ConstructorDecl, current_class_name: Option<&str>, type_resolver: &crate::codegen::signature::TypeNameResolver) -> String {
        let mut signature = String::new();
        
        // Add parameter types
        signature.push('(');
        for param in &constructor.parameters {
            signature.push_str(&crate::codegen::signature::type_ref_to_signature(&param.type_ref, None, current_class_name, type_resolver));
        }
        signature.push(')');
        
        // Constructor always returns void
        signature.push('V');
        
        signature
    }
    
    /// Erase types in a type declaration (class, interface, etc.)
    fn erase_type_declaration(&mut self, declaration: &mut TypeDecl) -> Result<()> {
        match declaration {
            TypeDecl::Class(class_decl) => self.erase_class_declaration(class_decl),
            TypeDecl::Interface(interface_decl) => self.erase_interface_declaration(interface_decl),
            TypeDecl::Enum(enum_decl) => self.erase_enum_declaration(enum_decl),
            TypeDecl::Annotation(annotation_decl) => self.erase_annotation_declaration(annotation_decl),
        }
    }
    
    /// Erase types in a class declaration
    fn erase_class_declaration(&mut self, class: &mut ClassDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Erasing types in class '{}'", class.name);
        
        // Erase superclass type arguments
        if let Some(ref mut extends) = class.extends {
            self.erase_type_ref(extends);
        }
        
        // Erase interface type arguments
        for interface in &mut class.implements {
            self.erase_type_ref(interface);
        }
        
        // Clear generic type parameters (they are erased)
        class.type_params.clear();
        
        // Erase member types
        for member in &mut class.body {
            self.erase_class_member(member)?;
        }
        
        Ok(())
    }
    
    /// Erase types in interface declaration
    fn erase_interface_declaration(&mut self, interface: &mut InterfaceDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Erasing types in interface '{}'", interface.name);
        
        // Erase super-interface type arguments
        for super_interface in &mut interface.extends {
            self.erase_type_ref(super_interface);
        }
        
        // Clear generic type parameters
        interface.type_params.clear();
        
        // Erase member types
        for member in &mut interface.body {
            self.erase_interface_member(member)?;
        }
        
        Ok(())
    }
    
    /// Erase types in enum declaration
    fn erase_enum_declaration(&mut self, enum_decl: &mut EnumDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Erasing types in enum '{}'", enum_decl.name);
        
        // Erase interface type arguments
        for interface in &mut enum_decl.implements {
            self.erase_type_ref(interface);
        }
        
        // Erase member types
        for member in &mut enum_decl.body {
            self.erase_class_member(member)?;
        }
        
        Ok(())
    }
    
    /// Erase types in annotation declaration
    fn erase_annotation_declaration(&mut self, annotation: &mut AnnotationDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Erasing types in annotation '{}'", annotation.name);
        
        // Erase member types
        for member in &mut annotation.body {
            self.erase_annotation_member(member)?;
        }
        
        Ok(())
    }
    
    /// Erase types in class member
    fn erase_class_member(&mut self, member: &mut ClassMember) -> Result<()> {
        match member {
            ClassMember::Field(field) => {
                self.erase_field_declaration(field)?;
            }
            ClassMember::Method(method) => {
                self.erase_method_declaration(method)?;
            }
            ClassMember::Constructor(constructor) => {
                self.erase_constructor_declaration(constructor)?;
            }
            ClassMember::TypeDecl(nested_type) => {
                self.erase_type_declaration(nested_type)?;
            }
            ClassMember::Initializer(_) => {
                // No types to erase in initializer blocks
            }
        }
        Ok(())
    }
    
    /// Erase types in interface member
    fn erase_interface_member(&mut self, member: &mut InterfaceMember) -> Result<()> {
        match member {
            InterfaceMember::Field(field) => {
                self.erase_field_declaration(field)?;
            }
            InterfaceMember::Method(method) => {
                self.erase_method_declaration(method)?;
            }
            InterfaceMember::TypeDecl(nested_type) => {
                self.erase_type_declaration(nested_type)?;
            }
        }
        Ok(())
    }
    
    /// Erase types in annotation member
    fn erase_annotation_member(&mut self, member: &mut AnnotationMember) -> Result<()> {
        // Erase the type reference of the annotation member
        self.erase_type_ref(&mut member.type_ref);
        Ok(())
    }
    
    /// Erase types in field declaration
    fn erase_field_declaration(&mut self, field: &mut FieldDecl) -> Result<()> {
        self.erase_type_ref(&mut field.type_ref);
        Ok(())
    }
    
    /// Erase types in parameter declaration
    fn erase_parameter_declaration(&mut self, parameter: &mut Parameter) -> Result<()> {
        self.erase_type_ref(&mut parameter.type_ref);
        Ok(())
    }
    
    /// Erase types in method declaration
    fn erase_method_declaration(&mut self, method: &mut MethodDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Erasing types in method '{}'", method.name);
        
        // Clear generic type parameters (they are erased)
        method.type_params.clear();
        
        // Erase return type
        if let Some(ref mut return_type) = method.return_type {
            self.erase_type_ref(return_type);
        }
        
        // Erase parameter types
        for parameter in &mut method.parameters {
            self.erase_parameter_declaration(parameter)?;
        }
        
        // Erase exception types
        for exception in &mut method.throws {
            self.erase_type_ref(exception);
        }
        
        Ok(())
    }
    
    /// Erase types in constructor declaration
    fn erase_constructor_declaration(&mut self, constructor: &mut ConstructorDecl) -> Result<()> {
        eprintln!("🔧 TRANS_TYPES: Erasing types in constructor");
        
        // Erase parameter types
        for parameter in &mut constructor.parameters {
            self.erase_parameter_declaration(parameter)?;
        }
        
        // Erase exception types
        for exception in &mut constructor.throws {
            self.erase_type_ref(exception);
        }
        
        Ok(())
    }
    
    /// Core type erasure logic - corresponds to JavaC's Types.erasure()
    fn erase_type_ref(&mut self, type_ref: &mut TypeRef) {
        // Clear all type arguments (this is the main erasure operation)
        if !type_ref.type_args.is_empty() {
            eprintln!("🔧 TRANS_TYPES: Erasing type arguments from '{}'", type_ref.name);
            type_ref.type_args.clear();
        }
        
        // Handle special generic type parameter names (T, U, E, K, V, etc.)
        // These should be erased to their bounds (default: Object)
        if self.is_type_parameter(&type_ref.name) {
            eprintln!("🔧 TRANS_TYPES: Erasing type parameter '{}' to Object", type_ref.name);
            type_ref.name = "java/lang/Object".to_string();
            type_ref.type_args.clear();
        }
    }
    
    /// Check if a type name represents a type parameter
    /// This is a simplified heuristic - in full implementation we would track type parameters
    fn is_type_parameter(&self, name: &str) -> bool {
        // Common generic type parameter names
        matches!(name, "T" | "U" | "E" | "K" | "V" | "R" | "S" | "X" | "Y" | "Z") ||
        // Single uppercase letters (common pattern for type parameters)
        (name.len() == 1 && name.chars().next().unwrap().is_ascii_uppercase())
    }
    
    /// Erase generic type to its raw form following Java type erasure rules
    fn erase_type(&self, resolved_type: &crate::codegen::attr::ResolvedType) -> String {
        use crate::codegen::attr::ResolvedType;
        
        match resolved_type {
            ResolvedType::Primitive(prim) => format!("{:?}", prim).to_lowercase(),
            ResolvedType::Reference(name) => name.clone(),
            ResolvedType::Array(element_type) => {
                format!("[{}", self.erase_type(element_type))
            }
            ResolvedType::Generic(name, _) => {
                // Erase to raw type
                name.clone()
            }
            ResolvedType::Class(class_type) => {
                // Erase to raw class name
                class_type.name.clone()
            }
            ResolvedType::TypeVariable(type_var) => {
                // Erase to first upper bound or Object
                if let Some(first_bound) = type_var.upper_bounds.first() {
                    self.erase_type(first_bound)
                } else {
                    "java.lang.Object".to_string()
                }
            }
            ResolvedType::Wildcard(wildcard) => {
                // Erase wildcard to its bound or Object
                if let Some(bound) = &wildcard.bound {
                    self.erase_type(bound)
                } else {
                    "java.lang.Object".to_string()
                }
            }
            ResolvedType::Captured(captured) => {
                // Erase captured type to its wildcard bound
                self.erase_type(&ResolvedType::Wildcard(captured.wildcard_bound.clone()))
            }
            ResolvedType::Intersection(types) => {
                // Erase to first type in intersection
                if let Some(first_type) = types.first() {
                    self.erase_type(first_type)
                } else {
                    "java.lang.Object".to_string()
                }
            }
            ResolvedType::Union(_types) => {
                // Erase to common supertype (simplified to Object)
                "java.lang.Object".to_string()
            }
            ResolvedType::Method(_params, _return_type) => {
                // Erase to functional interface (simplified)
                "java.util.function.Function".to_string()
            }
            ResolvedType::Null => "java.lang.Object".to_string(),
            ResolvedType::Error => "java.lang.Object".to_string(),
            ResolvedType::NoType => "void".to_string(),
        }
    }
    
    /// Get erased types for use by subsequent phases
    pub fn get_erased_types(&self) -> &HashMap<String, String> {
        &self.erased_types
    }
    
    /// Generate constructor descriptor for overload disambiguation
    fn constructor_to_descriptor(&self, constructor: &crate::ast::ConstructorDecl) -> String {
        let mut descriptor = String::from("(");
        
        // Add parameter descriptors
        for param in &constructor.parameters {
            descriptor.push_str(&self.type_ref_to_descriptor(&param.type_ref));
        }
        
        descriptor.push(')');
        descriptor.push('V'); // Constructors always return void
        
        descriptor
    }
    
    /// Convert TypeRef to JVM descriptor string  
    fn type_ref_to_descriptor(&self, type_ref: &crate::ast::TypeRef) -> String {
        let mut descriptor = String::new();
        
        // Add array dimensions
        for _ in 0..type_ref.array_dims {
            descriptor.push('[');
        }
        
        // Add base type descriptor
        match type_ref.name.as_str() {
            "boolean" => descriptor.push('Z'),
            "byte" => descriptor.push('B'),
            "char" => descriptor.push('C'),
            "short" => descriptor.push('S'),
            "int" => descriptor.push('I'),
            "long" => descriptor.push('J'),
            "float" => descriptor.push('F'),
            "double" => descriptor.push('D'),
            "void" => descriptor.push('V'),
            _ => {
                // Object type - ensure full package path
                descriptor.push('L');
                let class_name = if type_ref.name.contains('.') {
                    type_ref.name.replace('.', "/")
                } else {
                    // Try to expand short names to full package names
                    match type_ref.name.as_str() {
                        "Collection" => "java/util/Collection".to_string(),
                        "List" => "java/util/List".to_string(),
                        "Set" => "java/util/Set".to_string(),
                        "Map" => "java/util/Map".to_string(),
                        "String" => "java/lang/String".to_string(),
                        "Object" => "java/lang/Object".to_string(),
                        _ => type_ref.name.replace('.', "/")
                    }
                };
                descriptor.push_str(&class_name);
                descriptor.push(';');
            }
        }
        
        descriptor
    }
}

impl Default for TransTypes {
    fn default() -> Self {
        Self::new()
    }
}