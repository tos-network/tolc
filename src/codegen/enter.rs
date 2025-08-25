//! Enhanced Enter phase - Integrated with dynamic type resolution
//!
//! This is an enhanced version of the Enter phase that integrates with
//! the new ClasspathManager and TypeResolver for dynamic type resolution.

use crate::ast::{Ast, TypeDecl, ClassDecl, InterfaceDecl, EnumDecl, AnnotationDecl, TypeRef, ClassMember, InterfaceMember, FieldDecl, MethodDecl, Parameter};
use crate::common::error::Result;
use crate::common::type_resolver::TypeResolver;
use crate::common::import::ImportResolver;
//use crate::common::manager::ClasspathManager;
use crate::common::env::{SymbolKind, VariableSymbol, TypeParameterSymbol, MethodSymbol, ClassSymbol, SymbolEnvironment};
use std::collections::HashMap;

// Symbol table structures moved to common::env

// SymbolEnvironment implementation moved to common::env

pub struct EnhancedEnter {
    type_resolver: crate::common::type_resolver::StandaloneTypeResolver,
    import_resolver: Option<crate::common::import::StandaloneImportResolver>,
    symbol_env: SymbolEnvironment,
    current_class: Option<String>,
    resolved_types: HashMap<String, String>, // original_name -> fully_qualified_name
    classpath: String,
}

impl EnhancedEnter {
    pub fn new(classpath: &str) -> Self {
        eprintln!("🔧 ENHANCED_ENTER: Initializing with classpath: {}", classpath);
        
        EnhancedEnter {
            type_resolver: crate::common::type_resolver::StandaloneTypeResolver::new(classpath),
            import_resolver: None,
            symbol_env: SymbolEnvironment::default(),
            current_class: None,
            resolved_types: HashMap::new(),
            classpath: classpath.to_string(),
        }
    }
    
    /// Process AST with enhanced type resolution
    pub fn process(&mut self, mut ast: Ast) -> Result<Ast> {
        eprintln!("🔧 ENHANCED_ENTER: Starting Enter phase with dynamic type resolution");
        
        // Phase 1: Build import resolver from AST
        self.import_resolver = Some(
            crate::common::import::StandaloneImportResolver::from_ast(&ast, &self.classpath)
        );
        
        // Phase 2: Resolve all type references in the AST  
        self.resolve_all_type_references(&mut ast)?;
        
        // Phase 3: Build symbol tables with resolved types
        self.build_symbol_tables(&ast)?;
        
        eprintln!("✅ ENHANCED_ENTER: Completed Enter phase");
        eprintln!("   📊 Resolved {} type names", self.resolved_types.len());
        eprintln!("   📊 Symbol environment: {} fields, {} methods", 
                 self.symbol_env.fields.len(), 
                 self.symbol_env.methods.len());
        
        Ok(ast)
    }
    
    /// Resolve all type references in the AST to fully qualified names
    fn resolve_all_type_references(&mut self, ast: &mut Ast) -> Result<()> {
        eprintln!("🔍 ENHANCED_ENTER: Resolving type references");
        
        for type_decl in &mut ast.type_decls {
            match type_decl {
                TypeDecl::Class(class_decl) => {
                    self.current_class = Some(class_decl.name.clone());
                    self.resolve_class_type_references(class_decl)?;
                }
                TypeDecl::Interface(interface_decl) => {
                    self.current_class = Some(interface_decl.name.clone());
                    self.resolve_interface_type_references(interface_decl)?;
                }
                TypeDecl::Enum(enum_decl) => {
                    self.current_class = Some(enum_decl.name.clone());
                    self.resolve_enum_type_references(enum_decl)?;
                }
                TypeDecl::Annotation(annotation_decl) => {
                    self.current_class = Some(annotation_decl.name.clone());
                    self.resolve_annotation_type_references(annotation_decl)?;
                }
            }
        }
        
        Ok(())
    }
    
    fn resolve_class_type_references(&mut self, class_decl: &mut ClassDecl) -> Result<()> {
        eprintln!("🔍 ENHANCED_ENTER: Resolving types in class '{}'", class_decl.name);
        
        // Resolve superclass (extends clause)
        if let Some(ref mut extends) = class_decl.extends {
            self.resolve_type_ref(extends)?;
        }
        
        // Resolve implemented interfaces
        for interface in &mut class_decl.implements {
            self.resolve_type_ref(interface)?;
        }
        
        // Resolve types in class members
        for member in &mut class_decl.body {
            match member {
                ClassMember::Field(ref mut field_decl) => {
                    self.resolve_type_ref(&mut field_decl.type_ref)?;
                }
                ClassMember::Method(ref mut method_decl) => {
                    self.resolve_method_type_references(method_decl)?;
                }
                ClassMember::Constructor(ref mut constructor_decl) => {
                    // Resolve constructor parameter types
                    for param in &mut constructor_decl.parameters {
                        self.resolve_type_ref(&mut param.type_ref)?;
                    }
                }
                _ => {} // Handle other members if needed
            }
        }
        
        Ok(())
    }
    
    fn resolve_interface_type_references(&mut self, interface_decl: &mut InterfaceDecl) -> Result<()> {
        eprintln!("🔍 ENHANCED_ENTER: Resolving types in interface '{}'", interface_decl.name);
        
        // Resolve extended interfaces
        for extended_interface in &mut interface_decl.extends {
            self.resolve_type_ref(extended_interface)?;
        }
        
        // Resolve types in interface members
        for member in &mut interface_decl.body {
            match member {
                InterfaceMember::Method(ref mut method_decl) => {
                    self.resolve_method_type_references(method_decl)?;
                }
                _ => {} // Handle other members if needed
            }
        }
        
        Ok(())
    }
    
    fn resolve_enum_type_references(&mut self, enum_decl: &mut EnumDecl) -> Result<()> {
        eprintln!("🔍 ENHANCED_ENTER: Resolving types in enum '{}'", enum_decl.name);
        
        // Resolve implemented interfaces
        for interface in &mut enum_decl.implements {
            self.resolve_type_ref(interface)?;
        }
        
        // Resolve types in enum members
        for member in &mut enum_decl.body {
            match member {
                ClassMember::Field(ref mut field_decl) => {
                    self.resolve_type_ref(&mut field_decl.type_ref)?;
                }
                ClassMember::Method(ref mut method_decl) => {
                    self.resolve_method_type_references(method_decl)?;
                }
                ClassMember::Constructor(ref mut constructor_decl) => {
                    // Resolve constructor parameter types
                    for param in &mut constructor_decl.parameters {
                        self.resolve_type_ref(&mut param.type_ref)?;
                    }
                }
                _ => {} // Handle other members if needed
            }
        }
        
        Ok(())
    }
    
    
    fn resolve_method_type_references(&mut self, method_decl: &mut MethodDecl) -> Result<()> {
        // Resolve return type
        if let Some(ref mut return_type) = method_decl.return_type {
            self.resolve_type_ref(return_type)?;
        }
        
        // Resolve parameter types
        for param in &mut method_decl.parameters {
            self.resolve_type_ref(&mut param.type_ref)?;
        }
        
        // TODO: Resolve exception types in throws clause
        
        Ok(())
    }
    
    fn resolve_annotation_type_references(&mut self, annotation_decl: &mut AnnotationDecl) -> Result<()> {
        eprintln!("🔍 ENHANCED_ENTER: Resolving types in annotation '{}'", annotation_decl.name);
        
        // For now, annotations don't have complex type resolution needs
        // TODO: Handle annotation elements if they have type references
        
        Ok(())
    }
    
    
    /// Resolve a single TypeRef to its fully qualified name
    fn resolve_type_ref(&mut self, type_ref: &mut TypeRef) -> Result<()> {
        let original_name = type_ref.name.clone();
        
        // Skip if already resolved (contains dots or is primitive)
        if original_name.contains('.') || is_primitive_type(&original_name) {
            return Ok(());
        }
        
        // Check if we've already resolved this type
        if let Some(resolved) = self.resolved_types.get(&original_name) {
            type_ref.name = resolved.clone();
            return Ok(());
        }
        
        // Resolve using TypeResolver
        if let Some(ref mut import_resolver) = self.import_resolver {
            if let Some(fully_qualified) = self.type_resolver.resolve_type_name_standalone(&original_name, import_resolver) {
                eprintln!("✅ ENHANCED_ENTER: '{}' -> '{}'", original_name, fully_qualified);
                
                // Cache the resolution
                self.resolved_types.insert(original_name.clone(), fully_qualified.clone());
                
                // Update the TypeRef
                type_ref.name = fully_qualified;
            } else {
                eprintln!("⚠️  ENHANCED_ENTER: Could not resolve type '{}'", original_name);
                // Keep original name - might be a forward reference or external type
            }
        }
        
        Ok(())
    }
    
    /// Build symbol tables using the resolved type information
    fn build_symbol_tables(&mut self, ast: &Ast) -> Result<()> {
        eprintln!("🔧 ENHANCED_ENTER: Building symbol tables");
        
        for type_decl in &ast.type_decls {
            match type_decl {
                TypeDecl::Class(class_decl) => {
                    self.build_class_symbols(class_decl, ast)?;
                }
                TypeDecl::Interface(interface_decl) => {
                    self.build_interface_symbols(interface_decl, ast)?;
                }
                TypeDecl::Enum(enum_decl) => {
                    self.build_enum_symbols(enum_decl, ast)?;
                }
                TypeDecl::Annotation(annotation_decl) => {
                    self.build_annotation_symbols(annotation_decl, ast)?;
                }
            }
        }
        
        Ok(())
    }
    
    fn build_class_symbols(&mut self, class_decl: &ClassDecl, ast: &Ast) -> Result<()> {
        let class_owner = format!("class:{}", class_decl.name);
        
        // Get fully qualified name and package info
        let (fully_qualified_name, package_name) = self.get_fully_qualified_name(&class_decl.name, ast);
        
        // Process type parameters
        let type_parameters = self.process_type_parameters(&class_decl.type_params, &fully_qualified_name);
        
        // Create the ClassSymbol and add it to the classes table
        let mut class_methods = HashMap::new();
        let class_symbol = crate::common::env::ClassSymbol {
            name: class_decl.name.clone(),
            fully_qualified_name,
            package_name,
            is_interface: false,
            is_enum: false,
            is_annotation: false,
            super_class: class_decl.extends.as_ref().map(|e| e.name.clone()),
            interfaces: class_decl.implements.iter().map(|i| i.name.clone()).collect(),
            modifiers: class_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters,
            is_generic: !class_decl.type_params.is_empty(),
            methods: class_methods.clone(),
            enum_constants: vec![],
        };
        
        self.symbol_env.classes.insert(class_decl.name.clone(), class_symbol);
        eprintln!("📝 ENHANCED_ENTER: Added class symbol '{}'", class_decl.name);
        
        // Add symbols for class members
        for member in &class_decl.body {
            match member {
                ClassMember::Field(field_decl) => {
                    let field_key = format!("{}::{}", class_owner, field_decl.name);
                    let is_static = field_decl.modifiers.iter().any(|m| matches!(m, crate::ast::Modifier::Static));
                    
                    let field_symbol = VariableSymbol {
                        name: field_decl.name.clone(),
                        kind: SymbolKind::Field,
                        owner: class_owner.clone(),
                        var_type: self.build_full_type_name(&field_decl.type_ref), // Include array dimensions
                        is_static,
                        is_parameter: false,
                        local_slot: None,
                        modifiers: field_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
                    };
                    
                    self.symbol_env.fields.insert(field_key, field_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added field symbol '{}'", field_decl.name);
                }
                ClassMember::Method(method_decl) => {
                    let method_key = format!("{}:{}", class_decl.name, method_decl.name);
                    let return_type = method_decl.return_type.as_ref()
                        .map(|rt| rt.name.clone())
                        .unwrap_or_else(|| "void".to_string());
                    
                    let is_static = method_decl.modifiers.iter().any(|m| matches!(m, crate::ast::Modifier::Static));
                    
                    let method_symbol = MethodSymbol {
                        name: method_decl.name.clone(),
                        return_type, // Now contains resolved type
                        parameter_types: method_decl.parameters.iter()
                            .map(|p| p.type_ref.name.clone()) // Now contains resolved types
                            .collect(),
                        is_static,
                        owner_class: class_decl.name.clone(),
                        is_generic: !method_decl.type_params.is_empty(),
                        type_parameters: self.process_type_parameters(&method_decl.type_params, &method_key),
                    };
                    
                    self.symbol_env.methods.insert(method_key, method_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added method symbol '{}'", method_decl.name);
                    
                    // Add method parameters as variable symbols with proper slot assignment
                    let mut param_slot = if is_static { 0 } else { 1 }; // Start after 'this' for instance methods
                    for param in &method_decl.parameters {
                        let param_key = format!("method:{}#{}::{}", class_decl.name, method_decl.name, param.name);
                        let param_symbol = VariableSymbol {
                            name: param.name.clone(),
                            kind: SymbolKind::Variable,
                            owner: format!("{}:{}", class_decl.name, method_decl.name),
                            var_type: self.build_full_type_name(&param.type_ref), // Include array dimensions
                            is_static: false, // Parameters are never static
                            is_parameter: true,
                            local_slot: Some(param_slot), // Assign proper parameter slot
                            modifiers: param.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
                        };
                        
                        self.symbol_env.variables.insert(param_key, param_symbol); // Store in variables table
                        eprintln!("📝 ENHANCED_ENTER: Added parameter '{}' with slot {} for method '{}'", param.name, param_slot, method_decl.name);
                        param_slot += self.get_type_width(&param.type_ref); // Increment by type width (JavaC alignment)
                    }
                    
                    // Process method body for local variable declarations
                    if let Some(ref body) = method_decl.body {
                        self.process_method_body_statements(&body.statements, &class_decl.name, &method_decl.name)?;
                    }
                }
                ClassMember::Constructor(constructor_decl) => {
                    let constructor_key = format!("{}:{}", class_decl.name, "<init>");
                    
                    let constructor_symbol = MethodSymbol {
                        name: "<init>".to_string(), // JVM constructor name
                        return_type: "void".to_string(), // Constructors always return void
                        parameter_types: constructor_decl.parameters.iter()
                            .map(|p| p.type_ref.name.clone()) // Now contains resolved types
                            .collect(),
                        is_static: false, // Constructors are instance methods
                        owner_class: class_decl.name.clone(),
                        is_generic: false, // Constructors don't have type parameters
                        type_parameters: vec![], // No type parameters
                    };
                    
                    self.symbol_env.methods.insert(constructor_key, constructor_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added constructor symbol for class '{}'", class_decl.name);
                    
                    // Add constructor parameters as variable symbols with proper slot assignment
                    let mut param_slot = 1; // Constructors are instance methods, slot 0 = 'this'
                    for param in &constructor_decl.parameters {
                        let param_key = format!("method:{}#<init>::{}", class_decl.name, param.name);
                        let param_symbol = VariableSymbol {
                            name: param.name.clone(),
                            kind: SymbolKind::Variable,
                            owner: format!("{}:<init>", class_decl.name),
                            var_type: self.build_full_type_name(&param.type_ref), // Include array dimensions
                            is_static: false, // Parameters are never static
                            is_parameter: true,
                            local_slot: Some(param_slot), // Assign proper parameter slot
                            modifiers: param.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
                        };
                        
                        self.symbol_env.variables.insert(param_key, param_symbol);
                        eprintln!("📝 ENHANCED_ENTER: Added constructor parameter '{}' with slot {} for class '{}'", param.name, param_slot, class_decl.name);
                        param_slot += self.get_type_width(&param.type_ref); // Increment by type width (JavaC alignment)
                    }
                    
                    // Process constructor body for local variable declarations
                    self.process_method_body_statements(&constructor_decl.body.statements, &class_decl.name, "<init>")?;
                }
                _ => {} // Handle other members if needed
            }
        }
        
        Ok(())
    }
    
    fn build_interface_symbols(&mut self, interface_decl: &InterfaceDecl, ast: &Ast) -> Result<()> {
        // Similar to class symbols but for interfaces
        let class_owner = format!("class:{}", interface_decl.name);
        
        // Get fully qualified name and package info
        let (fully_qualified_name, package_name) = self.get_fully_qualified_name(&interface_decl.name, ast);
        
        // Process type parameters
        let type_parameters = self.process_type_parameters(&interface_decl.type_params, &fully_qualified_name);
        
        // Create the InterfaceSymbol and add it to the classes table
        let class_methods = HashMap::new();
        let class_symbol = crate::common::env::ClassSymbol {
            name: interface_decl.name.clone(),
            fully_qualified_name,
            package_name,
            is_interface: true,
            is_enum: false,
            is_annotation: false,
            super_class: None, // Interfaces don't extend classes
            interfaces: interface_decl.extends.iter().map(|i| i.name.clone()).collect(),
            modifiers: interface_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters,
            is_generic: !interface_decl.type_params.is_empty(),
            methods: class_methods,
            enum_constants: vec![],
        };
        
        self.symbol_env.classes.insert(interface_decl.name.clone(), class_symbol);
        eprintln!("📝 ENHANCED_ENTER: Added interface symbol '{}'", interface_decl.name);
        
        for member in &interface_decl.body {
            match member {
                InterfaceMember::Method(method_decl) => {
                    let method_key = format!("{}:{}", interface_decl.name, method_decl.name);
                    let return_type = method_decl.return_type.as_ref()
                        .map(|rt| rt.name.clone())
                        .unwrap_or_else(|| "void".to_string());
                    
                    let method_symbol = MethodSymbol {
                        name: method_decl.name.clone(),
                        return_type,
                        parameter_types: method_decl.parameters.iter()
                            .map(|p| p.type_ref.name.clone())
                            .collect(),
                        is_static: false, // Interface methods are not static by default
                        owner_class: interface_decl.name.clone(),
                        is_generic: !method_decl.type_params.is_empty(),
                        type_parameters: self.process_type_parameters(&method_decl.type_params, &method_key),
                    };
                    
                    self.symbol_env.methods.insert(method_key, method_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added interface method symbol '{}'", method_decl.name);
                    
                    // Add method parameters as variable symbols
                    for param in &method_decl.parameters {
                        let param_key = format!("method:{}#{}::{}", interface_decl.name, method_decl.name, param.name);
                        let param_symbol = VariableSymbol {
                            name: param.name.clone(),
                            kind: SymbolKind::Variable,
                            owner: format!("{}:{}", interface_decl.name, method_decl.name),
                            var_type: param.type_ref.name.clone(), // Resolved type
                            is_static: false, // Parameters are never static
                            is_parameter: true,
                            local_slot: None, // Will be set during code generation
                            modifiers: param.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
                        };
                        
                        self.symbol_env.variables.insert(param_key, param_symbol); // Store in variables table
                        eprintln!("📝 ENHANCED_ENTER: Added parameter symbol '{}' for interface method '{}'", param.name, method_decl.name);
                    }
                }
                _ => {} // Handle other members if needed
            }
        }
        
        Ok(())
    }
    
    fn build_enum_symbols(&mut self, enum_decl: &EnumDecl, ast: &Ast) -> Result<()> {
        // Build symbols for enum members (similar to class)
        let class_owner = format!("class:{}", enum_decl.name);
        
        // Get fully qualified name and package info
        let (fully_qualified_name, package_name) = self.get_fully_qualified_name(&enum_decl.name, ast);
        
        // Create the EnumSymbol and add it to the classes table
        let class_methods = HashMap::new();
        let class_symbol = crate::common::env::ClassSymbol {
            name: enum_decl.name.clone(),
            fully_qualified_name,
            package_name,
            is_interface: false,
            is_enum: true,
            is_annotation: false,
            super_class: Some("java.lang.Enum".to_string()), // Enums extend java.lang.Enum
            interfaces: enum_decl.implements.iter().map(|i| i.name.clone()).collect(),
            modifiers: enum_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: vec![], // Enums don't support type parameters in Java
            is_generic: false, // Enums don't support type parameters in Java
            methods: class_methods,
            enum_constants: enum_decl.constants.iter().map(|c| c.name.clone()).collect(),
        };
        
        self.symbol_env.classes.insert(enum_decl.name.clone(), class_symbol);
        eprintln!("📝 ENHANCED_ENTER: Added enum symbol '{}'", enum_decl.name);
        
        // Add symbols for enum members
        for member in &enum_decl.body {
            match member {
                ClassMember::Field(field_decl) => {
                    let field_key = format!("{}::{}", class_owner, field_decl.name);
                    let is_static = field_decl.modifiers.iter().any(|m| matches!(m, crate::ast::Modifier::Static));
                    
                    let field_symbol = VariableSymbol {
                        name: field_decl.name.clone(),
                        kind: SymbolKind::Field,
                        owner: class_owner.clone(),
                        var_type: self.build_full_type_name(&field_decl.type_ref), // Include array dimensions
                        is_static,
                        is_parameter: false,
                        local_slot: None,
                        modifiers: field_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
                    };
                    
                    self.symbol_env.fields.insert(field_key, field_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added enum field symbol '{}'", field_decl.name);
                }
                ClassMember::Method(method_decl) => {
                    let method_key = format!("{}:{}", enum_decl.name, method_decl.name);
                    let return_type = method_decl.return_type.as_ref()
                        .map(|rt| rt.name.clone())
                        .unwrap_or_else(|| "void".to_string());
                    
                    let is_static = method_decl.modifiers.iter().any(|m| matches!(m, crate::ast::Modifier::Static));
                    
                    let method_symbol = MethodSymbol {
                        name: method_decl.name.clone(),
                        return_type, // Now contains resolved type
                        parameter_types: method_decl.parameters.iter()
                            .map(|p| p.type_ref.name.clone()) // Now contains resolved types
                            .collect(),
                        is_static,
                        owner_class: enum_decl.name.clone(),
                        is_generic: !method_decl.type_params.is_empty(),
                        type_parameters: self.process_type_parameters(&method_decl.type_params, &method_key),
                    };
                    
                    self.symbol_env.methods.insert(method_key, method_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added enum method symbol '{}'", method_decl.name);
                    
                    // Add method parameters as variable symbols
                    for param in &method_decl.parameters {
                        let param_key = format!("method:{}#{}::{}", enum_decl.name, method_decl.name, param.name);
                        let param_symbol = VariableSymbol {
                            name: param.name.clone(),
                            kind: SymbolKind::Variable,
                            owner: format!("{}:{}", enum_decl.name, method_decl.name),
                            var_type: param.type_ref.name.clone(), // Resolved type
                            is_static: false, // Parameters are never static
                            is_parameter: true,
                            local_slot: None, // Will be set during code generation
                            modifiers: param.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
                        };
                        
                        self.symbol_env.variables.insert(param_key, param_symbol); // Store in variables table
                        eprintln!("📝 ENHANCED_ENTER: Added parameter symbol '{}' for enum method '{}'", param.name, method_decl.name);
                    }
                }
                _ => {} // Handle other members if needed
            }
        }
        
        Ok(())
    }
    
    fn build_annotation_symbols(&mut self, annotation_decl: &AnnotationDecl, ast: &Ast) -> Result<()> {
        eprintln!("📝 ENHANCED_ENTER: Building symbols for annotation '{}'", annotation_decl.name);
        
        // Get fully qualified name and package info
        let (fully_qualified_name, package_name) = self.get_fully_qualified_name(&annotation_decl.name, ast);
        
        // Create the AnnotationSymbol and add it to the classes table
        let class_methods = HashMap::new();
        let class_symbol = crate::common::env::ClassSymbol {
            name: annotation_decl.name.clone(),
            fully_qualified_name,
            package_name,
            is_interface: false,
            is_enum: false,
            is_annotation: true,
            super_class: Some("java.lang.annotation.Annotation".to_string()), // Annotations extend Annotation
            interfaces: vec![],
            modifiers: annotation_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: vec![], // Annotations don't support type parameters in Java
            is_generic: false, // Annotations don't support type parameters in Java
            methods: class_methods,
            enum_constants: vec![],
        };
        
        self.symbol_env.classes.insert(annotation_decl.name.clone(), class_symbol);
        eprintln!("📝 ENHANCED_ENTER: Added annotation symbol '{}'", annotation_decl.name);
        
        // Handle annotation methods/elements
        for member in &annotation_decl.body {
            let method_key = format!("{}:{}", annotation_decl.name, member.name);
            
            let method_symbol = MethodSymbol {
                name: member.name.clone(),
                return_type: member.type_ref.name.clone(),
                parameter_types: vec![], // Annotation methods don't have parameters
                is_static: false, // Annotation methods are instance methods
                owner_class: annotation_decl.name.clone(),
                is_generic: false, // Annotation methods are not generic
                type_parameters: vec![], // No type parameters
            };
            
            self.symbol_env.methods.insert(method_key, method_symbol);
            eprintln!("📝 ENHANCED_ENTER: Added annotation method symbol '{}'", member.name);
        }
        
        Ok(())
    }
    
    /// Get the symbol environment
    pub fn get_symbol_environment(&self) -> &SymbolEnvironment {
        &self.symbol_env
    }
    
    /// Process type parameters and convert them to TypeParameterSymbol
    fn process_type_parameters(&self, type_params: &[crate::ast::TypeParam], owner: &str) -> Vec<crate::common::env::TypeParameterSymbol> {
        type_params.iter().enumerate().map(|(index, param)| {
            crate::common::env::TypeParameterSymbol {
                name: param.name.clone(),
                bounds: param.bounds.iter().map(|bound| bound.name.clone()).collect(),
                owner: owner.to_string(),
                index,
            }
        }).collect()
    }
    
    /// Extract package name from AST and create fully qualified name
    fn get_fully_qualified_name(&self, class_name: &str, ast: &crate::ast::Ast) -> (String, Option<String>) {
        match &ast.package_decl {
            Some(package_decl) => {
                let package_name = package_decl.name.clone();
                let fully_qualified_name = format!("{}.{}", package_name, class_name);
                (fully_qualified_name, Some(package_name))
            }
            None => {
                // Default package
                (class_name.to_string(), None)
            }
        }
    }

    /// Get the resolved type mappings
    pub fn get_resolved_types(&self) -> &HashMap<String, String> {
        &self.resolved_types
    }
    
    /// Process method body statements to find local variable declarations
    fn process_method_body_statements(&mut self, statements: &[crate::ast::Stmt], class_name: &str, method_name: &str) -> Result<()> {
        // Initialize slot counter: 0 for static methods, 1 for instance methods (slot 0 = 'this')
        let mut next_local_slot = if method_name == "<init>" || !self.is_static_method(class_name, method_name) {
            1 // Instance method: slot 0 is 'this'
        } else {
            0 // Static method: start from slot 0
        };
        
        // Account for method parameters (they get slots before local variables)
        let param_count = self.count_method_parameters(class_name, method_name);
        eprintln!("🔍 SLOT_DEBUG: Method '{}', static={}, param_count={}, next_local_slot before params={}", 
                 method_name, 
                 self.is_static_method(class_name, method_name),
                 param_count,
                 next_local_slot);
        next_local_slot += param_count;
        eprintln!("🔍 SLOT_DEBUG: Next local slot after accounting for parameters: {}", next_local_slot);
        
        for stmt in statements {
            next_local_slot = self.process_statement_with_slots(stmt, class_name, method_name, next_local_slot)?;
        }
        Ok(())
    }
    
    /// Check if a method is static
    fn is_static_method(&self, class_name: &str, method_name: &str) -> bool {
        // Look up method in symbol table and check if it's static
        // FIXED: Use same key format as when storing methods (ClassName:methodName)
        let method_key = format!("{}:{}", class_name, method_name);
        eprintln!("🔍 STATIC_CHECK: Looking for method key '{}'", method_key);
        if let Some(method_symbol) = self.symbol_env.methods.get(&method_key) {
            eprintln!("🔍 STATIC_CHECK: Found method '{}', is_static={}", method_name, method_symbol.is_static);
            method_symbol.is_static
        } else {
            eprintln!("🔍 STATIC_CHECK: Method '{}' not found in symbol table, defaulting to instance method", method_name);
            false // Default to instance method
        }
    }
    
    /// Build full type name including array dimensions and type erasure
    fn build_full_type_name(&self, type_ref: &crate::ast::TypeRef) -> String {
        // Apply type erasure for generic type parameters (T, E, K, V, etc.)
        let base_type = if type_ref.name.len() == 1 && type_ref.name.chars().next().unwrap().is_uppercase() {
            "java.lang.Object".to_string()
        } else {
            type_ref.name.clone()
        };
        
        let mut type_name = base_type;
        
        // Add array dimensions
        for _ in 0..type_ref.array_dims {
            type_name.push_str("[]");
        }
        
        type_name
    }
    
    /// Calculate type width for proper slot allocation (JavaC alignment)
    /// long and double types take 2 slots, all others take 1 slot
    fn get_type_width(&self, type_ref: &crate::ast::TypeRef) -> usize {
        // Array references always take 1 slot
        if type_ref.array_dims > 0 {
            return 1;
        }
        
        // Check primitive types that take 2 slots
        match type_ref.name.as_str() {
            "long" | "double" => 2,
            _ => 1, // int, float, boolean, char, byte, short, and all reference types
        }
    }
    
    /// Count method parameters to calculate starting local slot
    fn count_method_parameters(&self, class_name: &str, method_name: &str) -> usize {
        let method_pattern = format!("method:{}#{}::", class_name, method_name);
        self.symbol_env.variables.keys()
            .filter(|k| k.starts_with(&method_pattern))
            .filter(|k| {
                if let Some(var_symbol) = self.symbol_env.variables.get(*k) {
                    var_symbol.is_parameter
                } else {
                    false
                }
            })
            .count()
    }
    
    /// Process statements with slot tracking
    fn process_statement_with_slots(&mut self, stmt: &crate::ast::Stmt, class_name: &str, method_name: &str, mut next_local_slot: usize) -> Result<usize> {
        use crate::ast::Stmt;
        
        match stmt {
            Stmt::Declaration(var_decl_stmt) => {
                // Process local variable declarations and assign slots
                for variable in &var_decl_stmt.variables {
                    let var_key = format!("method:{}#{}::{}", class_name, method_name, variable.name);
                    
                    let var_symbol = VariableSymbol {
                        name: variable.name.clone(),
                        kind: SymbolKind::Variable,
                        owner: format!("{}:{}", class_name, method_name),
                        var_type: self.build_full_type_name(&var_decl_stmt.type_ref),
                        is_static: false,
                        is_parameter: false,
                        local_slot: Some(next_local_slot), // Assign proper local slot
                        modifiers: vec![],
                    };
                    
                    self.symbol_env.variables.insert(var_key, var_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added local variable '{}' with slot {} in method '{}::{}'", 
                             variable.name, next_local_slot, class_name, method_name);
                    
                    next_local_slot += self.get_type_width(&var_decl_stmt.type_ref); // Use type width (JavaC alignment)
                }
            }
            Stmt::Block(block) => {
                // Recursively process statements in blocks
                for stmt in &block.statements {
                    next_local_slot = self.process_statement_with_slots(stmt, class_name, method_name, next_local_slot)?;
                }
            }
            Stmt::If(if_stmt) => {
                // Process then and else branches
                next_local_slot = self.process_statement_with_slots(&if_stmt.then_branch, class_name, method_name, next_local_slot)?;
                if let Some(ref else_branch) = if_stmt.else_branch {
                    next_local_slot = self.process_statement_with_slots(else_branch, class_name, method_name, next_local_slot)?;
                }
            }
            Stmt::For(for_stmt) => {
                // Process for loop initialization statements (may contain variable declarations)
                for init_stmt in &for_stmt.init {
                    next_local_slot = self.process_statement_with_slots(init_stmt, class_name, method_name, next_local_slot)?;
                }
                // Process the for loop body
                next_local_slot = self.process_statement_with_slots(&for_stmt.body, class_name, method_name, next_local_slot)?;
            }
            Stmt::While(while_stmt) => {
                // Process while loop body
                next_local_slot = self.process_statement_with_slots(&while_stmt.body, class_name, method_name, next_local_slot)?;
            }
            Stmt::DoWhile(do_while_stmt) => {
                // Process do-while loop body
                next_local_slot = self.process_statement_with_slots(&do_while_stmt.body, class_name, method_name, next_local_slot)?;
            }
            Stmt::Try(try_stmt) => {
                // Process try block
                for stmt in &try_stmt.try_block.statements {
                    next_local_slot = self.process_statement_with_slots(stmt, class_name, method_name, next_local_slot)?;
                }
                // Process catch blocks
                for catch in &try_stmt.catch_clauses {
                    // Add catch parameter as a local variable with proper slot
                    let param_key = format!("method:{}#{}::{}", class_name, method_name, catch.parameter.name);
                    let param_symbol = VariableSymbol {
                        name: catch.parameter.name.clone(),
                        kind: SymbolKind::Variable,
                        owner: format!("{}:{}", class_name, method_name),
                        var_type: self.build_full_type_name(&catch.parameter.type_ref),
                        is_static: false,
                        is_parameter: false,
                        local_slot: Some(next_local_slot), // Assign proper slot
                        modifiers: vec![],
                    };
                    
                    self.symbol_env.variables.insert(param_key, param_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added catch parameter '{}' with slot {} in method '{}::{}'", 
                             catch.parameter.name, next_local_slot, class_name, method_name);
                    
                    next_local_slot += self.get_type_width(&catch.parameter.type_ref); // Use type width (JavaC alignment)
                    
                    // Process catch body
                    for stmt in &catch.block.statements {
                        next_local_slot = self.process_statement_with_slots(stmt, class_name, method_name, next_local_slot)?;
                    }
                }
                // Process finally block if it exists
                if let Some(ref finally_block) = try_stmt.finally_block {
                    for stmt in &finally_block.statements {
                        next_local_slot = self.process_statement_with_slots(stmt, class_name, method_name, next_local_slot)?;
                    }
                }
            }
            _ => {
                // For other statement types, no local variables are declared
            }
        }
        
        Ok(next_local_slot)
    }
    
    /// Recursively process individual statements to find variable declarations
    fn process_statement(&mut self, stmt: &crate::ast::Stmt, class_name: &str, method_name: &str) -> Result<()> {
        use crate::ast::Stmt;
        
        match stmt {
            Stmt::Declaration(var_decl) => {
                // Process each variable declarator
                for declarator in &var_decl.variables {
                    let var_key = format!("method:{}#{}::{}", class_name, method_name, declarator.name);
                    let var_symbol = VariableSymbol {
                        name: declarator.name.clone(),
                        kind: SymbolKind::Variable,
                        owner: format!("{}:{}", class_name, method_name),
                        var_type: var_decl.type_ref.name.clone(), // Use declared type
                        is_static: false, // Local variables are never static
                        is_parameter: false, // This is a local variable, not a parameter
                        local_slot: None, // Will be assigned during code generation
                        modifiers: vec![], // Local variables don't have modifiers
                    };
                    
                    self.symbol_env.variables.insert(var_key, var_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added local variable symbol '{}' in method '{}::{}'", declarator.name, class_name, method_name);
                }
            }
            Stmt::Block(block) => {
                // Recursively process statements in blocks
                self.process_method_body_statements(&block.statements, class_name, method_name)?;
            }
            Stmt::If(if_stmt) => {
                // Process then and else branches
                self.process_statement(&if_stmt.then_branch, class_name, method_name)?;
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.process_statement(else_branch, class_name, method_name)?;
                }
            }
            Stmt::For(for_stmt) => {
                // Process for loop initialization statements (may contain variable declarations)
                for init_stmt in &for_stmt.init {
                    self.process_statement(init_stmt, class_name, method_name)?;
                }
                // Process the for loop body
                self.process_statement(&for_stmt.body, class_name, method_name)?;
            }
            Stmt::While(while_stmt) => {
                // Process while loop body
                self.process_statement(&while_stmt.body, class_name, method_name)?;
            }
            Stmt::DoWhile(do_while_stmt) => {
                // Process do-while loop body
                self.process_statement(&do_while_stmt.body, class_name, method_name)?;
            }
            Stmt::Try(try_stmt) => {
                // Process try block
                self.process_method_body_statements(&try_stmt.try_block.statements, class_name, method_name)?;
                // Process catch blocks
                for catch in &try_stmt.catch_clauses {
                    // Add catch parameter as a local variable
                    let param_key = format!("method:{}#{}::{}", class_name, method_name, catch.parameter.name);
                    let param_symbol = VariableSymbol {
                        name: catch.parameter.name.clone(),
                        kind: SymbolKind::Variable,
                        owner: format!("{}:{}", class_name, method_name),
                        var_type: self.build_full_type_name(&catch.parameter.type_ref),
                        is_static: false,
                        is_parameter: false, // It's more like a local variable
                        local_slot: None,
                        modifiers: vec![],
                    };
                    
                    self.symbol_env.variables.insert(param_key, param_symbol);
                    eprintln!("📝 ENHANCED_ENTER: Added catch parameter symbol '{}' in method '{}::{}'", catch.parameter.name, class_name, method_name);
                    
                    // Process catch body
                    self.process_method_body_statements(&catch.block.statements, class_name, method_name)?;
                }
                // Process finally block
                if let Some(ref finally_block) = try_stmt.finally_block {
                    self.process_method_body_statements(&finally_block.statements, class_name, method_name)?;
                }
            }
            Stmt::EnhancedFor(enhanced_for_stmt) => {
                // Add enhanced for loop variable
                let var_key = format!("method:{}#{}::{}", class_name, method_name, enhanced_for_stmt.variable_name);
                let var_symbol = VariableSymbol {
                    name: enhanced_for_stmt.variable_name.clone(),
                    kind: SymbolKind::Variable,
                    owner: format!("{}:{}", class_name, method_name),
                    var_type: enhanced_for_stmt.variable_type.name.clone(),
                    is_static: false,
                    is_parameter: false,
                    local_slot: None,
                    modifiers: vec![],
                };
                
                self.symbol_env.variables.insert(var_key, var_symbol);
                eprintln!("📝 ENHANCED_ENTER: Added foreach variable symbol '{}' in method '{}::{}'", enhanced_for_stmt.variable_name, class_name, method_name);
                
                // Process foreach body
                self.process_statement(&enhanced_for_stmt.body, class_name, method_name)?;
            }
            _ => {
                // Other statement types (Expression, Return, Break, Continue, etc.) 
                // don't introduce new variables, so we don't need to process them
            }
        }
        
        Ok(())
    }
}

/// Check if a type name is a primitive type
fn is_primitive_type(type_name: &str) -> bool {
    matches!(type_name, "boolean" | "byte" | "char" | "short" | "int" | "long" | "float" | "double" | "void")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ImportDecl, Span, Modifier};
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_enhanced_enter_integration() {
        // Create test classpath
        let temp_dir = TempDir::new().unwrap();
        let java_util = temp_dir.path().join("java/util");
        fs::create_dir_all(&java_util).unwrap();
        fs::write(java_util.join("List.java"), "package java.util; public interface List<T> {}").unwrap();
        
        let mut enhanced_enter = EnhancedEnter::new(&temp_dir.path().to_string_lossy());
        
        // Create test AST with unresolved type references
        let ast = Ast {
            package_decl: None,
            imports: vec![
                ImportDecl {
                    name: "java.util.List".to_string(),
                    is_static: false,
                    is_wildcard: false,
                    span: Span::default(),
                }
            ],
            type_decls: vec![
                TypeDecl::Class(ClassDecl {
                    modifiers: vec![],
                    annotations: vec![],
                    name: "TestClass".to_string(),
                    type_params: vec![],
                    extends: None,
                    implements: vec![],
                    body: vec![
                        ClassMember::Field(FieldDecl {
                            modifiers: vec![],
                            annotations: vec![],
                            type_ref: TypeRef {
                                name: "List".to_string(), // Unresolved
                                type_args: vec![],
                                annotations: vec![],
                                array_dims: 0,
                                span: Span::default(),
                            },
                            name: "list".to_string(),
                            initializer: None,
                            span: Span::default(),
                        })
                    ],
                    span: Span::default(),
                })
            ],
            span: Span::default(),
        };
        
        let processed_ast = enhanced_enter.process(ast).unwrap();
        
        // Verify type resolution
        if let TypeDecl::Class(ref class_decl) = processed_ast.type_decls[0] {
            // The List field type should now be resolved to java.util.List
            if let ClassMember::Field(ref field_decl) = class_decl.body[0] {
                assert_eq!(field_decl.type_ref.name, "java.util.List");
            }
        }
        
        // Verify symbol table
        let symbol_env = enhanced_enter.get_symbol_environment();
        assert!(symbol_env.fields.contains_key("class:TestClass::list"));
    }
}