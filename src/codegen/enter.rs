//! Enter phase - Symbol table construction and import resolution
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Enter` class.
//! This phase builds symbol tables for all compilation units, establishes
//! inheritance relationships, and handles import statements.

use crate::ast::{Ast, TypeDecl, ClassDecl, InterfaceDecl, EnumDecl, AnnotationDecl, TypeParam, TypeRef};
use crate::common::error::Result;
use crate::common::classpath;
use std::collections::HashMap;

/// Symbol kinds matching JavaC's Kinds.java
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    /// Variables (local variables, parameters) - JavaC VAR kind
    Variable,
    /// Instance and static fields - JavaC VAR kind with class owner
    Field,
    /// Methods and constructors - JavaC MTH kind  
    Method,
    /// Types (classes, interfaces, enums) - JavaC TYP kind
    Type,
}

/// Variable symbol following JavaC's VarSymbol pattern
#[derive(Debug, Clone)]
pub struct VariableSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub owner: String,          // Method or class that declares this (e.g., "method:main", "class:MyClass")
    pub var_type: String,       // Resolved type name
    pub is_static: bool,        // For fields - whether it's static
    pub is_parameter: bool,     // For variables - whether it's a method parameter
    pub local_slot: Option<usize>, // For local variables - JVM local slot number
    pub modifiers: Vec<String>, // Access modifiers (public, private, etc.)
}

/// Type parameter symbol for generic types
#[derive(Debug, Clone)]
pub struct TypeParameterSymbol {
    pub name: String,
    pub bounds: Vec<String>,
    pub owner: String, // Class or method that declares this type parameter
    pub index: usize,  // Position in type parameter list
}

/// Method symbol for generic methods
#[derive(Debug, Clone)]
pub struct MethodSymbol {
    pub name: String,
    pub owner_class: String,
    pub type_parameters: Vec<TypeParameterSymbol>,
    pub parameter_types: Vec<String>,
    pub return_type: String,
    pub is_static: bool,
    pub is_generic: bool,
}

/// Symbol table entry for a class, interface, enum, or annotation
#[derive(Debug, Clone)]
pub struct ClassSymbol {
    pub name: String,
    pub fully_qualified_name: String,
    pub package_name: Option<String>,
    pub is_interface: bool,
    pub is_enum: bool,
    pub is_annotation: bool,
    pub super_class: Option<String>,
    pub interfaces: Vec<String>,
    pub modifiers: Vec<String>,
    /// Generic type parameters declared on this class
    pub type_parameters: Vec<TypeParameterSymbol>,
    /// Whether this class is generic
    pub is_generic: bool,
    /// Methods declared in this class (including generic methods)
    pub methods: HashMap<String, MethodSymbol>,
    /// Enum constants (only for enums)
    pub enum_constants: Vec<String>,
}

/// Global symbol environment containing all symbols
#[derive(Debug, Clone)]
pub struct SymbolEnvironment {
    /// Map from class name to ClassSymbol
    pub classes: HashMap<String, ClassSymbol>,
    /// Import statements resolved to fully qualified names (single-type imports)
    pub imports: HashMap<String, String>,
    /// Wildcard imports - list of package names for on-demand imports
    pub wildcard_imports: Vec<String>,
    /// Static imports - map from simple name to fully qualified name
    pub static_imports: HashMap<String, String>,
    /// Static wildcard imports - list of class names for static on-demand imports
    pub static_wildcard_imports: Vec<String>,
    /// Current package being processed
    pub current_package: Option<String>,
    /// Map from type parameter name to its symbol (scoped by owner)
    pub type_parameters: HashMap<String, TypeParameterSymbol>,
    /// Map from method signature to MethodSymbol
    pub methods: HashMap<String, MethodSymbol>,
    /// Variable symbols - local variables and parameters (scoped by owner method)
    /// Key format: "method:owner::varname" (e.g., "method:main::x", "method:foo::args")
    pub variables: HashMap<String, VariableSymbol>,
    /// Field symbols - instance and static fields (scoped by owner class)
    /// Key format: "class:owner::fieldname" (e.g., "class:MyClass::next", "class:System::out")
    pub fields: HashMap<String, VariableSymbol>,
    /// Generic instantiation cache (T -> String, T -> Integer, etc.)
    pub instantiation_cache: HashMap<String, Vec<String>>,
}

impl SymbolEnvironment {
    /// Resolve simple type name to fully qualified name
    /// Follows Java name resolution rules: single import -> current package -> wildcard import -> java.lang
    pub fn resolve_type(&self, simple_name: &str) -> Option<String> {
        // 1. Single-type import
        if let Some(qualified) = self.imports.get(simple_name) {
            eprintln!("🔍 RESOLVE: {} -> {} (single import)", simple_name, qualified);
            return Some(qualified.clone());
        }
        
        // 2. Current package classes
        if let Some(ref pkg) = self.current_package {
            let qualified = format!("{}.{}", pkg, simple_name);
            if self.classes.contains_key(&qualified) {
                eprintln!("🔍 RESOLVE: {} -> {} (current package)", simple_name, qualified);
                return Some(qualified);
            }
        }
        
        // 3. Type-import-on-demand (wildcard imports)
        for package in &self.wildcard_imports {
            let candidate = format!("{}.{}", package, simple_name);
            // Check if it's a known type or can be resolved through classpath
            if self.classes.contains_key(&candidate) || classpath::class_exists(&candidate) {
                eprintln!("🔍 RESOLVE: {} -> {} (wildcard import)", simple_name, candidate);
                return Some(candidate);
            }
        }
        
        // 4. java.lang package (implicit import)
        let java_lang_candidate = format!("java.lang.{}", simple_name);
        if classpath::class_exists(&java_lang_candidate) {
            eprintln!("🔍 RESOLVE: {} -> {} (java.lang)", simple_name, java_lang_candidate);
            return Some(java_lang_candidate);
        }
        
        eprintln!("⚠️ RESOLVE: Cannot resolve type: {}", simple_name);
        None
    }
    
    /// Check if a type is known (exists in symbol table)
    pub fn is_known_type(&self, fully_qualified_name: &str) -> bool {
        self.classes.contains_key(fully_qualified_name)
    }
    
    /// Get class symbol by fully qualified name
    pub fn get_class_symbol(&self, fully_qualified_name: &str) -> Option<&ClassSymbol> {
        self.classes.get(fully_qualified_name)
    }
    
    /// Resolve type to internal name format (java.lang.String -> java/lang/String)
    pub fn resolve_to_internal_name(&self, simple_name: &str) -> String {
        self.resolve_type(simple_name)
            .unwrap_or_else(|| simple_name.to_string())
            .replace('.', "/")
    }
    
    /// Add a variable symbol (local variable or parameter) - follows JavaC's VarSymbol creation
    pub fn add_variable(&mut self, name: &str, owner_method: &str, var_type: &str, 
                       is_parameter: bool, local_slot: Option<usize>) {
        let key = format!("method:{}::{}", owner_method, name);
        let symbol = VariableSymbol {
            name: name.to_string(),
            kind: SymbolKind::Variable,
            owner: format!("method:{}", owner_method),
            var_type: var_type.to_string(),
            is_static: false,
            is_parameter,
            local_slot,
            modifiers: Vec::new(),
        };
        self.variables.insert(key, symbol);
        eprintln!("📝 ENTER: Added variable symbol '{}' in method '{}' (slot: {:?})", name, owner_method, local_slot);
    }
    
    /// Add a field symbol (instance or static field) - follows JavaC's VarSymbol creation for fields
    pub fn add_field(&mut self, name: &str, owner_class: &str, field_type: &str, 
                     is_static: bool, modifiers: Vec<String>) {
        let key = format!("class:{}::{}", owner_class, name);
        let symbol = VariableSymbol {
            name: name.to_string(),
            kind: SymbolKind::Field,
            owner: format!("class:{}", owner_class),
            var_type: field_type.to_string(),
            is_static,
            is_parameter: false,
            local_slot: None,
            modifiers,
        };
        self.fields.insert(key, symbol);
        eprintln!("📝 ENTER: Added field symbol '{}' in class '{}' (static: {})", name, owner_class, is_static);
    }
    
    /// Look up a variable symbol by name and method context - follows JavaC's lookup pattern
    pub fn lookup_variable(&self, name: &str, method_context: &str) -> Option<&VariableSymbol> {
        let key = format!("method:{}::{}", method_context, name);
        self.variables.get(&key)
    }
    
    /// Look up a field symbol by name and class context - follows JavaC's lookup pattern
    pub fn lookup_field(&self, name: &str, class_context: &str) -> Option<&VariableSymbol> {
        let key = format!("class:{}::{}", class_context, name);
        self.fields.get(&key)
    }
    
    /// Resolve identifier in context (like JavaC's Resolve.resolveIdent)
    /// Returns the appropriate symbol based on scoping rules
    pub fn resolve_identifier(&self, name: &str, method_context: Option<&str>, class_context: &str) -> Option<&VariableSymbol> {
        // 1. First check local variables and parameters if we're in a method
        if let Some(method) = method_context {
            if let Some(var_symbol) = self.lookup_variable(name, method) {
                eprintln!("🔍 RESOLVE_ID: Found variable '{}' in method '{}'", name, method);
                return Some(var_symbol);
            }
        }
        
        // 2. Then check instance and static fields of the current class
        if let Some(field_symbol) = self.lookup_field(name, class_context) {
            eprintln!("🔍 RESOLVE_ID: Found field '{}' in class '{}'", name, class_context);
            return Some(field_symbol);
        }
        
        // TODO: 3. Check inherited fields from superclasses
        // TODO: 4. Check static imports
        
        eprintln!("⚠️ RESOLVE_ID: Could not resolve identifier '{}'", name);
        None
    }
}

impl Default for SymbolEnvironment {
    fn default() -> Self {
        Self {
            classes: HashMap::new(),
            imports: HashMap::new(),
            wildcard_imports: Vec::new(),
            static_imports: HashMap::new(),
            static_wildcard_imports: Vec::new(),
            current_package: None,
            type_parameters: HashMap::new(),
            methods: HashMap::new(),
            variables: HashMap::new(),      // Initialize new field
            fields: HashMap::new(),         // Initialize new field  
            instantiation_cache: HashMap::new(),
        }
    }
}

/// Enter phase processor - corresponds to JavaC's Enter class
pub struct Enter {
    pub symbol_env: SymbolEnvironment,
}

impl Enter {
    pub fn new() -> Self {
        Self {
            symbol_env: SymbolEnvironment::default(),
        }
    }
    
    /// Process AST through Enter phase - build symbol tables
    /// Corresponds to JavaC's Enter.main() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        eprintln!("🔍 ENTER: Starting symbol table construction");
        
        // Process package declaration
        if let Some(ref package) = ast.package_decl {
            self.symbol_env.current_package = Some(package.name.clone());
            eprintln!("📦 ENTER: Package: {}", package.name);
        }
        
        // Process import declarations
        for import in &ast.imports {
            self.process_import(import)?;
        }
        
        // Process type declarations (classes, interfaces, enums)
        for type_decl in &ast.type_decls {
            self.process_type_decl(type_decl)?;
        }
        
        eprintln!("✅ ENTER: Symbol table construction complete");
        eprintln!("📊 ENTER: {} classes, {} imports, {} type parameters", 
                 self.symbol_env.classes.len(), 
                 self.symbol_env.imports.len(),
                 self.symbol_env.type_parameters.len());
        
        Ok(ast)
    }
    
    /// Process import declaration - JavaC style import resolution
    /// Handles both static and wildcard imports following JavaC's MemberEnter.visitImport pattern
    fn process_import(&mut self, import: &crate::ast::ImportDecl) -> Result<()> {
        if import.is_wildcard {
            // Import on demand (wildcard import)
            if import.is_static {
                // Static wildcard import: import static Class.*
                eprintln!("📦 ENTER: Static wildcard import: {}.*", import.name);
                self.symbol_env.static_wildcard_imports.push(import.name.clone());
                self.import_static_all(&import.name)?;
            } else {
                // Package wildcard import: import package.*
                eprintln!("📦 ENTER: Wildcard import: {}.*", import.name);
                self.symbol_env.wildcard_imports.push(import.name.clone());
                self.import_all(&import.name)?;
            }
        } else {
            // Named import
            if import.is_static {
                // Static named import: import static Class.member
                eprintln!("📥 ENTER: Static import: {}", import.name);
                self.import_named_static(&import.name)?;
            } else {
                // Single type import: import Class
                eprintln!("📥 ENTER: Type import: {}", import.name);
                self.import_named(&import.name)?;
            }
        }
        
        Ok(())
    }
    
    /// Import a single named type (JavaC importNamed equivalent)
    fn import_named(&mut self, qualified_name: &str) -> Result<()> {
        let simple_name = qualified_name.split('.').last().unwrap_or(qualified_name);
        self.symbol_env.imports.insert(simple_name.to_string(), qualified_name.to_string());
        eprintln!("✅ ENTER: Imported type: {} -> {}", simple_name, qualified_name);
        Ok(())
    }
    
    /// Import all types from a package (JavaC importAll equivalent)
    fn import_all(&mut self, package_name: &str) -> Result<()> {
        // For wildcard imports, we store the package name and resolve types on demand
        // This matches JavaC's behavior where wildcard imports are resolved during symbol lookup
        eprintln!("✅ ENTER: Added wildcard import for package: {}", package_name);
        Ok(())
    }
    
    /// Import a single static member (JavaC importNamedStatic equivalent)
    fn import_named_static(&mut self, qualified_name: &str) -> Result<()> {
        if let Some(member_name) = qualified_name.split('.').last() {
            self.symbol_env.static_imports.insert(member_name.to_string(), qualified_name.to_string());
            eprintln!("✅ ENTER: Imported static member: {} -> {}", member_name, qualified_name);
        }
        Ok(())
    }
    
    /// Import all static members from a class (JavaC importStaticAll equivalent)
    fn import_static_all(&mut self, class_name: &str) -> Result<()> {
        // For static wildcard imports, we store the class name and resolve members on demand
        // This matches JavaC's behavior for static import resolution
        eprintln!("✅ ENTER: Added static wildcard import for class: {}", class_name);
        Ok(())
    }
    
    /// Process type declaration - corresponds to JavaC's visitClassDef
    fn process_type_decl(&mut self, type_decl: &TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.process_class_decl(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                self.process_interface_decl(interface_decl)?;
            }
            TypeDecl::Enum(enum_decl) => {
                self.process_enum_decl(enum_decl)?;
            }
            TypeDecl::Annotation(annotation_decl) => {
                self.process_annotation_decl(annotation_decl)?;
            }
        }
        Ok(())
    }
    
    /// Process class declaration with generic support
    fn process_class_decl(&mut self, class_decl: &ClassDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, class_decl.name)
        } else {
            class_decl.name.clone()
        };
        
        eprintln!("🏛️  ENTER: Processing class: {}", fully_qualified_name);
        
        // Process type parameters
        let mut type_parameters = Vec::new();
        for (index, type_param) in class_decl.type_params.iter().enumerate() {
            let type_param_symbol = self.process_type_parameter(type_param, &class_decl.name, index)?;
            type_parameters.push(type_param_symbol);
        }
        
        // Determine if class is generic
        let is_generic = !class_decl.type_params.is_empty();
        
        if is_generic {
            eprintln!("🧬 ENTER: Generic class with {} type parameters", type_parameters.len());
        }
        
        // Process superclass
        let super_class = class_decl.extends.as_ref().map(|ext| self.resolve_type_name(ext));
        
        // Process interfaces
        let interfaces: Vec<String> = class_decl.implements.iter()
            .map(|iface| self.resolve_type_name(iface))
            .collect();
        
        // Process class members to collect method and field information
        let mut methods = HashMap::new();
        for member in &class_decl.body {
            match member {
                crate::ast::ClassMember::Method(method_decl) => {
                    let method_symbol = self.process_method_decl(method_decl, &class_decl.name)?;
                    let method_signature = format!("{}:{}", class_decl.name, method_decl.name);
                    methods.insert(method_signature.clone(), method_symbol.clone());
                    self.symbol_env.methods.insert(method_signature, method_symbol);
                }
                crate::ast::ClassMember::Field(field_decl) => {
                    // Process field declaration and add to symbol table
                    let is_static = field_decl.modifiers.iter().any(|m| matches!(m, crate::ast::Modifier::Static));
                    let field_modifiers: Vec<String> = field_decl.modifiers.iter()
                        .map(|m| format!("{:?}", m))
                        .collect();
                    
                    // Add field to symbol environment
                    self.symbol_env.add_field(
                        &field_decl.name,
                        &class_decl.name,
                        &self.resolve_type_name(&field_decl.type_ref),
                        is_static,
                        field_modifiers
                    );
                }
                crate::ast::ClassMember::Constructor(constructor_decl) => {
                    // Process constructor as a special method with name "<init>"
                    let method_symbol = self.process_constructor_decl(constructor_decl, &class_decl.name)?;
                    let method_signature = format!("{}:<init>", class_decl.name);
                    methods.insert(method_signature.clone(), method_symbol.clone());
                    self.symbol_env.methods.insert(method_signature, method_symbol);
                }
                _ => {
                    // Handle other member types (nested types, etc.) if needed
                }
            }
        }
        
        // Create class symbol
        let class_symbol = ClassSymbol {
            name: class_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: false,
            is_enum: false,
            is_annotation: false,
            super_class,
            interfaces,
            modifiers: class_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters,
            is_generic,
            methods,
            enum_constants: Vec::new(),
        };
        
        // Store class symbol
        self.symbol_env.classes.insert(class_decl.name.clone(), class_symbol);
        
        eprintln!("✅ ENTER: Class {} processed", class_decl.name);
        Ok(())
    }
    
    /// Process interface declaration with generic support
    fn process_interface_decl(&mut self, interface_decl: &InterfaceDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, interface_decl.name)
        } else {
            interface_decl.name.clone()
        };
        
        eprintln!("🔌 ENTER: Processing interface: {}", fully_qualified_name);
        
        // Process type parameters
        let mut type_parameters = Vec::new();
        for (index, type_param) in interface_decl.type_params.iter().enumerate() {
            let type_param_symbol = self.process_type_parameter(type_param, &interface_decl.name, index)?;
            type_parameters.push(type_param_symbol);
        }
        
        let is_generic = !interface_decl.type_params.is_empty();
        
        // Process extended interfaces
        let interfaces: Vec<String> = interface_decl.extends.iter()
            .map(|iface| self.resolve_type_name(iface))
            .collect();
        
        // Create class symbol for interface
        let interface_symbol = ClassSymbol {
            name: interface_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: true,
            is_enum: false,
            is_annotation: false,
            super_class: Some("java.lang.Object".to_string()),
            interfaces,
            modifiers: interface_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters,
            is_generic,
            methods: HashMap::new(),
            enum_constants: Vec::new(),
        };
        
        self.symbol_env.classes.insert(interface_decl.name.clone(), interface_symbol);
        
        eprintln!("✅ ENTER: Interface {} processed", interface_decl.name);
        Ok(())
    }
    
    /// Process type parameter and create symbol
    fn process_type_parameter(&mut self, type_param: &TypeParam, owner: &str, index: usize) -> Result<TypeParameterSymbol> {
        let bounds: Vec<String> = type_param.bounds.iter()
            .map(|bound| self.resolve_type_name(bound))
            .collect();
        
        let symbol = TypeParameterSymbol {
            name: type_param.name.clone(),
            bounds,
            owner: owner.to_string(),
            index,
        };
        
        // Store in scoped map (owner:name -> symbol)
        let scoped_name = format!("{}:{}", owner, type_param.name);
        self.symbol_env.type_parameters.insert(scoped_name, symbol.clone());
        
        eprintln!("🧬 ENTER: Type parameter {} with {} bounds", type_param.name, symbol.bounds.len());
        
        Ok(symbol)
    }
    
    /// Process method declaration and create symbol
    fn process_method_decl(&mut self, method_decl: &crate::ast::MethodDecl, owner_class: &str) -> Result<MethodSymbol> {
        // Process method type parameters
        let mut type_parameters = Vec::new();
        for (index, type_param) in method_decl.type_params.iter().enumerate() {
            let method_owner = format!("{}#{}", owner_class, method_decl.name);
            let type_param_symbol = self.process_type_parameter(type_param, &method_owner, index)?;
            type_parameters.push(type_param_symbol);
        }
        
        let is_generic = !method_decl.type_params.is_empty();
        let is_static = method_decl.modifiers.iter()
            .any(|m| matches!(m, crate::ast::Modifier::Static));
        
        // Process parameter types
        let parameter_types: Vec<String> = method_decl.parameters.iter()
            .map(|param| self.resolve_type_name(&param.type_ref))
            .collect();
        
        // Process return type
        let return_type = method_decl.return_type.as_ref()
            .map(|rt| self.resolve_type_name(rt))
            .unwrap_or_else(|| "void".to_string());
        
        let method_symbol = MethodSymbol {
            name: method_decl.name.clone(),
            owner_class: owner_class.to_string(),
            type_parameters,
            parameter_types,
            return_type,
            is_static,
            is_generic,
        };
        
        // Add method parameters to symbol table
        let method_owner = format!("{}#{}", owner_class, method_decl.name);
        let mut local_slot = if is_static { 0 } else { 1 }; // Static methods start at 0, instance methods at 1 (for 'this')
        
        for param in &method_decl.parameters {
            let param_type = self.resolve_type_name(&param.type_ref);
            self.symbol_env.add_variable(&param.name, &method_owner, &param_type, true, Some(local_slot));
            
            // JVM local variable slot computation (double/long take 2 slots)
            local_slot += match param_type.as_str() {
                "long" | "double" => 2,
                _ => 1,
            };
            
            eprintln!("📝 ENTER: Added parameter '{}' type '{}' to method '{}' at slot {}", 
                     param.name, param_type, method_owner, local_slot - 1);
        }
        
        if is_generic {
            eprintln!("🧬 ENTER: Generic method {} with {} type parameters", 
                     method_decl.name, method_symbol.type_parameters.len());
        }
        
        Ok(method_symbol)
    }
    
    /// Process constructor declaration and create method symbol (JavaC aligned)
    fn process_constructor_decl(&mut self, constructor_decl: &crate::ast::ConstructorDecl, owner_class: &str) -> Result<MethodSymbol> {
        // Constructors don't have their own type parameters in Java
        // They inherit type parameters from their declaring class
        let type_parameters = Vec::new();
        let is_generic = false;
        let is_static = false; // Constructors are never static
        
        // Process parameter types
        let parameter_types: Vec<String> = constructor_decl.parameters.iter()
            .map(|param| self.resolve_type_name(&param.type_ref))
            .collect();
        
        // Constructor return type is void
        let return_type = "void".to_string();
        
        let method_symbol = MethodSymbol {
            name: "<init>".to_string(), // JVM constructor name
            owner_class: owner_class.to_string(),
            type_parameters,
            parameter_types,
            return_type,
            is_static,
            is_generic,
        };
        
        // Add constructor parameters to symbol table
        let method_owner = format!("{}#<init>", owner_class);
        let mut local_slot = 1; // Constructors always start at slot 1 (for 'this')
        
        for param in &constructor_decl.parameters {
            let param_type = self.resolve_type_name(&param.type_ref);
            self.symbol_env.add_variable(&param.name, &method_owner, &param_type, true, Some(local_slot));
            
            // JVM local variable slot computation (double/long take 2 slots)
            local_slot += match param_type.as_str() {
                "long" | "double" => 2,
                _ => 1,
            };
            
            eprintln!("📝 ENTER: Added constructor parameter '{}' type '{}' to '{}' at slot {}", 
                     param.name, param_type, method_owner, local_slot - 1);
        }
        
        if is_generic {
            eprintln!("🧬 ENTER: Generic constructor <init> with {} type parameters", 
                     method_symbol.type_parameters.len());
        }
        
        Ok(method_symbol)
    }
    
    /// Resolve type name from TypeRef to string representation (JavaC aligned with import resolution)
    fn resolve_type_name(&self, type_ref: &TypeRef) -> String {
        use crate::codegen::array_type_info::{ArrayTypeInfo, ArrayAwareTypeResolver};
        
        // Use ArrayAwareTypeResolver for proper array handling
        if type_ref.array_dims > 0 {
            let final_type = ArrayAwareTypeResolver::resolve_type_ref(type_ref);
            eprintln!("🔍 ENTER: Resolved array type '{}' (array_dims={}) -> '{}'", 
                     type_ref.name, type_ref.array_dims, final_type);
            return final_type;
        }
        
        // Non-array type resolution
        let base_name = if type_ref.type_args.is_empty() {
            // Simple type - resolve through import hierarchy (JavaC pattern)
            self.resolve_simple_type_name(&type_ref.name)
        } else {
            // Generic type with type arguments
            let resolved_base = self.resolve_simple_type_name(&type_ref.name);
            let args: Vec<String> = type_ref.type_args.iter()
                .map(|arg| self.resolve_type_arg(arg))
                .collect();
            format!("{}<{}>", resolved_base, args.join(", "))
        };
        
        eprintln!("🔍 ENTER: Resolved non-array type '{}' -> '{}'", type_ref.name, base_name);
        base_name
    }
    
    /// Convert a type name to JVM descriptor format using enhanced ArrayTypeInfo
    fn type_name_to_descriptor(&self, type_name: &str) -> String {
        use crate::codegen::array_type_info::ArrayTypeInfo;
        
        // Clean up malformed type names like "test/int" -> "int"
        let clean_type_name = if type_name.starts_with("test/") {
            &type_name[5..] // Remove "test/" prefix
        } else {
            type_name
        };
        
        match clean_type_name {
            // Primitive types
            "int" => "I".to_string(),
            "long" => "J".to_string(),
            "float" => "F".to_string(),
            "double" => "D".to_string(),
            "boolean" => "Z".to_string(),
            "byte" => "B".to_string(),
            "char" => "C".to_string(),
            "short" => "S".to_string(),
            "void" => "V".to_string(),
            _ => {
                // Reference types: convert to L<classname>;
                let class_name = clean_type_name.replace('.', "/");
                format!("L{};", class_name)
            }
        }
    }
    
    /// Resolve a simple type name through the import resolution hierarchy
    /// Following JavaC's symbol resolution order: single imports -> wildcard imports -> classpath
    fn resolve_simple_type_name(&self, simple_name: &str) -> String {
        // 1. Check single-type imports (highest precedence)
        if let Some(qualified_name) = self.symbol_env.imports.get(simple_name) {
            eprintln!("🎯 ENTER: Found in single imports: {} -> {}", simple_name, qualified_name);
            return qualified_name.clone();
        }
        
        // 2. Check wildcard imports (on-demand)
        for package_name in &self.symbol_env.wildcard_imports {
            let candidate = format!("{}.{}", package_name, simple_name);
            // Try to resolve this candidate through classpath
            if let Some(resolved) = classpath::resolve_class_name(&candidate) {
                eprintln!("🎯 ENTER: Found via wildcard import: {} -> {}", simple_name, resolved);
                return resolved.to_string();
            }
        }
        
        // 3. Check current package
        if let Some(ref current_pkg) = self.symbol_env.current_package {
            let candidate = format!("{}.{}", current_pkg, simple_name);
            if let Some(resolved) = classpath::resolve_class_name(&candidate) {
                eprintln!("🎯 ENTER: Found in current package: {} -> {}", simple_name, resolved);
                return resolved.to_string();
            }
        }
        
        // 4. Try direct classpath resolution (for java.lang.* types)
        if let Some(resolved) = classpath::resolve_class_name(simple_name) {
            eprintln!("🎯 ENTER: Found in classpath: {} -> {}", simple_name, resolved);
            return resolved.to_string();
        }
        
        // 5. Fallback to package-based resolution
        let fallback = classpath::resolve_class_name_with_fallback(simple_name, self.symbol_env.current_package.as_deref());
        eprintln!("🔍 ENTER: Using fallback resolution: {} -> {}", simple_name, fallback);
        fallback
    }
    
    /// Resolve type argument to string representation
    fn resolve_type_arg(&self, type_arg: &crate::ast::TypeArg) -> String {
        match type_arg {
            crate::ast::TypeArg::Type(type_ref) => self.resolve_type_name(type_ref),
            crate::ast::TypeArg::Wildcard(wildcard) => {
                match &wildcard.bound {
                    Some((crate::ast::BoundKind::Extends, bound)) => {
                        format!("? extends {}", self.resolve_type_name(bound))
                    }
                    Some((crate::ast::BoundKind::Super, bound)) => {
                        format!("? super {}", self.resolve_type_name(bound))
                    }
                    None => "?".to_string(),
                }
            }
        }
    }
    
    /// Process enum declaration - corresponds to JavaC's visitClassDef for enums
    fn process_enum_decl(&mut self, enum_decl: &EnumDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, enum_decl.name)
        } else {
            enum_decl.name.clone()
        };
        
        eprintln!("🔢 ENTER: Processing enum: {}", fully_qualified_name);
        
        // Collect enum constants
        let enum_constants: Vec<String> = enum_decl.constants
            .iter()
            .map(|c| c.name.clone())
            .collect();
        
        eprintln!("📝 ENTER: Enum constants: {:?}", enum_constants);
        
        // Process interfaces implemented by the enum
        let interfaces: Vec<String> = enum_decl.implements
            .iter()
            .map(|iface| self.resolve_type_name(iface))
            .collect();
        
        // Create enum symbol (enums extend java.lang.Enum)
        let enum_symbol = ClassSymbol {
            name: enum_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: false,
            is_enum: true,
            is_annotation: false,
            super_class: Some("java.lang.Enum".to_string()),
            interfaces,
            modifiers: enum_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: Vec::new(), // Enums cannot have type parameters
            is_generic: false,
            methods: HashMap::new(),
            enum_constants,
        };
        
        self.symbol_env.classes.insert(enum_decl.name.clone(), enum_symbol);
        
        eprintln!("✅ ENTER: Enum {} processed", enum_decl.name);
        Ok(())
    }
    
    /// Process annotation declaration - corresponds to JavaC's visitClassDef for annotations
    fn process_annotation_decl(&mut self, annotation_decl: &AnnotationDecl) -> Result<()> {
        let fully_qualified_name = if let Some(ref package) = self.symbol_env.current_package {
            format!("{}.{}", package, annotation_decl.name)
        } else {
            annotation_decl.name.clone()
        };
        
        eprintln!("📋 ENTER: Processing annotation: {}", fully_qualified_name);
        
        // Create annotation symbol (annotations extend java.lang.annotation.Annotation)
        let annotation_symbol = ClassSymbol {
            name: annotation_decl.name.clone(),
            fully_qualified_name: fully_qualified_name.clone(),
            package_name: self.symbol_env.current_package.clone(),
            is_interface: true, // Annotations are special interfaces
            is_enum: false,
            is_annotation: true,
            super_class: Some("java.lang.Object".to_string()),
            interfaces: vec!["java.lang.annotation.Annotation".to_string()],
            modifiers: annotation_decl.modifiers.iter().map(|m| format!("{:?}", m)).collect(),
            type_parameters: Vec::new(), // Annotations cannot have type parameters
            is_generic: false,
            methods: HashMap::new(),
            enum_constants: Vec::new(),
        };
        
        self.symbol_env.classes.insert(annotation_decl.name.clone(), annotation_symbol);
        
        eprintln!("✅ ENTER: Annotation {} processed", annotation_decl.name);
        Ok(())
    }
    
    /// Get symbol environment for use by subsequent phases
    pub fn get_symbol_environment(&self) -> &SymbolEnvironment {
        &self.symbol_env
    }
}

impl Default for Enter {
    fn default() -> Self {
        Self::new()
    }
}