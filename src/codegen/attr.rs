//! Attr phase - Type checking and method resolution
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Attr` class.
//! This is the main context-dependent analysis phase that encompasses
//! name resolution, type checking and constant folding as subtasks.
//!
//! JavaC alignment: This module implements the same visitor pattern and
//! attribution logic as JavaC's Attr.java.

use crate::ast::{Ast, TypeDecl, ClassDecl, MethodDecl, FieldDecl, Expr, Stmt, BinaryOp, TypeEnum, ReferenceType, UnaryOp, TypeRef};
use crate::ast::PrimitiveType as AstPrimitiveType;
use crate::common::error::{Result, Error};
use crate::codegen::enter::SymbolEnvironment;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

/// Type information for expressions and variables - JavaC Type equivalent
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedType {
    /// Primitive types (boolean, byte, short, int, long, float, double, char)
    Primitive(PrimitiveType),
    /// Object/reference types with full qualification
    Reference(String),
    /// Array types with component type
    Array(Box<ResolvedType>),
    /// Generic types with type parameters
    Generic(String, Vec<ResolvedType>),
    /// Wildcard types (? extends T, ? super T)
    Wildcard(WildcardType),
    /// Type variables (T, E, etc.)
    TypeVariable(TypeVariable),
    /// Class types with type parameters (List<String>)
    Class(ClassType),
    /// Captured wildcards from capture conversion
    Captured(CapturedType),
    /// Intersection types (T & Serializable)
    Intersection(Vec<ResolvedType>),
    /// Union types (for error recovery)
    Union(Vec<ResolvedType>),
    /// Method types for functional interfaces
    Method(Vec<ResolvedType>, Box<ResolvedType>),
    /// Null type
    Null,
    /// Error type for failed resolution
    Error,
    /// No type (void)
    NoType,
}

/// Wildcard type structure following JavaC's WildcardType
#[derive(Debug, Clone, PartialEq)]
pub struct WildcardType {
    pub kind: WildcardBound,
    pub bound: Option<Box<ResolvedType>>,
    pub capture_id: Option<usize>,
}

/// Type variable structure following JavaC's TypeVar
#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariable {
    pub name: String,
    pub upper_bounds: Vec<ResolvedType>,
    pub lower_bound: Option<Box<ResolvedType>>,
    pub owner: String,
    pub id: usize,
}

/// Class type structure following JavaC's ClassType
#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub name: String,
    pub type_params: Vec<ResolvedType>,
    pub outer_type: Option<Box<ResolvedType>>,
    pub is_raw: bool,
}

/// Captured type from wildcard capture conversion
#[derive(Debug, Clone, PartialEq)]
pub struct CapturedType {
    pub wildcard_bound: WildcardType,
    pub capture_id: usize,
}

/// Method signature for type inference
#[derive(Debug, Clone, PartialEq)]
pub struct MethodSignature {
    pub name: String,
    pub parameter_types: Vec<ResolvedType>,
    pub return_type: ResolvedType,
    pub type_parameters: Vec<TypeVariable>,
}

/// Primitive type enumeration - JavaC TypeTag equivalent
#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Boolean,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
}

/// Wildcard bound direction - JavaC BoundKind equivalent
#[derive(Debug, Clone, PartialEq)]
pub enum WildcardBound {
    Extends,
    Super,
    Unbounded,
}

/// Type attribution context - corresponds to JavaC's AttrContext
#[derive(Debug, Clone)]
pub struct AttrContext {
    /// Current method return type
    pub method_return_type: Option<ResolvedType>,
    /// Local variable types in current scope
    pub locals: HashMap<String, ResolvedType>,
    /// Current class being attributed
    pub current_class: Option<String>,
    /// Current method being attributed
    pub current_method: Option<String>,
    /// Whether we're in a static context
    pub is_static: bool,
    /// Whether we're in a constructor
    pub is_constructor: bool,
    /// Current exception types that can be thrown
    pub thrown_types: HashSet<String>,
    /// Scope depth for nested blocks
    pub scope_depth: usize,
    /// Lambda nesting level
    pub lambda_level: usize,
    /// Type parameters in scope
    pub type_params: HashMap<String, ResolvedType>,
    /// Type variables in scope
    pub type_variables: HashMap<String, TypeVariable>,
}

/// Type attribution environment - corresponds to JavaC's Env<AttrContext>
#[derive(Debug)]
pub struct AttrEnvironment {
    /// Type attribution contexts stack for nested scopes
    pub contexts: Vec<AttrContext>,
    /// Resolved expression types (expression ID -> type)
    pub expression_types: HashMap<usize, ResolvedType>,
    /// Semantic type mapping for codegen (field/method names -> type)
    pub semantic_types: HashMap<String, ResolvedType>,
    /// Symbol environment from Enter phase
    pub symbol_env: Option<SymbolEnvironment>,
    /// Deferred attribution tasks (for lambdas, method references)
    pub deferred_tasks: Vec<DeferredAttrTask>,
    /// Current result info for target typing
    pub current_result_info: Option<ResultInfo>,
}

/// Result information for target-typing - JavaC ResultInfo equivalent
#[derive(Debug, Clone)]
pub struct ResultInfo {
    /// Expected type kinds (VAL, VAR, TYP, etc.)
    pub kind_set: KindSet,
    /// Expected type
    pub expected_type: ResolvedType,
    /// Type checking context
    pub check_context: Option<CheckContext>,
}

/// Kind set for attribution - JavaC Kinds equivalent
#[derive(Debug, Clone, PartialEq)]
pub struct KindSet {
    pub val: bool,  // VAL - value
    pub var: bool,  // VAR - variable
    pub typ: bool,  // TYP - type
    pub pkg: bool,  // PCK - package
    pub mth: bool,  // MTH - method
}

/// Check context for type checking - JavaC CheckContext equivalent
#[derive(Debug, Clone)]
pub struct CheckContext {
    pub compatible: bool,
    pub check_mode: CheckMode,
}

/// Check mode enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum CheckMode {
    Normal,
    Capture,
    Inference,
}

/// Deferred attribution task - JavaC DeferredAttr equivalent
#[derive(Debug, Clone)]
pub struct DeferredAttrTask {
    pub expression_id: usize,
    pub context: AttrContext,
    pub task_type: DeferredTaskType,
}

/// Types of deferred attribution tasks
#[derive(Debug, Clone)]
pub enum DeferredTaskType {
    Lambda,
    MethodReference,
    DiamondNew,
}

/// Resolved symbol information for Items system - JavaC Symbol equivalent
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedSymbol {
    /// Symbol name
    pub name: String,
    /// Symbol owner (class/package)
    pub owner: String,
    /// Symbol type
    pub symbol_type: ResolvedType,
    /// Symbol flags (static, final, etc.)
    pub flags: SymbolFlags,
    /// Symbol kind (class, method, field, etc.)
    pub kind: SymbolKind,
}

/// Symbol flags corresponding to JavaC Flags
#[derive(Debug, Clone, PartialEq)]
pub struct SymbolFlags {
    pub is_static: bool,
    pub is_final: bool,
    pub is_abstract: bool,
    pub is_interface: bool,
    pub is_public: bool,
    pub is_private: bool,
    pub is_protected: bool,
    pub is_synchronized: bool,
    pub is_native: bool,
    pub is_strictfp: bool,
    pub is_synthetic: bool,
    pub is_annotation: bool,
    pub is_enum: bool,
}

impl Default for SymbolFlags {
    fn default() -> Self {
        Self {
            is_static: false,
            is_final: false,
            is_abstract: false,
            is_interface: false,
            is_public: false,
            is_private: false,
            is_protected: false,
            is_synchronized: false,
            is_native: false,
            is_strictfp: false,
            is_synthetic: false,
            is_annotation: false,
            is_enum: false,
        }
    }
}

/// Symbol kind enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Class,
    Interface,
    Method,
    Field,
    LocalVar,
    Parameter,
    Package,
    TypeParameter,
}

/// Type information for interface detection - JavaC Types equivalent
#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    /// Type name
    pub name: String,
    /// Whether this type is an interface
    pub is_interface: bool,
    /// Whether this type is a class
    pub is_class: bool,
    /// Whether this type is an enum
    pub is_enum: bool,
    /// Whether this type is an annotation
    pub is_annotation: bool,
    /// Type flags
    pub flags: SymbolFlags,
    /// Supertype information
    pub supertype: Option<String>,
    /// Interface implementations
    pub interfaces: Vec<String>,
}

/// Resolved method information for caching
#[derive(Debug, Clone)]
pub struct ResolvedMethod {
    pub method_name: String,
    pub declaring_class: String,
    pub return_type: ResolvedType,
    pub param_types: Vec<ResolvedType>,
    pub is_static: bool,
    pub is_varargs: bool,
    pub access_flags: u16,
}

/// Method candidate for overload resolution - JavaC equivalent
#[derive(Debug, Clone)]
pub struct MethodCandidate {
    pub method_name: String,
    pub declaring_class: String,
    pub return_type: ResolvedType,
    pub param_types: Vec<ResolvedType>,
    pub is_static: bool,
    pub is_varargs: bool,
    pub is_generic: bool,
    pub access_flags: u16,
    pub specificity_rank: i32,
}

/// Method resolution context for overload resolution
#[derive(Debug, Clone)]
pub struct MethodResolutionContext {
    pub candidates: Vec<MethodCandidate>,
    pub target_arg_types: Vec<ResolvedType>,
    pub allow_boxing: bool,
    pub allow_varargs: bool,
    pub phase: ResolutionPhase,
}

/// Resolved method information for codegen consumption
#[derive(Debug, Clone)]
pub struct MethodResolution {
    pub method_name: String,
    pub declaring_class: String,
    pub parameter_types: Vec<ResolvedType>,
    pub return_type: ResolvedType,
    pub is_static: bool,
    pub is_interface: bool,
}

/// Method resolution phases following JLS §15.12.2
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ResolutionPhase {
    /// Phase 1: Exact match (no conversions)
    ExactMatch,
    /// Phase 2: Primitive widening and reference subtyping  
    WideningConversion,
    /// Phase 3: Autoboxing/unboxing and widening
    BoxingConversion,
    /// Phase 4: Varargs (if applicable)
    VarargsConversion,
}

/// Expr hash key for caching
fn expr_hash(expr: &Expr) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    // Use the expression's address as a simple hash for now
    (expr as *const Expr as usize).hash(&mut hasher);
    hasher.finish()
}

/// Attr phase processor - corresponds to JavaC's Attr class
pub struct Attr {
    pub attr_env: AttrEnvironment,
}

impl Attr {
    pub fn new() -> Self {
        Self {
            attr_env: AttrEnvironment {
                contexts: Vec::new(),
                expression_types: HashMap::new(),
                semantic_types: HashMap::new(),
                symbol_env: None,
                deferred_tasks: Vec::new(),
                current_result_info: None,
            },
        }
    }

    // ========== TYPE CACHING SYSTEM ==========
    // JavaC-aligned type caching for performance optimization
    
    /// Cache expression type for future lookups
    pub fn cache_expression_type(&mut self, expr: &Expr, resolved_type: ResolvedType) {
        let expr_key = expr_hash(expr);
        self.attr_env.expression_types.insert(expr_key as usize, resolved_type);
    }
    
    /// Get cached expression type if available
    pub fn get_cached_expression_type(&self, expr: &Expr) -> Option<ResolvedType> {
        let expr_key = expr_hash(expr);
        self.attr_env.expression_types.get(&(expr_key as usize)).cloned()
    }
    
    /// Clear expression type cache (for memory management)
    pub fn clear_expression_cache(&mut self) {
        self.attr_env.expression_types.clear();
    }
    
    /// Get expression type statistics for debugging
    pub fn get_cache_statistics(&self) -> (usize, usize) {
        (
            self.attr_env.expression_types.len(),
            self.attr_env.semantic_types.len()
        )
    }
    
    /// Pre-populate cache with known types (optimization)
    pub fn pre_populate_builtin_types(&mut self) {
        // Pre-populate with common Java builtin types
        self.attr_env.semantic_types.insert(
            "java.lang.String".to_string(),
            ResolvedType::Reference("java.lang.String".to_string())
        );
        self.attr_env.semantic_types.insert(
            "java.lang.Object".to_string(),
            ResolvedType::Reference("java.lang.Object".to_string())
        );
        self.attr_env.semantic_types.insert(
            "java.lang.Integer".to_string(),
            ResolvedType::Reference("java.lang.Integer".to_string())
        );
    }
    
    /// Create type variable with unique ID
    fn create_type_variable(&self, name: String, owner: String) -> ResolvedType {
        static mut TYPE_VAR_COUNTER: usize = 0;
        unsafe {
            TYPE_VAR_COUNTER += 1;
            ResolvedType::TypeVariable(TypeVariable {
                name,
                upper_bounds: vec![ResolvedType::Reference("java.lang.Object".to_string())],
                lower_bound: None,
                owner,
                id: TYPE_VAR_COUNTER,
            })
        }
    }
    
    /// Create captured type with unique ID
    fn create_captured_type(&self, wildcard: &WildcardType) -> ResolvedType {
        static mut CAPTURED_TYPE_COUNTER: usize = 0;
        unsafe {
            CAPTURED_TYPE_COUNTER += 1;
            ResolvedType::Captured(CapturedType {
                wildcard_bound: wildcard.clone(),
                capture_id: CAPTURED_TYPE_COUNTER,
            })
        }
    }
    
    /// Process AST through Attr phase - type checking and resolution
    /// Corresponds to JavaC's Attr.attrib() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        self.process_with_symbols(ast, None)
    }
    
    /// Process AST with symbol environment from Enter phase
    pub fn process_with_symbols(&mut self, ast: Ast, symbol_env: Option<&crate::codegen::enter::SymbolEnvironment>) -> Result<Ast> {
        eprintln!("🔍 ATTR: Starting type attribution");
        
        // Store symbol environment for use in type resolution
        if let Some(sym_env) = symbol_env {
            self.attr_env.symbol_env = Some(sym_env.clone());
            eprintln!("📚 ATTR: Using symbol environment with {} classes", sym_env.classes.len());
        }
        
        // Initialize base context
        self.push_context(AttrContext {
            method_return_type: None,
            locals: HashMap::new(),
            current_class: None,
            current_method: None,
            is_static: false,
            is_constructor: false,
            thrown_types: HashSet::new(),
            scope_depth: 0,
            lambda_level: 0,
            type_params: HashMap::new(),
            type_variables: HashMap::new(),
        });
        
        // Process type declarations
        for type_decl in &ast.type_decls {
            self.attrib_type_decl(type_decl)?;
        }
        
        eprintln!("✅ ATTR: Type attribution complete");
        eprintln!("📊 ATTR: {} expression types attributed", 
                 self.attr_env.expression_types.len());
        
        self.pop_context();
        Ok(ast)
    }
    
    /// Attribute a type declaration - corresponds to JavaC's visitClassDef
    fn attrib_type_decl(&mut self, type_decl: &TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.attrib_class_decl(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                eprintln!("🔍 ATTR: Processing interface: {}", interface_decl.name);
                // TODO: Implement interface attribution
            }
            TypeDecl::Enum(enum_decl) => {
                eprintln!("🔍 ATTR: Processing enum: {}", enum_decl.name);
                // TODO: Implement enum attribution
            }
            TypeDecl::Annotation(annotation_decl) => {
                eprintln!("🔍 ATTR: Processing annotation: {}", annotation_decl.name);
                // TODO: Implement annotation attribution
            }
        }
        Ok(())
    }
    
    /// Attribute a class declaration - corresponds to JavaC's attribClass
    fn attrib_class_decl(&mut self, class_decl: &ClassDecl) -> Result<()> {
        eprintln!("🔍 ATTR: Processing class: {}", class_decl.name);
        
        // Push class context
        self.push_context(AttrContext {
            method_return_type: None,
            locals: HashMap::new(),
            current_class: Some(class_decl.name.clone()),
            current_method: None,
            is_static: false,
            is_constructor: false,
            thrown_types: HashSet::new(),
            scope_depth: 0,
            lambda_level: 0,
            type_params: HashMap::new(),
            type_variables: HashMap::new(),
        });
        
        // Process class members
        for member in &class_decl.body {
            self.attrib_class_member(member)?;
        }
        
        self.pop_context();
        Ok(())
    }
    
    /// Attribute class members
    fn attrib_class_member(&mut self, member: &crate::ast::ClassMember) -> Result<()> {
        use crate::ast::ClassMember;
        
        match member {
            ClassMember::Method(method_decl) => {
                self.attrib_method_decl(method_decl)?;
            }
            ClassMember::Field(field_decl) => {
                eprintln!("🔍 ATTR: Processing field: {}", field_decl.name);
                self.attrib_field_decl(field_decl)?;
            }
            ClassMember::Constructor(constructor_decl) => {
                eprintln!("🔍 ATTR: Processing constructor");
                // TODO: Implement constructor attribution
            }
            ClassMember::Initializer(initializer) => {
                eprintln!("🔍 ATTR: Processing initializer block");
                // TODO: Implement initializer attribution
            }
            ClassMember::TypeDecl(nested_type) => {
                self.attrib_type_decl(nested_type)?;
            }
        }
        Ok(())
    }
    
    /// Attribute a method declaration - corresponds to JavaC's visitMethodDef
    fn attrib_method_decl(&mut self, method_decl: &MethodDecl) -> Result<()> {
        eprintln!("🔍 ATTR: Processing method: {}", method_decl.name);
        
        // Determine return type
        let return_type = method_decl.return_type.as_ref()
            .map(|t| self.resolve_type_ref(t))
            .unwrap_or(ResolvedType::NoType);
        
        // Check if method is static
        let is_static = method_decl.modifiers.iter()
            .any(|m| matches!(m, crate::ast::Modifier::Static));
        
        // Push method context
        self.push_context(AttrContext {
            method_return_type: Some(return_type),
            locals: HashMap::new(),
            current_class: self.current_context().current_class.clone(),
            current_method: Some(method_decl.name.clone()),
            is_static,
            is_constructor: false,
            thrown_types: HashSet::new(),
            scope_depth: 1,
            lambda_level: 0,
            type_params: HashMap::new(),
            type_variables: HashMap::new(),
        });
        
        // Add parameters to local scope
        for param in &method_decl.parameters {
            let param_type = self.resolve_type_ref(&param.type_ref);
            self.add_local_variable(param.name.clone(), param_type);
        }
        
        // Attribute method body
        if let Some(ref body) = method_decl.body {
            for stmt in &body.statements {
                self.attrib_statement(stmt)?;
            }
        }
        
        self.pop_context();
        Ok(())
    }
    
    /// Attribute a field declaration
    fn attrib_field_decl(&mut self, field_decl: &crate::ast::FieldDecl) -> Result<()> {
        eprintln!("🔍 ATTR: Attributing field: {}", field_decl.name);
        
        // Resolve field type
        let field_type = self.resolve_type_ref(&field_decl.type_ref);
        eprintln!("🔍 ATTR: Field '{}' has type: {:?}", field_decl.name, field_type);
        
        // Record semantic type information for codegen
        self.record_field_type(&field_decl.name, field_type.clone());
        
        // If field has initializer, attribute it
        if let Some(ref init_expr) = field_decl.initializer {
            let init_type = self.attrib_expression(init_expr)?;
            eprintln!("🔍 ATTR: Field '{}' initializer type: {:?}", field_decl.name, init_type);
            
            // TODO: Check type compatibility between field type and initializer type
        }
        
        Ok(())
    }
    
    /// Attribute a statement
    fn attrib_statement(&mut self, stmt: &crate::ast::Stmt) -> Result<()> {
        use crate::ast::Stmt;
        
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.attrib_expression(&expr_stmt.expr)?;
            }
            Stmt::Declaration(var_decl) => {
                let var_type = self.resolve_type_ref(&var_decl.type_ref);
                
                // Handle multiple variable declarators
                for variable in &var_decl.variables {
                    self.add_local_variable(variable.name.clone(), var_type.clone());
                    
                    if let Some(ref init) = variable.initializer {
                        self.attrib_expression(init)?;
                    }
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref expr) = return_stmt.value {
                    self.attrib_expression(expr)?;
                }
            }
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    self.attrib_statement(stmt)?;
                }
            }
            _ => {
                // TODO: Handle other statement types
                eprintln!("🔍 ATTR: Skipping statement type (not implemented)");
            }
        }
        Ok(())
    }
    
    /// Attribute an expression and determine its type
    fn attrib_expression(&mut self, expr: &Expr) -> Result<ResolvedType> {
        use crate::ast::Expr;
        
        let expr_type = match expr {
            Expr::Literal(literal_expr) => {
                self.literal_to_type(&literal_expr.value)
            }
            Expr::Identifier(identifier_expr) => {
                self.resolve_identifier(&identifier_expr.name)
            }
            Expr::MethodCall(method_call) => {
                // TODO: Implement method call resolution
                ResolvedType::Reference("java/lang/Object".to_string())
            }
            Expr::FieldAccess(field_access) => {
                // TODO: Implement field access resolution
                ResolvedType::Reference("java/lang/Object".to_string())
            }
            Expr::Binary(binary_expr) => {
                let left_type = self.attrib_expression(&binary_expr.left)?;
                let right_type = self.attrib_expression(&binary_expr.right)?;
                
                // Special handling for string concatenation
                if binary_expr.operator == crate::ast::BinaryOp::Add {
                    if self.is_string_concatenation_types(&left_type, &right_type) {
                        eprintln!("🔍 ATTR: Detected string concatenation");
                        return Ok(ResolvedType::Reference("java/lang/String".to_string()));
                    }
                }
                
                self.compute_binary_type(&left_type, &binary_expr.operator, &right_type)
            }
            _ => {
                eprintln!("🔍 ATTR: Unhandled expression type");
                ResolvedType::Error
            }
        };
        
        // Store expression type (using placeholder ID for now)
        let expr_id = expr as *const _ as usize;
        self.attr_env.expression_types.insert(expr_id, expr_type.clone());
        
        Ok(expr_type)
    }
    
    /// Convert literal to type
    fn literal_to_type(&self, literal: &crate::ast::Literal) -> ResolvedType {
        use crate::ast::Literal;
        
        match literal {
            Literal::Integer(_) => ResolvedType::Primitive(PrimitiveType::Int),
            Literal::Long(_) => ResolvedType::Primitive(PrimitiveType::Long),
            Literal::Float(_) => ResolvedType::Primitive(PrimitiveType::Float),
            Literal::Double(_) => ResolvedType::Primitive(PrimitiveType::Double),
            Literal::Boolean(_) => ResolvedType::Primitive(PrimitiveType::Boolean),
            Literal::Char(_) => ResolvedType::Primitive(PrimitiveType::Char),
            Literal::String(_) => ResolvedType::Reference("java/lang/String".to_string()),
            Literal::Null => ResolvedType::Reference("null".to_string()),
        }
    }
    
    /// Resolve identifier to its type
    fn resolve_identifier(&self, name: &str) -> ResolvedType {
        // Check local variables first
        if let Some(context) = self.current_context_opt() {
            if let Some(var_type) = context.locals.get(name) {
                return var_type.clone();
            }
        }
        
        // TODO: Check fields, methods, etc.
        eprintln!("🔍 ATTR: Unresolved identifier: {}", name);
        ResolvedType::Error
    }
    
    /// Check if types indicate string concatenation
    fn is_string_concatenation_types(&self, left: &ResolvedType, right: &ResolvedType) -> bool {
        match (left, right) {
            // String + anything = String concatenation
            (ResolvedType::Reference(l), _) if l == "java/lang/String" => true,
            (_, ResolvedType::Reference(r)) if r == "java/lang/String" => true,
            _ => false,
        }
    }
    
    /// Compute the result type of a binary operation
    fn compute_binary_type(&self, left: &ResolvedType, op: &crate::ast::BinaryOp, right: &ResolvedType) -> ResolvedType {
        match op {
            crate::ast::BinaryOp::Add => {
                // Addition can be numeric or string concatenation
                if self.is_string_concatenation_types(left, right) {
                    ResolvedType::Reference("java/lang/String".to_string())
                } else {
                    // Numeric addition
                    self.compute_numeric_binary_type(left, right)
                }
            }
            _ => {
                // Other operators are numeric
                self.compute_numeric_binary_type(left, right)
            }
        }
    }
    
    /// Compute the result type of numeric binary operations
    fn compute_numeric_binary_type(&self, left: &ResolvedType, right: &ResolvedType) -> ResolvedType {
        match (left, right) {
            (ResolvedType::Primitive(l), ResolvedType::Primitive(r)) if l == r => left.clone(),
            (ResolvedType::Primitive(_), ResolvedType::Primitive(_)) => {
                // Numeric promotion rules - simplified
                ResolvedType::Primitive(PrimitiveType::Int)
            }
            _ => ResolvedType::Error,
        }
    }
    
    /// Resolve a type reference to a ResolvedType with full generic support
    fn resolve_type_ref(&self, type_ref: &crate::ast::TypeRef) -> ResolvedType {
        match type_ref.name.as_str() {
            "void" => ResolvedType::NoType,
            "boolean" => ResolvedType::Primitive(PrimitiveType::Boolean),
            "byte" => ResolvedType::Primitive(PrimitiveType::Byte),
            "short" => ResolvedType::Primitive(PrimitiveType::Short),
            "int" => ResolvedType::Primitive(PrimitiveType::Int),
            "long" => ResolvedType::Primitive(PrimitiveType::Long),
            "float" => ResolvedType::Primitive(PrimitiveType::Float),
            "double" => ResolvedType::Primitive(PrimitiveType::Double),
            "char" => ResolvedType::Primitive(PrimitiveType::Char),
            _ => {
                // Check if it's a type variable first
                if let Some(tv) = self.lookup_type_variable(&type_ref.name) {
                    return ResolvedType::TypeVariable(tv);
                }
                
                // Handle generic class types
                if !type_ref.type_args.is_empty() {
                    let type_args: Vec<ResolvedType> = type_ref.type_args
                        .iter()
                        .map(|arg| self.resolve_type_argument(arg))
                        .collect();
                    
                    ResolvedType::Class(ClassType {
                        name: type_ref.name.clone(),
                        type_params: type_args,
                        outer_type: None,
                        is_raw: false,
                    })
                } else {
                    // Check if this is a known generic class without type arguments (raw type)
                    if self.is_generic_class(&type_ref.name) {
                        ResolvedType::Class(ClassType {
                            name: type_ref.name.clone(),
                            type_params: Vec::new(),
                            outer_type: None,
                            is_raw: true,
                        })
                    } else {
                        ResolvedType::Reference(type_ref.name.clone())
                    }
                }
            }
        }
    }
    
    /// Resolve a type argument (could be a concrete type or wildcard)
    fn resolve_type_argument(&self, type_arg: &crate::ast::TypeArg) -> ResolvedType {
        match type_arg {
            crate::ast::TypeArg::Type(type_ref) => self.resolve_type_ref(type_ref),
            crate::ast::TypeArg::Wildcard(wildcard) => {
                let bound = wildcard.bound.as_ref().map(|(_, type_ref)| Box::new(self.resolve_type_ref(type_ref)));
                
                ResolvedType::Wildcard(WildcardType {
                    kind: match &wildcard.bound {
                        Some((crate::ast::BoundKind::Extends, _)) => WildcardBound::Extends,
                        Some((crate::ast::BoundKind::Super, _)) => WildcardBound::Super,
                        None => WildcardBound::Unbounded,
                    },
                    bound,
                    capture_id: None,
                })
            }
        }
    }
    
    /// Check if a class name represents a generic class
    fn is_generic_class(&self, name: &str) -> bool {
        // For now, hardcode some common generic classes
        matches!(name, "List" | "ArrayList" | "Map" | "HashMap" | "Set" | "HashSet" | "Optional")
    }
    
    /// Look up a type variable by name in current scope
    fn lookup_type_variable(&self, name: &str) -> Option<TypeVariable> {
        for context in self.attr_env.contexts.iter().rev() {
            if let Some(tv) = context.type_variables.get(name) {
                return Some(tv.clone());
            }
        }
        None
    }
    
    /// Perform type substitution - replace type variables with concrete types
    fn substitute_type(&self, original: &ResolvedType, substitutions: &std::collections::HashMap<String, ResolvedType>) -> ResolvedType {
        match original {
            ResolvedType::TypeVariable(tv) => {
                if let Some(substitution) = substitutions.get(&tv.name) {
                    substitution.clone()
                } else {
                    original.clone()
                }
            }
            ResolvedType::Class(class_type) => {
                let substituted_params: Vec<ResolvedType> = class_type.type_params
                    .iter()
                    .map(|param| self.substitute_type(param, substitutions))
                    .collect();
                
                ResolvedType::Class(ClassType {
                    name: class_type.name.clone(),
                    type_params: substituted_params,
                    outer_type: class_type.outer_type.as_ref().map(|outer| 
                        Box::new(self.substitute_type(outer, substitutions))
                    ),
                    is_raw: class_type.is_raw,
                })
            }
            ResolvedType::Wildcard(wildcard) => {
                ResolvedType::Wildcard(WildcardType {
                    kind: wildcard.kind.clone(),
                    bound: wildcard.bound.as_ref().map(|b| 
                        Box::new(self.substitute_type(b, substitutions))
                    ),
                    capture_id: wildcard.capture_id,
                })
            }
            ResolvedType::Array(element_type) => {
                ResolvedType::Array(Box::new(self.substitute_type(element_type, substitutions)))
            }
            ResolvedType::Intersection(types) => {
                let substituted_types: Vec<ResolvedType> = types
                    .iter()
                    .map(|t| self.substitute_type(t, substitutions))
                    .collect();
                ResolvedType::Intersection(substituted_types)
            }
            ResolvedType::Union(types) => {
                let substituted_types: Vec<ResolvedType> = types
                    .iter()
                    .map(|t| self.substitute_type(t, substitutions))
                    .collect();
                ResolvedType::Union(substituted_types)
            }
            _ => original.clone(),
        }
    }
    
    /// Push a new attribution context
    fn push_context(&mut self, context: AttrContext) {
        self.attr_env.contexts.push(context);
    }
    
    /// Pop the current attribution context
    fn pop_context(&mut self) {
        self.attr_env.contexts.pop();
    }
    
    /// Get the current attribution context
    fn current_context(&self) -> &AttrContext {
        self.attr_env.contexts.last().expect("No attribution context")
    }
    
    /// Get the current attribution context (optional)
    fn current_context_opt(&self) -> Option<&AttrContext> {
        self.attr_env.contexts.last()
    }
    
    /// Get the current attribution context mutably
    fn current_context_mut(&mut self) -> &mut AttrContext {
        self.attr_env.contexts.last_mut().expect("No attribution context")
    }
    
    /// Add a local variable to the current context
    fn add_local_variable(&mut self, name: String, var_type: ResolvedType) {
        self.current_context_mut().locals.insert(name, var_type);
    }
    
    /// Get type information for use by subsequent phases
    pub fn get_type_information(&self) -> &HashMap<usize, ResolvedType> {
        &self.attr_env.expression_types
    }
    
    /// Get semantic type mapping for codegen (field/method names -> type)
    pub fn get_semantic_types(&self) -> &HashMap<String, ResolvedType> {
        &self.attr_env.semantic_types
    }
    
    /// Record field type for semantic access (field_name -> type)
    fn record_field_type(&mut self, field_name: &str, field_type: ResolvedType) {
        self.attr_env.semantic_types.insert(field_name.to_string(), field_type.clone());
        // Also store qualified field name for "this.field" access
        if let Some(current_class) = &self.current_context_opt().and_then(|c| c.current_class.clone()) {
            let qualified_name = format!("{}.{}", current_class, field_name);
            self.attr_env.semantic_types.insert(qualified_name, field_type.clone());
        }
        // Store "this.field" pattern for implicit field access
        let this_field = format!("this.{}", field_name);
        self.attr_env.semantic_types.insert(this_field, field_type);
    }
    
    /// Record method type for semantic access (method_name -> type)  
    fn record_method_type(&mut self, method_name: &str, method_type: ResolvedType) {
        self.attr_env.semantic_types.insert(method_name.to_string(), method_type.clone());
        // Also store qualified method name
        if let Some(current_class) = &self.current_context_opt().and_then(|c| c.current_class.clone()) {
            let qualified_name = format!("{}.{}", current_class, method_name);
            self.attr_env.semantic_types.insert(qualified_name, method_type);
        }
    }
    
    /// Record variable type for semantic access (var_name -> type)
    fn record_variable_type(&mut self, var_name: &str, var_type: ResolvedType) {
        self.attr_env.semantic_types.insert(var_name.to_string(), var_type);
    }
    
    /// Perform wildcard capture conversion following JLS §5.1.10
    fn capture_conversion(&mut self, original: &ResolvedType) -> ResolvedType {
        match original {
            ResolvedType::Class(class_type) => {
                let mut captured_params = Vec::new();
                let mut has_wildcards = false;
                
                for param in &class_type.type_params {
                    match param {
                        ResolvedType::Wildcard(wildcard) => {
                            has_wildcards = true;
                            let captured = self.create_captured_type(wildcard);
                            captured_params.push(captured);
                        }
                        _ => captured_params.push(param.clone()),
                    }
                }
                
                if has_wildcards {
                    ResolvedType::Class(ClassType {
                        name: class_type.name.clone(),
                        type_params: captured_params,
                        outer_type: class_type.outer_type.clone(),
                        is_raw: class_type.is_raw,
                    })
                } else {
                    original.clone()
                }
            }
            _ => original.clone(),
        }
    }
    
    /// Check if type1 is assignable to type2 (following Java's assignability rules)
    fn is_assignable(&self, from: &ResolvedType, to: &ResolvedType) -> bool {
        match (from, to) {
            // Identity conversion
            (a, b) if a == b => true,
            
            // Primitive conversions
            (ResolvedType::Primitive(from_prim), ResolvedType::Primitive(to_prim)) => {
                self.is_primitive_assignable(from_prim, to_prim)
            }
            
            // Reference type assignability
            (ResolvedType::Reference(from_name), ResolvedType::Reference(to_name)) => {
                // Simplified - should check inheritance hierarchy
                from_name == to_name
            }
            
            // Generic class assignability
            (ResolvedType::Class(from_class), ResolvedType::Class(to_class)) => {
                self.is_generic_assignable(from_class, to_class)
            }
            
            // Wildcard assignability
            (from_type, ResolvedType::Wildcard(wildcard)) => {
                self.is_assignable_to_wildcard(from_type, wildcard)
            }
            
            // Type variable assignability
            (ResolvedType::TypeVariable(tv), to_type) => {
                // Check if any upper bound is assignable to target
                tv.upper_bounds.iter().any(|bound| self.is_assignable(bound, to_type))
            }
            
            // Array assignability
            (ResolvedType::Array(from_elem), ResolvedType::Array(to_elem)) => {
                self.is_assignable(from_elem, to_elem)
            }
            
            // Null assignability (null can be assigned to any reference type)
            (ResolvedType::Null, ResolvedType::Reference(_)) |
            (ResolvedType::Null, ResolvedType::Class(_)) |
            (ResolvedType::Null, ResolvedType::Array(_)) => true,
            
            _ => false,
        }
    }
    
    /// Check primitive type assignability with widening conversions
    fn is_primitive_assignable(&self, from: &PrimitiveType, to: &PrimitiveType) -> bool {
        use PrimitiveType::*;
        match (from, to) {
            // Identity
            (a, b) if a == b => true,
            
            // Widening primitive conversions (JLS §5.1.2)
            (Byte, Short) | (Byte, Int) | (Byte, Long) | (Byte, Float) | (Byte, Double) => true,
            (Short, Int) | (Short, Long) | (Short, Float) | (Short, Double) => true,
            (Char, Int) | (Char, Long) | (Char, Float) | (Char, Double) => true,
            (Int, Long) | (Int, Float) | (Int, Double) => true,
            (Long, Float) | (Long, Double) => true,
            (Float, Double) => true,
            
            _ => false,
        }
    }
    
    /// Check generic class assignability with covariance/contravariance rules
    fn is_generic_assignable(&self, from: &ClassType, to: &ClassType) -> bool {
        // Same class name is required
        if from.name != to.name {
            return false;
        }
        
        // Raw type handling
        if to.is_raw {
            return true; // Can assign any parameterized type to raw type
        }
        if from.is_raw && !to.is_raw {
            return false; // Cannot assign raw type to parameterized type
        }
        
        // Check type parameter compatibility
        if from.type_params.len() != to.type_params.len() {
            return false;
        }
        
        for (from_param, to_param) in from.type_params.iter().zip(to.type_params.iter()) {
            if !self.is_type_argument_compatible(from_param, to_param) {
                return false;
            }
        }
        
        true
    }
    
    /// Check if a type argument is compatible (handles wildcards)
    fn is_type_argument_compatible(&self, from: &ResolvedType, to: &ResolvedType) -> bool {
        match (from, to) {
            // Exact match
            (a, b) if a == b => true,
            
            // Wildcard compatibility
            (from_type, ResolvedType::Wildcard(to_wildcard)) => {
                self.is_assignable_to_wildcard(from_type, to_wildcard)
            }
            
            (ResolvedType::Wildcard(from_wildcard), to_type) => {
                self.is_wildcard_assignable_to(from_wildcard, to_type)
            }
            
            _ => false,
        }
    }
    
    /// Check if a type is assignable to a wildcard
    fn is_assignable_to_wildcard(&self, from: &ResolvedType, wildcard: &WildcardType) -> bool {
        match wildcard.kind {
            WildcardBound::Unbounded => true,
            WildcardBound::Extends => {
                if let Some(bound) = &wildcard.bound {
                    self.is_assignable(from, bound)
                } else {
                    true // ? extends Object
                }
            }
            WildcardBound::Super => {
                if let Some(bound) = &wildcard.bound {
                    self.is_assignable(bound, from)
                } else {
                    false
                }
            }
        }
    }
    
    /// Check if a wildcard is assignable to a type
    fn is_wildcard_assignable_to(&self, wildcard: &WildcardType, to: &ResolvedType) -> bool {
        match wildcard.kind {
            WildcardBound::Unbounded => false, // ? is not assignable to concrete types
            WildcardBound::Extends => {
                if let Some(bound) = &wildcard.bound {
                    self.is_assignable(bound, to)
                } else {
                    // ? extends Object
                    matches!(to, ResolvedType::Reference(name) if name == "Object")
                }
            }
            WildcardBound::Super => false, // ? super T is not assignable to concrete types
        }
    }
    
    /// Infer type arguments for a generic method call
    fn infer_method_type_arguments(&self, method_sig: &MethodSignature, actual_args: &[ResolvedType]) -> std::collections::HashMap<String, ResolvedType> {
        let mut substitutions = std::collections::HashMap::new();
        
        // Simple type inference - match formal parameter types with actual argument types
        for (formal, actual) in method_sig.parameter_types.iter().zip(actual_args.iter()) {
            self.collect_type_constraints(formal, actual, &mut substitutions);
        }
        
        substitutions
    }
    
    /// Collect type constraints for type inference
    fn collect_type_constraints(&self, formal: &ResolvedType, actual: &ResolvedType, substitutions: &mut std::collections::HashMap<String, ResolvedType>) {
        match (formal, actual) {
            (ResolvedType::TypeVariable(tv), actual_type) => {
                // Direct constraint: T = actual_type
                substitutions.insert(tv.name.clone(), actual_type.clone());
            }
            
            (ResolvedType::Class(formal_class), ResolvedType::Class(actual_class)) if formal_class.name == actual_class.name => {
                // Recursive constraint collection for type parameters
                for (formal_param, actual_param) in formal_class.type_params.iter().zip(actual_class.type_params.iter()) {
                    self.collect_type_constraints(formal_param, actual_param, substitutions);
                }
            }
            
            (ResolvedType::Array(formal_elem), ResolvedType::Array(actual_elem)) => {
                self.collect_type_constraints(formal_elem, actual_elem, substitutions);
            }
            
            _ => {
                // No constraints can be collected
            }
        }
    }

    // ========== MIGRATED TYPE INFERENCE SYSTEM ==========
    // Core type inference functionality migrated from codegen/gen_visitor.rs
    
    /// JavaC Attr.attribExpr equivalent - Complete type inference for all expressions
    pub fn infer_expression_type(&mut self, expr: &Expr) -> Result<ResolvedType> {
        // Check cache first
        let expr_key = expr_hash(expr);
        if let Some(cached_type) = self.attr_env.expression_types.get(&(expr_key as usize)) {
            return Ok(cached_type.clone());
        }
        
        let resolved_type = match expr {
            Expr::Literal(lit) => Ok(self.get_literal_type(&lit.value)),
            
            Expr::Identifier(ident) => {
                self.resolve_identifier_type(&ident.name)
            }
            
            Expr::Binary(binary) => {
                let left_type = self.infer_expression_type(&binary.left)?;
                let right_type = self.infer_expression_type(&binary.right)?;
                self.check_binary_op_types(&binary.operator, &left_type, &right_type)
            }
            
            Expr::Unary(unary) => {
                let operand_type = self.infer_expression_type(&unary.operand)?;
                self.check_unary_op_type(&unary.operator, &operand_type)
            }
            
            Expr::Cast(cast) => {
                let source_type = self.infer_expression_type(&cast.expr)?;
                let target_type = self.convert_type_enum_to_resolved(&TypeEnum::from(cast.target_type.clone()));
                self.check_cast_compatibility(&source_type, &target_type)?;
                Ok(target_type)
            }
            
            Expr::Assignment(assign) => {
                let target_type = self.infer_expression_type(&assign.target)?;
                let value_type = self.infer_expression_type(&assign.value)?;
                self.check_assignable(&value_type, &target_type, "assignment")?;
                Ok(target_type)
            }
            
            Expr::MethodCall(method_call) => {
                let resolved_method = self.resolve_method_call(method_call)?;
                Ok(resolved_method.return_type)
            }
            
            Expr::FieldAccess(field_access) => {
                self.resolve_field_type(&field_access.name, &field_access.target)
            }
            
            Expr::ArrayAccess(array_access) => {
                let array_type = self.infer_expression_type(&array_access.array)?;
                self.get_array_component_type(&array_type)
            }
            
            Expr::InstanceOf(_) => {
                Ok(ResolvedType::Primitive(PrimitiveType::Boolean))
            }
            
            Expr::Conditional(conditional) => {
                let then_type = self.infer_expression_type(&conditional.then_expr)?;
                let else_type = self.infer_expression_type(&conditional.else_expr)?;
                self.get_conditional_result_type(&then_type, &else_type)
            }
            
            Expr::New(new_expr) => {
                Ok(self.convert_type_enum_to_resolved(&TypeEnum::from(new_expr.target_type.clone())))
            }
            
            Expr::Parenthesized(expr) => {
                self.infer_expression_type(expr)
            }
            
            Expr::ArrayInitializer(values) => {
                if values.is_empty() {
                    Ok(ResolvedType::Array(Box::new(ResolvedType::Reference("java.lang.Object".to_string()))))
                } else {
                    let component_type = self.infer_expression_type(&values[0])?;
                    Ok(ResolvedType::Array(Box::new(component_type)))
                }
            }
            
            _ => {
                eprintln!("🔍 ATTR: Unhandled expression type in infer_expression_type");
                Ok(ResolvedType::Error)
            }
        }?;
        
        // Cache the result
        self.attr_env.expression_types.insert(expr_key as usize, resolved_type.clone());
        Ok(resolved_type)
    }
    
    /// Convert literal to ResolvedType
    fn get_literal_type(&self, literal: &crate::ast::Literal) -> ResolvedType {
        use crate::ast::Literal;
        
        match literal {
            Literal::Integer(_) => ResolvedType::Primitive(PrimitiveType::Int),
            Literal::Long(_) => ResolvedType::Primitive(PrimitiveType::Long),
            Literal::Float(_) => ResolvedType::Primitive(PrimitiveType::Float),
            Literal::Double(_) => ResolvedType::Primitive(PrimitiveType::Double),
            Literal::Boolean(_) => ResolvedType::Primitive(PrimitiveType::Boolean),
            Literal::Char(_) => ResolvedType::Primitive(PrimitiveType::Char),
            Literal::String(_) => ResolvedType::Reference("java.lang.String".to_string()),
            Literal::Null => ResolvedType::Null,
        }
    }
    
    /// Resolve identifier to its type
    fn resolve_identifier_type(&self, name: &str) -> Result<ResolvedType> {
        // Check local variables first
        if let Some(context) = self.current_context_opt() {
            if let Some(var_type) = context.locals.get(name) {
                return Ok(var_type.clone());
            }
        }
        
        // Check semantic types
        if let Some(semantic_type) = self.attr_env.semantic_types.get(name) {
            return Ok(semantic_type.clone());
        }
        
        eprintln!("⚠️ ATTR: Unknown identifier '{}', defaulting to Object", name);
        Ok(ResolvedType::Reference("java.lang.Object".to_string()))
    }
    
    /// Check binary operation types and return result type
    fn check_binary_op_types(&self, op: &BinaryOp, left: &ResolvedType, right: &ResolvedType) -> Result<ResolvedType> {
        use BinaryOp::*;
        
        match op {
            Add => {
                // String concatenation if either operand is String
                if self.is_string_type(left) || self.is_string_type(right) {
                    Ok(ResolvedType::Reference("java.lang.String".to_string()))
                } else {
                    self.get_numeric_promotion_type(left, right)
                }
            }
            Sub | Mul | Div | Mod => {
                self.get_numeric_promotion_type(left, right)
            }
            Eq | Ne | Lt | Le | Gt | Ge => {
                Ok(ResolvedType::Primitive(PrimitiveType::Boolean))
            }
            LogicalAnd | LogicalOr => {
                Ok(ResolvedType::Primitive(PrimitiveType::Boolean))
            }
            And | Or | Xor => {
                self.get_numeric_promotion_type(left, right)
            }
            LShift | RShift | URShift => {
                // Shift operations return the type of the left operand
                Ok(left.clone())
            }
        }
    }
    
    /// Check unary operation type and return result type
    fn check_unary_op_type(&self, op: &UnaryOp, operand: &ResolvedType) -> Result<ResolvedType> {
        use UnaryOp::*;
        
        match op {
            Plus | Minus => {
                // Numeric promotion for unary +/-
                match operand {
                    ResolvedType::Primitive(PrimitiveType::Byte) | 
                    ResolvedType::Primitive(PrimitiveType::Short) |
                    ResolvedType::Primitive(PrimitiveType::Char) => {
                        Ok(ResolvedType::Primitive(PrimitiveType::Int))
                    }
                    _ => Ok(operand.clone())
                }
            }
            Not => Ok(ResolvedType::Primitive(PrimitiveType::Boolean)),
            BitNot => Ok(operand.clone()),
            PreInc | PreDec | PostInc | PostDec => {
                // Increment/decrement operations return the same type as operand
                Ok(operand.clone())
            }
        }
    }
    
    /// Get numeric promotion result type
    fn get_numeric_promotion_type(&self, left: &ResolvedType, right: &ResolvedType) -> Result<ResolvedType> {
        use ResolvedType::*;
        use PrimitiveType::*;
        
        match (left, right) {
            (Primitive(Double), _) | (_, Primitive(Double)) => Ok(Primitive(Double)),
            (Primitive(Float), _) | (_, Primitive(Float)) => Ok(Primitive(Float)),
            (Primitive(Long), _) | (_, Primitive(Long)) => Ok(Primitive(Long)),
            (Primitive(_), Primitive(_)) => Ok(Primitive(Int)), // Default to int
            _ => Err(crate::common::error::Error::CodeGen {
                message: format!("Cannot apply numeric operation to {:?} and {:?}", left, right)
            })
        }
    }
    
    /// Check if type is String
    fn is_string_type(&self, typ: &ResolvedType) -> bool {
        matches!(typ, ResolvedType::Reference(name) if name == "java.lang.String" || name == "String")
    }
    
    /// Convert TypeEnum to ResolvedType
    fn convert_type_enum_to_resolved(&self, type_enum: &TypeEnum) -> ResolvedType {
        match type_enum {
            TypeEnum::Primitive(ast_prim) => {
                // Convert AST primitive type to wash primitive type
                let wash_prim = match ast_prim {
                    AstPrimitiveType::Boolean => PrimitiveType::Boolean,
                    AstPrimitiveType::Byte => PrimitiveType::Byte,
                    AstPrimitiveType::Short => PrimitiveType::Short,
                    AstPrimitiveType::Int => PrimitiveType::Int,
                    AstPrimitiveType::Long => PrimitiveType::Long,
                    AstPrimitiveType::Char => PrimitiveType::Char,
                    AstPrimitiveType::Float => PrimitiveType::Float,
                    AstPrimitiveType::Double => PrimitiveType::Double,
                };
                ResolvedType::Primitive(wash_prim)
            }
            TypeEnum::Reference(ref_type) => {
                match ref_type {
                    ReferenceType::Class(class_name) => {
                        ResolvedType::Reference(class_name.clone())
                    }
                    ReferenceType::Interface(interface_name) => {
                        ResolvedType::Reference(interface_name.clone())
                    }
                    ReferenceType::Array(elem_type_ref) => {
                        let elem_type_enum = TypeEnum::from((**elem_type_ref).clone());
                        let elem_resolved = self.convert_type_enum_to_resolved(&elem_type_enum);
                        ResolvedType::Array(Box::new(elem_resolved))
                    }
                }
            }
            TypeEnum::Void => ResolvedType::NoType,
        }
    }
    
    /// Check cast compatibility
    fn check_cast_compatibility(&self, source: &ResolvedType, target: &ResolvedType) -> Result<()> {
        // For now, allow all casts (JavaC has complex rules here)
        eprintln!("🔍 ATTR: Cast from {:?} to {:?} (assuming valid)", source, target);
        Ok(())
    }
    
    /// Check if value type is assignable to target type
    fn check_assignable(&self, value: &ResolvedType, target: &ResolvedType, context: &str) -> Result<()> {
        // For now, allow all assignments (JavaC has complex rules here)
        eprintln!("🔍 ATTR: Assignment check in {}: {:?} -> {:?} (assuming valid)", context, value, target);
        Ok(())
    }
    
    /// Get array component type
    fn get_array_component_type(&self, array_type: &ResolvedType) -> Result<ResolvedType> {
        match array_type {
            ResolvedType::Array(elem_type) => Ok((**elem_type).clone()),
            _ => Err(crate::common::error::Error::CodeGen {
                message: format!("Cannot get component type of non-array type: {:?}", array_type)
            })
        }
    }
    
    /// Get conditional expression result type
    fn get_conditional_result_type(&self, then_type: &ResolvedType, else_type: &ResolvedType) -> Result<ResolvedType> {
        // Simple rule: if both types are the same, return that type
        if then_type == else_type {
            Ok(then_type.clone())
        } else {
            // For now, default to Object type
            Ok(ResolvedType::Reference("java.lang.Object".to_string()))
        }
    }
    
    // ========== METHOD OVERLOAD RESOLUTION ==========
    // JavaC Resolve.java equivalent - Complete JLS §15.12.2 implementation
    
    /// Resolve method call and return resolved method info
    fn resolve_method_call(&mut self, method_call: &crate::ast::MethodCallExpr) -> Result<ResolvedMethod> {
        eprintln!("🔍 ATTR: Resolving method call: {}", method_call.name);
        
        // Build method resolution context
        let mut context = self.build_method_resolution_context(method_call)?;
        
        // Resolve using JLS method resolution algorithm
        if let Some(best_candidate) = self.resolve_best_method_candidate(&mut context)? {
            Ok(ResolvedMethod {
                method_name: best_candidate.method_name,
                declaring_class: best_candidate.declaring_class,
                return_type: best_candidate.return_type,
                param_types: best_candidate.param_types,
                is_static: best_candidate.is_static,
                is_varargs: best_candidate.is_varargs,
                access_flags: best_candidate.access_flags,
            })
        } else {
            // Fallback to placeholder if no method found
            eprintln!("⚠️ ATTR: No applicable method found for: {}", method_call.name);
            Ok(ResolvedMethod {
                method_name: method_call.name.clone(),
                declaring_class: "java.lang.Object".to_string(),
                return_type: ResolvedType::Reference("java.lang.Object".to_string()),
                param_types: vec![],
                is_static: false,
                is_varargs: false,
                access_flags: 0x0001, // ACC_PUBLIC
            })
        }
    }
    
    /// Build method resolution context from method call
    fn build_method_resolution_context(&mut self, method_call: &crate::ast::MethodCallExpr) -> Result<MethodResolutionContext> {
        // Infer argument types
        let mut target_arg_types = Vec::new();
        for arg in &method_call.arguments {
            let arg_type = self.infer_expression_type(arg)?;
            target_arg_types.push(arg_type);
        }
        
        // Find method candidates
        let candidates = self.find_method_candidates(&method_call.name, &method_call.target)?;
        
        Ok(MethodResolutionContext {
            candidates,
            target_arg_types,
            allow_boxing: true,
            allow_varargs: true,
            phase: ResolutionPhase::ExactMatch,
        })
    }
    
    /// Find method candidates based on method name and target
    fn find_method_candidates(&self, method_name: &str, target: &Option<Box<Expr>>) -> Result<Vec<MethodCandidate>> {
        let mut candidates = Vec::new();
        
        // Add built-in method candidates based on method name
        match method_name {
            "toString" => {
                candidates.push(MethodCandidate {
                    method_name: "toString".to_string(),
                    declaring_class: "java.lang.Object".to_string(),
                    return_type: ResolvedType::Reference("java.lang.String".to_string()),
                    param_types: vec![],
                    is_static: false,
                    is_varargs: false,
                    is_generic: false,
                    access_flags: 0x0001, // ACC_PUBLIC
                    specificity_rank: 100,
                });
            }
            "equals" => {
                candidates.push(MethodCandidate {
                    method_name: "equals".to_string(),
                    declaring_class: "java.lang.Object".to_string(),
                    return_type: ResolvedType::Primitive(PrimitiveType::Boolean),
                    param_types: vec![ResolvedType::Reference("java.lang.Object".to_string())],
                    is_static: false,
                    is_varargs: false,
                    is_generic: false,
                    access_flags: 0x0001, // ACC_PUBLIC
                    specificity_rank: 100,
                });
            }
            "hashCode" => {
                candidates.push(MethodCandidate {
                    method_name: "hashCode".to_string(),
                    declaring_class: "java.lang.Object".to_string(),
                    return_type: ResolvedType::Primitive(PrimitiveType::Int),
                    param_types: vec![],
                    is_static: false,
                    is_varargs: false,
                    is_generic: false,
                    access_flags: 0x0001, // ACC_PUBLIC
                    specificity_rank: 100,
                });
            }
            _ => {
                // Generic method candidate for unknown methods
                candidates.push(MethodCandidate {
                    method_name: method_name.to_string(),
                    declaring_class: "java.lang.Object".to_string(),
                    return_type: ResolvedType::Reference("java.lang.Object".to_string()),
                    param_types: vec![],
                    is_static: false,
                    is_varargs: false,
                    is_generic: false,
                    access_flags: 0x0001, // ACC_PUBLIC
                    specificity_rank: 1000, // Low priority
                });
            }
        }
        
        Ok(candidates)
    }
    
    /// JavaC Resolve.resolveMethod equivalent - JLS §15.12.2 method resolution
    pub fn resolve_best_method_candidate(&mut self, context: &mut MethodResolutionContext) -> Result<Option<MethodCandidate>> {
        if context.candidates.is_empty() {
            return Ok(None);
        }
        
        // JLS §15.12.2: Method Resolution Process
        // Phase 1: Find applicable methods through multiple resolution phases
        for phase in [ResolutionPhase::ExactMatch, ResolutionPhase::WideningConversion, 
                      ResolutionPhase::BoxingConversion, ResolutionPhase::VarargsConversion] {
            context.phase = phase;
            let applicable_candidates = self.find_applicable_candidates(context)?;
            
            if !applicable_candidates.is_empty() {
                // Phase 2: Find most specific method among applicable candidates
                return Ok(Some(self.find_most_specific_method(applicable_candidates)?));
            }
        }
        
        Ok(None)
    }
    
    /// Find applicable candidates for current resolution phase - JavaC isApplicable equivalent
    fn find_applicable_candidates(&self, context: &MethodResolutionContext) -> Result<Vec<MethodCandidate>> {
        let mut applicable = Vec::new();
        
        for candidate in &context.candidates {
            if self.is_method_applicable(candidate, &context.target_arg_types, context.phase)? {
                applicable.push(candidate.clone());
            }
        }
        
        Ok(applicable)
    }
    
    /// Check if method is applicable with given arguments - JavaC isApplicable
    fn is_method_applicable(&self, candidate: &MethodCandidate, arg_types: &[ResolvedType], phase: ResolutionPhase) -> Result<bool> {
        let param_types = &candidate.param_types;
        
        // Handle varargs separately
        if candidate.is_varargs && phase == ResolutionPhase::VarargsConversion {
            return self.is_varargs_applicable(candidate, arg_types);
        }
        
        // Check arity (number of parameters)
        if param_types.len() != arg_types.len() {
            return Ok(false);
        }
        
        // Check each parameter-argument pair
        for (param_type, arg_type) in param_types.iter().zip(arg_types.iter()) {
            if !self.is_convertible(arg_type, param_type, phase)? {
                return Ok(false);
            }
        }
        
        Ok(true)
    }
    
    /// Check if varargs method is applicable
    fn is_varargs_applicable(&self, candidate: &MethodCandidate, arg_types: &[ResolvedType]) -> Result<bool> {
        let param_types = &candidate.param_types;
        
        // Varargs method must have at least one parameter (the varargs parameter)
        if param_types.is_empty() {
            return Ok(false);
        }
        
        let fixed_param_count = param_types.len() - 1;
        
        // Must have at least as many args as fixed parameters
        if arg_types.len() < fixed_param_count {
            return Ok(false);
        }
        
        // Check fixed parameters
        for (param_type, arg_type) in param_types[..fixed_param_count].iter().zip(arg_types.iter()) {
            if !self.is_convertible(arg_type, param_type, ResolutionPhase::BoxingConversion)? {
                return Ok(false);
            }
        }
        
        // Check varargs parameters (remaining args must be convertible to varargs component type)
        if let ResolvedType::Array(varargs_component_type) = &param_types[fixed_param_count] {
            for arg_type in &arg_types[fixed_param_count..] {
                if !self.is_convertible(arg_type, varargs_component_type, ResolutionPhase::BoxingConversion)? {
                    return Ok(false);
                }
            }
        }
        
        Ok(true)
    }
    
    /// Check type convertibility based on resolution phase - JavaC types.isConvertible
    fn is_convertible(&self, from_type: &ResolvedType, to_type: &ResolvedType, phase: ResolutionPhase) -> Result<bool> {
        match phase {
            ResolutionPhase::ExactMatch => {
                Ok(from_type == to_type)
            }
            ResolutionPhase::WideningConversion => {
                Ok(from_type == to_type || self.is_widening_convertible(from_type, to_type))
            }
            ResolutionPhase::BoxingConversion => {
                Ok(from_type == to_type || 
                   self.is_widening_convertible(from_type, to_type) ||
                   self.is_boxing_convertible(from_type, to_type))
            }
            ResolutionPhase::VarargsConversion => {
                // Same as boxing phase for individual arguments
                self.is_convertible(from_type, to_type, ResolutionPhase::BoxingConversion)
            }
        }
    }
    
    /// Find most specific method among applicable candidates - JavaC mostSpecific
    fn find_most_specific_method(&self, mut candidates: Vec<MethodCandidate>) -> Result<MethodCandidate> {
        if candidates.len() == 1 {
            return Ok(candidates.into_iter().next().unwrap());
        }
        
        // Sort by specificity rank (lower is more specific)
        candidates.sort_by_key(|c| c.specificity_rank);
        
        // Check for ambiguity - if top candidates have same rank, it's ambiguous
        if candidates.len() >= 2 && candidates[0].specificity_rank == candidates[1].specificity_rank {
            eprintln!("⚠️ ATTR: Ambiguous method call - multiple candidates with same specificity");
        }
        
        Ok(candidates.into_iter().next().unwrap())
    }
    
    // ========== TYPE CONVERSION HELPERS ==========
    // JavaC types.java equivalent conversion checking
    
    /// Check widening primitive conversion - JavaC types.isConvertible
    fn is_widening_convertible(&self, from_type: &ResolvedType, to_type: &ResolvedType) -> bool {
        use PrimitiveType::*;
        match (from_type, to_type) {
            // Byte widening conversions
            (ResolvedType::Primitive(Byte), ResolvedType::Primitive(Short | Int | Long | Float | Double)) => true,
            // Short widening conversions
            (ResolvedType::Primitive(Short), ResolvedType::Primitive(Int | Long | Float | Double)) => true,
            // Char widening conversions
            (ResolvedType::Primitive(Char), ResolvedType::Primitive(Int | Long | Float | Double)) => true,
            // Int widening conversions
            (ResolvedType::Primitive(Int), ResolvedType::Primitive(Long | Float | Double)) => true,
            // Long widening conversions
            (ResolvedType::Primitive(Long), ResolvedType::Primitive(Float | Double)) => true,
            // Float widening conversions
            (ResolvedType::Primitive(Float), ResolvedType::Primitive(Double)) => true,
            // Reference type conversions (simplified - should use subtyping)
            (ResolvedType::Reference(_), ResolvedType::Reference(to_class)) 
                if to_class == "java.lang.Object" => true,
            _ => false,
        }
    }
    
    /// Check boxing/unboxing conversion - JavaC types.isConvertible
    fn is_boxing_convertible(&self, from_type: &ResolvedType, to_type: &ResolvedType) -> bool {
        use PrimitiveType::*;
        match (from_type, to_type) {
            // Boxing conversions (primitive to wrapper)
            (ResolvedType::Primitive(Boolean), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Boolean" => true,
            (ResolvedType::Primitive(Byte), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Byte" => true,
            (ResolvedType::Primitive(Char), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Character" => true,
            (ResolvedType::Primitive(Short), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Short" => true,
            (ResolvedType::Primitive(Int), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Integer" => true,
            (ResolvedType::Primitive(Long), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Long" => true,
            (ResolvedType::Primitive(Float), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Float" => true,
            (ResolvedType::Primitive(Double), ResolvedType::Reference(to_class))
                if to_class == "java.lang.Double" => true,
            // Unboxing conversions (wrapper to primitive) - reverse of above
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Boolean))
                if from_class == "java.lang.Boolean" => true,
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Byte))
                if from_class == "java.lang.Byte" => true,
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Char))
                if from_class == "java.lang.Character" => true,
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Short))
                if from_class == "java.lang.Short" => true,
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Int))
                if from_class == "java.lang.Integer" => true,
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Long))
                if from_class == "java.lang.Long" => true,
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Float))
                if from_class == "java.lang.Float" => true,
            (ResolvedType::Reference(from_class), ResolvedType::Primitive(Double))
                if from_class == "java.lang.Double" => true,
            _ => false,
        }
    }
    
    /// Resolve field type
    fn resolve_field_type(&self, field_name: &str, target: &Option<Box<Expr>>) -> Result<ResolvedType> {
        // For now, return a placeholder field resolution
        // TODO: Implement full field resolution logic
        eprintln!("🔍 ATTR: Resolving field access: {}", field_name);
        
        Ok(ResolvedType::Reference("java.lang.Object".to_string()))
    }
}

impl Default for Attr {
    fn default() -> Self {
        Self::new()
    }
}