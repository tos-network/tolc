//! Attr phase - Type checking and method resolution
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Attr` class.
//! This is the main context-dependent analysis phase that encompasses
//! name resolution, type checking and constant folding as subtasks.
//!
//! JavaC alignment: This module implements the same visitor pattern and
//! attribution logic as JavaC's Attr.java.

use crate::ast::{Ast, TypeDecl, ClassDecl, MethodDecl, Expr, Stmt, BinaryOp};
use crate::common::error::{Result, Error};
use crate::wash::enter::SymbolEnvironment;
use std::collections::{HashMap, HashSet};

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
                symbol_env: None,
                deferred_tasks: Vec::new(),
                current_result_info: None,
            },
        }
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
    pub fn process_with_symbols(&mut self, ast: Ast, symbol_env: Option<&crate::wash::enter::SymbolEnvironment>) -> Result<Ast> {
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
                // TODO: Implement field attribution
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
}

impl Default for Attr {
    fn default() -> Self {
        Self::new()
    }
}