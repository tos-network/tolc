//! Attr phase - Type checking and method resolution
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Attr` class.
//! This is the main context-dependent analysis phase that encompasses
//! name resolution, type checking and constant folding as subtasks.
//!
//! JavaC alignment: This module implements the same visitor pattern and
//! attribution logic as JavaC's Attr.java.

use crate::ast::{Ast, TypeDecl, ClassDecl, MethodDecl, Expr, Stmt, BinaryOp};
use crate::error::{Result, Error};
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
    Wildcard(Option<Box<ResolvedType>>, WildcardBound),
    /// Type variables (T, E, etc.)
    TypeVariable(String),
    /// Method types for functional interfaces
    Method(Vec<ResolvedType>, Box<ResolvedType>),
    /// Error type for failed resolution
    Error,
    /// No type (void)
    NoType,
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
    Unbound,
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
    
    /// Process AST through Attr phase - type checking and resolution
    /// Corresponds to JavaC's Attr.attrib() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        eprintln!("🔍 ATTR: Starting type attribution");
        
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
    
    /// Resolve a type reference to a ResolvedType
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
            _ => ResolvedType::Reference(type_ref.name.clone()),
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
}

impl Default for Attr {
    fn default() -> Self {
        Self::new()
    }
}