//! Lower phase - Syntactic sugar desugaring
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Lower` class.
//! This phase transforms high-level language constructs into simpler forms
//! that are easier to compile to bytecode. This includes:
//! - Enhanced for loops → traditional for loops with iterators
//! - String concatenation → StringBuilder operations
//! - Autoboxing/unboxing → explicit constructor/method calls
//! - Try-with-resources → traditional try-catch-finally blocks
//! - Class literals → synthetic accessor methods
//! - Assertions → conditional throws

use crate::ast::{Ast, TypeDecl, ClassDecl, Stmt, Expr, MethodDecl, ConstructorDecl};
use crate::error::Result;
use std::collections::HashMap;

/// Lowering statistics
#[derive(Debug, Default)]
pub struct LoweringStats {
    pub enhanced_for_loops: usize,
    pub string_concatenations: usize,
    pub autoboxing_ops: usize,
    pub lambda_expressions: usize,
    pub try_with_resources: usize,
    pub class_literals: usize,
    pub assertions: usize,
    pub inner_classes: usize,
}

/// Context for lowering transformations
#[derive(Debug, Clone)]
pub struct LoweringContext {
    /// Current class being processed
    pub current_class: Option<String>,
    /// Current method being processed
    pub current_method: Option<String>,
    /// Variable counter for synthetic variables
    pub var_counter: usize,
    /// Generated synthetic methods
    pub synthetic_methods: Vec<MethodDecl>,
    /// Generated synthetic fields
    pub synthetic_fields: Vec<String>,
}

/// Lower phase processor - corresponds to JavaC's Lower class
pub struct Lower {
    pub stats: LoweringStats,
    pub context: LoweringContext,
    /// Map from original constructs to their lowered forms
    pub lowering_cache: HashMap<String, String>,
}

impl Lower {
    pub fn new() -> Self {
        Self {
            stats: LoweringStats::default(),
            context: LoweringContext {
                current_class: None,
                current_method: None,
                var_counter: 0,
                synthetic_methods: Vec::new(),
                synthetic_fields: Vec::new(),
            },
            lowering_cache: HashMap::new(),
        }
    }
    
    /// Process AST through Lower phase - desugar syntax
    /// Corresponds to JavaC's Lower.translateTopLevelClass() method
    pub fn process(&mut self, mut ast: Ast) -> Result<Ast> {
        self.process_with_types(ast, &std::collections::HashMap::new())
    }
    
    /// Process AST with erased type information from TransTypes phase
    pub fn process_with_types(&mut self, mut ast: Ast, erased_types: &std::collections::HashMap<String, String>) -> Result<Ast> {
        eprintln!("🔍 LOWER: Starting syntax desugaring");
        eprintln!("📊 LOWER: Using {} erased type mappings", erased_types.len());
        
        // Store erased types for use in lowering decisions
        // (In a full implementation, this would inform casting and bridge method decisions)
        
        // Process all type declarations
        for type_decl in &mut ast.type_decls {
            self.lower_type_declaration(type_decl)?;
        }
        
        eprintln!("✅ LOWER: Syntax desugaring complete");
        eprintln!("📊 LOWER: Stats - Enhanced for: {}, String concat: {}, Autoboxing: {}, Lambda: {}, Try-with-resources: {}, Class literals: {}, Assertions: {}, Inner classes: {}", 
                 self.stats.enhanced_for_loops,
                 self.stats.string_concatenations, 
                 self.stats.autoboxing_ops,
                 self.stats.lambda_expressions,
                 self.stats.try_with_resources,
                 self.stats.class_literals,
                 self.stats.assertions,
                 self.stats.inner_classes);
        
        Ok(ast)
    }
    
    /// Lower a type declaration - corresponds to JavaC's visitClassDef
    fn lower_type_declaration(&mut self, type_decl: &mut TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.lower_class_declaration(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                eprintln!("🔧 LOWER: Processing interface: {}", interface_decl.name);
                // Interfaces typically don't need much lowering
                for member in &mut interface_decl.body {
                    if let crate::ast::InterfaceMember::Method(method) = member {
                        self.context.current_method = Some(method.name.clone());
                        self.lower_method_declaration(method)?;
                        self.context.current_method = None;
                    }
                }
            }
            TypeDecl::Enum(enum_decl) => {
                eprintln!("🔧 LOWER: Processing enum: {}", enum_decl.name);
                // TODO: Implement enum lowering (synthetic methods, values(), valueOf(), etc.)
            }
            TypeDecl::Annotation(annotation_decl) => {
                eprintln!("🔧 LOWER: Processing annotation: {}", annotation_decl.name);
                // Annotations typically don't need lowering
            }
        }
        Ok(())
    }
    
    /// Lower a class declaration - corresponds to JavaC's translateClass
    fn lower_class_declaration(&mut self, class_decl: &mut ClassDecl) -> Result<()> {
        eprintln!("🔧 LOWER: Processing class: {}", class_decl.name);
        
        // Set current class context
        self.context.current_class = Some(class_decl.name.clone());
        
        // Process class members
        for member in &mut class_decl.body {
            self.lower_class_member(member)?;
        }
        
        // Add any synthetic methods that were generated
        for synthetic_method in &self.context.synthetic_methods {
            class_decl.body.push(crate::ast::ClassMember::Method(synthetic_method.clone()));
        }
        self.context.synthetic_methods.clear();
        
        // Clear current class context
        self.context.current_class = None;
        
        Ok(())
    }
    
    /// Lower a class member
    fn lower_class_member(&mut self, member: &mut crate::ast::ClassMember) -> Result<()> {
        match member {
            crate::ast::ClassMember::Method(method) => {
                self.context.current_method = Some(method.name.clone());
                self.lower_method_declaration(method)?;
                self.context.current_method = None;
            }
            crate::ast::ClassMember::Constructor(constructor) => {
                self.context.current_method = Some("<init>".to_string());
                self.lower_constructor_declaration(constructor)?;
                self.context.current_method = None;
            }
            crate::ast::ClassMember::Field(field) => {
                eprintln!("🔧 LOWER: Processing field: {}", field.name);
                // Fields typically don't need lowering unless they have complex initializers
            }
            crate::ast::ClassMember::TypeDecl(nested_type) => {
                eprintln!("🔧 LOWER: Processing nested type");
                self.stats.inner_classes += 1;
                self.lower_type_declaration(nested_type)?;
            }
            crate::ast::ClassMember::Initializer(initializer) => {
                eprintln!("🔧 LOWER: Processing initializer block");
                for stmt in &mut initializer.body.statements {
                    self.lower_statement(stmt)?;
                }
            }
        }
        Ok(())
    }
    
    /// Lower a method declaration
    fn lower_method_declaration(&mut self, method: &mut MethodDecl) -> Result<()> {
        eprintln!("🔧 LOWER: Processing method: {}", method.name);
        
        if let Some(ref mut body) = method.body {
            for stmt in &mut body.statements {
                self.lower_statement(stmt)?;
            }
        }
        
        Ok(())
    }
    
    /// Lower a constructor declaration
    fn lower_constructor_declaration(&mut self, constructor: &mut ConstructorDecl) -> Result<()> {
        eprintln!("🔧 LOWER: Processing constructor");
        
        for stmt in &mut constructor.body.statements {
            self.lower_statement(stmt)?;
        }
        
        Ok(())
    }
    
    /// Lower a statement - this is where most syntactic sugar is handled
    fn lower_statement(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            // Enhanced for loops and try-with-resources are not in current AST
            // They would be handled as part of For and Try statements respectively
            Stmt::Try(try_stmt) => {
                if !try_stmt.resources.is_empty() {
                    eprintln!("🔧 LOWER: Lowering try-with-resources");
                    self.stats.try_with_resources += 1;
                    // TODO: Transform try-with-resources into traditional try-catch-finally
                }
                
                for stmt in &mut try_stmt.try_block.statements {
                    self.lower_statement(stmt)?;
                }
                
                for catch_clause in &mut try_stmt.catch_clauses {
                    for stmt in &mut catch_clause.block.statements {
                        self.lower_statement(stmt)?;
                    }
                }
                
                if let Some(ref mut finally_block) = try_stmt.finally_block {
                    for stmt in &mut finally_block.statements {
                        self.lower_statement(stmt)?;
                    }
                }
            }
            Stmt::Expression(expr_stmt) => {
                self.lower_expression(&mut expr_stmt.expr)?;
            }
            Stmt::Declaration(var_decl) => {
                for variable in &mut var_decl.variables {
                    if let Some(ref mut init) = variable.initializer {
                        self.lower_expression(init)?;
                    }
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref mut expr) = return_stmt.value {
                    self.lower_expression(expr)?;
                }
            }
            Stmt::If(if_stmt) => {
                self.lower_expression(&mut if_stmt.condition)?;
                self.lower_statement(&mut if_stmt.then_branch)?;
                if let Some(ref mut else_branch) = if_stmt.else_branch {
                    self.lower_statement(else_branch)?;
                }
            }
            Stmt::While(while_stmt) => {
                self.lower_expression(&mut while_stmt.condition)?;
                self.lower_statement(&mut while_stmt.body)?;
            }
            Stmt::For(for_stmt) => {
                for init_stmt in &mut for_stmt.init {
                    self.lower_statement(init_stmt)?;
                }
                if let Some(ref mut condition) = for_stmt.condition {
                    self.lower_expression(condition)?;
                }
                for update_stmt in &mut for_stmt.update {
                    self.lower_expression(&mut update_stmt.expr)?;
                }
                self.lower_statement(&mut for_stmt.body)?;
            }
            Stmt::Block(block) => {
                for stmt in &mut block.statements {
                    self.lower_statement(stmt)?;
                }
            }
            Stmt::Assert(assert_stmt) => {
                eprintln!("🔧 LOWER: Lowering assertion");
                self.stats.assertions += 1;
                // TODO: Transform assertion into conditional throw
                self.lower_expression(&mut assert_stmt.condition)?;
                if let Some(ref mut message) = assert_stmt.message {
                    self.lower_expression(message)?;
                }
            }
            _ => {
                // Other statement types don't need lowering
            }
        }
        Ok(())
    }
    
    /// Lower an expression - handle autoboxing, string concatenation, etc.
    fn lower_expression(&mut self, expr: &mut Expr) -> Result<()> {
        match expr {
            Expr::Binary(binary_expr) => {
                self.lower_expression(&mut binary_expr.left)?;
                self.lower_expression(&mut binary_expr.right)?;
                
                // Check for string concatenation
                if binary_expr.operator == crate::ast::BinaryOp::Add {
                    if self.is_string_concatenation(&binary_expr) {
                        eprintln!("🔧 LOWER: Lowering string concatenation");
                        self.stats.string_concatenations += 1;
                        
                        // Transform: "str" + value -> StringBuilder.append() chain
                        let lowered_expr = self.lower_string_concatenation(binary_expr)?;
                        *expr = lowered_expr;
                        return Ok(());
                    }
                }
            }
            Expr::MethodCall(method_call) => {
                if let Some(ref mut target) = method_call.target {
                    self.lower_expression(target)?;
                }
                for arg in &mut method_call.arguments {
                    self.lower_expression(arg)?;
                }
                
                // Check for autoboxing/unboxing
                if self.is_autoboxing_operation(&method_call) {
                    eprintln!("🔧 LOWER: Lowering autoboxing operation");
                    self.stats.autoboxing_ops += 1;
                    // TODO: Transform autoboxing into explicit constructor/method calls
                }
            }
            Expr::Lambda(lambda_expr) => {
                eprintln!("🔧 LOWER: Lowering lambda expression");
                self.stats.lambda_expressions += 1;
                // TODO: Transform lambda into anonymous class
                // Lambda body can be expression or block - handle accordingly
                match &mut lambda_expr.body {
                    crate::ast::LambdaBody::Expression(expr) => {
                        self.lower_expression(expr)?;
                    }
                    crate::ast::LambdaBody::Block(block) => {
                        for stmt in &mut block.statements {
                            self.lower_statement(stmt)?;
                        }
                    }
                }
            }
            // Class literals not in current AST - would be special FieldAccess
            Expr::Assignment(assign_expr) => {
                self.lower_expression(&mut assign_expr.value)?;
                // Note: target is typically an identifier, no need to lower
            }
            Expr::Unary(unary_expr) => {
                self.lower_expression(&mut unary_expr.operand)?;
            }
            Expr::Conditional(cond_expr) => {
                self.lower_expression(&mut cond_expr.condition)?;
                self.lower_expression(&mut cond_expr.then_expr)?;
                self.lower_expression(&mut cond_expr.else_expr)?;
            }
            Expr::FieldAccess(field_access) => {
                if let Some(ref mut target) = field_access.target {
                    self.lower_expression(target)?;
                }
            }
            Expr::ArrayAccess(array_access) => {
                self.lower_expression(&mut array_access.array)?;
                self.lower_expression(&mut array_access.index)?;
            }
            Expr::New(new_expr) => {
                for arg in &mut new_expr.arguments {
                    self.lower_expression(arg)?;
                }
                // NewExpr doesn't have initializer - anonymous_body is for anonymous classes
                if let Some(ref mut anonymous_body) = new_expr.anonymous_body {
                    // Handle anonymous class body - would need to iterate through ClassMember
                    // For now, just note that this is where anonymous class lowering would happen
                    eprintln!("🔧 LOWER: Anonymous class detected");
                }
            }
            _ => {
                // Literals, identifiers, etc. don't need lowering
            }
        }
        Ok(())
    }
    
    /// Check if a binary expression represents string concatenation
    fn is_string_concatenation(&self, binary_expr: &crate::ast::BinaryExpr) -> bool {
        if binary_expr.operator != crate::ast::BinaryOp::Add {
            return false;
        }
        
        // Check if either operand is a string literal
        self.is_string_type(&binary_expr.left) || self.is_string_type(&binary_expr.right)
    }
    
    /// Check if an expression is likely a string type
    fn is_string_type(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(literal_expr) => {
                matches!(literal_expr.value, crate::ast::Literal::String(_))
            }
            Expr::MethodCall(method_call) => {
                // Methods that return String
                matches!(method_call.name.as_str(), "toString" | "concat" | "substring")
            }
            Expr::Binary(binary_expr) => {
                // Nested string concatenation
                binary_expr.operator == crate::ast::BinaryOp::Add && 
                (self.is_string_type(&binary_expr.left) || self.is_string_type(&binary_expr.right))
            }
            _ => false,
        }
    }
    
    /// Transform string concatenation into StringBuilder method calls
    /// "Hello " + variable -> new StringBuilder().append("Hello ").append(variable).toString()
    fn lower_string_concatenation(&mut self, binary_expr: &crate::ast::BinaryExpr) -> Result<Expr> {
        use crate::ast::*;
        
        // Create new StringBuilder() constructor call
        let string_builder_new = Expr::New(NewExpr {
            target_type: TypeRef {
                name: "StringBuilder".to_string(),
                type_args: vec![],
                span: binary_expr.span.clone(),
                annotations: vec![],
                array_dims: 0,
            },
            arguments: vec![],
            anonymous_body: None,
            span: binary_expr.span.clone(),
        });
        
        // Chain .append() calls for each operand
        let mut current_expr = string_builder_new;
        
        // Add left operand
        current_expr = self.create_append_call(current_expr, (*binary_expr.left).clone())?;
        
        // Add right operand  
        current_expr = self.create_append_call(current_expr, (*binary_expr.right).clone())?;
        
        // Finally call .toString()
        let result = Expr::MethodCall(MethodCallExpr {
            target: Some(Box::new(current_expr)),
            name: "toString".to_string(),
            arguments: vec![],
            span: binary_expr.span.clone(),
        });
        
        Ok(result)
    }
    
    /// Create a StringBuilder.append() call
    fn create_append_call(&self, target: Expr, argument: Expr) -> Result<Expr> {
        use crate::ast::*;
        
        Ok(Expr::MethodCall(MethodCallExpr {
            target: Some(Box::new(target)),
            name: "append".to_string(),
            arguments: vec![argument],
            span: Span::default(), // TODO: Use proper span
        }))
    }
    
    /// Check if a method call represents an autoboxing operation
    fn is_autoboxing_operation(&self, method_call: &crate::ast::MethodCallExpr) -> bool {
        // Check for common autoboxing method names
        matches!(method_call.name.as_str(), 
                "valueOf" | "intValue" | "doubleValue" | "booleanValue" | 
                "byteValue" | "shortValue" | "longValue" | "floatValue" | "charValue")
    }
    
    /// Generate a synthetic variable name
    fn generate_synthetic_variable(&mut self, prefix: &str) -> String {
        self.context.var_counter += 1;
        format!("{}${}", prefix, self.context.var_counter)
    }
    
    /// Generate a synthetic method name
    fn generate_synthetic_method(&mut self, prefix: &str) -> String {
        self.context.var_counter += 1;
        format!("{}${}", prefix, self.context.var_counter)
    }
}

impl Default for Lower {
    fn default() -> Self {
        Self::new()
    }
}