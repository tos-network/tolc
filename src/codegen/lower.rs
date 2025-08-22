//! Lower - AST transformation and optimization stage
//! 
//! Corresponds to javac's Lower.java - handles high-level optimizations
//! and transformations before bytecode generation.

use crate::ast::*;
use crate::common::error::Result;
use super::constant_optimizer::ConstantOptimizer;
use super::string_optimizer::StringOptimizer;
use super::loop_optimizer::LoopOptimizer;

/// Main lowering and optimization coordinator
/// Corresponds to javac's Lower class
pub struct Lower {
    /// Constant folding and compile-time computation
    constant_optimizer: ConstantOptimizer,
    
    /// String concatenation optimization  
    string_optimizer: StringOptimizer,
    
    /// Loop optimization
    loop_optimizer: LoopOptimizer,
    
    /// Enable/disable optimization passes
    enable_optimizations: bool,
}

impl Lower {
    /// Create new Lower instance
    pub fn new(enable_optimizations: bool) -> Self {
        Self {
            constant_optimizer: ConstantOptimizer::new(),
            string_optimizer: StringOptimizer::new(),
            loop_optimizer: LoopOptimizer::new(),
            enable_optimizations,
        }
    }
    
    /// Main lowering entry point - transform AST
    /// This corresponds to javac's Lower.translate() method
    pub fn lower_compilation_unit(&mut self, ast: &mut Ast) -> Result<()> {
        if !self.enable_optimizations {
            return Ok(());
        }
        
        // Apply optimizations to each type declaration
        for type_decl in &mut ast.type_decls {
            self.lower_type_decl(type_decl)?;
        }
        
        Ok(())
    }
    
    /// Lower type declaration
    fn lower_type_decl(&mut self, type_decl: &mut TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class) => self.lower_class(class),
            TypeDecl::Interface(interface) => self.lower_interface(interface),
            TypeDecl::Enum(enum_decl) => self.lower_enum(enum_decl),
            TypeDecl::Annotation(annotation) => self.lower_annotation(annotation),
        }
    }
    
    /// Lower class declaration
    fn lower_class(&mut self, class: &mut ClassDecl) -> Result<()> {
        for member in &mut class.body {
            self.lower_class_member(member)?;
        }
        Ok(())
    }
    
    /// Lower interface declaration
    fn lower_interface(&mut self, interface: &mut InterfaceDecl) -> Result<()> {
        for member in &mut interface.body {
            self.lower_interface_member(member)?;
        }
        Ok(())
    }
    
    /// Lower enum declaration
    fn lower_enum(&mut self, _enum_decl: &mut EnumDecl) -> Result<()> {
        // TODO: Implement enum-specific lowering
        Ok(())
    }
    
    /// Lower annotation declaration
    fn lower_annotation(&mut self, _annotation: &mut AnnotationDecl) -> Result<()> {
        // TODO: Implement annotation-specific lowering
        Ok(())
    }
    
    /// Lower class member
    fn lower_class_member(&mut self, member: &mut ClassMember) -> Result<()> {
        match member {
            ClassMember::Method(method) => self.lower_method(method),
            ClassMember::Field(_field) => Ok(()), // Fields don't need lowering
            ClassMember::Constructor(constructor) => self.lower_constructor(constructor),
            ClassMember::TypeDecl(type_decl) => self.lower_type_decl(type_decl),
        }
    }
    
    /// Lower interface member
    fn lower_interface_member(&mut self, member: &mut InterfaceMember) -> Result<()> {
        match member {
            InterfaceMember::Method(method) => self.lower_method(method),
            InterfaceMember::Field(_field) => Ok(()), // Fields don't need lowering
            InterfaceMember::TypeDecl(type_decl) => self.lower_type_decl(type_decl),
        }
    }
    
    /// Lower method declaration
    fn lower_method(&mut self, method: &mut MethodDecl) -> Result<()> {
        if let Some(ref mut body) = method.body {
            self.lower_stmt(body)?;
        }
        Ok(())
    }
    
    /// Lower constructor declaration
    fn lower_constructor(&mut self, constructor: &mut ConstructorDecl) -> Result<()> {
        if let Some(ref mut body) = constructor.body {
            self.lower_stmt(body)?;
        }
        Ok(())
    }
    
    /// Lower statement - main optimization entry point
    fn lower_stmt(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Block(block) => self.lower_block(block),
            Stmt::Expression(expr_stmt) => self.lower_expr_stmt(expr_stmt),
            Stmt::If(if_stmt) => self.lower_if_stmt(if_stmt),
            Stmt::While(while_stmt) => self.lower_while_stmt(while_stmt),
            Stmt::For(for_stmt) => self.lower_for_stmt(for_stmt),
            Stmt::EnhancedFor(_enhanced_for_stmt) => Ok(()), // No lowering needed yet
            Stmt::Return(return_stmt) => self.lower_return_stmt(return_stmt),
            Stmt::Declaration(var_stmt) => self.lower_var_stmt(var_stmt),
            Stmt::Try(try_stmt) => self.lower_try_stmt(try_stmt),
            Stmt::Synchronized(sync_stmt) => self.lower_synchronized_stmt(sync_stmt),
            _ => Ok(()), // Other statements don't need lowering
        }
    }
    
    /// Lower block statement
    fn lower_block(&mut self, block: &mut Block) -> Result<()> {
        for stmt in &mut block.statements {
            self.lower_stmt(stmt)?;
        }
        Ok(())
    }
    
    /// Lower expression statement
    fn lower_expr_stmt(&mut self, expr_stmt: &mut ExprStmt) -> Result<()> {
        self.lower_expr(&mut expr_stmt.expression)?;
        Ok(())
    }
    
    /// Lower if statement
    fn lower_if_stmt(&mut self, if_stmt: &mut IfStmt) -> Result<()> {
        // Lower condition
        self.lower_expr(&mut if_stmt.condition)?;
        
        // Apply constant folding to condition
        if let Some(constant_result) = self.constant_optimizer.fold_boolean_expression(&if_stmt.condition) {
            eprintln!("🔧 Lower: Constant folded if condition to: {}", constant_result);
            // TODO: Replace with constant true/false and eliminate dead branches
        }
        
        // Lower branches
        self.lower_stmt(&mut if_stmt.then_branch)?;
        if let Some(ref mut else_branch) = if_stmt.else_branch {
            self.lower_stmt(else_branch)?;
        }
        
        Ok(())
    }
    
    /// Lower while statement
    fn lower_while_stmt(&mut self, while_stmt: &mut WhileStmt) -> Result<()> {
        // Lower condition
        self.lower_expr(&mut while_stmt.condition)?;
        
        // Apply loop optimizations
        self.loop_optimizer.optimize_while_loop(while_stmt)?;
        
        // Lower body
        self.lower_stmt(&mut while_stmt.body)?;
        
        Ok(())
    }
    
    /// Lower for statement
    fn lower_for_stmt(&mut self, for_stmt: &mut ForStmt) -> Result<()> {
        // Lower initializers
        for init_stmt in &mut for_stmt.init {
            self.lower_stmt(init_stmt)?;
        }
        
        // Lower condition
        if let Some(ref mut condition) = for_stmt.condition {
            self.lower_expr(condition)?;
        }
        
        // Lower updates
        for update_stmt in &mut for_stmt.update {
            self.lower_expr(&mut update_stmt.expr)?;
        }
        
        // Apply loop optimizations
        self.loop_optimizer.optimize_for_loop(for_stmt)?;
        
        // Lower body
        self.lower_stmt(&mut for_stmt.body)?;
        
        Ok(())
    }
    
    /// Lower return statement
    fn lower_return_stmt(&mut self, return_stmt: &mut ReturnStmt) -> Result<()> {
        if let Some(ref mut expr) = return_stmt.expression {
            self.lower_expr(expr)?;
        }
        Ok(())
    }
    
    /// Lower variable statement
    fn lower_var_stmt(&mut self, var_stmt: &mut VarDeclStmt) -> Result<()> {
        for var_decl in &mut var_stmt.variables {
            self.lower_var_decl(var_decl)?;
        }
        Ok(())
    }
    
    /// Lower variable declaration
    fn lower_var_decl(&mut self, var_decl: &mut VariableDeclarator) -> Result<()> {
        if let Some(ref mut init) = var_decl.initializer {
            self.lower_expr(init)?;
        }
        Ok(())
    }
    
    /// Lower try statement
    fn lower_try_stmt(&mut self, try_stmt: &mut TryStmt) -> Result<()> {
        self.lower_stmt(&mut try_stmt.try_block)?;
        
        for catch_clause in &mut try_stmt.catch_clauses {
            self.lower_stmt(&mut catch_clause.catch_block)?;
        }
        
        if let Some(ref mut finally_block) = try_stmt.finally_block {
            self.lower_stmt(finally_block)?;
        }
        
        Ok(())
    }
    
    /// Lower synchronized statement
    fn lower_synchronized_stmt(&mut self, sync_stmt: &mut SynchronizedStmt) -> Result<()> {
        self.lower_expr(&mut sync_stmt.expression)?;
        self.lower_stmt(&mut sync_stmt.block)?;
        Ok(())
    }
    
    /// Lower expression - apply optimizations
    fn lower_expr(&mut self, expr: &mut Expr) -> Result<()> {
        match expr {
            Expr::Binary(bin_expr) => self.lower_binary_expr(bin_expr),
            Expr::Unary(un_expr) => self.lower_unary_expr(un_expr),
            Expr::MethodCall(call_expr) => self.lower_method_call(call_expr),
            Expr::FieldAccess(field_expr) => self.lower_field_access(field_expr),
            Expr::ArrayAccess(array_expr) => self.lower_array_access(array_expr),
            Expr::New(new_expr) => self.lower_new_expr(new_expr),
            Expr::Assignment(assign_expr) => self.lower_assignment(assign_expr),
            Expr::Cast(cast_expr) => self.lower_cast(cast_expr),
            _ => Ok(()), // Literals and identifiers don't need lowering
        }
    }
    
    /// Lower binary expression
    fn lower_binary_expr(&mut self, bin_expr: &mut BinaryExpr) -> Result<()> {
        // Lower operands first
        self.lower_expr(&mut bin_expr.left)?;
        self.lower_expr(&mut bin_expr.right)?;
        
        // Apply constant folding
        if let Some(constant_result) = self.constant_optimizer.fold_binary_expression(bin_expr) {
            eprintln!("🔧 Lower: Constant folded binary expression to: {:?}", constant_result);
            // TODO: Replace with constant literal
        }
        
        // Apply string concatenation optimization
        if bin_expr.operator == BinaryOp::Add {
            if let Some(optimized) = self.string_optimizer.optimize_string_concatenation(bin_expr)? {
                eprintln!("🔧 Lower: Optimized string concatenation");
                // TODO: Replace with optimized version
            }
        }
        
        Ok(())
    }
    
    /// Lower unary expression
    fn lower_unary_expr(&mut self, un_expr: &mut UnaryExpr) -> Result<()> {
        self.lower_expr(&mut un_expr.operand)?;
        
        // Apply constant folding for unary operations
        if let Some(constant_result) = self.constant_optimizer.fold_unary_expression(un_expr) {
            eprintln!("🔧 Lower: Constant folded unary expression to: {:?}", constant_result);
            // TODO: Replace with constant literal
        }
        
        Ok(())
    }
    
    /// Lower method call
    fn lower_method_call(&mut self, call_expr: &mut MethodCallExpr) -> Result<()> {
        // Lower receiver
        if let Some(ref mut receiver) = call_expr.receiver {
            self.lower_expr(receiver)?;
        }
        
        // Lower arguments
        for arg in &mut call_expr.arguments {
            self.lower_expr(arg)?;
        }
        
        Ok(())
    }
    
    // Additional lowering methods - placeholders for now
    fn lower_field_access(&mut self, _field_expr: &mut FieldAccessExpr) -> Result<()> { Ok(()) }
    fn lower_array_access(&mut self, _array_expr: &mut ArrayAccessExpr) -> Result<()> { Ok(()) }
    fn lower_new_expr(&mut self, _new_expr: &mut NewExpr) -> Result<()> { Ok(()) }
    fn lower_assignment(&mut self, _assign_expr: &mut AssignmentExpr) -> Result<()> { Ok(()) }
    fn lower_cast(&mut self, _cast_expr: &mut CastExpr) -> Result<()> { Ok(()) }
}