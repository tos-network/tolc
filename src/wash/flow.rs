//! Flow phase - Data flow analysis
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Flow` class.
//! This phase performs dataflow analysis including:
//! - Liveness analysis (reachability checking)
//! - Exception analysis (checked exception handling)
//! - Definite assignment analysis (variables assigned before use)
//! - Definite unassignment analysis (final variables assigned only once)
//! - Local variable capture analysis (effectively final analysis)

use crate::ast::{Ast, TypeDecl, ClassDecl, MethodDecl, Stmt, Expr};
use crate::error::Result;
use std::collections::{HashMap, HashSet};

/// Flow analysis mode for definite assignment
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FlowKind {
    /// Normal DA/DU analysis mode
    Normal,
    /// Speculative DA/DU analysis for loop bodies
    SpeculativeLoop,
}

/// Jump kind for control flow analysis
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JumpKind {
    Break,
    Continue,
    Return,
    Throw,
}

/// Pending exit representing control flow transfers
#[derive(Debug, Clone)]
pub struct PendingExit {
    pub jump_kind: JumpKind,
    pub target_label: Option<String>,
    pub statement_id: usize,
    pub expression_id: Option<usize>,
}

/// Variable assignment status
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentStatus {
    /// Definitely assigned
    DefinitelyAssigned,
    /// Definitely unassigned  
    DefinitelyUnassigned,
    /// Possibly assigned (unknown state)
    PossiblyAssigned,
}

/// Flow analysis environment
#[derive(Debug)]
pub struct FlowEnvironment {
    /// Current assignment status for each variable
    pub assignment_status: HashMap<String, AssignmentStatus>,
    /// Variables that are effectively final
    pub effectively_final: HashSet<String>,
    /// Variables that might be assigned in loops
    pub loop_assigned: HashSet<String>,
    /// Pending control flow exits
    pub pending_exits: Vec<PendingExit>,
    /// Current flow analysis mode
    pub flow_kind: FlowKind,
    /// Whether current code is reachable
    pub is_alive: bool,
    /// Exception types that can be thrown
    pub thrown_types: HashSet<String>,
    /// Variables captured by inner classes/lambdas
    pub captured_vars: HashSet<String>,
}

/// Flow phase processor - corresponds to JavaC's Flow class
pub struct Flow {
    pub flow_env: FlowEnvironment,
}

impl Flow {
    pub fn new() -> Self {
        Self {
            flow_env: FlowEnvironment {
                assignment_status: HashMap::new(),
                effectively_final: HashSet::new(),
                loop_assigned: HashSet::new(),
                pending_exits: Vec::new(),
                flow_kind: FlowKind::Normal,
                is_alive: true,
                thrown_types: HashSet::new(),
                captured_vars: HashSet::new(),
            },
        }
    }
    
    /// Process AST through Flow phase - dataflow analysis
    /// Corresponds to JavaC's Flow.analyzeTree() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        self.process_with_symbols(ast, None)
    }
    
    /// Process AST with symbol environment from Enter phase
    pub fn process_with_symbols(&mut self, ast: Ast, symbol_env: Option<&crate::wash::enter::SymbolEnvironment>) -> Result<Ast> {
        eprintln!("🔍 FLOW: Starting dataflow analysis");
        
        // Store symbol environment for generic type handling
        if let Some(sym_env) = symbol_env {
            eprintln!("📚 FLOW: Using symbol environment with {} classes", sym_env.classes.len());
            // TODO: Store symbol env for generic variable analysis
        }
        
        // Run multiple analyzers as in javac
        self.run_alive_analyzer(&ast)?;
        self.run_assign_analyzer(&ast)?;
        self.run_flow_analyzer(&ast)?;
        self.run_capture_analyzer(&ast)?;
        
        eprintln!("✅ FLOW: Dataflow analysis complete");
        eprintln!("📊 FLOW: {} variables analyzed, {} effectively final", 
                 self.flow_env.assignment_status.len(),
                 self.flow_env.effectively_final.len());
        
        Ok(ast)
    }
    
    /// Run liveness analysis - corresponds to JavaC's AliveAnalyzer
    fn run_alive_analyzer(&mut self, ast: &Ast) -> Result<()> {
        eprintln!("🔍 FLOW: Running liveness analysis");
        
        for type_decl in &ast.type_decls {
            self.analyze_type_decl_liveness(type_decl)?;
        }
        
        Ok(())
    }
    
    /// Run definite assignment analysis - corresponds to JavaC's AssignAnalyzer
    fn run_assign_analyzer(&mut self, ast: &Ast) -> Result<()> {
        eprintln!("🔍 FLOW: Running definite assignment analysis");
        
        for type_decl in &ast.type_decls {
            self.analyze_type_decl_assignment(type_decl)?;
        }
        
        Ok(())
    }
    
    /// Run exception flow analysis - corresponds to JavaC's FlowAnalyzer
    fn run_flow_analyzer(&mut self, ast: &Ast) -> Result<()> {
        eprintln!("🔍 FLOW: Running exception flow analysis");
        
        for type_decl in &ast.type_decls {
            self.analyze_type_decl_exceptions(type_decl)?;
        }
        
        Ok(())
    }
    
    /// Run capture analysis - corresponds to JavaC's CaptureAnalyzer
    fn run_capture_analyzer(&mut self, ast: &Ast) -> Result<()> {
        eprintln!("🔍 FLOW: Running capture analysis");
        
        for type_decl in &ast.type_decls {
            self.analyze_type_decl_capture(type_decl)?;
        }
        
        Ok(())
    }
    
    /// Analyze type declaration for liveness
    fn analyze_type_decl_liveness(&mut self, type_decl: &TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.analyze_class_decl_liveness(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                eprintln!("🔍 FLOW: Liveness analysis for interface: {}", interface_decl.name);
                // Interfaces have no executable code, skip
            }
            TypeDecl::Enum(enum_decl) => {
                eprintln!("🔍 FLOW: Liveness analysis for enum: {}", enum_decl.name);
                // TODO: Implement enum liveness analysis
            }
            TypeDecl::Annotation(annotation_decl) => {
                eprintln!("🔍 FLOW: Liveness analysis for annotation: {}", annotation_decl.name);
                // Annotations have no executable code, skip
            }
        }
        Ok(())
    }
    
    /// Analyze class declaration for liveness
    fn analyze_class_decl_liveness(&mut self, class_decl: &ClassDecl) -> Result<()> {
        eprintln!("🔍 FLOW: Liveness analysis for class: {}", class_decl.name);
        
        for member in &class_decl.body {
            match member {
                crate::ast::ClassMember::Method(method_decl) => {
                    self.analyze_method_decl_liveness(method_decl)?;
                }
                crate::ast::ClassMember::Constructor(constructor_decl) => {
                    eprintln!("🔍 FLOW: Liveness analysis for constructor");
                    // TODO: Implement constructor liveness analysis
                }
                crate::ast::ClassMember::Initializer(initializer) => {
                    eprintln!("🔍 FLOW: Liveness analysis for initializer");
                    // TODO: Implement initializer liveness analysis
                }
                _ => {} // Fields and nested types don't need liveness analysis
            }
        }
        
        Ok(())
    }
    
    /// Analyze method declaration for liveness
    fn analyze_method_decl_liveness(&mut self, method_decl: &MethodDecl) -> Result<()> {
        eprintln!("🔍 FLOW: Liveness analysis for method: {}", method_decl.name);
        
        // Reset liveness state for new method
        self.flow_env.is_alive = true;
        self.flow_env.pending_exits.clear();
        
        if let Some(ref body) = method_decl.body {
            for stmt in &body.statements {
                if self.flow_env.is_alive {
                    self.analyze_statement_liveness(stmt)?;
                } else {
                    eprintln!("⚠️ FLOW: Unreachable statement detected");
                    // TODO: Report unreachable code error
                }
            }
        }
        
        Ok(())
    }
    
    /// Analyze statement for liveness
    fn analyze_statement_liveness(&mut self, stmt: &Stmt) -> Result<()> {
        use crate::ast::Stmt;
        
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.analyze_expression_liveness(&expr_stmt.expr)?;
            }
            Stmt::Declaration(var_decl) => {
                // Variable declarations don't affect liveness
                for variable in &var_decl.variables {
                    if let Some(ref init) = variable.initializer {
                        self.analyze_expression_liveness(init)?;
                    }
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref expr) = return_stmt.value {
                    self.analyze_expression_liveness(expr)?;
                }
                // Return makes code unreachable
                self.flow_env.is_alive = false;
                self.record_pending_exit(JumpKind::Return, None);
            }
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    if self.flow_env.is_alive {
                        self.analyze_statement_liveness(stmt)?;
                    }
                }
            }
            Stmt::If(if_stmt) => {
                self.analyze_expression_liveness(&if_stmt.condition)?;
                
                let alive_before = self.flow_env.is_alive;
                self.analyze_statement_liveness(&if_stmt.then_branch)?;
                let alive_after_then = self.flow_env.is_alive;
                
                if let Some(ref else_stmt) = if_stmt.else_branch {
                    self.flow_env.is_alive = alive_before;
                    self.analyze_statement_liveness(else_stmt)?;
                    let alive_after_else = self.flow_env.is_alive;
                    
                    // Alive if either branch can complete
                    self.flow_env.is_alive = alive_after_then || alive_after_else;
                } else {
                    // If no else, can always fall through
                    self.flow_env.is_alive = true;
                }
            }
            Stmt::While(while_stmt) => {
                self.analyze_expression_liveness(&while_stmt.condition)?;
                
                let old_pending = self.flow_env.pending_exits.clone();
                self.flow_env.pending_exits.clear();
                
                self.analyze_statement_liveness(&while_stmt.body)?;
                
                // Check for continues
                self.resolve_continues();
                
                // While loops might not execute
                self.flow_env.is_alive = true;
                self.flow_env.pending_exits = old_pending;
            }
            Stmt::For(for_stmt) => {
                for init_stmt in &for_stmt.init {
                    self.analyze_statement_liveness(init_stmt)?;
                }
                
                if let Some(ref condition) = for_stmt.condition {
                    self.analyze_expression_liveness(condition)?;
                }
                
                let old_pending = self.flow_env.pending_exits.clone();
                self.flow_env.pending_exits.clear();
                
                self.analyze_statement_liveness(&for_stmt.body)?;
                
                for update_stmt in &for_stmt.update {
                    self.analyze_expression_liveness(&update_stmt.expr)?;
                }
                
                self.resolve_continues();
                
                // For loops might not execute
                self.flow_env.is_alive = true;
                self.flow_env.pending_exits = old_pending;
            }
            Stmt::Break(break_stmt) => {
                self.flow_env.is_alive = false;
                self.record_pending_exit(JumpKind::Break, break_stmt.label.clone());
            }
            Stmt::Continue(continue_stmt) => {
                self.flow_env.is_alive = false;
                self.record_pending_exit(JumpKind::Continue, continue_stmt.label.clone());
            }
            _ => {
                eprintln!("🔍 FLOW: Unhandled statement type for liveness analysis");
            }
        }
        
        Ok(())
    }
    
    /// Analyze expression for liveness
    fn analyze_expression_liveness(&mut self, expr: &Expr) -> Result<()> {
        use crate::ast::Expr;
        
        match expr {
            Expr::MethodCall(method_call) => {
                if let Some(ref object) = method_call.target {
                    self.analyze_expression_liveness(object)?;
                }
                for arg in &method_call.arguments {
                    self.analyze_expression_liveness(arg)?;
                }
                
                // TODO: Check if method can throw checked exceptions
            }
            Expr::Binary(binary_expr) => {
                self.analyze_expression_liveness(&binary_expr.left)?;
                self.analyze_expression_liveness(&binary_expr.right)?;
            }
            Expr::Unary(unary_expr) => {
                self.analyze_expression_liveness(&unary_expr.operand)?;
            }
            Expr::Assignment(assign_expr) => {
                self.analyze_expression_liveness(&assign_expr.value)?;
                // Assignment target is analyzed for definite assignment
            }
            _ => {
                // Literals, identifiers, etc. don't affect liveness
            }
        }
        
        Ok(())
    }
    
    /// Analyze type declaration for assignment
    fn analyze_type_decl_assignment(&mut self, type_decl: &TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.analyze_class_decl_assignment(class_decl)?;
            }
            _ => {} // Other types don't need assignment analysis
        }
        Ok(())
    }
    
    /// Analyze class declaration for assignment
    fn analyze_class_decl_assignment(&mut self, class_decl: &ClassDecl) -> Result<()> {
        eprintln!("🔍 FLOW: Assignment analysis for class: {}", class_decl.name);
        
        for member in &class_decl.body {
            if let crate::ast::ClassMember::Method(method_decl) = member {
                self.analyze_method_decl_assignment(method_decl)?;
            }
        }
        
        Ok(())
    }
    
    /// Analyze method declaration for assignment
    fn analyze_method_decl_assignment(&mut self, method_decl: &MethodDecl) -> Result<()> {
        eprintln!("🔍 FLOW: Assignment analysis for method: {}", method_decl.name);
        
        // Reset assignment state for new method
        self.flow_env.assignment_status.clear();
        
        // Parameters are definitely assigned
        for param in &method_decl.parameters {
            self.flow_env.assignment_status.insert(
                param.name.clone(), 
                AssignmentStatus::DefinitelyAssigned
            );
        }
        
        if let Some(ref body) = method_decl.body {
            for stmt in &body.statements {
                self.analyze_statement_assignment(stmt)?;
            }
        }
        
        Ok(())
    }
    
    /// Analyze statement for assignment
    fn analyze_statement_assignment(&mut self, stmt: &Stmt) -> Result<()> {
        use crate::ast::Stmt;
        
        match stmt {
            Stmt::Declaration(var_decl) => {
                for variable in &var_decl.variables {
                    if variable.initializer.is_some() {
                        self.flow_env.assignment_status.insert(
                            variable.name.clone(),
                            AssignmentStatus::DefinitelyAssigned
                        );
                    } else {
                        self.flow_env.assignment_status.insert(
                            variable.name.clone(),
                            AssignmentStatus::DefinitelyUnassigned
                        );
                    }
                }
            }
            Stmt::Expression(expr_stmt) => {
                self.analyze_expression_assignment(&expr_stmt.expr)?;
            }
            _ => {
                // TODO: Handle other statement types
            }
        }
        
        Ok(())
    }
    
    /// Analyze expression for assignment
    fn analyze_expression_assignment(&mut self, expr: &Expr) -> Result<()> {
        use crate::ast::Expr;
        
        match expr {
            Expr::Assignment(assign_expr) => {
                // Analyze RHS first
                self.analyze_expression_assignment(&assign_expr.value)?;
                
                // Mark LHS as assigned
                if let Expr::Identifier(ident) = &*assign_expr.target {
                    self.flow_env.assignment_status.insert(
                        ident.name.clone(),
                        AssignmentStatus::DefinitelyAssigned
                    );
                }
            }
            Expr::Identifier(ident) => {
                // Check if variable is definitely assigned before use
                match self.flow_env.assignment_status.get(&ident.name) {
                    Some(AssignmentStatus::DefinitelyUnassigned) | None => {
                        eprintln!("⚠️ FLOW: Variable '{}' might not be initialized", ident.name);
                        // TODO: Report definite assignment error
                    }
                    _ => {} // Variable is definitely assigned
                }
            }
            _ => {
                // TODO: Handle other expression types
            }
        }
        
        Ok(())
    }
    
    /// Analyze type declaration for exceptions
    fn analyze_type_decl_exceptions(&mut self, _type_decl: &TypeDecl) -> Result<()> {
        // TODO: Implement exception analysis
        Ok(())
    }
    
    /// Analyze type declaration for capture
    fn analyze_type_decl_capture(&mut self, _type_decl: &TypeDecl) -> Result<()> {
        // TODO: Implement capture analysis for lambdas and inner classes
        Ok(())
    }
    
    /// Record a pending exit for control flow
    fn record_pending_exit(&mut self, jump_kind: JumpKind, target_label: Option<String>) {
        let exit = PendingExit {
            jump_kind,
            target_label,
            statement_id: 0, // TODO: Use actual statement ID
            expression_id: None,
        };
        self.flow_env.pending_exits.push(exit);
    }
    
    /// Resolve continue statements
    fn resolve_continues(&mut self) {
        self.flow_env.pending_exits.retain(|exit| {
            exit.jump_kind != JumpKind::Continue
        });
    }
    
    /// Resolve break statements
    fn resolve_breaks(&mut self) {
        self.flow_env.pending_exits.retain(|exit| {
            exit.jump_kind != JumpKind::Break
        });
    }
}

impl Default for Flow {
    fn default() -> Self {
        Self::new()
    }
}