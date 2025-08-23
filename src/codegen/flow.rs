//! Flow phase - Data flow analysis
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Flow` class.
//! This phase performs dataflow analysis including:
//! - Liveness analysis (reachability checking)
//! - Exception analysis (checked exception handling)
//! - Definite assignment analysis (variables assigned before use)
//! - Definite unassignment analysis (final variables assigned only once)
//! - Local variable capture analysis (effectively final analysis)

use crate::ast::{Ast, TypeDecl, ClassDecl, MethodDecl, Stmt, Expr, Block, TryStmt};
use crate::common::error::Result;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

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
    /// Method-level flow analysis results cache
    pub method_flow_cache: HashMap<String, MethodFlowInfo>,
    /// Try-catch flow analysis cache
    pub try_catch_cache: HashMap<usize, TryCatchFlowInfo>,
    /// Flow analysis statistics
    pub flow_stats: FlowStatistics,
}

/// Method-level flow analysis results
#[derive(Debug, Clone)]
pub struct MethodFlowInfo {
    /// Method signature for identification
    pub method_signature: String,
    /// Variables definitely assigned at method entry
    pub entry_assignments: HashSet<String>,
    /// Variables effectively final in this method
    pub effectively_final_vars: HashSet<String>,
    /// Exception types this method may throw
    pub thrown_exceptions: HashSet<String>,
    /// Whether method can complete normally (not always throw/return)
    pub can_complete_normally: bool,
    /// Variables that need initialization checking
    pub uninitialized_vars: HashSet<String>,
}

/// Flow analysis statistics for performance tracking
#[derive(Debug, Clone, Default)]
pub struct FlowStatistics {
    /// Number of methods analyzed
    pub methods_analyzed: usize,
    /// Number of try-catch blocks analyzed
    pub try_catch_blocks: usize,
    /// Cache hit rate for flow queries
    pub cache_hits: usize,
    /// Total flow queries
    pub total_queries: usize,
    /// Analysis time tracking
    pub analysis_start_time: Option<std::time::Instant>,
}

/// Snapshot of flow analysis state for save/restore operations
#[derive(Debug, Clone)]
pub struct FlowState {
    /// Variable assignment status snapshot
    pub assignment_status: HashMap<String, AssignmentStatus>,
    /// Effectively final variables snapshot
    pub effectively_final: HashSet<String>,
    /// Code reachability state
    pub is_alive: bool,
    /// Exception types that can be thrown
    pub thrown_types: HashSet<String>,
    /// Pending control flow exits
    pub pending_exits: Vec<PendingExit>,
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
                method_flow_cache: HashMap::new(),
                try_catch_cache: HashMap::new(),
                flow_stats: FlowStatistics::default(),
            },
        }
    }
    
    /// Process AST through Flow phase - dataflow analysis
    /// Corresponds to JavaC's Flow.analyzeTree() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        self.process_with_symbols(ast, None)
    }
    
    /// Process AST with symbol environment from Enter phase
    pub fn process_with_symbols(&mut self, ast: Ast, symbol_env: Option<&crate::codegen::enter::SymbolEnvironment>) -> Result<Ast> {
        eprintln!("🔍 FLOW: Starting dataflow analysis");
        
        // Start timing
        self.start_analysis_timing();
        
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
        
        // Report statistics
        let stats = self.get_flow_statistics();
        let (cached_items, total_queries, hit_rate) = self.get_cache_info();
        
        eprintln!("✅ FLOW: Dataflow analysis complete");
        eprintln!("📊 FLOW: {} variables analyzed, {} effectively final", 
                 self.flow_env.assignment_status.len(),
                 self.flow_env.effectively_final.len());
        eprintln!("📊 FLOW: {} methods analyzed, {} try-catch blocks analyzed", 
                 stats.methods_analyzed, stats.try_catch_blocks);
        eprintln!("📊 FLOW: {} cached items, {} queries, {:.1}% hit rate", 
                 cached_items, total_queries, hit_rate * 100.0);
        
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
    fn analyze_type_decl_exceptions(&mut self, type_decl: &TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.analyze_class_decl_exceptions(class_decl)?;
            }
            _ => {} // Other types don't need exception analysis
        }
        Ok(())
    }
    
    /// Analyze class declaration for exceptions
    fn analyze_class_decl_exceptions(&mut self, class_decl: &ClassDecl) -> Result<()> {
        eprintln!("🔍 FLOW: Exception analysis for class: {}", class_decl.name);
        
        for member in &class_decl.body {
            if let crate::ast::ClassMember::Method(method_decl) = member {
                self.analyze_method_decl_exceptions(method_decl)?;
            }
        }
        
        Ok(())
    }
    
    /// Analyze method declaration for exceptions
    fn analyze_method_decl_exceptions(&mut self, method_decl: &MethodDecl) -> Result<()> {
        eprintln!("🔍 FLOW: Exception analysis for method: {}", method_decl.name);
        
        // Reset exception state for new method
        self.flow_env.thrown_types.clear();
        
        if let Some(ref body) = method_decl.body {
            for stmt in &body.statements {
                self.analyze_statement_exceptions(stmt)?;
            }
        }
        
        // Create and cache method flow info for later queries
        let method_signature = format!("{}({})", method_decl.name, 
            method_decl.parameters.iter()
                .map(|p| p.type_ref.name.clone())
                .collect::<Vec<_>>()
                .join(",")
        );
        self.create_method_flow_info(method_signature);
        
        Ok(())
    }
    
    /// Analyze statement for exceptions
    fn analyze_statement_exceptions(&mut self, stmt: &Stmt) -> Result<()> {
        use crate::ast::Stmt;
        
        match stmt {
            Stmt::Try(try_stmt) => {
                let _flow_info = self.analyze_try_catch_flow(try_stmt)?;
                // TODO: Use flow_info to track exception propagation
            }
            Stmt::Throw(throw_stmt) => {
                if let Some(exception_type) = self.extract_exception_type(&throw_stmt.expr)? {
                    self.flow_env.thrown_types.insert(exception_type);
                }
            }
            Stmt::Expression(expr_stmt) => {
                self.analyze_expression_exceptions(&expr_stmt.expr)?;
            }
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    self.analyze_statement_exceptions(stmt)?;
                }
            }
            _ => {
                // TODO: Handle other statement types that may throw
            }
        }
        
        Ok(())
    }
    
    /// Analyze expression for exceptions
    fn analyze_expression_exceptions(&mut self, expr: &Expr) -> Result<()> {
        use crate::ast::Expr;
        
        match expr {
            Expr::MethodCall(_) => {
                // Method calls may throw checked exceptions
                // For now, assume RuntimeException - would need method signature analysis
                self.flow_env.thrown_types.insert("java.lang.RuntimeException".to_string());
            }
            Expr::Binary(binary_expr) => {
                self.analyze_expression_exceptions(&binary_expr.left)?;
                self.analyze_expression_exceptions(&binary_expr.right)?;
            }
            Expr::Unary(unary_expr) => {
                self.analyze_expression_exceptions(&unary_expr.operand)?;
            }
            _ => {
                // Other expressions generally don't throw checked exceptions
            }
        }
        
        Ok(())
    }
    
    /// Complete flow analysis for try-catch-finally blocks
    /// This ensures proper exception handling semantics and control flow
    pub fn analyze_try_catch_flow(&mut self, tree: &TryStmt) -> Result<TryCatchFlowInfo> {
        // Generate a simple hash for the try statement (in practice, would use AST node ID)
        let try_id = format!("{:p}", tree as *const TryStmt).as_ptr() as usize;
        
        // Check cache first
        if let Some(cached_info) = self.get_try_catch_flow(try_id) {
            return Ok(cached_info.clone());
        }
        
        let mut flow_info = TryCatchFlowInfo::new();
        
        // Analyze try block for potential exceptions
        flow_info.try_may_throw = self.analyze_block_exceptions(&tree.try_block)?;
        
        // Analyze catch clauses
        for (i, catch_clause) in tree.catch_clauses.iter().enumerate() {
            let catch_analysis = CatchClauseAnalysis {
                exception_type: catch_clause.parameter.type_ref.name.clone(),
                handles_all_from_try: self.handles_exception_type(&catch_clause.parameter.type_ref.name, &flow_info.try_may_throw),
                may_throw_new: self.analyze_block_exceptions(&catch_clause.block)?,
                var_index: i as u16, // Placeholder - actual index set during code generation
            };
            flow_info.catch_analyses.push(catch_analysis);
        }
        
        // Analyze finally block if present
        if let Some(ref finally_block) = tree.finally_block {
            flow_info.finally_analysis = Some(FinallyBlockAnalysis {
                may_throw: self.analyze_block_exceptions(finally_block)?,
                unconditional_exit: self.has_unconditional_exit(finally_block)?,
            });
        }
        
        // Compute overall exception flow
        flow_info.compute_exception_propagation();
        
        // Cache the result for future queries
        self.cache_try_catch_flow(try_id, flow_info.clone());
        
        Ok(flow_info)
    }
    
    /// Analyze what exceptions a block may throw
    pub fn analyze_block_exceptions(&mut self, block: &Block) -> Result<Vec<String>> {
        let mut exceptions = Vec::new();
        
        // Simple analysis - look for throw statements and method calls
        for stmt in &block.statements {
            match stmt {
                Stmt::Throw(throw_stmt) => {
                    // Extract exception type from throw expression
                    if let Some(exception_type) = self.extract_exception_type(&throw_stmt.expr)? {
                        exceptions.push(exception_type);
                    }
                }
                Stmt::Expression(expr_stmt) => {
                    // Check for method calls that may throw checked exceptions
                    if let Expr::MethodCall(_) = &expr_stmt.expr {
                        // For now, assume method calls may throw RuntimeException
                        // In full implementation, would check method signatures
                        exceptions.push("java.lang.RuntimeException".to_string());
                    }
                }
                Stmt::Try(nested_try) => {
                    // Recursively analyze nested try blocks
                    let nested_exceptions = self.analyze_try_catch_flow(nested_try)?;
                    exceptions.extend(nested_exceptions.propagated_exceptions);
                }
                _ => {
                    // Other statements may implicitly throw (NullPointerException, etc.)
                    // For comprehensive analysis, would need to check all operations
                }
            }
        }
        
        Ok(exceptions)
    }
    
    /// Extract exception type from throw expression
    pub fn extract_exception_type(&mut self, expr: &Expr) -> Result<Option<String>> {
        match expr {
            Expr::New(new_expr) => {
                // Extract type from new expression
                Ok(Some(new_expr.target_type.name.clone()))
            }
            Expr::Identifier(_) => {
                // Variable reference - would need type inference
                Ok(Some("java.lang.Exception".to_string())) // Fallback
            }
            _ => Ok(None)
        }
    }
    
    /// Check if catch clause handles exception type
    pub fn handles_exception_type(&self, catch_type: &str, thrown_types: &[String]) -> bool {
        for thrown_type in thrown_types {
            if thrown_type == catch_type || self.is_subtype(thrown_type, catch_type) {
                return true;
            }
        }
        false
    }
    
    /// Simple subtype checking (would be more sophisticated in full implementation)
    pub fn is_subtype(&self, subtype: &str, supertype: &str) -> bool {
        // Simplified - just check for common inheritance patterns
        match (subtype, supertype) {
            (_, "java.lang.Throwable") => true,
            (_, "java.lang.Exception") => !subtype.contains("Error"),
            ("java.lang.RuntimeException", "java.lang.Exception") => true,
            (_, "java.lang.RuntimeException") => subtype.contains("RuntimeException"),
            _ => subtype == supertype,
        }
    }
    
    /// Check if block has unconditional exit (return, throw, etc.)
    pub fn has_unconditional_exit(&mut self, block: &Block) -> Result<bool> {
        if block.statements.is_empty() {
            return Ok(false);
        }
        
        // Check last statement for unconditional exit
        match block.statements.last() {
            Some(Stmt::Return(_)) => Ok(true),
            Some(Stmt::Throw(_)) => Ok(true),
            Some(Stmt::Block(nested_block)) => self.has_unconditional_exit(nested_block),
            _ => Ok(false),
        }
    }
    
    /// Analyze type declaration for capture
    fn analyze_type_decl_capture(&mut self, _type_decl: &TypeDecl) -> Result<()> {
        // TODO: Implement capture analysis for lambdas and inner classes
        Ok(())
    }
    
    // ================================================================================
    // FLOW ANALYSIS STATE MANAGEMENT - JavaC Flow patterns
    // ================================================================================
    
    /// Create method flow info and cache it for later queries
    pub fn create_method_flow_info(&mut self, method_signature: String) -> MethodFlowInfo {
        let flow_info = MethodFlowInfo {
            method_signature: method_signature.clone(),
            entry_assignments: self.flow_env.assignment_status.keys()
                .filter(|var| matches!(self.flow_env.assignment_status.get(*var), Some(AssignmentStatus::DefinitelyAssigned)))
                .cloned().collect(),
            effectively_final_vars: self.flow_env.effectively_final.clone(),
            thrown_exceptions: self.flow_env.thrown_types.clone(),
            can_complete_normally: self.flow_env.is_alive,
            uninitialized_vars: self.flow_env.assignment_status.keys()
                .filter(|var| matches!(self.flow_env.assignment_status.get(*var), Some(AssignmentStatus::DefinitelyUnassigned)))
                .cloned().collect(),
        };
        
        self.flow_env.method_flow_cache.insert(method_signature, flow_info.clone());
        self.flow_env.flow_stats.methods_analyzed += 1;
        
        flow_info
    }
    
    /// Query method flow information from cache
    pub fn get_method_flow_info(&mut self, method_signature: &str) -> Option<&MethodFlowInfo> {
        self.flow_env.flow_stats.total_queries += 1;
        
        if let Some(info) = self.flow_env.method_flow_cache.get(method_signature) {
            self.flow_env.flow_stats.cache_hits += 1;
            Some(info)
        } else {
            None
        }
    }
    
    /// Cache try-catch flow analysis results
    pub fn cache_try_catch_flow(&mut self, try_id: usize, flow_info: TryCatchFlowInfo) {
        self.flow_env.try_catch_cache.insert(try_id, flow_info);
        self.flow_env.flow_stats.try_catch_blocks += 1;
    }
    
    /// Query try-catch flow information from cache
    pub fn get_try_catch_flow(&mut self, try_id: usize) -> Option<&TryCatchFlowInfo> {
        self.flow_env.flow_stats.total_queries += 1;
        
        if let Some(info) = self.flow_env.try_catch_cache.get(&try_id) {
            self.flow_env.flow_stats.cache_hits += 1;
            Some(info)
        } else {
            None
        }
    }
    
    /// Check if a variable is effectively final
    /// Used by lambda expressions and inner classes
    pub fn is_effectively_final(&self, var_name: &str) -> bool {
        self.flow_env.effectively_final.contains(var_name)
    }
    
    /// Check if a variable is definitely assigned at current point
    /// Used for definite assignment checking (JLS §16)
    pub fn is_definitely_assigned(&self, var_name: &str) -> bool {
        matches!(
            self.flow_env.assignment_status.get(var_name),
            Some(AssignmentStatus::DefinitelyAssigned)
        )
    }
    
    /// Mark variable as effectively final
    /// Called when variable is not reassigned after initialization
    pub fn mark_effectively_final(&mut self, var_name: String) {
        self.flow_env.effectively_final.insert(var_name);
    }
    
    /// Mark variable as loop-assigned  
    /// Variables assigned in loops are not effectively final
    pub fn mark_loop_assigned(&mut self, var_name: String) {
        self.flow_env.loop_assigned.insert(var_name.clone());
        self.flow_env.effectively_final.remove(&var_name);
    }
    
    /// Get all captured variables for current scope
    /// Used by codegen for closure generation
    pub fn get_captured_variables(&self) -> &HashSet<String> {
        &self.flow_env.captured_vars
    }
    
    /// Check if current code point is reachable
    /// Used for unreachable code detection (JLS §14.21)
    pub fn is_reachable(&self) -> bool {
        self.flow_env.is_alive
    }
    
    /// Mark current code point as unreachable
    /// Called after return, throw, break, continue statements
    pub fn mark_unreachable(&mut self) {
        self.flow_env.is_alive = false;
    }
    
    /// Mark current code point as reachable
    /// Called at beginning of new blocks, after conditionals
    pub fn mark_reachable(&mut self) {
        self.flow_env.is_alive = true;
    }
    
    /// Save current flow state for later restoration
    /// Used for analyzing conditional branches independently
    pub fn save_flow_state(&self) -> FlowState {
        FlowState {
            assignment_status: self.flow_env.assignment_status.clone(),
            effectively_final: self.flow_env.effectively_final.clone(),
            is_alive: self.flow_env.is_alive,
            thrown_types: self.flow_env.thrown_types.clone(),
            pending_exits: self.flow_env.pending_exits.clone(),
        }
    }
    
    /// Restore previously saved flow state
    /// Used after analyzing conditional branches
    pub fn restore_flow_state(&mut self, state: FlowState) {
        self.flow_env.assignment_status = state.assignment_status;
        self.flow_env.effectively_final = state.effectively_final;
        self.flow_env.is_alive = state.is_alive;
        self.flow_env.thrown_types = state.thrown_types;
        self.flow_env.pending_exits = state.pending_exits;
    }
    
    /// Merge two flow states (used for joining control flow branches)
    /// Implements JavaC Flow.merge() semantics
    pub fn merge_flow_states(&mut self, state1: &FlowState, state2: &FlowState) {
        // Merge assignment status - conservative approach
        for (var, status) in &state1.assignment_status {
            match state2.assignment_status.get(var) {
                Some(other_status) => {
                    let merged_status = match (status, other_status) {
                        (AssignmentStatus::DefinitelyAssigned, AssignmentStatus::DefinitelyAssigned) => {
                            AssignmentStatus::DefinitelyAssigned
                        }
                        (AssignmentStatus::DefinitelyUnassigned, AssignmentStatus::DefinitelyUnassigned) => {
                            AssignmentStatus::DefinitelyUnassigned
                        }
                        _ => AssignmentStatus::PossiblyAssigned,
                    };
                    self.flow_env.assignment_status.insert(var.clone(), merged_status);
                }
                None => {
                    self.flow_env.assignment_status.insert(var.clone(), AssignmentStatus::PossiblyAssigned);
                }
            }
        }
        
        // Merge effectively final - intersection (must be final in both branches)
        self.flow_env.effectively_final = state1.effectively_final
            .intersection(&state2.effectively_final)
            .cloned()
            .collect();
        
        // Merge reachability - reachable if either branch is reachable
        self.flow_env.is_alive = state1.is_alive || state2.is_alive;
        
        // Merge thrown exceptions - union
        self.flow_env.thrown_types = state1.thrown_types
            .union(&state2.thrown_types)
            .cloned()
            .collect();
    }
    
    /// Start flow analysis timing
    pub fn start_analysis_timing(&mut self) {
        self.flow_env.flow_stats.analysis_start_time = Some(std::time::Instant::now());
    }
    
    /// Get flow analysis statistics
    pub fn get_flow_statistics(&self) -> &FlowStatistics {
        &self.flow_env.flow_stats
    }
    
    /// Reset flow analysis statistics
    pub fn reset_statistics(&mut self) {
        self.flow_env.flow_stats = FlowStatistics::default();
    }
    
    /// Clear all caches to free memory
    pub fn clear_caches(&mut self) {
        self.flow_env.method_flow_cache.clear();
        self.flow_env.try_catch_cache.clear();
    }
    
    /// Get cache utilization information
    pub fn get_cache_info(&self) -> (usize, usize, f64) {
        let total_cached = self.flow_env.method_flow_cache.len() + self.flow_env.try_catch_cache.len();
        let hit_rate = if self.flow_env.flow_stats.total_queries > 0 {
            self.flow_env.flow_stats.cache_hits as f64 / self.flow_env.flow_stats.total_queries as f64
        } else {
            0.0
        };
        (total_cached, self.flow_env.flow_stats.total_queries, hit_rate)
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

/// Try-catch flow analysis information - JavaC Flow equivalent for exception handling
#[derive(Debug, Clone)]
pub struct TryCatchFlowInfo {
    /// Exceptions that may be thrown from try block
    pub try_may_throw: Vec<String>,
    /// Analysis for each catch clause
    pub catch_analyses: Vec<CatchClauseAnalysis>,
    /// Analysis for finally block if present
    pub finally_analysis: Option<FinallyBlockAnalysis>,
    /// Exceptions that propagate out of the entire try-catch-finally
    pub propagated_exceptions: Vec<String>,
}

impl TryCatchFlowInfo {
    pub fn new() -> Self {
        Self {
            try_may_throw: Vec::new(),
            catch_analyses: Vec::new(),
            finally_analysis: None,
            propagated_exceptions: Vec::new(),
        }
    }
    
    /// Compute which exceptions propagate out of the entire construct
    /// Based on JavaC Flow.visitTry exception propagation logic
    pub fn compute_exception_propagation(&mut self) {
        self.propagated_exceptions.clear();
        
        // Start with exceptions from try block
        for exception in &self.try_may_throw {
            let mut handled = false;
            
            // Check if any catch clause handles this exception
            for catch_analysis in &self.catch_analyses {
                if catch_analysis.handles_exception(exception) {
                    handled = true;
                    break;
                }
            }
            
            // If not handled, it propagates
            if !handled {
                self.propagated_exceptions.push(exception.clone());
            }
        }
        
        // Add exceptions from catch clauses
        for catch_analysis in &self.catch_analyses {
            self.propagated_exceptions.extend(catch_analysis.may_throw_new.iter().cloned());
        }
        
        // Add exceptions from finally block
        if let Some(ref finally_analysis) = self.finally_analysis {
            self.propagated_exceptions.extend(finally_analysis.may_throw.iter().cloned());
        }
    }
}

/// Analysis of a single catch clause - JavaC Flow patterns
#[derive(Debug, Clone)]
pub struct CatchClauseAnalysis {
    /// Exception type this clause catches
    pub exception_type: String,
    /// Whether this clause handles all exceptions from try block
    pub handles_all_from_try: bool,
    /// New exceptions this catch clause may throw
    pub may_throw_new: Vec<String>,
    /// Local variable index for caught exception
    pub var_index: u16,
}

impl CatchClauseAnalysis {
    /// Check if this catch clause handles the given exception type
    /// Implements JavaC Types.isSubtype for exception hierarchy
    pub fn handles_exception(&self, exception_type: &str) -> bool {
        // Simple check - would be more sophisticated with proper type hierarchy
        exception_type == self.exception_type 
            || self.exception_type == "java.lang.Exception" 
            || self.exception_type == "java.lang.Throwable"
            || (self.exception_type == "java.lang.RuntimeException" && exception_type.contains("RuntimeException"))
    }
}

/// Analysis of finally block - JavaC Flow patterns for finally semantics
#[derive(Debug, Clone)]
pub struct FinallyBlockAnalysis {
    /// Exceptions that may be thrown from finally block
    pub may_throw: Vec<String>,
    /// Whether finally block has unconditional exit (return, throw)
    /// This affects exception propagation - exceptions are suppressed if finally exits
    pub unconditional_exit: bool,
}