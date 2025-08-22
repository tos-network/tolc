/// Loop optimization (javac-style)
/// Implements javac's sophisticated loop generation and optimization patterns

use crate::codegen::opcodes;
use crate::ast::{Stmt, Expr, ForStmt, WhileStmt};

#[derive(Debug, Clone)]
pub struct LoopPattern {
    pub loop_type: LoopType,
    pub body: Box<Stmt>,
    pub condition: Option<Expr>,
    pub initialization: Vec<Stmt>,
    pub step: Vec<Stmt>,
    pub optimization_opportunities: Vec<LoopOptimization>,
}

#[derive(Debug, Clone)]
pub enum LoopType {
    /// while (condition) body
    While,
    /// for (init; condition; step) body
    For,
    /// Enhanced for loop (for-each)
    Enhanced,
}

#[derive(Debug, Clone)]
pub enum LoopOptimization {
    /// Constant condition optimization
    ConstantCondition { always_true: bool },
    /// Loop invariant code motion
    InvariantCodeMotion { movable_expressions: Vec<Expr> },
    /// Loop unrolling for small fixed iterations
    LoopUnrolling { iteration_count: usize },
    /// Strength reduction (e.g., multiplication to addition)
    StrengthReduction,
    /// Dead code elimination in loop body
    DeadCodeElimination,
}

pub struct LoopOptimizer;

impl LoopOptimizer {
    /// Analyze loop pattern (javac genLoop pattern)
    pub fn analyze_loop_pattern(stmt: &Stmt) -> Option<LoopPattern> {
        match stmt {
            Stmt::While(while_stmt) => Some(Self::analyze_while_loop(while_stmt)),
            Stmt::For(for_stmt) => Some(Self::analyze_for_loop(for_stmt)),
            _ => None,
        }
    }
    
    /// Analyze while loop (javac visitWhileLoop pattern)
    fn analyze_while_loop(while_stmt: &WhileStmt) -> LoopPattern {
        let mut optimizations = Vec::new();
        
        // Check for constant condition
        if let Some(constant_result) = Self::evaluate_constant_condition(&while_stmt.condition) {
            optimizations.push(LoopOptimization::ConstantCondition {
                always_true: constant_result,
            });
        }
        
        // Analyze loop body for optimizations
        optimizations.extend(Self::analyze_loop_body(&while_stmt.body));
        
        LoopPattern {
            loop_type: LoopType::While,
            body: while_stmt.body.clone(),
            condition: Some(while_stmt.condition.clone()),
            initialization: Vec::new(),
            step: Vec::new(),
            optimization_opportunities: optimizations,
        }
    }
    

    
    /// Analyze for loop (javac visitForLoop pattern)
    fn analyze_for_loop(for_stmt: &ForStmt) -> LoopPattern {
        let mut optimizations = Vec::new();
        
        // Check for constant condition
        if let Some(ref condition) = for_stmt.condition {
            if let Some(constant_result) = Self::evaluate_constant_condition(condition) {
                optimizations.push(LoopOptimization::ConstantCondition {
                    always_true: constant_result,
                });
            }
        }
        
        // Check for loop unrolling opportunities
        if let Some(iteration_count) = Self::analyze_fixed_iteration_count(for_stmt) {
            if iteration_count <= 10 { // javac-style threshold
                optimizations.push(LoopOptimization::LoopUnrolling { iteration_count });
            }
        }
        
        // Analyze loop body for optimizations
        optimizations.extend(Self::analyze_loop_body(&for_stmt.body));
        
        LoopPattern {
            loop_type: LoopType::For,
            body: for_stmt.body.clone(),
            condition: for_stmt.condition.clone(),
            initialization: for_stmt.init.clone(),
            step: for_stmt.update.iter().map(|expr_stmt| Stmt::Expression(expr_stmt.clone())).collect(),
            optimization_opportunities: optimizations,
        }
    }
    
    /// Generate optimized loop bytecode (javac genLoop pattern)
    pub fn generate_optimized_loop(pattern: &LoopPattern) -> Vec<u8> {
        let bytecode = Vec::new();
        
        // Check for constant condition optimization
        if let Some(constant_opt) = pattern.optimization_opportunities.iter()
            .find_map(|opt| match opt {
                LoopOptimization::ConstantCondition { always_true } => Some(*always_true),
                _ => None,
            }) {
            
            if !constant_opt {
                // Condition is always false - skip loop entirely
                return bytecode;
            }
            
            // Condition is always true - generate infinite loop
            return Self::generate_infinite_loop(pattern);
        }
        
        // Check for loop unrolling
        if let Some(unroll_opt) = pattern.optimization_opportunities.iter()
            .find_map(|opt| match opt {
                LoopOptimization::LoopUnrolling { iteration_count } => Some(*iteration_count),
                _ => None,
            }) {
            
            return Self::generate_unrolled_loop(pattern, unroll_opt);
        }
        
        // Generate standard loop
        match pattern.loop_type {
            LoopType::While => Self::generate_while_loop(pattern),
            LoopType::For => Self::generate_for_loop(pattern),
            LoopType::Enhanced => Self::generate_enhanced_for_loop(pattern),
        }
    }
    
    /// Generate while loop bytecode (javac genLoop with testFirst=true)
    fn generate_while_loop(pattern: &LoopPattern) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Label for loop start
        let loop_start_label = 1u16;
        let loop_end_label = 2u16;
        
        // Mark loop start
        bytecode.extend_from_slice(&Self::mark_label(loop_start_label));
        
        // Generate condition test (javac testFirst pattern)
        if let Some(ref condition) = pattern.condition {
            bytecode.extend_from_slice(&Self::generate_condition_test(condition, loop_end_label, false));
        }
        
        // Generate loop body
        bytecode.extend_from_slice(&Self::generate_loop_body(&pattern.body));
        
        // Jump back to start
        bytecode.push(opcodes::GOTO);
        bytecode.extend_from_slice(&loop_start_label.to_be_bytes());
        
        // Mark loop end
        bytecode.extend_from_slice(&Self::mark_label(loop_end_label));
        
        bytecode
    }
    

    
    /// Generate for loop bytecode (javac visitForLoop pattern)
    fn generate_for_loop(pattern: &LoopPattern) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Generate initialization
        for init_stmt in &pattern.initialization {
            bytecode.extend_from_slice(&Self::generate_statement(init_stmt));
        }
        
        // Labels
        let loop_start_label = 1u16;
        let loop_end_label = 2u16;
        let continue_label = 3u16;
        
        // Mark loop start
        bytecode.extend_from_slice(&Self::mark_label(loop_start_label));
        
        // Generate condition test
        if let Some(ref condition) = pattern.condition {
            bytecode.extend_from_slice(&Self::generate_condition_test(condition, loop_end_label, false));
        }
        
        // Generate loop body
        bytecode.extend_from_slice(&Self::generate_loop_body(&pattern.body));
        
        // Mark continue point
        bytecode.extend_from_slice(&Self::mark_label(continue_label));
        
        // Generate step statements
        for step_stmt in &pattern.step {
            bytecode.extend_from_slice(&Self::generate_statement(step_stmt));
        }
        
        // Jump back to start
        bytecode.push(opcodes::GOTO);
        bytecode.extend_from_slice(&loop_start_label.to_be_bytes());
        
        // Mark loop end
        bytecode.extend_from_slice(&Self::mark_label(loop_end_label));
        
        bytecode
    }
    
    /// Generate enhanced for loop (for-each) bytecode
    fn generate_enhanced_for_loop(pattern: &LoopPattern) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Enhanced for loop is desugared to iterator pattern
        // for (T item : collection) body
        // becomes:
        // Iterator<T> iter = collection.iterator();
        // while (iter.hasNext()) {
        //     T item = iter.next();
        //     body
        // }
        
        // Get iterator
        bytecode.push(opcodes::INVOKEINTERFACE);
        bytecode.extend_from_slice(&Self::iterator_method_index().to_be_bytes());
        
        // Store iterator in local variable
        bytecode.push(opcodes::ASTORE_1); // Placeholder local variable index
        
        // Loop start
        let loop_start_label = 1u16;
        let loop_end_label = 2u16;
        
        bytecode.extend_from_slice(&Self::mark_label(loop_start_label));
        
        // Load iterator and call hasNext()
        bytecode.push(opcodes::ALOAD_1);
        bytecode.push(opcodes::INVOKEINTERFACE);
        bytecode.extend_from_slice(&Self::has_next_method_index().to_be_bytes());
        
        // If false, exit loop
        bytecode.push(opcodes::IFEQ);
        bytecode.extend_from_slice(&loop_end_label.to_be_bytes());
        
        // Load iterator and call next()
        bytecode.push(opcodes::ALOAD_1);
        bytecode.push(opcodes::INVOKEINTERFACE);
        bytecode.extend_from_slice(&Self::next_method_index().to_be_bytes());
        
        // Store loop variable
        bytecode.push(opcodes::ASTORE_2); // Placeholder local variable index
        
        // Generate loop body
        bytecode.extend_from_slice(&Self::generate_loop_body(&pattern.body));
        
        // Jump back to start
        bytecode.push(opcodes::GOTO);
        bytecode.extend_from_slice(&loop_start_label.to_be_bytes());
        
        // Mark loop end
        bytecode.extend_from_slice(&Self::mark_label(loop_end_label));
        
        bytecode
    }
    
    /// Generate infinite loop (constant true condition)
    fn generate_infinite_loop(pattern: &LoopPattern) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        let loop_start_label = 1u16;
        
        // Mark loop start
        bytecode.extend_from_slice(&Self::mark_label(loop_start_label));
        
        // Generate loop body
        bytecode.extend_from_slice(&Self::generate_loop_body(&pattern.body));
        
        // Unconditional jump back
        bytecode.push(opcodes::GOTO);
        bytecode.extend_from_slice(&loop_start_label.to_be_bytes());
        
        bytecode
    }
    
    /// Generate unrolled loop
    fn generate_unrolled_loop(pattern: &LoopPattern, iteration_count: usize) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Generate initialization once
        for init_stmt in &pattern.initialization {
            bytecode.extend_from_slice(&Self::generate_statement(init_stmt));
        }
        
        // Unroll loop body
        for _i in 0..iteration_count {
            bytecode.extend_from_slice(&Self::generate_loop_body(&pattern.body));
            
            // Generate step statements (except for last iteration)
            if _i < iteration_count - 1 {
                for step_stmt in &pattern.step {
                    bytecode.extend_from_slice(&Self::generate_statement(step_stmt));
                }
            }
        }
        
        bytecode
    }
    
    /// Evaluate constant condition at compile time
    fn evaluate_constant_condition(condition: &Expr) -> Option<bool> {
        match condition {
            Expr::Literal(literal_expr) => {
                match &literal_expr.value {
                    crate::ast::Literal::Boolean(b) => Some(*b),
                    crate::ast::Literal::Integer(i) => Some(*i != 0),
                    _ => None,
                }
            }
            _ => None, // Could be extended for more complex constant expressions
        }
    }
    
    /// Analyze fixed iteration count for loop unrolling
    fn analyze_fixed_iteration_count(for_stmt: &ForStmt) -> Option<usize> {
        // Simple pattern: for (int i = 0; i < N; i++) where N is constant
        if for_stmt.init.len() == 1 && for_stmt.update.len() == 1 {
            // This is a simplified analysis - could be much more sophisticated
            if let Some(ref condition) = for_stmt.condition {
                if let Some(limit) = Self::extract_constant_limit(condition) {
                    return Some(limit);
                }
            }
        }
        None
    }
    
    /// Extract constant limit from condition (simplified)
    fn extract_constant_limit(condition: &Expr) -> Option<usize> {
        // Simplified pattern matching for i < N where N is constant
        match condition {
            Expr::Binary(binary_expr) => {
                if binary_expr.operator == crate::ast::BinaryOp::Lt {
                    if let Expr::Literal(literal_expr) = &*binary_expr.right {
                        if let crate::ast::Literal::Integer(n) = &literal_expr.value {
                            return Some(*n as usize);
                        }
                    }
                }
            }
            _ => {}
        }
        None
    }
    
    /// Analyze loop body for optimization opportunities
    fn analyze_loop_body(body: &Stmt) -> Vec<LoopOptimization> {
        let mut optimizations = Vec::new();
        
        // Check for invariant code motion opportunities
        if let Some(invariant_exprs) = Self::find_loop_invariant_expressions(body) {
            if !invariant_exprs.is_empty() {
                optimizations.push(LoopOptimization::InvariantCodeMotion {
                    movable_expressions: invariant_exprs,
                });
            }
        }
        
        // Check for strength reduction opportunities
        if Self::has_strength_reduction_opportunities(body) {
            optimizations.push(LoopOptimization::StrengthReduction);
        }
        
        optimizations
    }
    
    /// Find expressions that are loop invariant
    fn find_loop_invariant_expressions(_body: &Stmt) -> Option<Vec<Expr>> {
        // Simplified analysis - would need more sophisticated data flow analysis
        // For now, just return empty vector
        Some(Vec::new())
    }
    
    /// Check for strength reduction opportunities
    fn has_strength_reduction_opportunities(_body: &Stmt) -> bool {
        // Simplified check - would analyze for patterns like i*2, i*3, etc.
        false
    }
    
    /// Generate condition test bytecode
    fn generate_condition_test(condition: &Expr, target_label: u16, jump_if_true: bool) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Generate condition evaluation (placeholder)
        bytecode.extend_from_slice(&Self::generate_expression(condition));
        
        // Generate conditional jump
        if jump_if_true {
            bytecode.push(opcodes::IFNE); // Jump if true (non-zero)
        } else {
            bytecode.push(opcodes::IFEQ); // Jump if false (zero)
        }
        bytecode.extend_from_slice(&target_label.to_be_bytes());
        
        bytecode
    }
    
    /// Generate loop body bytecode
    fn generate_loop_body(body: &Stmt) -> Vec<u8> {
        Self::generate_statement(body)
    }
    
    /// Generate statement bytecode (placeholder)
    fn generate_statement(_stmt: &Stmt) -> Vec<u8> {
        // Placeholder - would delegate to main statement generator
        Vec::new()
    }
    
    /// Generate expression bytecode (placeholder)
    fn generate_expression(_expr: &Expr) -> Vec<u8> {
        // Placeholder - would delegate to main expression generator
        vec![opcodes::ICONST_1] // Default to true for testing
    }
    
    /// Mark label (placeholder)
    fn mark_label(_label_id: u16) -> Vec<u8> {
        // Placeholder - would be handled by label management system
        vec![] // Labels don't generate bytecode directly
    }
    
    /// Placeholder method indices
    fn iterator_method_index() -> u16 { 1 }
    fn has_next_method_index() -> u16 { 2 }
    fn next_method_index() -> u16 { 3 }
}

/// Loop complexity analysis (javac-style)
pub struct LoopComplexityAnalyzer;

impl LoopComplexityAnalyzer {
    /// Calculate loop complexity for JSR optimization decisions (javac pattern)
    pub fn calculate_loop_complexity(stmt: &Stmt) -> LoopComplexity {
        let mut complexity = 0;
        
        match stmt {
            Stmt::While(_) => complexity += 1,
            Stmt::For(_) => complexity += 1,
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    complexity += Self::calculate_statement_complexity(stmt);
                }
            }
            _ => complexity += Self::calculate_statement_complexity(stmt),
        }
        
        LoopComplexity {
            total_complexity: complexity,
            nested_loop_count: Self::count_nested_loops(stmt),
            has_try_finally: Self::has_try_finally_blocks(stmt),
            recommended_optimization: Self::recommend_optimization(complexity),
        }
    }
    
    /// Calculate complexity of individual statement
    fn calculate_statement_complexity(stmt: &Stmt) -> usize {
        match stmt {
            Stmt::If(_) => 2,
            Stmt::Switch(_) => 5,
            Stmt::Try(_) => 6, // Higher complexity for exception handling
            Stmt::Synchronized(_) => 6,
            Stmt::While(_) | Stmt::For(_) => 1,
            Stmt::Break(_) | Stmt::Continue(_) => 1,
            Stmt::Return(_) => 1,
            Stmt::Throw(_) => 1,
            _ => 1,
        }
    }
    
    /// Count nested loops
    fn count_nested_loops(stmt: &Stmt) -> usize {
        match stmt {
            Stmt::While(while_stmt) => 1 + Self::count_nested_loops(&while_stmt.body),
            Stmt::For(for_stmt) => 1 + Self::count_nested_loops(&for_stmt.body),
            Stmt::Block(block) => {
                block.statements.iter()
                    .map(|s| Self::count_nested_loops(s))
                    .sum()
            }
            _ => 0,
        }
    }
    
    /// Check for try-finally blocks
    fn has_try_finally_blocks(_stmt: &Stmt) -> bool {
        // Simplified check - would need to traverse AST
        false
    }
    
    /// Recommend optimization based on complexity
    fn recommend_optimization(complexity: usize) -> OptimizationRecommendation {
        match complexity {
            0..=5 => OptimizationRecommendation::FullOptimization,
            6..=15 => OptimizationRecommendation::ModerateOptimization,
            16..=30 => OptimizationRecommendation::ConservativeOptimization,
            _ => OptimizationRecommendation::MinimalOptimization,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoopComplexity {
    pub total_complexity: usize,
    pub nested_loop_count: usize,
    pub has_try_finally: bool,
    pub recommended_optimization: OptimizationRecommendation,
}

#[derive(Debug, Clone)]
pub enum OptimizationRecommendation {
    FullOptimization,
    ModerateOptimization,
    ConservativeOptimization,
    MinimalOptimization,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, LiteralExpr, Literal, BinaryExpr, BinaryOp, IdentifierExpr};
    
    #[test]
    fn test_constant_condition_evaluation() {
        // Test true condition
        let true_condition = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: Span::from_to(0, 0, 0, 0),
        });
        
        assert_eq!(LoopOptimizer::evaluate_constant_condition(&true_condition), Some(true));
        
        // Test false condition
        let false_condition = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(false),
            span: Span::from_to(0, 0, 0, 0),
        });
        
        assert_eq!(LoopOptimizer::evaluate_constant_condition(&false_condition), Some(false));
    }
    
    #[test]
    fn test_fixed_iteration_analysis() {
        // Create a simple for loop: for (int i = 0; i < 5; i++)
        let condition = Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Identifier(IdentifierExpr {
                name: "i".to_string(),
                span: Span::from_to(0, 0, 0, 0),
            })),
            operator: BinaryOp::Lt,
            right: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Integer(5),
                span: Span::from_to(0, 0, 0, 0),
            })),
            span: Span::from_to(0, 0, 0, 0),
        });
        
        assert_eq!(LoopOptimizer::extract_constant_limit(&condition), Some(5));
    }
    
    #[test]
    fn test_loop_complexity_analysis() {
        let simple_block = crate::ast::Block {
            statements: vec![
                Stmt::Expression(crate::ast::ExprStmt {
                    expr: Expr::Literal(LiteralExpr {
                        value: Literal::Integer(1),
                        span: Span::from_to(0, 0, 0, 0),
                    }),
                    span: Span::from_to(0, 0, 0, 0),
                }),
            ],
            span: Span::from_to(0, 0, 0, 0),
        };
        
        let complexity = LoopComplexityAnalyzer::calculate_loop_complexity(&Stmt::Block(simple_block));
        
        assert_eq!(complexity.total_complexity, 1);
        assert_eq!(complexity.nested_loop_count, 0);
        assert!(matches!(complexity.recommended_optimization, OptimizationRecommendation::FullOptimization));
    }
}
