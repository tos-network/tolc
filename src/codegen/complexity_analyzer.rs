use crate::ast::*;

/// Complexity analyzer for determining whether to use fatcode (javac-style)
/// 
/// This analyzer calculates the complexity of a method to determine if it needs
/// wide jumps (fatcode mode). Based on javac's complexity calculation.
pub struct ComplexityAnalyzer {
    complexity: u32,
}

impl ComplexityAnalyzer {
    pub fn new() -> Self {
        Self { complexity: 0 }
    }
    
    /// Analyze the complexity of a statement
    pub fn analyze_complexity(stmt: &Stmt) -> u32 {
        let mut analyzer = ComplexityAnalyzer::new();
        analyzer.visit_stmt(stmt);
        analyzer.complexity
    }
    
    /// Visit a block and update complexity
    fn visit_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.visit_stmt(stmt);
        }
    }
    
    /// Visit a statement and update complexity
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    self.visit_stmt(stmt);
                }
            }
            Stmt::If(if_stmt) => {
                self.complexity += 2; // javac adds 2 for if statements
                self.visit_expr(&if_stmt.condition);
                self.visit_stmt(&if_stmt.then_branch);
                if let Some(else_branch) = &if_stmt.else_branch {
                    self.visit_stmt(else_branch);
                }
            }
            Stmt::While(while_stmt) => {
                self.complexity += 1; // javac adds 1 for loops
                self.visit_expr(&while_stmt.condition);
                self.visit_stmt(&while_stmt.body);
            }
            Stmt::For(for_stmt) => {
                self.complexity += 1; // javac adds 1 for loops
                // Visit init statements
                for init_stmt in &for_stmt.init {
                    self.visit_stmt(init_stmt);
                }
                // Visit condition
                if let Some(condition) = &for_stmt.condition {
                    self.visit_expr(condition);
                }
                // Visit update expressions
                for update_expr in &for_stmt.update {
                    self.visit_expr(&update_expr.expr);
                }
                self.visit_stmt(&for_stmt.body);
            }
            Stmt::Switch(switch_stmt) => {
                self.complexity += 1; // javac adds 1 for switch
                self.visit_expr(&switch_stmt.expression);
                for case in &switch_stmt.cases {
                    for stmt in &case.statements {
                        self.visit_stmt(stmt);
                    }
                }
            }
            Stmt::Try(try_stmt) => {
                self.complexity += 1; // javac adds 1 for try
                self.visit_block(&try_stmt.try_block);
                for catch in &try_stmt.catch_clauses {
                    self.visit_block(&catch.block);
                }
                if let Some(finally_block) = &try_stmt.finally_block {
                    self.visit_block(finally_block);
                }
            }
            Stmt::Synchronized(sync_stmt) => {
                self.complexity += 1; // javac adds 1 for synchronized
                self.visit_expr(&sync_stmt.lock);
                self.visit_block(&sync_stmt.body);
            }
            Stmt::Expression(expr_stmt) => {
                self.visit_expr(&expr_stmt.expr);
            }
            Stmt::Declaration(var_decl) => {
                for variable in &var_decl.variables {
                    if let Some(init_expr) = &variable.initializer {
                        self.visit_expr(init_expr);
                    }
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(expr) = &return_stmt.value {
                    self.visit_expr(expr);
                }
            }
            Stmt::Throw(throw_stmt) => {
                self.visit_expr(&throw_stmt.expr);
            }
            Stmt::Assert(assert_stmt) => {
                self.visit_expr(&assert_stmt.condition);
                if let Some(message) = &assert_stmt.message {
                    self.visit_expr(message);
                }
            }
            Stmt::Labeled(labeled_stmt) => {
                self.visit_stmt(&labeled_stmt.statement);
            }
            // Simple statements don't add complexity
            Stmt::Break(_) | Stmt::Continue(_) | Stmt::Empty => {}
            Stmt::TypeDecl(_) => {
                // Type declarations don't add runtime complexity
            }
        }
    }
    
    /// Visit an expression and update complexity
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary(binary) => {
                // Conditional operators add complexity
                match binary.operator {
                    BinaryOp::And | BinaryOp::Or => {
                        self.complexity += 1;
                    }
                    _ => {}
                }
                self.visit_expr(&binary.left);
                self.visit_expr(&binary.right);
            }
            Expr::Unary(unary) => {
                self.visit_expr(&unary.operand);
            }
            Expr::Assignment(assignment) => {
                self.visit_expr(&assignment.target);
                self.visit_expr(&assignment.value);
            }
            Expr::MethodCall(method_call) => {
                if let Some(target) = &method_call.target {
                    self.visit_expr(target);
                }
                for arg in &method_call.arguments {
                    self.visit_expr(arg);
                }
            }
            Expr::FieldAccess(field_access) => {
                if let Some(target) = &field_access.target {
                    self.visit_expr(target);
                }
            }
            Expr::ArrayAccess(array_access) => {
                self.visit_expr(&array_access.array);
                self.visit_expr(&array_access.index);
            }
            Expr::Cast(cast) => {
                self.visit_expr(&cast.expr);
            }
            Expr::InstanceOf(instanceof) => {
                self.visit_expr(&instanceof.expr);
            }
            Expr::Conditional(conditional) => {
                self.complexity += 1; // Ternary operator adds complexity
                self.visit_expr(&conditional.condition);
                self.visit_expr(&conditional.then_expr);
                self.visit_expr(&conditional.else_expr);
            }
            Expr::New(new_expr) => {
                for arg in &new_expr.arguments {
                    self.visit_expr(arg);
                }
                // Note: NewExpr doesn't have array_dimensions field in our AST
                // Array creation is handled differently
            }
            // Simple expressions don't add complexity
            Expr::Literal(_) | Expr::Identifier(_) => {}
            Expr::Parenthesized(expr) => {
                self.visit_expr(expr);
            }
            Expr::ArrayInitializer(exprs) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
        }
    }
    
    /// Determine if fatcode should be used based on complexity
    /// javac uses fatcode when complexity exceeds certain thresholds
    pub fn should_use_fatcode(complexity: u32) -> bool {
        // javac's threshold is around 100-200 for complexity
        // We use a conservative threshold
        complexity > 100
    }
    
    /// Calculate method size estimate for fatcode decision
    pub fn estimate_method_size(stmt: &Stmt) -> u32 {
        let mut analyzer = ComplexityAnalyzer::new();
        analyzer.estimate_size(stmt);
        analyzer.complexity
    }
    
    /// Estimate bytecode size of a statement
    fn estimate_size(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    self.estimate_size(stmt);
                }
            }
            Stmt::If(_) => {
                self.complexity += 10; // Estimated bytecode size for if
            }
            Stmt::While(_) | Stmt::For(_) => {
                self.complexity += 15; // Estimated bytecode size for loops
            }
            Stmt::Switch(_) => {
                self.complexity += 20; // Estimated bytecode size for switch
            }
            Stmt::Try(_) => {
                self.complexity += 25; // Estimated bytecode size for try-catch
            }
            Stmt::Synchronized(_) => {
                self.complexity += 15; // Estimated bytecode size for synchronized
            }
            Stmt::Expression(_) => {
                self.complexity += 3; // Estimated bytecode size for expression
            }
            Stmt::Declaration(_) => {
                self.complexity += 2; // Estimated bytecode size for variable declaration
            }
            Stmt::Return(_) => {
                self.complexity += 2; // Estimated bytecode size for return
            }
            Stmt::Throw(_) => {
                self.complexity += 3; // Estimated bytecode size for throw
            }
            Stmt::Assert(_) => {
                self.complexity += 5; // Estimated bytecode size for assert
            }
            Stmt::Labeled(labeled) => {
                self.estimate_size(&labeled.statement);
            }
            Stmt::Break(_) | Stmt::Continue(_) => {
                self.complexity += 3; // Estimated bytecode size for jump
            }
            Stmt::Empty => {
                // No bytecode generated
            }
            Stmt::TypeDecl(_) => {
                // Type declarations don't generate runtime bytecode
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location};
    
    #[test]
    fn test_simple_complexity() {
        let stmt = Stmt::Empty;
        let complexity = ComplexityAnalyzer::analyze_complexity(&stmt);
        assert_eq!(complexity, 0);
    }
    
    #[test]
    fn test_if_complexity() {
        let if_stmt = IfStmt {
            condition: Expr::Literal(LiteralExpr {
                value: Literal::Boolean(true),
                span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
            }),
            then_branch: Box::new(Stmt::Empty),
            else_branch: None,
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        };
        let stmt = Stmt::If(if_stmt);
        let complexity = ComplexityAnalyzer::analyze_complexity(&stmt);
        assert_eq!(complexity, 2); // javac adds 2 for if statements
    }
    
    #[test]
    fn test_loop_complexity() {
        let while_stmt = WhileStmt {
            condition: Expr::Literal(LiteralExpr {
                value: Literal::Boolean(true),
                span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
            }),
            body: Box::new(Stmt::Empty),
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        };
        let stmt = Stmt::While(while_stmt);
        let complexity = ComplexityAnalyzer::analyze_complexity(&stmt);
        assert_eq!(complexity, 1); // javac adds 1 for loops
    }
    
    #[test]
    fn test_fatcode_threshold() {
        assert!(!ComplexityAnalyzer::should_use_fatcode(50));
        assert!(ComplexityAnalyzer::should_use_fatcode(150));
    }
}
