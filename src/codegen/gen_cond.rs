use crate::ast::*;
use crate::codegen::cond_item::CondItem;
use crate::codegen::chain::Chain;
use crate::codegen::opcodes;
use crate::error::Result;

/// javac-style genCond implementation for advanced conditional generation
/// 
/// This module implements javac's sophisticated conditional expression generation
/// with short-circuit evaluation and jump optimization.
pub struct GenCond;

impl GenCond {
    /// Generate optimized conditional code (javac genCond equivalent)
    /// 
    /// This method analyzes conditional expressions and generates efficient
    /// bytecode with proper short-circuit evaluation and jump chains.
    pub fn gen_cond(expr: &Expr, mark_branches: bool) -> Result<CondItem> {
        // Skip parentheses (javac TreeInfo.skipParens equivalent)
        let inner_expr = Self::skip_parentheses(expr);
        
        match inner_expr {
            // Handle nested conditional expressions (ternary operators)
            Expr::Conditional(conditional) => {
                Self::gen_conditional_cond(conditional, mark_branches)
            }
            // Handle binary logical operations with short-circuit evaluation
            Expr::Binary(binary) => {
                match binary.operator {
                    BinaryOp::And => Self::gen_and_cond(binary, mark_branches),
                    BinaryOp::Or => Self::gen_or_cond(binary, mark_branches),
                    _ => Self::gen_comparison_cond(binary, mark_branches),
                }
            }
            // Handle unary NOT operations
            Expr::Unary(unary) => {
                match unary.operator {
                    UnaryOp::Not => {
                        let operand_cond = Self::gen_cond(&unary.operand, mark_branches)?;
                        Ok(operand_cond.negate())
                    }
                    _ => Self::gen_expression_cond(inner_expr, mark_branches),
                }
            }
            // Handle boolean literals
            Expr::Literal(literal) => {
                match &literal.value {
                    Literal::Boolean(true) => {
                        Ok(CondItem::new(opcodes::GOTO, None, None))
                    }
                    Literal::Boolean(false) => {
                        Ok(CondItem::new(opcodes::NOP, None, None))
                    }
                    _ => Self::gen_expression_cond(inner_expr, mark_branches),
                }
            }
            // Default case: treat as boolean expression
            _ => Self::gen_expression_cond(inner_expr, mark_branches),
        }
    }
    
    /// Generate conditional code for ternary expressions (javac-style)
    fn gen_conditional_cond(conditional: &ConditionalExpr, mark_branches: bool) -> Result<CondItem> {
        // Generate condition
        let cond = Self::gen_cond(&*conditional.condition, true)?;
        
        // Optimize constant conditions
        if cond.is_true() {
            // Condition is always true, use true branch
            let mut result = Self::gen_cond(&*conditional.then_expr, mark_branches)?;
            if mark_branches {
                result.tree = Some((*conditional.then_expr).clone());
            }
            return Ok(result);
        }
        
        if cond.is_false() {
            // Condition is always false, use false branch
            let mut result = Self::gen_cond(&*conditional.else_expr, mark_branches)?;
            if mark_branches {
                result.tree = Some((*conditional.else_expr).clone());
            }
            return Ok(result);
        }
        
        // Complex conditional: generate jump chains (javac-style)
        let false_chain = cond.jump_false();
        let true_chain = cond.jump_true();
        
        // Generate true branch
        let mut first = Self::gen_cond(&*conditional.then_expr, mark_branches)?;
        if mark_branches {
            first.tree = Some((*conditional.then_expr).clone());
        }
        
        let first_false_jumps = first.jump_false();
        let first_true_jumps = first.jump_true();
        
        // Generate false branch
        let mut second = Self::gen_cond(&*conditional.else_expr, mark_branches)?;
        if mark_branches {
            second.tree = Some((*conditional.else_expr).clone());
        }
        
        // Combine jump chains (javac mergeChains equivalent)
        let combined_true_jumps = Chain::merge_option(first_true_jumps, second.true_jumps);
        let combined_false_jumps = Chain::merge_option(first_false_jumps, second.false_jumps);
        
        let mut result = CondItem::new(second.opcode, combined_true_jumps, combined_false_jumps);
        if mark_branches {
            result.tree = Some((*conditional.else_expr).clone());
        }
        
        Ok(result)
    }
    
    /// Generate conditional code for AND operations with short-circuit evaluation
    fn gen_and_cond(binary: &BinaryExpr, mark_branches: bool) -> Result<CondItem> {
        // Short-circuit AND: left && right
        let left_cond = Self::gen_cond(&*binary.left, mark_branches)?;
        
        // If left is always false, entire expression is false
        if left_cond.is_false() {
            return Ok(left_cond);
        }
        
        let right_cond = Self::gen_cond(&*binary.right, mark_branches)?;
        
        // If left is always true, result is right condition
        if left_cond.is_true() {
            return Ok(right_cond);
        }
        
        // Combine conditions: true only if both are true
        // False if either is false (short-circuit)
        let combined_false_jumps = Chain::merge_option(left_cond.false_jumps, right_cond.false_jumps);
        
        Ok(CondItem::new(
            right_cond.opcode,
            right_cond.true_jumps,
            combined_false_jumps,
        ))
    }
    
    /// Generate conditional code for OR operations with short-circuit evaluation
    fn gen_or_cond(binary: &BinaryExpr, mark_branches: bool) -> Result<CondItem> {
        // Short-circuit OR: left || right
        let left_cond = Self::gen_cond(&*binary.left, mark_branches)?;
        
        // If left is always true, entire expression is true
        if left_cond.is_true() {
            return Ok(left_cond);
        }
        
        let right_cond = Self::gen_cond(&*binary.right, mark_branches)?;
        
        // If left is always false, result is right condition
        if left_cond.is_false() {
            return Ok(right_cond);
        }
        
        // Combine conditions: false only if both are false
        // True if either is true (short-circuit)
        let combined_true_jumps = Chain::merge_option(left_cond.true_jumps, right_cond.true_jumps);
        
        Ok(CondItem::new(
            right_cond.opcode,
            combined_true_jumps,
            right_cond.false_jumps,
        ))
    }
    
    /// Generate conditional code for comparison operations
    fn gen_comparison_cond(binary: &BinaryExpr, _mark_branches: bool) -> Result<CondItem> {
        let opcode = match binary.operator {
            BinaryOp::Eq => opcodes::IF_ICMPEQ,
            BinaryOp::Ne => opcodes::IF_ICMPNE,
            BinaryOp::Lt => opcodes::IF_ICMPLT,
            BinaryOp::Le => opcodes::IF_ICMPLE,
            BinaryOp::Gt => opcodes::IF_ICMPGT,
            BinaryOp::Ge => opcodes::IF_ICMPGE,
            _ => opcodes::IFNE, // Default for non-comparison operations
        };
        
        Ok(CondItem::new(opcode, None, None))
    }
    
    /// Generate conditional code for general expressions
    fn gen_expression_cond(_expr: &Expr, _mark_branches: bool) -> Result<CondItem> {
        // For general expressions, generate as boolean test
        // The expression will be evaluated and tested with IFNE (if not equal to zero)
        Ok(CondItem::new(opcodes::IFNE, None, None))
    }
    
    /// Skip parentheses around expressions (javac TreeInfo.skipParens equivalent)
    fn skip_parentheses(expr: &Expr) -> &Expr {
        match expr {
            Expr::Parenthesized(inner) => Self::skip_parentheses(inner),
            _ => expr,
        }
    }
    
    /// Generate bytecode for conditional expression with proper jump handling
    pub fn generate_conditional_bytecode(
        expr: &Expr,
        true_label: &str,
        false_label: &str,
        mark_branches: bool,
    ) -> Result<Vec<u8>> {
        let cond_item = Self::gen_cond(expr, mark_branches)?;
        
        let mut bytecode = Vec::new();
        
        // Handle constant conditions
        if cond_item.is_true() {
            // Always jump to true label
            bytecode.extend_from_slice(&[opcodes::GOTO, 0, 0]); // Offset will be patched
            return Ok(bytecode);
        }
        
        if cond_item.is_false() {
            // Always jump to false label
            bytecode.extend_from_slice(&[opcodes::GOTO, 0, 0]); // Offset will be patched
            return Ok(bytecode);
        }
        
        // Generate conditional jump
        bytecode.push(cond_item.opcode);
        bytecode.extend_from_slice(&[0, 0]); // Offset will be patched
        
        // If condition fails, jump to false label
        if cond_item.opcode != opcodes::GOTO {
            bytecode.extend_from_slice(&[opcodes::GOTO, 0, 0]); // Jump to false label
        }
        
        Ok(bytecode)
    }
}

/// Advanced conditional expression analysis and optimization
pub struct ConditionalAnalyzer;

impl ConditionalAnalyzer {
    /// Analyze conditional expression complexity (javac-style)
    pub fn analyze_complexity(expr: &Expr) -> u32 {
        match expr {
            Expr::Binary(binary) => {
                match binary.operator {
                    BinaryOp::And | BinaryOp::Or => {
                        // Logical operations add complexity due to short-circuit evaluation
                        2 + Self::analyze_complexity(&binary.left) + Self::analyze_complexity(&binary.right)
                    }
                    _ => {
                        // Simple comparisons
                        1 + Self::analyze_complexity(&binary.left) + Self::analyze_complexity(&binary.right)
                    }
                }
            }
            Expr::Unary(unary) => {
                1 + Self::analyze_complexity(&unary.operand)
            }
            Expr::Conditional(conditional) => {
                // Ternary operators add significant complexity
                3 + Self::analyze_complexity(&conditional.condition)
                  + Self::analyze_complexity(&conditional.then_expr)
                  + Self::analyze_complexity(&conditional.else_expr)
            }
            Expr::Parenthesized(inner) => {
                Self::analyze_complexity(inner)
            }
            _ => 1, // Base complexity for simple expressions
        }
    }
    
    /// Determine if expression benefits from short-circuit evaluation
    pub fn needs_short_circuit(expr: &Expr) -> bool {
        match expr {
            Expr::Binary(binary) => {
                matches!(binary.operator, BinaryOp::And | BinaryOp::Or)
            }
            Expr::Conditional(_) => true,
            Expr::Parenthesized(inner) => Self::needs_short_circuit(inner),
            _ => false,
        }
    }
    
    /// Estimate bytecode size for conditional expression
    pub fn estimate_bytecode_size(expr: &Expr) -> u32 {
        match expr {
            Expr::Binary(binary) => {
                match binary.operator {
                    BinaryOp::And | BinaryOp::Or => {
                        // Short-circuit evaluation requires more bytecode
                        8 + Self::estimate_bytecode_size(&binary.left) + Self::estimate_bytecode_size(&binary.right)
                    }
                    _ => {
                        // Simple comparisons
                        3 + Self::estimate_bytecode_size(&binary.left) + Self::estimate_bytecode_size(&binary.right)
                    }
                }
            }
            Expr::Unary(unary) => {
                2 + Self::estimate_bytecode_size(&unary.operand)
            }
            Expr::Conditional(conditional) => {
                // Ternary operators require jump instructions
                10 + Self::estimate_bytecode_size(&conditional.condition)
                   + Self::estimate_bytecode_size(&conditional.then_expr)
                   + Self::estimate_bytecode_size(&conditional.else_expr)
            }
            Expr::Literal(_) => 1,
            Expr::Identifier(_) => 2,
            _ => 3, // Default estimate
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location};
    
    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }
    
    #[test]
    fn test_boolean_literal_cond() {
        let true_expr = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&true_expr, false).unwrap();
        assert!(cond.is_true());
        
        let false_expr = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(false),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&false_expr, false).unwrap();
        assert!(cond.is_false());
    }
    
    #[test]
    fn test_not_operation() {
        let true_expr = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: create_span(),
        });
        
        let not_expr = Expr::Unary(UnaryExpr {
            operator: UnaryOp::Not,
            operand: Box::new(true_expr),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&not_expr, false).unwrap();
        assert!(cond.is_false()); // NOT true = false
    }
    
    #[test]
    fn test_comparison_operations() {
        let left = Expr::Literal(LiteralExpr {
            value: Literal::Integer(1),
            span: create_span(),
        });
        
        let right = Expr::Literal(LiteralExpr {
            value: Literal::Integer(2),
            span: create_span(),
        });
        
        let eq_expr = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: BinaryOp::Eq,
            right: Box::new(right),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&eq_expr, false).unwrap();
        assert_eq!(cond.opcode, opcodes::IF_ICMPEQ);
    }
    
    #[test]
    fn test_complexity_analysis() {
        let simple_expr = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: create_span(),
        });
        
        assert_eq!(ConditionalAnalyzer::analyze_complexity(&simple_expr), 1);
        
        let complex_expr = Expr::Binary(BinaryExpr {
            left: Box::new(simple_expr.clone()),
            operator: BinaryOp::And,
            right: Box::new(simple_expr),
            span: create_span(),
        });
        
        assert_eq!(ConditionalAnalyzer::analyze_complexity(&complex_expr), 4); // 2 + 1 + 1
    }
    
    #[test]
    fn test_short_circuit_detection() {
        let and_expr = Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Boolean(true),
                span: create_span(),
            })),
            operator: BinaryOp::And,
            right: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Boolean(false),
                span: create_span(),
            })),
            span: create_span(),
        });
        
        assert!(ConditionalAnalyzer::needs_short_circuit(&and_expr));
        
        let eq_expr = Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Integer(1),
                span: create_span(),
            })),
            operator: BinaryOp::Eq,
            right: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Integer(2),
                span: create_span(),
            })),
            span: create_span(),
        });
        
        assert!(!ConditionalAnalyzer::needs_short_circuit(&eq_expr));
    }
}
