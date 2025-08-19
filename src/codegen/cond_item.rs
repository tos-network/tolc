use crate::codegen::chain::{Chain, StackState};
use crate::codegen::opcodes;
use crate::ast::{Expr, BinaryOp};

/// Enhanced CondItem for advanced conditional expression optimization
/// 
/// This represents a conditional expression that can be efficiently compiled
/// to bytecode using short-circuit evaluation and jump chains.
#[derive(Debug, Clone)]
pub struct CondItem {
    /// The jump opcode to use for true branches
    pub opcode: u8,
    /// Chain of jumps that are taken when condition is true
    pub true_jumps: Option<Chain>,
    /// Chain of jumps that are taken when condition is false
    pub false_jumps: Option<Chain>,
    /// The original AST node (for debugging/analysis)
    pub tree: Option<Expr>,
}

impl CondItem {
    /// Create a new CondItem
    pub fn new(opcode: u8, true_jumps: Option<Chain>, false_jumps: Option<Chain>) -> Self {
        Self {
            opcode,
            true_jumps,
            false_jumps,
            tree: None,
        }
    }
    
    /// Create a CondItem with associated AST node
    pub fn with_tree(opcode: u8, true_jumps: Option<Chain>, false_jumps: Option<Chain>, tree: Expr) -> Self {
        Self {
            opcode,
            true_jumps,
            false_jumps,
            tree: Some(tree),
        }
    }
    
    /// Check if this condition is always true
    pub fn is_true(&self) -> bool {
        self.false_jumps.is_none() && self.opcode == opcodes::GOTO
    }
    
    /// Check if this condition is always false
    pub fn is_false(&self) -> bool {
        self.true_jumps.is_none() && self.opcode == opcodes::NOP // Using NOP as "dontgoto"
    }
    
    /// Negate this condition (javac-style)
    pub fn negate(self) -> Self {
        Self {
            opcode: negate_opcode(self.opcode),
            true_jumps: self.false_jumps,
            false_jumps: self.true_jumps,
            tree: self.tree,
        }
    }
    
    /// Create a jump chain for true condition
    pub fn jump_true(&self) -> Option<Chain> {
        if let Some(ref true_chain) = self.true_jumps {
            // Merge existing true jumps with a new conditional jump
            let new_chain = Chain::new(0, None, StackState { depth: 0, max_depth: 0 });
            Some(Chain::merge(Some(Box::new(true_chain.clone())), Some(Box::new(new_chain))).map(|boxed| *boxed).unwrap_or_else(|| true_chain.clone()))
        } else if self.opcode != opcodes::NOP {
            // Create new jump chain
            Some(Chain::new(0, None, StackState { depth: 0, max_depth: 0 }))
        } else {
            None
        }
    }
    
    /// Create a jump chain for false condition
    pub fn jump_false(&self) -> Option<Chain> {
        if let Some(ref false_chain) = self.false_jumps {
            // Merge existing false jumps with a new negated conditional jump
            let new_chain = Chain::new(0, None, StackState { depth: 0, max_depth: 0 });
            Some(Chain::merge(Some(Box::new(false_chain.clone())), Some(Box::new(new_chain))).map(|boxed| *boxed).unwrap_or_else(|| false_chain.clone()))
        } else if self.opcode != opcodes::GOTO {
            // Create new negated jump chain
            Some(Chain::new(0, None, StackState { depth: 0, max_depth: 0 }))
        } else {
            None
        }
    }
    
    /// Convert this CondItem to a boolean value on stack (javac-style load)
    pub fn to_boolean_bytecode(&self) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // If condition is always true
        if self.is_true() {
            bytecode.push(opcodes::ICONST_1);
            return bytecode;
        }
        
        // If condition is always false
        if self.is_false() {
            bytecode.push(opcodes::ICONST_0);
            return bytecode;
        }
        
        // Generate conditional load pattern (javac-style)
        // This would need to be integrated with the actual bytecode builder
        // For now, we return the pattern that should be generated
        
        // Pattern: 
        // condition_check
        // ifeq false_label
        // iconst_1
        // goto end_label
        // false_label:
        // iconst_0
        // end_label:
        
        bytecode.extend_from_slice(&[
            self.opcode,
            0, 7, // Jump offset to false_label (will be patched)
            opcodes::ICONST_1,
            opcodes::GOTO,
            0, 4, // Jump offset to end_label (will be patched)
            opcodes::ICONST_0,
            // end_label (implicit)
        ]);
        
        bytecode
    }
}

/// Negate a conditional opcode (javac-style)
pub fn negate_opcode(opcode: u8) -> u8 {
    match opcode {
        opcodes::IFEQ => opcodes::IFNE,
        opcodes::IFNE => opcodes::IFEQ,
        opcodes::IFLT => opcodes::IFGE,
        opcodes::IFGE => opcodes::IFLT,
        opcodes::IFGT => opcodes::IFLE,
        opcodes::IFLE => opcodes::IFGT,
        opcodes::IF_ICMPEQ => opcodes::IF_ICMPNE,
        opcodes::IF_ICMPNE => opcodes::IF_ICMPEQ,
        opcodes::IF_ICMPLT => opcodes::IF_ICMPGE,
        opcodes::IF_ICMPGE => opcodes::IF_ICMPLT,
        opcodes::IF_ICMPGT => opcodes::IF_ICMPLE,
        opcodes::IF_ICMPLE => opcodes::IF_ICMPGT,
        opcodes::IF_ACMPEQ => opcodes::IF_ACMPNE,
        opcodes::IF_ACMPNE => opcodes::IF_ACMPEQ,
        opcodes::IFNULL => opcodes::IFNONNULL,
        opcodes::IFNONNULL => opcodes::IFNULL,
        opcodes::GOTO => opcodes::NOP, // "dontgoto" - never jump
        opcodes::NOP => opcodes::GOTO, // "dontgoto" negated becomes always jump
        _ => opcode, // Unknown opcode, return as-is
    }
}

/// Advanced conditional expression optimizer (javac-style)
pub struct CondItemOptimizer;

impl CondItemOptimizer {
    /// Analyze a conditional expression and create optimized CondItem (javac genCond style)
    pub fn analyze_condition(expr: &Expr) -> CondItem {
        match expr {
            // Handle binary logical operations with short-circuit evaluation
            Expr::Binary(binary) => {
                match binary.operator {
                    BinaryOp::And => {
                        // Short-circuit AND: left && right
                        let left_cond = Self::analyze_condition(&binary.left);
                        let right_cond = Self::analyze_condition(&binary.right);
                        
                        // If left is always false, result is false
                        if left_cond.is_false() {
                            return left_cond;
                        }
                        
                        // If left is always true, result is right
                        if left_cond.is_true() {
                            return right_cond;
                        }
                        
                        // Combine conditions: true only if both are true
                        CondItem::new(
                            opcodes::IFNE, // Jump if non-zero (true)
                            right_cond.true_jumps,
                            Chain::merge_option(left_cond.false_jumps, right_cond.false_jumps),
                        )
                    }
                    BinaryOp::Or => {
                        // Short-circuit OR: left || right
                        let left_cond = Self::analyze_condition(&binary.left);
                        let right_cond = Self::analyze_condition(&binary.right);
                        
                        // If left is always true, result is true
                        if left_cond.is_true() {
                            return left_cond;
                        }
                        
                        // If left is always false, result is right
                        if left_cond.is_false() {
                            return right_cond;
                        }
                        
                        // Combine conditions: false only if both are false
                        CondItem::new(
                            opcodes::IFEQ, // Jump if zero (false)
                            Chain::merge_option(left_cond.true_jumps, right_cond.true_jumps),
                            right_cond.false_jumps,
                        )
                    }
                    BinaryOp::Eq => {
                        CondItem::new(opcodes::IF_ICMPEQ, None, None)
                    }
                    BinaryOp::Ne => {
                        CondItem::new(opcodes::IF_ICMPNE, None, None)
                    }
                    BinaryOp::Lt => {
                        CondItem::new(opcodes::IF_ICMPLT, None, None)
                    }
                    BinaryOp::Le => {
                        CondItem::new(opcodes::IF_ICMPLE, None, None)
                    }
                    BinaryOp::Gt => {
                        CondItem::new(opcodes::IF_ICMPGT, None, None)
                    }
                    BinaryOp::Ge => {
                        CondItem::new(opcodes::IF_ICMPGE, None, None)
                    }
                    _ => {
                        // For non-comparison operations, generate as boolean expression
                        CondItem::new(opcodes::IFNE, None, None)
                    }
                }
            }
            Expr::Unary(unary) => {
                match unary.operator {
                    crate::ast::UnaryOp::Not => {
                        // Logical NOT: negate the condition
                        Self::analyze_condition(&unary.operand).negate()
                    }
                    _ => {
                        // Other unary operations treated as boolean expressions
                        CondItem::new(opcodes::IFNE, None, None)
                    }
                }
            }
            Expr::Literal(literal) => {
                match &literal.value {
                    crate::ast::Literal::Boolean(true) => {
                        // Always true condition
                        CondItem::new(opcodes::GOTO, None, None)
                    }
                    crate::ast::Literal::Boolean(false) => {
                        // Always false condition
                        CondItem::new(opcodes::NOP, None, None)
                    }
                    _ => {
                        // Non-boolean literals treated as truthy
                        CondItem::new(opcodes::IFNE, None, None)
                    }
                }
            }
            Expr::Conditional(conditional) => {
                // Nested conditional: optimize recursively (javac-style)
                let cond = Self::analyze_condition(&conditional.condition);
                let true_part = Self::analyze_condition(&conditional.then_expr);
                let false_part = Self::analyze_condition(&conditional.else_expr);
                
                // If condition is constant, return the appropriate branch
                if cond.is_true() {
                    return true_part;
                }
                if cond.is_false() {
                    return false_part;
                }
                
                // Complex conditional combining (javac-style)
                CondItem::new(
                    true_part.opcode,
                    Chain::merge_option(true_part.true_jumps, false_part.true_jumps),
                    Chain::merge_option(true_part.false_jumps, false_part.false_jumps),
                )
            }
            _ => {
                // Default: treat as boolean expression that needs to be tested
                CondItem::new(opcodes::IFNE, None, None)
            }
        }
    }
    
    /// Optimize a conditional expression for efficient bytecode generation
    pub fn optimize_conditional(expr: &Expr) -> ConditionalOptimization {
        let cond_item = Self::analyze_condition(expr);
        
        if cond_item.is_true() {
            ConditionalOptimization::AlwaysTrue
        } else if cond_item.is_false() {
            ConditionalOptimization::AlwaysFalse
        } else {
            ConditionalOptimization::Conditional {
                opcode: cond_item.opcode,
                short_circuit: matches!(expr, Expr::Binary(binary) 
                    if matches!(binary.operator, BinaryOp::And | BinaryOp::Or)),
            }
        }
    }
}

/// Result of conditional optimization analysis
#[derive(Debug, Clone)]
pub enum ConditionalOptimization {
    /// Condition is always true (can be optimized away)
    AlwaysTrue,
    /// Condition is always false (can be optimized away)
    AlwaysFalse,
    /// Conditional with specific optimization strategy
    Conditional {
        opcode: u8,
        short_circuit: bool,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{LiteralExpr, Literal, UnaryExpr, Span, Location};
    
    #[test]
    fn test_cond_item_creation() {
        let cond = CondItem::new(opcodes::IFEQ, None, None);
        assert_eq!(cond.opcode, opcodes::IFEQ);
        assert!(!cond.is_true());
        assert!(!cond.is_false());
    }
    
    #[test]
    fn test_always_true_condition() {
        let cond = CondItem::new(opcodes::GOTO, None, None);
        assert!(cond.is_true());
        assert!(!cond.is_false());
    }
    
    #[test]
    fn test_always_false_condition() {
        let cond = CondItem::new(opcodes::NOP, None, None);
        assert!(!cond.is_true());
        assert!(cond.is_false());
    }
    
    #[test]
    fn test_opcode_negation() {
        assert_eq!(negate_opcode(opcodes::IFEQ), opcodes::IFNE);
        assert_eq!(negate_opcode(opcodes::IFNE), opcodes::IFEQ);
        assert_eq!(negate_opcode(opcodes::IFLT), opcodes::IFGE);
        assert_eq!(negate_opcode(opcodes::GOTO), opcodes::NOP);
    }
    
    #[test]
    fn test_condition_negation() {
        let cond = CondItem::new(opcodes::IFEQ, None, None);
        let negated = cond.negate();
        assert_eq!(negated.opcode, opcodes::IFNE);
    }
    
    #[test]
    fn test_boolean_literal_analysis() {
        let true_literal = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        });
        
        let cond = CondItemOptimizer::analyze_condition(&true_literal);
        assert!(cond.is_true());
        
        let false_literal = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(false),
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        });
        
        let cond = CondItemOptimizer::analyze_condition(&false_literal);
        assert!(cond.is_false());
    }
    
    #[test]
    fn test_logical_not_optimization() {
        let true_literal = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        });
        
        let not_expr = Expr::Unary(UnaryExpr {
            operator: crate::ast::UnaryOp::Not,
            operand: Box::new(true_literal),
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        });
        
        let cond = CondItemOptimizer::analyze_condition(&not_expr);
        assert!(cond.is_false()); // NOT true = false
    }
    
    #[test]
    fn test_comparison_optimization() {
        let optimization = CondItemOptimizer::optimize_conditional(&Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        }));
        
        assert!(matches!(optimization, ConditionalOptimization::AlwaysTrue));
    }
}
