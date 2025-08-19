//! Branch marking system for javac-style branch optimization
//! 
//! This module implements javac's markBranches functionality for optimizing
//! conditional branches and jump instructions.

use crate::ast::*;
use crate::error::Result;
use std::collections::{HashMap, HashSet};

/// Branch marking system (javac markBranches equivalent)
/// 
/// This system analyzes and marks branches in conditional expressions
/// to enable advanced optimizations like branch elimination and jump chaining.
pub struct BranchMarker {
    /// Map of branch labels to their targets
    branch_targets: HashMap<String, u16>,
    /// Set of reachable branches
    reachable_branches: HashSet<String>,
    /// Map of branch conditions to their complexity
    branch_complexity: HashMap<String, usize>,
    /// Current branch depth for nested conditions
    branch_depth: usize,
}

impl BranchMarker {
    /// Create a new BranchMarker
    pub fn new() -> Self {
        Self {
            branch_targets: HashMap::new(),
            reachable_branches: HashSet::new(),
            branch_complexity: HashMap::new(),
            branch_depth: 0,
        }
    }
    
    /// Mark branches in an expression (javac markBranches equivalent)
    pub fn mark_branches(&mut self, expr: &Expr) -> Result<BranchAnalysis> {
        self.branch_depth = 0;
        self.analyze_expression_branches(expr)
    }
    
    /// Analyze branches in an expression recursively
    fn analyze_expression_branches(&mut self, expr: &Expr) -> Result<BranchAnalysis> {
        self.branch_depth += 1;
        
        if self.branch_depth > 100 {
            return Err(crate::error::Error::codegen_error("Maximum branch depth exceeded"));
        }
        
        let analysis = match expr {
            Expr::Binary(binary) => self.analyze_binary_branches(binary)?,
            Expr::Unary(unary) => self.analyze_unary_branches(unary)?,
            Expr::Conditional(conditional) => self.analyze_conditional_branches(conditional)?,
            Expr::Parenthesized(inner_expr) => self.analyze_expression_branches(inner_expr)?,
            _ => BranchAnalysis::simple(),
        };
        
        self.branch_depth -= 1;
        Ok(analysis)
    }
    
    /// Analyze binary expression branches (AND, OR, comparisons)
    fn analyze_binary_branches(&mut self, binary: &BinaryExpr) -> Result<BranchAnalysis> {
        let left_analysis = self.analyze_expression_branches(&binary.left)?;
        let right_analysis = self.analyze_expression_branches(&binary.right)?;
        
        match binary.operator {
            BinaryOp::And => {
                // AND branches: left must be true to evaluate right
                Ok(BranchAnalysis {
                    branch_type: BranchType::ShortCircuitAnd,
                    complexity: left_analysis.complexity + right_analysis.complexity + 2,
                    has_side_effects: left_analysis.has_side_effects || right_analysis.has_side_effects,
                    true_branches: vec![format!("and_true_{}", self.branch_depth)],
                    false_branches: vec![format!("and_false_{}", self.branch_depth)],
                    needs_jump_table: false,
                })
            },
            BinaryOp::Or => {
                // OR branches: left must be false to evaluate right
                Ok(BranchAnalysis {
                    branch_type: BranchType::ShortCircuitOr,
                    complexity: left_analysis.complexity + right_analysis.complexity + 2,
                    has_side_effects: left_analysis.has_side_effects || right_analysis.has_side_effects,
                    true_branches: vec![format!("or_true_{}", self.branch_depth)],
                    false_branches: vec![format!("or_false_{}", self.branch_depth)],
                    needs_jump_table: false,
                })
            },
            BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                // Comparison branches
                Ok(BranchAnalysis {
                    branch_type: BranchType::Comparison,
                    complexity: left_analysis.complexity + right_analysis.complexity + 1,
                    has_side_effects: left_analysis.has_side_effects || right_analysis.has_side_effects,
                    true_branches: vec![format!("cmp_true_{}", self.branch_depth)],
                    false_branches: vec![format!("cmp_false_{}", self.branch_depth)],
                    needs_jump_table: false,
                })
            },
            _ => {
                // Other binary operations don't create branches
                Ok(BranchAnalysis::simple())
            }
        }
    }
    
    /// Analyze unary expression branches (NOT)
    fn analyze_unary_branches(&mut self, unary: &UnaryExpr) -> Result<BranchAnalysis> {
        let operand_analysis = self.analyze_expression_branches(&unary.operand)?;
        
        match unary.operator {
            UnaryOp::Not => {
                // NOT inverts the branches
                Ok(BranchAnalysis {
                    branch_type: BranchType::Negation,
                    complexity: operand_analysis.complexity + 1,
                    has_side_effects: operand_analysis.has_side_effects,
                    true_branches: operand_analysis.false_branches,
                    false_branches: operand_analysis.true_branches,
                    needs_jump_table: operand_analysis.needs_jump_table,
                })
            },
            _ => Ok(operand_analysis),
        }
    }
    
    /// Analyze conditional expression branches (ternary operator)
    fn analyze_conditional_branches(&mut self, conditional: &ConditionalExpr) -> Result<BranchAnalysis> {
        let condition_analysis = self.analyze_expression_branches(&conditional.condition)?;
        let then_analysis = self.analyze_expression_branches(&conditional.then_expr)?;
        let else_analysis = self.analyze_expression_branches(&conditional.else_expr)?;
        
        Ok(BranchAnalysis {
            branch_type: BranchType::Conditional,
            complexity: condition_analysis.complexity + then_analysis.complexity + else_analysis.complexity + 3,
            has_side_effects: condition_analysis.has_side_effects || then_analysis.has_side_effects || else_analysis.has_side_effects,
            true_branches: vec![format!("cond_then_{}", self.branch_depth)],
            false_branches: vec![format!("cond_else_{}", self.branch_depth)],
            needs_jump_table: condition_analysis.needs_jump_table || then_analysis.needs_jump_table || else_analysis.needs_jump_table,
        })
    }
    

    
    /// Mark a branch as reachable
    pub fn mark_reachable(&mut self, branch_label: &str) {
        self.reachable_branches.insert(branch_label.to_string());
    }
    
    /// Check if a branch is reachable
    pub fn is_reachable(&self, branch_label: &str) -> bool {
        self.reachable_branches.contains(branch_label)
    }
    
    /// Set branch target address
    pub fn set_branch_target(&mut self, branch_label: &str, target_pc: u16) {
        self.branch_targets.insert(branch_label.to_string(), target_pc);
    }
    
    /// Get branch target address
    pub fn get_branch_target(&self, branch_label: &str) -> Option<u16> {
        self.branch_targets.get(branch_label).copied()
    }
    
    /// Optimize branches by eliminating unreachable code
    pub fn optimize_branches(&mut self) -> BranchOptimization {
        let mut eliminated_branches = Vec::new();
        let mut optimized_jumps = Vec::new();
        
        // Find unreachable branches
        for (label, _target) in &self.branch_targets {
            if !self.reachable_branches.contains(label) {
                eliminated_branches.push(label.clone());
            }
        }
        
        // Find jump chains that can be optimized
        for (label, target) in &self.branch_targets {
            if let Some(next_target) = self.find_jump_chain_target(label) {
                if next_target != *target {
                    optimized_jumps.push(JumpOptimization {
                        original_label: label.clone(),
                        original_target: *target,
                        optimized_target: next_target,
                    });
                }
            }
        }
        
        BranchOptimization {
            eliminated_branches,
            optimized_jumps,
            total_branches: self.branch_targets.len(),
            reachable_branches: self.reachable_branches.len(),
        }
    }
    
    /// Find the final target of a jump chain
    fn find_jump_chain_target(&self, start_label: &str) -> Option<u16> {
        let mut visited = HashSet::new();
        let mut current_label = start_label.to_string();
        
        loop {
            if let Some(&target) = self.branch_targets.get(&current_label) {
                if visited.contains(&current_label) {
                    // Circular reference detected
                    break;
                }
                visited.insert(current_label.clone());
                
                // Look for a label at this target
                if let Some(next_label) = self.find_label_at_target(target) {
                    current_label = next_label;
                } else {
                    return Some(target);
                }
            } else {
                break;
            }
        }
        
        self.branch_targets.get(&current_label).copied()
    }
    
    /// Find a label at the given target address
    fn find_label_at_target(&self, target_pc: u16) -> Option<String> {
        for (label, &pc) in &self.branch_targets {
            if pc == target_pc {
                return Some(label.clone());
            }
        }
        None
    }
}

/// Branch analysis result
#[derive(Debug, Clone)]
pub struct BranchAnalysis {
    /// Type of branch
    pub branch_type: BranchType,
    /// Complexity score
    pub complexity: usize,
    /// Whether the expression has side effects
    pub has_side_effects: bool,
    /// Labels for true branches
    pub true_branches: Vec<String>,
    /// Labels for false branches
    pub false_branches: Vec<String>,
    /// Whether a jump table is needed
    pub needs_jump_table: bool,
}

impl BranchAnalysis {
    /// Create a simple branch analysis for non-branching expressions
    pub fn simple() -> Self {
        Self {
            branch_type: BranchType::Simple,
            complexity: 1,
            has_side_effects: false,
            true_branches: Vec::new(),
            false_branches: Vec::new(),
            needs_jump_table: false,
        }
    }
}

/// Types of branches
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BranchType {
    /// Simple expression (no branching)
    Simple,
    /// Short-circuit AND operation
    ShortCircuitAnd,
    /// Short-circuit OR operation
    ShortCircuitOr,
    /// Comparison operation
    Comparison,
    /// Negation (NOT operation)
    Negation,
    /// Conditional expression (ternary)
    Conditional,
}

/// Branch optimization result
#[derive(Debug)]
pub struct BranchOptimization {
    /// Branches that were eliminated as unreachable
    pub eliminated_branches: Vec<String>,
    /// Jumps that were optimized
    pub optimized_jumps: Vec<JumpOptimization>,
    /// Total number of branches
    pub total_branches: usize,
    /// Number of reachable branches
    pub reachable_branches: usize,
}

/// Jump optimization information
#[derive(Debug)]
pub struct JumpOptimization {
    /// Original branch label
    pub original_label: String,
    /// Original target address
    pub original_target: u16,
    /// Optimized target address
    pub optimized_target: u16,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location};
    
    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }
    
    #[test]
    fn test_simple_branch_analysis() {
        let mut marker = BranchMarker::new();
        
        let expr = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::Simple);
        assert_eq!(analysis.complexity, 1);
    }
    
    #[test]
    fn test_and_branch_analysis() {
        let mut marker = BranchMarker::new();
        
        let left = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: create_span(),
        });
        
        let right = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(false),
            span: create_span(),
        });
        
        let and_expr = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: BinaryOp::And,
            right: Box::new(right),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&and_expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::ShortCircuitAnd);
        assert_eq!(analysis.complexity, 4); // 1 + 1 + 2
    }
    
    #[test]
    fn test_branch_reachability() {
        let mut marker = BranchMarker::new();
        
        marker.mark_reachable("branch1");
        marker.set_branch_target("branch1", 100);
        marker.set_branch_target("branch2", 200);
        
        assert!(marker.is_reachable("branch1"));
        assert!(!marker.is_reachable("branch2"));
        
        let optimization = marker.optimize_branches();
        assert_eq!(optimization.eliminated_branches.len(), 1);
        assert_eq!(optimization.eliminated_branches[0], "branch2");
    }
    
    #[test]
    fn test_not_branch_analysis() {
        let mut marker = BranchMarker::new();
        
        let inner = Expr::Literal(LiteralExpr {
            value: Literal::Boolean(true),
            span: create_span(),
        });
        
        let not_expr = Expr::Unary(UnaryExpr {
            operator: UnaryOp::Not,
            operand: Box::new(inner),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&not_expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::Negation);
        assert_eq!(analysis.complexity, 2); // 1 + 1
    }
}
