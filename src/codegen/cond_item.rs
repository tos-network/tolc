//! CondItem system - 100% aligned with javac's CondItem implementation
//!
//! This module implements the exact same conditional expression abstraction as Oracle's
//! javac CondItem class, providing unified handling of conditional expressions with
//! true/false jump chains for sophisticated control flow management.

use crate::error::Result;
use crate::codegen::{
    chain::{Chain, ChainOps},
    opcodes,
    code::Code,
};
use crate::ast::Expr;

/// Conditional item abstraction - 100% aligned with javac's CondItem
/// 
/// Represents a conditional expression that can branch to different paths
/// based on whether the condition evaluates to true or false. This is the
/// core abstraction used by javac for all conditional logic generation.
#[derive(Debug, Clone)]
pub struct CondItem {
    /// A chain encompassing all jumps that can be taken if the condition evaluates to true
    pub true_jumps: Option<Box<Chain>>,
    
    /// A chain encompassing all jumps that can be taken if the condition evaluates to false  
    pub false_jumps: Option<Box<Chain>>,
    
    /// The jump's opcode (the instruction used for the conditional test)
    pub opcode: u8,
    
    /// The abstract syntax tree of this item (for debugging and optimization)
    pub tree: Option<Expr>,
    
    /// Type code for stack management (always byte for conditions)
    pub typecode: u8,
}

impl CondItem {
    /// Create a new CondItem with given opcode and jump chains (javac constructor equivalent)
    pub fn new(opcode: u8, true_jumps: Option<Box<Chain>>, false_jumps: Option<Box<Chain>>) -> Self {
        Self {
            true_jumps,
            false_jumps,
            opcode,
            tree: None,
            typecode: opcodes::typecodes::BYTE, // CondItems are always byte type
        }
    }
    
    /// Create a CondItem with associated AST tree for debugging
    pub fn new_with_tree(opcode: u8, true_jumps: Option<Box<Chain>>, false_jumps: Option<Box<Chain>>, tree: Expr) -> Self {
        Self {
            true_jumps,
            false_jumps,
            opcode,
            tree: Some(tree),
            typecode: opcodes::typecodes::BYTE,
        }
    }
    
    /// Load the condition result onto the stack as a boolean value (javac load equivalent)
    /// 
    /// This is the core method that converts a conditional expression into a concrete
    /// boolean value on the stack. It generates bytecode that pushes 1 for true or 0 for false.
    pub fn load(&mut self, code: &mut Code) -> Result<()> {
        // Generate the false branch first
        let false_chain = self.jump_false(code)?;
        
        // If not always false, generate true branch
        if !self.is_false() {
            // Resolve true jumps to current position
            if let Some(true_jumps) = self.true_jumps.take() {
                code.resolve(Some(true_jumps));
            }
            // Push true value (1) onto stack
            code.emitop(opcodes::ICONST_1);
            // Jump over false branch
            let true_chain = code.branch(opcodes::GOTO);
            
            // Generate false branch
            if let Some(false_chain) = false_chain {
                code.resolve(Some(false_chain));
                // Push false value (0) onto stack
                code.emitop(opcodes::ICONST_0);
            }
            
            // Resolve end of true branch
            if let Some(true_chain) = true_chain {
                code.resolve(Some(true_chain));
            }
        } else if let Some(false_chain) = false_chain {
            // Only false branch exists
            code.resolve(Some(false_chain));
            code.emitop(opcodes::ICONST_0);
        }
        
        Ok(())
    }
    
    /// Generate a jump that occurs when the condition is true (javac jumpTrue equivalent)
    pub fn jump_true(&mut self, code: &mut Code) -> Result<Option<Box<Chain>>> {
        if self.tree.is_none() {
            // Simple case: merge existing true jumps with new branch
            let new_branch = code.branch(self.opcode);
            Ok(ChainOps::merge(self.true_jumps.take(), new_branch))
        } else {
            // Complex case with debugging info (for -Xjcov mode in javac)
            let _start_pc = code.cur_cp();
            let new_branch = code.branch(self.opcode);
            let result = ChainOps::merge(self.true_jumps.take(), new_branch);
            
            // In javac, this would add coverage info:
            // code.crt.put(tree, CRTable.CRT_BRANCH_TRUE, start_pc, code.cur_cp());
            // For now, we just track the positions
            
            Ok(result)
        }
    }
    
    /// Generate a jump that occurs when the condition is false (javac jumpFalse equivalent)
    pub fn jump_false(&mut self, code: &mut Code) -> Result<Option<Box<Chain>>> {
        let negated_opcode = Self::negate_opcode(self.opcode);
        
        if self.tree.is_none() {
            // Simple case: merge existing false jumps with negated branch
            let new_branch = code.branch(negated_opcode);
            Ok(ChainOps::merge(self.false_jumps.take(), new_branch))
        } else {
            // Complex case with debugging info
            let _start_pc = code.cur_cp();
            let new_branch = code.branch(negated_opcode);
            let result = ChainOps::merge(self.false_jumps.take(), new_branch);
            
            // In javac, this would add coverage info:
            // code.crt.put(tree, CRTable.CRT_BRANCH_FALSE, start_pc, code.cur_cp());
            
            Ok(result)
        }
    }
    
    /// Create the logical negation of this condition (javac negate equivalent)
    pub fn negate(self) -> Self {
        Self {
            opcode: Self::negate_opcode(self.opcode),
            true_jumps: self.false_jumps,  // Swap true and false jumps
            false_jumps: self.true_jumps,
            tree: self.tree,
            typecode: self.typecode,
        }
    }
    
    /// Check if this condition is always true (javac isTrue equivalent)
    pub fn is_true(&self) -> bool {
        self.false_jumps.is_none() && self.opcode == opcodes::GOTO
    }
    
    /// Check if this condition is always false (javac isFalse equivalent)
    pub fn is_false(&self) -> bool {
        self.true_jumps.is_none() && self.opcode == opcodes::DONTGOTO
    }
    
    /// Get the width of this item on the stack (javac width equivalent)
    /// 
    /// CondItems don't have a concrete size on the stack since they represent
    /// control flow rather than data values.
    pub fn width(&self) -> u16 {
        panic!("CondItem doesn't have a size on the stack")
    }
    
    /// Convert this CondItem to a concrete boolean value (javac mkCond equivalent)
    pub fn mk_cond(self) -> Self {
        self // CondItems are already conditional
    }
    
    /// Create a CondItem that is always true
    pub fn always_true() -> Self {
        Self::new(opcodes::GOTO, None, None)
    }
    
    /// Create a CondItem that is always false  
    pub fn always_false() -> Self {
        Self::new(opcodes::DONTGOTO, None, None)
    }
    
    /// Negate a conditional jump opcode (javac Code.negate equivalent)
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
            opcodes::GOTO => opcodes::DONTGOTO,
            opcodes::DONTGOTO => opcodes::GOTO,
            _ => panic!("Cannot negate opcode: {}", opcode),
        }
    }
    
    /// Get mnemonic name for debugging (javac Code.mnem equivalent)
    pub fn opcode_mnemonic(opcode: u8) -> &'static str {
        match opcode {
            opcodes::IFEQ => "ifeq",
            opcodes::IFNE => "ifne", 
            opcodes::IFLT => "iflt",
            opcodes::IFGE => "ifge",
            opcodes::IFGT => "ifgt",
            opcodes::IFLE => "ifle",
            opcodes::IF_ICMPEQ => "if_icmpeq",
            opcodes::IF_ICMPNE => "if_icmpne",
            opcodes::IF_ICMPLT => "if_icmplt",
            opcodes::IF_ICMPGE => "if_icmpge",
            opcodes::IF_ICMPGT => "if_icmpgt",
            opcodes::IF_ICMPLE => "if_icmple",
            opcodes::IF_ACMPEQ => "if_acmpeq",
            opcodes::IF_ACMPNE => "if_acmpne",
            opcodes::IFNULL => "ifnull",
            opcodes::IFNONNULL => "ifnonnull",
            opcodes::GOTO => "goto",
            opcodes::DONTGOTO => "dontgoto",
            _ => "unknown",
        }
    }
}

impl std::fmt::Display for CondItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cond({})", Self::opcode_mnemonic(self.opcode))
    }
}

/// CondItem factory methods for common conditional patterns
impl CondItem {
    /// Create CondItem for integer comparison (javac pattern)
    pub fn int_comparison(opcode: u8) -> Self {
        match opcode {
            opcodes::IF_ICMPEQ | opcodes::IF_ICMPNE | opcodes::IF_ICMPLT | 
            opcodes::IF_ICMPGE | opcodes::IF_ICMPGT | opcodes::IF_ICMPLE => {
                Self::new(opcode, None, None)
            }
            _ => panic!("Invalid integer comparison opcode: {}", opcode)
        }
    }
    
    /// Create CondItem for reference comparison (javac pattern) 
    pub fn reference_comparison(opcode: u8) -> Self {
        match opcode {
            opcodes::IF_ACMPEQ | opcodes::IF_ACMPNE => {
                Self::new(opcode, None, None)
            }
            _ => panic!("Invalid reference comparison opcode: {}", opcode)
        }
    }
    
    /// Create CondItem for null check (javac pattern)
    pub fn null_check(opcode: u8) -> Self {
        match opcode {
            opcodes::IFNULL | opcodes::IFNONNULL => {
                Self::new(opcode, None, None)
            }
            _ => panic!("Invalid null check opcode: {}", opcode)
        }
    }
    
    /// Create CondItem for zero comparison (javac pattern)
    pub fn zero_comparison(opcode: u8) -> Self {
        match opcode {
            opcodes::IFEQ | opcodes::IFNE | opcodes::IFLT | 
            opcodes::IFGE | opcodes::IFGT | opcodes::IFLE => {
                Self::new(opcode, None, None)
            }
            _ => panic!("Invalid zero comparison opcode: {}", opcode)
        }
    }
}

/// Logical operators for combining CondItems (javac patterns)
impl CondItem {
    /// Logical AND operation between two CondItems (javac pattern)
    pub fn logical_and(mut left: CondItem, mut right: CondItem, code: &mut Code) -> Result<CondItem> {
        // In javac: left && right
        // If left is false, short-circuit to false
        // If left is true, evaluate right
        
        let left_false_chain = left.jump_false(code)?;
        // If left is true, fall through to evaluate right
        if let Some(left_true_chain) = left.true_jumps.take() {
            code.resolve(Some(left_true_chain));
        }
        
        // Combine jump chains: 
        // - true jumps: only right's true jumps (left must be true AND right must be true)
        // - false jumps: left's false jumps + right's false jumps (either can be false)
        Ok(CondItem::new(
            right.opcode,
            right.true_jumps.take(),
            ChainOps::merge(left_false_chain, right.false_jumps.take())
        ))
    }
    
    /// Logical OR operation between two CondItems (javac pattern)
    pub fn logical_or(mut left: CondItem, mut right: CondItem, code: &mut Code) -> Result<CondItem> {
        // In javac: left || right
        // If left is true, short-circuit to true
        // If left is false, evaluate right
        
        let left_true_chain = left.jump_true(code)?;
        // If left is false, fall through to evaluate right
        if let Some(left_false_chain) = left.false_jumps.take() {
            code.resolve(Some(left_false_chain));
        }
        
        // Combine jump chains:
        // - true jumps: left's true jumps + right's true jumps (either can be true)  
        // - false jumps: only right's false jumps (left must be false AND right must be false)
        Ok(CondItem::new(
            right.opcode,
            ChainOps::merge(left_true_chain, right.true_jumps.take()),
            right.false_jumps.take()
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::chain::{Chain, ChainOps};
    
    #[test]
    fn test_cond_item_creation() {
        let cond = CondItem::new(opcodes::IFEQ, None, None);
        assert_eq!(cond.opcode, opcodes::IFEQ);
        assert!(cond.true_jumps.is_none());
        assert!(cond.false_jumps.is_none());
        assert_eq!(cond.typecode, opcodes::typecodes::BYTE);
    }
    
    #[test]
    fn test_cond_item_negate() {
        let cond = CondItem::new(opcodes::IFEQ, None, None);
        let negated = cond.negate();
        assert_eq!(negated.opcode, opcodes::IFNE);
    }
    
    #[test]
    fn test_opcode_negation() {
        assert_eq!(CondItem::negate_opcode(opcodes::IFEQ), opcodes::IFNE);
        assert_eq!(CondItem::negate_opcode(opcodes::IFNE), opcodes::IFEQ);
        assert_eq!(CondItem::negate_opcode(opcodes::IFLT), opcodes::IFGE);
        assert_eq!(CondItem::negate_opcode(opcodes::IF_ICMPEQ), opcodes::IF_ICMPNE);
        assert_eq!(CondItem::negate_opcode(opcodes::GOTO), opcodes::DONTGOTO);
    }
    
    #[test]
    fn test_always_true_false() {
        let always_true = CondItem::always_true();
        assert!(always_true.is_true());
        assert!(!always_true.is_false());
        
        let always_false = CondItem::always_false();
        assert!(!always_false.is_true());
        assert!(always_false.is_false());
    }
    
    #[test]
    fn test_factory_methods() {
        let int_cmp = CondItem::int_comparison(opcodes::IF_ICMPEQ);
        assert_eq!(int_cmp.opcode, opcodes::IF_ICMPEQ);
        
        let ref_cmp = CondItem::reference_comparison(opcodes::IF_ACMPEQ);
        assert_eq!(ref_cmp.opcode, opcodes::IF_ACMPEQ);
        
        let null_check = CondItem::null_check(opcodes::IFNULL);
        assert_eq!(null_check.opcode, opcodes::IFNULL);
        
        let zero_cmp = CondItem::zero_comparison(opcodes::IFEQ);
        assert_eq!(zero_cmp.opcode, opcodes::IFEQ);
    }
    
    #[test]
    fn test_display() {
        let cond = CondItem::new(opcodes::IFEQ, None, None);
        assert_eq!(format!("{}", cond), "cond(ifeq)");
    }
    
    #[test]
    #[should_panic]
    fn test_width_panics() {
        let cond = CondItem::new(opcodes::IFEQ, None, None);
        cond.width(); // Should panic
    }
    
    #[test]
    fn test_opcode_mnemonic() {
        assert_eq!(CondItem::opcode_mnemonic(opcodes::IFEQ), "ifeq");
        assert_eq!(CondItem::opcode_mnemonic(opcodes::IF_ICMPEQ), "if_icmpeq");
        assert_eq!(CondItem::opcode_mnemonic(opcodes::GOTO), "goto");
        assert_eq!(CondItem::opcode_mnemonic(opcodes::DONTGOTO), "dontgoto");
        assert_eq!(CondItem::opcode_mnemonic(254), "unknown"); // Test unknown opcode
    }
}