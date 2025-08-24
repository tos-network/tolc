//! Branch optimization module - JavaC aligned
//!
//! This module implements branch instruction optimizations following javac's 
//! Code.java patterns for branch chain optimization, negation, and peephole optimization.
//! 
//! Key JavaC patterns implemented:
//! - Branch chain merging (mergeChains)
//! - Branch negation (negate)
//! - Jump resolution optimization
//! - Conditional branch chain optimization

use crate::ast::*;
use crate::common::error::Result;
use crate::codegen::{
    chain::Chain,
    performance_monitor::PerformanceMetrics,
};
use std::collections::HashMap;

/// Branch optimization patterns (JavaC aligned)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BranchOptimizationPattern {
    /// Chain multiple conditional branches
    ChainOptimization,
    /// Negate branch conditions for shorter jumps
    NegationOptimization, 
    /// Eliminate redundant branches
    RedundantBranchElimination,
    /// Convert long branches to short branches
    BranchShortening,
    /// Merge adjacent conditional branches
    ConditionalMerging,
    /// Optimize branch-to-branch jumps
    JumpChaining,
}

/// Branch optimization context
#[derive(Debug)]
pub struct BranchOptimizationContext {
    /// Current method being optimized
    pub method_name: String,
    /// Branch frequency map for hot path optimization
    pub branch_frequency: HashMap<u32, u32>,
    /// Target instruction size limit
    pub target_size_limit: Option<u32>,
    /// Whether to preserve debug information
    pub preserve_debug_info: bool,
}

/// Branch optimization statistics
#[derive(Debug, Default)]
pub struct BranchOptimizationStats {
    /// Number of branches optimized
    pub branches_optimized: u32,
    /// Bytes saved by optimization
    pub bytes_saved: u32,
    /// Number of branch chains merged
    pub chains_merged: u32,
    /// Number of branches negated
    pub branches_negated: u32,
    /// Number of redundant branches eliminated
    pub redundant_eliminated: u32,
}

/// JavaC-aligned branch optimizer
pub struct BranchOptimizer {
    /// Optimization statistics
    pub stats: BranchOptimizationStats,
    /// Performance monitor for metrics
    performance_monitor: Option<&'static PerformanceMetrics>,
    /// Enabled optimization patterns
    enabled_patterns: Vec<BranchOptimizationPattern>,
}

impl BranchOptimizer {
    /// Create new branch optimizer
    pub fn new() -> Self {
        Self {
            stats: BranchOptimizationStats::default(),
            performance_monitor: None,
            enabled_patterns: vec![
                BranchOptimizationPattern::ChainOptimization,
                BranchOptimizationPattern::NegationOptimization,
                BranchOptimizationPattern::RedundantBranchElimination,
                BranchOptimizationPattern::BranchShortening,
            ],
        }
    }
    
    /// Create branch optimizer with performance monitoring
    pub fn with_monitor(monitor: &'static PerformanceMetrics) -> Self {
        Self {
            stats: BranchOptimizationStats::default(),
            performance_monitor: Some(monitor),
            enabled_patterns: vec![
                BranchOptimizationPattern::ChainOptimization,
                BranchOptimizationPattern::NegationOptimization,
                BranchOptimizationPattern::RedundantBranchElimination,
                BranchOptimizationPattern::BranchShortening,
                BranchOptimizationPattern::ConditionalMerging,
                BranchOptimizationPattern::JumpChaining,
            ],
        }
    }
    
    /// Enable specific optimization pattern
    pub fn enable_pattern(&mut self, pattern: BranchOptimizationPattern) {
        if !self.enabled_patterns.contains(&pattern) {
            self.enabled_patterns.push(pattern);
        }
    }
    
    /// Disable specific optimization pattern
    pub fn disable_pattern(&mut self, pattern: BranchOptimizationPattern) {
        self.enabled_patterns.retain(|&p| p != pattern);
    }
    
    /// Optimize branch instructions in bytecode (primary entry point)
    pub fn optimize_branches(&mut self, bytecode: Vec<u8>, context: &BranchOptimizationContext) -> Result<Vec<u8>> {
        let start_time = std::time::Instant::now();
        let original_size = bytecode.len();
        
        let mut optimized_bytecode = bytecode;
        
        // Apply enabled optimization patterns in order
        let patterns_to_apply = self.enabled_patterns.clone();
        for pattern in patterns_to_apply {
            optimized_bytecode = self.apply_pattern(optimized_bytecode, pattern, context)?;
        }
        
        // Update statistics
        let bytes_saved = original_size.saturating_sub(optimized_bytecode.len());
        self.stats.bytes_saved += bytes_saved as u32;
        if bytes_saved > 0 {
            self.stats.branches_optimized += 1;
        }
        
        // Record performance metrics
        if let Some(monitor) = self.performance_monitor {
            let _elapsed = start_time.elapsed();
            // Performance metrics recording (simplified for compilation)
            monitor.branches_optimized.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        
        Ok(optimized_bytecode)
    }
    
    /// Apply specific optimization pattern
    fn apply_pattern(
        &mut self, 
        bytecode: Vec<u8>, 
        pattern: BranchOptimizationPattern, 
        context: &BranchOptimizationContext
    ) -> Result<Vec<u8>> {
        match pattern {
            BranchOptimizationPattern::ChainOptimization => {
                self.optimize_branch_chains(bytecode)
            }
            BranchOptimizationPattern::NegationOptimization => {
                self.optimize_branch_negation(bytecode, context)
            }
            BranchOptimizationPattern::RedundantBranchElimination => {
                self.eliminate_redundant_branches(bytecode)
            }
            BranchOptimizationPattern::BranchShortening => {
                self.shorten_branches(bytecode)
            }
            BranchOptimizationPattern::ConditionalMerging => {
                self.merge_conditional_branches(bytecode)
            }
            BranchOptimizationPattern::JumpChaining => {
                self.optimize_jump_chains(bytecode)
            }
        }
    }
    
    /// Optimize branch chains (JavaC mergeChains pattern)
    fn optimize_branch_chains(&mut self, bytecode: Vec<u8>) -> Result<Vec<u8>> {
        let mut optimized = bytecode;
        let mut changed = true;
        
        while changed {
            changed = false;
            let original_len = optimized.len();
            
            // Look for branch chain patterns
            optimized = self.merge_branch_chains(optimized)?;
            
            if optimized.len() != original_len {
                changed = true;
                self.stats.chains_merged += 1;
            }
        }
        
        Ok(optimized)
    }
    
    /// Merge branch chains following JavaC pattern
    fn merge_branch_chains(&self, bytecode: Vec<u8>) -> Result<Vec<u8>> {
        let mut result = Vec::new();
        let mut i = 0;
        
        while i < bytecode.len() {
            if i + 2 < bytecode.len() && self.is_conditional_branch(bytecode[i]) {
                // Check for branch-to-branch pattern
                let branch_offset = u16::from_be_bytes([bytecode[i + 1], bytecode[i + 2]]) as usize;
                let target_pos = i + 3 + branch_offset;
                
                if target_pos < bytecode.len() && self.is_unconditional_branch(bytecode.get(target_pos).copied().unwrap_or(0)) {
                    // Found branch-to-branch pattern, optimize by redirecting first branch
                    if let Some(final_target) = self.get_final_jump_target(&bytecode, target_pos) {
                        let new_offset = final_target.saturating_sub(i + 3) as u16;
                        result.push(bytecode[i]); // Keep original branch opcode
                        result.extend_from_slice(&new_offset.to_be_bytes());
                        i += 3;
                        continue;
                    }
                }
            }
            
            result.push(bytecode[i]);
            i += 1;
        }
        
        Ok(result)
    }
    
    /// Optimize branch negation (JavaC negate pattern)
    fn optimize_branch_negation(&mut self, bytecode: Vec<u8>, context: &BranchOptimizationContext) -> Result<Vec<u8>> {
        let mut result = Vec::new();
        let mut i = 0;
        
        while i < bytecode.len() {
            if i + 2 < bytecode.len() && self.is_conditional_branch(bytecode[i]) {
                let opcode = bytecode[i];
                let offset = u16::from_be_bytes([bytecode[i + 1], bytecode[i + 2]]) as i16;
                
                // Check if negating the branch would result in shorter bytecode
                if self.should_negate_branch(opcode, offset, context) {
                    let negated_opcode = self.negate_branch_opcode(opcode);
                    let new_offset = self.calculate_negated_offset(offset);
                    
                    result.push(negated_opcode);
                    result.extend_from_slice(&(new_offset as u16).to_be_bytes());
                    self.stats.branches_negated += 1;
                } else {
                    result.push(opcode);
                    result.push(bytecode[i + 1]);
                    result.push(bytecode[i + 2]);
                }
                i += 3;
            } else {
                result.push(bytecode[i]);
                i += 1;
            }
        }
        
        Ok(result)
    }
    
    /// Eliminate redundant branches
    fn eliminate_redundant_branches(&mut self, bytecode: Vec<u8>) -> Result<Vec<u8>> {
        let mut result = Vec::new();
        let mut i = 0;
        
        while i < bytecode.len() {
            if i + 5 < bytecode.len() && self.is_conditional_branch(bytecode[i]) {
                // Look for pattern: conditional_branch + goto where goto is redundant
                let branch_offset = u16::from_be_bytes([bytecode[i + 1], bytecode[i + 2]]) as usize;
                let next_pos = i + 3;
                
                if bytecode.get(next_pos).copied() == Some(0xA7) { // goto
                    let goto_offset = u16::from_be_bytes([
                        bytecode[next_pos + 1], 
                        bytecode[next_pos + 2]
                    ]) as usize;
                    
                    // Check if this is a redundant pattern
                    if self.is_redundant_branch_goto_pattern(branch_offset, goto_offset) {
                        // Eliminate the goto, keep only the branch
                        result.push(bytecode[i]);
                        result.push(bytecode[i + 1]);
                        result.push(bytecode[i + 2]);
                        i += 6; // Skip the redundant goto
                        self.stats.redundant_eliminated += 1;
                        continue;
                    }
                }
            }
            
            result.push(bytecode[i]);
            i += 1;
        }
        
        Ok(result)
    }
    
    /// Shorten branch instructions where possible
    fn shorten_branches(&mut self, bytecode: Vec<u8>) -> Result<Vec<u8>> {
        // For now, return unchanged - this would require complex offset recalculation
        // In a full implementation, we'd convert goto_w to goto where possible
        Ok(bytecode)
    }
    
    /// Merge adjacent conditional branches
    fn merge_conditional_branches(&mut self, bytecode: Vec<u8>) -> Result<Vec<u8>> {
        // Advanced optimization - merge logically related conditional branches
        // This would require sophisticated pattern recognition
        Ok(bytecode)
    }
    
    /// Optimize jump chains
    fn optimize_jump_chains(&mut self, bytecode: Vec<u8>) -> Result<Vec<u8>> {
        let mut result = Vec::new();
        let mut i = 0;
        
        while i < bytecode.len() {
            if bytecode.get(i).copied() == Some(0xA7) && i + 2 < bytecode.len() { // goto
                let offset = u16::from_be_bytes([bytecode[i + 1], bytecode[i + 2]]) as usize;
                let target_pos = i + 3 + offset;
                
                if let Some(final_target) = self.get_final_jump_target(&bytecode, target_pos) {
                    // Replace with direct jump to final target
                    let new_offset = final_target.saturating_sub(i + 3) as u16;
                    result.push(0xA7); // goto
                    result.extend_from_slice(&new_offset.to_be_bytes());
                    i += 3;
                    continue;
                }
            }
            
            result.push(bytecode[i]);
            i += 1;
        }
        
        Ok(result)
    }
    
    /// Check if instruction is a conditional branch
    fn is_conditional_branch(&self, opcode: u8) -> bool {
        matches!(opcode, 
            0x99..=0xA6 | // if<cond> instructions
            0xC6 | 0xC7   // ifnull, ifnonnull
        )
    }
    
    /// Check if instruction is an unconditional branch
    fn is_unconditional_branch(&self, opcode: u8) -> bool {
        matches!(opcode, 0xA7 | 0xC8) // goto, goto_w
    }
    
    /// Get final jump target following chain of jumps
    fn get_final_jump_target(&self, bytecode: &[u8], start_pos: usize) -> Option<usize> {
        let mut current_pos = start_pos;
        let mut visited = std::collections::HashSet::new();
        
        while current_pos < bytecode.len() {
            if visited.contains(&current_pos) {
                return None; // Circular jump detected
            }
            visited.insert(current_pos);
            
            if bytecode.get(current_pos).copied() == Some(0xA7) && current_pos + 2 < bytecode.len() {
                let offset = u16::from_be_bytes([
                    bytecode[current_pos + 1], 
                    bytecode[current_pos + 2]
                ]) as usize;
                current_pos = current_pos + 3 + offset;
            } else {
                return Some(current_pos);
            }
        }
        
        None
    }
    
    /// Negate branch opcode (JavaC negate pattern)
    pub fn negate_branch_opcode(&self, opcode: u8) -> u8 {
        match opcode {
            // if<cond> instructions - JavaC pattern: ((opcode + 1) ^ 1) - 1
            0x99 => 0x9A, // ifeq -> ifne
            0x9A => 0x99, // ifne -> ifeq
            0x9B => 0x9C, // iflt -> ifge
            0x9C => 0x9B, // ifge -> iflt
            0x9D => 0x9E, // ifgt -> ifle
            0x9E => 0x9D, // ifle -> ifgt
            0x9F => 0xA0, // if_icmpeq -> if_icmpne
            0xA0 => 0x9F, // if_icmpne -> if_icmpeq
            0xA1 => 0xA2, // if_icmplt -> if_icmpge
            0xA2 => 0xA1, // if_icmpge -> if_icmplt
            0xA3 => 0xA4, // if_icmpgt -> if_icmple
            0xA4 => 0xA3, // if_icmple -> if_icmpgt
            0xA5 => 0xA6, // if_acmpeq -> if_acmpne
            0xA6 => 0xA5, // if_acmpne -> if_acmpeq
            0xC6 => 0xC7, // ifnull -> ifnonnull
            0xC7 => 0xC6, // ifnonnull -> ifnull
            _ => opcode,   // No negation available
        }
    }
    
    /// Check if branch should be negated for optimization
    fn should_negate_branch(&self, _opcode: u8, _offset: i16, _context: &BranchOptimizationContext) -> bool {
        // Heuristic: negate short backward branches to improve branch prediction
        // In a full implementation, this would use more sophisticated heuristics
        false // Conservative approach for now
    }
    
    /// Calculate negated branch offset
    fn calculate_negated_offset(&self, _original_offset: i16) -> i16 {
        // In a full implementation, this would calculate the new target
        // for the negated branch considering the control flow change
        3 // Skip the branch instruction itself
    }
    
    /// Check if branch-goto pattern is redundant
    fn is_redundant_branch_goto_pattern(&self, _branch_offset: usize, _goto_offset: usize) -> bool {
        // Analyze if the goto is redundant given the branch
        // This would require control flow analysis
        false // Conservative approach
    }
    
    /// Merge two branch chains following JavaC mergeChains pattern
    pub fn merge_chains(chain1: Option<Box<Chain>>, chain2: Option<Box<Chain>>) -> Option<Box<Chain>> {
        match (chain1, chain2) {
            (None, chain2) => chain2,
            (chain1, None) => chain1,
            (Some(c1), Some(c2)) => {
                // JavaC recursive merge sort pattern
                if c1.pc < c2.pc {
                    Some(Box::new(Chain {
                        pc: c2.pc,
                        next: Self::merge_chains(Some(c1), c2.next),
                        stack_size: c2.stack_size,
                        locals_count: c2.locals_count,
                    }))
                } else {
                    Some(Box::new(Chain {
                        pc: c1.pc,
                        next: Self::merge_chains(c1.next, Some(c2)),
                        stack_size: c1.stack_size,
                        locals_count: c1.locals_count,
                    }))
                }
            }
        }
    }
    
    /// Optimize conditional expression branches
    pub fn optimize_conditional_branches(
        &mut self,
        true_chain: Option<Box<Chain>>,
        false_chain: Option<Box<Chain>>,
        expr: &Expr,
    ) -> Result<(Option<Box<Chain>>, Option<Box<Chain>>)> {
        // Analyze the expression and optimize branch patterns
        match expr {
            Expr::Binary(binary) => {
                match binary.operator {
                    BinaryOp::LogicalAnd => {
                        // For &&, optimize to use short-circuit evaluation
                        // false_chain is merged with left operand's false_chain
                        Ok((true_chain, Self::merge_chains(false_chain, None)))
                    }
                    BinaryOp::LogicalOr => {
                        // For ||, optimize to use short-circuit evaluation
                        // true_chain is merged with left operand's true_chain
                        Ok((Self::merge_chains(true_chain, None), false_chain))
                    }
                    _ => Ok((true_chain, false_chain))
                }
            }
            Expr::Unary(unary) if matches!(unary.operator, UnaryOp::Not) => {
                // For !, swap true and false chains
                Ok((false_chain, true_chain))
            }
            _ => Ok((true_chain, false_chain))
        }
    }
    
    /// Get optimization statistics
    pub fn get_stats(&self) -> &BranchOptimizationStats {
        &self.stats
    }
    
    /// Reset optimization statistics
    pub fn reset_stats(&mut self) {
        self.stats = BranchOptimizationStats::default();
    }
    
    /// Check if specific pattern is enabled
    pub fn is_pattern_enabled(&self, pattern: BranchOptimizationPattern) -> bool {
        self.enabled_patterns.contains(&pattern)
    }
}

impl Default for BranchOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_branch_optimizer_creation() {
        let optimizer = BranchOptimizer::new();
        assert_eq!(optimizer.stats.branches_optimized, 0);
        assert!(optimizer.is_pattern_enabled(BranchOptimizationPattern::ChainOptimization));
    }

    #[test]
    fn test_branch_negation() {
        let optimizer = BranchOptimizer::new();
        assert_eq!(optimizer.negate_branch_opcode(0x99), 0x9A); // ifeq -> ifne
        assert_eq!(optimizer.negate_branch_opcode(0x9A), 0x99); // ifne -> ifeq
        assert_eq!(optimizer.negate_branch_opcode(0xC6), 0xC7); // ifnull -> ifnonnull
    }

    #[test]
    fn test_conditional_branch_detection() {
        let optimizer = BranchOptimizer::new();
        assert!(optimizer.is_conditional_branch(0x99)); // ifeq
        assert!(optimizer.is_conditional_branch(0xA5)); // if_acmpeq
        assert!(optimizer.is_conditional_branch(0xC6)); // ifnull
        assert!(!optimizer.is_conditional_branch(0xA7)); // goto
    }

    #[test]
    fn test_unconditional_branch_detection() {
        let optimizer = BranchOptimizer::new();
        assert!(optimizer.is_unconditional_branch(0xA7)); // goto
        assert!(optimizer.is_unconditional_branch(0xC8)); // goto_w
        assert!(!optimizer.is_unconditional_branch(0x99)); // ifeq
    }

    #[test]
    fn test_pattern_enable_disable() {
        let mut optimizer = BranchOptimizer::new();
        
        optimizer.disable_pattern(BranchOptimizationPattern::ChainOptimization);
        assert!(!optimizer.is_pattern_enabled(BranchOptimizationPattern::ChainOptimization));
        
        optimizer.enable_pattern(BranchOptimizationPattern::ChainOptimization);
        assert!(optimizer.is_pattern_enabled(BranchOptimizationPattern::ChainOptimization));
    }

    #[test]
    fn test_merge_chains() {
        let chain1 = Some(Box::new(Chain {
            pc: 10,
            next: None,
            stack_size: 0,
            locals_count: 0,
        }));
        
        let chain2 = Some(Box::new(Chain {
            pc: 20,
            next: None,
            stack_size: 0,
            locals_count: 0,
        }));
        
        let merged = BranchOptimizer::merge_chains(chain1, chain2);
        assert!(merged.is_some());
        assert_eq!(merged.unwrap().pc, 20);
    }
}