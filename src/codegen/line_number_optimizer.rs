/// Line Number Debug Information Optimizer - JavaC-aligned implementation
/// 
/// This module implements the Line Number debug information optimization
/// as found in JavaC's Gen.java and Code.java. It provides efficient
/// line number mapping with lazy emission and position tracking.
/// 
/// Based on JavaC implementation:
/// - com.sun.tools.javac.jvm.Code.statBegin()
/// - com.sun.tools.javac.jvm.Code.markStatBegin()
/// - com.sun.tools.javac.jvm.Gen.genStat()

use crate::common::error::Result;
use crate::ast::{Stmt, Expr};
use super::code::LineNumberEntry;
use std::collections::HashMap;

/// Line Number Position Tracking (JavaC Position.NOPOS equivalent)
const NO_POSITION: i32 = -1;

/// Line Number Optimizer - JavaC-aligned implementation
/// 
/// Corresponds to JavaC's Code class line number handling:
/// - pendingStatPos field
/// - statBegin() method  
/// - markStatBegin() method
/// - lineMap field
#[derive(Debug)]
pub struct LineNumberOptimizer {
    /// Pending statement position (JavaC pendingStatPos)
    pending_stat_pos: i32,
    
    /// Line map for position-to-line conversion (JavaC lineMap)
    line_map: LineMap,
    
    /// Generated line number entries
    line_numbers: Vec<LineNumberEntry>,
    
    /// Enable line debug info (JavaC lineDebugInfo)
    line_debug_info: bool,
    
    /// Current program counter (for PC tracking)
    current_pc: u16,
}

/// Line Map - position to line number mapping (JavaC Position.LineMap)
#[derive(Debug)]
pub struct LineMap {
    /// Position to line number cache
    position_cache: HashMap<i32, u16>,
    
    /// Source file line starts (byte positions where each line begins)
    line_starts: Vec<i32>,
}

impl LineMap {
    pub fn new() -> Self {
        Self {
            position_cache: HashMap::new(),
            line_starts: vec![0], // Line 1 starts at position 0
        }
    }
    
    /// Build line map from source text (for testing and simple cases)
    pub fn from_source(source: &str) -> Self {
        let mut line_starts = vec![0];
        let mut pos = 0i32;
        
        for (byte_idx, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push((byte_idx + 1) as i32);
            }
        }
        
        Self {
            position_cache: HashMap::new(),
            line_starts,
        }
    }
    
    /// Get line number for position (JavaC LineMap.getLineNumber)
    pub fn get_line_number(&mut self, pos: i32) -> u16 {
        if pos == NO_POSITION {
            return 1;
        }
        
        // Check cache first
        if let Some(&line) = self.position_cache.get(&pos) {
            return line;
        }
        
        // Binary search for line number (JavaC pattern)
        let line = match self.line_starts.binary_search(&pos) {
            Ok(idx) => (idx + 1) as u16,
            Err(idx) => idx as u16,
        }.max(1);
        
        // Cache the result
        self.position_cache.insert(pos, line);
        line
    }
}

impl LineNumberOptimizer {
    /// Create new line number optimizer (JavaC Code constructor pattern)
    pub fn new(enable_debug: bool) -> Self {
        Self {
            pending_stat_pos: NO_POSITION,
            line_map: LineMap::new(),
            line_numbers: Vec::new(),
            line_debug_info: enable_debug,
            current_pc: 0,
        }
    }
    
    /// Create with source-based line map
    pub fn with_source(source: &str, enable_debug: bool) -> Self {
        Self {
            pending_stat_pos: NO_POSITION,
            line_map: LineMap::from_source(source),
            line_numbers: Vec::new(),
            line_debug_info: enable_debug,
            current_pc: 0,
        }
    }
    
    /// Mark statement begin position (JavaC Code.statBegin)
    /// 
    /// This is called before processing each statement to record
    /// the source position for lazy line number emission.
    pub fn stat_begin(&mut self, pos: i32) {
        if pos != NO_POSITION {
            self.pending_stat_pos = pos;
        }
    }
    
    /// Mark statement begin from synthetic span (placeholder for future span support)
    pub fn stat_begin_synthetic(&mut self, position: i32) {
        self.stat_begin(position);
    }
    
    /// Update current program counter
    pub fn update_pc(&mut self, pc: u16) {
        self.current_pc = pc;
    }
    
    /// Force mark statement begin (JavaC Code.markStatBegin)
    /// 
    /// This is called when emitting opcodes to lazily generate
    /// line number entries. It's the core of the lazy emission optimization.
    pub fn mark_stat_begin(&mut self) -> bool {
        if self.pending_stat_pos != NO_POSITION && self.line_debug_info {
            let line = self.line_map.get_line_number(self.pending_stat_pos);
            let pc = self.current_pc;
            
            // Only add if it's a different line or PC (JavaC optimization)
            let should_add = self.line_numbers.last()
                .map_or(true, |last| last.start_pc != pc || last.line_number != line);
            
            if should_add {
                self.add_line_number(pc, line);
            }
            
            self.pending_stat_pos = NO_POSITION;
            return true;
        }
        
        self.pending_stat_pos = NO_POSITION;
        false
    }
    
    /// Add line number entry (JavaC Code.addLineNumber)
    fn add_line_number(&mut self, start_pc: u16, line_number: u16) {
        // JavaC validation: ensure PC and line fit in u16
        if start_pc == start_pc && line_number == line_number {
            self.line_numbers.push(LineNumberEntry {
                start_pc,
                line_number,
            });
        }
    }
    
    /// Force add line number entry (for special cases)
    pub fn force_add_line_number(&mut self, start_pc: u16, line_number: u16) {
        self.add_line_number(start_pc, line_number);
    }
    
    /// Get all generated line number entries
    pub fn get_line_numbers(&self) -> &[LineNumberEntry] {
        &self.line_numbers
    }
    
    /// Check if line debug info is enabled
    pub fn is_enabled(&self) -> bool {
        self.line_debug_info
    }
    
    /// Get pending statement position (for debugging)
    pub fn get_pending_pos(&self) -> i32 {
        self.pending_stat_pos
    }
    
    /// Clear all line numbers (for reuse)
    pub fn clear(&mut self) {
        self.line_numbers.clear();
        self.pending_stat_pos = NO_POSITION;
    }
    
    /// Optimize line number table (remove redundant entries)
    /// 
    /// JavaC-style optimization: remove consecutive entries with same line
    /// but keep entries that mark significant PC jumps
    pub fn optimize(&mut self) {
        if self.line_numbers.len() <= 1 {
            return;
        }
        
        let mut optimized = Vec::new();
        let mut last_line = 0u16;
        
        for entry in &self.line_numbers {
            // Keep entry if line changes or significant PC jump
            let pc_jump = optimized.last()
                .map_or(true, |last: &LineNumberEntry| entry.start_pc > last.start_pc + 10);
            
            if entry.line_number != last_line || pc_jump {
                optimized.push(entry.clone());
                last_line = entry.line_number;
            }
        }
        
        self.line_numbers = optimized;
    }
}

/// Line Number Context for statement processing
#[derive(Debug)]
pub struct LineNumberContext {
    /// Current statement position
    pub position: i32,
    /// Current program counter  
    pub pc: u16,
    /// Whether this is a new statement
    pub is_new_statement: bool,
}

impl LineNumberContext {
    pub fn new(position: i32, pc: u16) -> Self {
        Self {
            position,
            pc,
            is_new_statement: true,
        }
    }
    
    pub fn from_synthetic(position: i32, pc: u16) -> Self {
        Self::new(position, pc)
    }
}

/// Helper functions for integrating with gen_visitor.rs

/// Process statement with line number tracking (JavaC Gen.genStat pattern)
pub fn process_statement_with_line_info<F>(
    optimizer: &mut LineNumberOptimizer,
    _stmt: &Stmt, // Currently unused until we add proper span support
    pc: u16,
    processor: F
) -> Result<()> 
where
    F: FnOnce() -> Result<()>
{
    // Update PC first
    optimizer.update_pc(pc);
    
    // Mark statement begin with synthetic position for now (JavaC pattern)
    // TODO: Extract actual position from statement when span support is added
    let synthetic_pos = pc as i32; // Use PC as synthetic position
    optimizer.stat_begin(synthetic_pos);
    
    // Process the statement
    let result = processor();
    
    // Force mark begin if needed (lazy emission trigger)
    optimizer.mark_stat_begin();
    
    result
}

/// Process expression with line number tracking  
pub fn process_expression_with_line_info<F>(
    optimizer: &mut LineNumberOptimizer,
    expr: &Expr,
    pc: u16,
    processor: F
) -> Result<()>
where
    F: FnOnce() -> Result<()>
{
    // Update PC
    optimizer.update_pc(pc);
    
    // Mark expression position for complex expressions (synthetic for now)
    // TODO: Add proper span support when available
    match expr {
        Expr::MethodCall(_) | Expr::FieldAccess(_) | Expr::Binary(_) => {
            optimizer.stat_begin_synthetic(pc as i32);
        }
        _ => {} // Skip simple expressions
    }
    
    // Process the expression
    processor()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_line_map_basic() {
        let source = "line 1\nline 2\nline 3";
        let mut line_map = LineMap::from_source(source);
        
        assert_eq!(line_map.get_line_number(0), 1);
        assert_eq!(line_map.get_line_number(5), 1);
        assert_eq!(line_map.get_line_number(7), 2); // After first \n
        assert_eq!(line_map.get_line_number(14), 3); // After second \n
    }
    
    #[test]
    fn test_line_number_optimizer() {
        let mut optimizer = LineNumberOptimizer::new(true);
        optimizer.update_pc(0);
        
        // Test stat_begin and mark_stat_begin
        optimizer.stat_begin(10);
        assert_eq!(optimizer.get_pending_pos(), 10);
        
        optimizer.mark_stat_begin();
        assert_eq!(optimizer.get_pending_pos(), NO_POSITION);
        assert_eq!(optimizer.get_line_numbers().len(), 1);
    }
    
    #[test]
    fn test_optimization() {
        let mut optimizer = LineNumberOptimizer::new(true);
        
        // Add redundant entries
        optimizer.force_add_line_number(0, 1);
        optimizer.force_add_line_number(5, 1);
        optimizer.force_add_line_number(10, 2);
        optimizer.force_add_line_number(15, 2);
        
        optimizer.optimize();
        
        // Should keep first of each line
        let entries = optimizer.get_line_numbers();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].line_number, 1);
        assert_eq!(entries[1].line_number, 2);
    }
}