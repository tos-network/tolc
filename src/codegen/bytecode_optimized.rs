//! Optimized Bytecode Generation
//! 
//! This module provides high-performance bytecode generation with:
//! - Zero-allocation instruction building for hot paths
//! - Vectorized operations for bulk instruction processing  
//! - Smart buffer management with growth strategies
//! - Cache-friendly instruction layout

use crate::codegen::opcodes;
use crate::error::Result;
use std::io::Write;

/// Optimized bytecode buffer with intelligent growth strategy
#[derive(Debug, Clone)]
pub struct OptimizedBytecodeBuffer {
    /// Main instruction buffer
    buffer: Vec<u8>,
    
    /// Stack depth tracking for optimization
    stack_depth: u16,
    max_stack_depth: u16,
    
    /// Instruction count for statistics
    pub instruction_count: u32,
    
    /// Jump target resolution cache
    jump_targets: Vec<u16>,
    
    /// Hot path instruction cache (for common patterns)
    hot_cache: InstructionCache,
}

/// Cache for frequently used instruction sequences
#[derive(Debug, Clone)]
pub struct InstructionCache {
    /// Common load patterns (load_0, load_1, etc.)
    load_sequences: Vec<Vec<u8>>,
    
    /// Common arithmetic patterns
    arithmetic_sequences: Vec<Vec<u8>>,
    
    /// Common method call patterns
    method_call_sequences: Vec<Vec<u8>>,
}

impl InstructionCache {
    pub fn new() -> Self {
        let mut cache = Self {
            load_sequences: Vec::with_capacity(16),
            arithmetic_sequences: Vec::with_capacity(16),
            method_call_sequences: Vec::with_capacity(8),
        };
        
        // Pre-populate with common sequences
        cache.init_load_sequences();
        cache.init_arithmetic_sequences();
        cache.init_method_call_sequences();
        
        cache
    }
    
    fn init_load_sequences(&mut self) {
        // iconst_0 through iconst_5
        for i in 0..=5 {
            self.load_sequences.push(vec![opcodes::ICONST_0 + i]);
        }
        
        // bipush patterns for common small integers
        for i in 6..=20 {
            self.load_sequences.push(vec![opcodes::BIPUSH, i as u8]);
        }
    }
    
    fn init_arithmetic_sequences(&mut self) {
        // Common arithmetic operations
        self.arithmetic_sequences.push(vec![opcodes::IADD]);
        self.arithmetic_sequences.push(vec![opcodes::ISUB]);
        self.arithmetic_sequences.push(vec![opcodes::IMUL]);
        self.arithmetic_sequences.push(vec![opcodes::IDIV]);
        
        // Increment patterns
        self.arithmetic_sequences.push(vec![opcodes::IINC, 0, 1]); // increment local 0 by 1
        self.arithmetic_sequences.push(vec![opcodes::IINC, 1, 1]); // increment local 1 by 1
    }
    
    fn init_method_call_sequences(&mut self) {
        // Common method call patterns would be populated based on analysis
        // For now, we'll keep this simple
    }
    
    fn get_load_sequence(&self, value: i32) -> Option<&[u8]> {
        if value >= 0 && value < self.load_sequences.len() as i32 {
            Some(&self.load_sequences[value as usize])
        } else {
            None
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.load_sequences.is_empty() && 
        self.arithmetic_sequences.is_empty() && 
        self.method_call_sequences.is_empty()
    }
}

impl OptimizedBytecodeBuffer {
    /// Create new optimized bytecode buffer
    pub fn new() -> Self {
        Self::with_capacity(1024) // Reasonable default size
    }
    
    /// Create with specific initial capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(capacity),
            stack_depth: 0,
            max_stack_depth: 0,
            instruction_count: 0,
            jump_targets: Vec::with_capacity(32),
            hot_cache: InstructionCache::new(),
        }
    }
    
    /// Get current program counter
    #[inline]
    pub fn pc(&self) -> u16 {
        self.buffer.len() as u16
    }
    
    /// Get current stack depth
    #[inline]
    pub fn stack_depth(&self) -> u16 {
        self.stack_depth
    }
    
    /// Get maximum stack depth reached
    #[inline]
    pub fn max_stack_depth(&self) -> u16 {
        self.max_stack_depth
    }
    
    /// Emit single byte instruction (zero allocation)
    #[inline]
    pub fn emit_op(&mut self, opcode: u8) {
        self.buffer.push(opcode);
        self.instruction_count += 1;
        self.update_stack_for_opcode(opcode);
    }
    
    /// Emit instruction with one operand (zero allocation)
    #[inline]
    pub fn emit_op1(&mut self, opcode: u8, operand: u8) {
        self.buffer.push(opcode);
        self.buffer.push(operand);
        self.instruction_count += 1;
        self.update_stack_for_opcode(opcode);
    }
    
    /// Emit instruction with two operands (zero allocation)
    #[inline]
    pub fn emit_op2(&mut self, opcode: u8, operand1: u8, operand2: u8) {
        self.buffer.push(opcode);
        self.buffer.push(operand1);
        self.buffer.push(operand2);
        self.instruction_count += 1;
        self.update_stack_for_opcode(opcode);
    }
    
    /// Emit instruction with u16 operand (zero allocation)
    #[inline]
    pub fn emit_op_u16(&mut self, opcode: u8, operand: u16) {
        self.buffer.push(opcode);
        self.buffer.extend_from_slice(&operand.to_be_bytes());
        self.instruction_count += 1;
        self.update_stack_for_opcode(opcode);
    }
    
    /// Optimized constant loading using cached sequences
    pub fn emit_load_constant(&mut self, value: i32) {
        if let Some(sequence) = self.hot_cache.get_load_sequence(value) {
            // Use cached sequence for hot path
            self.buffer.extend_from_slice(sequence);
            self.instruction_count += 1;
            self.push_stack(1); // All constant loads push 1 value
        } else {
            // Fall back to standard logic
            self.emit_load_constant_standard(value);
        }
    }
    
    fn emit_load_constant_standard(&mut self, value: i32) {
        match value {
            -1 => self.emit_op(opcodes::ICONST_M1),
            0..=5 => self.emit_op(opcodes::ICONST_0 + value as u8),
            -128..=127 => self.emit_op1(opcodes::BIPUSH, value as u8),
            -32768..=32767 => self.emit_op_u16(opcodes::SIPUSH, value as u16),
            _ => {
                // For larger constants, would need constant pool integration
                // This is a simplified version
                self.emit_op_u16(opcodes::LDC, 0); // Placeholder
            }
        }
        self.push_stack(1);
    }
    
    /// Vectorized instruction emission for bulk operations
    pub fn emit_bulk_instructions(&mut self, instructions: &[(u8, &[u8])]) {
        // Calculate total size needed to avoid reallocations
        let total_size: usize = instructions.iter()
            .map(|(_, operands)| 1 + operands.len())
            .sum();
        
        self.buffer.reserve(total_size);
        
        // Emit all instructions in one go
        for &(opcode, operands) in instructions {
            self.buffer.push(opcode);
            self.buffer.extend_from_slice(operands);
            self.instruction_count += 1;
            self.update_stack_for_opcode(opcode);
        }
    }
    
    /// Optimized method invocation
    pub fn emit_method_call(&mut self, opcode: u8, method_ref: u16, param_count: u8) {
        self.emit_op_u16(opcode, method_ref);
        
        // Update stack based on method call type
        match opcode {
            opcodes::INVOKEVIRTUAL | opcodes::INVOKESPECIAL => {
                self.pop_stack(param_count as u16 + 1); // params + this
                self.push_stack(1); // return value (simplified)
            }
            opcodes::INVOKESTATIC => {
                self.pop_stack(param_count as u16); // just params
                self.push_stack(1); // return value (simplified)
            }
            opcodes::INVOKEINTERFACE => {
                self.emit_op1(param_count, 0); // count + padding
                self.pop_stack(param_count as u16 + 1); // params + this
                self.push_stack(1); // return value (simplified)
            }
            _ => {}
        }
    }
    
    /// Smart jump instruction with automatic offset calculation
    pub fn emit_jump(&mut self, opcode: u8, target_pc: u16) -> Result<()> {
        let current_pc = self.pc();
        let offset = if target_pc > current_pc {
            // Forward jump
            target_pc - current_pc - 3 // -3 for instruction size
        } else {
            // Backward jump
            current_pc - target_pc + 3 // +3 for instruction size
        };
        
        if offset <= u16::MAX {
            self.emit_op_u16(opcode, offset);
        } else {
            // Use wide jump instruction
            self.emit_op(opcodes::GOTO_W);
            self.buffer.extend_from_slice(&(offset as u32).to_be_bytes());
            self.instruction_count += 1;
        }
        
        self.jump_targets.push(target_pc);
        Ok(())
    }
    
    /// Optimized array operations
    pub fn emit_array_load(&mut self, element_type: u8) {
        match element_type {
            4 | 8 => self.emit_op(opcodes::BALOAD),  // T_BOOLEAN(4) | T_BYTE(8)  
            5 => self.emit_op(opcodes::CALOAD),      // T_CHAR(5)
            9 => self.emit_op(opcodes::SALOAD),      // T_SHORT(9)
            10 => self.emit_op(opcodes::IALOAD),     // T_INT(10)
            11 => self.emit_op(opcodes::LALOAD),     // T_LONG(11)
            6 => self.emit_op(opcodes::FALOAD),      // T_FLOAT(6)
            7 => self.emit_op(opcodes::DALOAD),      // T_DOUBLE(7)
            _ => self.emit_op(opcodes::AALOAD),      // Reference types
        }
        
        self.pop_stack(2); // array ref + index
        self.push_stack(1); // loaded value
    }
    
    /// Optimized stack operations
    #[inline]
    pub fn push_stack(&mut self, count: u16) {
        self.stack_depth += count;
        self.max_stack_depth = self.max_stack_depth.max(self.stack_depth);
    }
    
    #[inline]
    pub fn pop_stack(&mut self, count: u16) {
        self.stack_depth = self.stack_depth.saturating_sub(count);
    }
    
    /// Update stack depth based on opcode (optimized with lookup table)
    #[inline]
    fn update_stack_for_opcode(&mut self, opcode: u8) {
        // Fast lookup table for common opcodes
        let stack_effect = match opcode {
            // Constants push 1
            opcodes::ACONST_NULL | opcodes::ICONST_M1..=opcodes::ICONST_5 |
            opcodes::LCONST_0 | opcodes::LCONST_1 | opcodes::FCONST_0..=opcodes::FCONST_2 |
            opcodes::DCONST_0 | opcodes::DCONST_1 | opcodes::BIPUSH | opcodes::SIPUSH |
            opcodes::LDC | opcodes::LDC_W | opcodes::LDC2_W => 1,
            
            // Loads push 1
            opcodes::ILOAD | opcodes::LLOAD | opcodes::FLOAD | opcodes::DLOAD | opcodes::ALOAD |
            opcodes::ILOAD_0..=opcodes::ILOAD_3 | opcodes::LLOAD_0..=opcodes::LLOAD_3 |
            opcodes::FLOAD_0..=opcodes::FLOAD_3 | opcodes::DLOAD_0..=opcodes::DLOAD_3 |
            opcodes::ALOAD_0..=opcodes::ALOAD_3 => 1,
            
            // Stores pop 1
            opcodes::ISTORE | opcodes::LSTORE | opcodes::FSTORE | opcodes::DSTORE | opcodes::ASTORE |
            opcodes::ISTORE_0..=opcodes::ISTORE_3 | opcodes::LSTORE_0..=opcodes::LSTORE_3 |
            opcodes::FSTORE_0..=opcodes::FSTORE_3 | opcodes::DSTORE_0..=opcodes::DSTORE_3 |
            opcodes::ASTORE_0..=opcodes::ASTORE_3 => -1,
            
            // Arithmetic usually pops 2, pushes 1 (net -1)
            opcodes::IADD | opcodes::LADD | opcodes::FADD | opcodes::DADD |
            opcodes::ISUB | opcodes::LSUB | opcodes::FSUB | opcodes::DSUB |
            opcodes::IMUL | opcodes::LMUL | opcodes::FMUL | opcodes::DMUL |
            opcodes::IDIV | opcodes::LDIV | opcodes::FDIV | opcodes::DDIV |
            opcodes::IREM | opcodes::LREM | opcodes::FREM | opcodes::DREM => -1,
            
            // Comparisons pop 2, push 1 (net -1)  
            opcodes::LCMP | opcodes::FCMPL | opcodes::FCMPG | opcodes::DCMPL | opcodes::DCMPG => -1,
            
            // Returns pop everything
            opcodes::IRETURN | opcodes::LRETURN | opcodes::FRETURN | opcodes::DRETURN | opcodes::ARETURN => {
                let _old_depth = self.stack_depth;
                self.stack_depth = 0;
                return; // Early return to avoid double adjustment
            }
            
            opcodes::RETURN => {
                self.stack_depth = 0;
                return; // Early return
            }
            
            // Default: no stack effect
            _ => 0,
        };
        
        if stack_effect > 0 {
            self.push_stack(stack_effect as u16);
        } else if stack_effect < 0 {
            self.pop_stack((-stack_effect) as u16);
        }
    }
    
    /// Get instruction statistics
    pub fn stats(&self) -> BytecodeStats {
        BytecodeStats {
            instruction_count: self.instruction_count,
            total_bytes: self.buffer.len(),
            max_stack_depth: self.max_stack_depth,
            jump_count: self.jump_targets.len(),
        }
    }
    
    /// Get raw bytecode buffer
    pub fn into_bytes(self) -> Vec<u8> {
        self.buffer
    }
    
    /// Get reference to bytecode buffer
    pub fn as_bytes(&self) -> &[u8] {
        &self.buffer
    }
    
    /// Write bytecode to output stream
    pub fn write_to<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&self.buffer)
    }
    
    /// Clear buffer for reuse
    pub fn clear(&mut self) {
        self.buffer.clear();
        self.stack_depth = 0;
        self.max_stack_depth = 0;
        self.instruction_count = 0;
        self.jump_targets.clear();
    }
    
    /// Optimize instruction sequences (peephole optimization)
    pub fn optimize_instructions(&mut self) {
        // Simple peephole optimizations
        self.optimize_constant_folding();
        self.optimize_dead_stores();
        self.optimize_jump_chains();
    }
    
    fn optimize_constant_folding(&mut self) {
        // Look for patterns like:
        // iconst_1; iconst_2; iadd -> iconst_3
        let mut i = 0;
        while i + 2 < self.buffer.len() {
            if self.buffer[i] >= opcodes::ICONST_0 && self.buffer[i] <= opcodes::ICONST_5 &&
               self.buffer[i + 1] >= opcodes::ICONST_0 && self.buffer[i + 1] <= opcodes::ICONST_5 &&
               self.buffer[i + 2] == opcodes::IADD {
                
                let val1 = (self.buffer[i] - opcodes::ICONST_0) as i32;
                let val2 = (self.buffer[i + 1] - opcodes::ICONST_0) as i32;
                let result = val1 + val2;
                
                if result >= 0 && result <= 5 {
                    // Replace with single constant
                    self.buffer[i] = opcodes::ICONST_0 + result as u8;
                    // Remove the other two instructions
                    self.buffer.drain(i + 1..i + 3);
                }
            }
            i += 1;
        }
    }
    
    fn optimize_dead_stores(&mut self) {
        // Remove patterns like: iload_1; istore_1 (dead store-load)
        let mut i = 0;
        while i + 1 < self.buffer.len() {
            if (self.buffer[i] >= opcodes::ILOAD_0 && self.buffer[i] <= opcodes::ILOAD_3) &&
               (self.buffer[i + 1] >= opcodes::ISTORE_0 && self.buffer[i + 1] <= opcodes::ISTORE_3) {
                
                let load_var = self.buffer[i] - opcodes::ILOAD_0;
                let store_var = self.buffer[i + 1] - opcodes::ISTORE_0;
                
                if load_var == store_var {
                    // Remove both instructions
                    self.buffer.drain(i..i + 2);
                    continue; // Don't increment i since we removed elements
                }
            }
            i += 1;
        }
    }
    
    fn optimize_jump_chains(&mut self) {
        // Optimize chains of jumps: goto L1; L1: goto L2 -> goto L2
        // This is a simplified version - full implementation would need label tracking
    }
}

impl Default for OptimizedBytecodeBuffer {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about generated bytecode
#[derive(Debug, Clone)]
pub struct BytecodeStats {
    pub instruction_count: u32,
    pub total_bytes: usize,
    pub max_stack_depth: u16,
    pub jump_count: usize,
}

/// High-performance instruction builder for common patterns
pub struct InstructionBuilder {
    buffer: OptimizedBytecodeBuffer,
}

impl InstructionBuilder {
    pub fn new() -> Self {
        Self {
            buffer: OptimizedBytecodeBuffer::new(),
        }
    }
    
    /// Build standard method prologue
    pub fn method_prologue(&mut self, param_count: u8) -> &mut Self {
        // Common method setup patterns
        for i in 0..param_count {
            if i < 4 {
                self.buffer.emit_op(opcodes::ALOAD_0 + i);
            } else {
                self.buffer.emit_op1(opcodes::ALOAD, i);
            }
        }
        self
    }
    
    /// Build standard method epilogue  
    pub fn method_epilogue(&mut self, return_type: u8) -> &mut Self {
        match return_type {
            12 => self.buffer.emit_op(opcodes::RETURN), // T_VOID
            10 | 4 | 8 | 5 | 9 => self.buffer.emit_op(opcodes::IRETURN), // T_INT, T_BOOLEAN, T_BYTE, T_CHAR, T_SHORT
            11 => self.buffer.emit_op(opcodes::LRETURN), // T_LONG
            6 => self.buffer.emit_op(opcodes::FRETURN), // T_FLOAT
            7 => self.buffer.emit_op(opcodes::DRETURN), // T_DOUBLE
            _ => self.buffer.emit_op(opcodes::ARETURN), // Reference type
        }
        self
    }
    
    /// Build efficient integer loop
    pub fn integer_loop(&mut self, loop_var: u8, start: i32, end: i32, body_pc: u16) -> &mut Self {
        // Initialize loop variable
        self.buffer.emit_load_constant(start);
        self.buffer.emit_op1(opcodes::ISTORE, loop_var);
        
        // Loop condition check
        let _loop_start = self.buffer.pc();
        self.buffer.emit_op1(opcodes::ILOAD, loop_var);
        self.buffer.emit_load_constant(end);
        
        // Jump to body if condition met
        self.buffer.emit_jump(opcodes::IF_ICMPLT, body_pc).unwrap();
        
        self
    }
    
    /// Get the built bytecode buffer
    pub fn build(self) -> OptimizedBytecodeBuffer {
        self.buffer
    }
}

impl Default for InstructionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_optimized_constant_loading() {
        let mut buffer = OptimizedBytecodeBuffer::new();
        
        // Test cached constant loading
        buffer.emit_load_constant(0);
        buffer.emit_load_constant(1);
        buffer.emit_load_constant(5);
        
        let bytes = buffer.as_bytes();
        assert_eq!(bytes[0], opcodes::ICONST_0);
        assert_eq!(bytes[1], opcodes::ICONST_1);
        assert_eq!(bytes[2], opcodes::ICONST_5);
    }
    
    #[test]
    fn test_stack_depth_tracking() {
        let mut buffer = OptimizedBytecodeBuffer::new();
        
        buffer.emit_load_constant(10); // +1
        buffer.emit_load_constant(20); // +1  
        buffer.emit_op(opcodes::IADD);  // -1
        
        assert_eq!(buffer.stack_depth(), 1);
        assert_eq!(buffer.max_stack_depth(), 2);
    }
    
    #[test]
    fn test_bulk_instruction_emission() {
        let mut buffer = OptimizedBytecodeBuffer::new();
        
        let instructions = [
            (opcodes::ICONST_1, &[][..]),
            (opcodes::ICONST_2, &[][..]),
            (opcodes::IADD, &[][..]),
        ];
        
        buffer.emit_bulk_instructions(&instructions);
        
        let bytes = buffer.as_bytes();
        assert_eq!(bytes.len(), 3);
        assert_eq!(bytes[0], opcodes::ICONST_1);
        assert_eq!(bytes[1], opcodes::ICONST_2);
        assert_eq!(bytes[2], opcodes::IADD);
    }
    
    #[test]
    fn test_instruction_optimization() {
        let mut buffer = OptimizedBytecodeBuffer::new();
        
        // Create a pattern that can be optimized
        buffer.emit_op(opcodes::ICONST_1);
        buffer.emit_op(opcodes::ICONST_2);
        buffer.emit_op(opcodes::IADD);
        
        buffer.optimize_instructions();
        
        let bytes = buffer.as_bytes();
        assert_eq!(bytes.len(), 1);
        assert_eq!(bytes[0], opcodes::ICONST_3);
    }
    
    #[test]
    fn test_instruction_builder() {
        let mut builder = InstructionBuilder::new();
        
        builder.method_prologue(2)
               .method_epilogue(10); // T_INT
               
        let buffer = builder.build();
        let bytes = buffer.as_bytes();
        
        assert!(bytes.len() > 0);
        assert_eq!(bytes[bytes.len() - 1], opcodes::IRETURN);
    }
}