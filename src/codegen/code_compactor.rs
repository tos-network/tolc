//! Code compaction system for javac-style bytecode optimization
//! 
//! This module implements javac's code compaction functionality including
//! fixedPc mechanism and instruction rewriting for size optimization.

use crate::codegen::opcodes;
use crate::error::Result;
use std::collections::{HashMap, HashSet};

/// Code compaction system (javac-style)
/// 
/// This system performs bytecode compaction by:
/// 1. Identifying fixed program counter locations (jump targets, exception handlers)
/// 2. Compacting instructions while preserving fixed locations
/// 3. Rewriting jump offsets after compaction
pub struct CodeCompactor {
    /// Set of fixed program counter locations that cannot be moved
    fixed_pcs: HashSet<u16>,
    /// Map of original PC to compacted PC
    pc_mapping: HashMap<u16, u16>,
    /// Original bytecode
    original_code: Vec<u8>,
    /// Compacted bytecode
    compacted_code: Vec<u8>,
    /// Jump instructions that need offset rewriting
    jump_instructions: Vec<JumpInstruction>,
    /// Exception table entries that need PC updates
    exception_entries: Vec<ExceptionEntry>,
}

impl CodeCompactor {
    /// Create a new CodeCompactor
    pub fn new() -> Self {
        Self {
            fixed_pcs: HashSet::new(),
            pc_mapping: HashMap::new(),
            original_code: Vec::new(),
            compacted_code: Vec::new(),
            jump_instructions: Vec::new(),
            exception_entries: Vec::new(),
        }
    }
    
    /// Mark a program counter location as fixed (javac fixedPc equivalent)
    pub fn mark_fixed_pc(&mut self, pc: u16) {
        self.fixed_pcs.insert(pc);
    }
    
    /// Check if a PC location is fixed
    pub fn is_fixed_pc(&self, pc: u16) -> bool {
        self.fixed_pcs.contains(&pc)
    }
    
    /// Perform code compaction (javac-style)
    pub fn compact_code(&mut self, code: Vec<u8>) -> Result<CompactionResult> {
        self.original_code = code;
        self.compacted_code.clear();
        self.pc_mapping.clear();
        self.jump_instructions.clear();
        
        // Phase 1: Analyze the code and identify compactable regions
        self.analyze_code()?;
        
        // Phase 2: Perform the actual compaction
        self.perform_compaction()?;
        
        // Phase 3: Rewrite jump offsets
        self.rewrite_jump_offsets()?;
        
        // Phase 4: Update exception table
        self.update_exception_table()?;
        
        Ok(CompactionResult {
            original_size: self.original_code.len(),
            compacted_size: self.compacted_code.len(),
            bytes_saved: self.original_code.len() - self.compacted_code.len(),
            fixed_locations: self.fixed_pcs.len(),
            rewritten_jumps: self.jump_instructions.len(),
        })
    }
    
    /// Analyze the code to identify jump instructions and compactable regions
    fn analyze_code(&mut self) -> Result<()> {
        let mut pc = 0;
        
        while pc < self.original_code.len() {
            let opcode = self.original_code[pc];
            let instruction_info = self.get_instruction_info(opcode);
            
            // Check if this is a jump instruction
            if instruction_info.is_jump {
                let jump_offset = self.read_jump_offset(pc, instruction_info.offset_size)?;
                let target_pc = if instruction_info.is_wide_jump {
                    (pc as i32 + jump_offset as i32) as u16
                } else {
                    (pc as i16 + jump_offset as i16) as u16
                };
                
                self.jump_instructions.push(JumpInstruction {
                    pc: pc as u16,
                    opcode,
                    original_offset: jump_offset,
                    target_pc,
                    offset_size: instruction_info.offset_size,
                });
                
                // Mark the target as fixed
                self.mark_fixed_pc(target_pc);
            }
            
            pc += instruction_info.length;
        }
        
        Ok(())
    }
    
    /// Perform the actual code compaction
    fn perform_compaction(&mut self) -> Result<()> {
        let mut original_pc = 0;
        let mut compacted_pc = 0;
        
        while original_pc < self.original_code.len() {
            // Map the original PC to compacted PC
            self.pc_mapping.insert(original_pc as u16, compacted_pc as u16);
            
            let opcode = self.original_code[original_pc];
            let instruction_info = self.get_instruction_info(opcode);
            
            // Check if we can compact this instruction
            if self.can_compact_instruction(original_pc as u16, &instruction_info) {
                let compacted_instruction = self.compact_instruction(original_pc, &instruction_info)?;
                self.compacted_code.extend_from_slice(&compacted_instruction);
                compacted_pc += compacted_instruction.len();
            } else {
                // Copy instruction as-is
                let instruction_bytes = &self.original_code[original_pc..original_pc + instruction_info.length];
                self.compacted_code.extend_from_slice(instruction_bytes);
                compacted_pc += instruction_info.length;
            }
            
            original_pc += instruction_info.length;
        }
        
        Ok(())
    }
    
    /// Check if an instruction can be compacted
    fn can_compact_instruction(&self, pc: u16, instruction_info: &InstructionInfo) -> bool {
        // Don't compact if this PC is fixed
        if self.is_fixed_pc(pc) {
            return false;
        }
        
        // Don't compact jump instructions (they need special handling)
        if instruction_info.is_jump {
            return false;
        }
        
        // Check for specific compaction opportunities
        match instruction_info.opcode {
            // Wide instructions can sometimes be compacted to narrow forms
            opcodes::WIDE => true,
            // Long form constants can be compacted to short forms
            opcodes::LDC_W => true,
            // Some load/store instructions can be compacted
            opcodes::ILOAD | opcodes::ISTORE | opcodes::ALOAD | opcodes::ASTORE => {
                // Check if we can use the short form (index < 4)
                if pc + 1 < self.original_code.len() as u16 {
                    let index = self.original_code[(pc + 1) as usize];
                    index < 4
                } else {
                    false
                }
            },
            _ => false,
        }
    }
    
    /// Compact an instruction to a smaller form
    fn compact_instruction(&self, pc: usize, instruction_info: &InstructionInfo) -> Result<Vec<u8>> {
        match instruction_info.opcode {
            opcodes::WIDE => {
                // Handle wide instruction compaction
                if pc + 3 < self.original_code.len() {
                    let wide_opcode = self.original_code[pc + 1];
                    let index = u16::from_be_bytes([
                        self.original_code[pc + 2],
                        self.original_code[pc + 3],
                    ]);
                    
                    // If index fits in a byte, use narrow form
                    if index < 256 {
                        return Ok(vec![wide_opcode, index as u8]);
                    }
                }
                // Can't compact, return original
                Ok(self.original_code[pc..pc + instruction_info.length].to_vec())
            },
            opcodes::LDC_W => {
                // Try to compact LDC_W to LDC
                if pc + 2 < self.original_code.len() {
                    let index = u16::from_be_bytes([
                        self.original_code[pc + 1],
                        self.original_code[pc + 2],
                    ]);
                    
                    if index < 256 {
                        return Ok(vec![opcodes::LDC, index as u8]);
                    }
                }
                Ok(self.original_code[pc..pc + instruction_info.length].to_vec())
            },
            opcodes::ILOAD | opcodes::ALOAD => {
                // Try to use short form load instructions
                if pc + 1 < self.original_code.len() {
                    let index = self.original_code[pc + 1];
                    if index < 4 {
                        let short_opcode = match instruction_info.opcode {
                            opcodes::ILOAD => opcodes::ILOAD_0 + index,
                            opcodes::ALOAD => opcodes::ALOAD_0 + index,
                            _ => unreachable!(),
                        };
                        return Ok(vec![short_opcode]);
                    }
                }
                Ok(self.original_code[pc..pc + instruction_info.length].to_vec())
            },
            opcodes::ISTORE | opcodes::ASTORE => {
                // Try to use short form store instructions
                if pc + 1 < self.original_code.len() {
                    let index = self.original_code[pc + 1];
                    if index < 4 {
                        let short_opcode = match instruction_info.opcode {
                            opcodes::ISTORE => opcodes::ISTORE_0 + index,
                            opcodes::ASTORE => opcodes::ASTORE_0 + index,
                            _ => unreachable!(),
                        };
                        return Ok(vec![short_opcode]);
                    }
                }
                Ok(self.original_code[pc..pc + instruction_info.length].to_vec())
            },
            _ => {
                // No compaction available
                Ok(self.original_code[pc..pc + instruction_info.length].to_vec())
            }
        }
    }
    
    /// Rewrite jump offsets after compaction
    fn rewrite_jump_offsets(&mut self) -> Result<()> {
        for jump in &self.jump_instructions {
            let compacted_pc = self.pc_mapping.get(&jump.pc)
                .ok_or_else(|| crate::error::Error::codegen_error("Jump PC not found in mapping"))?;
            
            let compacted_target = self.pc_mapping.get(&jump.target_pc)
                .ok_or_else(|| crate::error::Error::codegen_error("Jump target PC not found in mapping"))?;
            
            let new_offset = (*compacted_target as i32) - (*compacted_pc as i32);
            
            // Write the new offset
            match jump.offset_size {
                2 => {
                    // 16-bit offset
                    let offset_bytes = (new_offset as i16).to_be_bytes();
                    let offset_pos = (*compacted_pc as usize) + 1;
                    if offset_pos + 1 < self.compacted_code.len() {
                        self.compacted_code[offset_pos] = offset_bytes[0];
                        self.compacted_code[offset_pos + 1] = offset_bytes[1];
                    }
                },
                4 => {
                    // 32-bit offset (for wide jumps)
                    let offset_bytes = new_offset.to_be_bytes();
                    let offset_pos = (*compacted_pc as usize) + 1;
                    if offset_pos + 3 < self.compacted_code.len() {
                        for i in 0..4 {
                            self.compacted_code[offset_pos + i] = offset_bytes[i];
                        }
                    }
                },
                _ => return Err(crate::error::Error::codegen_error("Invalid jump offset size")),
            }
        }
        
        Ok(())
    }
    
    /// Update exception table entries with new PC values
    fn update_exception_table(&mut self) -> Result<()> {
        for entry in &mut self.exception_entries {
            entry.start_pc = *self.pc_mapping.get(&entry.start_pc)
                .ok_or_else(|| crate::error::Error::codegen_error("Exception start PC not found"))?;
            
            entry.end_pc = *self.pc_mapping.get(&entry.end_pc)
                .ok_or_else(|| crate::error::Error::codegen_error("Exception end PC not found"))?;
            
            entry.handler_pc = *self.pc_mapping.get(&entry.handler_pc)
                .ok_or_else(|| crate::error::Error::codegen_error("Exception handler PC not found"))?;
        }
        
        Ok(())
    }
    
    /// Read jump offset from bytecode
    fn read_jump_offset(&self, pc: usize, offset_size: usize) -> Result<i32> {
        match offset_size {
            2 => {
                if pc + 2 < self.original_code.len() {
                    let offset = i16::from_be_bytes([
                        self.original_code[pc + 1],
                        self.original_code[pc + 2],
                    ]);
                    Ok(offset as i32)
                } else {
                    Err(crate::error::Error::codegen_error("Invalid jump offset read"))
                }
            },
            4 => {
                if pc + 4 < self.original_code.len() {
                    let offset = i32::from_be_bytes([
                        self.original_code[pc + 1],
                        self.original_code[pc + 2],
                        self.original_code[pc + 3],
                        self.original_code[pc + 4],
                    ]);
                    Ok(offset)
                } else {
                    Err(crate::error::Error::codegen_error("Invalid wide jump offset read"))
                }
            },
            _ => Err(crate::error::Error::codegen_error("Invalid offset size")),
        }
    }
    
    /// Get instruction information for an opcode
    fn get_instruction_info(&self, opcode: u8) -> InstructionInfo {
        match opcode {
            // Jump instructions
            opcodes::GOTO => InstructionInfo { opcode, length: 3, is_jump: true, is_wide_jump: false, offset_size: 2 },
            opcodes::GOTO_W => InstructionInfo { opcode, length: 5, is_jump: true, is_wide_jump: true, offset_size: 4 },
            opcodes::IF_ICMPEQ | opcodes::IF_ICMPNE | opcodes::IF_ICMPLT | opcodes::IF_ICMPGE | opcodes::IF_ICMPGT | opcodes::IF_ICMPLE => {
                InstructionInfo { opcode, length: 3, is_jump: true, is_wide_jump: false, offset_size: 2 }
            },
            opcodes::IFEQ | opcodes::IFNE | opcodes::IFLT | opcodes::IFGE | opcodes::IFGT | opcodes::IFLE => {
                InstructionInfo { opcode, length: 3, is_jump: true, is_wide_jump: false, offset_size: 2 }
            },
            opcodes::IFNULL | opcodes::IFNONNULL => {
                InstructionInfo { opcode, length: 3, is_jump: true, is_wide_jump: false, offset_size: 2 }
            },
            opcodes::JSR => InstructionInfo { opcode, length: 3, is_jump: true, is_wide_jump: false, offset_size: 2 },
            opcodes::JSR_W => InstructionInfo { opcode, length: 5, is_jump: true, is_wide_jump: true, offset_size: 4 },
            
            // Load/store instructions
            opcodes::ILOAD | opcodes::LLOAD | opcodes::FLOAD | opcodes::DLOAD | opcodes::ALOAD => {
                InstructionInfo { opcode, length: 2, is_jump: false, is_wide_jump: false, offset_size: 0 }
            },
            opcodes::ISTORE | opcodes::LSTORE | opcodes::FSTORE | opcodes::DSTORE | opcodes::ASTORE => {
                InstructionInfo { opcode, length: 2, is_jump: false, is_wide_jump: false, offset_size: 0 }
            },
            
            // Constant loading
            opcodes::LDC => InstructionInfo { opcode, length: 2, is_jump: false, is_wide_jump: false, offset_size: 0 },
            opcodes::LDC_W | opcodes::LDC2_W => InstructionInfo { opcode, length: 3, is_jump: false, is_wide_jump: false, offset_size: 0 },
            
            // Wide instruction
            opcodes::WIDE => InstructionInfo { opcode, length: 4, is_jump: false, is_wide_jump: false, offset_size: 0 }, // Variable length, simplified
            
            // Default: single byte instruction
            _ => InstructionInfo { opcode, length: 1, is_jump: false, is_wide_jump: false, offset_size: 0 },
        }
    }
    
    /// Get the compacted bytecode
    pub fn get_compacted_code(&self) -> &[u8] {
        &self.compacted_code
    }
    
    /// Add an exception table entry
    pub fn add_exception_entry(&mut self, start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) {
        self.exception_entries.push(ExceptionEntry {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        });
        
        // Mark exception handler as fixed
        self.mark_fixed_pc(handler_pc);
    }
}

/// Information about a bytecode instruction
#[derive(Debug, Clone)]
struct InstructionInfo {
    /// The opcode
    opcode: u8,
    /// Length of the instruction in bytes
    length: usize,
    /// Whether this is a jump instruction
    is_jump: bool,
    /// Whether this is a wide jump (32-bit offset)
    is_wide_jump: bool,
    /// Size of the offset field (for jumps)
    offset_size: usize,
}

/// Jump instruction information
#[derive(Debug, Clone)]
struct JumpInstruction {
    /// Program counter of the jump instruction
    pc: u16,
    /// Jump opcode
    opcode: u8,
    /// Original jump offset
    original_offset: i32,
    /// Target program counter
    target_pc: u16,
    /// Size of the offset field
    offset_size: usize,
}

/// Exception table entry
#[derive(Debug, Clone)]
struct ExceptionEntry {
    /// Start PC of the try block
    start_pc: u16,
    /// End PC of the try block
    end_pc: u16,
    /// PC of the exception handler
    handler_pc: u16,
    /// Constant pool index of the exception type (0 for finally)
    catch_type: u16,
}

/// Result of code compaction
#[derive(Debug)]
pub struct CompactionResult {
    /// Original code size in bytes
    pub original_size: usize,
    /// Compacted code size in bytes
    pub compacted_size: usize,
    /// Number of bytes saved
    pub bytes_saved: usize,
    /// Number of fixed PC locations
    pub fixed_locations: usize,
    /// Number of rewritten jump instructions
    pub rewritten_jumps: usize,
}

impl CompactionResult {
    /// Calculate the compression ratio
    pub fn compression_ratio(&self) -> f64 {
        if self.original_size == 0 {
            0.0
        } else {
            (self.bytes_saved as f64) / (self.original_size as f64)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_fixed_pc_marking() {
        let mut compactor = CodeCompactor::new();
        
        compactor.mark_fixed_pc(100);
        compactor.mark_fixed_pc(200);
        
        assert!(compactor.is_fixed_pc(100));
        assert!(compactor.is_fixed_pc(200));
        assert!(!compactor.is_fixed_pc(150));
    }
    
    #[test]
    fn test_instruction_info() {
        let compactor = CodeCompactor::new();
        
        let goto_info = compactor.get_instruction_info(opcodes::GOTO);
        assert!(goto_info.is_jump);
        assert_eq!(goto_info.length, 3);
        assert_eq!(goto_info.offset_size, 2);
        
        let goto_w_info = compactor.get_instruction_info(opcodes::GOTO_W);
        assert!(goto_w_info.is_jump);
        assert!(goto_w_info.is_wide_jump);
        assert_eq!(goto_w_info.length, 5);
        assert_eq!(goto_w_info.offset_size, 4);
    }
    
    #[test]
    fn test_simple_compaction() {
        let mut compactor = CodeCompactor::new();
        
        // Simple code: ILOAD_0, ILOAD 1, IRETURN
        let code = vec![
            opcodes::ILOAD_0,           // Can't be compacted further
            opcodes::ILOAD, 1,          // Could be compacted to ILOAD_1
            opcodes::IRETURN,           // Single byte instruction
        ];
        
        let result = compactor.compact_code(code).unwrap();
        
        // Should save at least 1 byte (ILOAD 1 -> ILOAD_1)
        assert!(result.bytes_saved > 0);
        assert_eq!(result.original_size, 4);
    }
    
    #[test]
    fn test_compression_ratio() {
        let result = CompactionResult {
            original_size: 100,
            compacted_size: 80,
            bytes_saved: 20,
            fixed_locations: 5,
            rewritten_jumps: 3,
        };
        
        assert_eq!(result.compression_ratio(), 0.2); // 20% compression
    }
}
