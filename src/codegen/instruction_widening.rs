/// Instruction widening optimization (javac-style)
/// Automatically widens instructions when operands exceed byte range

use crate::codegen::opcodes;

#[derive(Debug, Clone)]
pub enum WideInstruction {
    // Load instructions
    ILoad(u16),
    LLoad(u16),
    FLoad(u16),
    DLoad(u16),
    ALoad(u16),
    
    // Store instructions
    IStore(u16),
    LStore(u16),
    FStore(u16),
    DStore(u16),
    AStore(u16),
    
    // Increment instruction
    IInc(u16, i16),
    
    // Return instruction
    Ret(u16),
}

pub struct InstructionWidener;

impl InstructionWidener {
    /// Widen instruction if operand exceeds byte range (javac emitop1w pattern)
    pub fn widen_single_operand(opcode: u8, operand: u16) -> Vec<u8> {
        if operand > 0xFF {
            // Use wide prefix
            vec![
                opcodes::WIDE,
                opcode,
                (operand >> 8) as u8,
                (operand & 0xFF) as u8,
            ]
        } else {
            // Normal instruction
            vec![opcode, operand as u8]
        }
    }
    
    /// Widen iinc instruction if operands exceed byte range (javac emitop1w pattern)
    pub fn widen_iinc(local_index: u16, increment: i16) -> Vec<u8> {
        if local_index > 0xFF || increment < -128 || increment > 127 {
            // Use wide prefix
            vec![
                opcodes::WIDE,
                opcodes::IINC,
                (local_index >> 8) as u8,
                (local_index & 0xFF) as u8,
                (increment >> 8) as u8,
                (increment & 0xFF) as u8,
            ]
        } else {
            // Normal iinc
            vec![
                opcodes::IINC,
                local_index as u8,
                increment as u8,
            ]
        }
    }
    
    /// Generate optimized load instruction (javac pattern)
    pub fn generate_load(var_type: LocalVarType, index: u16) -> Vec<u8> {
        match var_type {
            LocalVarType::Int => {
                match index {
                    0 => vec![opcodes::ILOAD_0],
                    1 => vec![opcodes::ILOAD_1],
                    2 => vec![opcodes::ILOAD_2],
                    3 => vec![opcodes::ILOAD_3],
                    _ => Self::widen_single_operand(opcodes::ILOAD, index),
                }
            }
            LocalVarType::Long => {
                match index {
                    0 => vec![opcodes::LLOAD_0],
                    1 => vec![opcodes::LLOAD_1],
                    2 => vec![opcodes::LLOAD_2],
                    3 => vec![opcodes::LLOAD_3],
                    _ => Self::widen_single_operand(opcodes::LLOAD, index),
                }
            }
            LocalVarType::Float => {
                match index {
                    0 => vec![opcodes::FLOAD_0],
                    1 => vec![opcodes::FLOAD_1],
                    2 => vec![opcodes::FLOAD_2],
                    3 => vec![opcodes::FLOAD_3],
                    _ => Self::widen_single_operand(opcodes::FLOAD, index),
                }
            }
            LocalVarType::Double => {
                match index {
                    0 => vec![opcodes::DLOAD_0],
                    1 => vec![opcodes::DLOAD_1],
                    2 => vec![opcodes::DLOAD_2],
                    3 => vec![opcodes::DLOAD_3],
                    _ => Self::widen_single_operand(opcodes::DLOAD, index),
                }
            }
            LocalVarType::Reference => {
                match index {
                    0 => vec![opcodes::ALOAD_0],
                    1 => vec![opcodes::ALOAD_1],
                    2 => vec![opcodes::ALOAD_2],
                    3 => vec![opcodes::ALOAD_3],
                    _ => Self::widen_single_operand(opcodes::ALOAD, index),
                }
            }
        }
    }
    
    /// Generate optimized store instruction (javac pattern)
    pub fn generate_store(var_type: LocalVarType, index: u16) -> Vec<u8> {
        match var_type {
            LocalVarType::Int => {
                match index {
                    0 => vec![opcodes::ISTORE_0],
                    1 => vec![opcodes::ISTORE_1],
                    2 => vec![opcodes::ISTORE_2],
                    3 => vec![opcodes::ISTORE_3],
                    _ => Self::widen_single_operand(opcodes::ISTORE, index),
                }
            }
            LocalVarType::Long => {
                match index {
                    0 => vec![opcodes::LSTORE_0],
                    1 => vec![opcodes::LSTORE_1],
                    2 => vec![opcodes::LSTORE_2],
                    3 => vec![opcodes::LSTORE_3],
                    _ => Self::widen_single_operand(opcodes::LSTORE, index),
                }
            }
            LocalVarType::Float => {
                match index {
                    0 => vec![opcodes::FSTORE_0],
                    1 => vec![opcodes::FSTORE_1],
                    2 => vec![opcodes::FSTORE_2],
                    3 => vec![opcodes::FSTORE_3],
                    _ => Self::widen_single_operand(opcodes::FSTORE, index),
                }
            }
            LocalVarType::Double => {
                match index {
                    0 => vec![opcodes::DSTORE_0],
                    1 => vec![opcodes::DSTORE_1],
                    2 => vec![opcodes::DSTORE_2],
                    3 => vec![opcodes::DSTORE_3],
                    _ => Self::widen_single_operand(opcodes::DSTORE, index),
                }
            }
            LocalVarType::Reference => {
                match index {
                    0 => vec![opcodes::ASTORE_0],
                    1 => vec![opcodes::ASTORE_1],
                    2 => vec![opcodes::ASTORE_2],
                    3 => vec![opcodes::ASTORE_3],
                    _ => Self::widen_single_operand(opcodes::ASTORE, index),
                }
            }
        }
    }
    
    /// Calculate instruction size including wide prefix
    pub fn instruction_size(opcode: u8, operand: u16) -> usize {
        if operand > 0xFF {
            match opcode {
                opcodes::IINC => 6, // wide + iinc + 2 bytes index + 2 bytes increment
                _ => 4, // wide + opcode + 2 bytes operand
            }
        } else {
            match opcode {
                opcodes::IINC => 3, // iinc + 1 byte index + 1 byte increment
                _ => 2, // opcode + 1 byte operand
            }
        }
    }
    
    /// Check if instruction needs widening
    pub fn needs_widening(operand: u16) -> bool {
        operand > 0xFF
    }
    
    /// Check if iinc needs widening
    pub fn iinc_needs_widening(local_index: u16, increment: i16) -> bool {
        local_index > 0xFF || increment < -128 || increment > 127
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LocalVarType {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

/// Advanced instruction optimization (javac-style)
pub struct AdvancedInstructionOptimizer;

impl AdvancedInstructionOptimizer {
    /// Optimize local variable access patterns
    pub fn optimize_local_access_pattern(accesses: &[LocalAccess]) -> Vec<OptimizedAccess> {
        let mut optimized = Vec::new();
        
        for access in accesses {
            let opt_access = match access.index {
                0..=3 => OptimizedAccess {
                    access: access.clone(),
                    use_specialized: true,
                    estimated_size: 1, // Single byte instruction
                },
                4..=255 => OptimizedAccess {
                    access: access.clone(),
                    use_specialized: false,
                    estimated_size: 2, // Opcode + 1 byte operand
                },
                _ => OptimizedAccess {
                    access: access.clone(),
                    use_specialized: false,
                    estimated_size: 4, // Wide + opcode + 2 byte operand
                },
            };
            optimized.push(opt_access);
        }
        
        optimized
    }
    
    /// Analyze local variable usage to suggest optimizations
    pub fn analyze_local_usage(accesses: &[LocalAccess]) -> LocalUsageAnalysis {
        let mut usage_counts = std::collections::HashMap::new();
        let mut max_index = 0;
        
        for access in accesses {
            *usage_counts.entry(access.index).or_insert(0) += 1;
            max_index = max_index.max(access.index);
        }
        
        let frequently_used: Vec<_> = usage_counts
            .iter()
            .filter(|(_, &count)| count >= 3)
            .map(|(&index, &count)| (index, count))
            .collect();
        
        let needs_wide = max_index > 255;
        
        LocalUsageAnalysis {
            max_local_index: max_index,
            frequently_used_locals: frequently_used,
            needs_wide_instructions: needs_wide,
            total_accesses: accesses.len(),
            unique_locals: usage_counts.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalAccess {
    pub index: u16,
    pub var_type: LocalVarType,
    pub is_load: bool,
}

#[derive(Debug, Clone)]
pub struct OptimizedAccess {
    pub access: LocalAccess,
    pub use_specialized: bool,
    pub estimated_size: usize,
}

#[derive(Debug, Clone)]
pub struct LocalUsageAnalysis {
    pub max_local_index: u16,
    pub frequently_used_locals: Vec<(u16, usize)>,
    pub needs_wide_instructions: bool,
    pub total_accesses: usize,
    pub unique_locals: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_no_widening_needed() {
        let bytecode = InstructionWidener::widen_single_operand(opcodes::ILOAD, 10);
        assert_eq!(bytecode, vec![opcodes::ILOAD, 10]);
        assert_eq!(bytecode.len(), 2);
    }
    
    #[test]
    fn test_widening_needed() {
        let bytecode = InstructionWidener::widen_single_operand(opcodes::ILOAD, 300);
        assert_eq!(bytecode, vec![opcodes::WIDE, opcodes::ILOAD, 1, 44]); // 300 = 0x012C
        assert_eq!(bytecode.len(), 4);
    }
    
    #[test]
    fn test_iinc_no_widening() {
        let bytecode = InstructionWidener::widen_iinc(5, 10);
        assert_eq!(bytecode, vec![opcodes::IINC, 5, 10]);
        assert_eq!(bytecode.len(), 3);
    }
    
    #[test]
    fn test_iinc_widening_large_index() {
        let bytecode = InstructionWidener::widen_iinc(300, 10);
        assert_eq!(bytecode, vec![opcodes::WIDE, opcodes::IINC, 1, 44, 0, 10]); // 300 = 0x012C
        assert_eq!(bytecode.len(), 6);
    }
    
    #[test]
    fn test_iinc_widening_large_increment() {
        let bytecode = InstructionWidener::widen_iinc(5, 200);
        assert_eq!(bytecode, vec![opcodes::WIDE, opcodes::IINC, 0, 5, 0, 200]);
        assert_eq!(bytecode.len(), 6);
    }
    
    #[test]
    fn test_load_optimization() {
        // Test specialized instructions for indices 0-3
        let bytecode0 = InstructionWidener::generate_load(LocalVarType::Int, 0);
        assert_eq!(bytecode0, vec![opcodes::ILOAD_0]);
        
        let bytecode1 = InstructionWidener::generate_load(LocalVarType::Int, 1);
        assert_eq!(bytecode1, vec![opcodes::ILOAD_1]);
        
        // Test normal instruction for index 4
        let bytecode4 = InstructionWidener::generate_load(LocalVarType::Int, 4);
        assert_eq!(bytecode4, vec![opcodes::ILOAD, 4]);
        
        // Test wide instruction for large index
        let bytecode300 = InstructionWidener::generate_load(LocalVarType::Int, 300);
        assert_eq!(bytecode300, vec![opcodes::WIDE, opcodes::ILOAD, 1, 44]);
    }
    
    #[test]
    fn test_instruction_size_calculation() {
        assert_eq!(InstructionWidener::instruction_size(opcodes::ILOAD, 10), 2);
        assert_eq!(InstructionWidener::instruction_size(opcodes::ILOAD, 300), 4);
        assert_eq!(InstructionWidener::instruction_size(opcodes::IINC, 10), 3);
        assert_eq!(InstructionWidener::instruction_size(opcodes::IINC, 300), 6);
    }
    
    #[test]
    fn test_local_usage_analysis() {
        let accesses = vec![
            LocalAccess { index: 0, var_type: LocalVarType::Int, is_load: true },
            LocalAccess { index: 0, var_type: LocalVarType::Int, is_load: false },
            LocalAccess { index: 0, var_type: LocalVarType::Int, is_load: true },
            LocalAccess { index: 1, var_type: LocalVarType::Reference, is_load: true },
            LocalAccess { index: 300, var_type: LocalVarType::Long, is_load: true },
        ];
        
        let analysis = AdvancedInstructionOptimizer::analyze_local_usage(&accesses);
        
        assert_eq!(analysis.max_local_index, 300);
        assert!(analysis.needs_wide_instructions);
        assert_eq!(analysis.total_accesses, 5);
        assert_eq!(analysis.unique_locals, 3);
        assert_eq!(analysis.frequently_used_locals.len(), 1); // Only index 0 has >= 3 accesses
        assert_eq!(analysis.frequently_used_locals[0], (0, 3));
    }
}
