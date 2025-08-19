/// Intelligent constant loading optimization (javac-style)
/// Chooses the most efficient instruction for loading constants

use crate::codegen::opcodes;

#[derive(Debug, Clone)]
pub enum ConstantInstruction {
    // Integer constants
    IconstM1,
    Iconst0,
    Iconst1,
    Iconst2,
    Iconst3,
    Iconst4,
    Iconst5,
    Bipush(i8),
    Sipush(i16),
    Ldc(u16),
    
    // Long constants
    Lconst0,
    Lconst1,
    Ldc2W(u16),
    
    // Float constants
    Fconst0,
    Fconst1,
    Fconst2,
    LdcFloat(u16),
    
    // Double constants
    Dconst0,
    Dconst1,
    LdcDouble(u16),
    
    // String constants
    LdcString(u16),
    
    // Null constant
    AconstNull,
}

pub struct ConstantOptimizer;

impl ConstantOptimizer {
    /// Optimize integer constant loading (javac algorithm)
    pub fn optimize_int(value: i32) -> ConstantInstruction {
        match value {
            -1 => ConstantInstruction::IconstM1,
            0 => ConstantInstruction::Iconst0,
            1 => ConstantInstruction::Iconst1,
            2 => ConstantInstruction::Iconst2,
            3 => ConstantInstruction::Iconst3,
            4 => ConstantInstruction::Iconst4,
            5 => ConstantInstruction::Iconst5,
            _ if value >= -128 && value <= 127 => {
                ConstantInstruction::Bipush(value as i8)
            }
            _ if value >= -32768 && value <= 32767 => {
                ConstantInstruction::Sipush(value as i16)
            }
            _ => {
                // For larger values, use LDC (index will be provided by caller)
                ConstantInstruction::Ldc(0) // Placeholder index
            }
        }
    }
    
    /// Optimize long constant loading (javac algorithm)
    pub fn optimize_long(value: i64) -> ConstantInstruction {
        match value {
            0 => ConstantInstruction::Lconst0,
            1 => ConstantInstruction::Lconst1,
            _ => {
                // For other values, use LDC2_W (index will be provided by caller)
                ConstantInstruction::Ldc2W(0) // Placeholder index
            }
        }
    }
    
    /// Optimize float constant loading (javac algorithm)
    pub fn optimize_float(value: f32) -> ConstantInstruction {
        if value == 0.0 {
            ConstantInstruction::Fconst0
        } else if value == 1.0 {
            ConstantInstruction::Fconst1
        } else if value == 2.0 {
            ConstantInstruction::Fconst2
        } else {
            // For other values, use LDC (index will be provided by caller)
            ConstantInstruction::LdcFloat(0) // Placeholder index
        }
    }
    
    /// Optimize double constant loading (javac algorithm)
    pub fn optimize_double(value: f64) -> ConstantInstruction {
        if value == 0.0 {
            ConstantInstruction::Dconst0
        } else if value == 1.0 {
            ConstantInstruction::Dconst1
        } else {
            // For other values, use LDC2_W (index will be provided by caller)
            ConstantInstruction::LdcDouble(0) // Placeholder index
        }
    }
    
    /// Generate bytecode for the optimized constant instruction
    pub fn emit_bytecode(instruction: &ConstantInstruction) -> Vec<u8> {
        match instruction {
            ConstantInstruction::IconstM1 => vec![opcodes::ICONST_M1],
            ConstantInstruction::Iconst0 => vec![opcodes::ICONST_0],
            ConstantInstruction::Iconst1 => vec![opcodes::ICONST_1],
            ConstantInstruction::Iconst2 => vec![opcodes::ICONST_2],
            ConstantInstruction::Iconst3 => vec![opcodes::ICONST_3],
            ConstantInstruction::Iconst4 => vec![opcodes::ICONST_4],
            ConstantInstruction::Iconst5 => vec![opcodes::ICONST_5],
            
            ConstantInstruction::Bipush(value) => {
                vec![opcodes::BIPUSH, *value as u8]
            }
            
            ConstantInstruction::Sipush(value) => {
                let bytes = value.to_be_bytes();
                vec![opcodes::SIPUSH, bytes[0], bytes[1]]
            }
            
            ConstantInstruction::Ldc(index) => {
                if *index <= 255 {
                    vec![opcodes::LDC, *index as u8]
                } else {
                    let bytes = index.to_be_bytes();
                    vec![opcodes::LDC_W, bytes[0], bytes[1]]
                }
            }
            
            ConstantInstruction::Lconst0 => vec![opcodes::LCONST_0],
            ConstantInstruction::Lconst1 => vec![opcodes::LCONST_1],
            
            ConstantInstruction::Ldc2W(index) => {
                let bytes = index.to_be_bytes();
                vec![opcodes::LDC2_W, bytes[0], bytes[1]]
            }
            
            ConstantInstruction::Fconst0 => vec![opcodes::FCONST_0],
            ConstantInstruction::Fconst1 => vec![opcodes::FCONST_1],
            ConstantInstruction::Fconst2 => vec![opcodes::FCONST_2],
            
            ConstantInstruction::LdcFloat(index) => {
                if *index <= 255 {
                    vec![opcodes::LDC, *index as u8]
                } else {
                    let bytes = index.to_be_bytes();
                    vec![opcodes::LDC_W, bytes[0], bytes[1]]
                }
            }
            
            ConstantInstruction::Dconst0 => vec![opcodes::DCONST_0],
            ConstantInstruction::Dconst1 => vec![opcodes::DCONST_1],
            
            ConstantInstruction::LdcDouble(index) => {
                let bytes = index.to_be_bytes();
                vec![opcodes::LDC2_W, bytes[0], bytes[1]]
            }
            
            ConstantInstruction::LdcString(index) => {
                if *index <= 255 {
                    vec![opcodes::LDC, *index as u8]
                } else {
                    let bytes = index.to_be_bytes();
                    vec![opcodes::LDC_W, bytes[0], bytes[1]]
                }
            }
            
            ConstantInstruction::AconstNull => vec![opcodes::ACONST_NULL],
        }
    }
    
    /// Get the stack effect of the constant instruction
    pub fn stack_effect(instruction: &ConstantInstruction) -> (u16, u16) { // (pop, push)
        match instruction {
            ConstantInstruction::Lconst0 | ConstantInstruction::Lconst1 |
            ConstantInstruction::Ldc2W(_) | ConstantInstruction::LdcDouble(_) |
            ConstantInstruction::Dconst0 | ConstantInstruction::Dconst1 => (0, 2), // long/double take 2 slots
            _ => (0, 1), // all other constants take 1 slot
        }
    }
    
    /// Calculate the bytecode size for the instruction
    pub fn instruction_size(instruction: &ConstantInstruction) -> usize {
        match instruction {
            ConstantInstruction::IconstM1 | ConstantInstruction::Iconst0 |
            ConstantInstruction::Iconst1 | ConstantInstruction::Iconst2 |
            ConstantInstruction::Iconst3 | ConstantInstruction::Iconst4 |
            ConstantInstruction::Iconst5 | ConstantInstruction::Lconst0 |
            ConstantInstruction::Lconst1 | ConstantInstruction::Fconst0 |
            ConstantInstruction::Fconst1 | ConstantInstruction::Fconst2 |
            ConstantInstruction::Dconst0 | ConstantInstruction::Dconst1 |
            ConstantInstruction::AconstNull => 1,
            
            ConstantInstruction::Bipush(_) => 2,
            
            ConstantInstruction::Sipush(_) => 3,
            
            ConstantInstruction::Ldc(index) => {
                if *index <= 255 { 2 } else { 3 }
            }
            
            ConstantInstruction::LdcFloat(index) => {
                if *index <= 255 { 2 } else { 3 }
            }
            
            ConstantInstruction::LdcString(index) => {
                if *index <= 255 { 2 } else { 3 }
            }
            
            ConstantInstruction::Ldc2W(_) | ConstantInstruction::LdcDouble(_) => 3,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_int_constant_optimization() {
        // Test iconst range
        assert!(matches!(ConstantOptimizer::optimize_int(-1), ConstantInstruction::IconstM1));
        assert!(matches!(ConstantOptimizer::optimize_int(0), ConstantInstruction::Iconst0));
        assert!(matches!(ConstantOptimizer::optimize_int(5), ConstantInstruction::Iconst5));
        
        // Test bipush range
        assert!(matches!(ConstantOptimizer::optimize_int(127), ConstantInstruction::Bipush(127)));
        assert!(matches!(ConstantOptimizer::optimize_int(-128), ConstantInstruction::Bipush(-128)));
        
        // Test sipush range
        assert!(matches!(ConstantOptimizer::optimize_int(32767), ConstantInstruction::Sipush(32767)));
        assert!(matches!(ConstantOptimizer::optimize_int(-32768), ConstantInstruction::Sipush(-32768)));
        
        // Test ldc range
        assert!(matches!(ConstantOptimizer::optimize_int(100000), ConstantInstruction::Ldc(_)));
    }
    
    #[test]
    fn test_float_constant_optimization() {
        assert!(matches!(ConstantOptimizer::optimize_float(0.0), ConstantInstruction::Fconst0));
        assert!(matches!(ConstantOptimizer::optimize_float(1.0), ConstantInstruction::Fconst1));
        assert!(matches!(ConstantOptimizer::optimize_float(2.0), ConstantInstruction::Fconst2));
        assert!(matches!(ConstantOptimizer::optimize_float(3.14), ConstantInstruction::LdcFloat(_)));
    }
    
    #[test]
    fn test_long_constant_optimization() {
        assert!(matches!(ConstantOptimizer::optimize_long(0), ConstantInstruction::Lconst0));
        assert!(matches!(ConstantOptimizer::optimize_long(1), ConstantInstruction::Lconst1));
        assert!(matches!(ConstantOptimizer::optimize_long(100), ConstantInstruction::Ldc2W(_)));
    }
    
    #[test]
    fn test_stack_effects() {
        assert_eq!(ConstantOptimizer::stack_effect(&ConstantInstruction::Iconst0), (0, 1));
        assert_eq!(ConstantOptimizer::stack_effect(&ConstantInstruction::Lconst0), (0, 2));
        assert_eq!(ConstantOptimizer::stack_effect(&ConstantInstruction::Dconst1), (0, 2));
    }
    
    #[test]
    fn test_instruction_sizes() {
        assert_eq!(ConstantOptimizer::instruction_size(&ConstantInstruction::Iconst0), 1);
        assert_eq!(ConstantOptimizer::instruction_size(&ConstantInstruction::Bipush(10)), 2);
        assert_eq!(ConstantOptimizer::instruction_size(&ConstantInstruction::Sipush(1000)), 3);
        assert_eq!(ConstantOptimizer::instruction_size(&ConstantInstruction::Ldc(100)), 2);
        assert_eq!(ConstantOptimizer::instruction_size(&ConstantInstruction::Ldc(300)), 3);
    }
}
