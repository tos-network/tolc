/// Advanced instruction optimization (javac-style)
/// Implements javac's sophisticated instruction selection and optimization

use crate::codegen::opcodes;
use crate::ast::{BinaryOp, UnaryOp};

/// Instruction selection optimizer (javac Code.java pattern)
pub struct InstructionOptimizer;

impl InstructionOptimizer {
    /// Optimize instruction with wide support (javac emitop1w pattern)
    pub fn emit_with_wide_support(opcode: u8, operand: u16) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        if operand > 0xFF {
            // Use wide instruction (javac wide pattern)
            bytecode.push(opcodes::WIDE);
            bytecode.push(opcode);
            bytecode.extend_from_slice(&operand.to_be_bytes());
        } else {
            // Standard instruction
            bytecode.push(opcode);
            bytecode.push(operand as u8);
        }
        
        bytecode
    }
    
    /// Optimize two-operand instruction with wide support (javac emitop1w pattern)
    pub fn emit_two_operand_with_wide(opcode: u8, operand1: u16, operand2: i16) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        if operand1 > 0xFF || operand2 < -128 || operand2 > 127 {
            // Use wide instruction
            bytecode.push(opcodes::WIDE);
            bytecode.push(opcode);
            bytecode.extend_from_slice(&operand1.to_be_bytes());
            bytecode.extend_from_slice(&operand2.to_be_bytes());
        } else {
            // Standard instruction
            bytecode.push(opcode);
            bytecode.push(operand1 as u8);
            bytecode.push(operand2 as u8);
        }
        
        bytecode
    }
    
    /// Optimize load instruction (javac pattern)
    pub fn optimize_load_instruction(var_type: &str, index: u16) -> Vec<u8> {
        match var_type {
            "int" | "boolean" | "byte" | "char" | "short" => {
                match index {
                    0 => vec![opcodes::ILOAD_0],
                    1 => vec![opcodes::ILOAD_1],
                    2 => vec![opcodes::ILOAD_2],
                    3 => vec![opcodes::ILOAD_3],
                    _ => Self::emit_with_wide_support(opcodes::ILOAD, index),
                }
            }
            "long" => {
                match index {
                    0 => vec![opcodes::LLOAD_0],
                    1 => vec![opcodes::LLOAD_1],
                    2 => vec![opcodes::LLOAD_2],
                    3 => vec![opcodes::LLOAD_3],
                    _ => Self::emit_with_wide_support(opcodes::LLOAD, index),
                }
            }
            "float" => {
                match index {
                    0 => vec![opcodes::FLOAD_0],
                    1 => vec![opcodes::FLOAD_1],
                    2 => vec![opcodes::FLOAD_2],
                    3 => vec![opcodes::FLOAD_3],
                    _ => Self::emit_with_wide_support(opcodes::FLOAD, index),
                }
            }
            "double" => {
                match index {
                    0 => vec![opcodes::DLOAD_0],
                    1 => vec![opcodes::DLOAD_1],
                    2 => vec![opcodes::DLOAD_2],
                    3 => vec![opcodes::DLOAD_3],
                    _ => Self::emit_with_wide_support(opcodes::DLOAD, index),
                }
            }
            _ => {
                // Reference type
                match index {
                    0 => vec![opcodes::ALOAD_0],
                    1 => vec![opcodes::ALOAD_1],
                    2 => vec![opcodes::ALOAD_2],
                    3 => vec![opcodes::ALOAD_3],
                    _ => Self::emit_with_wide_support(opcodes::ALOAD, index),
                }
            }
        }
    }
    
    /// Optimize store instruction (javac pattern)
    pub fn optimize_store_instruction(var_type: &str, index: u16) -> Vec<u8> {
        match var_type {
            "int" | "boolean" | "byte" | "char" | "short" => {
                match index {
                    0 => vec![opcodes::ISTORE_0],
                    1 => vec![opcodes::ISTORE_1],
                    2 => vec![opcodes::ISTORE_2],
                    3 => vec![opcodes::ISTORE_3],
                    _ => Self::emit_with_wide_support(opcodes::ISTORE, index),
                }
            }
            "long" => {
                match index {
                    0 => vec![opcodes::LSTORE_0],
                    1 => vec![opcodes::LSTORE_1],
                    2 => vec![opcodes::LSTORE_2],
                    3 => vec![opcodes::LSTORE_3],
                    _ => Self::emit_with_wide_support(opcodes::LSTORE, index),
                }
            }
            "float" => {
                match index {
                    0 => vec![opcodes::FSTORE_0],
                    1 => vec![opcodes::FSTORE_1],
                    2 => vec![opcodes::FSTORE_2],
                    3 => vec![opcodes::FSTORE_3],
                    _ => Self::emit_with_wide_support(opcodes::FSTORE, index),
                }
            }
            "double" => {
                match index {
                    0 => vec![opcodes::DSTORE_0],
                    1 => vec![opcodes::DSTORE_1],
                    2 => vec![opcodes::DSTORE_2],
                    3 => vec![opcodes::DSTORE_3],
                    _ => Self::emit_with_wide_support(opcodes::DSTORE, index),
                }
            }
            _ => {
                // Reference type
                match index {
                    0 => vec![opcodes::ASTORE_0],
                    1 => vec![opcodes::ASTORE_1],
                    2 => vec![opcodes::ASTORE_2],
                    3 => vec![opcodes::ASTORE_3],
                    _ => Self::emit_with_wide_support(opcodes::ASTORE, index),
                }
            }
        }
    }
    
    /// Optimize increment instruction (javac iinc pattern)
    pub fn optimize_increment_instruction(index: u16, increment: i16) -> Vec<u8> {
        if increment == 1 || increment == -1 {
            // Use optimized iinc for common cases
            Self::emit_two_operand_with_wide(opcodes::IINC, index, increment)
        } else {
            // Use load-add-store sequence for large increments
            let mut bytecode = Vec::new();
            bytecode.extend_from_slice(&Self::optimize_load_instruction("int", index));
            
            // Add constant
            if increment >= -128 && increment <= 127 {
                bytecode.push(opcodes::BIPUSH);
                bytecode.push(increment as u8);
            } else if increment >= -32768 && increment <= 32767 {
                bytecode.push(opcodes::SIPUSH);
                bytecode.extend_from_slice(&increment.to_be_bytes());
            } else {
                // Use LDC for large constants
                bytecode.push(opcodes::LDC);
                bytecode.push(1); // Constant pool index placeholder
            }
            
            bytecode.push(opcodes::IADD);
            bytecode.extend_from_slice(&Self::optimize_store_instruction("int", index));
            
            bytecode
        }
    }
    
    /// Optimize array load instruction (javac pattern)
    pub fn optimize_array_load(element_type: &str) -> Vec<u8> {
        match element_type {
            "int" => vec![opcodes::IALOAD],
            "long" => vec![opcodes::LALOAD],
            "float" => vec![opcodes::FALOAD],
            "double" => vec![opcodes::DALOAD],
            "byte" | "boolean" => vec![opcodes::BALOAD],
            "char" => vec![opcodes::CALOAD],
            "short" => vec![opcodes::SALOAD],
            _ => vec![opcodes::AALOAD], // Reference type
        }
    }
    
    /// Optimize array store instruction (javac pattern)
    pub fn optimize_array_store(element_type: &str) -> Vec<u8> {
        match element_type {
            "int" => vec![opcodes::IASTORE],
            "long" => vec![opcodes::LASTORE],
            "float" => vec![opcodes::FASTORE],
            "double" => vec![opcodes::DASTORE],
            "byte" | "boolean" => vec![opcodes::BASTORE],
            "char" => vec![opcodes::CASTORE],
            "short" => vec![opcodes::SASTORE],
            _ => vec![opcodes::AASTORE], // Reference type
        }
    }
    
    /// Optimize return instruction (javac pattern)
    pub fn optimize_return_instruction(return_type: &str) -> Vec<u8> {
        match return_type {
            "void" => vec![opcodes::RETURN],
            "int" | "boolean" | "byte" | "char" | "short" => vec![opcodes::IRETURN],
            "long" => vec![opcodes::LRETURN],
            "float" => vec![opcodes::FRETURN],
            "double" => vec![opcodes::DRETURN],
            _ => vec![opcodes::ARETURN], // Reference type
        }
    }
    
    /// Optimize binary operation (javac pattern)
    pub fn optimize_binary_operation(op: &BinaryOp, operand_type: &str) -> Vec<u8> {
        match (op, operand_type) {
            // Integer operations
            (BinaryOp::Add, "int") => vec![opcodes::IADD],
            (BinaryOp::Sub, "int") => vec![opcodes::ISUB],
            (BinaryOp::Mul, "int") => vec![opcodes::IMUL],
            (BinaryOp::Div, "int") => vec![opcodes::IDIV],
            (BinaryOp::Mod, "int") => vec![opcodes::IREM],
            (BinaryOp::And, "int") => vec![opcodes::IAND],
            (BinaryOp::Or, "int") => vec![opcodes::IOR],
            (BinaryOp::Xor, "int") => vec![opcodes::IXOR],
            (BinaryOp::LShift, "int") => vec![opcodes::ISHL],
            (BinaryOp::RShift, "int") => vec![opcodes::ISHR],
            (BinaryOp::URShift, "int") => vec![opcodes::IUSHR],
            
            // Long operations
            (BinaryOp::Add, "long") => vec![opcodes::LADD],
            (BinaryOp::Sub, "long") => vec![opcodes::LSUB],
            (BinaryOp::Mul, "long") => vec![opcodes::LMUL],
            (BinaryOp::Div, "long") => vec![opcodes::LDIV],
            (BinaryOp::Mod, "long") => vec![opcodes::LREM],
            (BinaryOp::And, "long") => vec![opcodes::LAND],
            (BinaryOp::Or, "long") => vec![opcodes::LOR],
            (BinaryOp::Xor, "long") => vec![opcodes::LXOR],
            (BinaryOp::LShift, "long") => vec![opcodes::LSHL],
            (BinaryOp::RShift, "long") => vec![opcodes::LSHR],
            (BinaryOp::URShift, "long") => vec![opcodes::LUSHR],
            
            // Float operations
            (BinaryOp::Add, "float") => vec![opcodes::FADD],
            (BinaryOp::Sub, "float") => vec![opcodes::FSUB],
            (BinaryOp::Mul, "float") => vec![opcodes::FMUL],
            (BinaryOp::Div, "float") => vec![opcodes::FDIV],
            (BinaryOp::Mod, "float") => vec![opcodes::FREM],
            
            // Double operations
            (BinaryOp::Add, "double") => vec![opcodes::DADD],
            (BinaryOp::Sub, "double") => vec![opcodes::DSUB],
            (BinaryOp::Mul, "double") => vec![opcodes::DMUL],
            (BinaryOp::Div, "double") => vec![opcodes::DDIV],
            (BinaryOp::Mod, "double") => vec![opcodes::DREM],
            
            _ => vec![opcodes::NOP], // Unsupported operation
        }
    }
    
    /// Optimize unary operation (javac pattern)
    pub fn optimize_unary_operation(op: &UnaryOp, operand_type: &str) -> Vec<u8> {
        match (op, operand_type) {
            (UnaryOp::Minus, "int") => vec![opcodes::INEG],
            (UnaryOp::Minus, "long") => vec![opcodes::LNEG],
            (UnaryOp::Minus, "float") => vec![opcodes::FNEG],
            (UnaryOp::Minus, "double") => vec![opcodes::DNEG],
            _ => vec![opcodes::NOP], // Unsupported operation
        }
    }
    
    /// Optimize type conversion (javac pattern)
    pub fn optimize_type_conversion(from_type: &str, to_type: &str) -> Vec<u8> {
        match (from_type, to_type) {
            // Integer conversions
            ("int", "long") => vec![opcodes::I2L],
            ("int", "float") => vec![opcodes::I2F],
            ("int", "double") => vec![opcodes::I2D],
            ("int", "byte") => vec![opcodes::I2B],
            ("int", "char") => vec![opcodes::I2C],
            ("int", "short") => vec![opcodes::I2S],
            
            // Long conversions
            ("long", "int") => vec![opcodes::L2I],
            ("long", "float") => vec![opcodes::L2F],
            ("long", "double") => vec![opcodes::L2D],
            
            // Float conversions
            ("float", "int") => vec![opcodes::F2I],
            ("float", "long") => vec![opcodes::F2L],
            ("float", "double") => vec![opcodes::F2D],
            
            // Double conversions
            ("double", "int") => vec![opcodes::D2I],
            ("double", "long") => vec![opcodes::D2L],
            ("double", "float") => vec![opcodes::D2F],
            
            _ => vec![], // No conversion needed or unsupported
        }
    }
    
    /// Optimize comparison operation (javac pattern)
    pub fn optimize_comparison(operand_type: &str, nan_behavior: NaNBehavior) -> Vec<u8> {
        match operand_type {
            "long" => vec![opcodes::LCMP],
            "float" => match nan_behavior {
                NaNBehavior::Greater => vec![opcodes::FCMPG],
                NaNBehavior::Less => vec![opcodes::FCMPL],
            },
            "double" => match nan_behavior {
                NaNBehavior::Greater => vec![opcodes::DCMPG],
                NaNBehavior::Less => vec![opcodes::DCMPL],
            },
            _ => vec![], // No comparison instruction needed
        }
    }
    
    /// Optimize monitor operations (javac pattern)
    pub fn optimize_monitor_operation(is_enter: bool) -> Vec<u8> {
        if is_enter {
            vec![opcodes::MONITORENTER]
        } else {
            vec![opcodes::MONITOREXIT]
        }
    }
    
    /// Optimize new array instruction (javac pattern)
    pub fn optimize_new_array(element_type: &str, dimensions: u8) -> Vec<u8> {
        if dimensions == 1 {
            match element_type {
                "boolean" => vec![opcodes::NEWARRAY, 4],
                "char" => vec![opcodes::NEWARRAY, 5],
                "float" => vec![opcodes::NEWARRAY, 6],
                "double" => vec![opcodes::NEWARRAY, 7],
                "byte" => vec![opcodes::NEWARRAY, 8],
                "short" => vec![opcodes::NEWARRAY, 9],
                "int" => vec![opcodes::NEWARRAY, 10],
                "long" => vec![opcodes::NEWARRAY, 11],
                _ => {
                    // Reference type
                    let mut bytecode = vec![opcodes::ANEWARRAY];
                    bytecode.extend_from_slice(&1u16.to_be_bytes()); // Class index placeholder
                    bytecode
                }
            }
        } else {
            // Multi-dimensional array
            let mut bytecode = vec![opcodes::MULTIANEWARRAY];
            bytecode.extend_from_slice(&1u16.to_be_bytes()); // Class index placeholder
            bytecode.push(dimensions);
            bytecode
        }
    }
}

/// NaN behavior for floating-point comparisons
#[derive(Debug, Clone, Copy)]
pub enum NaNBehavior {
    /// NaN compares greater (fcmpg, dcmpg)
    Greater,
    /// NaN compares less (fcmpl, dcmpl)
    Less,
}

/// Peephole optimizer (javac-style)
pub struct PeepholeOptimizer;

impl PeepholeOptimizer {
    /// Apply peephole optimizations to bytecode sequence
    pub fn optimize_sequence(bytecode: &[u8]) -> Vec<u8> {
        let mut optimized = Vec::new();
        let mut i = 0;
        
        while i < bytecode.len() {
            if let Some(optimization) = Self::try_optimize_at(bytecode, i) {
                optimized.extend_from_slice(&optimization.replacement);
                i += optimization.original_length;
            } else {
                optimized.push(bytecode[i]);
                i += 1;
            }
        }
        
        optimized
    }
    
    /// Try to optimize at specific position
    fn try_optimize_at(bytecode: &[u8], pos: usize) -> Option<PeepholeOptimization> {
        // Load-store elimination
        if pos + 1 < bytecode.len() {
            if let Some(opt) = Self::optimize_load_store(bytecode, pos) {
                return Some(opt);
            }
        }
        
        // Constant folding
        if pos + 2 < bytecode.len() {
            if let Some(opt) = Self::optimize_constant_folding(bytecode, pos) {
                return Some(opt);
            }
        }
        
        // Jump optimization
        if pos + 5 < bytecode.len() {
            if let Some(opt) = Self::optimize_jumps(bytecode, pos) {
                return Some(opt);
            }
        }
        
        None
    }
    
    /// Optimize load-store patterns
    fn optimize_load_store(bytecode: &[u8], pos: usize) -> Option<PeepholeOptimization> {
        // Pattern: iload_x, istore_x -> nop (redundant load-store)
        if bytecode[pos] >= opcodes::ILOAD_0 && bytecode[pos] <= opcodes::ILOAD_3 &&
           bytecode[pos + 1] >= opcodes::ISTORE_0 && bytecode[pos + 1] <= opcodes::ISTORE_3 &&
           (bytecode[pos] - opcodes::ILOAD_0) == (bytecode[pos + 1] - opcodes::ISTORE_0) {
            return Some(PeepholeOptimization {
                original_length: 2,
                replacement: vec![], // Remove redundant load-store
            });
        }
        
        None
    }
    
    /// Optimize constant folding patterns
    fn optimize_constant_folding(bytecode: &[u8], pos: usize) -> Option<PeepholeOptimization> {
        // Pattern: iconst_0, iconst_1, iadd -> iconst_1
        if bytecode[pos] == opcodes::ICONST_0 &&
           bytecode[pos + 1] == opcodes::ICONST_1 &&
           bytecode[pos + 2] == opcodes::IADD {
            return Some(PeepholeOptimization {
                original_length: 3,
                replacement: vec![opcodes::ICONST_1],
            });
        }
        
        // Pattern: iconst_1, iconst_1, iadd -> iconst_2
        if bytecode[pos] == opcodes::ICONST_1 &&
           bytecode[pos + 1] == opcodes::ICONST_1 &&
           bytecode[pos + 2] == opcodes::IADD {
            return Some(PeepholeOptimization {
                original_length: 3,
                replacement: vec![opcodes::ICONST_2],
            });
        }
        
        None
    }
    
    /// Optimize jump patterns
    fn optimize_jumps(bytecode: &[u8], pos: usize) -> Option<PeepholeOptimization> {
        // Pattern: goto next_instruction -> nop (remove unnecessary goto)
        if bytecode[pos] == opcodes::GOTO {
            let offset = i16::from_be_bytes([bytecode[pos + 1], bytecode[pos + 2]]);
            if offset == 3 { // Jump to next instruction
                return Some(PeepholeOptimization {
                    original_length: 3,
                    replacement: vec![], // Remove unnecessary goto
                });
            }
        }
        
        None
    }
}

#[derive(Debug, Clone)]
struct PeepholeOptimization {
    original_length: usize,
    replacement: Vec<u8>,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_wide_instruction_optimization() {
        // Test normal instruction
        let bytecode = InstructionOptimizer::emit_with_wide_support(opcodes::ILOAD, 5);
        assert_eq!(bytecode, vec![opcodes::ILOAD, 5]);
        
        // Test wide instruction
        let bytecode = InstructionOptimizer::emit_with_wide_support(opcodes::ILOAD, 300);
        assert_eq!(bytecode[0], opcodes::WIDE);
        assert_eq!(bytecode[1], opcodes::ILOAD);
        assert_eq!(u16::from_be_bytes([bytecode[2], bytecode[3]]), 300);
    }
    
    #[test]
    fn test_load_instruction_optimization() {
        // Test optimized load
        let bytecode = InstructionOptimizer::optimize_load_instruction("int", 0);
        assert_eq!(bytecode, vec![opcodes::ILOAD_0]);
        
        // Test general load
        let bytecode = InstructionOptimizer::optimize_load_instruction("int", 5);
        assert_eq!(bytecode, vec![opcodes::ILOAD, 5]);
        
        // Test wide load
        let bytecode = InstructionOptimizer::optimize_load_instruction("int", 300);
        assert_eq!(bytecode[0], opcodes::WIDE);
    }
    
    #[test]
    fn test_binary_operation_optimization() {
        let bytecode = InstructionOptimizer::optimize_binary_operation(&BinaryOp::Add, "int");
        assert_eq!(bytecode, vec![opcodes::IADD]);
        
        let bytecode = InstructionOptimizer::optimize_binary_operation(&BinaryOp::Mul, "long");
        assert_eq!(bytecode, vec![opcodes::LMUL]);
        
        let bytecode = InstructionOptimizer::optimize_binary_operation(&BinaryOp::Div, "float");
        assert_eq!(bytecode, vec![opcodes::FDIV]);
    }
    
    #[test]
    fn test_type_conversion_optimization() {
        let bytecode = InstructionOptimizer::optimize_type_conversion("int", "long");
        assert_eq!(bytecode, vec![opcodes::I2L]);
        
        let bytecode = InstructionOptimizer::optimize_type_conversion("float", "double");
        assert_eq!(bytecode, vec![opcodes::F2D]);
        
        let bytecode = InstructionOptimizer::optimize_type_conversion("double", "int");
        assert_eq!(bytecode, vec![opcodes::D2I]);
    }
    
    #[test]
    fn test_increment_optimization() {
        // Test simple increment
        let bytecode = InstructionOptimizer::optimize_increment_instruction(1, 1);
        assert!(bytecode.contains(&opcodes::IINC));
        
        // Test large increment (should use load-add-store)
        let bytecode = InstructionOptimizer::optimize_increment_instruction(1, 1000);
        assert!(bytecode.contains(&opcodes::ILOAD_1));
        assert!(bytecode.contains(&opcodes::IADD));
        assert!(bytecode.contains(&opcodes::ISTORE_1));
    }
    
    #[test]
    fn test_array_instruction_optimization() {
        let bytecode = InstructionOptimizer::optimize_array_load("int");
        assert_eq!(bytecode, vec![opcodes::IALOAD]);
        
        let bytecode = InstructionOptimizer::optimize_array_store("float");
        assert_eq!(bytecode, vec![opcodes::FASTORE]);
        
        let bytecode = InstructionOptimizer::optimize_array_load("java.lang.String");
        assert_eq!(bytecode, vec![opcodes::AALOAD]);
    }
    
    #[test]
    fn test_new_array_optimization() {
        // Test primitive array
        let bytecode = InstructionOptimizer::optimize_new_array("int", 1);
        assert_eq!(bytecode, vec![opcodes::NEWARRAY, 10]);
        
        // Test multi-dimensional array
        let bytecode = InstructionOptimizer::optimize_new_array("int", 2);
        assert_eq!(bytecode[0], opcodes::MULTIANEWARRAY);
        assert_eq!(bytecode[3], 2); // dimensions
    }
    
    #[test]
    fn test_peephole_optimization() {
        // Test constant folding
        let original = vec![opcodes::ICONST_0, opcodes::ICONST_1, opcodes::IADD];
        let optimized = PeepholeOptimizer::optimize_sequence(&original);
        assert_eq!(optimized, vec![opcodes::ICONST_1]);
        
        // Test redundant load-store elimination
        let original = vec![opcodes::ILOAD_1, opcodes::ISTORE_1];
        let optimized = PeepholeOptimizer::optimize_sequence(&original);
        assert!(optimized.is_empty());
    }
    
    #[test]
    fn test_comparison_optimization() {
        let bytecode = InstructionOptimizer::optimize_comparison("long", NaNBehavior::Greater);
        assert_eq!(bytecode, vec![opcodes::LCMP]);
        
        let bytecode = InstructionOptimizer::optimize_comparison("float", NaNBehavior::Less);
        assert_eq!(bytecode, vec![opcodes::FCMPL]);
        
        let bytecode = InstructionOptimizer::optimize_comparison("double", NaNBehavior::Greater);
        assert_eq!(bytecode, vec![opcodes::DCMPG]);
    }
}
