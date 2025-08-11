//! Opcode generator for Java bytecode
//! 
//! This module provides methods for generating individual Java bytecode instructions.

use super::opcodes;
use super::constpool::ConstantPool;
use std::rc::Rc;
use std::cell::RefCell;

/// Opcode generator for Java bytecode instructions
#[derive(Debug)]
pub struct OpcodeGenerator {
    #[allow(dead_code)]
    constant_pool: Option<Rc<RefCell<ConstantPool>>>,
}

#[allow(dead_code)]
impl OpcodeGenerator {
    /// Create a new opcode generator
    pub fn new() -> Self {
        Self {
            constant_pool: None,
        }
    }
    
    /// Create a new opcode generator with constant pool access
    pub fn new_with_constant_pool(constant_pool: Rc<RefCell<ConstantPool>>) -> Self {
        Self {
            constant_pool: Some(constant_pool),
        }
    }
    
    // Constants
    #[allow(dead_code)]
    pub fn aconst_null(&self) -> Vec<u8> {
        vec![opcodes::ACONST_NULL]
    }
    
    #[allow(dead_code)]
    pub fn iconst_m1(&self) -> Vec<u8> {
        vec![opcodes::ICONST_M1]
    }
    
    #[allow(dead_code)]
    pub fn iconst_0(&self) -> Vec<u8> {
        vec![opcodes::ICONST_0]
    }
    
    #[allow(dead_code)]
    pub fn iconst_1(&self) -> Vec<u8> {
        vec![opcodes::ICONST_1]
    }
    
    #[allow(dead_code)]
    pub fn iconst_2(&self) -> Vec<u8> {
        vec![opcodes::ICONST_2]
    }
    
    #[allow(dead_code)]
    pub fn iconst_3(&self) -> Vec<u8> {
        vec![opcodes::ICONST_3]
    }
    
    #[allow(dead_code)]
    pub fn iconst_4(&self) -> Vec<u8> {
        vec![opcodes::ICONST_4]
    }
    
    #[allow(dead_code)]
    pub fn iconst_5(&self) -> Vec<u8> {
        vec![opcodes::ICONST_5]
    }
    
    #[allow(dead_code)]
    pub fn lconst_0(&self) -> Vec<u8> {
        vec![opcodes::LCONST_0]
    }
    
    #[allow(dead_code)]
    pub fn lconst_1(&self) -> Vec<u8> {
        vec![opcodes::LCONST_1]
    }
    
    #[allow(dead_code)]
    pub fn fconst_0(&self) -> Vec<u8> {
        vec![opcodes::FCONST_0]
    }
    
    #[allow(dead_code)]
    pub fn fconst_1(&self) -> Vec<u8> {
        vec![opcodes::FCONST_1]
    }
    
    #[allow(dead_code)]
    pub fn fconst_2(&self) -> Vec<u8> {
        vec![opcodes::FCONST_2]
    }
    
    #[allow(dead_code)]
    pub fn dconst_0(&self) -> Vec<u8> {
        vec![opcodes::DCONST_0]
    }
    
    #[allow(dead_code)]
    pub fn dconst_1(&self) -> Vec<u8> {
        vec![opcodes::DCONST_1]
    }
    
    #[allow(dead_code)]
    pub fn bipush(&self, byte: i8) -> Vec<u8> {
        vec![opcodes::BIPUSH, byte as u8]
    }
    
    #[allow(dead_code)]
    pub fn sipush(&self, short: i16) -> Vec<u8> {
        let bytes = short.to_be_bytes();
        vec![opcodes::SIPUSH, bytes[0], bytes[1]]
    }
    
    #[allow(dead_code)]
    pub fn ldc(&self, index: u16) -> Vec<u8> {
        if index <= 0xFF {
            vec![opcodes::LDC, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::LDC_W, bytes[0], bytes[1]]
        }
    }
    
    #[allow(dead_code)]
    pub fn ldc2_w(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::LDC2_W, bytes[0], bytes[1]]
    }
    
    // Loads
    pub fn iload(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::ILOAD_0],
                1 => vec![opcodes::ILOAD_1],
                2 => vec![opcodes::ILOAD_2],
                3 => vec![opcodes::ILOAD_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::ILOAD, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::ILOAD, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn lload(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::LLOAD_0],
                1 => vec![opcodes::LLOAD_1],
                2 => vec![opcodes::LLOAD_2],
                3 => vec![opcodes::LLOAD_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::LLOAD, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::LLOAD, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn fload(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::FLOAD_0],
                1 => vec![opcodes::FLOAD_1],
                2 => vec![opcodes::FLOAD_2],
                3 => vec![opcodes::FLOAD_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::FLOAD, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::FLOAD, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn dload(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::DLOAD_0],
                1 => vec![opcodes::DLOAD_1],
                2 => vec![opcodes::DLOAD_2],
                3 => vec![opcodes::DLOAD_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::DLOAD, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::DLOAD, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn aload(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::ALOAD_0],
                1 => vec![opcodes::ALOAD_1],
                2 => vec![opcodes::ALOAD_2],
                3 => vec![opcodes::ALOAD_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::ALOAD, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::ALOAD, bytes[0], bytes[1], 0, 0]
        }
    }
    
    // Array loads
    pub fn iaload(&self) -> Vec<u8> {
        vec![opcodes::IALOAD]
    }
    
    pub fn laload(&self) -> Vec<u8> {
        vec![opcodes::LALOAD]
    }
    
    pub fn faload(&self) -> Vec<u8> {
        vec![opcodes::FALOAD]
    }
    
    pub fn daload(&self) -> Vec<u8> {
        vec![opcodes::DALOAD]
    }
    
    pub fn aaload(&self) -> Vec<u8> {
        vec![opcodes::AALOAD]
    }
    
    pub fn baload(&self) -> Vec<u8> {
        vec![opcodes::BALOAD]
    }
    
    pub fn caload(&self) -> Vec<u8> {
        vec![opcodes::CALOAD]
    }
    
    pub fn saload(&self) -> Vec<u8> {
        vec![opcodes::SALOAD]
    }
    
    // Stores
    pub fn istore(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::ISTORE_0],
                1 => vec![opcodes::ISTORE_1],
                2 => vec![opcodes::ISTORE_2],
                3 => vec![opcodes::ISTORE_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::ISTORE, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::ISTORE, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn lstore(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::LSTORE_0],
                1 => vec![opcodes::LSTORE_1],
                2 => vec![opcodes::LSTORE_2],
                3 => vec![opcodes::LSTORE_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::LSTORE, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::LSTORE, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn fstore(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::FSTORE_0],
                1 => vec![opcodes::FSTORE_1],
                2 => vec![opcodes::FSTORE_2],
                3 => vec![opcodes::FSTORE_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::FSTORE, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::FSTORE, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn dstore(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::DSTORE_0],
                1 => vec![opcodes::DSTORE_1],
                2 => vec![opcodes::DSTORE_2],
                3 => vec![opcodes::DSTORE_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::DSTORE, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::DSTORE, bytes[0], bytes[1], 0, 0]
        }
    }
    
    pub fn astore(&self, index: u16) -> Vec<u8> {
        if index <= 3 {
            match index {
                0 => vec![opcodes::ASTORE_0],
                1 => vec![opcodes::ASTORE_1],
                2 => vec![opcodes::ASTORE_2],
                3 => vec![opcodes::ASTORE_3],
                _ => unreachable!(),
            }
        } else if index <= 0xFF {
            vec![opcodes::ASTORE, index as u8]
        } else {
            let bytes = index.to_be_bytes();
            vec![opcodes::WIDE, opcodes::ASTORE, bytes[0], bytes[1], 0, 0]
        }
    }
    
    // Array stores
    pub fn iastore(&self) -> Vec<u8> {
        vec![opcodes::IASTORE]
    }
    
    pub fn lastore(&self) -> Vec<u8> {
        vec![opcodes::LASTORE]
    }
    
    pub fn fastore(&self) -> Vec<u8> {
        vec![opcodes::FASTORE]
    }
    
    pub fn dastore(&self) -> Vec<u8> {
        vec![opcodes::DASTORE]
    }
    
    pub fn aastore(&self) -> Vec<u8> {
        vec![opcodes::AASTORE]
    }
    
    pub fn bastore(&self) -> Vec<u8> {
        vec![opcodes::BASTORE]
    }
    
    pub fn castore(&self) -> Vec<u8> {
        vec![opcodes::CASTORE]
    }
    
    pub fn sastore(&self) -> Vec<u8> {
        vec![opcodes::SASTORE]
    }
    
    // Stack operations
    pub fn pop(&self) -> Vec<u8> {
        vec![opcodes::POP]
    }
    
    pub fn pop2(&self) -> Vec<u8> {
        vec![opcodes::POP2]
    }
    
    pub fn dup(&self) -> Vec<u8> {
        vec![opcodes::DUP]
    }
    
    pub fn dup_x1(&self) -> Vec<u8> {
        vec![opcodes::DUP_X1]
    }
    
    pub fn dup_x2(&self) -> Vec<u8> {
        vec![opcodes::DUP_X2]
    }
    
    pub fn dup2(&self) -> Vec<u8> {
        vec![opcodes::DUP2]
    }
    
    pub fn dup2_x1(&self) -> Vec<u8> {
        vec![opcodes::DUP2_X1]
    }
    
    pub fn dup2_x2(&self) -> Vec<u8> {
        vec![opcodes::DUP2_X2]
    }
    
    pub fn swap(&self) -> Vec<u8> {
        vec![opcodes::SWAP]
    }
    
    // Arithmetic operations
    pub fn iadd(&self) -> Vec<u8> {
        vec![opcodes::IADD]
    }
    
    pub fn ladd(&self) -> Vec<u8> {
        vec![opcodes::LADD]
    }
    
    pub fn fadd(&self) -> Vec<u8> {
        vec![opcodes::FADD]
    }
    
    pub fn dadd(&self) -> Vec<u8> {
        vec![opcodes::DADD]
    }
    
    pub fn isub(&self) -> Vec<u8> {
        vec![opcodes::ISUB]
    }
    
    pub fn lsub(&self) -> Vec<u8> {
        vec![opcodes::LSUB]
    }
    
    pub fn fsub(&self) -> Vec<u8> {
        vec![opcodes::FSUB]
    }
    
    pub fn dsub(&self) -> Vec<u8> {
        vec![opcodes::DSUB]
    }
    
    pub fn imul(&self) -> Vec<u8> {
        vec![opcodes::IMUL]
    }
    
    pub fn lmul(&self) -> Vec<u8> {
        vec![opcodes::LMUL]
    }
    
    pub fn fmul(&self) -> Vec<u8> {
        vec![opcodes::FMUL]
    }
    
    pub fn dmul(&self) -> Vec<u8> {
        vec![opcodes::DMUL]
    }
    
    pub fn idiv(&self) -> Vec<u8> {
        vec![opcodes::IDIV]
    }
    
    pub fn ldiv(&self) -> Vec<u8> {
        vec![opcodes::LDIV]
    }
    
    pub fn fdiv(&self) -> Vec<u8> {
        vec![opcodes::FDIV]
    }
    
    pub fn ddiv(&self) -> Vec<u8> {
        vec![opcodes::DDIV]
    }
    
    pub fn irem(&self) -> Vec<u8> {
        vec![opcodes::IREM]
    }
    
    pub fn lrem(&self) -> Vec<u8> {
        vec![opcodes::LREM]
    }
    
    pub fn frem(&self) -> Vec<u8> {
        vec![opcodes::FREM]
    }
    
    pub fn drem(&self) -> Vec<u8> {
        vec![opcodes::DREM]
    }
    
    pub fn ineg(&self) -> Vec<u8> {
        vec![opcodes::INEG]
    }
    
    pub fn lneg(&self) -> Vec<u8> {
        vec![opcodes::LNEG]
    }
    
    pub fn fneg(&self) -> Vec<u8> {
        vec![opcodes::FNEG]
    }
    
    pub fn dneg(&self) -> Vec<u8> {
        vec![opcodes::DNEG]
    }
    
    // Bit operations
    pub fn ishl(&self) -> Vec<u8> {
        vec![opcodes::ISHL]
    }
    
    pub fn lshl(&self) -> Vec<u8> {
        vec![opcodes::LSHL]
    }
    
    pub fn ishr(&self) -> Vec<u8> {
        vec![opcodes::ISHR]
    }
    
    pub fn lshr(&self) -> Vec<u8> {
        vec![opcodes::LSHR]
    }
    
    pub fn iushr(&self) -> Vec<u8> {
        vec![opcodes::IUSHR]
    }
    
    pub fn lushr(&self) -> Vec<u8> {
        vec![opcodes::LUSHR]
    }
    
    pub fn iand(&self) -> Vec<u8> {
        vec![opcodes::IAND]
    }
    
    pub fn land(&self) -> Vec<u8> {
        vec![opcodes::LAND]
    }
    
    pub fn ior(&self) -> Vec<u8> {
        vec![opcodes::IOR]
    }
    
    pub fn lor(&self) -> Vec<u8> {
        vec![opcodes::LOR]
    }
    
    pub fn ixor(&self) -> Vec<u8> {
        vec![opcodes::IXOR]
    }
    
    pub fn lxor(&self) -> Vec<u8> {
        vec![opcodes::LXOR]
    }
    
    // Increment
    pub fn iinc(&self, index: u16, delta: i16) -> Vec<u8> {
        if index <= 0xFF && delta >= -128 && delta <= 127 {
            vec![opcodes::IINC, index as u8, delta as u8]
        } else {
            let index_bytes = index.to_be_bytes();
            let delta_bytes = delta.to_be_bytes();
            vec![opcodes::WIDE, opcodes::IINC, index_bytes[0], index_bytes[1], delta_bytes[0], delta_bytes[1]]
        }
    }
    
    // Type conversions
    pub fn i2l(&self) -> Vec<u8> {
        vec![opcodes::I2L]
    }
    
    pub fn i2f(&self) -> Vec<u8> {
        vec![opcodes::I2F]
    }
    
    pub fn i2d(&self) -> Vec<u8> {
        vec![opcodes::I2D]
    }
    
    pub fn l2i(&self) -> Vec<u8> {
        vec![opcodes::L2I]
    }
    
    pub fn l2f(&self) -> Vec<u8> {
        vec![opcodes::L2F]
    }
    
    pub fn l2d(&self) -> Vec<u8> {
        vec![opcodes::L2D]
    }
    
    pub fn f2i(&self) -> Vec<u8> {
        vec![opcodes::F2I]
    }
    
    pub fn f2l(&self) -> Vec<u8> {
        vec![opcodes::F2L]
    }
    
    pub fn f2d(&self) -> Vec<u8> {
        vec![opcodes::F2D]
    }
    
    pub fn d2i(&self) -> Vec<u8> {
        vec![opcodes::D2I]
    }
    
    pub fn d2l(&self) -> Vec<u8> {
        vec![opcodes::D2L]
    }
    
    pub fn d2f(&self) -> Vec<u8> {
        vec![opcodes::D2F]
    }
    
    pub fn i2b(&self) -> Vec<u8> {
        vec![opcodes::I2B]
    }
    
    pub fn i2c(&self) -> Vec<u8> {
        vec![opcodes::I2C]
    }
    
    pub fn i2s(&self) -> Vec<u8> {
        vec![opcodes::I2S]
    }
    
    // Comparisons
    pub fn lcmp(&self) -> Vec<u8> {
        vec![opcodes::LCMP]
    }
    
    pub fn fcmpl(&self) -> Vec<u8> {
        vec![opcodes::FCMPL]
    }
    
    pub fn fcmpg(&self) -> Vec<u8> {
        vec![opcodes::FCMPG]
    }
    
    pub fn dcmpl(&self) -> Vec<u8> {
        vec![opcodes::DCMPL]
    }
    
    pub fn dcmpg(&self) -> Vec<u8> {
        vec![opcodes::DCMPG]
    }
    
    // Control flow
    pub fn ifeq(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFEQ, bytes[0], bytes[1]]
    }
    
    pub fn ifne(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFNE, bytes[0], bytes[1]]
    }
    
    pub fn iflt(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFLT, bytes[0], bytes[1]]
    }
    
    pub fn ifge(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFGE, bytes[0], bytes[1]]
    }
    
    pub fn ifgt(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFGT, bytes[0], bytes[1]]
    }
    
    pub fn ifle(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFLE, bytes[0], bytes[1]]
    }
    
    pub fn if_icmpeq(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ICMPEQ, bytes[0], bytes[1]]
    }
    
    pub fn if_icmpne(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ICMPNE, bytes[0], bytes[1]]
    }
    
    pub fn if_icmplt(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ICMPLT, bytes[0], bytes[1]]
    }
    
    pub fn if_icmpge(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ICMPGE, bytes[0], bytes[1]]
    }
    
    pub fn if_icmpgt(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ICMPGT, bytes[0], bytes[1]]
    }
    
    pub fn if_icmple(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ICMPLE, bytes[0], bytes[1]]
    }
    
    pub fn if_acmpeq(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ACMPEQ, bytes[0], bytes[1]]
    }
    
    pub fn if_acmpne(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IF_ACMPNE, bytes[0], bytes[1]]
    }
    
    pub fn ifnull(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFNULL, bytes[0], bytes[1]]
    }
    
    pub fn ifnonnull(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::IFNONNULL, bytes[0], bytes[1]]
    }
    
    pub fn goto(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::GOTO, bytes[0], bytes[1]]
    }
    
    pub fn goto_w(&self, offset: i32) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::GOTO_W, bytes[0], bytes[1], bytes[2], bytes[3]]
    }
    
    pub fn jsr(&self, offset: i16) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::JSR, bytes[0], bytes[1]]
    }
    
    pub fn jsr_w(&self, offset: i32) -> Vec<u8> {
        let bytes = offset.to_be_bytes();
        vec![opcodes::JSR_W, bytes[0], bytes[1], bytes[2], bytes[3]]
    }
    
    pub fn ret(&self, index: u8) -> Vec<u8> {
        vec![opcodes::RET, index]
    }
    
    // Returns
    pub fn ireturn(&self) -> Vec<u8> {
        vec![opcodes::IRETURN]
    }
    
    pub fn lreturn(&self) -> Vec<u8> {
        vec![opcodes::LRETURN]
    }
    
    pub fn freturn(&self) -> Vec<u8> {
        vec![opcodes::FRETURN]
    }
    
    pub fn dreturn(&self) -> Vec<u8> {
        vec![opcodes::DRETURN]
    }
    
    pub fn areturn(&self) -> Vec<u8> {
        vec![opcodes::ARETURN]
    }
    
    pub fn return_void(&self) -> Vec<u8> {
        vec![opcodes::RETURN]
    }
    
    // Field access
    pub fn getstatic(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::GETSTATIC, bytes[0], bytes[1]]
    }
    
    pub fn putstatic(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::PUTSTATIC, bytes[0], bytes[1]]
    }
    
    pub fn getfield(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::GETFIELD, bytes[0], bytes[1]]
    }
    
    pub fn putfield(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::PUTFIELD, bytes[0], bytes[1]]
    }
    
    // Method invocation
    pub fn invokevirtual(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::INVOKEVIRTUAL, bytes[0], bytes[1]]
    }
    
    pub fn invokespecial(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::INVOKESPECIAL, bytes[0], bytes[1]]
    }
    
    pub fn invokestatic(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::INVOKESTATIC, bytes[0], bytes[1]]
    }
    
    pub fn invokeinterface(&self, index: u16, count: u8) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::INVOKEINTERFACE, bytes[0], bytes[1], count, 0]
    }
    
    pub fn invokedynamic(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::INVOKEDYNAMIC, bytes[0], bytes[1], 0, 0]
    }
    
    // Object creation and manipulation
    pub fn new_object(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::NEW, bytes[0], bytes[1]]
    }
    
    pub fn newarray(&self, atype: u8) -> Vec<u8> {
        vec![opcodes::NEWARRAY, atype]
    }
    
    pub fn anewarray(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::ANEWARRAY, bytes[0], bytes[1]]
    }
    
    pub fn multianewarray(&self, index: u16, dimensions: u8) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::MULTIANEWARRAY, bytes[0], bytes[1], dimensions]
    }
    
    pub fn arraylength(&self) -> Vec<u8> {
        vec![opcodes::ARRAYLENGTH]
    }
    
    pub fn athrow(&self) -> Vec<u8> {
        vec![opcodes::ATHROW]
    }
    
    pub fn checkcast(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::CHECKCAST, bytes[0], bytes[1]]
    }
    
    pub fn instanceof(&self, index: u16) -> Vec<u8> {
        let bytes = index.to_be_bytes();
        vec![opcodes::INSTANCEOF, bytes[0], bytes[1]]
    }
    
    pub fn monitorenter(&self) -> Vec<u8> {
        vec![opcodes::MONITORENTER]
    }
    
    pub fn monitorexit(&self) -> Vec<u8> {
        vec![opcodes::MONITOREXIT]
    }
    
    // Other
    pub fn nop(&self) -> Vec<u8> {
        vec![opcodes::NOP]
    }
    
    pub fn wide(&self) -> Vec<u8> {
        vec![opcodes::WIDE]
    }
    
    pub fn tableswitch(&self, default_offset: i32, low: i32, high: i32, offsets: &[i32]) -> Vec<u8> {
        let mut code = vec![opcodes::TABLESWITCH];
        
        // Pad to 4-byte boundary
        let padding = (4 - (code.len() % 4)) % 4;
        code.extend(vec![0; padding]);
        
        // Add default, low, high
        code.extend_from_slice(&default_offset.to_be_bytes());
        code.extend_from_slice(&low.to_be_bytes());
        code.extend_from_slice(&high.to_be_bytes());
        
        // Add jump offsets
        for &offset in offsets {
            code.extend_from_slice(&offset.to_be_bytes());
        }
        
        code
    }
    
    pub fn lookupswitch(&self, default_offset: i32, npairs: i32, pairs: &[(i32, i32)]) -> Vec<u8> {
        let mut code = vec![opcodes::LOOKUPSWITCH];
        
        // Pad to 4-byte boundary
        let padding = (4 - (code.len() % 4)) % 4;
        code.extend(vec![0; padding]);
        
        // Add default, npairs
        code.extend_from_slice(&default_offset.to_be_bytes());
        code.extend_from_slice(&npairs.to_be_bytes());
        
        // Add match-offset pairs
        for &(match_value, offset) in pairs {
            code.extend_from_slice(&match_value.to_be_bytes());
            code.extend_from_slice(&offset.to_be_bytes());
        }
        
        code
    }
}
