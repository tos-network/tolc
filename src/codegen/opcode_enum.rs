/// Java bytecode opcode enumeration for type-safe instruction handling
/// 
/// This enum provides a type-safe way to represent Java bytecode instructions,
/// particularly useful for jump instruction analysis and optimization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    // Constants
    Nop,
    AconstNull,
    IconstM1,
    Iconst0,
    Iconst1,
    Iconst2,
    Iconst3,
    Iconst4,
    Iconst5,
    Lconst0,
    Lconst1,
    Fconst0,
    Fconst1,
    Fconst2,
    Dconst0,
    Dconst1,
    
    // Loads
    Bipush,
    Sipush,
    Ldc,
    LdcW,
    Ldc2W,
    
    // Local variable loads
    Iload,
    Lload,
    Fload,
    Dload,
    Aload,
    
    // Local variable stores
    Istore,
    Lstore,
    Fstore,
    Dstore,
    Astore,
    
    // Array operations
    Iaload,
    Laload,
    Faload,
    Daload,
    Aaload,
    Baload,
    Caload,
    Saload,
    
    Iastore,
    Lastore,
    Fastore,
    Dastore,
    Aastore,
    Bastore,
    Castore,
    Sastore,
    
    // Stack operations
    Pop,
    Pop2,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    Swap,
    
    // Arithmetic operations
    Iadd,
    Ladd,
    Fadd,
    Dadd,
    Isub,
    Lsub,
    Fsub,
    Dsub,
    Imul,
    Lmul,
    Fmul,
    Dmul,
    Idiv,
    Ldiv,
    Fdiv,
    Ddiv,
    Irem,
    Lrem,
    Frem,
    Drem,
    Ineg,
    Lneg,
    Fneg,
    Dneg,
    
    // Bitwise operations
    Ishl,
    Lshl,
    Ishr,
    Lshr,
    Iushr,
    Lushr,
    Iand,
    Land,
    Ior,
    Lor,
    Ixor,
    Lxor,
    
    // Increment
    Iinc,
    
    // Conversions
    I2l,
    I2f,
    I2d,
    L2i,
    L2f,
    L2d,
    F2i,
    F2l,
    F2d,
    D2i,
    D2l,
    D2f,
    I2b,
    I2c,
    I2s,
    
    // Comparisons
    Lcmp,
    Fcmpl,
    Fcmpg,
    Dcmpl,
    Dcmpg,
    
    // Conditional branches (most important for jump analysis)
    Ifeq,
    Ifne,
    Iflt,
    Ifge,
    Ifgt,
    Ifle,
    IfIcmpeq,
    IfIcmpne,
    IfIcmplt,
    IfIcmpge,
    IfIcmpgt,
    IfIcmple,
    IfAcmpeq,
    IfAcmpne,
    
    // Unconditional branches
    Goto,
    Jsr,
    Ret,
    
    // Wide versions for fatcode
    GotoW,
    JsrW,
    
    // Switch statements
    Tableswitch,
    Lookupswitch,
    
    // Returns
    Ireturn,
    Lreturn,
    Freturn,
    Dreturn,
    Areturn,
    Return,
    
    // Field operations
    Getstatic,
    Putstatic,
    Getfield,
    Putfield,
    
    // Method invocation
    Invokevirtual,
    Invokespecial,
    Invokestatic,
    Invokeinterface,
    Invokedynamic,
    
    // Object operations
    New,
    Newarray,
    Anewarray,
    Arraylength,
    Athrow,
    Checkcast,
    Instanceof,
    
    // Synchronization
    Monitorenter,
    Monitorexit,
    
    // Wide instruction
    Wide,
    
    // Multidimensional arrays
    Multianewarray,
    
    // Null checks
    Ifnull,
    Ifnonnull,
    
    // Reserved opcodes
    Breakpoint,
    Impdep1,
    Impdep2,
}

impl Opcode {
    /// Get the bytecode value for this opcode
    pub fn to_byte(self) -> u8 {
        use crate::codegen::opcodes::*;
        
        match self {
            Opcode::Nop => NOP,
            Opcode::AconstNull => ACONST_NULL,
            Opcode::IconstM1 => ICONST_M1,
            Opcode::Iconst0 => ICONST_0,
            Opcode::Iconst1 => ICONST_1,
            Opcode::Iconst2 => ICONST_2,
            Opcode::Iconst3 => ICONST_3,
            Opcode::Iconst4 => ICONST_4,
            Opcode::Iconst5 => ICONST_5,
            Opcode::Lconst0 => LCONST_0,
            Opcode::Lconst1 => LCONST_1,
            Opcode::Fconst0 => FCONST_0,
            Opcode::Fconst1 => FCONST_1,
            Opcode::Fconst2 => FCONST_2,
            Opcode::Dconst0 => DCONST_0,
            Opcode::Dconst1 => DCONST_1,
            
            Opcode::Bipush => BIPUSH,
            Opcode::Sipush => SIPUSH,
            Opcode::Ldc => LDC,
            Opcode::LdcW => LDC_W,
            Opcode::Ldc2W => LDC2_W,
            
            Opcode::Iload => ILOAD,
            Opcode::Lload => LLOAD,
            Opcode::Fload => FLOAD,
            Opcode::Dload => DLOAD,
            Opcode::Aload => ALOAD,
            
            Opcode::Istore => ISTORE,
            Opcode::Lstore => LSTORE,
            Opcode::Fstore => FSTORE,
            Opcode::Dstore => DSTORE,
            Opcode::Astore => ASTORE,
            
            Opcode::Iaload => IALOAD,
            Opcode::Laload => LALOAD,
            Opcode::Faload => FALOAD,
            Opcode::Daload => DALOAD,
            Opcode::Aaload => AALOAD,
            Opcode::Baload => BALOAD,
            Opcode::Caload => CALOAD,
            Opcode::Saload => SALOAD,
            
            Opcode::Iastore => IASTORE,
            Opcode::Lastore => LASTORE,
            Opcode::Fastore => FASTORE,
            Opcode::Dastore => DASTORE,
            Opcode::Aastore => AASTORE,
            Opcode::Bastore => BASTORE,
            Opcode::Castore => CASTORE,
            Opcode::Sastore => SASTORE,
            
            Opcode::Pop => POP,
            Opcode::Pop2 => POP2,
            Opcode::Dup => DUP,
            Opcode::DupX1 => DUP_X1,
            Opcode::DupX2 => DUP_X2,
            Opcode::Dup2 => DUP2,
            Opcode::Dup2X1 => DUP2_X1,
            Opcode::Dup2X2 => DUP2_X2,
            Opcode::Swap => SWAP,
            
            Opcode::Iadd => IADD,
            Opcode::Ladd => LADD,
            Opcode::Fadd => FADD,
            Opcode::Dadd => DADD,
            Opcode::Isub => ISUB,
            Opcode::Lsub => LSUB,
            Opcode::Fsub => FSUB,
            Opcode::Dsub => DSUB,
            Opcode::Imul => IMUL,
            Opcode::Lmul => LMUL,
            Opcode::Fmul => FMUL,
            Opcode::Dmul => DMUL,
            Opcode::Idiv => IDIV,
            Opcode::Ldiv => LDIV,
            Opcode::Fdiv => FDIV,
            Opcode::Ddiv => DDIV,
            Opcode::Irem => IREM,
            Opcode::Lrem => LREM,
            Opcode::Frem => FREM,
            Opcode::Drem => DREM,
            Opcode::Ineg => INEG,
            Opcode::Lneg => LNEG,
            Opcode::Fneg => FNEG,
            Opcode::Dneg => DNEG,
            
            Opcode::Ishl => ISHL,
            Opcode::Lshl => LSHL,
            Opcode::Ishr => ISHR,
            Opcode::Lshr => LSHR,
            Opcode::Iushr => IUSHR,
            Opcode::Lushr => LUSHR,
            Opcode::Iand => IAND,
            Opcode::Land => LAND,
            Opcode::Ior => IOR,
            Opcode::Lor => LOR,
            Opcode::Ixor => IXOR,
            Opcode::Lxor => LXOR,
            
            Opcode::Iinc => IINC,
            
            Opcode::I2l => I2L,
            Opcode::I2f => I2F,
            Opcode::I2d => I2D,
            Opcode::L2i => L2I,
            Opcode::L2f => L2F,
            Opcode::L2d => L2D,
            Opcode::F2i => F2I,
            Opcode::F2l => F2L,
            Opcode::F2d => F2D,
            Opcode::D2i => D2I,
            Opcode::D2l => D2L,
            Opcode::D2f => D2F,
            Opcode::I2b => I2B,
            Opcode::I2c => I2C,
            Opcode::I2s => I2S,
            
            Opcode::Lcmp => LCMP,
            Opcode::Fcmpl => FCMPL,
            Opcode::Fcmpg => FCMPG,
            Opcode::Dcmpl => DCMPL,
            Opcode::Dcmpg => DCMPG,
            
            Opcode::Ifeq => IFEQ,
            Opcode::Ifne => IFNE,
            Opcode::Iflt => IFLT,
            Opcode::Ifge => IFGE,
            Opcode::Ifgt => IFGT,
            Opcode::Ifle => IFLE,
            Opcode::IfIcmpeq => IF_ICMPEQ,
            Opcode::IfIcmpne => IF_ICMPNE,
            Opcode::IfIcmplt => IF_ICMPLT,
            Opcode::IfIcmpge => IF_ICMPGE,
            Opcode::IfIcmpgt => IF_ICMPGT,
            Opcode::IfIcmple => IF_ICMPLE,
            Opcode::IfAcmpeq => IF_ACMPEQ,
            Opcode::IfAcmpne => IF_ACMPNE,
            
            Opcode::Goto => GOTO,
            Opcode::Jsr => JSR,
            Opcode::Ret => RET,
            
            Opcode::GotoW => GOTO_W,
            Opcode::JsrW => JSR_W,
            
            Opcode::Tableswitch => TABLESWITCH,
            Opcode::Lookupswitch => LOOKUPSWITCH,
            
            Opcode::Ireturn => IRETURN,
            Opcode::Lreturn => LRETURN,
            Opcode::Freturn => FRETURN,
            Opcode::Dreturn => DRETURN,
            Opcode::Areturn => ARETURN,
            Opcode::Return => RETURN,
            
            Opcode::Getstatic => GETSTATIC,
            Opcode::Putstatic => PUTSTATIC,
            Opcode::Getfield => GETFIELD,
            Opcode::Putfield => PUTFIELD,
            
            Opcode::Invokevirtual => INVOKEVIRTUAL,
            Opcode::Invokespecial => INVOKESPECIAL,
            Opcode::Invokestatic => INVOKESTATIC,
            Opcode::Invokeinterface => INVOKEINTERFACE,
            Opcode::Invokedynamic => INVOKEDYNAMIC,
            
            Opcode::New => NEW,
            Opcode::Newarray => NEWARRAY,
            Opcode::Anewarray => ANEWARRAY,
            Opcode::Arraylength => ARRAYLENGTH,
            Opcode::Athrow => ATHROW,
            Opcode::Checkcast => CHECKCAST,
            Opcode::Instanceof => INSTANCEOF,
            
            Opcode::Monitorenter => MONITORENTER,
            Opcode::Monitorexit => MONITOREXIT,
            
            Opcode::Wide => WIDE,
            
            Opcode::Multianewarray => MULTIANEWARRAY,
            
            Opcode::Ifnull => IFNULL,
            Opcode::Ifnonnull => IFNONNULL,
            
            Opcode::Breakpoint => BREAKPOINT,
            Opcode::Impdep1 => IMPDEP1,
            Opcode::Impdep2 => IMPDEP2,
        }
    }
    
    /// Check if this opcode is a conditional branch
    pub fn is_conditional_branch(self) -> bool {
        matches!(self, 
            Opcode::Ifeq | Opcode::Ifne | Opcode::Iflt | Opcode::Ifge | Opcode::Ifgt | Opcode::Ifle |
            Opcode::IfIcmpeq | Opcode::IfIcmpne | Opcode::IfIcmplt | Opcode::IfIcmpge | 
            Opcode::IfIcmpgt | Opcode::IfIcmple | Opcode::IfAcmpeq | Opcode::IfAcmpne |
            Opcode::Ifnull | Opcode::Ifnonnull
        )
    }
    
    /// Check if this opcode is an unconditional branch
    pub fn is_unconditional_branch(self) -> bool {
        matches!(self, Opcode::Goto | Opcode::GotoW | Opcode::Jsr | Opcode::JsrW)
    }
    
    /// Check if this opcode is a return instruction
    pub fn is_return(self) -> bool {
        matches!(self, 
            Opcode::Ireturn | Opcode::Lreturn | Opcode::Freturn | 
            Opcode::Dreturn | Opcode::Areturn | Opcode::Return
        )
    }
    
    /// Check if this opcode is a terminal instruction (ends control flow)
    pub fn is_terminal(self) -> bool {
        self.is_return() || matches!(self, Opcode::Athrow)
    }
    
    /// Check if this opcode is a wide version
    pub fn is_wide(self) -> bool {
        matches!(self, Opcode::GotoW | Opcode::JsrW)
    }
    
    /// Get the instruction size in bytes (without operands)
    pub fn instruction_size(self) -> u32 {
        match self {
            // Most instructions are 1 byte + operands
            Opcode::Bipush | Opcode::Sipush | Opcode::Ldc => 2,
            Opcode::LdcW | Opcode::Ldc2W => 3,
            Opcode::Iload | Opcode::Lload | Opcode::Fload | Opcode::Dload | Opcode::Aload => 2,
            Opcode::Istore | Opcode::Lstore | Opcode::Fstore | Opcode::Dstore | Opcode::Astore => 2,
            
            // Conditional branches: 1 byte opcode + 2 byte offset
            _ if self.is_conditional_branch() => 3,
            
            // Unconditional branches
            Opcode::Goto | Opcode::Jsr => 3,
            Opcode::GotoW | Opcode::JsrW => 5, // Wide versions use 4-byte offset
            
            // Field and method operations
            Opcode::Getstatic | Opcode::Putstatic | Opcode::Getfield | Opcode::Putfield => 3,
            Opcode::Invokevirtual | Opcode::Invokespecial | Opcode::Invokestatic => 3,
            Opcode::Invokeinterface => 5,
            Opcode::Invokedynamic => 5,
            
            // Object operations
            Opcode::New | Opcode::Anewarray | Opcode::Checkcast | Opcode::Instanceof => 3,
            Opcode::Multianewarray => 4,
            
            // Switch instructions (variable size, return minimum)
            Opcode::Tableswitch | Opcode::Lookupswitch => 1, // Variable size
            
            // Most other instructions are single byte
            _ => 1,
        }
    }
}

impl From<u8> for Opcode {
    fn from(byte: u8) -> Self {
        use crate::codegen::opcodes::*;
        
        match byte {
            NOP => Opcode::Nop,
            ACONST_NULL => Opcode::AconstNull,
            ICONST_M1 => Opcode::IconstM1,
            ICONST_0 => Opcode::Iconst0,
            ICONST_1 => Opcode::Iconst1,
            ICONST_2 => Opcode::Iconst2,
            ICONST_3 => Opcode::Iconst3,
            ICONST_4 => Opcode::Iconst4,
            ICONST_5 => Opcode::Iconst5,
            LCONST_0 => Opcode::Lconst0,
            LCONST_1 => Opcode::Lconst1,
            FCONST_0 => Opcode::Fconst0,
            FCONST_1 => Opcode::Fconst1,
            FCONST_2 => Opcode::Fconst2,
            DCONST_0 => Opcode::Dconst0,
            DCONST_1 => Opcode::Dconst1,
            
            BIPUSH => Opcode::Bipush,
            SIPUSH => Opcode::Sipush,
            LDC => Opcode::Ldc,
            LDC_W => Opcode::LdcW,
            LDC2_W => Opcode::Ldc2W,
            
            ILOAD => Opcode::Iload,
            LLOAD => Opcode::Lload,
            FLOAD => Opcode::Fload,
            DLOAD => Opcode::Dload,
            ALOAD => Opcode::Aload,
            
            ISTORE => Opcode::Istore,
            LSTORE => Opcode::Lstore,
            FSTORE => Opcode::Fstore,
            DSTORE => Opcode::Dstore,
            ASTORE => Opcode::Astore,
            
            GOTO => Opcode::Goto,
            JSR => Opcode::Jsr,
            RET => Opcode::Ret,
            GOTO_W => Opcode::GotoW,
            JSR_W => Opcode::JsrW,
            
            IFEQ => Opcode::Ifeq,
            IFNE => Opcode::Ifne,
            IFLT => Opcode::Iflt,
            IFGE => Opcode::Ifge,
            IFGT => Opcode::Ifgt,
            IFLE => Opcode::Ifle,
            IF_ICMPEQ => Opcode::IfIcmpeq,
            IF_ICMPNE => Opcode::IfIcmpne,
            IF_ICMPLT => Opcode::IfIcmplt,
            IF_ICMPGE => Opcode::IfIcmpge,
            IF_ICMPGT => Opcode::IfIcmpgt,
            IF_ICMPLE => Opcode::IfIcmple,
            IF_ACMPEQ => Opcode::IfAcmpeq,
            IF_ACMPNE => Opcode::IfAcmpne,
            
            IFNULL => Opcode::Ifnull,
            IFNONNULL => Opcode::Ifnonnull,
            
            IRETURN => Opcode::Ireturn,
            LRETURN => Opcode::Lreturn,
            FRETURN => Opcode::Freturn,
            DRETURN => Opcode::Dreturn,
            ARETURN => Opcode::Areturn,
            RETURN => Opcode::Return,
            
            ATHROW => Opcode::Athrow,
            
            // Default to NOP for unknown opcodes
            _ => Opcode::Nop,
        }
    }
}
