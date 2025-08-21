/// Java bytecode instruction opcodes
/// 
/// All opcodes are defined according to the Java Virtual Machine Specification.
/// Values are in hexadecimal format and ordered by opcode value.

// 0x00 - 0x0F: Constants and basic operations
pub const NOP: u8 = 0x00;
pub const ACONST_NULL: u8 = 0x01;
pub const ICONST_M1: u8 = 0x02;
pub const ICONST_0: u8 = 0x03;
pub const ICONST_1: u8 = 0x04;
pub const ICONST_2: u8 = 0x05;
pub const ICONST_3: u8 = 0x06;
pub const ICONST_4: u8 = 0x07;
pub const ICONST_5: u8 = 0x08;
pub const LCONST_0: u8 = 0x09;
pub const LCONST_1: u8 = 0x0a;
pub const FCONST_0: u8 = 0x0b;
pub const FCONST_1: u8 = 0x0c;
pub const FCONST_2: u8 = 0x0d;
pub const DCONST_0: u8 = 0x0e;
pub const DCONST_1: u8 = 0x0f;
    
// 0x10 - 0x14: Extended constants and loads
pub const BIPUSH: u8 = 0x10;
pub const SIPUSH: u8 = 0x11;
pub const LDC: u8 = 0x12;
pub const LDC_W: u8 = 0x13;
pub const LDC2_W: u8 = 0x14;

// 0x15 - 0x35: Loads
pub const ILOAD: u8 = 0x15;
pub const LLOAD: u8 = 0x16;
pub const FLOAD: u8 = 0x17;
pub const DLOAD: u8 = 0x18;
pub const ALOAD: u8 = 0x19;
pub const ILOAD_0: u8 = 0x1a;
pub const ILOAD_1: u8 = 0x1b;
pub const ILOAD_2: u8 = 0x1c;
pub const ILOAD_3: u8 = 0x1d;
pub const LLOAD_0: u8 = 0x1e;
pub const LLOAD_1: u8 = 0x1f;
pub const LLOAD_2: u8 = 0x20;
pub const LLOAD_3: u8 = 0x21;
pub const FLOAD_0: u8 = 0x22;
pub const FLOAD_1: u8 = 0x23;
pub const FLOAD_2: u8 = 0x24;
pub const FLOAD_3: u8 = 0x25;
pub const DLOAD_0: u8 = 0x26;
pub const DLOAD_1: u8 = 0x27;
pub const DLOAD_2: u8 = 0x28;
pub const DLOAD_3: u8 = 0x29;
pub const ALOAD_0: u8 = 0x2a;
pub const ALOAD_1: u8 = 0x2b;
pub const ALOAD_2: u8 = 0x2c;
pub const ALOAD_3: u8 = 0x2d;

// 0x2E - 0x35: Array loads
pub const IALOAD: u8 = 0x2e;
pub const LALOAD: u8 = 0x2f;
pub const FALOAD: u8 = 0x30;
pub const DALOAD: u8 = 0x31;
pub const AALOAD: u8 = 0x32;
pub const BALOAD: u8 = 0x33;
pub const CALOAD: u8 = 0x34;
pub const SALOAD: u8 = 0x35;

// 0x36 - 0x4E: Stores
pub const ISTORE: u8 = 0x36;
pub const LSTORE: u8 = 0x37;
pub const FSTORE: u8 = 0x38;
pub const DSTORE: u8 = 0x39;
pub const ASTORE: u8 = 0x3a;
pub const ISTORE_0: u8 = 0x3b;
pub const ISTORE_1: u8 = 0x3c;
pub const ISTORE_2: u8 = 0x3d;
pub const ISTORE_3: u8 = 0x3e;
pub const LSTORE_0: u8 = 0x3f;
pub const LSTORE_1: u8 = 0x40;
pub const LSTORE_2: u8 = 0x41;
pub const LSTORE_3: u8 = 0x42;
pub const FSTORE_0: u8 = 0x43;
pub const FSTORE_1: u8 = 0x44;
pub const FSTORE_2: u8 = 0x45;
pub const FSTORE_3: u8 = 0x46;
pub const DSTORE_0: u8 = 0x47;
pub const DSTORE_1: u8 = 0x48;
pub const DSTORE_2: u8 = 0x49;
pub const DSTORE_3: u8 = 0x4a;
pub const ASTORE_0: u8 = 0x4b;
pub const ASTORE_1: u8 = 0x4c;
pub const ASTORE_2: u8 = 0x4d;
pub const ASTORE_3: u8 = 0x4e;

// 0x4F - 0x56: Array stores
pub const IASTORE: u8 = 0x4f;
pub const LASTORE: u8 = 0x50;
pub const FASTORE: u8 = 0x51;
pub const DASTORE: u8 = 0x52;
pub const AASTORE: u8 = 0x53;
pub const BASTORE: u8 = 0x54;
pub const CASTORE: u8 = 0x55;
pub const SASTORE: u8 = 0x56;

// 0x57 - 0x5F: Stack operations
pub const POP: u8 = 0x57;
pub const POP2: u8 = 0x58;
pub const DUP: u8 = 0x59;
pub const DUP_X1: u8 = 0x5a;
pub const DUP_X2: u8 = 0x5b;
pub const DUP2: u8 = 0x5c;
pub const DUP2_X1: u8 = 0x5d;
pub const DUP2_X2: u8 = 0x5e;
pub const SWAP: u8 = 0x5f;

// 0x60 - 0x77: Arithmetic operations
pub const IADD: u8 = 0x60;
pub const LADD: u8 = 0x61;
pub const FADD: u8 = 0x62;
pub const DADD: u8 = 0x63;
pub const ISUB: u8 = 0x64;
pub const LSUB: u8 = 0x65;
pub const FSUB: u8 = 0x66;
pub const DSUB: u8 = 0x67;
pub const IMUL: u8 = 0x68;
pub const LMUL: u8 = 0x69;
pub const FMUL: u8 = 0x6a;
pub const DMUL: u8 = 0x6b;
pub const IDIV: u8 = 0x6c;
pub const LDIV: u8 = 0x6d;
pub const FDIV: u8 = 0x6e;
pub const DDIV: u8 = 0x6f;
pub const IREM: u8 = 0x70;
pub const LREM: u8 = 0x71;
pub const FREM: u8 = 0x72;
pub const DREM: u8 = 0x73;
pub const INEG: u8 = 0x74;
pub const LNEG: u8 = 0x75;
pub const FNEG: u8 = 0x76;
pub const DNEG: u8 = 0x77;

// 0x78 - 0x83: Shifts and logical operations
pub const ISHL: u8 = 0x78;
pub const LSHL: u8 = 0x79;
pub const ISHR: u8 = 0x7a;
pub const LSHR: u8 = 0x7b;
pub const IUSHR: u8 = 0x7c;
pub const LUSHR: u8 = 0x7d;
pub const IAND: u8 = 0x7e;
pub const LAND: u8 = 0x7f;
pub const IOR: u8 = 0x80;
pub const LOR: u8 = 0x81;
pub const IXOR: u8 = 0x82;
pub const LXOR: u8 = 0x83;

// 0x84: Increment
pub const IINC: u8 = 0x84;

// 0x85 - 0x93: Type conversions
pub const I2L: u8 = 0x85;
pub const I2F: u8 = 0x86;
pub const I2D: u8 = 0x87;
pub const L2I: u8 = 0x88;
pub const L2F: u8 = 0x89;
pub const L2D: u8 = 0x8a;
pub const F2I: u8 = 0x8b;
pub const F2L: u8 = 0x8c;
pub const F2D: u8 = 0x8d;
pub const D2I: u8 = 0x8e;
pub const D2L: u8 = 0x8f;
pub const D2F: u8 = 0x90;
pub const I2B: u8 = 0x91;
pub const I2C: u8 = 0x92;
pub const I2S: u8 = 0x93;

// 0x94 - 0x98: Comparisons
pub const LCMP: u8 = 0x94;
pub const FCMPL: u8 = 0x95;
pub const FCMPG: u8 = 0x96;
pub const DCMPL: u8 = 0x97;
pub const DCMPG: u8 = 0x98;

// 0x99 - 0xB1: Control flow
pub const IFEQ: u8 = 0x99;
pub const IFNE: u8 = 0x9a;
pub const IFLT: u8 = 0x9b;
pub const IFGE: u8 = 0x9c;
pub const IFGT: u8 = 0x9d;
pub const IFLE: u8 = 0x9e;
pub const IF_ICMPEQ: u8 = 0x9f;
pub const IF_ICMPNE: u8 = 0xa0;
pub const IF_ICMPLT: u8 = 0xa1;
pub const IF_ICMPGE: u8 = 0xa2;
pub const IF_ICMPGT: u8 = 0xa3;
pub const IF_ICMPLE: u8 = 0xa4;
pub const IF_ACMPEQ: u8 = 0xa5;
pub const IF_ACMPNE: u8 = 0xa6;
pub const GOTO: u8 = 0xa7;
pub const JSR: u8 = 0xa8;
pub const RET: u8 = 0xa9;
pub const TABLESWITCH: u8 = 0xaa;
pub const LOOKUPSWITCH: u8 = 0xab;
pub const IRETURN: u8 = 0xac;
pub const LRETURN: u8 = 0xad;
pub const FRETURN: u8 = 0xae;
pub const DRETURN: u8 = 0xaf;
pub const ARETURN: u8 = 0xb0;
pub const RETURN: u8 = 0xb1;

// 0xB2 - 0xC3: References and object operations
pub const GETSTATIC: u8 = 0xb2;
pub const PUTSTATIC: u8 = 0xb3;
pub const GETFIELD: u8 = 0xb4;
pub const PUTFIELD: u8 = 0xb5;
pub const INVOKEVIRTUAL: u8 = 0xb6;
pub const INVOKESPECIAL: u8 = 0xb7;
pub const INVOKESTATIC: u8 = 0xb8;
pub const INVOKEINTERFACE: u8 = 0xb9;
pub const INVOKEDYNAMIC: u8 = 0xba;
pub const NEW: u8 = 0xbb;
pub const NEWARRAY: u8 = 0xbc;
pub const ANEWARRAY: u8 = 0xbd;
pub const ARRAYLENGTH: u8 = 0xbe;
pub const ATHROW: u8 = 0xbf;
pub const CHECKCAST: u8 = 0xc0;
pub const INSTANCEOF: u8 = 0xc1;
pub const MONITORENTER: u8 = 0xc2;
pub const MONITOREXIT: u8 = 0xc3;

// 0xC4 - 0xC9: Extended instructions
pub const WIDE: u8 = 0xc4;
pub const MULTIANEWARRAY: u8 = 0xc5;
pub const IFNULL: u8 = 0xc6;
pub const IFNONNULL: u8 = 0xc7;
pub const GOTO_W: u8 = 0xc8;

// Special opcodes for CondItem system (javac compatibility)
pub const DONTGOTO: u8 = 0xff;  // Pseudo-opcode: never jump (used for always-false conditions)
pub const JSR_W: u8 = 0xc9;

// 0xCA - 0xFE: Reserved and future use
// Note: These opcodes are reserved for future use or are implementation-specific
pub const BREAKPOINT: u8 = 0xca;
pub const IMPDEP1: u8 = 0xfe;
pub const IMPDEP2: u8 = 0xff;

/// Helper function to check if an opcode is valid
pub fn is_valid_opcode(opcode: u8) -> bool {
    match opcode {
        0x00..=0xca | 0xfe..=0xff => true,
        _ => false,
    }
}

/// Helper function to get opcode name as string
pub fn opcode_name(opcode: u8) -> &'static str {
    match opcode {
        0x00 => "NOP",
        0x01 => "ACONST_NULL",
        0x02 => "ICONST_M1",
        0x03 => "ICONST_0",
        0x04 => "ICONST_1",
        0x05 => "ICONST_2",
        0x06 => "ICONST_3",
        0x07 => "ICONST_4",
        0x08 => "ICONST_5",
        0x09 => "LCONST_0",
        0x0a => "LCONST_1",
        0x0b => "FCONST_0",
        0x0c => "FCONST_1",
        0x0d => "FCONST_2",
        0x0e => "DCONST_0",
        0x0f => "DCONST_1",
        0x10 => "BIPUSH",
        0x11 => "SIPUSH",
        0x12 => "LDC",
        0x13 => "LDC_W",
        0x14 => "LDC2_W",
        0x15 => "ILOAD",
        0x16 => "LLOAD",
        0x17 => "FLOAD",
        0x18 => "DLOAD",
        0x19 => "ALOAD",
        0x1a => "ILOAD_0",
        0x1b => "ILOAD_1",
        0x1c => "ILOAD_2",
        0x1d => "ILOAD_3",
        0x1e => "LLOAD_0",
        0x1f => "LLOAD_1",
        0x20 => "LLOAD_2",
        0x21 => "LLOAD_3",
        0x22 => "FLOAD_0",
        0x23 => "FLOAD_1",
        0x24 => "FLOAD_2",
        0x25 => "FLOAD_3",
        0x26 => "DLOAD_0",
        0x27 => "DLOAD_1",
        0x28 => "DLOAD_2",
        0x29 => "DLOAD_3",
        0x2a => "ALOAD_0",
        0x2b => "ALOAD_1",
        0x2c => "ALOAD_2",
        0x2d => "ALOAD_3",
        0x2e => "IALOAD",
        0x2f => "LALOAD",
        0x30 => "FALOAD",
        0x31 => "DALOAD",
        0x32 => "AALOAD",
        0x33 => "BALOAD",
        0x34 => "CALOAD",
        0x35 => "SALOAD",
        0x36 => "ISTORE",
        0x37 => "LSTORE",
        0x38 => "FSTORE",
        0x39 => "DSTORE",
        0x3a => "ASTORE",
        0x3b => "ISTORE_0",
        0x3c => "ISTORE_1",
        0x3d => "ISTORE_2",
        0x3e => "ISTORE_3",
        0x3f => "LSTORE_0",
        0x40 => "LSTORE_1",
        0x41 => "LSTORE_2",
        0x42 => "LSTORE_3",
        0x43 => "FSTORE_0",
        0x44 => "FSTORE_1",
        0x45 => "FSTORE_2",
        0x46 => "FSTORE_3",
        0x47 => "DSTORE_0",
        0x48 => "DSTORE_1",
        0x49 => "DSTORE_2",
        0x4a => "DSTORE_3",
        0x4b => "ASTORE_0",
        0x4c => "ASTORE_1",
        0x4d => "ASTORE_2",
        0x4e => "ASTORE_3",
        0x4f => "IASTORE",
        0x50 => "LASTORE",
        0x51 => "FASTORE",
        0x52 => "DASTORE",
        0x53 => "AASTORE",
        0x54 => "BASTORE",
        0x55 => "CASTORE",
        0x56 => "SASTORE",
        0x57 => "POP",
        0x58 => "POP2",
        0x59 => "DUP",
        0x5a => "DUP_X1",
        0x5b => "DUP_X2",
        0x5c => "DUP2",
        0x5d => "DUP2_X1",
        0x5e => "DUP2_X2",
        0x5f => "SWAP",
        0x60 => "IADD",
        0x61 => "LADD",
        0x62 => "FADD",
        0x63 => "DADD",
        0x64 => "ISUB",
        0x65 => "LSUB",
        0x66 => "FSUB",
        0x67 => "DSUB",
        0x68 => "IMUL",
        0x69 => "LMUL",
        0x6a => "FMUL",
        0x6b => "DMUL",
        0x6c => "IDIV",
        0x6d => "LDIV",
        0x6e => "FDIV",
        0x6f => "DDIV",
        0x70 => "IREM",
        0x71 => "LREM",
        0x72 => "FREM",
        0x73 => "DREM",
        0x74 => "INEG",
        0x75 => "LNEG",
        0x76 => "FNEG",
        0x77 => "DNEG",
        0x78 => "ISHL",
        0x79 => "LSHL",
        0x7a => "ISHR",
        0x7b => "LSHR",
        0x7c => "IUSHR",
        0x7d => "LUSHR",
        0x7e => "IAND",
        0x7f => "LAND",
        0x80 => "IOR",
        0x81 => "LOR",
        0x82 => "IXOR",
        0x83 => "LXOR",
        0x84 => "IINC",
        0x85 => "I2L",
        0x86 => "I2F",
        0x87 => "I2D",
        0x88 => "L2I",
        0x89 => "L2F",
        0x8a => "L2D",
        0x8b => "F2I",
        0x8c => "F2L",
        0x8d => "F2D",
        0x8e => "D2I",
        0x8f => "D2L",
        0x90 => "D2F",
        0x91 => "I2B",
        0x92 => "I2C",
        0x93 => "I2S",
        0x94 => "LCMP",
        0x95 => "FCMPL",
        0x96 => "FCMPG",
        0x97 => "DCMPL",
        0x98 => "DCMPG",
        0x99 => "IFEQ",
        0x9a => "IFNE",
        0x9b => "IFLT",
        0x9c => "IFGE",
        0x9d => "IFGT",
        0x9e => "IFLE",
        0x9f => "IF_ICMPEQ",
        0xa0 => "IF_ICMPNE",
        0xa1 => "IF_ICMPLT",
        0xa2 => "IF_ICMPGE",
        0xa3 => "IF_ICMPGT",
        0xa4 => "IF_ICMPLE",
        0xa5 => "IF_ACMPEQ",
        0xa6 => "IF_ACMPNE",
        0xa7 => "GOTO",
        0xa8 => "JSR",
        0xa9 => "RET",
        0xaa => "TABLESWITCH",
        0xab => "LOOKUPSWITCH",
        0xac => "IRETURN",
        0xad => "LRETURN",
        0xae => "FRETURN",
        0xaf => "DRETURN",
        0xb0 => "ARETURN",
        0xb1 => "RETURN",
        0xb2 => "GETSTATIC",
        0xb3 => "PUTSTATIC",
        0xb4 => "GETFIELD",
        0xb5 => "PUTFIELD",
        0xb6 => "INVOKEVIRTUAL",
        0xb7 => "INVOKESPECIAL",
        0xb8 => "INVOKESTATIC",
        0xb9 => "INVOKEINTERFACE",
        0xba => "INVOKEDYNAMIC",
        0xbb => "NEW",
        0xbc => "NEWARRAY",
        0xbd => "ANEWARRAY",
        0xbe => "ARRAYLENGTH",
        0xbf => "ATHROW",
        0xc0 => "CHECKCAST",
        0xc1 => "INSTANCEOF",
        0xc2 => "MONITORENTER",
        0xc3 => "MONITOREXIT",
        0xc4 => "WIDE",
        0xc5 => "MULTIANEWARRAY",
        0xc6 => "IFNULL",
        0xc7 => "IFNONNULL",
        0xc8 => "GOTO_W",
        0xc9 => "JSR_W",
        0xca => "BREAKPOINT",
        0xfe => "IMPDEP1",
        0xff => "IMPDEP2",
        _ => "UNKNOWN",
    }
}

/// Helper function to check if an opcode requires operands
pub fn requires_operands(opcode: u8) -> bool {
    match opcode {
        BIPUSH | SIPUSH | LDC | LDC_W | LDC2_W | ILOAD | LLOAD | FLOAD | DLOAD | ALOAD |
        ISTORE | LSTORE | FSTORE | DSTORE | ASTORE | IINC | IFEQ | IFNE | IFLT | IFGE |
        IFGT | IFLE | IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE |
        IF_ACMPEQ | IF_ACMPNE | GOTO | JSR | RET | TABLESWITCH | LOOKUPSWITCH |
        GETSTATIC | PUTSTATIC | GETFIELD | PUTFIELD | INVOKEVIRTUAL | INVOKESPECIAL |
        INVOKESTATIC | INVOKEINTERFACE | INVOKEDYNAMIC | NEW | NEWARRAY | ANEWARRAY |
        CHECKCAST | INSTANCEOF | WIDE | MULTIANEWARRAY | IFNULL | IFNONNULL | GOTO_W | JSR_W => true,
        _ => false,
    }
}

/// Helper function to get the number of operands for an opcode
pub fn operand_count(opcode: u8) -> usize {
    match opcode {
        BIPUSH => 1,
        SIPUSH => 2,
        LDC => 1,
        LDC_W | LDC2_W => 2,
        ILOAD | LLOAD | FLOAD | DLOAD | ALOAD | ISTORE | LSTORE | FSTORE | DSTORE | ASTORE => 1,
        IINC => 2,
        IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE | IF_ACMPEQ | IF_ACMPNE | GOTO | JSR => 2,
        RET => 1,
        TABLESWITCH | LOOKUPSWITCH => 0, // Variable length
        GETSTATIC | PUTSTATIC | GETFIELD | PUTFIELD | INVOKEVIRTUAL | INVOKESPECIAL | INVOKESTATIC | INVOKEINTERFACE => 2,
        INVOKEDYNAMIC => 4,
        NEW | NEWARRAY | ANEWARRAY | CHECKCAST | INSTANCEOF => 2,
        WIDE => 0, // Variable length
        MULTIANEWARRAY => 3,
        IFNULL | IFNONNULL => 2,
        GOTO_W | JSR_W => 4,
        _ => 0,
    }
}

/// Type codes for JVM types (javac compatibility)
pub mod typecodes {
    pub const BYTE: u8 = 0;
    pub const INT: u8 = 1;
    pub const LONG: u8 = 2;  
    pub const FLOAT: u8 = 3;
    pub const DOUBLE: u8 = 4;
    pub const OBJECT: u8 = 5;
    pub const ARRAY: u8 = 6;
    pub const VOID: u8 = 7;
}

