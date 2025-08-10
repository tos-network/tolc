//! Java bytecode structures and constants
//! 
//! This module defines the basic structures needed for generating Java .class files.

use std::collections::HashMap;

/// Java class file magic number (0xCAFEBABE)
pub const MAGIC: u32 = 0xCAFEBABE;

/// Java class file version constants
pub const JAVA_1_8: u16 = 52;
pub const JAVA_11: u16 = 55;
pub const JAVA_17: u16 = 61;
pub const JAVA_21: u16 = 65;

/// Access flags for classes, fields, and methods
pub mod access_flags {
    pub const ACC_PUBLIC: u16 = 0x0001;
    pub const ACC_PRIVATE: u16 = 0x0002;
    pub const ACC_PROTECTED: u16 = 0x0004;
    pub const ACC_STATIC: u16 = 0x0008;
    pub const ACC_FINAL: u16 = 0x0010;
    pub const ACC_SUPER: u16 = 0x0020;
    pub const ACC_SYNCHRONIZED: u16 = 0x0020;
    pub const ACC_VOLATILE: u16 = 0x0040;
    pub const ACC_BRIDGE: u16 = 0x0040;
    pub const ACC_TRANSIENT: u16 = 0x0080;
    pub const ACC_VARARGS: u16 = 0x0080;
    pub const ACC_NATIVE: u16 = 0x0100;
    pub const ACC_INTERFACE: u16 = 0x0200;
    pub const ACC_ABSTRACT: u16 = 0x0400;
    pub const ACC_STRICT: u16 = 0x0800;
    pub const ACC_SYNTHETIC: u16 = 0x1000;
    pub const ACC_ANNOTATION: u16 = 0x2000;
    pub const ACC_ENUM: u16 = 0x4000;
    pub const ACC_MODULE: u16 = 0x8000;
}

/// Constant pool tags
pub mod constant_tags {
    pub const CONSTANT_UTF8: u8 = 1;
    pub const CONSTANT_INTEGER: u8 = 3;
    pub const CONSTANT_FLOAT: u8 = 4;
    pub const CONSTANT_LONG: u8 = 5;
    pub const CONSTANT_DOUBLE: u8 = 6;
    pub const CONSTANT_CLASS: u8 = 7;
    pub const CONSTANT_STRING: u8 = 8;
    pub const CONSTANT_FIELDREF: u8 = 9;
    pub const CONSTANT_METHODREF: u8 = 10;
    pub const CONSTANT_INTERFACEMETHODREF: u8 = 11;
    pub const CONSTANT_NAMEANDTYPE: u8 = 12;
    pub const CONSTANT_METHODHANDLE: u8 = 15;
    pub const CONSTANT_METHODTYPE: u8 = 16;
    pub const CONSTANT_DYNAMIC: u8 = 17;
    pub const CONSTANT_INVOKEDYNAMIC: u8 = 18;
    pub const CONSTANT_MODULE: u8 = 19;
    pub const CONSTANT_PACKAGE: u8 = 20;
}

/// Java bytecode instruction opcodes
pub mod opcodes {
    // Constants
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
    
    // Loads
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
    
    // Stores
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
    
    // Stack operations
    pub const POP: u8 = 0x57;
    pub const POP2: u8 = 0x58;
    pub const DUP: u8 = 0x59;
    pub const DUP_X1: u8 = 0x5a;
    pub const DUP_X2: u8 = 0x5b;
    pub const DUP2: u8 = 0x5c;
    pub const DUP2_X1: u8 = 0x5d;
    pub const DUP2_X2: u8 = 0x5e;
    pub const SWAP: u8 = 0x5f;
    
    // Arithmetic
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
    
    // Shifts
    pub const ISHL: u8 = 0x78;
    pub const LSHL: u8 = 0x79;
    pub const ISHR: u8 = 0x7a;
    pub const LSHR: u8 = 0x7b;
    pub const IUSHR: u8 = 0x7c;
    pub const LUSHR: u8 = 0x7d;
    
    // Logical
    pub const IAND: u8 = 0x7e;
    pub const LAND: u8 = 0x7f;
    pub const IOR: u8 = 0x80;
    pub const LOR: u8 = 0x81;
    pub const IXOR: u8 = 0x82;
    pub const LXOR: u8 = 0x83;
    
    // Conversions
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
    
    // Comparisons
    pub const LCMP: u8 = 0x94;
    pub const FCMPL: u8 = 0x95;
    pub const FCMPG: u8 = 0x96;
    pub const DCMPL: u8 = 0x97;
    pub const DCMPG: u8 = 0x98;
    
    // Control flow
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
    
    // References
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
    
    // Extended
    pub const WIDE: u8 = 0xc4;
    pub const MULTIANEWARRAY: u8 = 0xc5;
    pub const IFNULL: u8 = 0xc6;
    pub const IFNONNULL: u8 = 0xc7;
    pub const GOTO_W: u8 = 0xc8;
    pub const JSR_W: u8 = 0xc9;
    
    // Wide instructions
    pub const WIDE_ILOAD: u8 = 0x15;
    pub const WIDE_LLOAD: u8 = 0x16;
    pub const WIDE_FLOAD: u8 = 0x17;
    pub const WIDE_DLOAD: u8 = 0x18;
    pub const WIDE_ALOAD: u8 = 0x19;
    pub const WIDE_ISTORE: u8 = 0x36;
    pub const WIDE_LSTORE: u8 = 0x37;
    pub const WIDE_FSTORE: u8 = 0x38;
    pub const WIDE_DSTORE: u8 = 0x39;
    pub const WIDE_ASTORE: u8 = 0x3a;
    pub const WIDE_RET: u8 = 0xa9;
    pub const WIDE_IINC: u8 = 0x84;
    
    // Missing opcodes
    pub const BIPUSH: u8 = 0x10;
    pub const SIPUSH: u8 = 0x11;
    pub const LDC: u8 = 0x12;
    pub const LDC_W: u8 = 0x13;
    pub const LDC2_W: u8 = 0x14;
    
    // Increment
    pub const IINC: u8 = 0x84;
    
    // Array operations
    pub const IALOAD: u8 = 0x2e;
    pub const LALOAD: u8 = 0x2f;
    pub const FALOAD: u8 = 0x30;
    pub const DALOAD: u8 = 0x31;
    pub const AALOAD: u8 = 0x32;
    pub const BALOAD: u8 = 0x33;
    pub const CALOAD: u8 = 0x34;
    pub const SALOAD: u8 = 0x35;
    
    pub const IASTORE: u8 = 0x4f;
    pub const LASTORE: u8 = 0x50;
    pub const FASTORE: u8 = 0x51;
    pub const DASTORE: u8 = 0x52;
    pub const AASTORE: u8 = 0x53;
    pub const BASTORE: u8 = 0x54;
    pub const CASTORE: u8 = 0x55;
    pub const SASTORE: u8 = 0x56;
}

/// Java class file structure
#[derive(Debug)]
pub struct ClassFile {
    pub magic: u32,
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<u16>,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<MethodInfo>,
    pub attributes: Vec<AttributeInfo>,
}

impl ClassFile {
    pub fn new() -> Self {
        Self {
            magic: MAGIC,
            minor_version: 0,
            major_version: JAVA_1_8,
            constant_pool: ConstantPool::new(),
            access_flags: 0,
            this_class: 0,
            super_class: 0,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            attributes: Vec::new(),
        }
    }
    
    /// Write the class file to a byte vector
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        // Magic number
        bytes.extend_from_slice(&self.magic.to_be_bytes());
        
        // Version
        bytes.extend_from_slice(&self.minor_version.to_be_bytes());
        bytes.extend_from_slice(&self.major_version.to_be_bytes());
        
        // Constant pool
        bytes.extend_from_slice(&self.constant_pool.to_bytes());
        
        // Access flags
        bytes.extend_from_slice(&self.access_flags.to_be_bytes());
        
        // This class
        bytes.extend_from_slice(&self.this_class.to_be_bytes());
        
        // Super class
        bytes.extend_from_slice(&self.super_class.to_be_bytes());
        
        // Interfaces
        bytes.extend_from_slice(&(self.interfaces.len() as u16).to_be_bytes());
        for interface in &self.interfaces {
            bytes.extend_from_slice(&interface.to_be_bytes());
        }
        
        // Fields
        bytes.extend_from_slice(&(self.fields.len() as u16).to_be_bytes());
        for field in &self.fields {
            bytes.extend_from_slice(&field.to_bytes(&self.constant_pool));
        }
        
        // Methods
        bytes.extend_from_slice(&(self.methods.len() as u16).to_be_bytes());
        for method in &self.methods {
            bytes.extend_from_slice(&method.to_bytes(&self.constant_pool));
        }
        
        // Attributes
        bytes.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        for attribute in &self.attributes {
            bytes.extend_from_slice(&attribute.to_bytes(&self.constant_pool));
        }
        
        bytes
    }
}

/// Constant pool for storing class file constants
#[derive(Debug)]
pub struct ConstantPool {
    constants: Vec<Constant>,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
        }
    }
    
    /// Add a UTF8 constant and return its index
    pub fn add_utf8(&mut self, value: &str) -> u16 {
        let constant = Constant::Utf8(value.to_string());
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a class constant and return its index
    pub fn add_class(&mut self, name: &str) -> u16 {
        let name_index = self.add_utf8(name);
        let constant = Constant::Class(name_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a name and type constant and return its index
    pub fn add_name_and_type(&mut self, name: &str, descriptor: &str) -> u16 {
        let name_index = self.add_utf8(name);
        let descriptor_index = self.add_utf8(descriptor);
        let constant = Constant::NameAndType(name_index, descriptor_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a field reference and return its index
    pub fn add_field_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, descriptor);
        let constant = Constant::FieldRef(class_index, name_and_type_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a method reference and return its index
    pub fn add_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, descriptor);
        let constant = Constant::MethodRef(class_index, name_and_type_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add an interface method reference and return its index
    pub fn add_interface_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, descriptor);
        let constant = Constant::InterfaceMethodRef(class_index, name_and_type_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a string constant and return its index
    pub fn add_string(&mut self, value: &str) -> u16 {
        let utf8_index = self.add_utf8(value);
        let constant = Constant::String(utf8_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add an integer constant and return its index
    pub fn add_integer(&mut self, value: i32) -> u16 {
        let constant = Constant::Integer(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a float constant and return its index
    pub fn add_float(&mut self, value: f32) -> u16 {
        let constant = Constant::Float(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a long constant and return its index
    pub fn add_long(&mut self, value: i64) -> u16 {
        let constant = Constant::Long(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a double constant and return its index
    pub fn add_double(&mut self, value: f64) -> u16 {
        let constant = Constant::Double(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a method handle constant and return its index
    pub fn add_method_handle(&mut self, reference_kind: u8, reference_index: u16) -> u16 {
        let constant = Constant::MethodHandle(reference_kind, reference_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Add a method type constant and return its index
    pub fn add_method_type(&mut self, descriptor: &str) -> u16 {
        let descriptor_index = self.add_utf8(descriptor);
        let constant = Constant::MethodType(descriptor_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    
    /// Convert to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        // Constant pool count (includes 1-based indexing)
        bytes.extend_from_slice(&((self.constants.len() + 1) as u16).to_be_bytes());
        
        // Write each constant
        for constant in &self.constants {
            bytes.extend_from_slice(&constant.to_bytes());
        }
        
        bytes
    }
}

/// Constant pool entry
#[derive(Debug)]
pub enum Constant {
    Utf8(String),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    FieldRef(u16, u16),
    MethodRef(u16, u16),
    InterfaceMethodRef(u16, u16),
    NameAndType(u16, u16),
    MethodHandle(u8, u16), // reference_kind, reference_index
    MethodType(u16),        // descriptor_index
    Dynamic(u16, u16),      // bootstrap_method_attr_index, name_and_type_index
    InvokeDynamic(u16, u16), // bootstrap_method_attr_index, name_and_type_index
    Module(u16),            // name_index
    Package(u16),           // name_index
}

impl Constant {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        match self {
            Constant::Utf8(value) => {
                bytes.push(constant_tags::CONSTANT_UTF8);
                let utf8_bytes = value.as_bytes();
                bytes.extend_from_slice(&(utf8_bytes.len() as u16).to_be_bytes());
                bytes.extend_from_slice(utf8_bytes);
            }
            Constant::Integer(value) => {
                bytes.push(constant_tags::CONSTANT_INTEGER);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Float(value) => {
                bytes.push(constant_tags::CONSTANT_FLOAT);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Long(value) => {
                bytes.push(constant_tags::CONSTANT_LONG);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Double(value) => {
                bytes.push(constant_tags::CONSTANT_DOUBLE);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Class(name_index) => {
                bytes.push(constant_tags::CONSTANT_CLASS);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::String(string_index) => {
                bytes.push(constant_tags::CONSTANT_STRING);
                bytes.extend_from_slice(&string_index.to_be_bytes());
            }
            Constant::FieldRef(class_index, name_and_type_index) => {
                bytes.push(constant_tags::CONSTANT_FIELDREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::MethodRef(class_index, name_and_type_index) => {
                bytes.push(constant_tags::CONSTANT_METHODREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::InterfaceMethodRef(class_index, name_and_type_index) => {
                bytes.push(constant_tags::CONSTANT_INTERFACEMETHODREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::NameAndType(name_index, descriptor_index) => {
                bytes.push(constant_tags::CONSTANT_NAMEANDTYPE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
                bytes.extend_from_slice(&descriptor_index.to_be_bytes());
            }
            Constant::MethodHandle(reference_kind, reference_index) => {
                bytes.push(constant_tags::CONSTANT_METHODHANDLE);
                bytes.push(*reference_kind);
                bytes.extend_from_slice(&reference_index.to_be_bytes());
            }
            Constant::MethodType(descriptor_index) => {
                bytes.push(constant_tags::CONSTANT_METHODTYPE);
                bytes.extend_from_slice(&descriptor_index.to_be_bytes());
            }
            Constant::Dynamic(bootstrap_method_attr_index, name_and_type_index) => {
                bytes.push(constant_tags::CONSTANT_DYNAMIC);
                bytes.extend_from_slice(&bootstrap_method_attr_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::InvokeDynamic(bootstrap_method_attr_index, name_and_type_index) => {
                bytes.push(constant_tags::CONSTANT_INVOKEDYNAMIC);
                bytes.extend_from_slice(&bootstrap_method_attr_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::Module(name_index) => {
                bytes.push(constant_tags::CONSTANT_MODULE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::Package(name_index) => {
                bytes.push(constant_tags::CONSTANT_PACKAGE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
        }
        
        bytes
    }
}

/// Field information
#[derive(Debug)]
pub struct FieldInfo {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<AttributeInfo>,
}

impl FieldInfo {
    pub fn new(access_flags: u16, name_index: u16, descriptor_index: u16) -> Self {
        Self {
            access_flags,
            name_index,
            descriptor_index,
            attributes: Vec::new(),
        }
    }
    
    pub fn to_bytes(&self, constant_pool: &ConstantPool) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        bytes.extend_from_slice(&self.access_flags.to_be_bytes());
        bytes.extend_from_slice(&self.name_index.to_be_bytes());
        bytes.extend_from_slice(&self.descriptor_index.to_be_bytes());
        
        // Attributes
        bytes.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        for attribute in &self.attributes {
            bytes.extend_from_slice(&attribute.to_bytes(constant_pool));
        }
        
        bytes
    }
}

/// Method information
#[derive(Debug)]
pub struct MethodInfo {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<AttributeInfo>,
}

impl MethodInfo {
    pub fn new(access_flags: u16, name_index: u16, descriptor_index: u16) -> Self {
        Self {
            access_flags,
            name_index,
            descriptor_index,
            attributes: Vec::new(),
        }
    }
    
    pub fn to_bytes(&self, constant_pool: &ConstantPool) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        bytes.extend_from_slice(&self.access_flags.to_be_bytes());
        bytes.extend_from_slice(&self.name_index.to_be_bytes());
        bytes.extend_from_slice(&self.descriptor_index.to_be_bytes());
        
        // Attributes
        bytes.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        for attribute in &self.attributes {
            bytes.extend_from_slice(&attribute.to_bytes(constant_pool));
        }
        
        bytes
    }
}

/// Attribute information
#[derive(Debug)]
pub struct AttributeInfo {
    pub name_index: u16,
    pub info: Vec<u8>,
}

/// Code attribute for method bodies
#[derive(Debug)]
pub struct CodeAttribute {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<u8>,
    pub exception_table: Vec<ExceptionTableEntry>,
    pub attributes: Vec<AttributeInfo>,
}

impl CodeAttribute {
    pub fn new(max_stack: u16, max_locals: u16, code: Vec<u8>) -> Self {
        Self {
            max_stack,
            max_locals,
            code,
            exception_table: Vec::new(),
            attributes: Vec::new(),
        }
    }
    
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        bytes.extend_from_slice(&self.max_stack.to_be_bytes());
        bytes.extend_from_slice(&self.max_locals.to_be_bytes());
        
        // Code length and code
        bytes.extend_from_slice(&(self.code.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&self.code);
        
        // Exception table
        bytes.extend_from_slice(&(self.exception_table.len() as u16).to_be_bytes());
        for entry in &self.exception_table {
            bytes.extend_from_slice(&entry.to_bytes());
        }
        
        // Attributes
        bytes.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        for attribute in &self.attributes {
            bytes.extend_from_slice(&attribute.to_bytes(&ConstantPool::new()));
        }
        
        bytes
    }
}

/// Exception table entry for try-catch blocks
#[derive(Debug)]
pub struct ExceptionTableEntry {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16,
}

impl ExceptionTableEntry {
    pub fn new(start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) -> Self {
        Self {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        }
    }
    
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.start_pc.to_be_bytes());
        bytes.extend_from_slice(&self.end_pc.to_be_bytes());
        bytes.extend_from_slice(&self.handler_pc.to_be_bytes());
        bytes.extend_from_slice(&self.catch_type.to_be_bytes());
        bytes
    }
}

/// Line number table attribute for debugging
#[derive(Debug)]
pub struct LineNumberTableAttribute {
    pub line_number_table: Vec<LineNumberEntry>,
}

impl LineNumberTableAttribute {
    pub fn new() -> Self {
        Self {
            line_number_table: Vec::new(),
        }
    }
    
    pub fn add_line_number(&mut self, start_pc: u16, line_number: u16) {
        self.line_number_table.push(LineNumberEntry {
            start_pc,
            line_number,
        });
    }
    
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        bytes.extend_from_slice(&(self.line_number_table.len() as u16).to_be_bytes());
        for entry in &self.line_number_table {
            bytes.extend_from_slice(&entry.to_bytes());
        }
        
        bytes
    }
}

/// Line number entry for debugging
#[derive(Debug)]
pub struct LineNumberEntry {
    pub start_pc: u16,
    pub line_number: u16,
}

impl LineNumberEntry {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.start_pc.to_be_bytes());
        bytes.extend_from_slice(&self.line_number.to_be_bytes());
        bytes
    }
}

impl AttributeInfo {
    pub fn new(name_index: u16, info: Vec<u8>) -> Self {
        Self {
            name_index,
            info,
        }
    }
    
    pub fn to_bytes(&self, _constant_pool: &ConstantPool) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        bytes.extend_from_slice(&self.name_index.to_be_bytes());
        bytes.extend_from_slice(&(self.info.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&self.info);
        
        bytes
    }
}
