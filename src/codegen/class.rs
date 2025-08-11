//! Core classfile structures: ClassFile, FieldInfo, MethodInfo

use super::constpool::ConstantPool;
use super::attribute::NamedAttribute;
use super::field::FieldInfo;
use super::method::MethodInfo;
use super::defs::{MAGIC, JAVA_1_8};

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
    pub attributes: Vec<NamedAttribute>,
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

    // Serialization moved to writer::class_file_to_bytes
}

// FieldInfo moved to field.rs; MethodInfo moved to method.rs


