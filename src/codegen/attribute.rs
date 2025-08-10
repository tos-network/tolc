//! Attributes and exception table structures for Java class files

use super::constpool::ConstantPool;

#[derive(Debug)]
pub struct AttributeInfo {
    pub name_index: u16,
    pub info: Vec<u8>,
}

impl AttributeInfo {
    pub fn new(name_index: u16, info: Vec<u8>) -> Self {
        Self { name_index, info }
    }

    pub fn to_bytes(&self, _constant_pool: &ConstantPool) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.name_index.to_be_bytes());
        bytes.extend_from_slice(&(self.info.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&self.info);
        bytes
    }
}

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
        bytes.extend_from_slice(&(self.code.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&self.code);
        bytes.extend_from_slice(&(self.exception_table.len() as u16).to_be_bytes());
        for entry in &self.exception_table {
            bytes.extend_from_slice(&entry.to_bytes());
        }
        bytes.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        for attribute in &self.attributes {
            bytes.extend_from_slice(&attribute.to_bytes(&ConstantPool::new()));
        }
        bytes
    }
}

#[derive(Debug)]
pub struct ExceptionTableEntry {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16,
}

impl ExceptionTableEntry {
    pub fn new(start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) -> Self {
        Self { start_pc, end_pc, handler_pc, catch_type }
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

#[derive(Debug)]
pub struct LineNumberTableAttribute {
    pub line_number_table: Vec<LineNumberEntry>,
}

impl LineNumberTableAttribute {
    pub fn new() -> Self { Self { line_number_table: Vec::new() } }

    pub fn add_line_number(&mut self, start_pc: u16, line_number: u16) {
        self.line_number_table.push(LineNumberEntry { start_pc, line_number });
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

/// Helper to build an AttributeInfo for LineNumberTable
pub fn make_line_number_table_attribute(constant_pool: &mut ConstantPool, table: &LineNumberTableAttribute) -> AttributeInfo {
    let name_index = constant_pool.add_utf8("LineNumberTable");
    let info = table.to_bytes();
    AttributeInfo::new(name_index, info)
}


#[derive(Debug)]
pub struct LocalVariableTableAttribute {
    pub entries: Vec<LocalVariableEntry>,
}

impl LocalVariableTableAttribute {
    pub fn new() -> Self { Self { entries: Vec::new() } }
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&(self.entries.len() as u16).to_be_bytes());
        for e in &self.entries {
            bytes.extend_from_slice(&e.to_bytes());
        }
        bytes
    }
}

#[derive(Debug)]
pub struct LocalVariableEntry {
    pub start_pc: u16,
    pub length: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub index: u16,
}

impl LocalVariableEntry {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.start_pc.to_be_bytes());
        bytes.extend_from_slice(&self.length.to_be_bytes());
        bytes.extend_from_slice(&self.name_index.to_be_bytes());
        bytes.extend_from_slice(&self.descriptor_index.to_be_bytes());
        bytes.extend_from_slice(&self.index.to_be_bytes());
        bytes
    }
}

/// Helper to build an AttributeInfo for LocalVariableTable
pub fn make_local_variable_table_attribute(constant_pool: &mut ConstantPool, table: &LocalVariableTableAttribute) -> AttributeInfo {
    let name_index = constant_pool.add_utf8("LocalVariableTable");
    let info = table.to_bytes();
    AttributeInfo::new(name_index, info)
}


