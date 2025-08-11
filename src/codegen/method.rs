//! MethodInfo structure and serialization

use super::attribute::NamedAttribute;
use super::constpool::ConstantPool;

#[derive(Debug)]
pub struct MethodInfo {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<NamedAttribute>,
}

impl MethodInfo {
    pub fn new(access_flags: u16, name_index: u16, descriptor_index: u16) -> Self {
        Self { access_flags, name_index, descriptor_index, attributes: Vec::new() }
    }

    pub fn to_bytes(&self, constant_pool: &ConstantPool) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.access_flags.to_be_bytes());
        bytes.extend_from_slice(&self.name_index.to_be_bytes());
        bytes.extend_from_slice(&self.descriptor_index.to_be_bytes());
        bytes.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        for attribute in &self.attributes { bytes.extend_from_slice(&attribute.to_bytes(constant_pool)); }
        bytes
    }
}


