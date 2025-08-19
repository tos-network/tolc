//! FieldInfo structure and serialization

use super::attribute::NamedAttribute;
use super::constpool::ConstantPool;

#[derive(Debug)]
pub struct FieldInfo {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<NamedAttribute>,
}

impl FieldInfo {
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
    
    pub fn to_bytes_with_mapping(&self, constant_pool: &ConstantPool, index_mapping: &std::collections::HashMap<u16, u16>) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.access_flags.to_be_bytes());
        
        // Apply index mapping to name and descriptor indices
        let mapped_name_index = *index_mapping.get(&self.name_index).unwrap_or(&self.name_index);
        let mapped_descriptor_index = *index_mapping.get(&self.descriptor_index).unwrap_or(&self.descriptor_index);
        
        bytes.extend_from_slice(&mapped_name_index.to_be_bytes());
        bytes.extend_from_slice(&mapped_descriptor_index.to_be_bytes());
        bytes.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        
        // Attributes also need mapping
        for attribute in &self.attributes { 
            bytes.extend_from_slice(&attribute.to_bytes_with_mapping(constant_pool, index_mapping)); 
        }
        bytes
    }
}


