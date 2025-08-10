//! Classfile writer: serialize `ClassFile` into bytes

use super::class::ClassFile;

pub fn class_file_to_bytes(cf: &ClassFile) -> Vec<u8> {
    let mut bytes = Vec::new();

    // Magic and version
    bytes.extend_from_slice(&cf.magic.to_be_bytes());
    bytes.extend_from_slice(&cf.minor_version.to_be_bytes());
    bytes.extend_from_slice(&cf.major_version.to_be_bytes());

    // Constant pool
    bytes.extend_from_slice(&cf.constant_pool.to_bytes());

    // Access flags, this, super
    bytes.extend_from_slice(&cf.access_flags.to_be_bytes());
    bytes.extend_from_slice(&cf.this_class.to_be_bytes());
    bytes.extend_from_slice(&cf.super_class.to_be_bytes());

    // Interfaces
    bytes.extend_from_slice(&(cf.interfaces.len() as u16).to_be_bytes());
    for &iface in &cf.interfaces {
        bytes.extend_from_slice(&iface.to_be_bytes());
    }

    // Fields
    bytes.extend_from_slice(&(cf.fields.len() as u16).to_be_bytes());
    for f in &cf.fields {
        bytes.extend_from_slice(&f.to_bytes(&cf.constant_pool));
    }

    // Methods
    bytes.extend_from_slice(&(cf.methods.len() as u16).to_be_bytes());
    for m in &cf.methods {
        bytes.extend_from_slice(&m.to_bytes(&cf.constant_pool));
    }

    // Attributes
    bytes.extend_from_slice(&(cf.attributes.len() as u16).to_be_bytes());
    for a in &cf.attributes {
        bytes.extend_from_slice(&a.to_bytes(&cf.constant_pool));
    }

    bytes
}


