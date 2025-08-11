//! Trait-based serialization for classfile structures

use std::io::Write;
use super::class::ClassFile;
use super::constpool::ConstantPool;

/// An object which can be written into a classfile.
/// This trait provides a unified interface for serializing all classfile components.
pub trait ClassfileWritable {
    /// Writes the bytes of this object into the given buffer.
    ///
    /// # Arguments
    ///
    /// * `buffer` - classfile byte-buffer into which this object should be written
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) -> std::io::Result<()>;

    /// Writes the bytes of this object into a newly created buffer.
    fn to_classfile_bytes(&self) -> Vec<u8> {
        let mut buffer = Vec::new();
        let _ = self.write_to_classfile(&mut buffer);
        buffer
    }
}

impl ClassfileWritable for ClassFile {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) -> std::io::Result<()> {
        // Write magic number
        buffer.write_all(&self.magic.to_be_bytes())?;
        
        // Write version
        buffer.write_all(&self.minor_version.to_be_bytes())?;
        buffer.write_all(&self.major_version.to_be_bytes())?;
        
        // Write constant pool
        self.constant_pool.write_to_classfile(buffer)?;
        
        // Write access flags
        buffer.write_all(&self.access_flags.to_be_bytes())?;
        
        // Write class indices
        buffer.write_all(&self.this_class.to_be_bytes())?;
        buffer.write_all(&self.super_class.to_be_bytes())?;
        
        // Write interfaces count and data
        buffer.write_all(&(self.interfaces.len() as u16).to_be_bytes())?;
        for interface in &self.interfaces {
            buffer.write_all(&interface.to_be_bytes())?;
        }
        
        // Write fields count and data
        buffer.write_all(&(self.fields.len() as u16).to_be_bytes())?;
        for field in &self.fields {
            let bytes = field.to_bytes(&self.constant_pool);
            buffer.write_all(&bytes)?;
        }
        
        // Write methods count and data
        buffer.write_all(&(self.methods.len() as u16).to_be_bytes())?;
        for method in &self.methods {
            let bytes = method.to_bytes(&self.constant_pool);
            buffer.write_all(&bytes)?;
        }
        
        // Write attributes count and data
        buffer.write_all(&(self.attributes.len() as u16).to_be_bytes())?;
        for attribute in &self.attributes {
            // name_index
            buffer.write_all(&attribute.name.as_u16().to_be_bytes())?;
            // payload
            let payload = attribute.info.to_bytes(&self.constant_pool);
            buffer.write_all(&(payload.len() as u32).to_be_bytes())?;
            buffer.write_all(&payload)?;
        }
        Ok(())
    }
}

impl ClassfileWritable for ConstantPool {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) -> std::io::Result<()> {
        // Write constant pool count (size + 1)
        let count = (self.constants.len() + 1) as u16;
        buffer.write_all(&count.to_be_bytes())?;
        
        // Write each constant
        for constant in &self.constants {
            constant.write_to_classfile(buffer)?;
        }
        Ok(())
    }
}

impl ClassfileWritable for super::constpool::Constant {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) -> std::io::Result<()> {
        use super::constpool::Constant::*;
        
        match self {
            Utf8(value) => {
                buffer.write_all(&[1u8])?; // tag
                let utf8_bytes = value.as_bytes();
                buffer.write_all(&(utf8_bytes.len() as u16).to_be_bytes())?;
                buffer.write_all(utf8_bytes)?;
            }
            Integer(value) => {
                buffer.write_all(&[3u8])?; // tag
                buffer.write_all(&value.to_be_bytes())?;
            }
            Float(value) => {
                buffer.write_all(&[4u8])?; // tag
                buffer.write_all(&value.to_be_bytes())?;
            }
            Long(value) => {
                buffer.write_all(&[5u8])?; // tag
                buffer.write_all(&value.to_be_bytes())?;
            }
            Double(value) => {
                buffer.write_all(&[6u8])?; // tag
                buffer.write_all(&value.to_be_bytes())?;
            }
            Class(name_index) => {
                buffer.write_all(&[7u8])?; // tag
                buffer.write_all(&name_index.to_be_bytes())?;
            }
            String(string_index) => {
                buffer.write_all(&[8u8])?; // tag
                buffer.write_all(&string_index.to_be_bytes())?;
            }
            FieldRef(class_index, name_and_type_index) => {
                buffer.write_all(&[9u8])?; // tag
                buffer.write_all(&class_index.to_be_bytes())?;
                buffer.write_all(&name_and_type_index.to_be_bytes())?;
            }
            MethodRef(class_index, name_and_type_index) => {
                buffer.write_all(&[10u8])?; // tag
                buffer.write_all(&class_index.to_be_bytes())?;
                buffer.write_all(&name_and_type_index.to_be_bytes())?;
            }
            InterfaceMethodRef(class_index, name_and_type_index) => {
                buffer.write_all(&[11u8])?; // tag
                buffer.write_all(&class_index.to_be_bytes())?;
                buffer.write_all(&name_and_type_index.to_be_bytes())?;
            }
            NameAndType(name_index, descriptor_index) => {
                buffer.write_all(&[12u8])?; // tag
                buffer.write_all(&name_index.to_be_bytes())?;
                buffer.write_all(&descriptor_index.to_be_bytes())?;
            }
            MethodHandle(reference_kind, reference_index) => {
                buffer.write_all(&[15u8])?; // tag
                buffer.write_all(&[*reference_kind])?;
                buffer.write_all(&reference_index.to_be_bytes())?;
            }
            MethodType(descriptor_index) => {
                buffer.write_all(&[16u8])?; // tag
                buffer.write_all(&descriptor_index.to_be_bytes())?;
            }
            Dynamic(bootstrap_method_attr_index, name_and_type_index) => {
                buffer.write_all(&[17u8])?; // tag
                buffer.write_all(&bootstrap_method_attr_index.to_be_bytes())?;
                buffer.write_all(&name_and_type_index.to_be_bytes())?;
            }
            InvokeDynamic(bootstrap_method_attr_index, name_and_type_index) => {
                buffer.write_all(&[18u8])?; // tag
                buffer.write_all(&bootstrap_method_attr_index.to_be_bytes())?;
                buffer.write_all(&name_and_type_index.to_be_bytes())?;
            }
            Module(name_index) => {
                buffer.write_all(&[19u8])?; // tag
                buffer.write_all(&name_index.to_be_bytes())?;
            }
            Package(name_index) => {
                buffer.write_all(&[20u8])?; // tag
                buffer.write_all(&name_index.to_be_bytes())?;
            }
        }
        Ok(())
    }
}

// FieldInfo and MethodInfo are serialized via ClassFile to access constant pool

// Writer stays payload-oriented; the higher layer prepares name_index + payload bytes.
// AttributeInfo itself no longer carries name_index/info; writing happens where pairs exist.

// Legacy function for backward compatibility
pub fn class_file_to_bytes(class_file: &ClassFile) -> Vec<u8> {
    class_file.to_classfile_bytes()
}


