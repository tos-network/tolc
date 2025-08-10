//! Trait-based serialization for classfile structures

use std::io::Write;
use super::class::ClassFile;
use super::constpool::ConstantPool;
use super::field::FieldInfo;
use super::method::MethodInfo;
use super::attribute::AttributeInfo;

/// An object which can be written into a classfile.
/// This trait provides a unified interface for serializing all classfile components.
pub trait ClassfileWritable {
    /// Writes the bytes of this object into the given buffer.
    ///
    /// # Arguments
    ///
    /// * `buffer` - classfile byte-buffer into which this object should be written
    fn write_to_classfile<W: Write>(&self, buffer: &mut W);

    /// Writes the bytes of this object into a newly created buffer.
    fn to_classfile_bytes(&self) -> Vec<u8> {
        let mut buffer = Vec::new();
        self.write_to_classfile(&mut buffer);
        buffer
    }
}

impl ClassfileWritable for ClassFile {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) {
        // Write magic number
        buffer.write_all(&self.magic.to_be_bytes()).unwrap();
        
        // Write version
        buffer.write_all(&self.minor_version.to_be_bytes()).unwrap();
        buffer.write_all(&self.major_version.to_be_bytes()).unwrap();
        
        // Write constant pool
        self.constant_pool.write_to_classfile(buffer);
        
        // Write access flags
        buffer.write_all(&self.access_flags.to_be_bytes()).unwrap();
        
        // Write class indices
        buffer.write_all(&self.this_class.to_be_bytes()).unwrap();
        buffer.write_all(&self.super_class.to_be_bytes()).unwrap();
        
        // Write interfaces count and data
        buffer.write_all(&(self.interfaces.len() as u16).to_be_bytes()).unwrap();
        for interface in &self.interfaces {
            buffer.write_all(&interface.to_be_bytes()).unwrap();
        }
        
        // Write fields count and data
        buffer.write_all(&(self.fields.len() as u16).to_be_bytes()).unwrap();
        for field in &self.fields {
            field.write_to_classfile(buffer);
        }
        
        // Write methods count and data
        buffer.write_all(&(self.methods.len() as u16).to_be_bytes()).unwrap();
        for method in &self.methods {
            method.write_to_classfile(buffer);
        }
        
        // Write attributes count and data
        buffer.write_all(&(self.attributes.len() as u16).to_be_bytes()).unwrap();
        for attribute in &self.attributes {
            attribute.write_to_classfile(buffer);
        }
    }
}

impl ClassfileWritable for ConstantPool {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) {
        // Write constant pool count (size + 1)
        let count = (self.constants.len() + 1) as u16;
        buffer.write_all(&count.to_be_bytes()).unwrap();
        
        // Write each constant
        for constant in &self.constants {
            constant.write_to_classfile(buffer);
        }
    }
}

impl ClassfileWritable for super::constpool::Constant {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) {
        use super::constpool::Constant::*;
        
        match self {
            Utf8(value) => {
                buffer.write_all(&[1u8]).unwrap(); // tag
                let utf8_bytes = value.as_bytes();
                buffer.write_all(&(utf8_bytes.len() as u16).to_be_bytes()).unwrap();
                buffer.write_all(utf8_bytes).unwrap();
            }
            Integer(value) => {
                buffer.write_all(&[3u8]).unwrap(); // tag
                buffer.write_all(&value.to_be_bytes()).unwrap();
            }
            Float(value) => {
                buffer.write_all(&[4u8]).unwrap(); // tag
                buffer.write_all(&value.to_be_bytes()).unwrap();
            }
            Long(value) => {
                buffer.write_all(&[5u8]).unwrap(); // tag
                buffer.write_all(&value.to_be_bytes()).unwrap();
            }
            Double(value) => {
                buffer.write_all(&[6u8]).unwrap(); // tag
                buffer.write_all(&value.to_be_bytes()).unwrap();
            }
            Class(name_index) => {
                buffer.write_all(&[7u8]).unwrap(); // tag
                buffer.write_all(&name_index.to_be_bytes()).unwrap();
            }
            String(string_index) => {
                buffer.write_all(&[8u8]).unwrap(); // tag
                buffer.write_all(&string_index.to_be_bytes()).unwrap();
            }
            FieldRef(class_index, name_and_type_index) => {
                buffer.write_all(&[9u8]).unwrap(); // tag
                buffer.write_all(&class_index.to_be_bytes()).unwrap();
                buffer.write_all(&name_and_type_index.to_be_bytes()).unwrap();
            }
            MethodRef(class_index, name_and_type_index) => {
                buffer.write_all(&[10u8]).unwrap(); // tag
                buffer.write_all(&class_index.to_be_bytes()).unwrap();
                buffer.write_all(&name_and_type_index.to_be_bytes()).unwrap();
            }
            InterfaceMethodRef(class_index, name_and_type_index) => {
                buffer.write_all(&[11u8]).unwrap(); // tag
                buffer.write_all(&class_index.to_be_bytes()).unwrap();
                buffer.write_all(&name_and_type_index.to_be_bytes()).unwrap();
            }
            NameAndType(name_index, descriptor_index) => {
                buffer.write_all(&[12u8]).unwrap(); // tag
                buffer.write_all(&name_index.to_be_bytes()).unwrap();
                buffer.write_all(&descriptor_index.to_be_bytes()).unwrap();
            }
            MethodHandle(reference_kind, reference_index) => {
                buffer.write_all(&[15u8]).unwrap(); // tag
                buffer.write_all(&[*reference_kind]).unwrap();
                buffer.write_all(&reference_index.to_be_bytes()).unwrap();
            }
            MethodType(descriptor_index) => {
                buffer.write_all(&[16u8]).unwrap(); // tag
                buffer.write_all(&descriptor_index.to_be_bytes()).unwrap();
            }
            Dynamic(bootstrap_method_attr_index, name_and_type_index) => {
                buffer.write_all(&[17u8]).unwrap(); // tag
                buffer.write_all(&bootstrap_method_attr_index.to_be_bytes()).unwrap();
                buffer.write_all(&name_and_type_index.to_be_bytes()).unwrap();
            }
            InvokeDynamic(bootstrap_method_attr_index, name_and_type_index) => {
                buffer.write_all(&[18u8]).unwrap(); // tag
                buffer.write_all(&bootstrap_method_attr_index.to_be_bytes()).unwrap();
                buffer.write_all(&name_and_type_index.to_be_bytes()).unwrap();
            }
            Module(name_index) => {
                buffer.write_all(&[19u8]).unwrap(); // tag
                buffer.write_all(&name_index.to_be_bytes()).unwrap();
            }
            Package(name_index) => {
                buffer.write_all(&[20u8]).unwrap(); // tag
                buffer.write_all(&name_index.to_be_bytes()).unwrap();
            }
        }
    }
}

impl ClassfileWritable for FieldInfo {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) {
        buffer.write_all(&self.access_flags.to_be_bytes()).unwrap();
        buffer.write_all(&self.name_index.to_be_bytes()).unwrap();
        buffer.write_all(&self.descriptor_index.to_be_bytes()).unwrap();
        
        // Write attributes count and data
        buffer.write_all(&(self.attributes.len() as u16).to_be_bytes()).unwrap();
        for attribute in &self.attributes {
            attribute.write_to_classfile(buffer);
        }
    }
}

impl ClassfileWritable for MethodInfo {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) {
        buffer.write_all(&self.access_flags.to_be_bytes()).unwrap();
        buffer.write_all(&self.name_index.to_be_bytes()).unwrap();
        buffer.write_all(&self.descriptor_index.to_be_bytes()).unwrap();
        
        // Write attributes count and data
        buffer.write_all(&(self.attributes.len() as u16).to_be_bytes()).unwrap();
        for attribute in &self.attributes {
            attribute.write_to_classfile(buffer);
        }
    }
}

impl ClassfileWritable for AttributeInfo {
    fn write_to_classfile<W: Write>(&self, buffer: &mut W) {
        buffer.write_all(&self.name_index.to_be_bytes()).unwrap();
        buffer.write_all(&(self.info.len() as u32).to_be_bytes()).unwrap();
        buffer.write_all(&self.info).unwrap();
    }
}

// Legacy function for backward compatibility
pub fn class_file_to_bytes(class_file: &ClassFile) -> Vec<u8> {
    class_file.to_classfile_bytes()
}


