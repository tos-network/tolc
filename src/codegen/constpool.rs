//! Constant pool and constants for Java class files

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
    MethodHandle(u8, u16),
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16),
}

mod constant_tags {
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

impl Constant {
    pub fn to_bytes(&self) -> Vec<u8> {
        use constant_tags::*;
        let mut bytes = Vec::new();
        match self {
            Constant::Utf8(value) => {
                bytes.push(CONSTANT_UTF8);
                let utf8_bytes = value.as_bytes();
                bytes.extend_from_slice(&(utf8_bytes.len() as u16).to_be_bytes());
                bytes.extend_from_slice(utf8_bytes);
            }
            Constant::Integer(value) => {
                bytes.push(CONSTANT_INTEGER);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Float(value) => {
                bytes.push(CONSTANT_FLOAT);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Long(value) => {
                bytes.push(CONSTANT_LONG);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Double(value) => {
                bytes.push(CONSTANT_DOUBLE);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Class(name_index) => {
                bytes.push(CONSTANT_CLASS);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::String(string_index) => {
                bytes.push(CONSTANT_STRING);
                bytes.extend_from_slice(&string_index.to_be_bytes());
            }
            Constant::FieldRef(class_index, name_and_type_index) => {
                bytes.push(CONSTANT_FIELDREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::MethodRef(class_index, name_and_type_index) => {
                bytes.push(CONSTANT_METHODREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::InterfaceMethodRef(class_index, name_and_type_index) => {
                bytes.push(CONSTANT_INTERFACEMETHODREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::NameAndType(name_index, descriptor_index) => {
                bytes.push(CONSTANT_NAMEANDTYPE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
                bytes.extend_from_slice(&descriptor_index.to_be_bytes());
            }
            Constant::MethodHandle(reference_kind, reference_index) => {
                bytes.push(CONSTANT_METHODHANDLE);
                bytes.push(*reference_kind);
                bytes.extend_from_slice(&reference_index.to_be_bytes());
            }
            Constant::MethodType(descriptor_index) => {
                bytes.push(CONSTANT_METHODTYPE);
                bytes.extend_from_slice(&descriptor_index.to_be_bytes());
            }
            Constant::Dynamic(bootstrap_method_attr_index, name_and_type_index) => {
                bytes.push(CONSTANT_DYNAMIC);
                bytes.extend_from_slice(&bootstrap_method_attr_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::InvokeDynamic(bootstrap_method_attr_index, name_and_type_index) => {
                bytes.push(CONSTANT_INVOKEDYNAMIC);
                bytes.extend_from_slice(&bootstrap_method_attr_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::Module(name_index) => {
                bytes.push(CONSTANT_MODULE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::Package(name_index) => {
                bytes.push(CONSTANT_PACKAGE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
        }
        bytes
    }
}

#[derive(Debug)]
pub struct ConstantPool {
    pub(crate) constants: Vec<Constant>,
}

impl ConstantPool {
    pub fn new() -> Self { Self { constants: Vec::new() } }

    pub fn add_utf8(&mut self, value: &str) -> u16 {
        let constant = Constant::Utf8(value.to_string());
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_class(&mut self, name: &str) -> u16 {
        let name_index = self.add_utf8(name);
        let constant = Constant::Class(name_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_name_and_type(&mut self, name: &str, descriptor: &str) -> u16 {
        let name_index = self.add_utf8(name);
        let descriptor_index = self.add_utf8(descriptor);
        let constant = Constant::NameAndType(name_index, descriptor_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_field_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, descriptor);
        let constant = Constant::FieldRef(class_index, name_and_type_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, descriptor);
        let constant = Constant::MethodRef(class_index, name_and_type_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_interface_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let class_index = self.add_class(class);
        let name_and_type_index = self.add_name_and_type(name, descriptor);
        let constant = Constant::InterfaceMethodRef(class_index, name_and_type_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_string(&mut self, value: &str) -> u16 {
        let utf8_index = self.add_utf8(value);
        let constant = Constant::String(utf8_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_integer(&mut self, value: i32) -> u16 {
        let constant = Constant::Integer(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_float(&mut self, value: f32) -> u16 {
        let constant = Constant::Float(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_long(&mut self, value: i64) -> u16 {
        let constant = Constant::Long(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_double(&mut self, value: f64) -> u16 {
        let constant = Constant::Double(value);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_method_handle(&mut self, reference_kind: u8, reference_index: u16) -> u16 {
        let constant = Constant::MethodHandle(reference_kind, reference_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }
    pub fn add_method_type(&mut self, descriptor: &str) -> u16 {
        let descriptor_index = self.add_utf8(descriptor);
        let constant = Constant::MethodType(descriptor_index);
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&((self.constants.len() + 1) as u16).to_be_bytes());
        for constant in &self.constants {
            bytes.extend_from_slice(&constant.to_bytes());
        }
        bytes
    }
}


