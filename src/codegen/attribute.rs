//! Attributes and exception table structures for Java class files

use super::constpool::ConstantPool;
use super::typed_index::{ConstPoolIndex, ConstClassInfo, ConstNameAndTypeInfo};
use super::frame::StackMapFrame;
use super::vec::{JvmVecU2, JvmVecError};
use thiserror::Error;

// Add missing type definitions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstUtf8Info;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstValueInfo;

// Implement ConstPoolEntryInfo for the new types
impl super::typed_index::ConstPoolEntryInfo for ConstUtf8Info {
    fn is_valid_entry(entry: &super::constpool::Constant) -> bool {
        matches!(entry, super::constpool::Constant::Utf8(_))
    }
}

impl super::typed_index::ConstPoolEntryInfo for ConstValueInfo {
    fn is_valid_entry(entry: &super::constpool::Constant) -> bool {
        matches!(entry, 
            super::constpool::Constant::Integer(_) |
            super::constpool::Constant::Float(_) |
            super::constpool::Constant::Long(_) |
            super::constpool::Constant::Double(_) |
            super::constpool::Constant::String(_)
        )
    }
}

/// An error which may occur while creating a new attribute.
#[derive(Error, Debug)]
pub enum AttributeCreateError {
    #[error("Failed to store constant pool entry")]
    ConstPoolError(String),
    #[error("Invalid attribute data")]
    InvalidData(String),
    #[error("JVM vector error: {0}")]
    JvmVec(#[from] JvmVecError),
    #[error("Constant pool error: {0}")]
    ConstPool(#[from] super::constpool::ConstPoolError),
}

/// An error which may occur while adding a new attribute.
#[derive(Error, Debug)]
pub enum AttributeAddError {
    #[error("Failed to add attribute")]
    AddError(String),
    #[error("Attribute could not be created")]
    CreateError(#[from] AttributeCreateError),
}

/// Named attribute with name and content.
#[derive(Debug, Clone)]
pub struct NamedAttribute {
    pub name: ConstPoolIndex<ConstUtf8Info>,
    pub info: AttributeInfo,
}

impl NamedAttribute {
    pub fn new(name: ConstPoolIndex<ConstUtf8Info>, info: AttributeInfo) -> Self {
        Self { name, info }
    }

    // Factory methods for creating standard attributes
    pub fn new_const_value_attribute(
        const_pool: &mut ConstantPool,
        value: ConstPoolIndex<ConstValueInfo>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("ConstantValue")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::ConstantValue(ConstantValueAttribute { value }) })
    }

    pub fn new_source_file_attribute(
        const_pool: &mut ConstantPool,
        filename: String,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("SourceFile")?;
        let filename_index = const_pool.try_add_utf8(&filename)?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::SourceFile(SourceFileAttribute { filename: ConstPoolIndex::from(filename_index) }) })
    }

    pub fn new_synthetic_attribute(
        const_pool: &mut ConstantPool,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("Synthetic")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::Synthetic(SyntheticAttribute) })
    }

    pub fn new_deprecated_attribute(
        const_pool: &mut ConstantPool,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("Deprecated")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::Deprecated(DeprecatedAttribute) })
    }

    pub fn new_signature_attribute(
        const_pool: &mut ConstantPool,
        signature: String,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("Signature")?;
        let signature_index = const_pool.try_add_utf8(&signature)?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::Signature(SignatureAttribute { signature: ConstPoolIndex::from(signature_index) }) })
    }

    pub fn new_code_attribute(
        const_pool: &mut ConstantPool,
        max_stack: u16,
        max_locals: u16,
        code: Vec<u8>,
        exception_tables: Vec<ExceptionTableEntry>,
        attributes: Vec<NamedAttribute>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("Code")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::Code(CodeAttribute { max_stack, max_locals, code, exception_table: exception_tables, attributes }) })
    }

    pub fn new_stack_map_table_attribute(
        const_pool: &mut ConstantPool,
        stack_map_frames: Vec<StackMapFrame>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("StackMapTable")?;
        let frames = super::vec::JvmVecU2::from_vec_checked(stack_map_frames)?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::StackMapTable(StackMapTableAttribute { stack_map_frames: frames }) })
    }

    pub fn new_exceptions_attribute(
        const_pool: &mut ConstantPool,
        exceptions: Vec<ConstPoolIndex<ConstClassInfo>>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("Exceptions")?;
        let exceptions_vec = super::vec::JvmVecU2::from_vec_checked(exceptions)?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::Exceptions(ExceptionsAttribute { exceptions: exceptions_vec }) })
    }

    pub fn new_line_number_table_attribute(
        const_pool: &mut ConstantPool,
        line_number_table: Vec<LineNumberEntry>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("LineNumberTable")?;
        let table = super::vec::JvmVecU2::from_vec_checked(line_number_table)?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::LineNumberTable(LineNumberTableAttribute { line_number_table: table }) })
    }

    pub fn new_local_variable_table_attribute(
        const_pool: &mut ConstantPool,
        local_variable_table: Vec<LocalVariableEntry>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("LocalVariableTable")?;
        let entries = super::vec::JvmVecU2::from_vec_checked(local_variable_table)?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::LocalVariableTable(LocalVariableTableAttribute { entries }) })
    }

    pub fn new_local_variable_type_table_attribute(
        const_pool: &mut ConstantPool,
        local_variable_type_table: Vec<LocalVariableTypeTableEntry>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("LocalVariableTypeTable")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::LocalVariableTypeTable(LocalVariableTypeTableAttribute { local_variable_type_table }) })
    }

    pub fn new_custom_attribute(
        const_pool: &mut ConstantPool,
        name: String,
        payload: Vec<u8>,
    ) -> Result<Self, AttributeCreateError> {
        let name_index = const_pool.try_add_utf8(&name)?;
        Ok(Self { name: ConstPoolIndex::from(name_index), info: AttributeInfo::Custom(CustomAttribute { payload }) })
    }

    pub fn new_record_attribute(
        const_pool: &mut ConstantPool,
        components: Vec<RecordComponentInfo>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("Record")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::Record(RecordAttribute { components }) })
    }

    pub fn new_permitted_subclasses_attribute(
        const_pool: &mut ConstantPool,
        classes: Vec<ConstPoolIndex<ConstClassInfo>>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("PermittedSubclasses")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::PermittedSubclasses(PermittedSubclassesAttribute { classes }) })
    }

    pub fn new_inner_classes_attribute(
        const_pool: &mut ConstantPool,
        classes: Vec<InnerClassInfo>,
    ) -> Result<Self, AttributeCreateError> {
        let name = const_pool.try_add_utf8("InnerClasses")?;
        Ok(Self { name: ConstPoolIndex::from(name), info: AttributeInfo::InnerClasses(InnerClassesAttribute { classes }) })
    }

    pub fn to_bytes(&self, const_pool: &ConstantPool) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.name.as_u16().to_be_bytes());
        
        let info_bytes = self.info.to_bytes(const_pool);
        bytes.extend_from_slice(&(info_bytes.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&info_bytes);
        
        bytes
    }
    
    pub fn to_bytes_with_mapping(&self, const_pool: &ConstantPool, index_mapping: &std::collections::HashMap<u16, u16>) -> Vec<u8> {
        let mut bytes = Vec::new();
        
        // Apply index mapping to name index
        let mapped_name_index = *index_mapping.get(&self.name.as_u16()).unwrap_or(&self.name.as_u16());
        bytes.extend_from_slice(&mapped_name_index.to_be_bytes());
        
        let info_bytes = self.info.to_bytes_with_mapping(const_pool, index_mapping);
        bytes.extend_from_slice(&(info_bytes.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&info_bytes);
        
        bytes
    }
}

/// Trait for converting objects into NamedAttribute
pub trait TryIntoNamedAttribute {
    fn try_into_named_attribute(
        self,
        const_pool: &mut ConstantPool,
    ) -> Result<NamedAttribute, AttributeCreateError>;
}

impl TryIntoNamedAttribute for NamedAttribute {
    fn try_into_named_attribute(
        self,
        _: &mut ConstantPool,
    ) -> Result<NamedAttribute, AttributeCreateError> {
        Ok(self)
    }
}

/// Trait for adding attributes to objects
pub trait Attributable {
    fn add_attribute(&mut self, attribute: NamedAttribute) -> Result<(), AttributeAddError>;
}

// Add all missing attribute structure definitions
#[derive(Debug, Clone)]
pub struct ConstantValueAttribute {
    pub value: ConstPoolIndex<ConstValueInfo>,
}

impl ConstantValueAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>) {
        // payload: u2 constantvalue_index
        buffer.extend_from_slice(&self.value.as_u16().to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct SourceFileAttribute {
    pub filename: ConstPoolIndex<ConstUtf8Info>,
}

impl SourceFileAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.filename.as_u16().to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct SyntheticAttribute;

#[derive(Debug, Clone)]
pub struct DeprecatedAttribute;

#[derive(Debug, Clone)]
pub struct SignatureAttribute {
    pub signature: ConstPoolIndex<ConstUtf8Info>,
}

impl SignatureAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.signature.as_u16().to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct StackMapTableAttribute {
    pub stack_map_frames: JvmVecU2<StackMapFrame>,
}

impl StackMapTableAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(&self.stack_map_frames.len().to_be_bytes());
        for frame in self.stack_map_frames.iter() {
            buffer.extend_from_slice(&frame.to_bytes());
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExceptionsAttribute {
    pub exceptions: JvmVecU2<ConstPoolIndex<ConstClassInfo>>,
}

impl ExceptionsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.exceptions.len().to_be_bytes());
        for exception in self.exceptions.iter() {
            buffer.extend_from_slice(&exception.as_u16().to_be_bytes());
        }
    }
}

#[derive(Debug, Clone)]
pub struct InnerClassesAttribute {
    pub classes: Vec<InnerClassInfo>,
}

impl InnerClassesAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&(self.classes.len() as u16).to_be_bytes());
        for class_info in &self.classes {
            buffer.extend_from_slice(&class_info.inner_class.as_u16().to_be_bytes());
            buffer.extend_from_slice(&class_info.outer_class.as_u16().to_be_bytes());
            buffer.extend_from_slice(&class_info.inner_name.as_u16().to_be_bytes());
            buffer.extend_from_slice(&class_info.inner_class_access_flags.to_be_bytes());
        }
    }
}

#[derive(Debug, Clone)]
pub struct InnerClassInfo {
    pub inner_class: ConstPoolIndex<ConstClassInfo>,
    pub outer_class: ConstPoolIndex<ConstClassInfo>,
    pub inner_name: ConstPoolIndex<ConstUtf8Info>,
    pub inner_class_access_flags: u16,
}

#[derive(Debug, Clone)]
pub struct EnclosingMethodAttribute {
    pub class: ConstPoolIndex<ConstClassInfo>,
    pub method: ConstPoolIndex<ConstNameAndTypeInfo>,
}

impl EnclosingMethodAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.class.as_u16().to_be_bytes());
        buffer.extend_from_slice(&self.method.as_u16().to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct SourceDebugExtensionAttribute {
    pub debug_extension: Vec<u8>,
}

impl SourceDebugExtensionAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(&self.debug_extension);
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariableTypeTableAttribute {
    pub local_variable_type_table: Vec<LocalVariableTypeTableEntry>,
}

impl LocalVariableTypeTableAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&(self.local_variable_type_table.len() as u16).to_be_bytes());
        for entry in &self.local_variable_type_table {
            buffer.extend_from_slice(&entry.start_pc.to_be_bytes());
            buffer.extend_from_slice(&entry.length.to_be_bytes());
            buffer.extend_from_slice(&entry.name.as_u16().to_be_bytes());
            buffer.extend_from_slice(&entry.signature.as_u16().to_be_bytes());
            buffer.extend_from_slice(&entry.index.to_be_bytes());
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariableTypeTableEntry {
    pub start_pc: u16,
    pub length: u16,
    pub name: ConstPoolIndex<ConstUtf8Info>,
    pub signature: ConstPoolIndex<ConstUtf8Info>,
    pub index: u16,
}

#[derive(Debug, Clone)]
pub struct RuntimeVisibleAnnotationsAttribute {
    pub annotations: Vec<AnnotationEntry>,
}

impl RuntimeVisibleAnnotationsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // Placeholder implementation
        buffer.extend_from_slice(&(self.annotations.len() as u16).to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeInvisibleAnnotationsAttribute {
    pub annotations: Vec<AnnotationEntry>,
}

impl RuntimeInvisibleAnnotationsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // Placeholder implementation
        buffer.extend_from_slice(&(self.annotations.len() as u16).to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeVisibleParameterAnnotationsAttribute {
    pub annotations: Vec<Vec<AnnotationEntry>>,
}

impl RuntimeVisibleParameterAnnotationsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // Placeholder implementation
        buffer.extend_from_slice(&(self.annotations.len() as u8).to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeInvisibleParameterAnnotationsAttribute {
    pub annotations: Vec<Vec<AnnotationEntry>>,
}

impl RuntimeInvisibleParameterAnnotationsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // Placeholder implementation
        buffer.extend_from_slice(&(self.annotations.len() as u8).to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeVisibleTypeAnnotationsAttribute {
    pub annotations: Vec<TypeAnnotationEntry>,
}

impl RuntimeVisibleTypeAnnotationsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // Placeholder implementation
        buffer.extend_from_slice(&(self.annotations.len() as u16).to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeInvisibleTypeAnnotationsAttribute {
    pub annotations: Vec<TypeAnnotationEntry>,
}

impl RuntimeInvisibleTypeAnnotationsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // Placeholder implementation
        buffer.extend_from_slice(&(self.annotations.len() as u16).to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct AnnotationDefaultAttribute {
    pub value: ElementValue,
}

impl AnnotationDefaultAttribute {
    fn write_to_classfile(&self, _buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // Placeholder implementation
    }
}

#[derive(Debug, Clone)]
pub struct BootstrapMethodsAttribute {
    pub bootstrap_methods: Vec<BootstrapMethod>,
}

impl BootstrapMethodsAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&(self.bootstrap_methods.len() as u16).to_be_bytes());
        for method in &self.bootstrap_methods {
            buffer.extend_from_slice(&method.bootstrap_method.as_u16().to_be_bytes());
            buffer.extend_from_slice(&(method.bootstrap_arguments.len() as u16).to_be_bytes());
            for arg in &method.bootstrap_arguments {
                buffer.extend_from_slice(&arg.as_u16().to_be_bytes());
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct BootstrapMethod {
    pub bootstrap_method: ConstPoolIndex<ConstClassInfo>,
    pub bootstrap_arguments: Vec<ConstPoolIndex<ConstClassInfo>>,
}

#[derive(Debug, Clone)]
pub struct MethodParametersAttribute {
    pub method_parameters: Vec<MethodParameter>,
}

impl MethodParametersAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&(self.method_parameters.len() as u8).to_be_bytes());
        for param in &self.method_parameters {
            buffer.extend_from_slice(&param.name.as_u16().to_be_bytes());
            buffer.extend_from_slice(&param.access_flags.to_be_bytes());
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodParameter {
    pub name: ConstPoolIndex<ConstUtf8Info>,
    pub access_flags: u16,
}

#[derive(Debug, Clone)]
pub struct ModuleAttribute {
    pub name: ConstPoolIndex<ConstUtf8Info>,
    pub flags: u16,
    pub version: ConstPoolIndex<ConstUtf8Info>,
    pub requires: JvmVecU2<ModuleRequires>,
    pub exports: JvmVecU2<ModuleExports>,
    pub opens: JvmVecU2<ModuleOpens>,
    pub uses: JvmVecU2<ModuleUses>,
    pub provides: JvmVecU2<ModuleProvides>,
}

impl ModuleAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        // module_name_index, module_flags, module_version_index
        buffer.extend_from_slice(&self.name.as_u16().to_be_bytes());
        buffer.extend_from_slice(&self.flags.to_be_bytes());
        buffer.extend_from_slice(&self.version.as_u16().to_be_bytes());

        // requires
        buffer.extend_from_slice(&self.requires.len().to_be_bytes());
        for r in self.requires.iter() {
            buffer.extend_from_slice(&r.requires.as_u16().to_be_bytes());
            buffer.extend_from_slice(&r.flags.to_be_bytes());
            buffer.extend_from_slice(&r.version.as_u16().to_be_bytes());
        }

        // exports
        buffer.extend_from_slice(&self.exports.len().to_be_bytes());
        for e in self.exports.iter() {
            buffer.extend_from_slice(&e.exports.as_u16().to_be_bytes());
            buffer.extend_from_slice(&e.flags.to_be_bytes());
            buffer.extend_from_slice(&e.exports_to.len().to_be_bytes());
            for to in e.exports_to.iter() {
                buffer.extend_from_slice(&to.as_u16().to_be_bytes());
            }
        }

        // opens
        buffer.extend_from_slice(&self.opens.len().to_be_bytes());
        for o in self.opens.iter() {
            buffer.extend_from_slice(&o.opens.as_u16().to_be_bytes());
            buffer.extend_from_slice(&o.flags.to_be_bytes());
            buffer.extend_from_slice(&o.opens_to.len().to_be_bytes());
            for to in o.opens_to.iter() {
                buffer.extend_from_slice(&to.as_u16().to_be_bytes());
            }
        }

        // uses
        buffer.extend_from_slice(&self.uses.len().to_be_bytes());
        for u in self.uses.iter() {
            buffer.extend_from_slice(&u.uses.as_u16().to_be_bytes());
        }

        // provides
        buffer.extend_from_slice(&self.provides.len().to_be_bytes());
        for p in self.provides.iter() {
            buffer.extend_from_slice(&p.provides.as_u16().to_be_bytes());
            buffer.extend_from_slice(&p.provides_with.len().to_be_bytes());
            for w in p.provides_with.iter() {
                buffer.extend_from_slice(&w.as_u16().to_be_bytes());
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleRequires {
    pub requires: ConstPoolIndex<ConstUtf8Info>,
    pub flags: u16,
    pub version: ConstPoolIndex<ConstUtf8Info>,
}

#[derive(Debug, Clone)]
pub struct ModuleExports {
    pub exports: ConstPoolIndex<ConstUtf8Info>,
    pub flags: u16,
    pub exports_to: JvmVecU2<ConstPoolIndex<ConstUtf8Info>>,
}

#[derive(Debug, Clone)]
pub struct ModuleOpens {
    pub opens: ConstPoolIndex<ConstUtf8Info>,
    pub flags: u16,
    pub opens_to: JvmVecU2<ConstPoolIndex<ConstUtf8Info>>,
}

#[derive(Debug, Clone)]
pub struct ModuleUses {
    pub uses: ConstPoolIndex<ConstUtf8Info>,
}

#[derive(Debug, Clone)]
pub struct ModuleProvides {
    pub provides: ConstPoolIndex<ConstUtf8Info>,
    pub provides_with: JvmVecU2<ConstPoolIndex<ConstUtf8Info>>,
}

#[derive(Debug, Clone)]
pub struct ModulePackagesAttribute {
    pub packages: JvmVecU2<ConstPoolIndex<ConstUtf8Info>>,
}

impl ModulePackagesAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.packages.len().to_be_bytes());
        for package in self.packages.iter() {
            buffer.extend_from_slice(&package.as_u16().to_be_bytes());
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleMainClassAttribute {
    pub main_class: ConstPoolIndex<ConstClassInfo>,
}

impl ModuleMainClassAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.main_class.as_u16().to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct NestHostAttribute {
    pub host_class: ConstPoolIndex<ConstClassInfo>,
}

impl NestHostAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.host_class.as_u16().to_be_bytes());
    }
}

#[derive(Debug, Clone)]
pub struct NestMembersAttribute {
    pub classes: Vec<ConstPoolIndex<ConstClassInfo>>,
}

impl NestMembersAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&(self.classes.len() as u16).to_be_bytes());
        for class in &self.classes {
            buffer.extend_from_slice(&class.as_u16().to_be_bytes());
        }
    }
}

// Java 14+: Record
#[derive(Debug, Clone)]
pub struct RecordAttribute {
    pub components: Vec<RecordComponentInfo>,
}

impl RecordAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&(self.components.len() as u16).to_be_bytes());
        for c in &self.components {
            buffer.extend_from_slice(&c.name.as_u16().to_be_bytes());
            buffer.extend_from_slice(&c.descriptor.as_u16().to_be_bytes());
            buffer.extend_from_slice(&(c.attributes.len() as u16).to_be_bytes());
            for a in &c.attributes { buffer.extend_from_slice(&a.to_bytes(&ConstantPool::new())); }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RecordComponentInfo {
    pub name: ConstPoolIndex<ConstUtf8Info>,
    pub descriptor: ConstPoolIndex<ConstUtf8Info>,
    pub attributes: Vec<NamedAttribute>,
}

// Java 15+: PermittedSubclasses
#[derive(Debug, Clone)]
pub struct PermittedSubclassesAttribute {
    pub classes: Vec<ConstPoolIndex<ConstClassInfo>>, // u2 count + u2 classes[count]
}

impl PermittedSubclassesAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&(self.classes.len() as u16).to_be_bytes());
        for c in &self.classes { buffer.extend_from_slice(&c.as_u16().to_be_bytes()); }
    }
}

#[derive(Debug, Clone)]
pub struct CustomAttribute {
    pub payload: Vec<u8>,
}

impl CustomAttribute {
    fn write_to_classfile(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(&self.payload);
    }
}

// Placeholder but enriched types for annotations (carry optional retention/targets metadata for verify-time checks)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RetentionPolicy { Source, Class, Runtime }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationTarget {
    Type,
    Field,
    Method,
    Parameter,
    Constructor,
    LocalVariable,
    AnnotationType,
    Package,
    TypeParameter,
    TypeUse,
}

#[derive(Debug, Clone)]
pub struct AnnotationEntry {
    pub type_name: ConstPoolIndex<ConstUtf8Info>,
    pub retention: Option<RetentionPolicy>,
    pub targets: Vec<AnnotationTarget>,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotationEntry {
    pub type_name: ConstPoolIndex<ConstUtf8Info>,
    pub retention: Option<RetentionPolicy>,
    pub targets: Vec<AnnotationTarget>,
}

#[derive(Debug, Clone)]
pub struct ElementValue;

/// Strongly-typed attribute info (aligned to javac-rs)
#[derive(Debug, Clone)]
pub enum AttributeInfo {
    ConstantValue(ConstantValueAttribute),
    Code(CodeAttribute),
    StackMapTable(StackMapTableAttribute),
    Exceptions(ExceptionsAttribute),
    InnerClasses(InnerClassesAttribute),
    EnclosingMethod(EnclosingMethodAttribute),
    Synthetic(SyntheticAttribute),
    Signature(SignatureAttribute),
    SourceFile(SourceFileAttribute),
    SourceDebugExtension(SourceDebugExtensionAttribute),
    LineNumberTable(LineNumberTableAttribute),
    LocalVariableTable(LocalVariableTableAttribute),
    LocalVariableTypeTable(LocalVariableTypeTableAttribute),
    Deprecated(DeprecatedAttribute),
    RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute),
    RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute),
    RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute),
    RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute),
    RuntimeVisibleTypeAnnotations(RuntimeVisibleTypeAnnotationsAttribute),
    RuntimeInvisibleTypeAnnotations(RuntimeInvisibleTypeAnnotationsAttribute),
    AnnotationDefault(AnnotationDefaultAttribute),
    BootstrapMethods(BootstrapMethodsAttribute),
    MethodParameters(MethodParametersAttribute),
    Module(ModuleAttribute),
    ModulePackages(ModulePackagesAttribute),
    ModuleMainClass(ModuleMainClassAttribute),
    NestHost(NestHostAttribute),
    NestMembers(NestMembersAttribute),
    // Java 14+: Record attribute
    Record(RecordAttribute),
    // Java 15+: PermittedSubclasses attribute for sealed classes
    PermittedSubclasses(PermittedSubclassesAttribute),
    Custom(CustomAttribute),
}

impl AttributeInfo {
    pub fn to_bytes(&self, const_pool: &ConstantPool) -> Vec<u8> {
        let mut buffer = Vec::new();
        match self {
            Self::ConstantValue(v) => v.write_to_classfile(&mut buffer),
            Self::Code(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::StackMapTable(v) => v.write_to_classfile(&mut buffer),
            Self::Exceptions(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::InnerClasses(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::EnclosingMethod(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::Synthetic(_) => {},
            Self::Signature(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::SourceFile(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::SourceDebugExtension(v) => v.write_to_classfile(&mut buffer),
            Self::LineNumberTable(v) => v.write_to_classfile(&mut buffer),
            Self::LocalVariableTable(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::LocalVariableTypeTable(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::Deprecated(_) => {},
            Self::RuntimeVisibleAnnotations(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::RuntimeInvisibleAnnotations(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::RuntimeVisibleParameterAnnotations(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::RuntimeInvisibleParameterAnnotations(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::RuntimeVisibleTypeAnnotations(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::RuntimeInvisibleTypeAnnotations(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::AnnotationDefault(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::BootstrapMethods(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::MethodParameters(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::Module(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::ModulePackages(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::ModuleMainClass(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::NestHost(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::NestMembers(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::Record(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::PermittedSubclasses(v) => v.write_to_classfile(&mut buffer, const_pool),
            Self::Custom(v) => v.write_to_classfile(&mut buffer),
        }
        buffer
    }
    
    pub fn to_bytes_with_mapping(&self, const_pool: &ConstantPool, index_mapping: &std::collections::HashMap<u16, u16>) -> Vec<u8> {
        // For now, we'll implement mapping for the most common attributes
        // and fall back to the original method for others
        match self {
            Self::Signature(v) => {
                let mut buffer = Vec::new();
                // Apply mapping to signature index
                let mapped_signature_index = *index_mapping.get(&v.signature.as_u16()).unwrap_or(&v.signature.as_u16());
                buffer.extend_from_slice(&mapped_signature_index.to_be_bytes());
                buffer
            }
            Self::SourceFile(v) => {
                let mut buffer = Vec::new();
                // Apply mapping to filename index
                let mapped_filename_index = *index_mapping.get(&v.filename.as_u16()).unwrap_or(&v.filename.as_u16());
                buffer.extend_from_slice(&mapped_filename_index.to_be_bytes());
                buffer
            }
            // For other attributes, fall back to original method
            // TODO: Add mapping support for other attribute types as needed
            _ => self.to_bytes(const_pool)
        }
    }

    // Back-compat constructor used by class_writer helpers
    pub fn new(_name_index: u16, info: Vec<u8>) -> Self {
        // Without the name we cannot discriminate the type reliably; keep as Custom
        Self::Custom(CustomAttribute { payload: info })
    }
}

#[derive(Debug, Clone)]
pub struct CodeAttribute {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<u8>,
    pub exception_table: Vec<ExceptionTableEntry>,
    pub attributes: Vec<NamedAttribute>,
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

    pub fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.max_stack.to_be_bytes());
        buffer.extend_from_slice(&self.max_locals.to_be_bytes());
        buffer.extend_from_slice(&(self.code.len() as u32).to_be_bytes());
        buffer.extend_from_slice(&self.code);
        buffer.extend_from_slice(&(self.exception_table.len() as u16).to_be_bytes());
        for entry in &self.exception_table {
            buffer.extend_from_slice(&entry.to_bytes());
        }
        buffer.extend_from_slice(&(self.attributes.len() as u16).to_be_bytes());
        for attribute in &self.attributes {
            buffer.extend_from_slice(&attribute.to_bytes(&ConstantPool::new()));
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LineNumberTableAttribute {
    pub line_number_table: JvmVecU2<LineNumberEntry>,
}

impl LineNumberTableAttribute {
    pub fn new() -> Self { Self { line_number_table: JvmVecU2::new() } }

    pub fn add_line_number(&mut self, start_pc: u16, line_number: u16) -> super::vec::JvmVecResult<u16> {
        self.line_number_table.push(LineNumberEntry { start_pc, line_number })
    }

    pub fn write_to_classfile(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(&self.line_number_table.len().to_be_bytes());
        for entry in self.line_number_table.iter() {
            buffer.extend_from_slice(&entry.to_bytes());
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.line_number_table.len().to_be_bytes());
        for entry in self.line_number_table.iter() {
            bytes.extend_from_slice(&entry.to_bytes());
        }
        bytes
    }
}

#[derive(Debug, Clone)]
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
pub fn make_line_number_table_attribute(constant_pool: &mut ConstantPool, table: &LineNumberTableAttribute) -> Result<NamedAttribute, AttributeCreateError> {
    let name_index = constant_pool.try_add_utf8("LineNumberTable")?;
    Ok(NamedAttribute::new(ConstPoolIndex::from(name_index), AttributeInfo::LineNumberTable(table.clone())))
}

#[derive(Debug, Clone)]
pub struct LocalVariableTableAttribute {
    pub entries: JvmVecU2<LocalVariableEntry>,
}

impl LocalVariableTableAttribute {
    pub fn new() -> Self { Self { entries: JvmVecU2::new() } }
    
    pub fn write_to_classfile(&self, buffer: &mut Vec<u8>, _const_pool: &ConstantPool) {
        buffer.extend_from_slice(&self.entries.len().to_be_bytes());
        for entry in self.entries.iter() {
            buffer.extend_from_slice(&entry.to_bytes());
        }
    }
    
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.entries.len().to_be_bytes());
        for entry in self.entries.iter() {
            bytes.extend_from_slice(&entry.to_bytes());
        }
        bytes
    }
}

#[derive(Debug, Clone)]
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
pub fn make_local_variable_table_attribute(constant_pool: &mut ConstantPool, table: &LocalVariableTableAttribute) -> Result<NamedAttribute, AttributeCreateError> {
    let name_index = constant_pool.try_add_utf8("LocalVariableTable")?;
    Ok(NamedAttribute::new(ConstPoolIndex::from(name_index), AttributeInfo::LocalVariableTable(table.clone())))
}

/// Helper to build a StackMapTable attribute
pub fn make_stack_map_attribute(constant_pool: &mut ConstantPool, frames: &[StackMapFrame]) -> Result<NamedAttribute, AttributeCreateError> {
    let name_index = constant_pool.try_add_utf8("StackMapTable")?;
    let attr = StackMapTableAttribute { stack_map_frames: frames.to_vec().into() };
    Ok(NamedAttribute::new(ConstPoolIndex::from(name_index), AttributeInfo::StackMapTable(attr)))
}


