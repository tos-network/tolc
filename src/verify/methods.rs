use crate::codegen::class::ClassFile;
use crate::codegen::constpool::Constant;
use super::method_access_flags;
use crate::codegen::opcodes;
use crate::codegen::attribute::AttributeInfo;

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum MethodVerifyError {
    #[error("Invalid constant pool index {0}")]
    InvalidConstantPoolIndex(u16),
    #[error("Invalid constant pool index type {0}")]
    InvalidConstantPoolIndexType(u16),
    #[error("Invalid method access flags: 0x{0:04x}")]
    InvalidMethodAccessFlags(u16),
    #[error("Method must have Code attribute unless abstract or native")]
    MissingCodeAttribute,
    #[error("Abstract or native method must not have Code attribute")]
    ForbiddenCodeAttribute,
    #[error("Invalid method attribute: {0}")]
    InvalidMethodAttribute(String),
    #[error("Duplicate method attribute: {0}")]
    DuplicateMethodAttribute(String),
    #[error("Return opcode does not match descriptor: expected {expected}, found {found}")]
    ReturnMismatch { expected: &'static str, found: String },
}

pub type Result<T> = std::result::Result<T, MethodVerifyError>;

/// Verify the ClassFile methods
pub fn verify(class_file: &ClassFile) -> Result<()> {
    for method in &class_file.methods {
        // access flags
        if let Err(super::method_access_flags::MethodAccessFlagsError::Invalid(bits)) = 
            method_access_flags::verify(class_file, method)
        {
            return Err(MethodVerifyError::InvalidMethodAccessFlags(bits));
        }
        verify_name_index(class_file, method.name_index)?;
        verify_descriptor_index(class_file, method.descriptor_index)?;
        verify_method_attributes(class_file, method)?;
    }
    Ok(())
}

fn verify_name_index(class_file: &ClassFile, name_index: u16) -> Result<()> {
    let idx = (name_index as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(_)) => Ok(()),
        None => Err(MethodVerifyError::InvalidConstantPoolIndex(name_index)),
        _ => Err(MethodVerifyError::InvalidConstantPoolIndexType(name_index)),
    }
}

fn verify_descriptor_index(class_file: &ClassFile, descriptor_index: u16) -> Result<()> {
    let idx = (descriptor_index as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(_)) => Ok(()),
        None => Err(MethodVerifyError::InvalidConstantPoolIndex(descriptor_index)),
        _ => Err(MethodVerifyError::InvalidConstantPoolIndexType(descriptor_index)),
    }
}

fn verify_method_attributes(class_file: &ClassFile, method: &crate::codegen::method::MethodInfo) -> Result<()> {
    let mut has_code = false;
    let mut has_signature = false;
    let mut has_exceptions = false;
    let is_abstract = method.access_flags & crate::codegen::flag::access_flags::ACC_ABSTRACT != 0;
    let is_native = method.access_flags & crate::codegen::flag::access_flags::ACC_NATIVE != 0;

    for a in &method.attributes {
        match &a.info {
            AttributeInfo::Code(code_attr) => {
                if has_code { return Err(MethodVerifyError::DuplicateMethodAttribute("Code".to_string())); }
                has_code = true;
                // If method has a body, verify last opcode matches descriptor return
                let desc = cp_utf8(class_file, method.descriptor_index)?;
                if let Some(last) = code_attr.code.last() {
                    let expected = expected_return_for_descriptor(&desc);
                    if !matches_return_opcode(*last, &expected) {
                        return Err(MethodVerifyError::ReturnMismatch {
                            expected: expected.name,
                            found: format!("0x{:02x}", last),
                        });
                    }
                }
            }
            AttributeInfo::Exceptions(ex_attr) => {
                if has_exceptions { return Err(MethodVerifyError::DuplicateMethodAttribute("Exceptions".to_string())); }
                has_exceptions = true;
                // Validate each exception index points to Class
                for exc in ex_attr.exceptions.iter() {
                    let u = exc.as_u16();
                    let idx = (u as usize).saturating_sub(1);
                    match class_file.constant_pool.constants.get(idx) {
                        Some(Constant::Class(_)) => {}
                        None => return Err(MethodVerifyError::InvalidConstantPoolIndex(u)),
                        _ => return Err(MethodVerifyError::InvalidConstantPoolIndexType(u)),
                    }
                }
            }
            AttributeInfo::Signature(_) => {
                if has_signature { return Err(MethodVerifyError::DuplicateMethodAttribute("Signature".to_string())); }
                has_signature = true;
            }
            AttributeInfo::Synthetic(_) | AttributeInfo::Deprecated(_) => {}
            AttributeInfo::RuntimeVisibleAnnotations(_)
            | AttributeInfo::RuntimeInvisibleAnnotations(_)
            | AttributeInfo::RuntimeVisibleParameterAnnotations(_)
            | AttributeInfo::RuntimeInvisibleParameterAnnotations(_)
            | AttributeInfo::RuntimeVisibleTypeAnnotations(_)
            | AttributeInfo::RuntimeInvisibleTypeAnnotations(_)
            | AttributeInfo::MethodParameters(_) 
            | AttributeInfo::AnnotationDefault(_) => {}
            // Disallow non-method-only attributes
            AttributeInfo::ConstantValue(_)
            | AttributeInfo::LineNumberTable(_)
            | AttributeInfo::LocalVariableTable(_)
            | AttributeInfo::LocalVariableTypeTable(_)
            | AttributeInfo::StackMapTable(_)
            | AttributeInfo::InnerClasses(_)
            | AttributeInfo::EnclosingMethod(_)
            | AttributeInfo::SourceFile(_)
            | AttributeInfo::SourceDebugExtension(_)
            | AttributeInfo::BootstrapMethods(_)
            | AttributeInfo::Module(_)
            | AttributeInfo::ModulePackages(_)
            | AttributeInfo::ModuleMainClass(_)
            | AttributeInfo::NestHost(_)
            | AttributeInfo::NestMembers(_) => {
                return Err(MethodVerifyError::InvalidMethodAttribute(attribute_name(class_file, a)));
            }
            AttributeInfo::Custom(_) => {
                return Err(MethodVerifyError::InvalidMethodAttribute(attribute_name(class_file, a)));
            }
        }
    }

    if is_abstract || is_native {
        if has_code { return Err(MethodVerifyError::ForbiddenCodeAttribute); }
    } else {
        if !has_code { return Err(MethodVerifyError::MissingCodeAttribute); }
    }

    Ok(())
}

fn attribute_name(class_file: &ClassFile, a: &crate::codegen::attribute::NamedAttribute) -> String {
    let idx = (a.name.as_u16() as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(s)) => s.clone(),
        _ => String::from("<invalid-name>"),
    }
}

struct ExpectedReturn { name: &'static str, opcodes: &'static [u8] }

fn expected_return_for_descriptor(descriptor: &str) -> ExpectedReturn {
    // descriptor format: (args)Ret
    let ret = descriptor.rsplit(')').next().unwrap_or("V");
    let mut chars = ret.chars();
    let c = chars.next().unwrap_or('V');
    match c {
        'V' => ExpectedReturn { name: "RETURN", opcodes: &[opcodes::RETURN] },
        'J' => ExpectedReturn { name: "LRETURN", opcodes: &[opcodes::LRETURN] },
        'F' => ExpectedReturn { name: "FRETURN", opcodes: &[opcodes::FRETURN] },
        'D' => ExpectedReturn { name: "DRETURN", opcodes: &[opcodes::DRETURN] },
        'I' | 'S' | 'B' | 'C' | 'Z' => ExpectedReturn { name: "IRETURN", opcodes: &[opcodes::IRETURN] },
        'L' | '[' => ExpectedReturn { name: "ARETURN", opcodes: &[opcodes::ARETURN] },
        _ => ExpectedReturn { name: "<unknown>", opcodes: &[] },
    }
}

fn matches_return_opcode(last: u8, expected: &ExpectedReturn) -> bool {
    expected.opcodes.contains(&last)
}

fn cp_utf8(class_file: &ClassFile, idx_u16: u16) -> Result<String> {
    let idx = (idx_u16 as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(s)) => Ok(s.clone()),
        None => Err(MethodVerifyError::InvalidConstantPoolIndex(idx_u16)),
        _ => Err(MethodVerifyError::InvalidConstantPoolIndexType(idx_u16)),
    }
}


