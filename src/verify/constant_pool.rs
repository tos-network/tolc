use crate::codegen::constpool::Constant;
use crate::codegen::class::ClassFile;
use crate::codegen::attribute::AttributeInfo;

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum ConstantPoolVerifyError {
    #[error("Invalid version constant tag {0}")]
    InvalidVersionConstant(u8),
    #[error("Invalid constant pool index {0}")]
    InvalidConstantPoolIndex(u16),
    #[error("Invalid constant pool index type {0}")]
    InvalidConstantPoolIndexType(u16),
    #[error("BootstrapMethods attribute not defined")]
    BootstrapMethodsNotDefined,
    #[error("Invalid bootstrap method index {0}")]
    InvalidBootstrapMethodIndex(usize),
}

pub type Result<T> = std::result::Result<T, ConstantPoolVerifyError>;

/// Verify the ClassFile ConstantPool
pub fn verify(class_file: &ClassFile) -> Result<()> {
    verify_version_constants(class_file)?;
    verify_constant_indexes(class_file)?;
    Ok(())
}

fn verify_version_constants(_class_file: &ClassFile) -> Result<()> {
    // Our Constant type does not currently carry version validity helpers.
    // Leave as no-op for now; hook when versioning utilities are added.
    Ok(())
}

fn verify_constant_indexes(class_file: &ClassFile) -> Result<()> {
    let pool = &class_file.constant_pool.constants;
    for (i, constant) in pool.iter().enumerate() {
        let index = i as u16;
        match constant {
            Constant::Class(name_index)
            | Constant::Module(name_index)
            | Constant::Package(name_index) => match pool.get((*name_index as usize).saturating_sub(1)) {
                Some(Constant::Utf8(_)) => {}
                None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
            },
            Constant::String(string_index) => match pool.get((*string_index as usize).saturating_sub(1)) {
                Some(Constant::Utf8(_)) => {}
                None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
            },
            Constant::FieldRef(class_index, nat_index)
            | Constant::MethodRef(class_index, nat_index)
            | Constant::InterfaceMethodRef(class_index, nat_index) => {
                match pool.get((*class_index as usize).saturating_sub(1)) {
                    Some(Constant::Class(_)) => {}
                    None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                    _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
                }
                match pool.get((*nat_index as usize).saturating_sub(1)) {
                    Some(Constant::NameAndType(_, _)) => {}
                    None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                    _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
                }
            }
            Constant::NameAndType(name_index, desc_index) => {
                match pool.get((*name_index as usize).saturating_sub(1)) {
                    Some(Constant::Utf8(_)) => {}
                    None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                    _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
                }
                match pool.get((*desc_index as usize).saturating_sub(1)) {
                    Some(Constant::Utf8(_)) => {}
                    None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                    _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
                }
            }
            Constant::MethodHandle(_kind, reference_index) => {
                // Without ReferenceKind type mapping in our consts, conservatively allow Field/Method/InterfaceMethodRefs
                match pool.get((*reference_index as usize).saturating_sub(1)) {
                    Some(Constant::FieldRef(_, _))
                    | Some(Constant::MethodRef(_, _))
                    | Some(Constant::InterfaceMethodRef(_, _)) => {}
                    None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                    _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
                }
            }
            Constant::MethodType(descriptor_index) => match pool.get((*descriptor_index as usize).saturating_sub(1)) {
                Some(Constant::Utf8(_)) => {}
                None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
            },
            Constant::Dynamic(_bsm_index, nat_index)
            | Constant::InvokeDynamic(_bsm_index, nat_index) => {
                // Ensure BootstrapMethods attribute exists
                let mut has_bootstrap = false;
                for a in &class_file.attributes {
                    if let AttributeInfo::BootstrapMethods(_) = a.info { has_bootstrap = true; break; }
                }
                if !has_bootstrap {
                    return Err(ConstantPoolVerifyError::BootstrapMethodsNotDefined);
                }
                match pool.get((*nat_index as usize).saturating_sub(1)) {
                    Some(Constant::NameAndType(_, _)) => {}
                    None => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndex(index)),
                    _ => return Err(ConstantPoolVerifyError::InvalidConstantPoolIndexType(index)),
                }
            }
            _ => {}
        }
    }
    Ok(())
}


