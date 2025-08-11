use crate::codegen::class::ClassFile;
use crate::codegen::constpool::Constant;

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum InterfacesVerifyError {
    #[error("Invalid constant pool index {0}")]
    InvalidConstantPoolIndex(u16),
    #[error("Invalid constant pool index type {0}")]
    InvalidConstantPoolIndexType(u16),
}

pub type Result<T> = std::result::Result<T, InterfacesVerifyError>;

/// Verify the ClassFile interfaces
pub fn verify(class_file: &ClassFile) -> Result<()> {
    let pool = &class_file.constant_pool.constants;
    for interface in &class_file.interfaces {
        let idx = (*interface as usize).saturating_sub(1);
        match pool.get(idx) {
            Some(Constant::Class(_)) => {}
            None => return Err(InterfacesVerifyError::InvalidConstantPoolIndex(*interface)),
            _ => return Err(InterfacesVerifyError::InvalidConstantPoolIndexType(*interface)),
        }
    }
    Ok(())
}


