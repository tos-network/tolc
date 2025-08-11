use crate::codegen::class::ClassFile;
use crate::codegen::method::MethodInfo;
use crate::codegen::flag::access_flags;

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum MethodAccessFlagsError {
    #[error("Invalid method access flags: 0x{0:04x}")]
    Invalid(u16),
}

pub type Result<T> = std::result::Result<T, MethodAccessFlagsError>;

/// Verify method access flags (adapted from Ristretto)
pub fn verify(class_file: &ClassFile, method: &MethodInfo) -> Result<()> {
    let flags = method.access_flags;
    let public_set = flags & access_flags::ACC_PUBLIC != 0;
    let protected_set = flags & access_flags::ACC_PROTECTED != 0;
    let private_set = flags & access_flags::ACC_PRIVATE != 0;

    if (public_set as u8 + protected_set as u8 + private_set as u8) > 1 {
        return Err(MethodAccessFlagsError::Invalid(flags));
    }

    let class_is_interface = class_file.access_flags & access_flags::ACC_INTERFACE != 0;
    if class_is_interface
        && (flags & access_flags::ACC_PROTECTED != 0
            || flags & access_flags::ACC_FINAL != 0
            || flags & access_flags::ACC_SYNCHRONIZED != 0
            || flags & access_flags::ACC_NATIVE != 0)
    {
        return Err(MethodAccessFlagsError::Invalid(flags));
    }

    if flags & access_flags::ACC_ABSTRACT != 0 {
        if flags & access_flags::ACC_PRIVATE != 0
            || flags & access_flags::ACC_STATIC != 0
            || flags & access_flags::ACC_FINAL != 0
            || flags & access_flags::ACC_SYNCHRONIZED != 0
            || flags & access_flags::ACC_NATIVE != 0
        {
            return Err(MethodAccessFlagsError::Invalid(flags));
        }

        // Our ClassFile lacks a full version struct; assuming JAVA_8 (52) semantics as default
        // If later we add version, enforce 46..=60 range, and ACC_STRICT invalid with ABSTRACT
        if flags & access_flags::ACC_STRICT != 0 {
            return Err(MethodAccessFlagsError::Invalid(flags));
        }
    }

    Ok(())
}


