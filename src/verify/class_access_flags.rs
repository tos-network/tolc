use crate::codegen::class::ClassFile;
use crate::codegen::flag::access_flags;

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum ClassAccessFlagsError {
    #[error("Invalid class access flags: 0x{0:04x}")]
    Invalid(u16),
}

pub type Result<T> = std::result::Result<T, ClassAccessFlagsError>;

/// Verify the ClassFile access flags, adapted from ristretto
pub fn verify(class_file: &ClassFile, class_name: Option<&str>) -> Result<()> {
    let flags = class_file.access_flags;

    let has = |bit: u16| flags & bit != 0;

    // @interface must also be interface
    if has(access_flags::ACC_ANNOTATION) && !has(access_flags::ACC_INTERFACE) {
        return Err(ClassAccessFlagsError::Invalid(flags));
    }

    if has(access_flags::ACC_INTERFACE) {
        // interface must be abstract unless it's package-info
        if !has(access_flags::ACC_ABSTRACT) {
            let name_ok = match class_name {
                Some(full) => full.split('/').last().unwrap_or("") == "package-info",
                None => false,
            };
            if !name_ok {
                return Err(ClassAccessFlagsError::Invalid(flags));
            }
        }
        // interface cannot be final, super, enum, or module
        if has(access_flags::ACC_FINAL)
            || has(access_flags::ACC_SUPER)
            || has(access_flags::ACC_ENUM)
            || has(access_flags::ACC_MODULE)
        {
            return Err(ClassAccessFlagsError::Invalid(flags));
        }
    } else if has(access_flags::ACC_FINAL) && has(access_flags::ACC_ABSTRACT) {
        // class cannot be both abstract and final
        return Err(ClassAccessFlagsError::Invalid(flags));
    }

    Ok(())
}


