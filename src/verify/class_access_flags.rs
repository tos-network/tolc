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
    eprintln!("🔍 VERIFY: Checking class access flags: 0x{:04x}", flags);

    let has = |bit: u16| flags & bit != 0;

    // @interface must also be interface
    if has(access_flags::ACC_ANNOTATION) && !has(access_flags::ACC_INTERFACE) {
        eprintln!("🔍 VERIFY: Failed - annotation without interface");
        return Err(ClassAccessFlagsError::Invalid(flags));
    }

    if has(access_flags::ACC_INTERFACE) {
        eprintln!("🔍 VERIFY: Checking interface constraints");
        // interface must be abstract unless it's package-info
        if !has(access_flags::ACC_ABSTRACT) {
            let name_ok = match class_name {
                Some(full) => full.split('/').last().unwrap_or("") == "package-info",
                None => false,
            };
            if !name_ok {
                eprintln!("🔍 VERIFY: Failed - interface without abstract");
                return Err(ClassAccessFlagsError::Invalid(flags));
            }
        }
        // interface cannot be final, super, enum, or module
        if has(access_flags::ACC_FINAL)
            || has(access_flags::ACC_SUPER)
            || has(access_flags::ACC_ENUM)
            || has(access_flags::ACC_MODULE)
        {
            eprintln!("🔍 VERIFY: Failed - interface with forbidden flags");
            return Err(ClassAccessFlagsError::Invalid(flags));
        }
    } else if has(access_flags::ACC_FINAL) && has(access_flags::ACC_ABSTRACT) {
        // class cannot be both abstract and final
        eprintln!("🔍 VERIFY: Failed - final and abstract");
        return Err(ClassAccessFlagsError::Invalid(flags));
    }

    // Sealed classes (ACC_SEALED) sanity: do not allow on interfaces here, and require not final
    // NOTE: ACC_SEALED (0x0020) conflicts with ACC_SUPER historically - only validate for Java 17+
    // For now, skip sealed class validation since ACC_SEALED == ACC_SUPER
    // if has(access_flags::ACC_SEALED) && major_version >= 61 {
    //     eprintln!("🔍 VERIFY: Checking sealed class constraints");
    //     if has(access_flags::ACC_INTERFACE) { 
    //         eprintln!("🔍 VERIFY: Failed - sealed interface");
    //         return Err(ClassAccessFlagsError::Invalid(flags)); 
    //     }
    //     if has(access_flags::ACC_FINAL) { 
    //         eprintln!("🔍 VERIFY: Failed - sealed and final");
    //         return Err(ClassAccessFlagsError::Invalid(flags)); 
    //     }
    // }
    
    eprintln!("🔍 VERIFY: Access flags verification passed");

    Ok(())
}


