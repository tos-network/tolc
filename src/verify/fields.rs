use crate::codegen::class::ClassFile;
use crate::codegen::constpool::Constant;
use crate::codegen::flag::access_flags;
use crate::codegen::attribute::{AttributeInfo, NamedAttribute};

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum FieldVerifyError {
    #[error("Invalid constant pool index {0}")]
    InvalidConstantPoolIndex(u16),
    #[error("Invalid constant pool index type {0}")]
    InvalidConstantPoolIndexType(u16),
    #[error("Invalid field access flags: 0x{0:04x}")]
    InvalidFieldAccessFlags(u16),
    #[error("Invalid field attribute: {0}")]
    InvalidFieldAttribute(String),
    #[error("Duplicate field attribute: {0}")]
    DuplicateFieldAttribute(String),
    #[error("ConstantValue on non-static field")]
    ConstantValueOnNonStatic,
    #[error("ConstantValue refers to invalid constant kind for field")]
    InvalidConstantValueKind,
}

pub type Result<T> = std::result::Result<T, FieldVerifyError>;

pub fn verify(class_file: &ClassFile) -> Result<()> {
    for field in &class_file.fields {
        verify_name_index(class_file, field.name_index)?;
        verify_descriptor_index(class_file, field.descriptor_index)?;
        verify_access_flags(class_file, field.access_flags)?;
        verify_attributes(class_file, field.access_flags, field.attributes.as_slice())?;
    }
    Ok(())
}

fn verify_name_index(class_file: &ClassFile, name_index: u16) -> Result<()> {
    let idx = (name_index as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(_)) => Ok(()),
        None => Err(FieldVerifyError::InvalidConstantPoolIndex(name_index)),
        _ => Err(FieldVerifyError::InvalidConstantPoolIndexType(name_index)),
    }
}

fn verify_descriptor_index(class_file: &ClassFile, descriptor_index: u16) -> Result<()> {
    let idx = (descriptor_index as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(_)) => Ok(()),
        None => Err(FieldVerifyError::InvalidConstantPoolIndex(descriptor_index)),
        _ => Err(FieldVerifyError::InvalidConstantPoolIndexType(descriptor_index)),
    }
}

fn verify_access_flags(class_file: &ClassFile, access_flags: u16) -> Result<()> {
    let public_set = access_flags & access_flags::ACC_PUBLIC != 0;
    let protected_set = access_flags & access_flags::ACC_PROTECTED != 0;
    let private_set = access_flags & access_flags::ACC_PRIVATE != 0;

    if (public_set as u8 + protected_set as u8 + private_set as u8) > 1 {
        return Err(FieldVerifyError::InvalidFieldAccessFlags(access_flags));
    }

    let _class_is_interface = class_file.access_flags & access_flags::ACC_INTERFACE != 0;
    if _class_is_interface {
        let must = access_flags::ACC_PUBLIC | access_flags::ACC_STATIC | access_flags::ACC_FINAL;
        let has_all = access_flags & must == must;
        let illegal = access_flags::ACC_PRIVATE
            | access_flags::ACC_PROTECTED
            | access_flags::ACC_VOLATILE
            | access_flags::ACC_TRANSIENT
            | access_flags::ACC_ENUM
            | access_flags::ACC_SUPER;
        if !has_all || (access_flags & illegal != 0) {
            return Err(FieldVerifyError::InvalidFieldAccessFlags(access_flags));
        }
    } else {
        // class case: final and volatile cannot be both set
        let final_set = access_flags & access_flags::ACC_FINAL != 0;
        let volatile_set = access_flags & access_flags::ACC_VOLATILE != 0;
        if final_set && volatile_set {
            return Err(FieldVerifyError::InvalidFieldAccessFlags(access_flags));
        }
    }
    Ok(())
}

fn verify_attributes(class_file: &ClassFile, field_access: u16, attrs: &[NamedAttribute]) -> Result<()> {
    let mut has_constant_value = false;
    let mut has_signature = false;
    let _class_is_interface = class_file.access_flags & access_flags::ACC_INTERFACE != 0;

    for a in attrs {
        match &a.info {
            AttributeInfo::ConstantValue(cv) => {
                // At most one
                if has_constant_value {
                    return Err(FieldVerifyError::DuplicateFieldAttribute("ConstantValue".to_string()));
                }
                has_constant_value = true;
                // Must be static (JVMS); often also final
                let is_static = field_access & access_flags::ACC_STATIC != 0;
                if !is_static { return Err(FieldVerifyError::ConstantValueOnNonStatic); }
                // Check constant pool kind
                let idx = (cv.value.as_u16() as usize).saturating_sub(1);
                match class_file.constant_pool.constants.get(idx) {
                    Some(Constant::Integer(_))
                    | Some(Constant::Float(_))
                    | Some(Constant::Long(_))
                    | Some(Constant::Double(_))
                    | Some(Constant::String(_)) => {}
                    _ => return Err(FieldVerifyError::InvalidConstantValueKind),
                }
            }
            AttributeInfo::Signature(sig_attr) => {
                if has_signature {
                    return Err(FieldVerifyError::DuplicateFieldAttribute("Signature".to_string()));
                }
                has_signature = true;
                // Minimal signature content validation
                let idx = sig_attr.signature.as_u16();
                let idx_usize = (idx as usize).saturating_sub(1);
                if let Some(Constant::Utf8(s)) = class_file.constant_pool.constants.get(idx_usize) {
                    if !crate::verify::signature::is_valid_field_signature(s) || !crate::verify::attributes::is_valid_signature(s) {
                        return Err(FieldVerifyError::InvalidFieldAttribute("Invalid Signature".to_string()));
                    }
                } else { return Err(FieldVerifyError::InvalidFieldAttribute("Invalid Signature".to_string())); }
            }
            AttributeInfo::Synthetic(_) | AttributeInfo::Deprecated(_) => {}
            AttributeInfo::RuntimeVisibleAnnotations(_)
            | AttributeInfo::RuntimeInvisibleAnnotations(_)
            | AttributeInfo::RuntimeVisibleParameterAnnotations(_)
            | AttributeInfo::RuntimeInvisibleParameterAnnotations(_)
            | AttributeInfo::RuntimeVisibleTypeAnnotations(_)
            | AttributeInfo::RuntimeInvisibleTypeAnnotations(_) => {}
            // Disallow method/class/code-only attributes on fields
            AttributeInfo::Code(_)
            | AttributeInfo::StackMapTable(_)
            | AttributeInfo::Exceptions(_)
            | AttributeInfo::InnerClasses(_)
            | AttributeInfo::EnclosingMethod(_)
            | AttributeInfo::SourceFile(_)
            | AttributeInfo::SourceDebugExtension(_)
            | AttributeInfo::LineNumberTable(_)
            | AttributeInfo::LocalVariableTable(_)
            | AttributeInfo::LocalVariableTypeTable(_)
            | AttributeInfo::AnnotationDefault(_)
            | AttributeInfo::BootstrapMethods(_)
            | AttributeInfo::MethodParameters(_)
            | AttributeInfo::Module(_)
            | AttributeInfo::ModulePackages(_)
            | AttributeInfo::ModuleMainClass(_)
            | AttributeInfo::NestHost(_)
            | AttributeInfo::NestMembers(_) => {
                return Err(FieldVerifyError::InvalidFieldAttribute(attribute_name(class_file, a)));
            }
            AttributeInfo::Custom(_) | AttributeInfo::Record(_) | AttributeInfo::PermittedSubclasses(_) => {
                // Custom attributes are not recognized for fields here
                return Err(FieldVerifyError::InvalidFieldAttribute(attribute_name(class_file, a)));
            }
        }
    }

    Ok(())
}

fn attribute_name(class_file: &ClassFile, a: &NamedAttribute) -> String {
    let idx = (a.name.as_u16() as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(s)) => s.clone(),
        _ => String::from("<invalid-name>"),
    }
}


