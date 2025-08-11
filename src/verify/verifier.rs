use crate::ast::*;
// AST-level review moved to crate::review; this module is ClassFile-level
use crate::codegen::class::ClassFile;
use crate::codegen::constpool::Constant;
use crate::codegen::flag::access_flags;
use super::{constant_pool, class_access_flags, fields, methods, interfaces, attributes};

pub type VerifyResult<T> = Result<T, VerifyError>;

#[derive(thiserror::Error, Debug)]
pub enum VerifyError {
    #[error("Missing package name")]
    MissingPackage,
    #[error("Duplicate import: {0}")]
    DuplicateImport(String),
    #[error("Duplicate type declaration: {0}")]
    DuplicateType(String),
    #[error("Empty class name")]
    EmptyClassName,
    #[error("Class '{0}' cannot be both abstract and final")]
    ClassAbstractAndFinal(String),
    #[error("Interface '{0}' cannot be final")]
    InterfaceFinal(String),
    #[error("Internal verifier error: {0}")]
    Internal(String),
}


/// Verify the ClassFile by orchestrating all sub-verifiers
pub fn verify(class_file: &ClassFile) -> VerifyResult<()> {
    // Java 8-only target: reject class files beyond major 52
    if class_file.major_version > 52 {
        return Err(VerifyError::Internal(format!(
            "Unsupported class file version {} (target is Java 8 / 52)",
            class_file.major_version
        )));
    }
    constant_pool::verify(class_file).map_err(|e| VerifyError::Internal(e.to_string()))?;
    class_access_flags::verify(class_file, None).map_err(|e| VerifyError::Internal(e.to_string()))?;
    verify_this_class(class_file)?;
    verify_super_class(class_file)?;
    interfaces::verify(class_file).map_err(|e| VerifyError::Internal(e.to_string()))?;
    fields::verify(class_file).map_err(|e| VerifyError::Internal(e.to_string()))?;
    methods::verify(class_file).map_err(|e| VerifyError::Internal(e.to_string()))?;
    attributes::verify(class_file).map_err(|e| VerifyError::Internal(e.to_string()))?;
    Ok(())
}

fn verify_this_class(class_file: &ClassFile) -> VerifyResult<()> {
    let this_class = class_file.this_class;
    let idx = (this_class as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Class(_)) => Ok(()),
        None => Err(VerifyError::Internal(format!("InvalidConstantPoolIndex({})", this_class))),
        _ => Err(VerifyError::Internal(format!("InvalidConstantPoolIndexType({})", this_class))),
    }
}

fn verify_super_class(class_file: &ClassFile) -> VerifyResult<()> {
    let super_class = class_file.super_class;

    let class_is_interface = class_file.access_flags & access_flags::ACC_INTERFACE != 0;
    if !class_is_interface && super_class == 0 {
        return Ok(());
    }

    let idx = (super_class as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Class(_)) => Ok(()),
        None => Err(VerifyError::Internal(format!("InvalidConstantPoolIndex({})", super_class))),
        _ => Err(VerifyError::Internal(format!("InvalidConstantPoolIndexType({})", super_class))),
    }
}

fn verify_types(ast: &Ast) -> VerifyResult<()> {
    use std::collections::HashSet;
    let mut seen: HashSet<String> = HashSet::new();
    for td in &ast.type_decls {
        match td {
            TypeDecl::Class(c) => {
                if c.name.trim().is_empty() { return Err(VerifyError::EmptyClassName); }
                if !seen.insert(c.name.clone()) { return Err(VerifyError::DuplicateType(c.name.clone())); }
                verify_class(c)?;
            }
            TypeDecl::Interface(i) => {
                if i.name.trim().is_empty() { return Err(VerifyError::EmptyClassName); }
                if !seen.insert(i.name.clone()) { return Err(VerifyError::DuplicateType(i.name.clone())); }
                verify_interface(i)?;
            }
            TypeDecl::Enum(e) => {
                if e.name.trim().is_empty() { return Err(VerifyError::EmptyClassName); }
                if !seen.insert(e.name.clone()) { return Err(VerifyError::DuplicateType(e.name.clone())); }
                verify_enum(e)?;
            }
            TypeDecl::Annotation(a) => {
                if a.name.trim().is_empty() { return Err(VerifyError::EmptyClassName); }
                if !seen.insert(a.name.clone()) { return Err(VerifyError::DuplicateType(a.name.clone())); }
            }
        }
    }
    Ok(())
}

fn verify_class(c: &ClassDecl) -> VerifyResult<()> {
    use Modifier::*;
    let is_abstract = c.modifiers.contains(&Abstract);
    let is_final = c.modifiers.contains(&Final);
    if is_abstract && is_final {
        return Err(VerifyError::ClassAbstractAndFinal(c.name.clone()));
    }
    // If we have a synthesized ClassFile handy we could validate flag combos against the same rules
    // Here we mirror ristretto checks already via modifiers; deeper integration can be added later.
    Ok(())
}

fn verify_interface(i: &InterfaceDecl) -> VerifyResult<()> {
    use Modifier::*;
    if i.modifiers.contains(&Final) {
        return Err(VerifyError::InterfaceFinal(i.name.clone()));
    }
    Ok(())
}

fn verify_enum(_e: &EnumDecl) -> VerifyResult<()> { Ok(()) }


