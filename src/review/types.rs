use super::{ReviewError, ReviewResult};
use crate::ast::*;

pub(crate) fn review_types(ast: &Ast) -> ReviewResult<()> {
    use std::collections::HashSet;
    let mut seen: HashSet<String> = HashSet::new();
    for td in &ast.type_decls {
        match td {
            TypeDecl::Class(c) => {
                if c.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(c.name.clone()) { return Err(ReviewError::DuplicateType(c.name.clone())); }
                super::class::review_class(c)?;
            }
            TypeDecl::Interface(i) => {
                if i.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(i.name.clone()) { return Err(ReviewError::DuplicateType(i.name.clone())); }
                super::interface::review_interface(i)?;
            }
            TypeDecl::Enum(e) => {
                if e.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(e.name.clone()) { return Err(ReviewError::DuplicateType(e.name.clone())); }
                super::enums::review_enum(e)?;
            }
            TypeDecl::Annotation(a) => {
                if a.name.trim().is_empty() { return Err(ReviewError::EmptyClassName); }
                if !seen.insert(a.name.clone()) { return Err(ReviewError::DuplicateType(a.name.clone())); }
                super::annotation::review_annotation(a)?;
            }
        }
    }
    Ok(())
}

