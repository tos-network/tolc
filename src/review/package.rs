use super::{ReviewResult, ReviewError};
use crate::ast::Ast;

pub(crate) fn review_package(ast: &Ast) -> ReviewResult<()> {
    // Allow no package, but if present it must have a non-empty name
    if let Some(pkg) = &ast.package_decl {
        if pkg.name.trim().is_empty() {
            return Err(ReviewError::MissingPackage);
        }
    }
    Ok(())
}
