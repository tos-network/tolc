use super::verifier::VerifyResult;
use super::verifier::VerifyError;
use crate::ast::Ast;

pub(crate) fn verify_package(ast: &Ast) -> VerifyResult<()> {
    // Allow no package, but if present it must have a non-empty name
    if let Some(pkg) = &ast.package_decl {
        if pkg.name.trim().is_empty() {
            return Err(VerifyError::MissingPackage);
        }
    }
    Ok(())
}
