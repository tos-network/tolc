// reserved for future import-specific validation rules
use super::verifier::VerifyResult;
use super::verifier::VerifyError;
use crate::ast::Ast;

pub(crate) fn verify_imports(ast: &Ast) -> VerifyResult<()> {
    use std::collections::HashSet;
    let mut seen: HashSet<String> = HashSet::new();
    for imp in &ast.imports {
        let key = imp.name.clone();
        if !seen.insert(key.clone()) {
            return Err(VerifyError::DuplicateImport(key));
        }
    }
    Ok(())
}