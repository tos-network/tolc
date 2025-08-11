// reserved for future import-specific validation rules
use super::{ReviewResult, ReviewError};
use crate::ast::Ast;

pub(crate) fn review_imports(ast: &Ast) -> ReviewResult<()> {
    use std::collections::HashSet;
    let mut seen: HashSet<String> = HashSet::new();
    for imp in &ast.imports {
        let key = imp.name.clone();
        if !seen.insert(key.clone()) {
            return Err(ReviewError::DuplicateImport(key));
        }
    }
    Ok(())
}