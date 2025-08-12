// reserved for future import-specific validation rules
use super::{ReviewResult, ReviewError};
use crate::ast::Ast;

pub(crate) fn review_imports(ast: &Ast) -> ReviewResult<()> {
    use std::collections::HashSet;
    #[derive(Hash, Eq, PartialEq)]
    struct ImportKey<'a> { name: &'a str, is_static: bool, is_wildcard: bool }
    let mut seen: HashSet<ImportKey> = HashSet::new();
    for imp in &ast.imports {
        let key = ImportKey { name: &imp.name, is_static: imp.is_static, is_wildcard: imp.is_wildcard };
        if !seen.insert(key) {
            log::debug!("duplicate import detected: {} (static={}, wildcard={})", imp.name, imp.is_static, imp.is_wildcard);
            return Err(ReviewError::DuplicateImport(imp.name.clone()));
        }
    }
    Ok(())
}