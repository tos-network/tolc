use crate::ast::*;
mod types;
mod class;
mod interface;
mod annotation;
mod enums;

pub type ReviewResult<T> = Result<T, ReviewError>;

#[derive(thiserror::Error, Debug)]
pub enum ReviewError {
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
}

/// AST-level review before codegen
pub fn review(ast: &Ast) -> ReviewResult<()> {
    review_package(ast)?;
    review_imports(ast)?;
    types::review_types(ast)?;
    Ok(())
}

fn review_package(ast: &Ast) -> ReviewResult<()> {
    if let Some(pkg) = &ast.package_decl {
        if pkg.name.trim().is_empty() {
            return Err(ReviewError::MissingPackage);
        }
    }
    Ok(())
}

fn review_imports(ast: &Ast) -> ReviewResult<()> {
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

// Per-kind checks are implemented in dedicated modules for clarity

