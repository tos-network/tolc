use super::{ReviewError, ReviewResult};
use crate::ast::AnnotationDecl;

pub(crate) fn review_annotation(_a: &AnnotationDecl) -> ReviewResult<()> {
    // Placeholder for future annotation-specific checks
    Ok(())
}


