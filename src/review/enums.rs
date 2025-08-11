use super::{ReviewError, ReviewResult};
use crate::ast::EnumDecl;

pub(crate) fn review_enum(_e: &EnumDecl) -> ReviewResult<()> {
    // Placeholder for future enum-specific checks
    Ok(())
}


