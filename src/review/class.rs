use super::{ReviewError, ReviewResult};
use crate::ast::ClassDecl;

pub(crate) fn review_class(c: &ClassDecl) -> ReviewResult<()> {
    use crate::ast::Modifier::*;
    let is_abstract = c.modifiers.contains(&Abstract);
    let is_final = c.modifiers.contains(&Final);
    if is_abstract && is_final {
        return Err(ReviewError::ClassAbstractAndFinal(c.name.clone()));
    }
    Ok(())
}


