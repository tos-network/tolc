use super::{ReviewError, ReviewResult};
use crate::ast::{EnumDecl, ClassMember, Modifier};

pub(crate) fn review_enum(_e: &EnumDecl) -> ReviewResult<()> {
    // Enum constructors cannot be public or protected (they are implicitly private)
    for member in &_e.body {
        if let ClassMember::Constructor(c) = member {
            let has_public = c.modifiers.contains(&Modifier::Public);
            let has_protected = c.modifiers.contains(&Modifier::Protected);
            if has_public || has_protected {
                return Err(ReviewError::IllegalEnumConstructorVisibility(_e.name.clone()));
            }
        }
    }
    Ok(())
}


