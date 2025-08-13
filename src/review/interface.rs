use super::{ReviewError, ReviewResult};
use crate::ast::InterfaceDecl;

pub(crate) fn review_interface(i: &InterfaceDecl) -> ReviewResult<()> {
	use crate::ast::Modifier::*;
	if i.modifiers.contains(&Final) {
		return Err(ReviewError::InterfaceFinal(i.name.clone()));
	}
	Ok(())
}


