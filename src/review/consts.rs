// Re-export central exception classification lists from crate-level consts
// so review passes can refer to super::consts::* consistently.
pub use crate::consts::{
    UNCHECKED_BASE_EXCEPTIONS,
    UNCHECKED_COMMON_SUBCLASSES,
};


