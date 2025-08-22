//! Common utilities and definitions shared across modules
//! 
//! This module contains shared types, configurations, error definitions,
//! constants, and classpath resolution used throughout the tolc compiler.

pub mod classpath;
pub mod config;
pub mod consts;
pub mod error;
pub mod rt;

// Re-export commonly used items for convenience
pub use config::Config;
pub use error::{Error, Result};
pub use consts::*;