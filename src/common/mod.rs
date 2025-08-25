//! Common utilities and definitions shared across modules
//! 
//! This module contains shared types, configurations, error definitions,
//! constants, and classpath resolution used throughout the tolc compiler.

pub mod classloader;
pub mod class_manager;  // Renamed from manager
pub mod classpath;      // JavaC-aligned classpath resolution
pub mod compilation_context;  // New unified context
pub mod config;
pub mod consts;
pub mod env;
pub mod error;
pub mod import;
pub mod type_resolver;

// Re-export commonly used items for convenience
pub use config::Config;
pub use error::{Error, Result};
pub use consts::*;
pub use class_manager::{ClassManager, ClassInfo, MethodInfo, FieldInfo};
pub use compilation_context::{CompilationContext, CompilePhase};