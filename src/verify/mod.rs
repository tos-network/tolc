//! AST verifiers for .tol source
//!
//! This module provides structural and semantic checks over the parsed AST
//! prior to code generation. It is inspired by ristretto's verifiers layout.

mod verifier;
mod package;
mod imports;
mod types;
mod methods;
pub mod class_access_flags;
pub mod constant_pool;
pub mod fields;
pub mod interfaces;
pub mod method_access_flags;


pub use verifier::{verify, VerifyError, VerifyResult};

