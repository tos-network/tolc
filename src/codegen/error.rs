//! Specific error types for code generation operations

use thiserror::Error;

/// Errors that can occur during constant pool operations
#[derive(Error, Debug)]
pub enum ConstPoolError {
    #[error("Constant pool is out of space")]
    OutOfSpace,
    #[error("This constant pool reference is already associated with an index")]
    AlreadyStored,
    #[error("Invalid constant pool index: {0}")]
    InvalidIndex(u16),
    #[error("Failed to store constant: {message}")]
    StoreError { message: String },
}

/// Errors that can occur during class file generation
#[derive(Error, Debug)]
pub enum ClassGenerationError {
    #[error("Constant pool error: {0}")]
    ConstPool(#[from] ConstPoolError),
    #[error("Method generation error: {0}")]
    MethodGeneration(#[from] MethodGenerationError),
    #[error("Field generation error: {0}")]
    FieldGeneration(#[from] FieldGenerationError),
    #[error("Attribute generation error: {0}")]
    AttributeGeneration(#[from] AttributeGenerationError),
    #[error("Invalid class name: {name}")]
    InvalidClassName { name: String },
    #[error("Invalid super class: {name}")]
    InvalidSuperClass { name: String },
    #[error("Class file validation failed: {reason}")]
    ValidationFailed { reason: String },
}

/// Errors that can occur during method generation
#[derive(Error, Debug)]
pub enum MethodGenerationError {
    #[error("Bytecode generation error: {0}")]
    Bytecode(#[from] BytecodeError),
    #[error("Invalid method name: {name}")]
    InvalidMethodName { name: String },
    #[error("Invalid method descriptor: {descriptor}")]
    InvalidMethodDescriptor { descriptor: String },
    #[error("Method validation failed: {reason}")]
    ValidationFailed { reason: String },
    #[error("Stack overflow in method: {method_name}")]
    StackOverflow { method_name: String },
    #[error("Local variable index out of bounds: {index}")]
    LocalVariableOutOfBounds { index: u16 },
}

/// Errors that can occur during field generation
#[derive(Error, Debug)]
pub enum FieldGenerationError {
    #[error("Invalid field name: {name}")]
    InvalidFieldName { name: String },
    #[error("Invalid field descriptor: {descriptor}")]
    InvalidFieldDescriptor { descriptor: String },
    #[error("Field validation failed: {reason}")]
    ValidationFailed { reason: String },
}

/// Errors that can occur during attribute generation
#[derive(Error, Debug)]
pub enum AttributeGenerationError {
    #[error("Invalid attribute name: {name}")]
    InvalidAttributeName { name: String },
    #[error("Attribute data too large: {size} bytes")]
    DataTooLarge { size: usize },
    #[error("Attribute validation failed: {reason}")]
    ValidationFailed { reason: String },
}

/// Errors that can occur during bytecode generation
#[derive(Error, Debug)]
pub enum BytecodeError {
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Stack overflow")]
    StackOverflow,
    #[error("Local variable index out of bounds: {index}")]
    LocalIndexOutOfBounds { index: u16 },
    #[error("Branch target too far: {offset}")]
    BranchTooFar { offset: i32 },
    #[error("Invalid opcode: {opcode}")]
    InvalidOpcode { opcode: u8 },
    #[error("Invalid constant pool index: {index}")]
    InvalidConstPoolIndex { index: u16 },
    #[error("Method invocation error: {message}")]
    MethodInvocationError { message: String },
}

/// Errors that can occur during descriptor generation
#[derive(Error, Debug)]
pub enum DescriptorError {
    #[error("Invalid type descriptor: {descriptor}")]
    InvalidTypeDescriptor { descriptor: String },
    #[error("Unsupported type: {type_name}")]
    UnsupportedType { type_name: String },
    #[error("Invalid method descriptor: {descriptor}")]
    InvalidMethodDescriptor { descriptor: String },
}

/// Errors that can occur during flag generation
#[derive(Error, Debug)]
pub enum FlagError {
    #[error("Invalid access flag combination: {flags:?}")]
    InvalidAccessFlagCombination { flags: Vec<String> },
    #[error("Conflicting flags: {flag1} and {flag2}")]
    ConflictingFlags { flag1: String, flag2: String },
}

/// Generic result type for code generation operations
pub type CodeGenResult<T> = Result<T, ClassGenerationError>;

/// Generic result type for constant pool operations
pub type ConstPoolResult<T> = Result<T, ConstPoolError>;

/// Generic result type for method generation operations
pub type MethodResult<T> = Result<T, MethodGenerationError>;

/// Generic result type for bytecode operations
pub type BytecodeResult<T> = Result<T, BytecodeError>;
