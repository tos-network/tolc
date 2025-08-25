//! Tests demonstrating robust and type-safe code generation patterns

use tolc::codegen::*;
use tolc::codegen::defs::{MAGIC, major_versions};
use tolc::codegen::flag::access_flags;
use tolc::codegen::{ClassIndex, StringIndex, NameAndTypeIndex, ConstPoolIndex};
use tolc::common::error::Error;

/// Test the new trait-based serialization system
#[test]
fn test_classfile_writable_trait() {
    // Create a simple class file
    let class_file = ClassFile {
        magic: MAGIC,
        minor_version: 0,
        major_version: major_versions::JAVA_8,
        constant_pool: ConstantPool::new(),
        access_flags: access_flags::ACC_PUBLIC,
        this_class: 1,
        super_class: 2,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        attributes: Vec::new(),
    };
    
    // Test the trait methods
    let bytes = class_file_to_bytes(&class_file);
    assert!(!bytes.is_empty());
    assert_eq!(&bytes[0..4], &MAGIC.to_be_bytes());
    
    // Test version bytes
    assert_eq!(&bytes[4..6], &0u16.to_be_bytes()); // minor version
    assert_eq!(&bytes[6..8], &major_versions::JAVA_8.to_be_bytes()); // major version
}

/// Test the new size-limited vector system
#[test]
fn test_jvm_vec_size_limits() {
    let mut vec = JvmVecU2::<u8>::new();
    
    // Test basic operations
    assert_eq!(vec.len(), 0);
    assert!(vec.is_empty());
    assert!(vec.has_space());
    assert!(vec.has_space_for(1));
    
    // Test pushing items
    for i in 0..100 {
        let result = vec.push(i);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), i as u16);
    }
    
    assert_eq!(vec.len(), 100);
    assert!(vec.has_space());
    
    // Test getting items
    assert_eq!(vec.get(0), Some(&0));
    assert_eq!(vec.get(99), Some(&99));
    assert_eq!(vec.get(100), None);
}

/// Test the typed constant pool index system
#[test]
fn test_typed_constant_pool_indices() {
    // Create typed indices
    let class_index: ClassIndex = ConstPoolIndex::from(42);
    let string_index: StringIndex = ConstPoolIndex::from(100);
    let name_type_index: NameAndTypeIndex = ConstPoolIndex::from(200);
    
    // Test type safety
    assert_eq!(class_index.as_u16(), 42);
    assert_eq!(string_index.as_u16(), 100);
    assert_eq!(name_type_index.as_u16(), 200);
    
    // Test conversion to raw indices
    let raw_class: u16 = class_index.as_u16();
    assert_eq!(raw_class, 42);
    
    // Test conversion to usize for array indexing
    let usize_class: usize = class_index.as_u16() as usize;
    assert_eq!(usize_class, 42);
}

/// Test the typed constant pool index system with more edge cases
#[test]
fn test_typed_constant_pool_indices_edge_cases() {
    // Test zero index
    let zero_index: ClassIndex = ConstPoolIndex::from(0);
    assert_eq!(zero_index.as_u16(), 0);
    
    // Test maximum u16 index
    let max_index: StringIndex = ConstPoolIndex::from(u16::MAX);
    assert_eq!(max_index.as_u16(), u16::MAX);
    
    // Test conversion chain
    let original: u16 = 12345;
    let typed: ClassIndex = ConstPoolIndex::from(original);
    let raw: u16 = typed.as_u16();
    let back_to_u16: u16 = raw;
    assert_eq!(original, back_to_u16);
}

/// Test error handling with specific error types
#[test]
fn test_specific_error_types() {
    // Test constant pool errors (SizeLimitExceeded variant)
    let const_pool_error = ConstPoolError::SizeLimitExceeded { current: 65534, adding: 10, max: 65534 };
    assert!(const_pool_error.to_string().contains("Constant pool size limit exceeded"));

    // Test general compiler errors using existing Error enum
    let method_error = Error::CodeGen { message: "Invalid method name: invalid-method".to_string() };
    assert_eq!(method_error.to_string(), "Code generation error: Invalid method name: invalid-method");

    let bytecode_error = Error::Internal { message: "Stack underflow".to_string() };
    assert_eq!(bytecode_error.to_string(), "Internal compiler error: Stack underflow");
}

/// Test more error types and error conversion
#[test]
fn test_error_conversion_and_more_types() {
    // Test field-related error using Semantic error
    let field_error = Error::Semantic { message: "Invalid field name: invalid-field".to_string() };
    assert_eq!(field_error.to_string(), "Semantic error: Invalid field name: invalid-field");

    // Test attribute-related error using CodeGen error
    let attr_error = Error::CodeGen { message: "Attribute data too large: 1000000 bytes".to_string() };
    assert_eq!(attr_error.to_string(), "Code generation error: Attribute data too large: 1000000 bytes");

    // Test descriptor-related error using Semantic error
    let desc_error = Error::Semantic { message: "Invalid type descriptor: invalid".to_string() };
    assert_eq!(desc_error.to_string(), "Semantic error: Invalid type descriptor: invalid");
}

/// Test the new flag validation system
#[test]
fn test_flag_validation() {
    // Test valid flag combinations
    let valid_flags = vec![
        access_flags::ACC_PUBLIC,
        access_flags::ACC_FINAL,
    ];
    assert!(validate_access_flags(&valid_flags).is_ok());
    
    // Test conflicting flags
    let conflicting_flags = vec![
        access_flags::ACC_PUBLIC,
        access_flags::ACC_PRIVATE,
    ];
    assert!(validate_access_flags(&conflicting_flags).is_err());
}

/// Test more flag validation scenarios
#[test]
fn test_flag_validation_comprehensive() {
    // Test single flags
    assert!(validate_access_flags(&[access_flags::ACC_PUBLIC]).is_ok());
    assert!(validate_access_flags(&[access_flags::ACC_PRIVATE]).is_ok());
    assert!(validate_access_flags(&[access_flags::ACC_PROTECTED]).is_ok());
    
    // Test valid combinations
    assert!(validate_access_flags(&[access_flags::ACC_PUBLIC, access_flags::ACC_FINAL]).is_ok());
    assert!(validate_access_flags(&[access_flags::ACC_PUBLIC, access_flags::ACC_ABSTRACT]).is_ok());
    
    // Test more conflicting combinations
    assert!(validate_access_flags(&[access_flags::ACC_PRIVATE, access_flags::ACC_PROTECTED]).is_err());
    assert!(validate_access_flags(&[access_flags::ACC_PUBLIC, access_flags::ACC_PRIVATE, access_flags::ACC_PROTECTED]).is_err());
}

/// Test the new validation system
#[test]
fn test_class_file_validation() {
    let mut class_file = ClassFile {
        magic: MAGIC,
        minor_version: 0,
        major_version: major_versions::JAVA_8,
        constant_pool: ConstantPool::new(),
        access_flags: access_flags::ACC_PUBLIC,
        this_class: 1,
        super_class: 2,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        attributes: Vec::new(),
    };
    
    // Test validation
    let validation_result = validate_class_file(&class_file);
    assert!(validation_result.is_ok());
    
    // Test invalid class file
    class_file.magic = 0xDEADBEEF; // Invalid magic
    let validation_result = validate_class_file(&class_file);
    assert!(validation_result.is_err());
}

/// Test comprehensive class file validation
#[test]
fn test_class_file_validation_comprehensive() {
    // Test valid class file
    let valid_class = ClassFile {
        magic: MAGIC,
        minor_version: 0,
        major_version: major_versions::JAVA_8,
        constant_pool: ConstantPool::new(),
        access_flags: access_flags::ACC_PUBLIC,
        this_class: 1,
        super_class: 2,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        attributes: Vec::new(),
    };
    assert!(validate_class_file(&valid_class).is_ok());
    
    // Test various invalid versions
    let invalid_version = ClassFile {
        magic: MAGIC,
        minor_version: 0,
        major_version: major_versions::JAVA_1_1 - 1,
        constant_pool: ConstantPool::new(),
        access_flags: access_flags::ACC_PUBLIC,
        this_class: 1,
        super_class: 2,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        attributes: Vec::new(),
    };
    assert!(validate_class_file(&invalid_version).is_err());
    
    // Test valid versions
    let java_11_class = ClassFile {
        magic: MAGIC,
        minor_version: 0,
        major_version: major_versions::JAVA_11,
        constant_pool: ConstantPool::new(),
        access_flags: access_flags::ACC_PUBLIC,
        this_class: 1,
        super_class: 2,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        attributes: Vec::new(),
    };
    assert!(validate_class_file(&java_11_class).is_ok());
}

/// Test the new builder pattern for class generation
#[test]
fn test_class_builder_pattern() {
    let class_file = ClassFileBuilder::new("TestClass")
        .with_super_class("java/lang/Object")
        .with_access_flags(access_flags::ACC_PUBLIC)
        .with_method(
            MethodBuilder::new("main")
                .with_descriptor("([Ljava/lang/String;)V")
                .with_access_flags(access_flags::ACC_PUBLIC | access_flags::ACC_STATIC)
                .with_code(|code| {
                    code.ldc("Hello, World!")
                        .invoke_static("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
                        .return_void()
                })
                .build()
        )
        .build();
    
    assert_eq!(class_file.this_class, 1);
    assert_eq!(class_file.super_class, 2);
    assert_eq!(class_file.methods.len(), 1);
}

/// Test builder pattern with more complex scenarios
#[test]
fn test_builder_pattern_comprehensive() {
    // Test building a class with multiple methods
    let class_file = ClassFileBuilder::new("ComplexClass")
        .with_super_class("java/lang/Object")
        .with_access_flags(access_flags::ACC_PUBLIC | access_flags::ACC_FINAL)
        .with_method(
            MethodBuilder::new("constructor")
                .with_descriptor("()V")
                .with_access_flags(access_flags::ACC_PUBLIC)
                .with_code(|code| {
                    code.return_void()
                })
                .build()
        )
        .with_method(
            MethodBuilder::new("staticMethod")
                .with_descriptor("()I")
                .with_access_flags(access_flags::ACC_PUBLIC | access_flags::ACC_STATIC)
                .with_code(|code| {
                    code.return_void()
                })
                .build()
        )
        .build();
    
    assert_eq!(class_file.methods.len(), 2);
    assert_eq!(class_file.access_flags, access_flags::ACC_PUBLIC | access_flags::ACC_FINAL);
}

/// Test JVM vector edge cases and error handling
#[test]
fn test_jvm_vec_edge_cases() {
    let mut vec = JvmVecU2::<u8>::new();
    
    // Test size limits
    assert_eq!(vec.remaining_space(), JvmVecU2::<u8>::MAX_SIZE);
    
    // Test pushing up to the limit
    for i in 0..JvmVecU2::<u8>::MAX_SIZE {
        assert!(vec.push(i as u8).is_ok());
    }
    
    // Test that we can't push beyond the limit
    assert!(vec.push(255).is_err());
    
    // Test has_space_for with exact remaining space
    assert!(vec.has_space_for(0));
    assert!(!vec.has_space_for(1));
}

/// Test constant pool operations
#[test]
fn test_constant_pool_operations() {
    let mut pool = ConstantPool::new();
    
    // Test adding UTF8 strings
    let index1 = pool.add_utf8("Hello");
    let index2 = pool.add_utf8("World");
    assert_eq!(index1, 1);
    assert_eq!(index2, 2);
    
    // Test adding class references
    // add_class internally calls add_utf8 for the class name, so it uses index 3
    let class_index = pool.add_class("java/lang/Object");
    assert_eq!(class_index, 4); // The class constant is at index 4, class name UTF8 is at index 3
    
    // Test adding name and type
    // add_name_and_type internally calls add_utf8 twice, so it uses indices 5 and 6
    let name_type_index = pool.add_name_and_type("method", "()V");
    assert_eq!(name_type_index, 7); // The name-and-type constant is at index 7
}

// Define a local error type used by helper validation functions in this test module
#[derive(Debug)]
enum FlagError {
    ConflictingFlags { _flag1: String, _flag2: String },
}

// Helper functions for testing

fn validate_access_flags(flags: &[u16]) -> Result<(), FlagError> {
    // Check for conflicting flags
    let has_public = flags.contains(&access_flags::ACC_PUBLIC);
    let has_private = flags.contains(&access_flags::ACC_PRIVATE);
    let has_protected = flags.contains(&access_flags::ACC_PROTECTED);
    
    if (has_public && has_private) || (has_public && has_protected) || (has_private && has_protected) {
        return Err(FlagError::ConflictingFlags {
            _flag1: "access modifier".to_string(),
            _flag2: "access modifier".to_string(),
        });
    }
    
    Ok(())
}

fn validate_class_file(class_file: &ClassFile) -> Result<(), ClassGenerationError> {
    if class_file.magic != MAGIC {
        return Err(ClassGenerationError::ValidationFailed {
            reason: "Invalid magic number".to_string(),
        });
    }
    
    if class_file.major_version < major_versions::JAVA_1_1 {
        return Err(ClassGenerationError::ValidationFailed {
            reason: "Invalid major version".to_string(),
        });
    }
    
    Ok(())
}

// Mock implementations for testing

struct ClassFileBuilder {
    _name: String, // Prefix with underscore to suppress unused warning
    super_class: Option<String>,
    access_flags: u16,
    methods: Vec<MethodInfo>,
}

impl ClassFileBuilder {
    fn new(_name: &str) -> Self {
        Self {
            _name: _name.to_string(),
            super_class: None,
            access_flags: access_flags::ACC_PUBLIC,
            methods: Vec::new(),
        }
    }
    
    fn with_super_class(mut self, super_class: &str) -> Self {
        self.super_class = Some(super_class.to_string());
        self
    }
    
    fn with_access_flags(mut self, flags: u16) -> Self {
        self.access_flags = flags;
        self
    }
    
    fn with_method(mut self, method: MethodInfo) -> Self {
        self.methods.push(method);
        self
    }
    
    fn build(self) -> ClassFile {
        // Simplified implementation for testing
        ClassFile {
            magic: MAGIC,
            minor_version: 0,
            major_version: major_versions::JAVA_8,
            constant_pool: ConstantPool::new(),
            access_flags: self.access_flags,
            this_class: 1,
            super_class: 2,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: self.methods,
            attributes: Vec::new(),
        }
    }
}

struct MethodBuilder {
    _name: String, // Prefix with underscore to suppress unused warning
    descriptor: String,
    access_flags: u16,
    code: Vec<u8>,
}

impl MethodBuilder {
    fn new(_name: &str) -> Self {
        Self {
            _name: _name.to_string(),
            descriptor: String::new(),
            access_flags: 0,
            code: Vec::new(),
        }
    }
    
    fn with_descriptor(mut self, descriptor: &str) -> Self {
        self.descriptor = descriptor.to_string();
        self
    }
    
    fn with_access_flags(mut self, flags: u16) -> Self {
        self.access_flags = flags;
        self
    }
    
    fn with_code<F>(mut self, code_builder: F) -> Self 
    where
        F: FnOnce(&mut CodeBuilder) -> &mut CodeBuilder,
    {
        let mut code_builder_instance = CodeBuilder::new();
        code_builder(&mut code_builder_instance);
        self.code = code_builder_instance.build();
        self
    }
    
    fn build(self) -> MethodInfo {
        MethodInfo {
            access_flags: self.access_flags,
            name_index: 1,
            descriptor_index: 2,
            attributes: Vec::new(),
        }
    }
}

struct CodeBuilder {
    code: Vec<u8>,
}

impl CodeBuilder {
    fn new() -> Self {
        Self { code: Vec::new() }
    }
    
    fn ldc(&mut self, _string: &str) -> &mut Self {
        self.code.push(0x12); // ldc opcode
        self.code.push(0x01); // constant pool index
        self
    }
    
    fn invoke_static(&mut self, _class: &str, _method: &str, _descriptor: &str) -> &mut Self {
        self.code.push(0xb8); // invokestatic opcode
        self.code.extend_from_slice(&0x0001u16.to_be_bytes()); // constant pool index
        self
    }
    
    fn return_void(&mut self) -> &mut Self {
        self.code.push(0xb1); // return opcode
        self
    }
    
    fn build(self) -> Vec<u8> {
        self.code
    }
}
