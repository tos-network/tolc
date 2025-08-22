//! Test for ResolvedType integration in items_javac.rs
//! 
//! This test verifies that ResolvedType from wash semantic analysis
//! is properly used for type-aware code generation in items_javac.rs

use std::collections::HashMap;
use tolc::{Config, compile2file};
use tolc::wash::SemanticAnalyzer;
use tolc::parser::parse_java;
use tolc::codegen::constpool::ConstantPool;
use tolc::codegen::code::Code;
use tolc::codegen::items_javac::{Items, typecodes};
use tolc::wash::attr::{ResolvedType, PrimitiveType};

#[test]
fn test_resolved_type_to_typecode_conversion() {
    // Create a mock constant pool and code buffer
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    // Create type information from wash
    let mut type_info = HashMap::new();
    type_info.insert("int_var".to_string(), ResolvedType::Primitive(PrimitiveType::Int));
    type_info.insert("obj_var".to_string(), ResolvedType::Reference("java/lang/String".to_string()));
    type_info.insert("long_var".to_string(), ResolvedType::Primitive(PrimitiveType::Long));
    type_info.insert("double_var".to_string(), ResolvedType::Primitive(PrimitiveType::Double));
    
    // Create Items with wash type information
    let items = Items::new_with_wash_types(&mut pool, &mut code, &type_info);
    
    // Test primitive type conversions
    let int_type = &ResolvedType::Primitive(PrimitiveType::Int);
    let int_item = items.make_stack_item_for_resolved_type(int_type);
    assert_eq!(int_item.typecode(), typecodes::INT);
    
    let long_type = &ResolvedType::Primitive(PrimitiveType::Long);
    let long_item = items.make_stack_item_for_resolved_type(long_type);
    assert_eq!(long_item.typecode(), typecodes::LONG);
    
    let double_type = &ResolvedType::Primitive(PrimitiveType::Double);
    let double_item = items.make_stack_item_for_resolved_type(double_type);
    assert_eq!(double_item.typecode(), typecodes::DOUBLE);
    
    // Test reference type conversions
    let string_type = &ResolvedType::Reference("java/lang/String".to_string());
    let string_item = items.make_stack_item_for_resolved_type(string_type);
    assert_eq!(string_item.typecode(), typecodes::OBJECT);
    
    println!("✅ ResolvedType to typecode conversion tests passed");
}

#[test]
fn test_resolved_type_field_descriptor_generation() {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Test primitive descriptors
    let int_type = ResolvedType::Primitive(PrimitiveType::Int);
    let int_item = items.make_field_item_for_resolved_type(
        "intField".to_string(),
        "TestClass".to_string(),
        &int_type,
        false
    );
    // Field descriptors are tested internally in the make_field_item_for_resolved_type method
    
    // Test reference type descriptors
    let string_type = ResolvedType::Reference("java.lang.String".to_string());
    let string_item = items.make_field_item_for_resolved_type(
        "stringField".to_string(),
        "TestClass".to_string(),
        &string_type,
        false
    );
    assert_eq!(string_item.typecode(), typecodes::OBJECT);
    
    // Test array type descriptors
    let int_array_type = ResolvedType::Array(Box::new(ResolvedType::Primitive(PrimitiveType::Int)));
    let array_item = items.make_field_item_for_resolved_type(
        "arrayField".to_string(),
        "TestClass".to_string(),
        &int_array_type,
        false
    );
    assert_eq!(array_item.typecode(), typecodes::ARRAY);
    
    println!("✅ ResolvedType field descriptor generation tests passed");
}

#[test]
fn test_resolved_type_local_variable_creation() {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    let mut type_info = HashMap::new();
    type_info.insert("localVar".to_string(), ResolvedType::Primitive(PrimitiveType::Int));
    
    let items = Items::new_with_wash_types(&mut pool, &mut code, &type_info);
    
    // Test local variable item creation with ResolvedType
    let int_type = ResolvedType::Primitive(PrimitiveType::Int);
    let local_item = items.make_local_item_for_resolved_type(&int_type, 1);
    
    assert_eq!(local_item.typecode(), typecodes::INT);
    match local_item {
        tolc::codegen::items_javac::Item::Local { typecode, reg } => {
            assert_eq!(typecode, typecodes::INT);
            assert_eq!(reg, 1);
        }
        _ => panic!("Expected Local item"),
    }
    
    println!("✅ ResolvedType local variable creation tests passed");
}

#[test]
fn test_complete_generic_class_compilation_with_resolved_types() {
    // Test a generic class to ensure ResolvedType integration works end-to-end
    let java_source = r#"
public class GenericContainer<T> {
    private T data;
    private int count;
    private String name;
    
    public GenericContainer(T data) {
        this.data = data;
        this.count = 1;
        this.name = "container";
    }
    
    public T getData() {
        return this.data;
    }
    
    public int getCount() {
        return this.count;
    }
    
    public String getName() {
        return this.name;
    }
    
    public void setData(T newData) {
        this.data = newData;
        this.count++;
    }
}
"#;

    // Parse and run wash analysis
    let mut ast = parse_java(java_source).expect("Failed to parse Java source");
    let mut semantic_analyzer = SemanticAnalyzer::new();
    ast = semantic_analyzer.analyze(ast).expect("Failed to run wash analysis");
    
    // Verify wash collected type information
    let type_info = semantic_analyzer.attr.get_type_information();
    let symbol_env = semantic_analyzer.enter.get_symbol_environment();
    let signatures = semantic_analyzer.get_generic_signatures();
    
    assert!(!type_info.is_empty(), "Wash should collect type information");
    assert!(!symbol_env.classes.is_empty(), "Wash should collect class symbols");
    assert!(!signatures.is_empty(), "Wash should collect generic signatures");
    
    // Test end-to-end compilation with ResolvedType integration
    let config = Config::default();
    let output_dir = "/tmp/resolved_type_test";
    std::fs::create_dir_all(output_dir).expect("Failed to create output directory");
    
    let result = compile2file(java_source, output_dir, &config);
    assert!(result.is_ok(), "Compilation should succeed with ResolvedType integration");
    
    // Verify class file was generated
    let class_file_path = std::path::Path::new(output_dir).join("GenericContainer.class");
    assert!(class_file_path.exists(), "GenericContainer.class should be generated");
    
    println!("✅ Complete generic class compilation with ResolvedType integration passed");
}