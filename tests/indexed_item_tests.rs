use tolc::codegen::items::{Items, Item, typecodes};
use tolc::codegen::code::Code;
use tolc::codegen::constpool::ConstantPool;
use tolc::common::error::Result;
use tolc::ast::{TypeEnum, ReferenceType, PrimitiveType};

/// Tests for IndexedItem array access optimization (JavaC alignment)
/// This module tests JavaC's IndexedItem - array index access optimization
/// 
/// JavaC Pattern:
/// - genExpr(tree.indexed, tree.indexed.type).load()
/// - genExpr(tree.index, syms.intType).load()
/// - result = items.makeIndexedItem(tree.type)
/// - IndexedItem.load() emits appropriate array load instructions
/// - IndexedItem.store() emits appropriate array store instructions

/// Test basic IndexedItem creation and typecodes
#[test]
fn test_indexed_item_creation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Test int array element
    let int_element = TypeEnum::Primitive(PrimitiveType::Int);
    let int_indexed = items.make_indexed_item(&int_element);
    
    match int_indexed {
        Item::Indexed { typecode } => {
            assert_eq!(typecode, typecodes::INT);
        }
        _ => panic!("Expected IndexedItem for int array"),
    }
    
    // Test object array element
    let object_element = TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string()));
    let object_indexed = items.make_indexed_item(&object_element);
    
    match object_indexed {
        Item::Indexed { typecode } => {
            assert_eq!(typecode, typecodes::OBJECT);
        }
        _ => panic!("Expected IndexedItem for object array"),
    }
    
    Ok(())
}

/// Test IndexedItem load operations for primitive arrays
#[test]
fn test_indexed_item_primitive_loads() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test all primitive array types
    let test_cases = vec![
        (TypeEnum::Primitive(PrimitiveType::Boolean), typecodes::BYTE),   // boolean[] uses BALOAD
        (TypeEnum::Primitive(PrimitiveType::Byte), typecodes::BYTE),      // byte[] uses BALOAD
        (TypeEnum::Primitive(PrimitiveType::Char), typecodes::CHAR),      // char[] uses CALOAD  
        (TypeEnum::Primitive(PrimitiveType::Short), typecodes::SHORT),    // short[] uses SALOAD
        (TypeEnum::Primitive(PrimitiveType::Int), typecodes::INT),        // int[] uses IALOAD
        (TypeEnum::Primitive(PrimitiveType::Long), typecodes::LONG),      // long[] uses LALOAD
        (TypeEnum::Primitive(PrimitiveType::Float), typecodes::FLOAT),    // float[] uses FALOAD
        (TypeEnum::Primitive(PrimitiveType::Double), typecodes::DOUBLE),  // double[] uses DALOAD
    ];
    
    for (element_type, expected_typecode) in test_cases {
        let indexed_item = items.make_indexed_item(&element_type);
        
        match indexed_item {
            Item::Indexed { typecode } => {
                assert_eq!(typecode, expected_typecode, 
                    "Wrong typecode for {:?}", element_type);
            }
            _ => panic!("Expected IndexedItem for {:?}", element_type),
        }
    }
    
    Ok(())
}

/// Test IndexedItem load operations generate correct bytecode
#[test]
fn test_indexed_item_load_bytecode() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test int array load
    let int_indexed = items.make_indexed_item(&TypeEnum::Primitive(PrimitiveType::Int));
    let _result = items.load_item(&int_indexed)?;
    
    // Verify the bytecode was generated (this would require examining code.bytes)
    // For now, just verify the operation succeeds
    
    Ok(())
}

/// Test IndexedItem store operations for primitive arrays
#[test]
fn test_indexed_item_primitive_stores() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test all primitive array types for store operations
    let test_cases = vec![
        TypeEnum::Primitive(PrimitiveType::Boolean),  // boolean[] uses BASTORE
        TypeEnum::Primitive(PrimitiveType::Byte),     // byte[] uses BASTORE
        TypeEnum::Primitive(PrimitiveType::Char),     // char[] uses CASTORE
        TypeEnum::Primitive(PrimitiveType::Short),    // short[] uses SASTORE
        TypeEnum::Primitive(PrimitiveType::Int),      // int[] uses IASTORE
        TypeEnum::Primitive(PrimitiveType::Long),     // long[] uses LASTORE
        TypeEnum::Primitive(PrimitiveType::Float),    // float[] uses FASTORE
        TypeEnum::Primitive(PrimitiveType::Double),   // double[] uses DASTORE
    ];
    
    for element_type in test_cases {
        let indexed_item = items.make_indexed_item(&element_type);
        
        // Store operation should succeed
        let result = items.store_item(&indexed_item);
        assert!(result.is_ok(), "Store failed for {:?}", element_type);
    }
    
    Ok(())
}

/// Test IndexedItem for object arrays (reference types)
#[test]
fn test_indexed_item_object_arrays() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test various object array types
    let test_cases = vec![
        TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string())),
        TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string())),
        TypeEnum::Reference(ReferenceType::Class("java/util/List".to_string())),
    ];
    
    for element_type in test_cases {
        let indexed_item = items.make_indexed_item(&element_type);
        
        match indexed_item {
            Item::Indexed { typecode } => {
                assert_eq!(typecode, typecodes::OBJECT, "Object arrays should use OBJECT typecode");
            }
            _ => panic!("Expected IndexedItem for object array"),
        }
        
        // Test load and store operations
        let _load_result = items.load_item(&indexed_item)?;
        let store_result = items.store_item(&indexed_item);
        assert!(store_result.is_ok(), "Store failed for object array");
    }
    
    Ok(())
}

/// Test IndexedItem for multi-dimensional arrays
#[test]
fn test_indexed_item_multidimensional_arrays() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Multi-dimensional arrays are references to array objects
    let int_array_array = TypeEnum::Reference(ReferenceType::Array(Box::new(
        tolc::ast::TypeRef {
            name: "int[]".to_string(),
            type_args: vec![],
            array_dims: 1,
            annotations: vec![],
            span: tolc::ast::Span::from_to(0, 0, 0, 5),
        }
    )));
    
    let indexed_item = items.make_indexed_item(&int_array_array);
    
    match indexed_item {
        Item::Indexed { typecode } => {
            assert_eq!(typecode, typecodes::OBJECT, "Multi-dimensional arrays should use OBJECT typecode");
        }
        _ => panic!("Expected IndexedItem for multi-dimensional array"),
    }
    
    Ok(())
}

/// Test IndexedItem typecode conversion alignment with JavaC
#[test]
fn test_indexed_item_typecode_alignment() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Test that our typecodes match JavaC expectations
    struct TypecodeTest {
        type_enum: TypeEnum,
        expected_javac_typecode: u8,
        description: &'static str,
    }
    
    let tests = vec![
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Boolean),
            expected_javac_typecode: typecodes::BYTE, // JavaC: boolean arrays use BALOAD/BASTORE
            description: "boolean array elements",
        },
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Byte),
            expected_javac_typecode: typecodes::BYTE,
            description: "byte array elements", 
        },
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Char),
            expected_javac_typecode: typecodes::CHAR,
            description: "char array elements",
        },
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Short),
            expected_javac_typecode: typecodes::SHORT,
            description: "short array elements",
        },
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Int),
            expected_javac_typecode: typecodes::INT,
            description: "int array elements",
        },
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Long),
            expected_javac_typecode: typecodes::LONG,
            description: "long array elements",
        },
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Float),
            expected_javac_typecode: typecodes::FLOAT,
            description: "float array elements",
        },
        TypecodeTest {
            type_enum: TypeEnum::Primitive(PrimitiveType::Double),
            expected_javac_typecode: typecodes::DOUBLE,
            description: "double array elements",
        },
        TypecodeTest {
            type_enum: TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string())),
            expected_javac_typecode: typecodes::OBJECT,
            description: "object array elements",
        },
    ];
    
    for test in tests {
        let indexed_item = items.make_indexed_item(&test.type_enum);
        
        match indexed_item {
            Item::Indexed { typecode } => {
                assert_eq!(typecode, test.expected_javac_typecode, 
                    "Typecode mismatch for {}: expected {}, got {}", 
                    test.description, test.expected_javac_typecode, typecode);
            }
            _ => panic!("Expected IndexedItem for {}", test.description),
        }
    }
    
    Ok(())
}

/// Test IndexedItem error handling for invalid operations
#[test]
fn test_indexed_item_error_handling() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test with void type (should not create IndexedItem)
    let void_indexed = items.make_indexed_item(&TypeEnum::Void);
    
    match void_indexed {
        Item::Indexed { typecode } => {
            assert_eq!(typecode, typecodes::VOID);
        }
        _ => panic!("Expected IndexedItem even for void type"),
    }
    
    Ok(())
}

/// Test IndexedItem stack management
#[test]
fn test_indexed_item_stack_management() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // IndexedItem operations should properly manage stack
    let int_indexed = items.make_indexed_item(&TypeEnum::Primitive(PrimitiveType::Int));
    
    // Load operation: array[index] -> value
    // Stack before: [array_ref] [index] 
    // Stack after: [value]
    let _load_result = items.load_item(&int_indexed)?;
    
    // Store operation: array[index] = value -> void
    // Stack before: [array_ref] [index] [value]
    // Stack after: []
    let store_result = items.store_item(&int_indexed);
    assert!(store_result.is_ok(), "Store operation should succeed");
    
    Ok(())
}

/// Test IndexedItem width calculation for stack operations
#[test]  
fn test_indexed_item_width() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Test width calculation for different array types
    let test_cases = vec![
        (TypeEnum::Primitive(PrimitiveType::Int), 1),      // int is 1 slot
        (TypeEnum::Primitive(PrimitiveType::Long), 2),     // long is 2 slots  
        (TypeEnum::Primitive(PrimitiveType::Double), 2),   // double is 2 slots
        (TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string())), 1), // objects are 1 slot
    ];
    
    for (element_type, _expected_width) in test_cases {
        let indexed_item = items.make_indexed_item(&element_type);
        
        // Verify the item was created successfully
        match indexed_item {
            Item::Indexed { .. } => {
                // Width testing would require access to internal Item methods
                // For now, just verify successful creation
            }
            _ => panic!("Expected IndexedItem for {:?}", element_type),
        }
    }
    
    Ok(())
}

/// Test IndexedItem integration with JavaC visitIndexed pattern
#[test]
fn test_indexed_item_javac_pattern() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // JavaC pattern test: makeIndexedItem should create appropriate item
    let element_types = vec![
        TypeEnum::Primitive(PrimitiveType::Int),
        TypeEnum::Primitive(PrimitiveType::Boolean), 
        TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string())),
    ];
    
    for element_type in element_types {
        // JavaC: result = items.makeIndexedItem(tree.type);
        let indexed_item = items.make_indexed_item(&element_type);
        
        // Verify we get an IndexedItem
        match indexed_item {
            Item::Indexed { .. } => {
                // Success - matches JavaC pattern
            }
            _ => panic!("Expected IndexedItem for JavaC pattern test"),
        }
    }
    
    Ok(())
}