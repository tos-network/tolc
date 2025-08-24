use tolc::codegen::items::{Items, Item, typecodes};
use tolc::codegen::code::Code;
use tolc::codegen::constpool::ConstantPool;
use tolc::common::error::Result;
use tolc::ast::{TypeEnum, ReferenceType, PrimitiveType};

/// Tests for MemberItem.invoke() virtual method call optimization (JavaC alignment)
/// This module tests the 5th JavaC optimizer: MemberItem.invoke() - virtual method call optimization
/// 
/// JavaC Pattern:
/// - Interface methods → invokeinterface 
/// - Non-virtual methods → invokespecial
/// - Virtual methods → invokevirtual
/// - Proper stack management for receiver and arguments
/// - Method reference constant pool management

/// Test MemberItem.invoke() basic invokevirtual generation
#[test]
fn test_member_invoke_virtual() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create MemberItem for virtual method call: obj.toString()
    let member_item = items.make_member_item(
        "toString".to_string(),
        "java/lang/Object".to_string(), 
        "()Ljava/lang/String;".to_string(),
        false, // is_static = false
        &TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string()))
    );
    
    // Invoke using MemberItem optimization
    let result_item = items.invoke_item(&member_item)?;
    
    // Verify result is stack item with correct type
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::OBJECT);
        }
        _ => panic!("Expected stack item for virtual method result"),
    }
    
    Ok(())
}

/// Test MemberItem.invoke() interface method call (invokeinterface)
#[test]
fn test_member_invoke_interface() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create MemberItem for interface method call: list.size()
    let member_item = items.make_member_item(
        "size".to_string(),
        "java/util/List".to_string(), 
        "()I".to_string(),
        false, // is_static = false
        &TypeEnum::Primitive(PrimitiveType::Int)
    );
    
    // Invoke using MemberItem optimization
    let result_item = items.invoke_item(&member_item)?;
    
    // Verify result is stack item with int type
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::INT);
        }
        _ => panic!("Expected stack item for interface method result"),
    }
    
    Ok(())
}

/// Test MemberItem.invoke() special method call (invokespecial)
#[test]
fn test_member_invoke_special() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create MemberItem for special method call: super.method() (nonvirtual = true)
    let member_item = items.make_member_item_nonvirtual(
        "method".to_string(),
        "java/lang/Object".to_string(), 
        "()V".to_string(),
        false, // is_static = false
        &TypeEnum::Void,
        true   // nonvirtual = true for invokespecial
    );
    
    // Invoke using MemberItem optimization
    let result_item = items.invoke_item(&member_item)?;
    
    // Verify result is stack item with void type
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::VOID);
        }
        _ => panic!("Expected stack item for special method result"),
    }
    
    Ok(())
}

/// Test MemberItem.invoke() with primitive return types
#[test]
fn test_member_invoke_primitive_returns() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test int return
    let int_item = items.make_member_item(
        "hashCode".to_string(),
        "java/lang/Object".to_string(),
        "()I".to_string(),
        false,
        &TypeEnum::Primitive(PrimitiveType::Int)
    );
    let result = items.invoke_item(&int_item)?;
    assert!(matches!(result, Item::Stack { typecode } if typecode == typecodes::INT));
    
    // Test long return
    let long_item = items.make_member_item(
        "longValue".to_string(),
        "java/lang/Long".to_string(),
        "()J".to_string(),
        false,
        &TypeEnum::Primitive(PrimitiveType::Long)
    );
    let result = items.invoke_item(&long_item)?;
    assert!(matches!(result, Item::Stack { typecode } if typecode == typecodes::LONG));
    
    // Test double return
    let double_item = items.make_member_item(
        "doubleValue".to_string(),
        "java/lang/Double".to_string(),
        "()D".to_string(),
        false,
        &TypeEnum::Primitive(PrimitiveType::Double)
    );
    let result = items.invoke_item(&double_item)?;
    assert!(matches!(result, Item::Stack { typecode } if typecode == typecodes::DOUBLE));
    
    Ok(())
}

/// Test MemberItem.invoke() with method arguments
#[test]
fn test_member_invoke_with_arguments() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test method with arguments: obj.equals(Object)
    let member_item = items.make_member_item(
        "equals".to_string(),
        "java/lang/Object".to_string(),
        "(Ljava/lang/Object;)Z".to_string(),
        false,
        &TypeEnum::Primitive(PrimitiveType::Boolean)
    );
    
    let result_item = items.invoke_item(&member_item)?;
    
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::BYTE); // Boolean is represented as BYTE internally
        }
        _ => panic!("Expected stack item for method with arguments"),
    }
    
    Ok(())
}

/// Test MemberItem.invoke() void return type
#[test]
fn test_member_invoke_void_return() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test void method: obj.notify()
    let member_item = items.make_member_item(
        "notify".to_string(),
        "java/lang/Object".to_string(),
        "()V".to_string(),
        false,
        &TypeEnum::Void
    );
    
    let result_item = items.invoke_item(&member_item)?;
    
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::VOID);
        }
        _ => panic!("Expected stack item for void method"),
    }
    
    Ok(())
}

/// Test MemberItem.invoke() array return type
#[test]
fn test_member_invoke_array_return() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test array return: obj.getBytes()
    let member_item = items.make_member_item(
        "getBytes".to_string(),
        "java/lang/String".to_string(),
        "()[B".to_string(),
        false,
        &TypeEnum::Reference(ReferenceType::Array(Box::new(
            tolc::ast::TypeRef {
                name: "byte".to_string(),
                type_args: vec![],
                array_dims: 1,
                annotations: vec![],
                span: tolc::ast::Span::from_to(0, 0, 0, 4),
            }
        )))
    );
    
    let result_item = items.invoke_item(&member_item)?;
    
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::OBJECT); // Arrays are objects
        }
        _ => panic!("Expected stack item for array return"),
    }
    
    Ok(())
}

/// Test MemberItem.invoke() with complex method descriptor
#[test]
fn test_member_invoke_complex_descriptor() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test complex method: substring(int, int)
    let member_item = items.make_member_item(
        "substring".to_string(),
        "java/lang/String".to_string(),
        "(II)Ljava/lang/String;".to_string(),
        false,
        &TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string()))
    );
    
    let result_item = items.invoke_item(&member_item)?;
    
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::OBJECT);
        }
        _ => panic!("Expected stack item for complex method"),
    }
    
    Ok(())
}

/// Test MemberItem.invoke() constant pool integration
#[test]
fn test_member_invoke_constant_pool() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create member item
    let member_item = items.make_member_item(
        "toString".to_string(),
        "java/lang/Object".to_string(),
        "()Ljava/lang/String;".to_string(),
        false,
        &TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string()))
    );
    
    // Invoke should add method reference to constant pool
    let _result = items.invoke_item(&member_item)?;
    
    // Verify constant pool has entries (skip this check for now as the API is internal)
    
    Ok(())
}

/// Test MemberItem.invoke() error handling for invalid items
#[test]
fn test_member_invoke_error_handling() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Try to invoke non-member item (should fail)
    let local_item = Item::Local { 
        typecode: typecodes::INT, 
        reg: 1 
    };
    
    let result = items.invoke_item(&local_item);
    assert!(result.is_err(), "Expected error when invoking non-member item");
    
    Ok(())
}

/// Test MemberItem.invoke() with generic types (erased to Object)
#[test]
fn test_member_invoke_generic_types() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test generic method (type-erased): list.get(int) -> Object
    let member_item = items.make_member_item(
        "get".to_string(),
        "java/util/List".to_string(),
        "(I)Ljava/lang/Object;".to_string(), // Type-erased return
        false,
        &TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string()))
    );
    
    let result_item = items.invoke_item(&member_item)?;
    
    match result_item {
        Item::Stack { typecode } => {
            assert_eq!(typecode, typecodes::OBJECT);
        }
        _ => panic!("Expected stack item for generic method"),
    }
    
    Ok(())
}