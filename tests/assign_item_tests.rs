use tolc::codegen::items::{Item, Items};
use tolc::codegen::code::Code;
use tolc::codegen::constpool::ConstantPool;
use tolc::codegen::opcodes;
use tolc::common::error::Result;
use tolc::ast::{TypeEnum, PrimitiveType};

/// Tests for AssignItem assignment optimization (JavaC alignment)
/// This module tests JavaC's AssignItem - assignment expression optimization
/// 
/// JavaC Pattern:
/// - AssignItem represents assignment expressions with optimized bytecode generation
/// - AssignItem.load() executes assignment: lhs.stash(typecode); lhs.store(); return stackItem[typecode]
/// - AssignItem.duplicate() duplicates assignment result: load().duplicate()
/// - AssignItem.drop() drops assignment result: lhs.store()
/// - AssignItem.stash() should throw error (not allowed)
/// - AssignItem.width() returns lhs.width() + Code.width(typecode)

/// Test AssignItem creation and basic properties
#[test]
fn test_assign_item_creation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create a simple local variable item as lhs
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    
    // Create an AssignItem
    let assign_item = items.make_assign_item(lhs);
    
    // Verify it's an assignment item
    if let Item::Assign { lhs: _, typecode } = &assign_item {
        assert_eq!(*typecode, 4); // INT typecode
    } else {
        panic!("Expected AssignItem, got {:?}", assign_item);
    }
    
    Ok(())
}

/// Test AssignItem load operation (JavaC pattern: lhs.stash(); lhs.store(); return stackItem)
#[test]
fn test_assign_item_load_operation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create a local variable and assign item
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Load should execute the assignment and return stack item
    let result = assign_item.load(&mut items)?;
    
    // Verify result is a stack item
    if let Item::Stack { typecode, .. } = &result {
        assert_eq!(*typecode, 4); // INT typecode
    } else {
        panic!("Expected StackItem, got {:?}", result);
    }
    
    // Verify code was generated
    assert!(code.cur_cp() > 0);
    
    Ok(())
}

/// Test AssignItem duplicate operation (JavaC pattern: load().duplicate())
#[test]
fn test_assign_item_duplicate() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create assignment item
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Test duplicate operation
    assign_item.duplicate(&mut items)?;
    
    // Verify code was generated for both assignment and duplication
    assert!(code.cur_cp() > 1);
    
    Ok(())
}

/// Test AssignItem drop operation (JavaC pattern: lhs.store())
#[test]
fn test_assign_item_drop() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create assignment item
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Test drop operation
    assign_item.drop(&mut items)?;
    
    // Verify code was generated
    assert!(code.cur_cp() > 0);
    
    Ok(())
}

/// Test AssignItem width calculation (JavaC pattern: lhs.width() + Code.width(typecode))
#[test]
fn test_assign_item_width_calculation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test with INT assignment (width should be lhs.width() + 1)
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs_int = items.make_local_item(&int_type, 1);
    let assign_int = items.make_assign_item(lhs_int);
    assert_eq!(assign_int.width(), 2); // local(1) + int(1)
    
    // Test with LONG assignment (width should be lhs.width() + 2)
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    let lhs_long = items.make_local_item(&long_type, 2);
    let assign_long = items.make_assign_item(lhs_long);
    assert_eq!(assign_long.width(), 4); // local_long(2) + long_type_width(2)
    
    // Test with DOUBLE assignment (width should be lhs.width() + 2)
    let double_type = TypeEnum::Primitive(PrimitiveType::Double);
    let lhs_double = items.make_local_item(&double_type, 3);
    let assign_double = items.make_assign_item(lhs_double);
    assert_eq!(assign_double.width(), 4); // local_double(2) + double_type_width(2)
    
    Ok(())
}

/// Test AssignItem stash operation should error (JavaC pattern: Assert.error())
#[test]
fn test_assign_item_stash_should_error() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create assignment item
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Stash should return an error
    let result = assign_item.stash(opcodes::typecodes::INT, &mut items);
    assert!(result.is_err());
    
    Ok(())
}

/// Test AssignItem store operation should error (JavaC pattern: Assert.error())
#[test]
fn test_assign_item_store_should_error() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create assignment item
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Store should return an error
    let result = assign_item.store(&mut items);
    assert!(result.is_err());
    
    Ok(())
}

/// Test AssignItem with different types
#[test]
fn test_assign_item_different_types() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(30, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test INT assignment
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs_int = items.make_local_item(&int_type, 1);
    let assign_int = items.make_assign_item(lhs_int);
    let result_int = assign_int.load(&mut items)?;
    if let Item::Stack { typecode, .. } = result_int {
        assert_eq!(typecode, 4); // INT typecode
    }
    
    // Test LONG assignment
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    let lhs_long = items.make_local_item(&long_type, 2);
    let assign_long = items.make_assign_item(lhs_long);
    let result_long = assign_long.load(&mut items)?;
    if let Item::Stack { typecode, .. } = result_long {
        assert_eq!(typecode, 5); // LONG typecode
    }
    
    // Test FLOAT assignment
    let float_type = TypeEnum::Primitive(PrimitiveType::Float);
    let lhs_float = items.make_local_item(&float_type, 4);
    let assign_float = items.make_assign_item(lhs_float);
    let result_float = assign_float.load(&mut items)?;
    if let Item::Stack { typecode, .. } = result_float {
        assert_eq!(typecode, 6); // FLOAT typecode
    }
    
    // Test DOUBLE assignment
    let double_type = TypeEnum::Primitive(PrimitiveType::Double);
    let lhs_double = items.make_local_item(&double_type, 5);
    let assign_double = items.make_assign_item(lhs_double);
    let result_double = assign_double.load(&mut items)?;
    if let Item::Stack { typecode, .. } = result_double {
        assert_eq!(typecode, 7); // DOUBLE typecode
    }
    
    // Test BOOLEAN assignment (maps to BYTE typecode)
    let bool_type = TypeEnum::Primitive(PrimitiveType::Boolean);
    let lhs_bool = items.make_local_item(&bool_type, 6);
    let assign_bool = items.make_assign_item(lhs_bool);
    let result_bool = assign_bool.load(&mut items)?;
    if let Item::Stack { typecode, .. } = result_bool {
        assert_eq!(typecode, 1); // BYTE typecode (boolean maps to BYTE)
    }
    
    Ok(())
}

/// Test AssignItem display formatting
#[test]
fn test_assign_item_display() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create assignment item
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Test display formatting
    let display_str = format!("{}", assign_item);
    assert!(display_str.contains("assign"));
    
    Ok(())
}

/// Test AssignItem with complex lhs (JavaC alignment)
#[test]
fn test_assign_item_complex_lhs() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(40, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create a member item as complex lhs
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let member_item = items.make_member_item("field".to_string(), "TestClass".to_string(), "I".to_string(), false, &int_type);
    
    // Create assignment with member as lhs
    let assign_item = items.make_assign_item(member_item);
    
    // Test load operation with complex lhs
    let result = assign_item.load(&mut items)?;
    if let Item::Stack { typecode, .. } = result {
        assert_eq!(typecode, 4); // INT typecode
    }
    
    // Verify more complex bytecode was generated
    assert!(code.cur_cp() > 2);
    
    Ok(())
}

/// Test AssignItem integration with code generation patterns
#[test]
fn test_assign_item_code_generation_patterns() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(50, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Simulate: x = 42 (where we need the result)
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Load the assignment result (JavaC pattern for expression statements that need result)
    let result = assign_item.load(&mut items)?;
    
    // Use the result (simulate further computation)
    let result_clone = result.clone();
    result.duplicate(&mut items)?;
    result_clone.drop(&mut items)?;
    
    // Verify substantial code generation
    assert!(code.cur_cp() > 3);
    
    Ok(())
}

/// Test AssignItem chain operations (a = b = c pattern)
#[test]
fn test_assign_item_chain_operations() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(60, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Simulate: a = (b = c) pattern
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    
    // First create b = c
    let b = items.make_local_item(&int_type, 2);
    let assign_b_c = items.make_assign_item(b);
    
    // Load b = c to get result on stack
    let bc_result = assign_b_c.load(&mut items)?;
    
    // Now create a = (result of b = c)
    let a = items.make_local_item(&int_type, 1);
    let assign_a_bc = items.make_assign_item(a);
    
    // Load final assignment
    let final_result = assign_a_bc.load(&mut items)?;
    
    // Verify both are stack items with correct type
    if let Item::Stack { typecode, .. } = bc_result {
        assert_eq!(typecode, 4); // INT typecode
    }
    if let Item::Stack { typecode, .. } = final_result {
        assert_eq!(typecode, 4); // INT typecode
    }
    
    // Verify complex code generation
    assert!(code.cur_cp() > 4);
    
    Ok(())
}

/// Test AssignItem error handling edge cases
#[test]
fn test_assign_item_error_handling() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create assignment item
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Test operations that should error
    let assign_item_clone = assign_item.clone();
    assert!(assign_item.stash(opcodes::typecodes::INT, &mut items).is_err());
    assert!(assign_item_clone.store(&mut items).is_err());
    
    // Test operations that should succeed
    let assign_item2 = items.make_assign_item(items.make_local_item(&int_type, 2));
    let assign_item3 = items.make_assign_item(items.make_local_item(&int_type, 3));
    assert!(assign_item2.duplicate(&mut items).is_ok());
    assert!(assign_item3.drop(&mut items).is_ok());
    
    Ok(())
}

/// Test AssignItem JavaC bytecode alignment
#[test]
fn test_assign_item_javac_bytecode_alignment() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(30, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test assignment that should generate JavaC-aligned bytecode
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let lhs = items.make_local_item(&int_type, 1);
    let assign_item = items.make_assign_item(lhs);
    
    // Load should generate store instruction for assignment
    let _result = assign_item.load(&mut items)?;
    
    // Verify bytecode generation occurred
    assert!(items.code.cur_cp() > 0);
    
    // Duplicate should generate both assignment and duplication
    let assign_item2 = items.make_assign_item(items.make_local_item(&int_type, 2));
    assign_item2.duplicate(&mut items)?;
    
    // Should generate more instructions for duplicate
    assert!(items.code.cur_cp() > 2);
    
    Ok(())
}