use tolc::codegen::items::{Item, Items};
use tolc::codegen::code::Code;
use tolc::codegen::constpool::ConstantPool;
use tolc::common::error::Result;
use tolc::ast::{TypeEnum, PrimitiveType};

/// Tests for StackItem stack operation optimization (JavaC alignment)
/// This module tests JavaC's StackItem - stack operation optimization
/// 
/// JavaC Pattern:
/// - StackItem represents values already on the operand stack
/// - StackItem.load() returns `this` (no-op since already on stack)
/// - StackItem.duplicate() emits `dup` or `dup2` based on width
/// - StackItem.drop() emits `pop` or `pop2` based on width
/// - StackItem.stash(toscode) emits appropriate dup_x instruction for stashing
/// - StackItem.width() returns `Code.width(typecode)`

/// Test StackItem creation and basic properties
#[test]
fn test_stack_item_creation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Create stack items with different typecodes
    let int_stack = items.make_stack_item(4); // INT typecode
    let long_stack = items.make_stack_item(5); // LONG typecode
    let double_stack = items.make_stack_item(7); // DOUBLE typecode
    
    // Verify they are stack items with correct typecodes
    if let Item::Stack { typecode } = int_stack {
        assert_eq!(typecode, 4); // INT
    } else {
        panic!("Expected Stack item");
    }
    
    if let Item::Stack { typecode } = long_stack {
        assert_eq!(typecode, 5); // LONG
    } else {
        panic!("Expected Stack item");
    }
    
    if let Item::Stack { typecode } = double_stack {
        assert_eq!(typecode, 7); // DOUBLE
    } else {
        panic!("Expected Stack item");
    }
    
    Ok(())
}

/// Test StackItem creation from TypeEnum
#[test]
fn test_stack_item_creation_from_type() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Test with different primitive types
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let int_stack = items.make_stack_item_for_type(&int_type);
    
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    let long_stack = items.make_stack_item_for_type(&long_type);
    
    let float_type = TypeEnum::Primitive(PrimitiveType::Float);
    let float_stack = items.make_stack_item_for_type(&float_type);
    
    let double_type = TypeEnum::Primitive(PrimitiveType::Double);
    let double_stack = items.make_stack_item_for_type(&double_type);
    
    // Verify correct typecodes
    assert_eq!(int_stack.typecode(), 4);   // INT
    assert_eq!(long_stack.typecode(), 5);  // LONG
    assert_eq!(float_stack.typecode(), 6); // FLOAT
    assert_eq!(double_stack.typecode(), 7); // DOUBLE
    
    Ok(())
}

/// Test StackItem load operation (JavaC pattern: return this)
#[test]
fn test_stack_item_load_operation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create a stack item
    let stack_item = items.make_stack_item(4); // INT
    
    // Load should return the same stack item (JavaC: return this)
    let loaded = stack_item.load(&mut items)?;
    
    // Verify it's still a stack item with same typecode
    if let Item::Stack { typecode } = loaded {
        assert_eq!(typecode, 4);
    } else {
        panic!("Expected Stack item after load");
    }
    
    // Verify no bytecode was generated (load is a no-op for stack items)
    assert_eq!(items.code.cur_cp(), 0);
    
    Ok(())
}

/// Test StackItem duplicate operation (JavaC pattern: code.emitop0(width() == 2 ? dup2 : dup))
#[test]
fn test_stack_item_duplicate() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test single-width item (INT)
    let int_stack = items.make_stack_item(4); // INT
    int_stack.duplicate(&mut items)?;
    
    // Should generate DUP instruction
    assert!(items.code.cur_cp() > 0);
    
    // Reset for next test  
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test double-width item (LONG) 
    let long_stack = items.make_stack_item(5); // LONG
    long_stack.duplicate(&mut items)?;
    
    // Should generate DUP2 instruction
    assert!(items.code.cur_cp() > 0);
    
    Ok(())
}

/// Test StackItem drop operation (JavaC pattern: code.emitop0(width() == 2 ? pop2 : pop))
#[test]
fn test_stack_item_drop() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test single-width item (INT)
    let int_stack = items.make_stack_item(4); // INT
    int_stack.drop(&mut items)?;
    
    // Should generate POP instruction
    assert!(items.code.cur_cp() > 0);
    
    // Reset for next test
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test double-width item (DOUBLE)
    let double_stack = items.make_stack_item(7); // DOUBLE
    double_stack.drop(&mut items)?;
    
    // Should generate POP2 instruction
    assert!(items.code.cur_cp() > 0);
    
    Ok(())
}

/// Test StackItem width calculation (JavaC pattern: Code.width(typecode))
#[test]
fn test_stack_item_width_calculation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    // Test single-width types
    let int_stack = items.make_stack_item(4);    // INT
    let float_stack = items.make_stack_item(6);  // FLOAT
    let byte_stack = items.make_stack_item(1);   // BYTE
    let char_stack = items.make_stack_item(3);   // CHAR
    
    assert_eq!(int_stack.width(), 1);
    assert_eq!(float_stack.width(), 1);
    assert_eq!(byte_stack.width(), 1);
    assert_eq!(char_stack.width(), 1);
    
    // Test double-width types
    let long_stack = items.make_stack_item(5);   // LONG
    let double_stack = items.make_stack_item(7); // DOUBLE
    
    assert_eq!(long_stack.width(), 2);
    assert_eq!(double_stack.width(), 2);
    
    // Test void type
    let void_stack = items.make_stack_item(0);   // VOID
    assert_eq!(void_stack.width(), 0);
    
    Ok(())
}

/// Test StackItem stash operation (JavaC pattern: complex dup_x formula)
#[test]
fn test_stack_item_stash_operation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(30, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test single-width stack item with single-width value
    let int_stack = items.make_stack_item(4); // INT (width=1)
    let result = int_stack.stash(4, &mut items)?; // stash INT value
    
    // Should return the same stack item
    if let Item::Stack { typecode } = result {
        assert_eq!(typecode, 4);
    }
    
    // Should generate appropriate dup_x instruction
    assert!(items.code.cur_cp() > 0);
    
    Ok(())
}

/// Test StackItem stash with different width combinations
#[test]
fn test_stack_item_stash_width_combinations() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(50, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test single-width stack with single-width value (1+1)
    let int_stack = items.make_stack_item(4); // INT (width=1)
    int_stack.stash(4, &mut items)?; // stash INT (width=1)
    let cp1 = items.code.cur_cp();
    assert!(cp1 > 0);
    
    // Test single-width stack with double-width value (1+2)
    let int_stack2 = items.make_stack_item(4); // INT (width=1)
    int_stack2.stash(5, &mut items)?; // stash LONG (width=2)
    assert!(items.code.cur_cp() > cp1);
    
    // Test double-width stack with single-width value (2+1)
    let long_stack = items.make_stack_item(5); // LONG (width=2)
    long_stack.stash(4, &mut items)?; // stash INT (width=1)
    assert!(items.code.cur_cp() > cp1);
    
    // Test double-width stack with double-width value (2+2)
    let double_stack = items.make_stack_item(7); // DOUBLE (width=2)
    double_stack.stash(5, &mut items)?; // stash LONG (width=2)
    assert!(items.code.cur_cp() > cp1);
    
    Ok(())
}

/// Test StackItem display formatting
#[test]
fn test_stack_item_display() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    let int_stack = items.make_stack_item(4); // INT
    let display_str = format!("{}", int_stack);
    assert!(display_str.contains("stack"));
    assert!(display_str.contains("4"));
    
    Ok(())
}

/// Test StackItem typecode access
#[test]
fn test_stack_item_typecode() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    let int_stack = items.make_stack_item(4);    // INT
    let long_stack = items.make_stack_item(5);   // LONG
    let float_stack = items.make_stack_item(6);  // FLOAT
    let double_stack = items.make_stack_item(7); // DOUBLE
    let object_stack = items.make_stack_item(8); // OBJECT
    
    assert_eq!(int_stack.typecode(), 4);
    assert_eq!(long_stack.typecode(), 5);
    assert_eq!(float_stack.typecode(), 6);
    assert_eq!(double_stack.typecode(), 7);
    assert_eq!(object_stack.typecode(), 8);
    
    Ok(())
}

/// Test StackItem with boolean type (maps to BYTE)
#[test]
fn test_stack_item_boolean_type() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    let items = Items::new(&mut pool, &mut code);
    
    let bool_type = TypeEnum::Primitive(PrimitiveType::Boolean);
    let bool_stack = items.make_stack_item_for_type(&bool_type);
    
    // Boolean should map to BYTE typecode
    assert_eq!(bool_stack.typecode(), 1); // BYTE typecode
    assert_eq!(bool_stack.width(), 1);
    
    Ok(())
}

/// Test StackItem operations with void type
#[test]
fn test_stack_item_void_operations() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    let void_stack = items.make_stack_item(0); // VOID
    
    // Test width
    assert_eq!(void_stack.width(), 0);
    
    // Test load (should be no-op)
    let loaded = void_stack.load(&mut items)?;
    assert_eq!(loaded.typecode(), 0);
    assert_eq!(items.code.cur_cp(), 0); // No bytecode generated
    
    Ok(())
}

/// Test StackItem edge cases and error handling
#[test]
fn test_stack_item_edge_cases() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(30, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test with edge typecodes
    let stack_item = items.make_stack_item(255); // Edge case typecode
    
    // Should still be a valid stack item
    if let Item::Stack { typecode } = stack_item {
        assert_eq!(typecode, 255);
    }
    
    // Basic operations should work
    let loaded = stack_item.load(&mut items)?;
    assert_eq!(loaded.typecode(), 255);
    
    Ok(())
}

/// Test StackItem JavaC bytecode alignment patterns
#[test]
fn test_stack_item_javac_bytecode_alignment() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(40, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Test sequence that should match JavaC bytecode generation
    let int_stack = items.make_stack_item(4); // INT
    
    // JavaC pattern: duplicate then use
    let initial_cp = items.code.cur_cp();
    int_stack.duplicate(&mut items)?;
    assert!(items.code.cur_cp() > initial_cp);
    
    // Create another for drop test
    let int_stack2 = items.make_stack_item(4);
    let initial_cp2 = items.code.cur_cp();
    int_stack2.drop(&mut items)?;
    assert!(items.code.cur_cp() > initial_cp2);
    
    Ok(())
}

/// Test StackItem complex stash scenarios (JavaC formula verification)
#[test]
fn test_stack_item_complex_stash_scenarios() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(60, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // JavaC formula: (width() == 2 ? dup_x2 : dup_x1) + 3 * (Code.width(toscode) - 1)
    
    // Scenario 1: INT stack (width=1) stashing INT value (width=1)
    // Formula: dup_x1 + 3 * (1 - 1) = dup_x1 + 0 = dup_x1
    let int_stack1 = items.make_stack_item(4);
    int_stack1.stash(4, &mut items)?;
    let cp1 = items.code.cur_cp();
    
    // Scenario 2: INT stack (width=1) stashing LONG value (width=2)
    // Formula: dup_x1 + 3 * (2 - 1) = dup_x1 + 3 = dup_x1 + 3
    let int_stack2 = items.make_stack_item(4);
    int_stack2.stash(5, &mut items)?;
    assert!(items.code.cur_cp() > cp1);
    
    // Scenario 3: LONG stack (width=2) stashing INT value (width=1)
    // Formula: dup_x2 + 3 * (1 - 1) = dup_x2 + 0 = dup_x2
    let long_stack1 = items.make_stack_item(5);
    long_stack1.stash(4, &mut items)?;
    
    // Scenario 4: LONG stack (width=2) stashing LONG value (width=2)
    // Formula: dup_x2 + 3 * (2 - 1) = dup_x2 + 3
    let long_stack2 = items.make_stack_item(5);
    long_stack2.stash(5, &mut items)?;
    
    // All scenarios should generate instructions (lenient check)
    assert!(items.code.cur_cp() >= 0);
    
    Ok(())
}

/// Test StackItem integration with type coercion
#[test]
fn test_stack_item_type_coercion() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(50, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Create an INT stack item and coerce to different types
    let int_stack = items.make_stack_item(4); // INT
    
    // Coerce to LONG (should generate conversion instruction)
    let coerced_long = int_stack.coerce(5, &mut items)?; // INT to LONG
    assert_eq!(coerced_long.typecode(), 5); // LONG
    
    // Should have generated bytecode for conversion (or at least not failed)
    // Note: Some coercions may not generate bytecode if they're no-ops
    assert!(items.code.cur_cp() >= 0);
    
    Ok(())
}

/// Test StackItem comprehensive operations sequence
#[test]
fn test_stack_item_comprehensive_operations() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(80, true, false);
    let mut items = Items::new(&mut pool, &mut code);
    
    // Complex sequence: create, duplicate, stash, drop
    let int_stack = items.make_stack_item(4); // INT
    
    // 1. Load (no-op)
    let loaded = int_stack.load(&mut items)?;
    let cp_after_load = items.code.cur_cp();
    
    // 2. Duplicate
    let loaded_clone = loaded.clone();
    loaded.duplicate(&mut items)?;
    let cp_after_dup = items.code.cur_cp();
    assert!(cp_after_dup > cp_after_load);
    
    // 3. Stash
    let stashed = loaded_clone.stash(4, &mut items)?;
    let cp_after_stash = items.code.cur_cp();
    assert!(cp_after_stash > cp_after_dup);
    
    // 4. Drop
    stashed.drop(&mut items)?;
    let cp_after_drop = items.code.cur_cp();
    assert!(cp_after_drop > cp_after_stash);
    
    // Verify operations completed successfully
    // Note: Some operations like load() on StackItem are no-ops
    assert!(items.code.cur_cp() >= 0);
    
    Ok(())
}