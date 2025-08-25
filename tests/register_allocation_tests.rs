use tolc::codegen::register_alloc::{RegisterAllocator, ItemsRegisterExt};
use tolc::codegen::code::Code;
use tolc::codegen::constpool::ConstantPool;
use tolc::codegen::items::{Items, typecodes};
use tolc::ast::{TypeEnum, PrimitiveType};
use tolc::common::error::Result;

/// Tests for Register Allocation optimization (JavaC alignment)
/// This module tests JavaC's register allocation - local variable slot optimization
/// 
/// JavaC Pattern:
/// - nextreg tracks next available local variable slot
/// - newLocal() allocates slots with proper width handling (long/double = 2 slots)
/// - makeTemp() creates temporary variables for intermediate computations
/// - newRegSegment() starts fresh register segments
/// - Width handling: INT=1, LONG=2, DOUBLE=2, VOID=0

/// Test basic register allocation
#[test]
fn test_register_allocation_basic() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(0, false, false); // Start with 0 max_locals
    
    // Test initial state
    assert_eq!(allocator.current_nextreg(), 0);
    
    // Allocate INT slot (width=1)
    let reg1 = allocator.new_local_by_typecode(typecodes::INT, &mut code);
    assert_eq!(reg1, 0);
    assert_eq!(allocator.current_nextreg(), 1);
    assert_eq!(code.max_locals(), 1);
    
    // Allocate LONG slot (width=2)
    let reg2 = allocator.new_local_by_typecode(typecodes::LONG, &mut code);
    assert_eq!(reg2, 1);
    assert_eq!(allocator.current_nextreg(), 3);
    assert_eq!(code.max_locals(), 3);
    
    // Allocate FLOAT slot (width=1)
    let reg3 = allocator.new_local_by_typecode(typecodes::FLOAT, &mut code);
    assert_eq!(reg3, 3);
    assert_eq!(allocator.current_nextreg(), 4);
    assert_eq!(code.max_locals(), 4);
    
    Ok(())
}

/// Test register allocation with different types
#[test]
fn test_register_allocation_different_types() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(0, false, false); // Start with 0 max_locals
    
    // Test all primitive types
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    let float_type = TypeEnum::Primitive(PrimitiveType::Float);
    let double_type = TypeEnum::Primitive(PrimitiveType::Double);
    let boolean_type = TypeEnum::Primitive(PrimitiveType::Boolean);
    let byte_type = TypeEnum::Primitive(PrimitiveType::Byte);
    let short_type = TypeEnum::Primitive(PrimitiveType::Short);
    let char_type = TypeEnum::Primitive(PrimitiveType::Char);
    
    // Allocate slots for different types
    let int_reg = allocator.new_local_by_type(&int_type, &mut code);
    assert_eq!(int_reg, 0);
    assert_eq!(allocator.current_nextreg(), 1);
    
    let long_reg = allocator.new_local_by_type(&long_type, &mut code);
    assert_eq!(long_reg, 1);
    assert_eq!(allocator.current_nextreg(), 3); // long takes 2 slots
    
    let float_reg = allocator.new_local_by_type(&float_type, &mut code);
    assert_eq!(float_reg, 3);
    assert_eq!(allocator.current_nextreg(), 4);
    
    let double_reg = allocator.new_local_by_type(&double_type, &mut code);
    assert_eq!(double_reg, 4);
    assert_eq!(allocator.current_nextreg(), 6); // double takes 2 slots
    
    let boolean_reg = allocator.new_local_by_type(&boolean_type, &mut code);
    assert_eq!(boolean_reg, 6);
    assert_eq!(allocator.current_nextreg(), 7);
    
    let byte_reg = allocator.new_local_by_type(&byte_type, &mut code);
    assert_eq!(byte_reg, 7);
    assert_eq!(allocator.current_nextreg(), 8);
    
    let short_reg = allocator.new_local_by_type(&short_type, &mut code);
    assert_eq!(short_reg, 8);
    assert_eq!(allocator.current_nextreg(), 9);
    
    let char_reg = allocator.new_local_by_type(&char_type, &mut code);
    assert_eq!(char_reg, 9);
    assert_eq!(allocator.current_nextreg(), 10);
    
    assert_eq!(code.max_locals(), 10);
    
    Ok(())
}

/// Test temporary variable creation (JavaC makeTemp equivalent)
#[test]
fn test_register_allocation_temp_variables() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, false, false);
    
    // Create temporary variables
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    
    // Create separate Items instances for each temp allocation
    let temp1 = {
        let mut items = Items::new(&mut pool, &mut code);
        allocator.make_temp(&int_type, &mut items)
    };
    assert_eq!(temp1.typecode(), typecodes::INT);
    assert_eq!(allocator.current_nextreg(), 1);
    
    let temp2 = {
        let mut items = Items::new(&mut pool, &mut code);
        allocator.make_temp(&long_type, &mut items)
    };
    assert_eq!(temp2.typecode(), typecodes::LONG);
    assert_eq!(allocator.current_nextreg(), 3); // long takes 2 slots
    
    let temp3 = {
        let mut items = Items::new(&mut pool, &mut code);
        allocator.make_temp(&int_type, &mut items)
    };
    assert_eq!(temp3.typecode(), typecodes::INT);
    assert_eq!(allocator.current_nextreg(), 4);
    
    Ok(())
}

/// Test named variable allocation with reuse optimization
#[test]
fn test_register_allocation_named_variables() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(10, false, false);
    
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    
    // Allocate named variable
    let reg1 = allocator.new_local_named("x".to_string(), &int_type, &mut code);
    assert_eq!(reg1, 0);
    assert_eq!(allocator.current_nextreg(), 1);
    
    // Try to allocate same named variable (should reuse)
    let reg2 = allocator.new_local_named("x".to_string(), &int_type, &mut code);
    assert_eq!(reg2, 0); // Should reuse slot 0
    assert_eq!(allocator.current_nextreg(), 1); // nextreg shouldn't change
    
    // Allocate different named variable
    let reg3 = allocator.new_local_named("y".to_string(), &int_type, &mut code);
    assert_eq!(reg3, 1);
    assert_eq!(allocator.current_nextreg(), 2);
    
    Ok(())
}

/// Test register segment management (JavaC newRegSegment)
#[test]
fn test_register_allocation_segments() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(0, false, false); // Start with 0 max_locals
    
    // Allocate some slots
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    allocator.new_local_by_type(&int_type, &mut code);
    allocator.new_local_by_type(&int_type, &mut code);
    allocator.new_local_by_type(&int_type, &mut code);
    
    assert_eq!(allocator.current_nextreg(), 3);
    assert_eq!(code.max_locals(), 3);
    
    // Start new register segment (JavaC pattern: nextreg = max_locals)
    allocator.new_reg_segment(code.max_locals());
    assert_eq!(allocator.current_nextreg(), 3); // Should be same as max_locals
    
    // Allocate more in new segment
    allocator.new_local_by_type(&int_type, &mut code);
    assert_eq!(allocator.current_nextreg(), 4);
    assert_eq!(code.max_locals(), 4);
    
    Ok(())
}

/// Test width calculations for different typecodes (JavaC Code.width equivalent)
#[test]
fn test_register_allocation_width_calculations() -> Result<()> {
    let allocator = RegisterAllocator::new();
    
    // Test single-width types (1 slot)
    assert_eq!(allocator.width_from_typecode(typecodes::BYTE), 1); // boolean maps to BYTE
    assert_eq!(allocator.width_from_typecode(typecodes::BYTE), 1);
    assert_eq!(allocator.width_from_typecode(typecodes::SHORT), 1);
    assert_eq!(allocator.width_from_typecode(typecodes::CHAR), 1);
    assert_eq!(allocator.width_from_typecode(typecodes::INT), 1);
    assert_eq!(allocator.width_from_typecode(typecodes::FLOAT), 1);
    assert_eq!(allocator.width_from_typecode(typecodes::OBJECT), 1);
    
    // Test double-width types (2 slots)
    assert_eq!(allocator.width_from_typecode(typecodes::LONG), 2);
    assert_eq!(allocator.width_from_typecode(typecodes::DOUBLE), 2);
    
    // Test void type (0 slots)
    assert_eq!(allocator.width_from_typecode(typecodes::VOID), 0);
    
    Ok(())
}

/// Test slot reservation functionality
#[test]
fn test_register_allocation_slot_reservation() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(0, false, false); // Start with 0 max_locals
    
    // Reserve 3 slots
    let start_reg = allocator.reserve_slots(3, &mut code);
    assert_eq!(start_reg, 0);
    assert_eq!(allocator.current_nextreg(), 3);
    assert_eq!(code.max_locals(), 3);
    
    // Allocate normal variable after reservation
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let reg = allocator.new_local_by_type(&int_type, &mut code);
    assert_eq!(reg, 3);
    assert_eq!(allocator.current_nextreg(), 4);
    
    // Reserve more slots
    let start_reg2 = allocator.reserve_slots(2, &mut code);
    assert_eq!(start_reg2, 4);
    assert_eq!(allocator.current_nextreg(), 6);
    assert_eq!(code.max_locals(), 6);
    
    Ok(())
}

/// Test integration with Items system (JavaC alignment)
#[test]
fn test_register_allocation_items_integration() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut pool = ConstantPool::new();
    let mut code = Code::new(0, false, false); // Start with 0 max_locals
    
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    
    // Use Items extension methods in scope
    let (temp1, local_auto, local_named, final_max_locals) = {
        let mut items = Items::new(&mut pool, &mut code);
        
        let temp1 = items.make_temp(&int_type, &mut allocator);
        let local_auto = items.make_local_auto(&long_type, &mut allocator);
        let local_named = items.make_local_named("variable".to_string(), &int_type, &mut allocator);
        let max_locals = items.code.max_locals();
        
        (temp1, local_auto, local_named, max_locals)
    };
    
    assert_eq!(temp1.typecode(), typecodes::INT);
    assert_eq!(local_auto.typecode(), typecodes::LONG);
    assert_eq!(local_named.typecode(), typecodes::INT);
    
    // Check final state
    assert_eq!(allocator.current_nextreg(), 4); // 1 (int) + 2 (long) + 1 (int) = 4
    assert_eq!(final_max_locals, 4);
    
    Ok(())
}

/// Test reset functionality
#[test]
fn test_register_allocation_reset() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(10, false, false);
    
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    
    // Allocate some variables
    allocator.new_local_by_type(&int_type, &mut code);
    allocator.new_local_named("test".to_string(), &int_type, &mut code);
    
    assert_eq!(allocator.current_nextreg(), 2);
    
    // Reset allocator
    allocator.reset();
    assert_eq!(allocator.current_nextreg(), 0);
    
    // Should be able to allocate from scratch
    let reg = allocator.new_local_by_type(&int_type, &mut code);
    assert_eq!(reg, 0); // Back to slot 0
    assert_eq!(allocator.current_nextreg(), 1);
    
    Ok(())
}

/// Test register allocation with method parameter offset (JavaC pattern)
#[test]
fn test_register_allocation_with_parameter_offset() -> Result<()> {
    // JavaC pattern: instance methods reserve slot 0 for 'this', parameters start at slot 1+
    let mut allocator = RegisterAllocator::new_with_offset(3); // Simulate 'this' + 2 parameters
    let mut code = Code::new(20, false, false);
    
    // First local variable should start after parameters
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let reg = allocator.new_local_by_type(&int_type, &mut code);
    assert_eq!(reg, 3); // After 'this' + 2 params
    assert_eq!(allocator.current_nextreg(), 4);
    
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    let reg2 = allocator.new_local_by_type(&long_type, &mut code);
    assert_eq!(reg2, 4); // Next available slot
    assert_eq!(allocator.current_nextreg(), 6); // long takes 2 slots
    
    Ok(())
}

/// Test complex register allocation scenarios
#[test]
fn test_register_allocation_complex_scenarios() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut pool = ConstantPool::new();
    let mut code = Code::new(50, false, false);
    
    // Mix of different allocation methods
    let int_type = TypeEnum::Primitive(PrimitiveType::Int);
    let long_type = TypeEnum::Primitive(PrimitiveType::Long);
    let double_type = TypeEnum::Primitive(PrimitiveType::Double);
    
    // Direct typecode allocation
    let reg1 = allocator.new_local_by_typecode(typecodes::INT, &mut code);
    assert_eq!(reg1, 0);
    
    // Type-based allocation
    let reg2 = allocator.new_local_by_type(&long_type, &mut code);
    assert_eq!(reg2, 1);
    assert_eq!(allocator.current_nextreg(), 3); // long takes 2 slots
    
    // Named variable
    let reg3 = allocator.new_local_named("temp".to_string(), &int_type, &mut code);
    assert_eq!(reg3, 3);
    
    // Temporary variable - create Items instance in scope
    let temp = {
        let mut items = Items::new(&mut pool, &mut code);
        allocator.make_temp(&double_type, &mut items)
    };
    assert_eq!(temp.typecode(), typecodes::DOUBLE);
    
    // Slot reservation
    let reserved_start = allocator.reserve_slots(2, &mut code);
    assert_eq!(reserved_start, 6); // after double (2 slots)
    
    // Final state verification
    assert_eq!(allocator.current_nextreg(), 8); // 1 + 2 + 1 + 2 + 2 = 8
    assert!(code.max_locals() >= 8);
    
    Ok(())
}

/// Test JavaC compatibility patterns
#[test]
fn test_register_allocation_javac_compatibility() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(0, false, false); // Start with 0 max_locals
    
    // Test JavaC newLocal(int typecode) equivalent
    let reg1 = allocator.new_local_by_typecode(typecodes::LONG, &mut code);
    assert_eq!(reg1, 0);
    assert_eq!(allocator.current_nextreg(), 2); // LONG width = 2
    assert_eq!(code.max_locals(), 2); // max_locals updated
    
    // Test JavaC newRegSegment() equivalent
    let initial_max_locals = code.max_locals();
    allocator.new_reg_segment(initial_max_locals);
    assert_eq!(allocator.current_nextreg(), initial_max_locals);
    
    // Continue allocation after segment
    let reg2 = allocator.new_local_by_typecode(typecodes::INT, &mut code);
    assert_eq!(reg2, initial_max_locals);
    assert_eq!(allocator.current_nextreg(), initial_max_locals + 1);
    assert_eq!(code.max_locals(), initial_max_locals + 1);
    
    Ok(())
}

/// Test edge cases and error conditions
#[test]
fn test_register_allocation_edge_cases() -> Result<()> {
    let mut allocator = RegisterAllocator::new();
    let mut code = Code::new(5, false, false); // Small limit
    
    // Test with unusual typecodes
    let reg1 = allocator.new_local_by_typecode(255, &mut code);
    assert_eq!(reg1, 0);
    assert_eq!(allocator.current_nextreg(), 1); // Unknown typecode defaults to width 1
    
    // Test void width
    assert_eq!(allocator.width_from_typecode(typecodes::VOID), 0);
    
    // Test large reservation
    let reserved_start = allocator.reserve_slots(100, &mut code);
    assert_eq!(reserved_start, 1);
    assert_eq!(allocator.current_nextreg(), 101);
    assert_eq!(code.max_locals(), 101); // max_locals grows as needed
    
    Ok(())
}

/// Test register allocation display and debugging
#[test]
fn test_register_allocation_debugging() -> Result<()> {
    let allocator = RegisterAllocator::new_with_offset(5);
    
    // Test initial state
    assert_eq!(allocator.current_nextreg(), 5);
    
    // Test that we can create allocator with different initial states
    let allocator2 = RegisterAllocator::new();
    assert_eq!(allocator2.current_nextreg(), 0);
    
    Ok(())
}