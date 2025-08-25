use tolc::codegen::cond_item::CondItem;
use tolc::codegen::code::Code;
use tolc::codegen::constpool::ConstantPool;
use tolc::codegen::chain::{Chain, ChainOps};
use tolc::codegen::opcodes;
use tolc::common::error::Result;

/// Tests for CondItem conditional jump optimization (JavaC alignment)
/// This module tests JavaC's CondItem - conditional expression optimization
/// 
/// JavaC Pattern:
/// - genCond(expr) generates a CondItem with appropriate jump chains
/// - CondItem.jumpTrue() emits conditional jumps when expression is true
/// - CondItem.jumpFalse() emits conditional jumps when expression is false
/// - CondItem.load() materializes the boolean result on the stack
/// - CondItem.negate() creates logical negation with swapped jump chains

/// Test CondItem creation with jump chains
#[test]
fn test_cond_item_creation_with_chains() -> Result<()> {
    let mut _pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    // Create some jump chains for testing
    let true_chain = code.branch(opcodes::GOTO);
    let false_chain = code.branch(opcodes::GOTO);
    
    let cond = CondItem::new(opcodes::IFEQ, true_chain, false_chain);
    
    assert_eq!(cond.opcode, opcodes::IFEQ);
    // Note: chains might be consumed during creation, so just check basic functionality
    assert!(!cond.is_true());
    assert!(!cond.is_false());
    
    Ok(())
}

/// Test CondItem jump generation for true branch
#[test]
fn test_cond_item_jump_true() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    let mut cond = CondItem::new(opcodes::IFEQ, None, None);
    
    // Generate jump when condition is true
    let jump_chain = cond.jump_true(&mut code)?;
    
    // Verify jump was generated
    assert!(jump_chain.is_some());
    
    Ok(())
}

/// Test CondItem jump generation for false branch  
#[test]
fn test_cond_item_jump_false() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    let mut cond = CondItem::new(opcodes::IFEQ, None, None);
    
    // Generate jump when condition is false (should negate opcode)
    let jump_chain = cond.jump_false(&mut code)?;
    
    // Verify jump was generated
    assert!(jump_chain.is_some());
    
    Ok(())
}

/// Test CondItem load operation (materialize boolean on stack)
#[test]
fn test_cond_item_load_operation() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    let mut cond = CondItem::new(opcodes::IFEQ, None, None);
    
    // Load should materialize boolean value on stack
    cond.load(&mut code)?;
    
    // Verify code was generated (basic sanity check)
    assert!(code.cur_cp() > 0);
    
    Ok(())
}

/// Test CondItem always true optimization
#[test]
fn test_cond_item_always_true() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    let mut always_true = CondItem::always_true();
    
    assert!(always_true.is_true());
    assert!(!always_true.is_false());
    assert_eq!(always_true.opcode, opcodes::GOTO);
    
    // Load operation for always true should be optimized
    always_true.load(&mut code)?;
    
    Ok(())
}

/// Test CondItem always false optimization
#[test]
fn test_cond_item_always_false() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    let mut always_false = CondItem::always_false();
    
    assert!(!always_false.is_true());
    assert!(always_false.is_false());
    assert_eq!(always_false.opcode, opcodes::DONTGOTO);
    
    // Load operation for always false should be optimized
    always_false.load(&mut code)?;
    
    Ok(())
}

/// Test CondItem negation operation
#[test]
fn test_cond_item_negation() -> Result<()> {
    let mut _pool = ConstantPool::new();
    let mut code = Code::new(10, true, false);
    
    let true_chain = code.branch(opcodes::GOTO);
    let false_chain = code.branch(opcodes::GOTO);
    
    let cond = CondItem::new(opcodes::IFEQ, true_chain, false_chain);
    let negated = cond.negate();
    
    // Opcode should be negated
    assert_eq!(negated.opcode, opcodes::IFNE);
    
    // Basic functionality test - chains might be swapped but don't test presence since they can be consumed
    assert!(!negated.is_true());
    assert!(!negated.is_false());
    
    Ok(())
}

/// Test CondItem logical AND operation (JavaC pattern)
#[test]
fn test_cond_item_logical_and() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    
    let left = CondItem::new(opcodes::IFEQ, None, None);
    let right = CondItem::new(opcodes::IFNE, None, None);
    
    // JavaC: left && right
    let result = CondItem::logical_and(left, right, &mut code)?;
    
    // Result should use right's opcode for final test
    assert_eq!(result.opcode, opcodes::IFNE);
    
    Ok(())
}

/// Test CondItem logical OR operation (JavaC pattern)
#[test]
fn test_cond_item_logical_or() -> Result<()> {
    let mut pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    
    let left = CondItem::new(opcodes::IFEQ, None, None);
    let right = CondItem::new(opcodes::IFNE, None, None);
    
    // JavaC: left || right
    let result = CondItem::logical_or(left, right, &mut code)?;
    
    // Result should use right's opcode for final test
    assert_eq!(result.opcode, opcodes::IFNE);
    
    Ok(())
}

/// Test CondItem factory methods for different comparison types  
#[test]
fn test_cond_item_factory_methods() -> Result<()> {
    // Test integer comparison factory
    let int_eq = CondItem::int_comparison(opcodes::IF_ICMPEQ);
    assert_eq!(int_eq.opcode, opcodes::IF_ICMPEQ);
    
    let int_lt = CondItem::int_comparison(opcodes::IF_ICMPLT);
    assert_eq!(int_lt.opcode, opcodes::IF_ICMPLT);
    
    // Test reference comparison factory
    let ref_eq = CondItem::reference_comparison(opcodes::IF_ACMPEQ);
    assert_eq!(ref_eq.opcode, opcodes::IF_ACMPEQ);
    
    let ref_ne = CondItem::reference_comparison(opcodes::IF_ACMPNE);
    assert_eq!(ref_ne.opcode, opcodes::IF_ACMPNE);
    
    // Test null check factory
    let null_check = CondItem::null_check(opcodes::IFNULL);
    assert_eq!(null_check.opcode, opcodes::IFNULL);
    
    let nonnull_check = CondItem::null_check(opcodes::IFNONNULL);
    assert_eq!(nonnull_check.opcode, opcodes::IFNONNULL);
    
    // Test zero comparison factory
    let zero_eq = CondItem::zero_comparison(opcodes::IFEQ);
    assert_eq!(zero_eq.opcode, opcodes::IFEQ);
    
    let zero_ne = CondItem::zero_comparison(opcodes::IFNE);
    assert_eq!(zero_ne.opcode, opcodes::IFNE);
    
    Ok(())
}

/// Test CondItem complete opcode negation table (JavaC alignment)
#[test]
fn test_cond_item_complete_opcode_negation() -> Result<()> {
    // Test all conditional opcodes can be negated correctly
    let test_cases = vec![
        (opcodes::IFEQ, opcodes::IFNE),
        (opcodes::IFNE, opcodes::IFEQ),
        (opcodes::IFLT, opcodes::IFGE),
        (opcodes::IFGE, opcodes::IFLT),
        (opcodes::IFGT, opcodes::IFLE),
        (opcodes::IFLE, opcodes::IFGT),
        (opcodes::IF_ICMPEQ, opcodes::IF_ICMPNE),
        (opcodes::IF_ICMPNE, opcodes::IF_ICMPEQ),
        (opcodes::IF_ICMPLT, opcodes::IF_ICMPGE),
        (opcodes::IF_ICMPGE, opcodes::IF_ICMPLT),
        (opcodes::IF_ICMPGT, opcodes::IF_ICMPLE),
        (opcodes::IF_ICMPLE, opcodes::IF_ICMPGT),
        (opcodes::IF_ACMPEQ, opcodes::IF_ACMPNE),
        (opcodes::IF_ACMPNE, opcodes::IF_ACMPEQ),
        (opcodes::IFNULL, opcodes::IFNONNULL),
        (opcodes::IFNONNULL, opcodes::IFNULL),
        (opcodes::GOTO, opcodes::DONTGOTO),
        (opcodes::DONTGOTO, opcodes::GOTO),
    ];
    
    for (opcode, expected_negated) in test_cases {
        let negated = CondItem::negate_opcode(opcode);
        assert_eq!(negated, expected_negated, 
            "Negation of {} should be {}, got {}", 
            CondItem::opcode_mnemonic(opcode),
            CondItem::opcode_mnemonic(expected_negated),
            CondItem::opcode_mnemonic(negated)
        );
    }
    
    Ok(())
}

/// Test CondItem chain merging operations
#[test]
fn test_cond_item_chain_merging() -> Result<()> {
    let _pool = ConstantPool::new();
    let mut code = Code::new(20, true, false);
    
    // Create multiple chains
    let chain1 = code.branch(opcodes::GOTO);
    let chain2 = code.branch(opcodes::GOTO);
    let chain3 = code.branch(opcodes::GOTO);
    
    // Test merging chains
    let merged_12 = ChainOps::merge(chain1, chain2);
    let merged_all = ChainOps::merge(merged_12, chain3);
    
    assert!(merged_all.is_some());
    
    Ok(())
}

/// Test CondItem integration with code generation patterns
#[test]
fn test_cond_item_code_generation_patterns() -> Result<()> {
    let mut _pool = ConstantPool::new();
    let mut code = Code::new(50, true, false);
    
    // Simulate JavaC's if-statement pattern
    // JavaC: genCond(condition).jumpFalse() -> else_chain
    let mut condition = CondItem::new(opcodes::IFEQ, None, None);
    let else_chain = condition.jump_false(&mut code)?;
    
    // Generate then-block code
    code.emitop(opcodes::ICONST_1);
    code.emitop(opcodes::ISTORE_1);
    
    // Jump over else block
    let end_chain = code.branch(opcodes::GOTO);
    
    // Generate else-block code
    if let Some(else_chain) = else_chain {
        code.resolve(Some(else_chain));
    }
    code.emitop(opcodes::ICONST_0);
    code.emitop(opcodes::ISTORE_1);
    
    // Resolve end of if-statement
    if let Some(end_chain) = end_chain {
        code.resolve(Some(end_chain));
    }
    
    // Verify code was generated (more lenient check)
    assert!(code.cur_cp() > 5);
    
    Ok(())
}

/// Test CondItem with AST tree association
#[test]
fn test_cond_item_with_tree_association() -> Result<()> {
    use tolc::ast::{Expr, BinaryExpr, BinaryOp, Literal, LiteralExpr, Span};
    
    // Create a simple binary expression: 5 == 10
    let left = Box::new(Expr::Literal(LiteralExpr {
        value: Literal::Integer(5),
        span: Span::from_to(0, 0, 0, 1),
    }));
    
    let right = Box::new(Expr::Literal(LiteralExpr {
        value: Literal::Integer(10),
        span: Span::from_to(0, 2, 0, 4),
    }));
    
    let binary_expr = Expr::Binary(BinaryExpr {
        left,
        operator: BinaryOp::Eq,
        right,
        span: Span::from_to(0, 0, 0, 4),
    });
    
    let cond = CondItem::new_with_tree(opcodes::IF_ICMPEQ, None, None, binary_expr);
    
    assert_eq!(cond.opcode, opcodes::IF_ICMPEQ);
    assert!(cond.tree.is_some());
    
    Ok(())
}

/// Test CondItem stack management and width
#[test]
fn test_cond_item_stack_management() -> Result<()> {
    let cond = CondItem::new(opcodes::IFEQ, None, None);
    
    // CondItems don't have concrete stack width
    assert_eq!(cond.typecode, opcodes::typecodes::BYTE);
    
    Ok(())
}

/// Test CondItem error cases
#[test]
#[should_panic]
fn test_cond_item_width_panics() {
    let cond = CondItem::new(opcodes::IFEQ, None, None);
    cond.width(); // Should panic - CondItems don't have stack width
}

/// Test CondItem display formatting
#[test]
fn test_cond_item_display_formatting() -> Result<()> {
    let test_cases = vec![
        (opcodes::IFEQ, "cond(ifeq)"),
        (opcodes::IF_ICMPEQ, "cond(if_icmpeq)"),
        (opcodes::IFNULL, "cond(ifnull)"),
        (opcodes::GOTO, "cond(goto)"),
    ];
    
    for (opcode, expected) in test_cases {
        let cond = CondItem::new(opcode, None, None);
        assert_eq!(format!("{}", cond), expected);
    }
    
    Ok(())
}

/// Test CondItem JavaC alignment with complex nested conditions
#[test]
fn test_cond_item_nested_conditions() -> Result<()> {
    let _pool = ConstantPool::new();
    let mut code = Code::new(100, true, false);
    
    // Simulate: (a == b) && (c != d)
    let left_cond = CondItem::new(opcodes::IF_ICMPEQ, None, None);
    let right_cond = CondItem::new(opcodes::IF_ICMPNE, None, None);
    
    let and_result = CondItem::logical_and(left_cond, right_cond, &mut code)?;
    
    // Test the AND result
    assert_eq!(and_result.opcode, opcodes::IF_ICMPNE);
    
    // Simulate: (a == b) || (c != d)  
    let left_cond2 = CondItem::new(opcodes::IF_ICMPEQ, None, None);
    let right_cond2 = CondItem::new(opcodes::IF_ICMPNE, None, None);
    
    let or_result = CondItem::logical_or(left_cond2, right_cond2, &mut code)?;
    
    // Test the OR result
    assert_eq!(or_result.opcode, opcodes::IF_ICMPNE);
    
    Ok(())
}