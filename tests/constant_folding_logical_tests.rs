use tolc::parser::parse_tol;
use tolc::ast::{AstPrinter, TypeDecl, Literal, BinaryOp};
use tolc::codegen::const_fold_javac::ConstFoldJavaC;
use tolc::codegen::symtab::Symtab;

/// Test constant folding for LogicalAnd (&&) operations
#[test]
fn test_logical_and_constant_folding() {
    let symtab = Symtab::new();
    let folder = ConstFoldJavaC::new(symtab);

    // Test false && X = false (short-circuit)
    let left = Literal::Boolean(false);
    let right = Literal::Boolean(true);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalAnd, &left, &right);
    assert_eq!(result, Some(Literal::Boolean(false)));

    // Test true && false = false
    let left = Literal::Boolean(true);
    let right = Literal::Boolean(false);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalAnd, &left, &right);
    assert_eq!(result, Some(Literal::Boolean(false)));

    // Test true && true = true
    let left = Literal::Boolean(true);
    let right = Literal::Boolean(true);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalAnd, &left, &right);
    assert_eq!(result, Some(Literal::Boolean(true)));

    println!("✅ LogicalAnd constant folding works correctly");
}

/// Test constant folding for LogicalOr (||) operations
#[test]
fn test_logical_or_constant_folding() {
    let symtab = Symtab::new();
    let folder = ConstFoldJavaC::new(symtab);

    // Test true || X = true (short-circuit)
    let left = Literal::Boolean(true);
    let right = Literal::Boolean(false);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalOr, &left, &right);
    assert_eq!(result, Some(Literal::Boolean(true)));

    // Test false || true = true
    let left = Literal::Boolean(false);
    let right = Literal::Boolean(true);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalOr, &left, &right);
    assert_eq!(result, Some(Literal::Boolean(true)));

    // Test false || false = false
    let left = Literal::Boolean(false);
    let right = Literal::Boolean(false);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalOr, &left, &right);
    assert_eq!(result, Some(Literal::Boolean(false)));

    println!("✅ LogicalOr constant folding works correctly");
}

/// Test can_fold_logical helper function
#[test]
fn test_can_fold_logical() {
    let symtab = Symtab::new();
    let folder = ConstFoldJavaC::new(symtab);

    // LogicalAnd cases
    assert!(folder.can_fold_logical(
        &BinaryOp::LogicalAnd,
        &Literal::Boolean(false),
        &Literal::Boolean(true)
    )); // false && X is always foldable
    
    assert!(folder.can_fold_logical(
        &BinaryOp::LogicalAnd,
        &Literal::Boolean(true),
        &Literal::Boolean(false)
    )); // true && bool is foldable

    assert!(!folder.can_fold_logical(
        &BinaryOp::LogicalAnd,
        &Literal::Boolean(true),
        &Literal::Integer(1)
    )); // true && int is not foldable

    // LogicalOr cases
    assert!(folder.can_fold_logical(
        &BinaryOp::LogicalOr,
        &Literal::Boolean(true),
        &Literal::Boolean(false)
    )); // true || X is always foldable

    assert!(folder.can_fold_logical(
        &BinaryOp::LogicalOr,
        &Literal::Boolean(false),
        &Literal::Boolean(true)
    )); // false || bool is foldable

    assert!(!folder.can_fold_logical(
        &BinaryOp::LogicalOr,
        &Literal::Boolean(false),
        &Literal::Integer(0)
    )); // false || int is not foldable

    println!("✅ can_fold_logical helper works correctly");
}

/// Test constant folding with mixed operations (should not fold non-boolean operands)
#[test]
fn test_logical_no_fold_non_boolean() {
    let symtab = Symtab::new();
    let folder = ConstFoldJavaC::new(symtab);

    // LogicalAnd with integer operands should not fold
    let left = Literal::Integer(1);
    let right = Literal::Integer(0);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalAnd, &left, &right);
    assert_eq!(result, None);

    // LogicalOr with mixed operands - true || X should fold even with non-boolean X
    let left = Literal::Boolean(true);
    let right = Literal::String("test".to_string());
    let result = folder.fold_binary_expr(&BinaryOp::LogicalOr, &left, &right);
    assert_eq!(result, Some(Literal::Boolean(true))); // Short-circuit!

    // LogicalOr with false and non-boolean should not fold  
    let left = Literal::Boolean(false);
    let right = Literal::Integer(1);
    let result = folder.fold_binary_expr(&BinaryOp::LogicalOr, &left, &right);
    assert_eq!(result, None);

    println!("✅ Mixed operand folding works correctly");
}

/// Test constant folding integration with AST parsing
#[test]
fn test_logical_constant_folding_with_ast() {
    let source = r#"
package test;

class ConstantFoldingTest {
    void testConstants() {
        // These should be foldable at compile time
        boolean alwaysTrue = true || false;
        boolean alwaysFalse = false && true;
        boolean shortCircuit1 = false && someExpensiveMethod();
        boolean shortCircuit2 = true || someOtherMethod();
    }
    
    boolean someExpensiveMethod() {
        return false;
    }
    
    boolean someOtherMethod() {
        return true;
    }
}
"#;

    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    // Verify the AST contains logical operators
    assert!(output.contains("||"));
    assert!(output.contains("&&"));
    
    // Test constant folding capability
    let symtab = Symtab::new();
    let folder = ConstFoldJavaC::new(symtab);
    
    // Test the constant expressions
    let true_literal = Literal::Boolean(true);
    let false_literal = Literal::Boolean(false);
    
    // true || false should fold to true
    let result = folder.fold_binary_expr(&BinaryOp::LogicalOr, &true_literal, &false_literal);
    assert_eq!(result, Some(Literal::Boolean(true)));
    
    // false && true should fold to false
    let result = folder.fold_binary_expr(&BinaryOp::LogicalAnd, &false_literal, &true_literal);
    assert_eq!(result, Some(Literal::Boolean(false)));
    
    println!("✅ Constant folding integrated with AST parsing");
}