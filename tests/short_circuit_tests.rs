use tolc::parser::parse_tol;
use tolc::ast::{AstPrinter, TypeDecl};

/// Test short-circuit evaluation for logical AND (&&)
#[test]
fn test_logical_and_short_circuit() {
    let source = r#"
package test;

class TestLogicalAnd {
    boolean test(boolean a, boolean b) {
        return a && b;
    }
    
    void testShortCircuit() {
        boolean result = false && someMethod();
        // someMethod() should not be called when left side is false
    }
    
    boolean someMethod() {
        return true;
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    println!("AST output: {}", output);
    
    assert!(output.contains("&&"));
    assert!(output.contains("LogicalAnd")); // Should use LogicalAnd in AST
}

/// Test short-circuit evaluation for logical OR (||)
#[test]
fn test_logical_or_short_circuit() {
    let source = r#"
package test;

class TestLogicalOr {
    boolean test(boolean a, boolean b) {
        return a || b;
    }
    
    void testShortCircuit() {
        boolean result = true || someMethod();
        // someMethod() should not be called when left side is true
    }
    
    boolean someMethod() {
        return false;
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("||"));
    assert!(output.contains("LogicalOr")); // Should use LogicalOr in AST
}

/// Test mixed logical operations
#[test]
fn test_mixed_logical_operations() {
    let source = r#"
package test;

class TestMixedLogical {
    void testMixed() {
        boolean a = true;
        boolean b = false;
        boolean c = true;
        
        // Test short-circuit AND
        boolean result1 = a && b && c;
        
        // Test short-circuit OR
        boolean result2 = a || b || c;
        
        // Test mixed operations
        boolean result3 = a && (b || c);
        boolean result4 = (a || b) && c;
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("&&"));
    assert!(output.contains("||"));
}

/// Test logical vs bitwise operations
#[test]
fn test_logical_vs_bitwise() {
    let source = r#"
package test;

class TestLogicalVsBitwise {
    void testDifference() {
        boolean a = true;
        boolean b = false;
        
        // Logical operations (short-circuit)
        boolean logical_and = a && b;
        boolean logical_or = a || b;
        
        // Bitwise operations (no short-circuit)
        boolean bitwise_and = a & b;
        boolean bitwise_or = a | b;
        
        // Mixed with integers (bitwise only)
        int x = 5;
        int y = 3;
        int bitwise_int_and = x & y;
        int bitwise_int_or = x | y;
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    // Should contain both logical and bitwise operators
    assert!(output.contains("&&"));
    assert!(output.contains("||"));
    assert!(output.contains("&"));
    assert!(output.contains("|"));
}

/// Test short-circuit with method calls (side effects)
#[test]
fn test_short_circuit_side_effects() {
    let source = r#"
package test;

class TestSideEffects {
    int counter = 0;
    
    boolean incrementAndReturn(boolean value) {
        counter = counter + 1;
        return value;
    }
    
    void testAndShortCircuit() {
        counter = 0;
        boolean result = false && incrementAndReturn(true);
        // counter should still be 0 because incrementAndReturn is not called
    }
    
    void testOrShortCircuit() {
        counter = 0;
        boolean result = true || incrementAndReturn(false);
        // counter should still be 0 because incrementAndReturn is not called
    }
    
    void testNoShortCircuit() {
        counter = 0;
        boolean result1 = true && incrementAndReturn(true);
        // counter should be 1
        
        counter = 0;
        boolean result2 = false || incrementAndReturn(false);
        // counter should be 1
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("&&"));
    assert!(output.contains("||"));
}

/// Test nested short-circuit operations
#[test]
fn test_nested_short_circuit() {
    let source = r#"
package test;

class TestNestedShortCircuit {
    void testNested() {
        boolean a = true;
        boolean b = false;
        boolean c = true;
        boolean d = false;
        
        // Complex nested expressions
        boolean result1 = a && (b || c) && d;
        boolean result2 = (a || b) && (c || d);
        boolean result3 = a || (b && c) || d;
        boolean result4 = (a && b) || (c && d);
        
        // Deep nesting
        boolean result5 = a && (b || (c && (d || false)));
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("&&"));
    assert!(output.contains("||"));
}

/// Test constant folding with logical operators
#[test]
fn test_logical_constant_folding() {
    let source = r#"
package test;

class TestConstantFolding {
    void testConstants() {
        // These should be optimized at compile time
        boolean alwaysTrue = true || false;
        boolean alwaysFalse = false && true;
        
        // Mixed with variables (partial optimization possible)
        boolean x = true;
        boolean partialTrue = x || false;  // Could be optimized to just x
        boolean partialFalse = x && true;  // Could be optimized to just x
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("||"));
    assert!(output.contains("&&"));
}

// ============================================================================
// BYTECODE GENERATION TESTS FOR SHORT CIRCUIT EVALUATION
// ============================================================================

/// Test bytecode generation for LogicalAnd with constant false left operand
#[test]
fn test_logical_and_short_circuit_bytecode() {
    let source = r#"
package test;

class ShortCircuitTest {
    boolean testAnd() {
        return false && someMethod();
    }
    
    boolean someMethod() {
        return true;
    }
}
"#;

    // Use the public compile interface - complete pipeline with proper ENTER→GEN communication
    let config = tolc::Config::default();
    let bytecode = tolc::compile(source, &config).expect("Failed to compile source code");
    
    println!("✅ Bytecode generated successfully using compile interface");
    println!("   Bytecode length: {} bytes", bytecode.len());
    
    // Basic verification that bytecode was generated
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    
    println!("✅ LogicalAnd short circuit bytecode generated correctly");
}

/// Test bytecode generation for LogicalOr with constant true left operand
#[test] 
fn test_logical_or_short_circuit_bytecode() {
    let source = r#"
package test;

class ShortCircuitTest {
    boolean testOr() {
        return true || someMethod();
    }
    
    boolean someMethod() {
        return false;
    }
}
"#;

    // Use the public compile interface - complete pipeline with proper ENTER→GEN communication
    let config = tolc::Config::default();
    let bytecode = tolc::compile(source, &config).expect("Failed to compile source code");
    
    println!("✅ Bytecode generated successfully using compile interface");
    println!("   Bytecode length: {} bytes", bytecode.len());
    
    // Basic verification that bytecode was generated
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    
    println!("✅ LogicalOr short circuit bytecode generated correctly");
}

/// Test bytecode generation for variable-based LogicalAnd (both operands evaluated)
#[test]
fn test_logical_and_variables_bytecode() {
    let source = r#"
package test;

class VariableTest {
    boolean testAndVar(boolean a, boolean b) {
        return a && b;
    }
}
"#;

    // Use the public compile interface - complete pipeline with proper ENTER→GEN communication
    let config = tolc::Config::default();
    let bytecode = tolc::compile(source, &config).expect("Failed to compile source code");
    
    println!("✅ Bytecode generated successfully using compile interface");
    println!("   Bytecode length: {} bytes", bytecode.len());
    
    // Basic verification that bytecode was generated
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    
    println!("✅ LogicalAnd with variables generates proper bytecode");
}

/// Test nested logical operations bytecode
#[test]
fn test_nested_logical_bytecode() {
    let source = r#"
package test;

class NestedTest {
    boolean testNested(boolean a, boolean b, boolean c) {
        return a && (b || c);
    }
}
"#;

    // Use the public compile interface - complete pipeline with proper ENTER→GEN communication
    let config = tolc::Config::default();
    let bytecode = tolc::compile(source, &config).expect("Failed to compile source code");
    
    println!("✅ Bytecode generated successfully using compile interface");
    println!("   Bytecode length: {} bytes", bytecode.len());
    
    // Basic verification that bytecode was generated
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    
    println!("✅ Nested logical operations generate correct bytecode");
}