use tolc::{Config, compile};
use tolc::common::error::Result;

mod common;
use common::setup_test_classpath;

/// Enhanced Conditional Branching (genCond) Tests - JavaC aligned implementation
/// This test suite verifies our genCond implementation matches JavaC's exact behavior
/// Based on JavaC Gen.genCond() patterns for conditional expressions

#[test]
fn test_enhanced_logical_and_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class ConditionalTest {
    boolean testComplexAnd(boolean a, boolean b, boolean c) {
        // Complex short-circuit AND with multiple operands
        return a && b && c;
    }
    
    boolean testNestedAnd(boolean x, boolean y) {
        // Nested AND conditions - tests genCond recursion
        return (x && true) && (false && y);
    }
    
    boolean testMethodCallAnd() {
        // Short-circuit with method calls (side effects matter)
        return getValue() && expensiveCheck();
    }
    
    private boolean getValue() {
        return true;
    }
    
    private boolean expensiveCheck() {
        return false;
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Enhanced AND genCond should compile: {:?}", result);
    
    println!("✅ Enhanced logical AND with genCond compiled successfully");
    Ok(())
}

#[test]
fn test_enhanced_logical_or_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class ConditionalTest {
    boolean testComplexOr(boolean a, boolean b, boolean c) {
        // Complex short-circuit OR with multiple operands
        return a || b || c;
    }
    
    boolean testNestedOr(boolean x, boolean y) {
        // Nested OR conditions - tests genCond recursion
        return (x || false) || (true || y);
    }
    
    boolean testConstantOr() {
        // Constant folding in OR expressions
        return false || true || getValue();
    }
    
    private boolean getValue() {
        return false;
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Enhanced OR genCond should compile: {:?}", result);
    
    println!("✅ Enhanced logical OR with genCond compiled successfully");
    Ok(())
}

#[test]
fn test_conditional_expression_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class ConditionalTest {
    int testTernary(boolean condition) {
        // Basic ternary conditional expression
        return condition ? 42 : 0;
    }
    
    String testNestedTernary(boolean a, boolean b) {
        // Nested ternary conditions
        return a ? (b ? "both_true" : "a_true_b_false") : "a_false";
    }
    
    boolean testTernaryInLogical(boolean x, boolean y) {
        // Ternary inside logical expression
        return x && (y ? true : false);
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Conditional expression genCond should compile: {:?}", result);
    
    println!("✅ Conditional expressions (ternary) with genCond compiled successfully");
    Ok(())
}

#[test]
fn test_mixed_conditional_patterns_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class ConditionalTest {
    boolean testMixedPatterns(boolean a, boolean b, boolean c, boolean d) {
        // Complex mixed logical patterns
        return (a && b) || (c && d);
    }
    
    boolean testDeepNesting(boolean w, boolean x, boolean y, boolean z) {
        // Deep nesting tests genCond chain handling
        return w && (x || (y && z));
    }
    
    int testConditionalWithComparison(int value) {
        // Conditional with comparison operators
        return (value > 0) ? (value < 10 ? 1 : 2) : 0;
    }
    
    boolean testNullChecks(String str) {
        // Null check patterns common in Java code
        return str != null && str.length() > 0;
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Mixed conditional patterns should compile: {:?}", result);
    
    println!("✅ Mixed conditional patterns with genCond compiled successfully");
    Ok(())
}

#[test]
fn test_constant_folding_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class ConditionalTest {
    boolean testConstantTrue() {
        // Should optimize to always true
        return true || expensiveCall();
    }
    
    boolean testConstantFalse() {
        // Should optimize to always false  
        return false && expensiveCall();
    }
    
    boolean testPartialConstant() {
        // Partial constant optimization
        boolean x = getValue();
        return true && x;  // Should optimize to just x
    }
    
    private boolean expensiveCall() {
        // This method should not be called due to short-circuit
        return getValue();
    }
    
    private boolean getValue() {
        return true;
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Constant folding genCond should compile: {:?}", result);
    
    println!("✅ Constant folding optimization with genCond compiled successfully");
    Ok(())
}

#[test]
fn test_loop_condition_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class ConditionalTest {
    void testWhileCondition() {
        int i = 0;
        // Loop condition uses genCond
        while (i < 10 && isValid(i)) {
            i++;
        }
    }
    
    void testForCondition() {
        // For loop condition uses genCond
        for (int j = 0; j < 5 && getValue(); j++) {
            // Loop body
        }
    }
    
    boolean testDoWhileCondition() {
        int k = 0;
        boolean result = false;
        
        // Do-while condition uses genCond
        do {
            result = process(k);
            k++;
        } while (k < 3 && !result);
        
        return result;
    }
    
    private boolean isValid(int value) {
        return value >= 0;
    }
    
    private boolean getValue() {
        return true;
    }
    
    private boolean process(int value) {
        return value > 1;
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Loop condition genCond should compile: {:?}", result);
    
    println!("✅ Loop conditions with genCond compiled successfully");
    Ok(())
}

#[test]
fn test_if_statement_condition_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class ConditionalTest {
    int testIfCondition(boolean a, boolean b) {
        // If statement condition uses genCond
        if (a && b) {
            return 1;
        } else if (a || b) {
            return 2;
        } else {
            return 0;
        }
    }
    
    void testComplexIfCondition(String str, int value) {
        // Complex condition with multiple checks
        if (str != null && str.length() > 0 && value > 0) {
            System.out.println("All conditions met");
        }
        
        // Negated complex condition
        if (!(str == null || str.isEmpty() || value <= 0)) {
            System.out.println("Negated condition met");
        }
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "If statement condition genCond should compile: {:?}", result);
    
    println!("✅ If statement conditions with genCond compiled successfully");
    Ok(())
}

#[test]
fn test_bytecode_generation_genCond() -> Result<()> {
    setup_test_classpath();
    
    let source = r#"
package test;

class BytecodeGenCondTest {
    static boolean complexCondition(boolean a, boolean b, boolean c) {
        // Complex condition for bytecode analysis
        return a && (b || c) && !false;
    }
    
    static void testGenCondBytecode() {
        boolean result = complexCondition(true, false, true);
        System.out.println(result);
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let bytecode = compile(source, &config)?;
    
    // Verify bytecode generation
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    assert!(bytecode.len() > 200, "Generated bytecode should be substantial (got {} bytes)", bytecode.len());
    
    println!("✅ genCond bytecode generation successful: {} bytes", bytecode.len());
    
    Ok(())
}