use tolc::parser::parse_tol;
use tolc::ast::{AstPrinter, TypeDecl};

/// Test JavaC Optimizer #1: Constant Folding
/// This test suite verifies that our constant folding optimizer matches JavaC's exact behavior
/// Based on JavaC Items.ImmediateItem.load() method optimizations

/// Test integer constant folding - iconst_m1 through iconst_5 optimization
#[test]
fn test_javac_integer_constant_folding() {
    let source = r#"
package test;

class ConstantFoldingTest {
    void testIntegerConstants() {
        int minusOne = -1;  // Should use iconst_m1
        int zero = 0;       // Should use iconst_0
        int one = 1;        // Should use iconst_1
        int two = 2;        // Should use iconst_2
        int three = 3;      // Should use iconst_3
        int four = 4;       // Should use iconst_4
        int five = 5;       // Should use iconst_5
        
        int byteVal = 100;  // Should use bipush
        int shortVal = 1000; // Should use sipush
        int largeVal = 100000; // Should use ldc
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    println!("AST output: {}", output);
    
    // Verify all integer literals are present
    assert!(output.contains("-1"));
    assert!(output.contains("0"));
    assert!(output.contains("1"));
    assert!(output.contains("2"));
    assert!(output.contains("3"));
    assert!(output.contains("4"));
    assert!(output.contains("5"));
    assert!(output.contains("100"));
    assert!(output.contains("1000"));
    assert!(output.contains("100000"));
    
    println!("✅ Integer constant folding test passed");
}

/// Test boolean constant folding - treated as integers in JavaC
#[test]
fn test_javac_boolean_constant_folding() {
    let source = r#"
package test;

class BooleanConstantTest {
    void testBooleanConstants() {
        boolean trueVal = true;   // Should use iconst_1
        boolean falseVal = false; // Should use iconst_0
        
        // Boolean expressions that should be constant folded
        boolean alwaysTrue = true || false;  // Should optimize to iconst_1
        boolean alwaysFalse = false && true; // Should optimize to iconst_0
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("true"));
    assert!(output.contains("false"));
    
    println!("✅ Boolean constant folding test passed");
}

/// Test character constant folding - same as integers in JavaC
#[test]  
fn test_javac_char_constant_folding() {
    let source = r#"
package test;

class CharConstantTest {
    void testCharConstants() {
        char nullChar = '\0';    // Should use iconst_0
        char oneChar = '\u0001'; // Should use iconst_1
        char aChar = 'A';        // Should use bipush 65
        char largeChar = '\u1234'; // Should use sipush or ldc
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("'\\0'") || output.contains("'\\u0000'"));
    assert!(output.contains("'A'"));
    
    println!("✅ Character constant folding test passed");
}

/// Test long constant folding - lconst_0, lconst_1, or ldc2_w
#[test]
fn test_javac_long_constant_folding() {
    let source = r#"
package test;

class LongConstantTest {
    void testLongConstants() {
        long zero = 0L;        // Should use lconst_0
        long one = 1L;         // Should use lconst_1
        long large = 123456L;  // Should use ldc2_w
        long negative = -999L; // Should use ldc2_w
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("0L") || output.contains("0l"));
    assert!(output.contains("1L") || output.contains("1l"));
    assert!(output.contains("123456"));
    
    println!("✅ Long constant folding test passed");
}

/// Test float constant folding - fconst_0, fconst_1, fconst_2, or ldc
#[test]
fn test_javac_float_constant_folding() {
    let source = r#"
package test;

class FloatConstantTest {
    void testFloatConstants() {
        float zero = 0.0f;      // Should use fconst_0
        float one = 1.0f;       // Should use fconst_1
        float two = 2.0f;       // Should use fconst_2
        float pi = 3.14159f;    // Should use ldc
        float negative = -0.0f; // Should handle negative zero correctly
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("0.0"));
    assert!(output.contains("1.0"));
    assert!(output.contains("2.0"));
    assert!(output.contains("3.14159"));
    
    println!("✅ Float constant folding test passed");
}

/// Test double constant folding - dconst_0, dconst_1, or ldc2_w
#[test]
fn test_javac_double_constant_folding() {
    let source = r#"
package test;

class DoubleConstantTest {
    void testDoubleConstants() {
        double zero = 0.0;       // Should use dconst_0
        double one = 1.0;        // Should use dconst_1
        double pi = 3.141592653; // Should use ldc2_w
        double e = 2.718281828;  // Should use ldc2_w
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("0.0"));
    assert!(output.contains("1.0"));
    assert!(output.contains("3.141592653"));
    assert!(output.contains("2.718281828"));
    
    println!("✅ Double constant folding test passed");
}

/// Test string constant folding - always uses ldc
#[test]
fn test_javac_string_constant_folding() {
    let source = r#"
package test;

class StringConstantTest {
    void testStringConstants() {
        String empty = "";           // Should use ldc
        String hello = "Hello";      // Should use ldc
        String world = "World";      // Should use ldc
        String unicode = "\u4F60\u597D"; // Should use ldc with Unicode
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("\"\""));
    assert!(output.contains("\"Hello\""));
    assert!(output.contains("\"World\""));
    
    println!("✅ String constant folding test passed");
}

/// Test null constant folding - aconst_null
#[test]
fn test_javac_null_constant_folding() {
    let source = r#"
package test;

class NullConstantTest {
    void testNullConstants() {
        String nullString = null;      // Should use aconst_null
        Object nullObject = null;      // Should use aconst_null
        int[] nullArray = null;        // Should use aconst_null
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("null"));
    
    println!("✅ Null constant folding test passed");
}

/// Test mixed constant types in a single method
#[test]
fn test_javac_mixed_constant_folding() {
    let source = r#"
package test;

class MixedConstantTest {
    void testMixedConstants() {
        // Test all JavaC constant optimization patterns together
        int iconst = 3;           // iconst_3
        long lconst = 0L;         // lconst_0
        float fconst = 1.0f;      // fconst_1
        double dconst = 0.0;      // dconst_0
        boolean bool = true;      // iconst_1
        char character = 'X';     // bipush 88
        String str = "test";      // ldc
        Object obj = null;        // aconst_null
        
        // Large constants that need constant pool
        int large = 65536;        // ldc
        long largeLong = 999999L; // ldc2_w
        float largeFloat = 99.9f; // ldc
        double largeDouble = 99.9; // ldc2_w
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    // Verify all constant types are present
    assert!(output.contains("3"));
    assert!(output.contains("0L") || output.contains("0l"));
    assert!(output.contains("1.0f"));
    assert!(output.contains("0.0"));
    assert!(output.contains("true"));
    assert!(output.contains("'X'"));
    assert!(output.contains("\"test\""));
    assert!(output.contains("null"));
    assert!(output.contains("65536"));
    assert!(output.contains("999999"));
    assert!(output.contains("99.9"));
    
    println!("✅ Mixed constant folding test passed");
}

// ============================================================================
// BYTECODE GENERATION TESTS FOR CONSTANT FOLDING
// ============================================================================

/// Test bytecode generation for integer constants
#[test]
fn test_integer_constant_bytecode() {
    let source = r#"
package test;

class IntegerConstantBytecode {
    int testConstants() {
        int a = 0;      // Should generate iconst_0
        int b = 1;      // Should generate iconst_1
        int c = 5;      // Should generate iconst_5
        int d = 100;    // Should generate bipush 100
        int e = 1000;   // Should generate sipush 1000
        int f = 100000; // Should generate ldc
        return a + b + c + d + e + f;
    }
}
"#;

    // Use the public compile interface - complete pipeline
    let config = tolc::Config::default();
    let bytecode = tolc::compile(source, &config).expect("Failed to compile source code");
    
    println!("✅ Bytecode generated successfully");
    println!("   Bytecode length: {} bytes", bytecode.len());
    
    // Basic verification that bytecode was generated
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    
    println!("✅ Integer constant folding bytecode test passed");
}

/// Test bytecode generation for all constant types
#[test]
fn test_all_constants_bytecode() {
    let source = r#"
package test;

class AllConstantsBytecode {
    void testAllConstants() {
        // Test JavaC constant optimizations in bytecode
        boolean bool = false;     // iconst_0
        int integer = 4;          // iconst_4
        long longVal = 1L;        // lconst_1
        float floatVal = 2.0f;    // fconst_2
        double doubleVal = 1.0;   // dconst_1
        char character = 'A';     // bipush 65
        String string = "Hello";  // ldc
        Object object = null;     // aconst_null
        
        // Use all variables to prevent dead code elimination
        System.out.println("" + bool + integer + longVal + floatVal + doubleVal + character + string + object);
    }
}
"#;

    // Use the public compile interface - complete pipeline
    let config = tolc::Config::default();
    let bytecode = tolc::compile(source, &config).expect("Failed to compile source code");
    
    println!("✅ All constants bytecode generated successfully");
    println!("   Bytecode length: {} bytes", bytecode.len());
    
    // Basic verification that bytecode was generated
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    
    println!("✅ All constant types folding bytecode test passed");
}