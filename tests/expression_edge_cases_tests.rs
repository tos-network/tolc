use tolc::{Config, compile};
use tolc::common::error::Result;

mod common;
use common::setup_test_classpath;

#[test]
fn test_operator_precedence_complex() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class PrecedenceTest {
            public static void main(String[] args) {
                int a = 2;
                int b = 3;
                int c = 4;
                int d = 5;
                
                // Complex precedence combinations
                int result1 = a + b * c - d / 2;                    // 2 + 12 - 2 = 12
                int result2 = (a + b) * (c - d) + 1;               // 5 * (-1) + 1 = -4
                int result3 = a << 2 + b * c >> 1;                 // a << (2 + 12) >> 1
                
                // Logical and arithmetic mixed
                boolean test1 = a + b > c && c * d < 30;           // 5 > 4 && 20 < 30 = true
                boolean test2 = a == 2 || b != 3 && c > d;         // true || (false && false) = true
                
                // Assignment precedence
                int x = a + b;
                x += c * d;                                          // x = x + (c * d)
                x = x > 10 ? x - 10 : x + 10;                       // Conditional has lower precedence
                
                // Unary operators
                int neg_result = -a * b + c;                        // (-a) * b + c = -6 + 4 = -2
                boolean not_result = !(a > b) && c == 4;            // (!false) && true = true
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Complex precedence should compile: {:?}", result);
    
    Ok(())
}

#[test]
fn test_numeric_literals_edge_cases() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class NumericLiteralsTest {
            public static void main(String[] args) {
                // Integer edge cases
                int max_int = 2147483647;              // Integer.MAX_VALUE
                int min_int = -2147483648;             // Integer.MIN_VALUE
                int zero = 0;
                int hex_max = 0x7FFFFFFF;
                int octal = 0777;
                int binary = 0b11111111;
                
                // Long edge cases  
                long max_long = 9223372036854775807L;  // Long.MAX_VALUE
                long min_long = -9223372036854775808L; // Long.MIN_VALUE
                long hex_long = 0x7FFFFFFFFFFFFFFFL;
                
                // Float edge cases
                float positive_infinity = Float.POSITIVE_INFINITY;
                float negative_infinity = Float.NEGATIVE_INFINITY;
                float nan = Float.NaN;
                float max_float = 3.4028235e+38f;
                float min_float = -3.4028235e+38f;
                float tiny_float = 1.4e-45f;
                
                // Double edge cases
                double max_double = 1.7976931348623157e+308;
                double min_double = -1.7976931348623157e+308;
                double tiny_double = 4.9e-324;
                
                // Special floating point values
                double positive_zero = +0.0;
                double negative_zero = -0.0;
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Numeric literals edge cases should compile: {:?}", result);
    
    Ok(())
}

#[test]
fn test_string_and_char_edge_cases() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class StringCharTest {
            public static void main(String[] args) {
                // String escape sequences
                String newline = "Line1\nLine2";
                String tab = "Col1\tCol2";
                String backslash = "Path\\file";
                String quote = "Say \"hello\"";
                String unicode = "Unicode: \u0041\u0042\u0043";  // ABC
                String empty = "";
                
                // Character edge cases
                char space = ' ';
                char newline_char = '\n';
                char tab_char = '\t';
                char backslash_char = '\\';
                char quote_char = '\'';
                char unicode_char = '\u0041';  // A
                
                // Null character and special values
                char null_char = '\0';
                char max_char = '\uFFFF';
                char digit = '9';
                char letter = 'Z';
                
                // String concatenation edge cases
                String concat1 = "Hello" + " " + "World";
                String concat2 = "Number: " + 42;
                String concat3 = null + "test";  // Should handle null
                String concat4 = "" + "";       // Empty concatenation
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "String and char edge cases should compile: {:?}", result);
    
    Ok(())
}

#[test]
fn test_array_edge_cases() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class ArrayEdgeCasesTest {
            public static void main(String[] args) {
                // Empty arrays
                int[] empty_array = new int[0];
                String[] empty_strings = {};
                
                // Large arrays (within reasonable limits for testing)
                int[] large_array = new int[1000];
                
                // Multi-dimensional edge cases
                int[][] jagged = new int[3][];
                jagged[0] = new int[1];
                jagged[1] = new int[2];
                jagged[2] = new int[3];
                
                // Array of arrays
                int[][] matrix = {{1, 2}, {3, 4}, {5, 6}};
                
                // Mixed type arrays (Object arrays)
                Object[] mixed = {"string", 42, true, null};
                
                // Array boundary access patterns
                int[] numbers = {10, 20, 30, 40, 50};
                int first = numbers[0];
                int last = numbers[numbers.length - 1];
                
                // Null array handling
                int[] null_array = null;
                // Note: null_array.length would cause NullPointerException
                
                // Array assignment patterns
                int[] source_array = {1, 2, 3};
                int[] dest_array = new int[3];
                for (int i = 0; i < source_array.length; i++) {
                    dest_array[i] = source_array[i];
                }
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Array edge cases should compile: {:?}", result);
    
    Ok(())
}

#[test]
fn test_short_circuit_evaluation_complex() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class ShortCircuitTest {
            private static boolean sideEffect = false;
            
            private static boolean setSideEffect() {
                sideEffect = true;
                return true;
            }
            
            public static void main(String[] args) {
                // Short-circuit AND
                sideEffect = false;
                boolean result1 = false && setSideEffect();  // setSideEffect() should not be called
                
                sideEffect = false;
                boolean result2 = true && setSideEffect();   // setSideEffect() should be called
                
                // Short-circuit OR
                sideEffect = false;
                boolean result3 = true || setSideEffect();   // setSideEffect() should not be called
                
                sideEffect = false;
                boolean result4 = false || setSideEffect();  // setSideEffect() should be called
                
                // Complex short-circuit chains
                boolean a = true;
                boolean b = false;
                boolean c = true;
                
                boolean complex1 = a && b && c;              // Should stop at b
                boolean complex2 = a || b || c;              // Should stop at a
                boolean complex3 = b && (a || c);            // Should stop at b
                boolean complex4 = a || (b && c);            // Should stop at a
                
                // Mixed with method calls
                boolean mixed1 = (5 > 3) && setSideEffect(); // Should call setSideEffect()
                boolean mixed2 = (3 > 5) && setSideEffect(); // Should not call setSideEffect()
                
                // Nested conditions
                boolean nested = (a || b) && (c || setSideEffect());
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Short-circuit evaluation should compile: {:?}", result);
    
    Ok(())
}

#[test]
fn test_type_conversion_edge_cases() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class TypeConversionTest {
            public static void main(String[] args) {
                // Implicit conversions
                byte b = 42;
                short s = b;           // byte to short
                int i = s;             // short to int
                long l = i;            // int to long
                float f = l;           // long to float (may lose precision)
                double d = f;          // float to double
                
                // Explicit narrowing casts
                double big_double = 123.456;
                float from_double = (float) big_double;
                long from_float = (long) from_double;
                int from_long = (int) from_float;
                short from_int = (short) from_long;
                byte from_short = (byte) from_int;
                
                // Overflow cases
                int max_int = 2147483647;
                int overflow = max_int + 1;          // Should wrap to -2147483648
                
                byte max_byte = 127;
                byte byte_overflow = (byte) (max_byte + 1); // Should wrap to -128
                
                // Char conversions
                char c = 'A';                        // 65
                int char_to_int = c;                 // 65
                char int_to_char = (char) 66;        // 'B'
                
                // Boolean conversions (only explicit)
                boolean bool_val = true;
                // int bool_to_int = (int) bool_val;  // Not allowed in Java
                
                // String to number conversions (through methods)
                String num_str = "42";
                int parsed_int = Integer.parseInt(num_str);
                double parsed_double = Double.parseDouble("3.14");
                
                // Null handling in conversions
                Object obj = null;
                String str = (String) obj;           // Null cast is allowed
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Type conversion edge cases should compile: {:?}", result);
    
    Ok(())
}

#[test]
fn test_nested_expressions_deep() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class NestedExpressionsTest {
            public static void main(String[] args) {
                int a = 1, b = 2, c = 3, d = 4, e = 5;
                
                // Deeply nested arithmetic
                int deep_calc = ((((a + b) * c) - d) / e) + (a * (b + (c - (d + e))));
                
                // Nested conditionals
                int nested_cond = a > b ? 
                    (c > d ? a + c : b + d) : 
                    (e > a ? c - e : d + a);
                
                // Nested method calls
                int max_nested = Math.max(
                    Math.max(a, b), 
                    Math.max(
                        Math.max(c, d), 
                        e
                    )
                );
                
                // Complex array access
                int[][] matrix = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
                int complex_access = matrix[a > b ? 0 : 1][c < d ? 1 : 2];
                
                // Nested logical expressions
                boolean complex_logic = 
                    (a > b && (c < d || e > a)) || 
                    (b > c && (d < e && a > c)) && 
                    !(a == b || (c == d && e == a));
                
                // Method calls with complex arguments
                System.out.println("Result: " + 
                    (a + b > c ? 
                        "Greater: " + Math.max(a + b, c) : 
                        "Lesser: " + Math.min(a + b, c)
                    )
                );
                
                // Nested casts and operations
                double result = (double)((int)(a * 2.5) + (long)(b * 1.5)) / 
                               (float)(c + d);
                
                // Assignment in complex expressions
                int x = 0;
                int y = (x = a + b) + (x = x * c);  // x is assigned multiple times
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Deeply nested expressions should compile: {:?}", result);
    
    Ok(())
}

#[test]
fn test_null_and_reference_edge_cases() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
        public class NullReferenceTest {
            public static void main(String[] args) {
                // Null assignments
                String null_string = null;
                Object null_object = null;
                int[] null_array = null;
                
                // Null comparisons
                boolean is_null = null_string == null;
                boolean not_null = null_string != null;
                boolean cross_null = null_string == null_object;  // Both null
                
                // Null in expressions
                String result1 = null + "test";        // Should become "nulltest"
                String result2 = "test" + null;        // Should become "testnull"
                String result3 = null + null;          // Should become "nullnull"
                
                // Null with instanceof
                boolean instanceof_null = null instanceof String;  // Always false
                boolean instanceof_null2 = null instanceof Object; // Always false
                
                // Null casting
                String casted_null = (String) null;    // Allowed, still null
                Object object_null = (Object) null;    // Allowed, still null
                
                // Reference equality vs object equality
                String str1 = new String("test");
                String str2 = new String("test");
                String str3 = "test";                   // String literal
                String str4 = "test";                   // Same literal
                
                boolean ref_eq1 = str1 == str2;        // false (different objects)
                boolean ref_eq2 = str3 == str4;        // true (same literal)
                boolean obj_eq1 = str1.equals(str2);   // true (same content)
                
                // Null method calls (would cause NullPointerException at runtime)
                // String null_length = null_string.length();  // Don't test this
                
                // Safe null handling patterns
                int safe_length = null_string != null ? null_string.length() : 0;
                String safe_upper = null_string != null ? null_string.toUpperCase() : null;
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Null and reference edge cases should compile: {:?}", result);
    
    Ok(())
}