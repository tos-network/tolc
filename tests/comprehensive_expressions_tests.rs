use tolc::ast::*;
use tolc::codegen::*;
use tolc::parser::Parser;
use tolc::common::error::Result;

#[test]
fn test_arithmetic_binary_expressions() -> Result<()> {
    let source = r#"
        public class ArithmeticTest {
            public static void main(String[] args) {
                int a = 10;
                int b = 5;
                
                // Basic arithmetic
                int add_result = a + b;      // 15
                int sub_result = a - b;      // 5
                int mul_result = a * b;      // 50
                int div_result = a / b;      // 2
                int mod_result = a % b;      // 0
                
                // Nested arithmetic
                int complex = (a + b) * (a - b) / 2 + a % b;
                
                // Mixed type arithmetic
                long long_val = 100L;
                long mixed_result = a + long_val;
                
                float float_val = 3.14f;
                float float_result = a + float_val;
                
                double double_val = 2.718;
                double double_result = a + double_val;
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Arithmetic expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_comparison_expressions() -> Result<()> {
    let source = r#"
        public class ComparisonTest {
            public static void main(String[] args) {
                int a = 10;
                int b = 5;
                
                // Comparison operators
                boolean less_than = a < b;           // false
                boolean less_equal = a <= b;         // false
                boolean greater_than = a > b;        // true
                boolean greater_equal = a >= b;      // true
                boolean equal = a == b;              // false
                boolean not_equal = a != b;          // true
                
                // Object comparison
                String str1 = "hello";
                String str2 = "world";
                boolean string_equal = str1 == str2;
                boolean string_not_equal = str1 != str2;
                
                // Null comparison
                String null_str = null;
                boolean is_null = null_str == null;
                boolean not_null = null_str != null;
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Comparison expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_logical_expressions() -> Result<()> {
    let source = r#"
        public class LogicalTest {
            public static void main(String[] args) {
                boolean a = true;
                boolean b = false;
                
                // Logical operators
                boolean logical_and = a && b;        // false
                boolean logical_or = a || b;         // true
                
                // Bitwise logical operators
                boolean bitwise_and = a & b;         // false
                boolean bitwise_or = a | b;          // true
                boolean bitwise_xor = a ^ b;         // true
                
                // Integer bitwise operations
                int x = 15;  // 1111 in binary
                int y = 10;  // 1010 in binary
                
                int int_and = x & y;                 // 1010 = 10
                int int_or = x | y;                  // 1111 = 15
                int int_xor = x ^ y;                 // 0101 = 5
                
                // Shift operations
                int left_shift = x << 2;             // 60
                int right_shift = x >> 2;            // 3
                int unsigned_right_shift = x >>> 2;  // 3
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Logical expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_unary_expressions() -> Result<()> {
    let source = r#"
        public class UnaryTest {
            public static void main(String[] args) {
                int x = 10;
                boolean flag = true;
                
                // Unary arithmetic
                int positive = +x;               // 10
                int negative = -x;               // -10
                
                // Logical negation
                boolean not_flag = !flag;        // false
                
                // Bitwise complement
                int complement = ~x;             // -11
                
                // Pre/post increment/decrement
                int pre_inc = ++x;               // x becomes 11, returns 11
                int post_inc = x++;              // returns 11, x becomes 12
                int pre_dec = --x;               // x becomes 11, returns 11
                int post_dec = x--;              // returns 11, x becomes 10
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Unary expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_assignment_expressions() -> Result<()> {
    let source = r#"
        public class AssignmentTest {
            public static void main(String[] args) {
                int x = 10;
                
                // Basic assignment
                int y = x;                       // 10
                
                // Compound assignments
                x += 5;                          // x = x + 5 = 15
                x -= 3;                          // x = x - 3 = 12
                x *= 2;                          // x = x * 2 = 24
                x /= 4;                          // x = x / 4 = 6
                x %= 5;                          // x = x % 5 = 1
                
                // Bitwise compound assignments
                x |= 8;                          // x = x | 8 = 9
                x &= 15;                         // x = x & 15 = 9
                x ^= 5;                          // x = x ^ 5 = 12
                x <<= 2;                         // x = x << 2 = 48
                x >>= 3;                         // x = x >> 3 = 6
                x >>>= 1;                        // x = x >>> 1 = 3
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Assignment expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_array_expressions() -> Result<()> {
    let source = r#"
        public class ArrayTest {
            public static void main(String[] args) {
                // Array creation
                int[] numbers = new int[10];
                int[] initialized = {1, 2, 3, 4, 5};
                
                // Array access
                int first = numbers[0];
                int second = initialized[1];
                
                // Array assignment
                numbers[0] = 42;
                numbers[1] = first + second;
                
                // Multi-dimensional arrays
                int[][] matrix = new int[3][3];
                matrix[0][0] = 1;
                matrix[1][1] = 2;
                matrix[2][2] = 3;
                
                // Array length
                int length = numbers.length;
                int matrix_rows = matrix.length;
                int matrix_cols = matrix[0].length;
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Array expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_field_access_expressions() -> Result<()> {
    let source = r#"
        public class FieldAccessTest {
            private int instance_field = 42;
            private static int static_field = 100;
            
            public void testFieldAccess() {
                // Instance field access
                int value = this.instance_field;
                this.instance_field = 84;
                
                // Static field access
                int static_value = FieldAccessTest.static_field;
                FieldAccessTest.static_field = 200;
                
                // Chained field access
                String str = "hello";
                int str_length = str.length();
                
                // System field access
                System.out.println("Test");
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Field access expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_conditional_expressions() -> Result<()> {
    let source = r#"
        public class ConditionalTest {
            public static void main(String[] args) {
                int a = 10;
                int b = 5;
                
                // Ternary conditional operator
                int max = a > b ? a : b;
                String result = a == b ? "equal" : "not equal";
                
                // Nested conditionals
                int sign = a > 0 ? 1 : (a < 0 ? -1 : 0);
                
                // Conditional with method calls
                String message = max > 10 ? "large" : max.toString();
                
                // Conditional in assignment
                boolean flag = true;
                int conditional_assignment = flag ? (a + b) : (a - b);
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Conditional expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_cast_expressions() -> Result<()> {
    let source = r#"
        public class CastTest {
            public static void main(String[] args) {
                // Primitive casts
                int int_val = 42;
                long long_val = (long) int_val;
                float float_val = (float) int_val;
                double double_val = (double) int_val;
                
                // Narrowing casts
                double d = 3.14159;
                float f = (float) d;
                int i = (int) d;
                short s = (short) i;
                byte b = (byte) i;
                char c = (char) i;
                
                // Object casts
                Object obj = "hello";
                String str = (String) obj;
                
                // Array casts
                Object[] objects = new String[10];
                String[] strings = (String[]) objects;
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Cast expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_instanceof_expressions() -> Result<()> {
    let source = r#"
        public class InstanceOfTest {
            public static void main(String[] args) {
                Object obj = "hello";
                
                // Basic instanceof
                boolean is_string = obj instanceof String;
                boolean is_object = obj instanceof Object;
                
                // Null instanceof
                Object null_obj = null;
                boolean null_instanceof = null_obj instanceof String;  // false
                
                // Array instanceof
                int[] numbers = {1, 2, 3};
                boolean is_int_array = numbers instanceof int[];
                boolean is_object_array = numbers instanceof Object;
                
                // Interface instanceof
                String str = "test";
                boolean is_serializable = str instanceof java.io.Serializable;
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "InstanceOf expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_new_expressions() -> Result<()> {
    let source = r#"
        public class NewTest {
            public static void main(String[] args) {
                // Object creation
                String str = new String("hello");
                StringBuilder sb = new StringBuilder();
                
                // Array creation
                int[] numbers = new int[10];
                String[] strings = new String[5];
                
                // Multi-dimensional array creation
                int[][] matrix = new int[3][3];
                String[][] string_matrix = new String[2][4];
                
                // Array creation with initialization
                int[] initialized = new int[]{1, 2, 3, 4, 5};
                String[] string_array = new String[]{"a", "b", "c"};
                
                // Anonymous array creation
                int[] anonymous = {10, 20, 30};
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "New expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_parenthesized_expressions() -> Result<()> {
    let source = r#"
        public class ParenthesizedTest {
            public static void main(String[] args) {
                int a = 5;
                int b = 3;
                int c = 2;
                
                // Operator precedence with parentheses
                int result1 = a + b * c;        // 11 (5 + 6)
                int result2 = (a + b) * c;      // 16 (8 * 2)
                
                // Complex nested expressions
                int complex = ((a + b) * c) + (a - (b + c));
                
                // Parentheses in conditions
                boolean condition = (a > b) && (b > c);
                
                // Parentheses in method calls
                int method_result = Math.max((a + b), (c * 2));
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Parenthesized expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}

#[test]
fn test_literal_expressions() -> Result<()> {
    let source = r#"
        public class LiteralTest {
            public static void main(String[] args) {
                // Integer literals
                int decimal = 42;
                int hex = 0x2A;
                int octal = 052;
                int binary = 0b101010;
                
                // Long literals
                long long_decimal = 42L;
                long long_hex = 0x2AL;
                
                // Float literals
                float float_val = 3.14f;
                float float_exp = 1.23e-4f;
                
                // Double literals
                double double_val = 3.14159;
                double double_exp = 1.23e-10;
                
                // Boolean literals
                boolean true_val = true;
                boolean false_val = false;
                
                // Character literals
                char char_val = 'A';
                char unicode_char = '\u0041';
                char escape_char = '\n';
                
                // String literals
                String str = "Hello, World!";
                String empty_str = "";
                String escape_str = "Line 1\nLine 2\tTabbed";
                
                // Null literal
                Object null_obj = null;
            }
        }
    "#;
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Literal expression generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration");
    }
    
    Ok(())
}