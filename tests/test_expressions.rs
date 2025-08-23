use tolc::{Config, compile_file};
use std::env;

#[test]
fn test_arithmetic_expressions() {
    let source = r#"
package test;

public class ArithmeticTest {
    public int calculate() {
        int a = 10;
        int b = 5;
        int result = a + b * 2 - 3;
        return result;
    }
    
    public double calculateFloat() {
        double x = 3.14;
        double y = 2.0;
        return x * y + 1.0;
    }
}
"#;
    
    // Write source to temporary file
    let temp_dir = env::temp_dir();
    let temp_file = temp_dir.join("ArithmeticTest.java");
    std::fs::write(&temp_file, source).expect("Failed to write temp file");
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile_file(&temp_file.to_string_lossy(), &temp_dir.to_string_lossy(), &config);
    assert!(result.is_ok(), "Arithmetic expressions compilation should succeed: {:?}", result);
    
    // Cleanup
    let _ = std::fs::remove_file(&temp_file);
}

#[test]
fn test_unary_expressions() {
    let source = r#"
package test;

public class UnaryTest {
    public int testUnary(int x) {
        int neg = -x;
        int pos = +x;
        return neg + pos;
    }
    
    public boolean testLogical(boolean flag) {
        return !flag;
    }
}
"#;
    
    // Write source to temporary file
    let temp_dir = env::temp_dir();
    let temp_file = temp_dir.join("UnaryTest.java");
    std::fs::write(&temp_file, source).expect("Failed to write temp file");
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile_file(&temp_file.to_string_lossy(), &temp_dir.to_string_lossy(), &config);
    assert!(result.is_ok(), "Unary expressions compilation should succeed: {:?}", result);
    
    // Cleanup
    let _ = std::fs::remove_file(&temp_file);
}