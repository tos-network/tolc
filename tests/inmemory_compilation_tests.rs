use tolc::{compile, Config};

#[test]
fn test_simple_class_inmemory_compilation() {
    let java_source = r#"
public class SimpleTest {
    public static void main(String[] args) {
        int x = 42;
    }
}
"#;

    let config = Config::default()
        .with_target_java_version(6)
        .with_emit_frames(false); // Disable StackMapTable for Java 6
    let result = compile(java_source, &config);
    
    match result {
        Ok(bytecode) => {
            assert!(!bytecode.is_empty(), "Bytecode should not be empty");
            // Java class file should start with magic number 0xCAFEBABE
            assert_eq!(bytecode[0..4], [0xCA, 0xFE, 0xBA, 0xBE], "Invalid class file magic number");
            println!("✅ Generated {} bytes of bytecode", bytecode.len());
        }
        Err(e) => {
            panic!("Compilation failed: {}", e);
        }
    }
}

#[test]
fn test_simple_method_inmemory_compilation() {
    let java_source = r#"
public class MethodTest {
    public int add(int a, int b) {
        return a + b;
    }
}
"#;

    let config = Config::default()
        .with_target_java_version(6)
        .with_emit_frames(false); // Disable StackMapTable for Java 6
    let result = compile(java_source, &config);
    
    match result {
        Ok(bytecode) => {
            assert!(!bytecode.is_empty(), "Bytecode should not be empty");
            assert_eq!(bytecode[0..4], [0xCA, 0xFE, 0xBA, 0xBE], "Invalid class file magic number");
            println!("✅ Generated {} bytes of bytecode for method test", bytecode.len());
        }
        Err(e) => {
            panic!("Method compilation failed: {}", e);
        }
    }
}

#[test]
fn test_throw_statement_inmemory_compilation() {
    let java_source = r#"
public class ThrowTest {
    public void throwException() {
        throw new RuntimeException("Test exception");
    }
}
"#;

    let config = Config::default()
        .with_target_java_version(6)
        .with_emit_frames(false); // Disable StackMapTable for Java 6
    let result = compile(java_source, &config);
    
    match result {
        Ok(bytecode) => {
            assert!(!bytecode.is_empty(), "Bytecode should not be empty");
            assert_eq!(bytecode[0..4], [0xCA, 0xFE, 0xBA, 0xBE], "Invalid class file magic number");
            println!("✅ Generated {} bytes of bytecode for throw statement test", bytecode.len());
        }
        Err(e) => {
            panic!("Throw statement compilation failed: {}", e);
        }
    }
}