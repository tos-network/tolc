use tolc::{Config, compile};
use tolc::common::error::Result;

#[test]
fn test_method_call_with_current_class() -> Result<()> {
    // Test that method calls without a target are properly resolved to the current class
    let source = r#"
        public class TestClass {
            public void testMethod() {
                anotherMethod(); // This should resolve to TestClass.anotherMethod
            }
            
            private void anotherMethod() {
                // Empty method
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Method call compilation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_system_out_println() -> Result<()> {
    // Test that System.out.println calls are handled correctly
    let source = r#"
        public class HelloWorld {
            public static void main(String[] args) {
                System.out.println("Hello, World!");
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Method call compilation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_static_method_call() -> Result<()> {
    let source = r#"
        public class MathUtils {
            public static int add(int a, int b) {
                return a + b;
            }
            
            public static void main(String[] args) {
                int result = MathUtils.add(5, 3);
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Static method call generation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_instance_method_call() -> Result<()> {
    let source = r#"
        public class Calculator {
            private int value;
            
            public void setValue(int val) {
                this.value = val;
            }
            
            public int getValue() {
                return this.value;
            }
            
            public void test() {
                setValue(42);
                int result = getValue();
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Instance method call generation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_constructor_call() -> Result<()> {
    let source = r#"
        public class Person {
            private String name;
            
            public Person(String name) {
                this.name = name;
            }
            
            public static void main(String[] args) {
                Person p = new Person("John");
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Constructor call generation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_chained_method_calls() -> Result<()> {
    let source = r#"
        public class StringBuilder {
            public StringBuilder append(String str) {
                return this;
            }
            
            public String toString() {
                return "";
            }
            
            public static void main(String[] args) {
                String result = new StringBuilder()
                    .append("Hello")
                    .append(" ")
                    .append("World")
                    .toString();
            }
        }
    "#;
    
    // Use complete 7-phase compilation pipeline
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Chained method call generation should succeed: {:?}", result);
    
    Ok(())
}
