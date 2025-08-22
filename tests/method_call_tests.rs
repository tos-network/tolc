use tolc::ast::*;
use tolc::codegen::*;
use tolc::parser::Parser;
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
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    // Verify that the AST was parsed correctly
    assert!(!ast.type_decls.is_empty());
    
    // Try to generate bytecode for the class
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        
        // The generation should succeed now that we have proper method resolution
        assert!(result.is_ok(), "Class generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration in the AST");
    }
    
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
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    // Verify that the AST was parsed correctly
    assert!(!ast.type_decls.is_empty());
    
    // Try to generate bytecode for the class
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        
        // The generation should succeed
        assert!(result.is_ok(), "Class generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration in the AST");
    }
    
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
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Static method call generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration in the AST");
    }
    
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
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Instance method call generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration in the AST");
    }
    
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
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Constructor call generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration in the AST");
    }
    
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
    
    let parser = Parser::new(source)?;
    let ast = parser.parse()?;
    
    assert!(!ast.type_decls.is_empty());
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut class_writer = ClassWriter::new();
        let result = class_writer.generate_class(class);
        assert!(result.is_ok(), "Chained method call generation should succeed: {:?}", result);
    } else {
        panic!("Expected a class declaration in the AST");
    }
    
    Ok(())
}
