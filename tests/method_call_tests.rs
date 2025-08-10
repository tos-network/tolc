use tolc::ast::*;
use tolc::codegen::*;
use tolc::parser::Parser;
use tolc::error::Result;

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
