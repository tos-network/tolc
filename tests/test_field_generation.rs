use tolc::parser::parse_and_verify;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::TypeDecl;

mod common;
use common::setup_test_classpath;

#[test]
fn test_field_generation() {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
package test;

public class FieldTest {
    public int value;
    public String name;
    
    public FieldTest() {
        this.value = 42;
        this.name = "test";
    }
}
"#;
    
    let ast = parse_and_verify(source).expect("Failed to parse");
    let type_decl = ast.type_decls.iter().find(|td| matches!(td, TypeDecl::Class(c) if c.name == "FieldTest")).expect("No FieldTest class found");
    
    let mut cw = ClassWriter::new();
    cw.set_package_name(Some("test"));
    cw.set_debug(true);
    
    match type_decl {
        TypeDecl::Class(c) => {
            cw.generate_class(c).expect("Failed to generate class");
        }
        _ => panic!("Expected class"),
    }
    
    // Get the class file and convert to bytes
    let class_file = cw.get_class_file();
    let bytes = class_file_to_bytes(&class_file);
    
    // Write to file for inspection
    std::fs::write("FieldTest.class", &bytes).expect("Failed to write class file");
    
    // Run javap to see the output
    let output = std::process::Command::new("javap")
        .arg("-v")
        .arg("FieldTest.class")
        .output()
        .expect("Failed to run javap");
    
    let javap_output = String::from_utf8_lossy(&output.stdout);
    println!("=== javap output ===");
    println!("{}", javap_output);
    
    // Check if fields are correctly generated
    assert!(javap_output.contains("public int value;"), "Field 'value' not found");
    assert!(javap_output.contains("public java.lang.String name;"), "Field 'name' not found");
    
    // Clean up
    std::fs::remove_file("FieldTest.class").ok();
}
