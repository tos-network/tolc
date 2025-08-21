use tolc::parser::parse_and_verify;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::TypeDecl;

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
    
    let ast = parse_and_verify(source).expect("Failed to parse");
    let type_decl = ast.type_decls.iter().find(|td| matches!(td, TypeDecl::Class(c) if c.name == "ArithmeticTest")).expect("No ArithmeticTest class found");
    
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
    std::fs::write("ArithmeticTest.class", &bytes).expect("Failed to write class file");
    
    // Run javap to see the output
    let output = std::process::Command::new("javap")
        .arg("-c")
        .arg("-verbose")
        .arg("ArithmeticTest.class")
        .output()
        .expect("Failed to run javap");
    
    let javap_output = String::from_utf8_lossy(&output.stdout);
    println!("=== javap output ===");
    println!("{}", javap_output);
    
    // Check if methods are correctly generated
    assert!(javap_output.contains("public int calculate();"), "Method calculate() not found");
    assert!(javap_output.contains("public double calculateFloat();"), "Method calculateFloat() not found");
    
    // Check for arithmetic operations
    assert!(javap_output.contains("iadd") || javap_output.contains("imul"), "Integer arithmetic operations not found");
    assert!(javap_output.contains("dadd") || javap_output.contains("dmul"), "Double arithmetic operations not found");
    
    // Clean up
    std::fs::remove_file("ArithmeticTest.class").ok();
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
    
    let ast = parse_and_verify(source).expect("Failed to parse");
    let type_decl = ast.type_decls.iter().find(|td| matches!(td, TypeDecl::Class(c) if c.name == "UnaryTest")).expect("No UnaryTest class found");
    
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
    std::fs::write("UnaryTest.class", &bytes).expect("Failed to write class file");
    
    // Run javap to see the output
    let output = std::process::Command::new("javap")
        .arg("-c")
        .arg("-verbose")
        .arg("UnaryTest.class")
        .output()
        .expect("Failed to run javap");
    
    let javap_output = String::from_utf8_lossy(&output.stdout);
    println!("=== javap output ===");
    println!("{}", javap_output);
    
    // Check if methods are correctly generated
    assert!(javap_output.contains("public int testUnary(int);"), "Method testUnary not found");
    assert!(javap_output.contains("public boolean testLogical(boolean);"), "Method testLogical not found");
    
    // Check for unary operations
    assert!(javap_output.contains("ineg"), "Integer negation not found");
    
    // Clean up
    std::fs::remove_file("UnaryTest.class").ok();
}