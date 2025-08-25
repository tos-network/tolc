use tolc::parser::parse_and_verify;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::TypeDecl;

mod common;
use common::setup_test_classpath;

#[test]
fn test_simple_class_generation() {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
package test;

public class Simple {
    public int value;
    
    public Simple() {
        this.value = 42;
    }
    
    public int getValue() {
        return this.value;
    }
}
"#;
    
    let ast = parse_and_verify(source).expect("Failed to parse");
    let type_decl = ast.type_decls.iter().find(|td| matches!(td, TypeDecl::Class(c) if c.name == "Simple")).expect("No Simple class found");
    
    let mut cw = ClassWriter::new();
    cw.set_package_name(Some("test"));
    cw.set_debug(true);
    
    match type_decl {
        TypeDecl::Class(c) => {
            cw.generate_class(c).expect("Failed to generate class");
        }
        _ => panic!("Expected class"),
    }
    
    let class_file = cw.get_class_file();
    let class_bytes = class_file_to_bytes(&class_file);
    
    // Write to temp file for inspection
    let temp_path = std::env::temp_dir().join("Simple.class");
    std::fs::write(&temp_path, &class_bytes).expect("Failed to write class file");
    
    println!("Generated class file: {}", temp_path.display());
    println!("Class file size: {} bytes", class_bytes.len());
    
    // Try to run javap on it
    let output = std::process::Command::new("javap")
        .arg("-c")
        .arg("-verbose")
        .arg(&temp_path)
        .output();
    
    match output {
        Ok(out) => {
            if out.status.success() {
                let javap_output = String::from_utf8_lossy(&out.stdout);
                println!("javap output:\n{}", javap_output);
            } else {
                let error = String::from_utf8_lossy(&out.stderr);
                println!("javap failed: {}", error);
            }
        }
        Err(e) => {
            println!("Failed to run javap: {}", e);
        }
    }
}
