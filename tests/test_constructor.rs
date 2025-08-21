use tolc::parser::parse_and_verify;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::TypeDecl;

#[test]
fn test_constructor_initialization() {
    let source = r#"
package test;

public class ConstructorTest {
    public int value;
    public String name;
    public boolean flag;
    
    public ConstructorTest() {
        this.value = 42;
        this.name = "hello";
        this.flag = true;
    }
    
    public ConstructorTest(int v) {
        this.value = v;
        this.name = "param";
        this.flag = false;
    }
}
"#;
    
    let ast = parse_and_verify(source).expect("Failed to parse");
    let type_decl = ast.type_decls.iter().find(|td| matches!(td, TypeDecl::Class(c) if c.name == "ConstructorTest")).expect("No ConstructorTest class found");
    
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
    std::fs::write("ConstructorTest.class", &bytes).expect("Failed to write class file");
    
    // Run javap to see the output
    let output = std::process::Command::new("javap")
        .arg("-v")
        .arg("ConstructorTest.class")
        .output()
        .expect("Failed to run javap");
    
    let javap_output = String::from_utf8_lossy(&output.stdout);
    println!("=== javap output ===");
    println!("{}", javap_output);
    
    // Check if fields are correctly generated
    assert!(javap_output.contains("public int value;"), "Field 'value' not found");
    assert!(javap_output.contains("public java.lang.String name;"), "Field 'name' not found");
    assert!(javap_output.contains("public boolean flag;"), "Field 'flag' not found");
    
    // Check if constructors are generated
    assert!(javap_output.contains("public ConstructorTest();"), "Default constructor not found");
    assert!(javap_output.contains("public ConstructorTest(int);"), "Parameterized constructor not found");
    
    // Check if constructor bodies contain field initialization bytecode
    assert!(javap_output.contains("bipush        42"), "Constructor should initialize value to 42");
    assert!(javap_output.contains("ldc           #"), "Constructor should load string constant");
    assert!(javap_output.contains("iconst_1"), "Constructor should initialize flag to true");
    assert!(javap_output.contains("putfield"), "Constructor should use putfield for field assignment");
    
    // Clean up
    std::fs::remove_file("ConstructorTest.class").ok();
}