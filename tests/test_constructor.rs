use tolc::parser::parse_java;
use tolc::codegen::SemanticAnalyzer;
use tolc::{Config, compile2file};

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
    
    // Parse and run complete semantic analysis pipeline
    let mut ast = parse_java(source).expect("Failed to parse Java source");
    let mut semantic_analyzer = SemanticAnalyzer::new();
    ast = semantic_analyzer.analyze(ast).expect("Failed to run semantic analysis");
    
    // Use complete compilation pipeline
    let config = Config::default();
    let output_dir = "/tmp/constructor_test";
    std::fs::create_dir_all(output_dir).expect("Failed to create output directory");
    
    let result = compile2file(source, output_dir, &config);
    assert!(result.is_ok(), "Compilation should succeed with complete pipeline");
    
    // Verify class file was generated (check both possible locations)
    let class_file_path_with_package = std::path::Path::new(output_dir).join("test").join("ConstructorTest.class");
    let class_file_path_direct = std::path::Path::new(output_dir).join("ConstructorTest.class");
    
    let class_file_path = if class_file_path_with_package.exists() {
        class_file_path_with_package
    } else if class_file_path_direct.exists() {
        class_file_path_direct
    } else {
        panic!("ConstructorTest.class should be generated in {} or {}", 
               class_file_path_with_package.display(), class_file_path_direct.display());
    };
    assert!(class_file_path.exists(), "ConstructorTest.class should be generated");
    
    // Read the generated class file for verification
    let bytes = std::fs::read(&class_file_path).expect("Failed to read generated class file");
    
    // Copy to current directory for javap inspection
    std::fs::copy(&class_file_path, "ConstructorTest.class").expect("Failed to copy class file");
    
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
    assert!(javap_output.contains("public test.ConstructorTest();"), "Default constructor not found");
    assert!(javap_output.contains("public test.ConstructorTest(int);"), "Parameterized constructor not found");
    
    // Check if constructor bodies contain field initialization bytecode
    assert!(javap_output.contains("bipush        42"), "Constructor should initialize value to 42");
    assert!(javap_output.contains("ldc           #"), "Constructor should load string constant");
    assert!(javap_output.contains("iconst_1"), "Constructor should initialize flag to true");
    assert!(javap_output.contains("putfield"), "Constructor should use putfield for field assignment");
    
    // Clean up
    std::fs::remove_file("ConstructorTest.class").ok();
}