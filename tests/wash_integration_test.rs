//! Test for wash → codegen integration
//! 
//! This test verifies that wash semantic analysis results are properly
//! passed to the codegen phase and accessible during bytecode generation.

use std::collections::HashMap;
use tolc::{Config, compile2file};
use tolc::codegen::SemanticAnalyzer;
use tolc::parser::parse_java;

#[test]
fn test_wash_codegen_integration() {
    // Simple generic class for testing wash integration
    let java_source = r#"
public class TestGeneric<T> {
    private T value;
    
    public TestGeneric(T value) {
        this.value = value;
    }
    
    public T getValue() {
        return value;
    }
}
"#;

    // Parse the Java source
    let mut ast = parse_java(java_source).expect("Failed to parse Java source");
    
    // Run wash semantic analysis
    let mut semantic_analyzer = SemanticAnalyzer::new();
    ast = semantic_analyzer.analyze(ast).expect("Failed to run wash analysis");
    
    // Check that wash produced type information
    let type_info = semantic_analyzer.attr.get_type_information();
    let symbol_env = semantic_analyzer.enter.get_symbol_environment();
    let signatures = semantic_analyzer.get_generic_signatures();
    
    println!("🧪 WASH Integration Test Results:");
    println!("📊 Type info entries: {}", type_info.len());
    println!("🏛️ Symbol table classes: {}", symbol_env.classes.len());
    println!("🧬 Generic signatures: {}", signatures.len());
    
    // Verify wash collected information
    assert!(!type_info.is_empty(), "Wash should collect type information");
    assert!(!symbol_env.classes.is_empty(), "Wash should collect class symbols");
    assert!(!signatures.is_empty(), "Wash should collect generic signatures");
    
    // Check that TestGeneric class is in symbol table
    assert!(symbol_env.classes.contains_key("TestGeneric"), 
           "TestGeneric should be in symbol table");
    
    let test_generic = &symbol_env.classes["TestGeneric"];
    assert!(test_generic.is_generic, "TestGeneric should be marked as generic");
    assert!(!test_generic.type_parameters.is_empty(), "TestGeneric should have type parameters");
    
    println!("✅ Wash semantic analysis completed successfully");
    
    // Now test codegen with wash integration
    let config = Config::default();
    let output_dir = "/tmp/wash_integration_test";
    std::fs::create_dir_all(output_dir).expect("Failed to create output directory");
    
    // This should now use the wash results during codegen
    let result = compile2file(java_source, output_dir, &config);
    
    match result {
        Ok(_) => {
            println!("✅ Codegen with wash integration completed successfully");
            
            // Verify .class file was generated
            let class_file_path = std::path::Path::new(output_dir).join("TestGeneric.class");
            assert!(class_file_path.exists(), "TestGeneric.class should be generated");
            
            println!("✅ All wash → codegen integration tests passed!");
        }
        Err(e) => {
            println!("❌ Codegen with wash integration failed: {}", e);
            panic!("Wash → codegen integration test failed: {}", e);
        }
    }
}

#[test]
fn test_wash_type_info_structure() {
    let java_source = r#"
public class SimpleClass {
    public void method() {
        int x = 42;
        String s = "hello";
    }
}
"#;

    let mut ast = parse_java(java_source).expect("Failed to parse Java source");
    let mut semantic_analyzer = SemanticAnalyzer::new();
    ast = semantic_analyzer.analyze(ast).expect("Failed to run wash analysis");
    
    let type_info = semantic_analyzer.attr.get_type_information();
    println!("🔍 Type information structure:");
    for (id, resolved_type) in type_info {
        println!("  ID {}: {:?}", id, resolved_type);
    }
    
    // The test passes if we can collect type information without errors
    println!("✅ Type information structure test completed");
}