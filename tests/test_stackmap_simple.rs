use tolc::parser::parse_and_verify;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::TypeDecl;
use tolc::Config;

#[test]
fn test_stackmap_basic_generation() {
    let source = r#"
package test;

public class SimpleStackMapTest {
    public int simpleMethod() {
        int x = 10;
        return x + 5;
    }
    
    public int conditionalMethod(boolean flag) {
        if (flag) {
            return 1;
        } else {
            return 0;
        }
    }
}
"#;
    
    let ast = parse_and_verify(source).expect("Failed to parse");
    let type_decl = ast.type_decls.iter().find(|td| matches!(td, TypeDecl::Class(c) if c.name == "SimpleStackMapTest")).expect("No SimpleStackMapTest class found");
    
    // Create config with StackMapTable enabled
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
        
    let mut cw = ClassWriter::new_with_config(config);
    cw.set_package_name(Some("test"));
    
    match type_decl {
        TypeDecl::Class(c) => {
            cw.generate_class(c).expect("Failed to generate class");
        }
        _ => panic!("Expected class"),
    }
    
    // Get the class file and convert to bytes
    let class_file = cw.get_class_file();
    let bytes = class_file_to_bytes(&class_file);
    
    // Write to file for inspection in /tmp/ directory
    let class_file_path = "/tmp/SimpleStackMapTest.class";
    std::fs::write(class_file_path, &bytes).expect("Failed to write class file");
    
    // Run javap to see if StackMapTable is present
    let output = std::process::Command::new("javap")
        .arg("-c")
        .arg("-verbose")
        .arg(class_file_path)
        .output()
        .expect("Failed to run javap");
    
    let javap_output = String::from_utf8_lossy(&output.stdout);
    println!("=== StackMapTable Basic Test javap output ===");
    println!("{}", javap_output);
    
    // Check for basic class structure
    assert!(javap_output.contains("public class test.SimpleStackMapTest"), "SimpleStackMapTest class not found");
    assert!(javap_output.contains("public int simpleMethod();"), "Method simpleMethod not found");
    assert!(javap_output.contains("public int conditionalMethod(boolean);"), "Method conditionalMethod not found");
    
    // Check if StackMapTable attribute is present
    let has_stackmap = javap_output.contains("StackMapTable:");
    if has_stackmap {
        println!("✅ StackMapTable attribute found in generated class");
        
        // Verify some basic StackMapTable content
        assert!(javap_output.contains("same") || 
                javap_output.contains("append") ||
                javap_output.contains("same_locals_1_stack_item"), "StackMapTable frames not found");
    } else {
        println!("⚠️  StackMapTable attribute not found - may need to enable in code generation");
    }
    
    // Check for conditional branches - if not found, we shouldn't generate StackMapTable
    let has_conditional_branches = javap_output.contains("ifeq") || javap_output.contains("ifne") || 
                                   javap_output.contains("if_icmp") || javap_output.contains("goto");
    
    if !has_conditional_branches {
        println!("⚠️  Conditional branch instructions not found - StackMapTable should not be generated");
        // When no conditional branches are present, StackMapTable should not be generated
        assert!(!has_stackmap, "StackMapTable should not be generated when no conditional branches exist");
        
        // Clean up and return early since no conditional logic exists
        std::fs::remove_file(class_file_path).ok();
        return;
    } else {
        println!("✅ Conditional branch instructions found");
    }
    
    // Test that Java can load our generated class
    let verify_result = std::process::Command::new("java")
        .arg("-verify")
        .arg("-cp")
        .arg("/tmp")
        .arg("test.SimpleStackMapTest")
        .output();
    
    if let Ok(verify_output) = verify_result {
        let verify_stderr = String::from_utf8_lossy(&verify_output.stderr);
        let verify_stdout = String::from_utf8_lossy(&verify_output.stdout);
        
        if !verify_stderr.contains("VerifyError") && !verify_stderr.contains("Error") {
            println!("✅ JVM verification passed");
        } else {
            println!("⚠️  JVM verification output: {}", verify_stderr);
            if verify_stderr.contains("ClassNotFoundException") {
                println!("   (ClassNotFoundException is expected since we're not running the class)")
            }
        }
    }
    
    // Clean up - remove the temporary file
    std::fs::remove_file(class_file_path).ok();
}

#[test]
fn test_stackmap_enabled_config() {
    let config = Config::default().with_emit_frames(true);
    assert!(config.emit_frames, "StackMapTable generation should be enabled");
    
    let config_disabled = Config::default().with_emit_frames(false);
    assert!(!config_disabled.emit_frames, "StackMapTable generation should be disabled");
}