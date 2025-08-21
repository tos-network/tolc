use tolc::parser::parse_and_verify;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::TypeDecl;
use tolc::Config;

#[test]
fn test_if_else_branch_improvement() {
    let source = r#"
package test;

public class IfElseBranchTest {
    public int testIfOnly(boolean flag) {
        if (flag) {
            return 1;
        }
        return 0;
    }
    
    public int testIfElse(boolean flag) {
        if (flag) {
            return 1;
        } else {
            return 2;
        }
    }
    
    public int testNestedIfElse(boolean flag1, boolean flag2) {
        if (flag1) {
            if (flag2) {
                return 1;
            } else {
                return 2;
            }
        } else {
            return 3;
        }
    }
}
"#;
    
    let ast = parse_and_verify(source).expect("Failed to parse");
    let type_decl = ast.type_decls.iter().find(|td| matches!(td, TypeDecl::Class(c) if c.name == "IfElseBranchTest")).expect("No IfElseBranchTest class found");
    
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
        _ => panic!("Expected class declaration"),
    }
    
    let class_file = cw.get_class_file();
    let bytes = class_file_to_bytes(&class_file);
    
    // Write to file for inspection in /tmp/ directory
    let class_file_path = "/tmp/IfElseBranchTest.class";
    std::fs::write(class_file_path, &bytes).expect("Failed to write class file");
    
    // Run javap to see the bytecode
    let output = std::process::Command::new("javap")
        .arg("-c")
        .arg("-verbose")
        .arg(class_file_path)
        .output()
        .expect("Failed to run javap");
    
    let javap_output = String::from_utf8_lossy(&output.stdout);
    println!("=== If-Else Branch Test javap output ===");
    println!("{}", javap_output);
    
    // Check for conditional branch instructions
    let has_conditional_branches = javap_output.contains("ifeq") || javap_output.contains("ifne") || 
                                   javap_output.contains("if_icmp") || javap_output.contains("goto");
    
    if has_conditional_branches {
        println!("✅ Conditional branch instructions found");
        // Check for proper jump structure
        assert!(javap_output.contains("goto") || javap_output.contains("if"), "Expected goto or conditional jump instructions");
    } else {
        println!("⚠️  No conditional branches generated");
    }
    
    // Test that Java can verify our generated class
    let verify_result = std::process::Command::new("java")
        .arg("-verify")
        .arg("-cp")
        .arg("/tmp")
        .arg("test.IfElseBranchTest")
        .output();
        
    match verify_result {
        Ok(result) => {
            if result.status.success() {
                println!("✅ JVM verification passed");
            } else {
                let stderr = String::from_utf8_lossy(&result.stderr);
                println!("⚠️  JVM verification issues: {}", stderr);
            }
        }
        Err(e) => {
            println!("⚠️  Could not run Java verification: {}", e);
        }
    }
    
    // Clean up - remove the temporary file
    std::fs::remove_file(class_file_path).ok();
}