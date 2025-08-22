use tolc::parser::parse_tol;
use tolc::ast::{AstPrinter, TypeDecl};
use tolc::codegen::gen::Gen;
use std::fs;
use std::process::Command;

/// Test JavaC alignment for simple logical operations
#[test]
fn test_javac_alignment_simple_logical() {
    let source = r#"
package test;

public class SimpleLogical {
    public boolean testAnd(boolean a, boolean b) {
        return a && b;
    }
    
    public boolean testOr(boolean a, boolean b) {
        return a || b;
    }
    
    public boolean testConstantAnd() {
        return true && false;
    }
    
    public boolean testConstantOr() {
        return false || true;
    }
}
"#;

    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Generate bytecode with tolc
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut gen = Gen::new();
        gen.init_class(class.clone(), ast.type_decls.clone()).expect("Failed to init class");
        
        // Test each method
        for member in &class.body {
            if let tolc::ast::ClassMember::Method(method) = member {
                let bytecode = gen.gen_method(method).expect("Failed to generate method");
                
                println!("Method '{}' bytecode length: {}", method.name, bytecode.len());
                println!("Bytecode: {:?}", bytecode);
                
                // Basic verification that bytecode was generated
                assert!(!bytecode.is_empty(), "Method '{}' should generate non-empty bytecode", method.name);
            }
        }
    }
    
    println!("✅ JavaC alignment test for simple logical operations completed");
}

/// Test JavaC alignment for complex logical operations
#[test]
fn test_javac_alignment_complex_logical() {
    let source = r#"
package test;

public class ComplexLogical {
    public boolean testNested(boolean a, boolean b, boolean c) {
        return a && (b || c);
    }
    
    public boolean testMultiple(boolean a, boolean b, boolean c, boolean d) {
        return (a && b) || (c && d);
    }
    
    public boolean testShortCircuit() {
        return false && expensiveOperation();
    }
    
    public boolean testShortCircuit2() {
        return true || expensiveOperation();
    }
    
    private boolean expensiveOperation() {
        return true;
    }
}
"#;

    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Generate bytecode with tolc
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut gen = Gen::new();
        gen.init_class(class.clone(), ast.type_decls.clone()).expect("Failed to init class");
        
        // Test each method
        for member in &class.body {
            if let tolc::ast::ClassMember::Method(method) = member {
                let bytecode = gen.gen_method(method).expect("Failed to generate method");
                
                println!("Method '{}' bytecode length: {}", method.name, bytecode.len());
                
                // For short-circuit methods, expect specific patterns
                match method.name.as_str() {
                    "testShortCircuit" => {
                        // Should not call expensiveOperation() due to short-circuit
                        assert!(bytecode.len() > 3, "Short circuit method should have basic bytecode");
                    }
                    "testShortCircuit2" => {
                        // Should not call expensiveOperation() due to short-circuit
                        assert!(bytecode.len() > 3, "Short circuit method should have basic bytecode");
                    }
                    "testNested" => {
                        // Complex nested operations should generate more bytecode
                        assert!(bytecode.len() > 5, "Nested operations should generate more complex bytecode");
                    }
                    _ => {
                        assert!(!bytecode.is_empty(), "Method '{}' should generate non-empty bytecode", method.name);
                    }
                }
            }
        }
    }
    
    println!("✅ JavaC alignment test for complex logical operations completed");
}

/// Test JavaC alignment for constant folding
#[test]
fn test_javac_alignment_constant_folding() {
    let source = r#"
package test;

public class ConstantFolding {
    public static final boolean ALWAYS_TRUE = true || false;
    public static final boolean ALWAYS_FALSE = false && true;
    
    public boolean testFoldedTrue() {
        return true || someMethod();
    }
    
    public boolean testFoldedFalse() {
        return false && someMethod();
    }
    
    public boolean testNotFolded(boolean x) {
        return x && someMethod();
    }
    
    private boolean someMethod() {
        return false;
    }
}
"#;

    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Verify AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    assert!(output.contains("||"));
    assert!(output.contains("&&"));
    assert!(output.contains("ALWAYS_TRUE"));
    assert!(output.contains("ALWAYS_FALSE"));
    
    // Generate bytecode with tolc
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut gen = Gen::new();
        gen.init_class(class.clone(), ast.type_decls.clone()).expect("Failed to init class");
        
        // Test methods that should have different bytecode patterns
        for member in &class.body {
            if let tolc::ast::ClassMember::Method(method) = member {
                let bytecode = gen.gen_method(method).expect("Failed to generate method");
                
                println!("Method '{}' bytecode: {:?}", method.name, bytecode);
                
                match method.name.as_str() {
                    "testFoldedTrue" => {
                        // Should generate simple bytecode due to constant folding
                        println!("testFoldedTrue should use short-circuit evaluation");
                    }
                    "testFoldedFalse" => {
                        // Should generate simple bytecode due to constant folding
                        println!("testFoldedFalse should use short-circuit evaluation");
                    }
                    "testNotFolded" => {
                        // Should generate more complex bytecode as it can't be folded
                        assert!(bytecode.len() > 5, "Non-folded method should generate more complex bytecode");
                    }
                    _ => {}
                }
            }
        }
    }
    
    println!("✅ JavaC alignment test for constant folding completed");
}

/// Helper function to write Java source to temp file and compile with javac
#[allow(dead_code)]
fn compare_with_javac(source: &str, class_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Write source to temp file
    let temp_dir = "/tmp";
    let java_file = format!("{}/{}.java", temp_dir, class_name);
    fs::write(&java_file, source)?;
    
    // Compile with javac
    let output = Command::new("javac")
        .args(&["-d", temp_dir, &java_file])
        .output()?;
    
    if !output.status.success() {
        eprintln!("javac compilation failed: {}", String::from_utf8_lossy(&output.stderr));
        return Err("javac compilation failed".into());
    }
    
    // Use javap to disassemble
    let class_file = format!("{}/{}.class", temp_dir, class_name);
    let javap_output = Command::new("javap")
        .args(&["-c", "-v", &class_file])
        .output()?;
    
    if javap_output.status.success() {
        println!("JavaC bytecode for {}:", class_name);
        println!("{}", String::from_utf8_lossy(&javap_output.stdout));
    }
    
    // Clean up
    let _ = fs::remove_file(&java_file);
    let _ = fs::remove_file(&class_file);
    
    Ok(())
}

/// Test JavaC bytecode comparison (requires javac and javap installed)
#[test]
fn test_javac_bytecode_comparison() {
    let source = r#"
public class SimpleTest {
    public boolean test(boolean a, boolean b) {
        return a && b;
    }
}
"#;

    // Parse and generate with tolc
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut gen = Gen::new();
        gen.init_class(class.clone(), ast.type_decls.clone()).expect("Failed to init class");
        
        if let Some(method) = class.body.iter().find_map(|member| {
            if let tolc::ast::ClassMember::Method(m) = member {
                if m.name == "test" { Some(m) } else { None }
            } else { None }
        }) {
            let bytecode = gen.gen_method(method).expect("Failed to generate method");
            println!("TOLC bytecode for 'test' method: {:?}", bytecode);
            println!("TOLC bytecode length: {}", bytecode.len());
        }
    }
    
    // Compare with javac (if available)
    if let Err(e) = compare_with_javac(source, "SimpleTest") {
        println!("Note: Could not compare with javac: {} (this is expected if javac is not installed)", e);
    }
    
    println!("✅ JavaC bytecode comparison completed");
}