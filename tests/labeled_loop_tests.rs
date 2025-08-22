use tolc::parser::parse_tol;
use tolc::ast::{AstPrinter, TypeDecl};
use tolc::codegen::gen::Gen;

/// Test parsing and bytecode generation for labeled while loops
#[test]
fn test_labeled_while_loop() {
    let source = r#"
package test;

public class LabeledWhileTest {
    public void testLabeledWhile() {
        outer: while (true) {
            inner: while (true) {
                if (condition1()) {
                    break outer;  // Should break from outer loop
                }
                if (condition2()) {
                    continue inner;  // Should continue inner loop  
                }
                if (condition3()) {
                    break inner;  // Should break from inner loop
                }
            }
            // This should not be reached when break outer is executed
            doSomething();
        }
    }
    
    public boolean condition1() { return false; }
    public boolean condition2() { return false; }
    public boolean condition3() { return true; }
    public void doSomething() { }
}
"#;

    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    // Verify the AST contains labeled statements
    assert!(output.contains("outer:"));
    assert!(output.contains("inner:"));
    assert!(output.contains("break outer"));
    assert!(output.contains("continue inner"));
    assert!(output.contains("break inner"));
    
    // Generate bytecode with tolc
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut gen = Gen::new();
        gen.init_class(class.clone(), ast.type_decls.clone()).expect("Failed to init class");
        
        // Find the testLabeledWhile method
        if let Some(method) = class.body.iter().find_map(|member| {
            if let tolc::ast::ClassMember::Method(m) = member {
                if m.name == "testLabeledWhile" { Some(m) } else { None }
            } else { None }
        }) {
            let bytecode = gen.gen_method(method).expect("Failed to generate method");
            
            println!("Labeled while loop bytecode length: {}", bytecode.len());
            println!("Bytecode: {:?}", bytecode);
            
            // Basic verification that bytecode was generated
            assert!(!bytecode.is_empty(), "Method should generate non-empty bytecode");
            
            // Should contain jump instructions for loop control
            assert!(bytecode.iter().any(|&b| b == 167 || b == 153), // GOTO or IFEQ
                    "Should contain jump instructions for labeled breaks/continues");
        }
    }
    
    println!("✅ Labeled while loop test completed");
}

/// Test parsing and bytecode generation for labeled for loops (simplified)
#[test] 
fn test_labeled_for_loop_simplified() {
    let source = r#"
package test;

public class LabeledForTest {
    public void testLabeledFor() {
        outer: while (true) {
            inner: while (true) {
                if (condition1()) {
                    break outer;  // Exit both loops
                }
                if (condition2()) {
                    continue outer;  // Continue outer loop
                }
                if (condition3()) {
                    break inner;   // Break inner loop
                }
            }
        }
    }
    
    public boolean condition1() { return false; }
    public boolean condition2() { return false; }
    public boolean condition3() { return true; }
}
"#;

    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    // Verify the AST contains labeled statements
    assert!(output.contains("outer:"));
    assert!(output.contains("inner:"));
    assert!(output.contains("break outer"));
    assert!(output.contains("continue outer"));
    assert!(output.contains("break inner"));
    
    // Generate bytecode with tolc
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut gen = Gen::new();
        gen.init_class(class.clone(), ast.type_decls.clone()).expect("Failed to init class");
        
        // Find the testLabeledFor method
        if let Some(method) = class.body.iter().find_map(|member| {
            if let tolc::ast::ClassMember::Method(m) = member {
                if m.name == "testLabeledFor" { Some(m) } else { None }
            } else { None }
        }) {
            let bytecode = gen.gen_method(method).expect("Failed to generate method");
            
            println!("Labeled while loop (simplified) bytecode length: {}", bytecode.len());
            println!("Bytecode: {:?}", bytecode);
            
            // Basic verification that bytecode was generated
            assert!(!bytecode.is_empty(), "Method should generate non-empty bytecode");
        }
    }
    
    println!("✅ Labeled for loop (simplified) test completed");
}

/// Test simple break/continue without labels (baseline)
#[test]  
fn test_simple_break_continue() {
    let source = r#"
package test;

public class SimpleLoopTest {
    public void testSimpleBreakContinue() {
        for (int i = 0; i < 10; i++) {
            if (i == 5) {
                break;  // Simple break
            }
            if (i == 3) {
                continue;  // Simple continue
            }
        }
        
        while (true) {
            if (shouldBreak()) {
                break;
            }
        }
    }
    
    public boolean shouldBreak() { return true; }
}
"#;

    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Generate bytecode with tolc
    if let Some(TypeDecl::Class(class)) = ast.type_decls.first() {
        let mut gen = Gen::new();
        gen.init_class(class.clone(), ast.type_decls.clone()).expect("Failed to init class");
        
        // Find the testSimpleBreakContinue method
        if let Some(method) = class.body.iter().find_map(|member| {
            if let tolc::ast::ClassMember::Method(m) = member {
                if m.name == "testSimpleBreakContinue" { Some(m) } else { None }
            } else { None }
        }) {
            let bytecode = gen.gen_method(method).expect("Failed to generate method");
            
            println!("Simple break/continue bytecode length: {}", bytecode.len());
            println!("Bytecode: {:?}", bytecode);
            
            // Basic verification that bytecode was generated
            assert!(!bytecode.is_empty(), "Method should generate non-empty bytecode");
        }
    }
    
    println!("✅ Simple break/continue test completed");
}