use tolc::parser::parse_tol;
use tolc::ast::{AstPrinter, TypeDecl};

/// Helper function to test loop control flow parsing
fn test_loop_parsing(source: &str, expected_keywords: &[&str]) {
    let ast = parse_tol(source).expect("Failed to parse source code");
    
    // Test AST parsing
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    
    for keyword in expected_keywords {
        assert!(output.contains(keyword), "Output should contain '{}' keyword", keyword);
    }
}

#[test]
fn control_flow_if_else_while_return() {
    let source = r#"
package p;

class A {
    void m() {
        if (1 < 2) { return; } else { return; }
        while (1 > 0) { return; }
    }
}
"#;
    test_loop_parsing(source, &["class", "while", "return"]);
}

/// Test Phase 2.3: Basic while loop with break statement
#[test]
fn test_while_loop_with_break() {
    let source = r#"
package test;

class TestWhileBreak {
    void test() {
        int i = 0;
        while (i < 10) {
            if (i == 5) {
                break;
            }
            i = i + 1;
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "while", "break"]);
}

/// Test Phase 2.3: For loop with continue statement
#[test]
fn test_for_loop_with_continue() {
    let source = r#"
package test;

class TestForContinue {
    void test() {
        for (int i = 0; i < 10; i = i + 1) {
            if (i % 2 == 0) {
                continue;
            }
            // Process odd numbers
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "for", "continue"]);
}

/// Test Phase 2.3: Nested loops with break and continue
#[test]
fn test_nested_loops_control_flow() {
    let source = r#"
package test;

class TestNestedLoops {
    void test() {
        for (int i = 0; i < 5; i = i + 1) {
            while (true) {
                if (i == 3) {
                    break;  // Should break inner while loop
                }
                if (i == 1) {
                    continue;  // Should continue inner while loop
                }
                break;  // Exit inner loop normally
            }
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "for", "while", "break", "continue"]);
}

/// Test Phase 2.3: Multiple break conditions in same loop
#[test]
fn test_multiple_breaks_in_loop() {
    let source = r#"
package test;

class TestMultipleBreaks {
    void test() {
        int i = 0;
        while (i < 100) {
            if (i == 10) {
                break;
            }
            if (i == 20) {
                break;
            }
            if (i > 50) {
                break;
            }
            i = i + 1;
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "while", "break"]);
}

/// Test Phase 2.3: Multiple continue statements
#[test]
fn test_multiple_continues_in_loop() {
    let source = r#"
package test;

class TestMultipleContinues {
    void test() {
        for (int i = 0; i < 20; i = i + 1) {
            if (i < 5) {
                continue;
            }
            if (i % 3 == 0) {
                continue;
            }
            if (i > 15) {
                continue;
            }
            // Process i
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "for", "continue"]);
}

/// Test Phase 2.3: Loop with both break and continue
#[test]
fn test_loop_break_and_continue_mixed() {
    let source = r#"
package test;

class TestBreakAndContinue {
    void test() {
        int i = 0;
        while (i < 50) {
            if (i < 10) {
                i = i + 1;
                continue;
            }
            if (i > 40) {
                break;
            }
            if (i == 25) {
                i = i + 5;
                continue;
            }
            i = i + 1;
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "while", "break", "continue"]);
}

/// Test Phase 2.3: Immediate break in loop
#[test]
fn test_immediate_break_in_loop() {
    let source = r#"
package test;

class TestImmediateBreak {
    void test() {
        while (true) {
            break;
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "while", "break"]);
}

/// Test Phase 2.3: Loop with unreachable code after break
#[test]
fn test_unreachable_after_break() {
    let source = r#"
package test;

class TestUnreachableAfterBreak {
    void test() {
        while (true) {
            break;
            // This code is unreachable but should still compile
            int x = 42;
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "while", "break"]);
}

/// Test Phase 2.3: Complex loop structure with JavaC alignment
#[test]
fn test_javac_aligned_loop_structure() {
    let source = r#"
package test;

class TestJavacAlignment {
    void complexLoopStructure() {
        for (int i = 0; i < 10; i = i + 1) {
            if (i == 5) {
                break;
            }
            if (i % 2 == 1) {
                continue;
            }
            
            while (i < 8) {
                if (i == 7) {
                    break;
                }
                i = i + 1;
            }
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "for", "while", "break", "continue"]);
}

/// Test Phase 2.3: Chain resolution with proper PC tracking
#[test]
fn test_chain_resolution_pc_tracking() {
    let source = r#"
package test;

class TestChainResolution {
    void test() {
        int i = 0;
        while (i < 100) {
            if (i == 10) break;
            if (i == 20) break;
            if (i == 30) break;
            
            if (i % 2 == 0) continue;
            if (i % 3 == 0) continue;
            if (i % 5 == 0) continue;
            
            i = i + 1;
        }
    }
}
"#;
    test_loop_parsing(source, &["class", "while", "break", "continue"]);
}

/// Test do-while loop basic functionality
#[test]
fn test_do_while_basic() {
    let source = r#"
package test;

class TestDoWhile {
    void test() {
        int i = 0;
        do {
            i = i + 1;
        } while (i < 10);
    }
}
"#;
    test_loop_parsing(source, &["class", "do", "while"]);
}

/// Test do-while loop with break and continue
#[test]
fn test_do_while_with_break_continue() {
    let source = r#"
package test;

class TestDoWhileBreakContinue {
    void test() {
        int i = 0;
        do {
            i = i + 1;
            if (i == 5) {
                break;
            }
            if (i % 2 == 0) {
                continue;
            }
            // Process odd numbers except 5
        } while (i < 20);
    }
}
"#;
    test_loop_parsing(source, &["class", "do", "while", "break", "continue"]);
}

/// Test nested do-while loops
#[test]
fn test_nested_do_while() {
    let source = r#"
package test;

class TestNestedDoWhile {
    void test() {
        int i = 0;
        do {
            int j = 0;
            do {
                j = j + 1;
                if (j > 3) break;
            } while (j < 10);
            i = i + 1;
        } while (i < 5);
    }
}
"#;
    test_loop_parsing(source, &["class", "do", "while", "break"]);
}

/// Test do-while with single statement body (no braces)
#[test]
fn test_do_while_single_statement() {
    let source = r#"
package test;

class TestDoWhileSingle {
    void test() {
        int i = 0;
        do
            i = i + 1;
        while (i < 5);
    }
}
"#;
    test_loop_parsing(source, &["class", "do", "while"]);
}
