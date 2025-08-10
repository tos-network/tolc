use tolc::parser::parse_tol;

// These tests ensure the parser doesn't hang or panic on malformed inputs.
// We only assert that parsing returns Ok (parser recovers and produces a partial AST),
// not that the AST is complete.

#[test]
fn error_unclosed_block() {
    let source = r#"
package p;
class A {
    void m(int x) {
        if (x > 0) {
            x = x + 1;
        // missing closing braces for method and/or class
"#;
    let _ = parse_tol(source).expect("parser should recover and not hang on unclosed block");
}

#[test]
fn error_switch_missing_colon() {
    let source = r#"
package p;
class A {
    void m(int x) {
        switch (x) {
            case 1
                return;
            default:
                return;
        }
    }
}
"#;
    let _ = parse_tol(source).expect("parser should recover from missing ':' after case");
}

#[test]
fn error_return_missing_semicolon_in_switch() {
    let source = r#"
package p;
class A {
    void m(int x) {
        switch (x) {
            case 1:
                return
            default:
                return;
        }
    }
}
"#;
    let _ = parse_tol(source).expect("parser should recover from missing ';' after return");
}

#[test]
fn error_method_header_unmatched_paren() {
    let source = r#"
package p;
class A {
    void m(int x {
        return;
    }
}
"#;
    let _ = parse_tol(source).expect("parser should recover from unmatched '(' in method header");
}
