use tolc::parser::parse_tol;
use tolc::ast::AstPrinter;

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
    let ast = parse_tol(source).expect("Failed to parse");
    let mut p = AstPrinter::new();
    let out = p.print(&ast);
    assert!(out.contains("class A"));
}
