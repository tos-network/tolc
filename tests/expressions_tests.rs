use tolc::parser::parse_tol;
use tolc::ast::AstPrinter;

#[test]
fn expressions_binary_unary_assignment() {
    let source = r#"
package p;

class A {
    void m() {
        int x; x = 1 + 2 * 3 - (4 / 2);
        x = -x;
        x = !x;
        this.f = x;
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut p = AstPrinter::new();
    let out = p.print(&ast);
    assert!(out.contains("class A"));
}

#[test]
fn expressions_calls_and_field_access() {
    let source = r#"
package p;

class A {
    void m() {
        System.out.println("X");
        foo(1, 2);
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut p = AstPrinter::new();
    let out = p.print(&ast);
    assert!(out.contains("class A"));
}
