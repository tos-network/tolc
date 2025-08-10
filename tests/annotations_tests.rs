use tolc::parser::parse_tol;
use tolc::ast::AstPrinter;

#[test]
fn annotations_on_members_and_params() {
    let source = r#"
package p;

class C {
    @Deprecated
    private int x;

    @SuppressWarnings("x")
    <T> void m(@Deprecated T t) { return; }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut p = AstPrinter::new();
    let out = p.print(&ast);
    assert!(out.contains("class C"));
}

#[test]
fn annotation_declaration_print() {
    let source = r#"
package p;

public @interface Ann {
    int value();
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut p = AstPrinter::new();
    let out = p.print(&ast);
    assert!(out.contains("@interface Ann"));
}
