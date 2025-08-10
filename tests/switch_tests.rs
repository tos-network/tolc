use tolc::parser::parse_tol;
use tolc::ast::{TypeDecl, ClassMember, Stmt};

#[test]
fn switch_with_cases_and_default() {
    let source = r#"
package p;

class A {
    void m(int x) {
        switch (x) {
            case 1:
            case 2:
                return;
            default:
                return;
        }
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    // Find class A -> method m -> body contains a Switch stmt
    let class = ast.type_decls.iter().find_map(|t| match t {
        TypeDecl::Class(c) if c.name == "A" => Some(c),
        _ => None,
    }).expect("class A not found");

    let method = class.body.iter().find_map(|m| match m {
        ClassMember::Method(m) if m.name == "m" => Some(m),
        _ => None,
    }).expect("method m not found");

    let body = method.body.as_ref().expect("method body missing");
    let has_switch = body.statements.iter().any(|s| matches!(s, Stmt::Switch(_)));
    assert!(has_switch, "expected a switch statement in method body");
}
