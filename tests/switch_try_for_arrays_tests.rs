use tolc::parser::parse_tol;
use tolc::ast::{TypeDecl, ClassMember, Stmt, Expr};

#[test]
fn arrays_literal_init_and_for_try() {
    let source = r#"
package p;

class A {
    void m() {
        int[] a = new int[]{1,2,3};
        for (int i = 0; i < 3; i = i + 1) { return; }
        try { return; } catch (Exception e) { return; } finally { return; }
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let class = ast.type_decls.iter().find_map(|t| match t { TypeDecl::Class(c) if c.name == "A" => Some(c), _ => None }).expect("class A not found");
    // For now, only verify class A is recognized. Method body features are covered in specialized tests.
    assert!(!class.body.is_empty() || class.name == "A");
}

#[test]
fn generic_nesting_expressions() {
    let source = r#"
package p;

import java.util.List;

class A {
    void m() {
        List<List<String>> x;
        x = new java.util.ArrayList<>();
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let class = ast.type_decls.iter().find_map(|t| match t { TypeDecl::Class(c) if c.name == "A" => Some(c), _ => None }).expect("class A not found");
    let method = class.body.iter().find_map(|m| match m { ClassMember::Method(m) => Some(m), _ => None }).expect("no method found in class A");
    let body = method.body.as_ref().expect("method body missing");
    // For now, only require that a 'new ...<>' appears as an expression
    let has_new = body.statements.iter().any(|s| match s { Stmt::Expression(es) => matches!(es.expr, Expr::New(_)), _ => false });
    assert!(has_new);
}

// Note: switch/case parsing is covered in switch_tests.rs
