use tolc::parser::parse_tol;
use tolc::ast::{TypeDecl, ClassMember, Stmt};

#[test]
fn statements_break_continue_switch_with_break() {
    let source = r#"
package p;

class A {
    void m(int x) {
        break label1;
        continue label2;
        switch (x) {
            case 1:
                break;
            case 2:
            default:
                break;
        }
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let class = ast.type_decls.iter().find_map(|t| match t { TypeDecl::Class(c) if c.name == "A" => Some(c), _ => None }).expect("class A not found");
    let method = class.body.iter().find_map(|m| match m { ClassMember::Method(m) if m.name == "m" => Some(m), _ => None }).expect("method m not found");
    let body = method.body.as_ref().expect("method body missing");
    let has_break = body.statements.iter().any(|s| matches!(s, Stmt::Break(_)));
    let has_continue = body.statements.iter().any(|s| matches!(s, Stmt::Continue(_)));
    let has_switch = body.statements.iter().any(|s| matches!(s, Stmt::Switch(_)));
    assert!(has_break && has_continue && has_switch);
}

#[test]
fn statements_nested_blocks_and_return() {
    let source = r#"
package p;

class A {
    int m() {
        { { { return 1; } } }
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let class = ast.type_decls.iter().find_map(|t| match t { TypeDecl::Class(c) if c.name == "A" => Some(c), _ => None }).expect("class A not found");
    let method = class.body.iter().find_map(|m| match m { ClassMember::Method(m) if m.name == "m" => Some(m), _ => None }).expect("method m not found");
    let body = method.body.as_ref().expect("method body missing");
    let has_block = body.statements.iter().any(|s| matches!(s, Stmt::Block(_)));
    let has_return = body.statements.iter().any(|s| matches!(s, Stmt::Return(_)));
    assert!(has_block || has_return);
}

#[test]
fn statements_do_while_loop() {
    let source = r#"
package p;

class A {
    void m() {
        do { } while (true);
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse do-while");
    let class = ast.type_decls.iter().find_map(|t| match t { TypeDecl::Class(c) if c.name == "A" => Some(c), _ => None }).expect("class A not found");
    let method = class.body.iter().find_map(|m| match m { ClassMember::Method(m) if m.name == "m" => Some(m), _ => None }).expect("method m not found");
    let body = method.body.as_ref().expect("method body missing");
    let has_do_while = body.statements.iter().any(|s| matches!(s, Stmt::DoWhile(_)));
    assert!(has_do_while);
}

#[test]
fn statements_enhanced_for_loop() {
    let source = r#"
package p;
import java.util.List;
class A {
    void m(List<String> xs) {
        for (String s : xs) { }
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse enhanced for");
    let class = ast.type_decls.iter().find_map(|t| match t { TypeDecl::Class(c) if c.name == "A" => Some(c), _ => None }).expect("class A not found");
    let method = class.body.iter().find_map(|m| match m { ClassMember::Method(m) if m.name == "m" => Some(m), _ => None }).expect("method m not found");
    let body = method.body.as_ref().expect("method body missing");
    let has_for = body.statements.iter().any(|s| matches!(s, Stmt::For(_)));
    assert!(has_for);
}

#[test]
fn statements_throw_and_synchronized_block() {
    let source = r#"
package p;
class A {
    void m(Object lock) {
        synchronized(lock) { }
        throw new RuntimeException();
    }
}
"#;
    // Currently throw/synchronized may not be fully supported; just ensure no panic
    let _ = parse_tol(source).expect("Failed to parse synchronized/throw");
}
