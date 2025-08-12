use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn non_abstract_method_must_return() {
    let src = r#"
package p;
class T { int m(){ if (true) { return 1; } else { return 2; } } }
"#;
    ok(src);
}

#[test]
fn abstract_and_final_class_is_rejected() {
    let src = r#"
package p;
abstract final class T {}
"#;
    err_contains(src, "Class 'T' cannot be both abstract and final");
}


