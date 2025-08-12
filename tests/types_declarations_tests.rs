use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn duplicate_type_names_in_compilation_unit_errors() {
    let src = r#"
package p;
class A {}
class A {}
"#;
    err_contains(src, "Duplicate type");
}

#[test]
fn interface_cannot_be_final() {
    let src = r#"
package p;
final interface I { void m(); }
"#;
    err_contains(src, "Interface 'I' cannot be final");
}


