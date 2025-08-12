use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn cannot_reduce_visibility_on_override() {
    let src = r#"
package p;
class S { public void m() {} }
class T extends S { void m() {} }
"#;
    err_contains(src, "override reduces visibility");
}

#[test]
fn interface_method_must_be_public() {
    let src = r#"
package p;
interface I { void m(); }
class T implements I { void m() {} }
"#;
    err_contains(src, "must be public to implement interface method");
}


