use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{}", e); }

// If two interfaces provide conflicting defaults for the same method, the class must override
#[test]
fn conflicting_defaults_require_override() {
    let src = r#"
    package p;
    interface I { void m() {} }
    interface J { void m() {} }
    class C implements I, J { }
    "#;
    err_contains(src, "conflicting default methods");
}

// If class overrides, it's fine
#[test]
fn conflicting_defaults_resolved_by_class_override_ok() {
    let src = r#"
    package p;
    interface I { void m() {} }
    interface J { void m() {} }
    class C implements I, J { public void m() {} }
    "#;
    ok(src);
}

// Abstract interface method must be implemented by class (unless class is abstract)
#[test]
fn abstract_interface_method_must_be_implemented() {
    let src = r#"
    package p;
    interface I { void m(); }
    class C implements I { }
    "#;
    err_contains(src, "must implement interface method");
}

// If class provides implementation, OK
#[test]
fn abstract_interface_method_implemented_ok() {
    let src = r#"
    package p;
    interface I { void m(); }
    class C implements I { public void m() {} }
    "#;
    ok(src);
}


