use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

// Negative: extends mismatch
#[test]
fn list_extends_number_rejects_list_string() {
    err_contains(
        r#"package p; class T { void f(){ java.util.List<? extends Number> a=null; java.util.List<String> b=null; a=b; } }"#,
        "Incompatible initializer",
    );
}

// Negative: super mismatch
#[test]
fn list_super_number_rejects_list_integer() {
    err_contains(
        r#"package p; class T { void f(){ java.util.List<? super Number> a=null; java.util.List<Integer> b=null; a=b; } }"#,
        "Incompatible initializer",
    );
}

// Wildcards not allowed in new expressions
#[test]
fn wildcard_in_new_is_rejected() {
    err_contains(
        r#"package p; class T { void f(){ java.util.List<? extends Number> a = new java.util.ArrayList<? extends Number>(); } }"#,
        "wildcard type arguments are not allowed in object instantiation",
    );
}

// Cast with nested wildcard should be accepted in our assignability model
#[test]
fn cast_to_nested_extends_is_ok() {
    ok(r#"package p; class T { @SuppressWarnings("unchecked") void f(){ java.util.List<Integer> b=null; java.util.List<? extends Number> a = (java.util.List<? extends Number>) (Object) b; } }"#);
}

