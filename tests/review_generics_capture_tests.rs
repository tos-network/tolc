use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn list_extends_number_accepts_list_integer_assignment_ok() {
    ok(r#"package p; class T { void f(java.util.List<? extends Number> a){ java.util.List<Integer> b=null; a=b; } }"#);
}

#[test]
fn list_super_integer_accepts_list_number_assignment_ok() {
    ok(r#"package p; class T { void f(java.util.List<? super Integer> a){ java.util.List<Number> b=null; a=b; } }"#);
}

#[test]
fn list_of_string_not_assignable_to_list_of_object_error() {
    // Enforce invariance when no wildcard present
    err_contains(
        r#"package p; class T { void f(){ java.util.List<Object> a=null; java.util.List<String> b=null; a=b; } }"#,
        "Incompatible initializer",
    );
}

#[test]
fn list_string_to_extends_object_ok() {
    ok(r#"package p; class T { void f(java.util.List<? extends Object> a){ java.util.List<String> b=null; a=b; } }"#);
}

// Negative: invariance with arrays and raw types edge cases
#[test]
fn array_list_string_not_assignable_to_array_list_object_error() {
    err_contains(
        r#"package p; class T { void f(){ java.util.ArrayList<Object>[] a=null; java.util.ArrayList<String>[] b=null; a=b; } }"#,
        "Incompatible initializer",
    );
}


