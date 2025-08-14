use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn generic_type_argument_annotations_are_parsed() {
    ok(r#"package p; @interface A{} class C<T>{ T f; } class D { C<@A String> c; }"#);
}

#[test]
fn array_dimension_annotations_are_parsed() {
    ok(r#"package p; @interface A{} class C { String @A [] a; int @A [][] b; }"#);
}

#[test]
fn throws_type_annotations_are_parsed() {
    ok(r#"package p; @interface A{} class E extends java.lang.Exception{} class C { void m() throws @A E {} }"#);
}

#[test]
fn catch_type_annotations_are_parsed() {
    ok(r#"package p; @interface A{} class E extends java.lang.Exception{} class C { void m(){ try{}catch(@A E e){} } }"#);
}


