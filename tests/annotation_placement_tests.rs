use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn annotations_allowed_on_class_field_method_param() {
    // Parser expects type annotations right after keyword and before name
    let src = r#"package p;
@interface A {}
@interface B {}
class @B C {
  @A int f;
  @A C() {}
  void m(@A int x) {}
}
"#;
    ok(src);
}

#[test]
fn repeated_annotation_on_field_is_rejected() {
    let src = r#"package p;
@interface A {}
class C {
  @A @A int f;
}
"#;
    err_contains(src, "repeated annotation");
}


