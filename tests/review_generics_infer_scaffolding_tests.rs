use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }

// Placeholder tests to ensure parser/review pipeline tolerates explicit method type args syntax (when added later)
// Currently these remain simple and do not assert inference; they only ensure no regressions in our subset.

#[test]
fn method_invocation_with_simple_generic_signature_is_ok() {
    ok(r#"package p; class U { <T> T id(T t){ return t; } void g(){ Integer x = id(1); } }"#);
}


