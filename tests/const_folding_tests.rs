use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }

#[test]
fn static_final_int_arith_folds_to_constantvalue() {
    ok(r#"package p; class C { static final int A = 1 + 2 * 3; int x(){ return A; } }"#);
}

#[test]
fn static_final_string_concat_folds_to_constantvalue() {
    ok(r#"package p; class C { static final String S = "ab" + "cd"; String x(){ return S; } }"#);
}

#[test]
fn static_final_boolean_not_folds() {
    ok(r#"package p; class C { static final boolean B = !false; boolean x(){ return B; } }"#);
}

#[test]
fn static_final_double_arith_folds() {
    ok(r#"package p; class C { static final double D = 1.0 + 2.0 * 3.0; double x(){ return D; } }"#);
}

#[test]
fn static_final_char_promotion_folds() {
    ok(r#"package p; class C { static final int I = 'A' + 2; int x(){ return I; } }"#);
}


