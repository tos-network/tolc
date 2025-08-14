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

#[test]
fn static_final_mixed_numeric_widening_folds() {
    ok(r#"package p; class C { static final double D = 1 + 2.5; double x(){ return D; } }"#);
}

#[test]
fn static_final_boolean_and_or_xor_fold() {
    ok(r#"package p; class C { static final boolean B = (true & false) ^ (true | false); boolean x(){ return B; } }"#);
}

#[test]
fn static_final_shift_masking_int_behavior() {
    ok(r#"package p; class C { static final int I = 1 << 33; int x(){ return I; } }"#);
}

#[test]
fn static_final_relational_comparisons_fold() {
    ok(r#"package p; class C { static final boolean B1 = 3 < 4; static final boolean B2 = 5.0 >= 2; static final boolean B3 = (0.0/0.0)==(0.0/0.0); static final boolean B4 = (1.0/0.0) > 9999999.0; }"#);
}

#[test]
fn static_final_string_concat_broadened() {
    ok(r#"package p; class C { static final String S1 = 1 + "a"; static final String S2 = true + "b"; static final String S3 = 'Z' + "!"; static final String S4 = (1+2) + ("x"+3); }"#);
}

#[test]
fn static_final_long_shift_masking_behavior() {
    ok(r#"package p; class C { static final int I = ((int)((long)1 << 65)); static final int J = (int)( ( (9223372036854775807L) >> 70) ); }"#);
}

#[test]
fn static_final_long_bitwise_urshift_and_mask() {
    ok(r#"package p; class C { static final int U = (int)(((long)-1) >>> 40); int x(){ return U; } }"#);
}

#[test]
fn static_final_string_concat_more_shapes() {
    ok(r#"package p; class C { static final String S = ("a" + 'b') + 3 + false; }"#);
}

#[test]
fn static_final_long_bitwise_parity_fold() {
    ok(r#"package p; class C { static final int A = (int)(((long)1) & 3); static final int B = (int)(((long)1) | 2); static final int Cc = (int)(((long)1) ^ 3); }"#);
}

#[test]
fn static_final_string_concat_nested_parens_shapes() {
    ok(r#"package p; class C { static final String S = (("x"+"y") + (1+2)) + (true + "z"); }"#);
}


