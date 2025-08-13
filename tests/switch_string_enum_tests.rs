use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn string_switch_constant_selector_picks_single_path() {
    // Constant selector with string literal chooses only matching case fallthrough
    ok(r#"package p; class C { int m(){ int x; switch("b"){ case "a": x=1; break; case "b": x=2; break; default: x=3; } return x; } }"#);
}

#[test]
fn string_switch_duplicate_labels_rejected() {
    err_contains(r#"package p; class C { void m(String s){ switch(s){ case "a": break; case "a": break; default: break; } } }"#, "duplicate switch case label");
}

#[test]
fn enum_switch_constant_selector_picks_single_path() {
    ok(r#"package p; enum E { A, B } class C { int m(){ int x; switch(E.B){ case A: x=1; break; case B: x=2; break; } return x; } }"#);
}

#[test]
fn enum_switch_duplicate_labels_rejected() {
    err_contains(r#"package p; enum E { A, B } class C { void m(E e){ switch(e){ case A: break; case A: break; default: break; } } }"#, "duplicate switch case label");
}

#[test]
fn enum_switch_exhaustive_without_default_establishes_no_nomatch_path() {
    // All enum constants listed; DA should rely only on case paths. Here each case assigns and breaks, so DA holds.
    ok(r#"package p; enum E { A, B } class C { int m(E e){ int x; switch(e){ case A: x=1; break; case B: x=2; break; } return x; } }"#);
}

#[test]
fn enum_switch_non_exhaustive_without_default_includes_nomatch_path() {
    // Missing one constant and no default: DA fails when returning x
    err_contains(r#"package p; enum E { A, B } class C { int m(E e){ int x; switch(e){ case A: x=1; break; } return x; } }"#, "use of local variable");
}


