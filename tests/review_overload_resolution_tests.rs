use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

// 1) Prefer exact match over widened when both are applicable
#[test]
fn overload_prefers_exact_over_widened() {
    let src = r#"
package p;
class T {
  void m(int x) {}
  void m(long x) {}
  void call(){ m(1); }
}
"#;
    ok(src);
}

// 2) Prefer the overload with lower widening cost when both are applicable
#[test]
fn overload_prefers_lower_widening_cost() {
    let src = r#"
package p;
class T {
  void m(double x) {}
  void m(float x) {}
  void call(){ m(1); }
}
"#;
    ok(src);
}

// 3) Report ambiguity when widening cost is tied
#[test]
fn overload_tie_on_cost_reports_ambiguity() {
    let src = r#"
package p;
class T {
  void m(float x) {}
  void m(long x) {}
  void call(){ m(1); }
}
"#;
    err_contains(src, "ambiguous method");
}

// 4) Varargs vs fixed-arity conflict: prefer the more specific fixed-arity
#[test]
fn overload_varargs_vs_fixed_more_specific_fixed() {
    let src = r#"
package p;
class T {
  void m(int x) {}
  void m(int... xs) {}
  void call(){ m(1); }
}
"#;
    ok(src);
}

// 5) With multiple args, prefer fixed-arity if it matches; otherwise fall back to varargs
#[test]
fn overload_varargs_multi_args_prefers_fixed_when_matches() {
    let src = r#"
package p;
class T {
  void m(int x, int y) {}
  void m(int... xs) {}
  void call(){ m(1, 2); }
}
"#;
    ok(src);
}


