use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn instance_target_method_call_propagates_throws_error_without_coverage() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class A { void m() throws IOException {} }
class T { void t(){ A a = new A(); a.m(); } }
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}

#[test]
fn instance_target_method_call_caught_ok() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class A { void m() throws IOException {} }
class T { void t(){ try { new A().m(); } catch(IOException e) {} } }
"#;
    ok(src);
}

#[test]
fn overload_by_signature_throws_selection_matches_string_arg() {
    // Select m(String) which declares Exception; declaring only IOException should fail
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class String {}
class A { void m(int x) throws IOException {} void m(String s) throws Exception {} }
class T { void t() throws IOException { A a = new A(); a.m(new String()); } }
"#;
    err_contains(src, "unreported checked exception 'Exception' thrown");
}

#[test]
fn overload_by_signature_throws_selection_matches_int_arg_ok() {
    // Force selection of m(int) via cast; declaring IOException suffices
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class A { void m(int x) throws IOException {} void m(String s) throws Exception {} }
class T { void t() throws IOException { new A().m((int)1); } }
"#;
    ok(src);
}

#[test]
fn twr_resource_initializer_exception_requires_coverage() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
interface Closeable { }
class R implements Closeable { }
class T {
  static R initR() throws IOException { return new R(); }
  void m(){ try (R r = initR()) { } }
}
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}

#[test]
fn ctor_implicit_super_throws_requires_declaration() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class S { S() throws IOException { } }
class T extends S { T() { } }
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}

#[test]
fn ctor_explicit_super_throws_declare_ok() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class S { S() throws IOException { } }
class T extends S { T() throws IOException { super(); } }
"#;
    ok(src);
}

#[test]
fn ctor_this_delegation_throws_requires_declaration() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class T {
  T() throws IOException { }
  T(int x) { this(); }
}
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}


