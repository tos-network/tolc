use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn throw_checked_exception_without_throws_or_catch_errors() {
    // Define a checked exception E (not RuntimeException/Error) and throw it
    let src = r#"
package p;
class Exception {}
class E extends Exception {}
class T { void m(){ throw new E(); } }
"#;
    err_contains(src, "unreported checked exception 'E' thrown");
}

#[test]
fn throw_runtime_exception_without_throws_ok() {
    // RuntimeException is treated as unchecked
    let src = r#"
package p;
class Exception {}
class RuntimeException extends Exception {}
class T { void m(){ throw new RuntimeException(); } }
"#;
    ok(src);
}

#[test]
fn catch_exception_covers_ioexception_ok() {
    // Catching Exception should cover IOException
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class T { void m(){ try { throw new IOException(); } catch(Exception e) { } } }
"#;
    ok(src);
}

#[test]
fn declares_throws_covers_throw_ok() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class T { void m() throws IOException { throw new IOException(); } }
"#;
    ok(src);
}

#[test]
fn same_class_call_site_propagates_throws() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class T {
  void callee() throws IOException { throw new IOException(); }
  void caller(){ callee(); }
}
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}

#[test]
fn cross_type_call_site_propagates_throws() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class A { static void m() throws IOException { throw new IOException(); } }
class T { void t(){ A.m(); } }
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}

#[test]
fn cross_type_constructor_propagates_throws() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class A { A() throws IOException { throw new IOException(); } }
class T { void t(){ new A(); } }
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}


