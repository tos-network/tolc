use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn multicatch_covers_union_ok() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class SecurityException extends Exception {}
class T {
  void t(boolean b){
    try {
      if (b) { throw new IOException(); } else { throw new SecurityException(); }
    } catch (IOException | SecurityException e) {
    }
  }
}
"#;
    ok(src);
}

#[test]
fn multicatch_covers_called_method_ok() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class SecurityException extends Exception {}
class A { void m(boolean b) throws IOException { if (b) { throw new IOException(); } } }
class T {
  void t(boolean b){
    try { new A().m(b); } catch (IOException | SecurityException e) {}
  }
}
"#;
    ok(src);
}

#[test]
fn generic_class_type_var_throws_requires_exception_error() {
    // class-level type var bound used as throws
    let src = r#"
package p;
class Exception {}
class A<T extends Exception> { void m() throws T {} }
class T0 { void call(){ A<Exception> a = new A<Exception>(); a.m(); } }
"#;
    err_contains(src, "unreported checked exception 'Exception' thrown");
}

#[test]
fn generic_class_type_var_throws_declare_exception_ok() {
    let src = r#"
package p;
class Exception {}
class A<T extends Exception> { void m() throws T {} }
class T0 { void call() throws Exception { new A<Exception>().m(); } }
"#;
    ok(src);
}

#[test]
fn multiple_catch_later_covers_ok() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class SecurityException extends Exception {}
class T {
  void t(){
    try { throw new IOException(); }
    catch (SecurityException e) {}
    catch (Exception e) {}
  }
}
"#;
    ok(src);
}

#[test]
fn multicatch_with_precise_rethrow_requires_coverage_error() {
    // Rethrow of catch parameter should require declaring/catching the precise thrown type from try
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class SecurityException extends Exception {}
class T {
  void t(){
    try { throw new IOException(); }
    catch (Exception | SecurityException e) { throw e; }
  }
}
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}

#[test]
fn nested_try_catch_finally_coverage_ok() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class T {
  void t(){
    try {
      try { throw new IOException(); }
      catch (IOException e) { }
      finally { }
    } catch (Exception e) { }
  }
}
"#;
    ok(src);
}

// Method-level type variable throws: <X extends Exception> void m() throws X
#[test]
fn method_generic_throws_exception_ok() {
    let src = r#"
package p;
class Exception {}
class T {
  <X extends Exception> void m() throws X {}
  void call(){ m(); }
}
"#;
    ok(src);
}

#[test]
fn method_generic_throws_exception_catch_ok() {
    let src = r#"
package p;
class Exception {}
class T {
  <X extends Exception> void m() throws X {}
  void call(){ try { m(); } catch (Exception e) {} }
}
"#;
    ok(src);
}

#[test]
fn method_generic_throws_io_requires_coverage_error() {
    let src = r#"
package p;
class Exception {}
class IOException extends Exception {}
class T {
  <X extends IOException> void m() throws X {}
  void call(){ m(); }
}
"#;
    err_contains(src, "unreported checked exception 'IOException' thrown");
}

#[test]
fn method_generic_throws_runtime_is_unchecked_ok() {
    let src = r#"
package p;
class Exception {}
class RuntimeException extends Exception {}
class T {
  <X extends RuntimeException> void m() throws X {}
  void call(){ m(); }
}
"#;
    ok(src);
}

// Note: method-level type parameters in throws are not yet parsed in this subset; class-level generic tests cover the mapping.


