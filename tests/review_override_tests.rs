use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{}", e); }

#[test]
fn cannot_override_final_method() {
    let src = r#"
package p;
class Super { final void m(){} }
class Sub extends Super { void m(){} }
"#;
    err_contains(src, "cannot override final method");
}

#[test]
fn static_vs_instance_override_is_illegal() {
    // instance overrides static
    let src1 = r#"
package p;
class Super { static void m(){} }
class Sub extends Super { void m(){} }
"#;
    err_contains(src1, "overrides static method");

    // static hides instance
    let src2 = r#"
package p;
class Super { void m(){} }
class Sub extends Super { static void m(){} }
"#;
    err_contains(src2, "hides instance method");
}

#[test]
fn visibility_cannot_be_reduced() {
    let src = r#"
package p;
class Super { public void m(){} }
class Sub extends Super { protected void m(){} }
"#;
    err_contains(src, "override reduces visibility");
}

#[test]
fn covariant_return_reference_ok() {
    let src = r#"
package p;
class Super { Object m(){ return null; } }
class Sub extends Super { String m(){ return null; } }
"#;
    ok(src);
}

#[test]
fn primitive_return_mismatch_errors() {
    let src = r#"
package p;
class Super { int m(){ return 0; } }
class Sub extends Super { long m(){ return 0; } }
"#;
    err_contains(src, "incompatible return type");
}

#[test]
fn throws_narrowing_ok_broadening_errors() {
    // Narrowing: super throws Exception; sub throws IOException -> OK
    let src_ok = r#"
package p;
class Exception {}
class IOException extends Exception {}
class Super { void m() throws Exception {} }
class Sub extends Super { void m() throws IOException {} }
"#;
    ok(src_ok);

    // Broadening: super throws IOException; sub throws Exception -> error
    let src_err = r#"
package p;
class Exception {}
class IOException extends Exception {}
class Super { void m() throws IOException {} }
class Sub extends Super { void m() throws Exception {} }
"#;
    err_contains(src_err, "throws incompatible exception");
}

#[test]
fn interface_method_implementation_must_be_public() {
    let src = r#"
package p;
interface I { public void m(); }
class C implements I { void m(){} }
"#;
    err_contains(src, "must be public");
}


