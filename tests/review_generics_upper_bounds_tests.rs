use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn new_with_single_upper_bound_ok() {
    let src = r#"
package p;
class A {}
class B extends A {}
class G<T extends A> {}
class T0 { void t(){ new G<A>(); new G<B>(); } }
"#;
    ok(src);
}

#[test]
fn new_with_single_upper_bound_violation_errors() {
    let src = r#"
package p;
class A {}
class C {}
class G<T extends A> {}
class T0 { void t(){ new G<C>(); } }
"#;
    err_contains(src, "does not satisfy upper bound");
}

#[test]
fn new_with_intersection_bounds_ok() {
    let src = r#"
package p;
interface I {}
class A {}
class B extends A implements I {}
class G<T extends A & I> {}
class T0 { void t(){ new G<B>(); } }
"#;
    ok(src);
}

#[test]
fn new_with_intersection_bounds_violation_errors() {
    let src = r#"
package p;
interface I {}
class A {}
class C extends A {}
class G<T extends A & I> {}
class T0 { void t(){ new G<C>(); } }
"#;
    err_contains(src, "does not satisfy upper bound");
}

#[test]
fn cast_with_upper_bound_violation_errors() {
    let src = r#"
package p;
class A {}
class C {}
class G<T extends A> {}
class T0 { void t(Object o){ G<C> x = (G<C>) o; } }
"#;
    err_contains(src, "does not satisfy upper bound");
}

#[test]
fn instanceof_with_upper_bound_violation_errors() {
    let src = r#"
package p;
class A {}
class C {}
class G<T extends A> {}
class T0 { boolean t(Object o){ return o instanceof G<C>; } }
"#;
    err_contains(src, "does not satisfy upper bound");
}


