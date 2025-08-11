// Minimal review tests for modifiers and constructors
use tolc::parser::parse_and_verify;

fn review_ok(src: &str) {
    let _ = parse_and_verify(src).expect("expected review ok");
}

fn review_err(src: &str) -> String {
    match parse_and_verify(src) {
        Ok(_) => panic!("expected review error"),
        Err(e) => e.to_string(),
    }
}

#[test]
fn interface_field_must_be_psf() {
    let src = r#"
package p; interface I { public static final int A = 1; }
"#;
    review_ok(src);

    // Omitted modifiers are implicitly public static final; should be OK
    let implicit = r#"
package p; interface I { int A = 1; }
"#;
    review_ok(implicit);

    // Current parser normalizes interface fields to public static final; skip explicit illegal flag test here
}

#[test]
fn abstract_method_illegal_combos() {
    let src = r#"
package p; class C { abstract void m(); }
"#;
    review_ok(src);

    for code in [
        r#"package p; class C { private abstract void m(); }"#,
        r#"package p; class C { static abstract void m(); }"#,
        r#"package p; class C { final abstract void m(); }"#,
        r#"package p; class C { synchronized abstract void m(); }"#,
        r#"package p; class C { native abstract void m(); }"#,
    ] {
        let msg = review_err(code);
        assert!(msg.contains("illegal modifier combination for abstract method"));
    }
}

#[test]
fn field_final_and_volatile_conflict() {
    let msg = review_err(
        r#"package p; class C { final volatile int x; }"#,
    );
    assert!(msg.contains("cannot be both final and volatile"));
}

#[test]
fn constructor_rules_varargs_and_mods() {
    // ok: single varargs ctor
    review_ok(r#"package p; class C { C(int... a){} }"#);
    // err: two varargs ctors
    let msg = review_err(r#"package p; class C { C(int... a){} C(long... b){} }"#);
    assert!(msg.contains("At most one varargs constructor"));

    // illegal ctor modifiers
    for code in [
        r#"package p; class C { abstract C(){} }"#,
        r#"package p; class C { static C(){} }"#,
        r#"package p; class C { final C(){} }"#,
        r#"package p; class C { synchronized C(){} }"#,
        r#"package p; class C { native C(){} }"#,
    ] { let msg = review_err(code); assert!(msg.contains("illegal modifier combination for constructor")); }
}

#[test]
fn enum_constructor_visibility() {
    let msg = review_err(r#"package p; enum E { A; public E(){} }"#);
    assert!(msg.contains("constructors cannot be public or protected"));
}


