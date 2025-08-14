use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

// Instance initializer assignment counts once in the effective (non-delegating) constructor
#[test]
fn instance_initializer_counts_in_effective_ctor() {
    ok(r#"package p; class T { final int x; { x = 1; } T(){ this(0); } T(int a){ } }"#);
}

// Assigning final after returning from delegated target causes multiple-assignment error
#[test]
fn delegating_then_assign_after_is_multiple_assignment_error() {
    err_contains(
        r#"package p; class T { final int x; T(){ this(0); x=2; } T(int a){ x=1; } }"#,
        "final field 'x' is assigned more than once in constructor",
    );
}

// No assignment anywhere across delegation chain is an error
#[test]
fn no_assignment_across_delegation_is_error() {
    err_contains(
        r#"package p; class T { final int x; T(){ this(0); } T(int a){ } }"#,
        "final field 'x' must be assigned exactly once",
    );
}

// Instance initializer plus constructor assignment yields multiple-assignment error
#[test]
fn initializer_plus_ctor_assignment_is_multiple_assignment_error() {
    err_contains(
        r#"package p; class T { final int x; { x=1; } T(){ x=2; } }"#,
        "final field 'x' is assigned more than once",
    );
}

// Multi-level delegation: deepest target assigns exactly once → ok
#[test]
fn multi_level_this_delegation_single_assignment_ok() {
    ok(r#"package p; class T { final int x; T(){ this(1); } T(int a){ this(2, a); } T(int a, int b){ x = a + b - a; } }"#);
}

// Delegating ctor may not assign after returning from this(...) target (already covered), but the effective target can assign before super()
#[test]
fn delegating_then_target_assigns_before_super_ok() {
    ok(r#"package p; class S { S(){} } class T extends S { final int x; T(){ this(1); } T(int a){ x=a; super(); } }"#);
}

// Instance initializer executes only once along effective ctor path: ensure ordering is target(super)->init->body
#[test]
fn instance_initializer_ordering_applied_once() {
    ok(r#"package p; class S { S(){} } class T extends S { final int x; { x = 1; } T(){ this(2); } T(int a){ super(); } }"#);
}


