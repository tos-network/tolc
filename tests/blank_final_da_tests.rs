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

// Delegation chain where both target and intermediate assign → multiple assignment error
#[test]
fn delegating_chain_double_assign_error() {
    err_contains(
        r#"package p; class T { final int x; T(){ this(1); } T(int a){ this("s"); x=1; } T(String s){ x=2; } }"#,
        "final field 'x' is assigned more than once",
    );
}

// Conditional ensures exactly-one assignment along all paths → ok
#[test]
fn conditional_both_branches_assign_ok() {
    ok(r#"package p; class T { final int x; T(boolean f){ if(f){ x=1; } else { x=2; } } }"#);
}

// Try/finally: assignment only in try, empty finally → ok
#[test]
fn try_finally_single_assignment_ok() {
    ok(r#"package p; class T { final int x; T(){ try { x=1; } finally { } } }"#);
}

// Try/finally: assignments in both try and finally → multiple assignment error
#[test]
fn try_finally_double_assignment_error() {
    err_contains(
        r#"package p; class T { final int x; T(){ try { x=1; } finally { x=2; } } }"#,
        "final field 'x' is assigned more than once",
    );
}

// Constructor that does not complete normally (throws) need not assign
#[test]
fn throwing_constructor_without_assignment_ok() {
    ok(r#"package p; class T { final int x; T(){ throw new RuntimeException(); } }"#);
}

// Multiple finals: initializer assigns one; constructor assigns the other → ok
#[test]
fn multiple_finals_initializer_and_ctor_ok() {
    ok(r#"package p; class T { final int x; final int y; { x=1; } T(){ y=2; } }"#);
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

// Two instance initializer blocks both assign the same final -> error
#[test]
fn two_initializers_assigning_same_final_error() {
    err_contains(
        r#"package p; class T { final int x; { x=1; } { x=2; } T(){ } }"#,
        "final field 'x' is assigned more than once",
    );
}

// Instance initializer + target assignment via delegation -> multiple assignment error
#[test]
fn initializer_plus_target_assignment_via_delegation_error() {
    err_contains(
        r#"package p; class T { final int x; { x=1; } T(){ this(0); } T(int a){ x=2; } }"#,
        "final field 'x' is assigned more than once",
    );
}

// Only initializer assigns; target does not -> ok
#[test]
fn initializer_only_with_delegation_target_no_assignment_ok() {
    ok(r#"package p; class T { final int x; { x=1; } T(){ this(0); } T(int a){ } }"#);
}

// Delegation to a target that may complete normally depending on arg: conservative analysis requires assignment → error
#[test]
fn delegating_to_target_that_always_throws_is_still_rejected_without_assignment() {
    err_contains(
        r#"package p; class T { final int x; T(){ this(true); } T(boolean b){ if(b){ throw new RuntimeException(); } else { x=1; } } }"#,
        "final field 'x' must be assigned exactly once",
    );
}

// Finally always throws -> ctor does not complete normally, ok without assignment
#[test]
fn finally_always_throws_no_assignment_ok() {
    ok(r#"package p; class T { final int x; T(){ try { } finally { throw new RuntimeException(); } } }"#);
}

// Early return before assignment -> ok (no normal completion along the early-return path)
#[test]
fn early_return_before_assignment_ok() {
    ok(r#"package p; class T { final int x; T(boolean b){ if(b){ return; } x=1; } }"#);
}

// Early return after assignment -> ok
#[test]
fn early_return_after_assignment_ok() {
    ok(r#"package p; class T { final int x; T(){ x=1; return; } }"#);
}

// try { return; } finally { x=1; } -> ok (assignment in finally before normal completion)
#[test]
fn try_return_then_assign_in_finally_ok() {
    ok(r#"package p; class T { final int x; T(){ try { return; } finally { x=1; } } }"#);
}

// try assigns and returns; finally empty -> ok
#[test]
fn try_assign_then_return_in_try_ok() {
    ok(r#"package p; class T { final int x; T(){ try { x=1; return; } finally { } } }"#);
}

// try assigns; finally also assigns and returns -> multiple assignment error
#[test]
fn try_assign_and_finally_assign_then_return_error() {
    err_contains(
        r#"package p; class T { final int x; T(){ try { x=1; } finally { x=2; return; } } }"#,
        "final field 'x' is assigned more than once",
    );
}

// Instance initializer always throws -> ok without assignment
#[test]
fn instance_initializer_always_throws_ok() {
    ok(r#"package p; class T { final int x; { throw new RuntimeException(); } T(){ } }"#);
}

// Loop guarantees assignment before break -> ok
#[test]
fn loop_guaranteed_break_assign_ok() {
    ok(r#"package p; class T { final int x; T(){ while(true){ x=1; break; } } }"#);
}

// Loop may break without assignment on some path -> error
#[test]
fn loop_break_without_assign_error() {
    err_contains(
        r#"package p; class T { final int x; T(boolean b){ while(true){ if(b){ x=1; break; } else { break; } } } }"#,
        "final field 'x' must be assigned exactly once",
    );
}


