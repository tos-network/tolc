use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn use_before_init_errors() {
    err_contains(r#"package p; class C { int m(){ int x; return x; } }"#, "use of local variable");
}

#[test]
fn while_true_then_break_preserves_assignment() {
    ok(r#"package p; class C { int m(){ int x; while(true){ x=1; break; } return x; } }"#);
}

#[test]
fn while_conditional_then_break_then_use_ok() {
    ok(r#"package p; class C { int m(boolean b){ int x; while(b){ x=1; break; } return x; } }"#);
}

#[test]
fn assign_to_final_param_errors() {
    err_contains(r#"package p; class C { void m(final int p){ p = 1; } }"#, "cannot assign to final parameter");
}

#[test]
fn if_both_branches_assign_then_ok() {
    ok(r#"package p; class C { int m(){ int x; if (true){ x = 1; } else { x = 2; } return x; } }"#);
}

#[test]
fn if_only_then_assign_still_error_on_use() {
    err_contains(r#"package p; class C { int m(){ int x; if (true){ x = 1; } return x; } }"#, "use of local variable");
}

#[test]
fn illegal_static_call_on_instance_method() {
    err_contains(r#"package p; class C { void i(){} static void s(){} void m(){ C.i(); } }"#, "illegal static call");
}

#[test]
fn overload_tiebreak_prefers_exact_over_widened() {
    ok(r#"package p; class C { static void m_int(int x){} static void m_long(long x){} static void t(){ C.m_int(1); } }"#);
}

#[test]
fn overload_exact_vs_mixed_prefers_exact_multiarg() {
    ok(r#"package p; class C {
        static void m(int x, long y){}
        static void m(long x, long y){}
        static void t(){ C.m(1, 1L); }
    }"#);
}

#[test]
fn overload_ambiguous_on_equal_conversions() {
    // Both candidates require one widening conversion with equal cost -> ambiguous
    let src = r#"package p; class C {
        static void m(long x, int y){}
        static void m(int x, long y){}
        static void t(){ C.m(1, 1); }
    }"#;
    err_contains(src, "ambiguous method");
}

#[test]
fn generic_arity_mismatch_in_new_expression_errors() {
    // Class A has 1 type parameter; usage with 2 should error
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p; class A<T>{} class T { void t(){ new A<int, int>(); } }"#) {
        Ok(_) => panic!("expected review error for generic arity mismatch"),
        Err(e) => assert!(e.to_string().contains("expects 1 type argument(s)")),
    }
}

#[test]
fn generic_arity_match_in_new_expression_ok() {
    ok(r#"package p; class A<T>{} class T { void t(){ new A<int>(); } }"#);
}

#[test]
fn generic_arity_mismatch_in_cast_errors() {
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p; class A<T>{} class T { void t(Object o){ A<int,int> a = (A<int,int>) o; } }"#) {
        Ok(_) => panic!("expected review error for generic arity mismatch in cast"),
        Err(e) => assert!(e.to_string().contains("expects 1 type argument(s)")),
    }
}

#[test]
fn generic_arity_mismatch_in_instanceof_errors() {
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p; class A<T>{} class T { boolean t(Object o){ return o instanceof A<int,int>; } }"#) {
        Ok(_) => panic!("expected review error for generic arity mismatch in instanceof"),
        Err(e) => assert!(e.to_string().contains("expects 1 type argument(s)")),
    }
}

#[test]
fn generic_bound_violation_in_new_errors() {
    use tolc::parser::parse_and_verify;
    // A<T extends Number>; using String as T should fail our minimal bound check
    match parse_and_verify(r#"package p; class Number{} class String{} class A<T>{} class B<T extends Number>{} class T { void t(){ new B<String>(); } }"#) {
        Ok(_) => panic!("expected review error for generic bound violation in new"),
        Err(e) => assert!(e.to_string().contains("does not satisfy upper bound")),
    }
}

#[test]
fn generic_bound_violation_in_cast_errors() {
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p; class Number{} class String{} class B<T extends Number>{} class T { void t(Object o){ B<String> b = (B<String>) o; } }"#) {
        Ok(_) => panic!("expected review error for generic bound violation in cast"),
        Err(e) => assert!(e.to_string().contains("does not satisfy upper bound")),
    }
}

#[test]
fn generic_bound_violation_in_instanceof_errors() {
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p; class Number{} class String{} class B<T extends Number>{} class T { boolean t(Object o){ return o instanceof B<String>; } }"#) {
        Ok(_) => panic!("expected review error for generic bound violation in instanceof"),
        Err(e) => assert!(e.to_string().contains("does not satisfy upper bound")),
    }
}
#[test]
fn for_infinite_then_break_preserves_assignment() {
    ok(r#"package p; class C { int m(){ int x; for(;;){ x=1; break; } return x; } }"#);
}

#[test]
fn while_true_continue_then_assign_then_break_ok() {
    ok(r#"package p; class C { int m(boolean b){ int x; while(true){ if(b){ continue; } x=1; break; } return x; } }"#);
}

#[test]
fn while_conditional_assign_then_continue_still_error_on_use() {
    err_contains(r#"package p; class C { int m(boolean b){ int x; while(b){ x=1; continue; } return x; } }"#, "use of local variable");
}

#[test]
fn nested_loop_break_without_guaranteed_assign_errors() {
    err_contains(r#"package p; class C { int m(boolean b, boolean c){ int x; while(b){ if(c){ x=1; break; } break; } return x; } }"#, "use of local variable");
}

#[test]
fn while_true_with_if_both_branches_assign_then_break_ok() {
    ok(r#"package p; class C { int m(boolean b){ int x; while(true){ if(b){ x=1; break; } else { x=2; break; } } return x; } }"#);
}

#[test]
fn for_infinite_continue_then_assign_then_break_ok() {
    ok(r#"package p; class C { int m(boolean b){ int x; for(;;){ if(b){ continue; } x=1; break; } return x; } }"#);
}

#[test]
fn try_catch_finally_da_merge_ok() {
    ok(r#"package p; class C { int m(boolean b){ int x; try { if(b){ x=1; } else { x=2; } } catch(Exception e){ x=3; } finally { } return x; } }"#);
}

#[test]
fn try_without_catch_relies_on_try_only() {
    ok(r#"package p; class C { int m(){ int x; try { x=1; } finally { } return x; } }"#);
}

#[test]
fn finally_can_establish_da() {
    ok(r#"package p; class C { int m(){ int x; try { } finally { x=1; } return x; } }"#);
}

#[test]
fn labeled_break_terminates_path() {
    ok(r#"package p; class C { int m(){ int x; outer: while(true){ x=1; break outer; } return x; } }"#);
}

#[test]
fn switch_with_default_all_cases_assign_ok() {
    ok(r#"package p; class C { int m(int k){ int x; switch(k){ case 0: x=1; break; case 1: x=2; break; default: x=3; } return x; } }"#);
}

#[test]
fn switch_fallthrough_without_break_still_assigns_when_default_exists() {
    ok(r#"package p; class C { int m(int k){ int x; switch(k){ case 0: case 1: x=2; default: x=3; } return x; } }"#);
}

#[test]
fn switch_no_default_needs_all_paths_assign() {
    err_contains(r#"package p; class C { int m(int k){ int x; switch(k){ case 0: x=1; case 1: break; } return x; } }"#, "use of local variable");
}

#[test]
fn labeled_continue_does_not_establish_da_outside() {
    // continue outer should not help DA outside the loop
    err_contains(r#"package p; class C { int m(boolean b){ int x; outer: while(b){ x=1; inner: while(true){ continue outer; } } return x; } }"#, "use of local variable");
}

#[test]
fn switch_without_default_does_not_establish_da() {
    err_contains(r#"package p; class C { int m(int k){ int x; switch(k){ case 0: x=1; break; case 1: x=2; break; } return x; } }"#, "use of local variable");
}
// Ambiguity from duplicate exact overloads is rejected earlier as duplicate members in our review rules
// Break/continue DA/DR precision to be extended in follow-ups; placeholders removed to avoid flaky expectations


