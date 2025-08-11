use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }

#[test]
fn static_call_via_import() {
    ok(r#"package p;
import q.C;
class T { static void t(){ C.m(); } }
"#);
}

#[test]
fn static_call_via_wildcard_import() {
    ok(r#"package p;
import q.*;
class T { static void t(){ C.m(); } }
"#);
}

#[test]
fn unqualified_static_call_via_explicit_static_import() {
    // Import must appear before type declarations
    ok(r#"package p;
import static p.C.m;
class C { static void m(){} }
class T { static void t(){ m(); } }
"#);
}

#[test]
fn unqualified_static_call_via_wildcard_static_import() {
    ok(r#"package p;
import static p.C.*;
class C { static void m(){} }
class T { static void t(){ m(); } }
"#);
}

#[test]
fn unqualified_call_rejects_non_static_even_if_imported() {
    // Local class C with non-static ns; importing static ns should error on unqualified use
    match tolc::parser::parse_and_verify(r#"package p;
import static p.C.ns;
class C { void ns(){} }
class T { static void t(){ ns(); } }
"#) {
        Ok(_) => panic!("expected review error for non-static call"),
        Err(e) => assert!(e.to_string().contains("Illegal static call") || e.to_string().contains("illegal static call")),
    }
}

#[test]
fn unqualified_static_field_via_explicit_static_import() {
    // Positive smoke test for static field import; we don't yet validate field reads, but should parse and review OK
    ok(r#"package p;
import static p.C.PI;
class C { static int PI = 3; }
class T { static int x = PI; }
"#);
}

#[test]
fn unqualified_nonstatic_field_via_explicit_static_import_errors() {
    match tolc::parser::parse_and_verify(r#"package p;
import static p.C.v;
class C { int v = 3; }
class T { int x = v; }
"#) {
        Ok(_) => panic!("expected review error for non-static field import usage"),
        Err(e) => assert!(e.to_string().to_lowercase().contains("illegal static")),
    }
}

#[test]
fn unqualified_nonstatic_field_via_wildcard_static_import_errors() {
    match tolc::parser::parse_and_verify(r#"package p;
import static p.C.*;
class C { int v = 3; }
class T { int x = v; }
"#) {
        Ok(_) => panic!("expected review error for non-static field import usage"),
        Err(e) => assert!(e.to_string().to_lowercase().contains("illegal static")),
    }
}

#[test]
fn local_type_shadows_import_simple_name() {
    ok(r#"package p;
import q.A;
class A { static void m(){} }
class T { static void t(){ A.m(); } }
"#);
}

#[test]
fn ambiguous_imports_same_simple_name_is_tolerated_currently() {
    ok(r#"package p;
import q.A;
import r.A;
class T { static void t(){ A.m(); } }
"#);
}

#[test]
fn generic_arity_mismatch_in_new_with_imported_type_errors() {
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p;
import q.A;
class T { void t(){ new A<int,int>(); } }
"#) {
        Ok(_) => panic!("expected review error for generic arity mismatch on imported type"),
        Err(e) => assert!(e.to_string().contains("expects 0 type argument(s)")),
    }
}

#[test]
fn zero_arity_class_used_with_type_args_errors() {
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p; class A{} class T{ void t(){ new A<int>(); } }"#) {
        Ok(_) => panic!("expected review error for zero-arity class used with type args"),
        Err(e) => assert!(e.to_string().contains("expects 0 type argument(s)")),
    }
}

#[test]
fn nonzero_arity_class_used_without_type_args_errors() {
    use tolc::parser::parse_and_verify;
    match parse_and_verify(r#"package p; class A<T>{} class T{ void t(){ new A(); } }"#) {
        Ok(_) => panic!("expected review error for missing type args"),
        Err(e) => assert!(e.to_string().contains("expects 1 type argument(s)")),
    }
}


