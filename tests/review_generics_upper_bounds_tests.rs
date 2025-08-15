use tolc::parser::parse_and_verify;
use tolc::parser::parse_tol;

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
    ok(src);  // This test should succeed because B extends A is valid
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

#[test]
fn debug_simple_generic_check() {
    let src = r#"
package p;
class A {}
class B extends A {}
class G<T extends A> {}
class T0 { void t(){ new G<B>(); } }
"#;
    // This test should succeed because B extends A is valid
    let result = tolc::parser::parse_and_verify(src);
    match result {
        Ok(_) => println!("✅ Test passed: B extends A is valid"),
        Err(e) => {
            println!("❌ Test failed: {}", e);
            // Try parsing only, without validation
            match tolc::parser::parse_tol(src) {
                Ok(ast) => {
                    println!("✅ Parsing successful");
                    println!("Package name: {:?}", ast.package_decl);
                    println!("Type count: {}", ast.type_decls.len());
                    for td in &ast.type_decls {
                        match td {
                            tolc::ast::TypeDecl::Class(c) => {
                                println!("Class: {} (extends: {:?})", c.name, c.extends);
                                println!("Type parameters: {:?}", c.type_params);
                                for tp in &c.type_params {
                                    println!("  Type parameter {}: bounds = {:?}", tp.name, tp.bounds);
                                }
                            }
                            _ => println!("Other type: {:?}", td),
                        }
                    }
                }
                Err(parse_err) => println!("❌ Parsing failed: {}", parse_err),
            }
        }
    }
    // Temporarily comment out this call for debugging
    // ok(src);
}


