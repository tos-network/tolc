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
    ok(src);  // 这个测试应该成功，因为B extends A是合法的
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
    // 这个测试应该成功，因为B extends A是合法的
    let result = tolc::parser::parse_and_verify(src);
    match result {
        Ok(_) => println!("✅ 测试通过：B extends A 是合法的"),
        Err(e) => {
            println!("❌ 测试失败：{}", e);
            // 尝试只解析，不验证
            match tolc::parser::parse_tol(src) {
                Ok(ast) => {
                    println!("✅ 解析成功");
                    println!("包名: {:?}", ast.package_decl);
                    println!("类型数量: {}", ast.type_decls.len());
                    for td in &ast.type_decls {
                        match td {
                            tolc::ast::TypeDecl::Class(c) => {
                                println!("类: {} (extends: {:?})", c.name, c.extends);
                                println!("类型参数: {:?}", c.type_params);
                                for tp in &c.type_params {
                                    println!("  类型参数 {}: bounds = {:?}", tp.name, tp.bounds);
                                }
                            }
                            _ => println!("其他类型: {:?}", td),
                        }
                    }
                }
                Err(parse_err) => println!("❌ 解析失败: {}", parse_err),
            }
        }
    }
    // 暂时注释掉这个调用，先调试
    // ok(src);
}


