use tolc::parser::parse_and_verify;
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};
use std::env;
use std::sync::{Mutex, OnceLock};

static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

fn ok(src: &str) {
    let lock = ENV_LOCK.get_or_init(|| Mutex::new(()));
    let _g = match lock.lock() { Ok(g) => g, Err(p) => p.into_inner() };
    let _ = parse_and_verify(src).expect("expected ok");
}
fn err_contains(src: &str, needle: &str) {
    let lock = ENV_LOCK.get_or_init(|| Mutex::new(()));
    let _g = match lock.lock() { Ok(g) => g, Err(p) => p.into_inner() };
    let e = parse_and_verify(src).unwrap_err().to_string();
    assert!(e.contains(needle), "{e}");
}

// package-private field is accessible within the same package (instance access)
#[test]
fn package_private_field_accessible_same_package() {
    let src = r#"
package p;
class A { int f; }
class T { void t(){ A a = new A(); int x = a.f; } }
"#;
    ok(src);
    env::remove_var("TOLC_CLASSPATH");
}

// private field is not accessible even in the same package (instance access)
#[test]
fn private_field_inaccessible() {
    let src = r#"
package p;
class A { private int f; }
class T { void t(){ A a = new A(); int x = a.f; } }
"#;
    err_contains(src, "inaccessible member 'f' in 'A'");
}

// protected field is accessible within the same package (instance access)
#[test]
fn protected_field_accessible_same_package() {
    let src = r#"
package p;
class A { protected int f; }
class T { void t(){ A a = new A(); int x = a.f; } }
"#;
    ok(src);
}


// cross-package: subclass code accessing protected via this/subobject is allowed
#[test]
fn protected_cross_package_subclass_through_this_ok() {
    // Hold the env lock while mutating and parsing to avoid flakiness
    let lock = ENV_LOCK.get_or_init(|| Mutex::new(()));
    let _g = match lock.lock() { Ok(g) => g, Err(p) => p.into_inner() };
    // Prepare classpath with p1.A
    let mut cp = env::temp_dir();
    let stamp = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
    cp.push(format!("tolc_cp_{}_this", stamp));
    fs::create_dir_all(cp.join("p1")).unwrap();
    fs::write(cp.join("p1").join("A.java"), "package p1; public class A { protected int f; }").unwrap();
    env::set_var("TOLC_CLASSPATH", cp.as_os_str());
    let src = r#"
package p2;
import p1.A;
class B extends A { void t(){ int x = this.f; } }
"#;
    let _ = parse_and_verify(src).expect("expected ok");
    env::remove_var("TOLC_CLASSPATH");
}

// cross-package: subclass code accessing protected via unrelated qualifier is not allowed
#[test]
fn protected_cross_package_subclass_through_unrelated_inaccessible() {
    // Hold the env lock while mutating and parsing to avoid flakiness
    let lock = ENV_LOCK.get_or_init(|| Mutex::new(()));
    let _g = match lock.lock() { Ok(g) => g, Err(p) => p.into_inner() };
    // Prepare classpath with p1.A
    let mut cp = env::temp_dir();
    let stamp = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
    cp.push(format!("tolc_cp_{}_unrel", stamp));
    fs::create_dir_all(cp.join("p1")).unwrap();
    fs::write(cp.join("p1").join("A.java"), "package p1; public class A { protected int f; }").unwrap();
    env::set_var("TOLC_CLASSPATH", cp.as_os_str());
    let src = r#"
package p2;
import p1.A;
class B extends A { void t(){ A a = new A(); int x = a.f; } }
"#;
    let e = parse_and_verify(src).unwrap_err().to_string();
    assert!(e.contains("inaccessible member 'f' in 'A'"), "{e}");
    env::remove_var("TOLC_CLASSPATH");
}
