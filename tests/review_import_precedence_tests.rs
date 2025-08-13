use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

#[test]
fn explicit_import_beats_wildcard() {
    // java.util.List vs java.awt.List; wildcard imports both, explicit selects util.List
    let src = r#"package p;
import java.util.List;
import java.awt.*;
class C { List f; }"#;
    ok(src);
}

#[test]
fn current_package_beats_wildcard_import() {
    // Class A in current package p should resolve, not q.A from wildcard
    let src = r#"package p;
import q.*;
class A {}
class C { A f; }"#;
    ok(src);
}

#[test]
fn implicit_java_lang_is_visible_without_import() {
    ok(r#"package p; class C { String s; }"#);
}


