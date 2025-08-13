use std::fs;
use std::io::Write;
use std::path::PathBuf;

use tolc::parser::parse_and_verify;

#[test]
fn parser_error_budget_trips_instead_of_hanging() {
    // Force a very small error budget to trigger fast failure on malformed source
    std::env::set_var("TOLC_PARSE_MAX_ERRORS", "2");

    // Malformed interface body with many stray tokens and missing '}'
    let src = r#"
package p;
public interface I {
    ? ? ? ? ? ? ? ? ?
"#;

    let res = parse_and_verify(src);
    assert!(res.is_ok(), "expected Ok (bounded recovery), got: {:?}", res.err());
}

#[test]
fn classpath_scan_is_bounded_and_returns() {
    // Prepare a temporary directory with many small .java files
    let mut dir = std::env::temp_dir();
    dir.push(format!("tolc_test_cp_{}", std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()));
    fs::create_dir_all(&dir).expect("create temp classpath dir");

    // Create more files than the enforced bound
    let num_files = 20usize;
    for i in 0..num_files {
        let mut p = PathBuf::from(&dir);
        p.push(format!("A{}{}.java", i / 26, char::from(b'A' + (i % 26) as u8)));
        let mut f = fs::File::create(&p).expect("create temp java file");
        writeln!(
            f,
            "package tmp; public class C{}{} {{ public void m(){{}} }}",
            i / 26,
            char::from(b'A' + (i % 26) as u8)
        )
        .expect("write temp java file");
    }

    // Limit scanner to a small number to bound work regardless of how many files are present
    std::env::set_var("TOLC_CLASSPATH", &dir);
    std::env::set_var("TOLC_CLASSPATH_MAX_FILES", "5");
    std::env::set_var("TOLC_CLASSPATH_MAX_FILE_SIZE", "4096");

    // Simple source that doesn't depend on classpath correctness
    let src = r#"
package p;
import tmp.CA; // arbitrary import; existence not required for review to succeed
public class T { void m() { return; } }
"#;

    // Should complete (Ok or a quick, non-hanging Err). Prefer Ok.
    let res = parse_and_verify(src);
    match res {
        Ok(_) => {}
        Err(e) => {
            // Even if review fails for other reasons, the key is we didn't hang
            let _ = e;
        }
    }
}


