use std::fs;
use std::path::{Path, PathBuf};
use tolc::parser::parse_tol;
use walkdir::WalkDir;

fn java_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("java")
}

#[test]
fn parse_all_java_files_under_tests_java() {
    let root = java_root();
    assert!(root.exists(), "tests/java directory not found: {}", root.display());

    let mut failures: Vec<(String, String)> = Vec::new();
    let mut total: usize = 0;

    for entry in WalkDir::new(&root).into_iter().filter_map(Result::ok) {
        let path = entry.path();
        if entry.file_type().is_file() && path.extension().map(|e| e == "java").unwrap_or(false) {
            total += 1;
            match fs::read_to_string(path) {
                Ok(source) => {
                    match parse_tol(&source) {
                        Ok(_) => {}
                        Err(e) => {
                            // Show a small prefix of the source for context
                            let snippet: String = source.lines().take(30).collect::<Vec<_>>().join("\n");
                            let msg = format!("{}\n--- snippet ---\n{}\n--------------", e, snippet);
                            failures.push((path.display().to_string(), msg));
                        }
                    }
                }
                Err(io_err) => {
                    failures.push((path.display().to_string(), format!("IO error: {}", io_err)));
                }
            }
        }
    }

    if !failures.is_empty() {
        eprintln!("Parsed {} Java files. {} failed:\n", total, failures.len());
        for (p, e) in &failures {
            eprintln!("- {} -> {}\n", p, e);
        }
        panic!("Java parse failures: {} of {}", failures.len(), total);
    }
}
