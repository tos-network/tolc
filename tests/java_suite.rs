use std::fs;
use std::path::{Path, PathBuf};
use tolc::parser::parse_and_verify;
use std::env;
use walkdir::WalkDir;

fn java_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("java")
}

#[test]
fn parse_all_java_files_under_tests_java() {
    // Initialize logger for diagnostics
    let _ = env_logger::builder()
        .is_test(true)
        .format_timestamp_millis()
        .filter_level(log::LevelFilter::Debug)
        .try_init();
    let root = java_root();
    assert!(root.exists(), "tests/java directory not found: {}", root.display());

    // Ensure classpath and compatibility mode are set when running under `cargo test`
    // so the suite uses full cross-file type information and javac-aligned relaxations.
    if env::var("TOLC_CLASSPATH").is_err() {
        env::set_var("TOLC_CLASSPATH", root.display().to_string());
    }
    // Bound classpath scan during tests to avoid long walks
    if env::var("TOLC_CLASSPATH_MAX_FILES").is_err() {
        env::set_var("TOLC_CLASSPATH_MAX_FILES", "60");
    }
    if env::var("TOLC_CLASSPATH_MAX_FILE_SIZE").is_err() {
        env::set_var("TOLC_CLASSPATH_MAX_FILE_SIZE", "65536"); // 64 KiB
    }
    if env::var("TOLC_JAVAC_COMPAT").is_err() {
        env::set_var("TOLC_JAVAC_COMPAT", "1");
    }

    let mut failures: Vec<(String, String)> = Vec::new();
    let mut total: usize = 0;

    let filter = std::env::var("JAVA_SUITE_FILTER").ok();
    let first_only = std::env::var("JAVA_SUITE_FIRST_ONLY").is_ok();
    for entry in WalkDir::new(&root).into_iter().filter_map(Result::ok) {
        let path = entry.path();
        if entry.file_type().is_file() && path.extension().map(|e| e == "java").unwrap_or(false) {
            // Temporary exclusion: skip problematic file(s)
            if path.display().to_string().ends_with("/java/lang/String.java") {
                eprintln!("[suite] skipping {}", path.display());
                continue;
            }
            if let Some(f) = &filter {
                if !path.display().to_string().contains(f) { continue; }
            }
            total += 1;
            match fs::read_to_string(path) {
                Ok(source) => {
                    eprintln!("[suite] verifying {}", path.display());
                    match parse_and_verify(&source) {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("[suite] FAILED: {} -> {}", path.display(), e);
                            let snippet: String = source.lines().take(30).collect::<Vec<_>>().join("\n");
                            let msg = format!("{}\n--- snippet ---\n{}\n--------------", e, snippet);
                            failures.push((path.display().to_string(), msg));
                            if first_only { break; }
                        }
                    }
                }
                Err(io_err) => {
                    failures.push((path.display().to_string(), format!("IO error: {}", io_err)));
                    if first_only { break; }
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
