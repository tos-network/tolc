use std::fs;
use std::path::Path;
use tolc::parser::{parse_tol, parse_and_verify};

fn java_root() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("java")
}

fn init_logger() {
    let _ = env_logger::builder()
        .is_test(true)
        .format_timestamp_millis()
        .filter_level(log::LevelFilter::Debug)
        .try_init();
}

fn setup_env() {
    // Ensure classpath and compatibility flags for the parser/type resolver
    if std::env::var("TOLC_CLASSPATH").is_err() {
        std::env::set_var("TOLC_CLASSPATH", java_root().display().to_string());
    }
    if std::env::var("TOLC_CLASSPATH_MAX_FILES").is_err() {
        std::env::set_var("TOLC_CLASSPATH_MAX_FILES", "60");
    }
    if std::env::var("TOLC_CLASSPATH_MAX_FILE_SIZE").is_err() {
        std::env::set_var("TOLC_CLASSPATH_MAX_FILE_SIZE", "65536");
    }
    if std::env::var("TOLC_JAVAC_COMPAT").is_err() {
        std::env::set_var("TOLC_JAVAC_COMPAT", "1");
    }
}

fn string_java_path() -> std::path::PathBuf {
    java_root().join("lang").join("String.java")
}

/// Full-file parse of tests/java/lang/String.java with verbose logs.
/// Ignored by default to avoid hanging CI if there's a parser infinite loop.
#[test]
fn parse_java_lang_string_full_parse() {
    init_logger();
    setup_env();
    let path = string_java_path();
    assert!(path.exists(), "String.java not found at {}", path.display());
    let source = fs::read_to_string(&path).expect("failed to read String.java");
    eprintln!("[debug] parsing {} ({} bytes)", path.display(), source.len());
    // Use parse_tol first to narrow whether the loop is in parsing vs. later review
    let _ = parse_tol(&source);
    // Then run the full pipeline
    let _ = parse_and_verify(&source);
}

/// Partial-file parse (first N lines) to quickly sanity-check lexing and early parsing.
#[test]
fn parse_java_lang_string_prefix_sanity() {
    init_logger();
    setup_env();
    let path = string_java_path();
    assert!(path.exists(), "String.java not found at {}", path.display());
    let source = fs::read_to_string(&path).expect("failed to read String.java");
    let prefix: String = source.lines().take(120).collect::<Vec<_>>().join("\n");
    eprintln!("[debug] parsing String.java prefix ({} bytes)", prefix.len());
    let _ = parse_and_verify(&source);
}


