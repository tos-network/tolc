// Common test utilities

/// Setup test classpath to resolve java.lang.String and other standard library classes properly
/// Since test files are in tests/ directory, use current directory as classpath
pub fn setup_test_classpath() {
    let current_dir = std::env::current_dir().unwrap();
    let tests_dir = current_dir.join("tests");
    std::env::set_var("TOLC_CLASSPATH", tests_dir.to_string_lossy().as_ref());
}

/// Setup test classpath only if not already set (non-destructive)
pub fn setup_test_classpath_if_needed() {
    if std::env::var("TOLC_CLASSPATH").is_err() {
        setup_test_classpath();
    }
}