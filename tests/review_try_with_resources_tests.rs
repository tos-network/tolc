use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

// try-with-resources: Closeable.close() throws IOException by contract
// Unhandled close() should be reported as checked exception
#[test]
fn twr_closeable_unhandled_close_errors() {
    let src = r#"
    package p;
    class Exception {}
    class IOException extends Exception {}
    interface AutoCloseable { void close() throws Exception; }
    interface Closeable extends AutoCloseable { void close() throws IOException; }

    class R implements Closeable {
        public void close() throws IOException {}
    }

    class T {
        void m() {
            try (R r = new R()) { }
        }
    }
    "#;
    err_contains(src, "unreported checked exception 'IOException'");
}

// Caught IOException covers Closeable.close()
#[test]
fn twr_closeable_caught_ok() {
    let src = r#"
    package p;
    class Exception {}
    class IOException extends Exception {}
    interface AutoCloseable { void close() throws Exception; }
    interface Closeable extends AutoCloseable { void close() throws IOException; }

    class R implements Closeable {
        public void close() throws IOException {}
    }

    class T {
        void m() {
            try (R r = new R()) { }
            catch (IOException e) { }
        }
    }
    "#;
    ok(src);
}

// Declared throws IOException covers Closeable.close()
#[test]
fn twr_closeable_declared_ok() {
    let src = r#"
    package p;
    class Exception {}
    class IOException extends Exception {}
    interface AutoCloseable { void close() throws Exception; }
    interface Closeable extends AutoCloseable { void close() throws IOException; }

    class R implements Closeable {
        public void close() throws IOException {}
    }

    class T {
        void m() throws IOException {
            try (R r = new R()) { }
        }
    }
    "#;
    ok(src);
}

// AutoCloseable.close() throws Exception by contract
// Unhandled Exception from close() should be reported
#[test]
fn twr_autocloseable_unhandled_close_errors() {
    let src = r#"
    package p;
    class Exception {}
    interface AutoCloseable { void close() throws Exception; }

    class R implements AutoCloseable {
        public void close() throws Exception {}
    }

    class T {
        void m() {
            try (R r = new R()) { }
        }
    }
    "#;
    err_contains(src, "unreported checked exception 'Exception'");
}

// If the declared resource type's close() does not declare checked exceptions, no handling needed
#[test]
fn twr_declared_type_close_no_throws_ok() {
    let src = r#"
    package p;
    class Exception {}
    interface AutoCloseable { void close() throws Exception; }

    class R implements AutoCloseable {
        // Narrowed throws: no checked exceptions declared
        public void close() { }
    }

    class T {
        void m() {
            // Declared type is R whose close() declares no checked exceptions
            try (R r = new R()) { }
        }
    }
    "#;
    ok(src);
}

// Multiple resources: both close() may throw; catching Exception should cover all
#[test]
fn twr_multiple_resources_caught_ok() {
    let src = r#"
    package p;
    class Exception {}
    class IOException extends Exception {}
    interface AutoCloseable { void close() throws Exception; }
    interface Closeable extends AutoCloseable { void close() throws IOException; }

    class R1 implements Closeable { public void close() throws IOException {} }
    class R2 implements Closeable { public void close() throws IOException {} }

    class T {
        void m() {
            try (R1 a = new R1(); R2 b = new R2()) { }
            catch (Exception e) { }
        }
    }
    "#;
    ok(src);
}


