use tolc::parser::parse_tol;
use tolc::ast::AstPrinter;

#[test]
fn types_generics_arrays_varargs() {
    let source = r#"
package p;

import java.util.List;
import java.util.ArrayList;

class A<T> {
    private List<String> items = new ArrayList<>();
    void h(int... xs) { return; }
    int[] arr;
    A() {}
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut p = AstPrinter::new();
    let out = p.print(&ast);
    assert!(out.contains("class A"));
}

#[test]
fn types_interface_and_enum() {
    let source = r#"
package p;

public interface I extends java.io.Closeable {
    void close();
}

enum E {
    A, B;
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut p = AstPrinter::new();
    let out = p.print(&ast);
    assert!(out.contains("interface I"));
    assert!(out.contains("enum E"));
}
