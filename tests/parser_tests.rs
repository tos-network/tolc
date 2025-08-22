use tolc::parser::parse_tol;
use tolc::ast::AstPrinter;

#[test]
fn parse_simple_class_with_main() {
    let source = r#"
package com.example;

public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);

    assert!(output.contains("class HelloWorld"));
    assert!(output.contains("main"));
}

#[test]
fn parse_with_imports_and_field_init() {
    let source = r#"
package com.example;

import java.util.List;
import java.util.ArrayList;

public class TestClass {
    private List<String> items = new ArrayList<>();
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);

    assert!(output.contains("import java.util.List"));
    assert!(output.contains("import java.util.ArrayList"));
    assert!(output.contains("class TestClass"));
}

#[test]
fn parse_class_with_annotations_and_generics() {
    let source = r#"
package p;

public class C<T, U> {
    @Deprecated
    private java.util.List<T> list = new java.util.ArrayList<>();

    @SuppressWarnings("x")
    public <K> void foo(@Deprecated T t, U u) {
        return;
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("class C"));
    assert!(output.contains("void foo"));
}

#[test]
fn parse_enum_with_constants_and_members() {
    let source = r#"
package p;

enum E {
    A,
    B,
    C;

    public void run() { return; }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("enum E"));
}

#[test]
fn parse_interface_with_extends_and_method() {
    let source = r#"
package p;

public interface I extends java.io.Closeable {
    void close();
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("interface I"));
}

#[test]
fn parse_new_with_args_and_return() {
    let source = r#"
package p;

class A {
    A() {}
    int f() {
        return 1;
    }
    void g() {
        java.util.List<String> xs = new java.util.ArrayList<>(1);
        return;
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("class A"));
}

#[test]
fn parse_varargs_and_arrays() {
    let source = r#"
package p;

class A {
    void h(int... xs) { return; }
    int[] arr;
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("class A"));
}

#[test]
fn parse_chained_qualified_call() {
    let source = r#"
package p;

class A {
    void p() {
        System.out.println("X");
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("class A"));
}

#[test]
fn parse_if_while_and_expressions() {
    let source = r#"
package p;

class A {
    void m() {
        if (1 < 2) { return; } else { return; }
        while (1 + 2 * 3 > 0 && 4 != 5) { return; }
        int x; x = 1; x = x + 2; x = -x; x = !x; // unary won't type-check but parse
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("class A"));
}

#[test]
fn parse_array_creation_and_init() {
    let source = r#"
package p;

class A {
    void m() {
        int[] a; a = new int[10];
        String[][] b; b = new String[1][2];
    }
}
"#;
    let ast = parse_tol(source).expect("Failed to parse");
    let mut printer = AstPrinter::new();
    let output = printer.print(&ast);
    assert!(output.contains("class A"));
}

#[test]
fn lambda_syntax_is_accepted() {
    let source = r#"
package p;
class A { void m(){ java.util.List<Integer> xs = null; xs.forEach(x -> {}); } }
"#;
    let ast = tolc::parser::parse_tol(source).unwrap();
    // Lambda expressions should now be successfully parsed
    assert!(ast.type_decls.len() == 1);
}

#[test]
fn method_reference_syntax_is_accepted() {
    let source = r#"
package p;
class A { void m(){ java.util.function.Function<String,Integer> f = String::length; } }
"#;
    let ast = tolc::parser::parse_tol(source).unwrap();
    // Method references should now be successfully parsed
    assert!(ast.type_decls.len() == 1);
}
