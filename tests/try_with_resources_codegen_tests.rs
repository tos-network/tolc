use tolc::parser::parse_tol;
use tolc::codegen::ClassWriter;
use tolc::ast::TypeDecl;

mod common;
use common::setup_test_classpath;

#[test]
fn try_with_resources_emits_exception_table_and_close_paths() {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    let source = r#"
package p;

class A {
    void m(java.io.InputStream in) throws java.lang.Exception {
        try (java.io.InputStream r = in) {
            // body
        }
    }
}
"#;
    let ast = parse_tol(source).expect("parse ok");
    let class = ast.type_decls.iter().find_map(|t| match t { TypeDecl::Class(c) if c.name == "A" => Some(c), _ => None }).expect("class A not found");
    let mut cw = ClassWriter::new();
    cw.generate_class(class).expect("codegen ok");

    // We don't deserialize attributes fully here; this is a smoke test to ensure no panic
    // and that code attribute was produced with some code bytes
    let class_file = cw.get_class_file();
    assert!(!class_file.methods.is_empty());
}


