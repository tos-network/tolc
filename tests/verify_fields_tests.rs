use tolc::codegen::attribute::NamedAttribute;
use tolc::codegen::class::ClassFile;
use tolc::codegen::constpool::ConstantPool;
use tolc::verify::{verify, VerifyError};

#[test]
fn verify_field_constantvalue_requires_static_and_valid_kind() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let name = cp.add_utf8("x");
    let desc = cp.add_utf8("I");

    // constant value: integer
    let int_idx = cp.add_integer(42);
    let cv_attr = NamedAttribute::new_const_value_attribute(&mut cp, tolc::codegen::typed_index::ConstPoolIndex::from(int_idx)).unwrap();

    // Build field: non-static should fail
    let mut cf = ClassFile::new();
    cf.constant_pool = cp.clone();
    cf.this_class = this_class;
    cf.fields.push(tolc::codegen::field::FieldInfo { access_flags: 0, name_index: name, descriptor_index: desc, attributes: vec![cv_attr.clone()] });

    match verify(&cf) {
        Ok(_) => panic!("verify should fail: ConstantValue on non-static field"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("ConstantValue on non-static")),
        Err(e) => panic!("unexpected: {}", e),
    }

    // Make static: should pass
    if let Some(f) = cf.fields.get_mut(0) { f.access_flags = tolc::codegen::flag::access_flags::ACC_STATIC; }
    assert!(verify(&cf).is_ok());
}


