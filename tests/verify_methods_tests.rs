use tolc::codegen::attribute::{NamedAttribute, AnnotationEntry, ConstUtf8Info};
use tolc::codegen::class::ClassFile;
use tolc::codegen::constpool::ConstantPool;
use tolc::codegen::method::MethodInfo;
use tolc::codegen::opcodes;
use tolc::codegen::typed_index::{ConstPoolIndex, ConstClassInfo};
use tolc::verify::{verify, VerifyError};
use tolc::codegen::flag::access_flags;
use tolc::codegen::frame::{StackMapFrame, VerificationType};

#[test]
fn verify_rejects_duplicate_exceptions_attribute_entries() {
    // Build minimal constant pool
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("Test");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");

    // Create duplicate exception class index
    let exc_class = cp.add_class("java/lang/Exception");
    let exc_typed_1: ConstPoolIndex<ConstClassInfo> = ConstPoolIndex::from(exc_class);
    let exc_typed_2: ConstPoolIndex<ConstClassInfo> = ConstPoolIndex::from(exc_class);
    let exceptions = vec![exc_typed_1, exc_typed_2]; // duplicate entries

    // Build method with Code and Exceptions attributes
    let mut method = MethodInfo::new(0, m_name, m_desc);
    let code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 1, vec![opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let ex_attr = NamedAttribute::new_exceptions_attribute(&mut cp, exceptions)
        .expect("create Exceptions attribute");
    method.attributes.push(code_attr);
    method.attributes.push(ex_attr);

    // Assemble ClassFile
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);

    // Verify should fail due to duplicate exceptions
    match verify(&cf) {
        Ok(_) => panic!("verify should fail for duplicate Exceptions entries"),
        Err(VerifyError::Internal(msg)) => {
            assert!(msg.contains("duplicate exceptions declared"), "unexpected error: {}", msg);
        }
        Err(other) => panic!("unexpected error variant: {}", other),
    }
}

#[test]
fn verify_rejects_duplicate_stackmaptable_inside_code() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");

    // Code with RETURN
    let mut method = MethodInfo::new(0, m_name, m_desc);
    let mut code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 1, vec![opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");

    // Two StackMapTable attributes attached to Code
    let smt1 = NamedAttribute::new_stack_map_table_attribute(&mut cp, Vec::<tolc::codegen::frame::StackMapFrame>::new())
        .expect("smt1");
    let smt2 = NamedAttribute::new_stack_map_table_attribute(&mut cp, Vec::<tolc::codegen::frame::StackMapFrame>::new())
        .expect("smt2");

    if let tolc::codegen::attribute::AttributeInfo::Code(ref mut code) = code_attr.info {
        code.attributes.push(smt1);
        code.attributes.push(smt2);
    } else {
        panic!("expected Code attribute variant");
    }

    method.attributes.push(code_attr);

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);

    match verify(&cf) {
        Ok(_) => panic!("verify should fail for duplicate StackMapTable inside Code"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("Duplicate method attribute: StackMapTable"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected error: {}", other),
    }
}

#[test]
fn verify_rejects_param_annotations_count_mismatch_visible() {
    // cp and class
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("(I)V"); // 1 parameter

    // Build method with Code and mismatched RuntimeVisibleParameterAnnotations (0 entries)
    let mut method = MethodInfo::new(0, m_name, m_desc);
    let code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 2, vec![opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let name_idx = cp.add_utf8("RuntimeVisibleParameterAnnotations");
    let name_typed: ConstPoolIndex<ConstUtf8Info> = ConstPoolIndex::from(name_idx);
    let rvpa = tolc::codegen::attribute::RuntimeVisibleParameterAnnotationsAttribute { annotations: vec![] };
    let rvpa_attr = NamedAttribute::new(name_typed, tolc::codegen::attribute::AttributeInfo::RuntimeVisibleParameterAnnotations(rvpa));
    method.attributes.push(code_attr);
    method.attributes.push(rvpa_attr);

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);

    match verify(&cf) {
        Ok(_) => panic!("verify should fail for mismatched RuntimeVisibleParameterAnnotations count"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("RuntimeVisibleParameterAnnotations count"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected error: {}", other),
    }
}

#[test]
fn verify_rejects_param_annotations_count_mismatch_invisible() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("(I)V"); // 1 parameter

    let mut method = MethodInfo::new(0, m_name, m_desc);
    let code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 2, vec![opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let name_idx = cp.add_utf8("RuntimeInvisibleParameterAnnotations");
    let name_typed: ConstPoolIndex<ConstUtf8Info> = ConstPoolIndex::from(name_idx);
    let dummy_ann = AnnotationEntry { type_name: ConstPoolIndex::from(cp.add_utf8("LAnno;")), retention: None, targets: vec![] };
    let ripa = tolc::codegen::attribute::RuntimeInvisibleParameterAnnotationsAttribute { annotations: vec![vec![dummy_ann], vec![]] }; // 2 entries != 1 param
    let ripa_attr = NamedAttribute::new(name_typed, tolc::codegen::attribute::AttributeInfo::RuntimeInvisibleParameterAnnotations(ripa));
    method.attributes.push(code_attr);
    method.attributes.push(ripa_attr);

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);

    match verify(&cf) {
        Ok(_) => panic!("verify should fail for mismatched RuntimeInvisibleParameterAnnotations count"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("RuntimeInvisibleParameterAnnotations count"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected error: {}", other),
    }
}

#[test]
fn verify_rejects_duplicate_signature_attribute() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");

    let mut method = MethodInfo::new(0, m_name, m_desc);
    let code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 1, vec![opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    // Use a trivially valid method signature per JVMS (no generics): ()V
    let sig1 = NamedAttribute::new_signature_attribute(&mut cp, "()V".to_string()).expect("sig1");
    let sig2 = NamedAttribute::new_signature_attribute(&mut cp, "()V".to_string()).expect("sig2");
    method.attributes.push(code_attr);
    method.attributes.push(sig1);
    method.attributes.push(sig2);

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);

    match verify(&cf) {
        Ok(_) => panic!("verify should fail for duplicate Signature attribute"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("Duplicate method attribute: Signature"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected error: {}", other),
    }
}

#[test]
fn verify_rejects_duplicate_exceptions_attribute_blocks() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");

    // Prepare a single exception class
    let exc_class = cp.add_class("java/lang/Exception");
    let e1: ConstPoolIndex<ConstClassInfo> = ConstPoolIndex::from(exc_class);

    let mut method = MethodInfo::new(0, m_name, m_desc);
    let code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 1, vec![opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let ex_a = NamedAttribute::new_exceptions_attribute(&mut cp, vec![e1]).expect("ex a");
    let ex_b = NamedAttribute::new_exceptions_attribute(&mut cp, vec![]).expect("ex b");
    method.attributes.push(code_attr);
    method.attributes.push(ex_a);
    method.attributes.push(ex_b);

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);

    match verify(&cf) {
        Ok(_) => panic!("verify should fail for duplicate Exceptions attribute blocks"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("Duplicate method attribute: Exceptions"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected error: {}", other),
    }
}

#[test]
fn stackmaptable_pc_locals_stack_within_bounds() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");

    let mut method = MethodInfo::new(access_flags::ACC_PUBLIC, m_name, m_desc);
    let mut code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 1, vec![0x03, 0x3c, 0x57, opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let smt = NamedAttribute::new_stack_map_table_attribute(&mut cp, vec![
        StackMapFrame::Same { offset_delta: 0 },
        StackMapFrame::Same { offset_delta: 1 },
        StackMapFrame::Same { offset_delta: 1 },
    ]).expect("smt");
    if let tolc::codegen::attribute::AttributeInfo::Code(ref mut code) = code_attr.info {
        code.attributes.push(smt);
    }
    method.attributes.push(code_attr);

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);
    assert!(verify(&cf).is_ok());
}

#[test]
fn stackmaptable_pc_beyond_code_is_rejected() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");
    let mut method = MethodInfo::new(access_flags::ACC_PUBLIC, m_name, m_desc);
    let mut code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 1, vec![opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let smt = NamedAttribute::new_stack_map_table_attribute(&mut cp, vec![
        StackMapFrame::Same { offset_delta: 10 }
    ]).expect("smt");
    if let tolc::codegen::attribute::AttributeInfo::Code(ref mut code) = code_attr.info {
        code.attributes.push(smt);
    }
    method.attributes.push(code_attr);
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);
    match verify(&cf) {
        Ok(_) => panic!("expected pc exceeds error"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("StackMapTable pc exceeds code length")),
        Err(other) => panic!("unexpected: {}", other),
    }
}

#[test]
fn stackmaptable_stack_exceeds_max_is_rejected() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");
    let mut method = MethodInfo::new(access_flags::ACC_PUBLIC, m_name, m_desc);
    let mut code_attr = NamedAttribute::new_code_attribute(&mut cp, 0, 1, vec![0x03, 0x57, opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let smt = NamedAttribute::new_stack_map_table_attribute(&mut cp, vec![
        StackMapFrame::SameLocals1StackItem { offset_delta: 0, stack: VerificationType::Integer }
    ]).expect("smt");
    if let tolc::codegen::attribute::AttributeInfo::Code(ref mut code) = code_attr.info {
        code.attributes.push(smt);
    }
    method.attributes.push(code_attr);
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);
    match verify(&cf) {
        Ok(_) => panic!("expected stack exceed error"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("StackMapTable stack exceed max_stack")),
        Err(other) => panic!("unexpected: {}", other),
    }
}

#[test]
fn stackmaptable_locals_exceeds_max_is_rejected() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");
    let m_name = cp.add_utf8("m");
    let m_desc = cp.add_utf8("()V");
    let mut method = MethodInfo::new(access_flags::ACC_PUBLIC, m_name, m_desc);
    let mut code_attr = NamedAttribute::new_code_attribute(&mut cp, 1, 0, vec![0x03, 0x57, opcodes::RETURN], vec![], vec![])
        .expect("create Code attribute");
    let smt = NamedAttribute::new_stack_map_table_attribute(&mut cp, vec![
        StackMapFrame::Append { k: 1, offset_delta: 0, locals: vec![VerificationType::Integer] }
    ]).expect("smt");
    if let tolc::codegen::attribute::AttributeInfo::Code(ref mut code) = code_attr.info {
        code.attributes.push(smt);
    }
    method.attributes.push(code_attr);
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.methods.push(method);
    match verify(&cf) {
        Ok(_) => panic!("expected locals exceed error"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("StackMapTable locals exceed max_locals")),
        Err(other) => panic!("unexpected: {}", other),
    }
}

