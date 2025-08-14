use tolc::codegen::class::ClassFile;
use tolc::codegen::constpool::ConstantPool;
use tolc::codegen::attribute::{NamedAttribute, AttributeInfo, TypeAnnotationEntry, RetentionPolicy, AnnotationTarget, RuntimeVisibleTypeAnnotationsAttribute, RuntimeInvisibleTypeAnnotationsAttribute, ConstUtf8Info};
use tolc::codegen::typed_index::ConstPoolIndex;
use tolc::verify::{verify, VerifyError};

fn make_cf() -> ClassFile {
    let mut cf = ClassFile::new();
    cf.major_version = 52; // Java 8
    cf
}

#[test]
fn type_use_runtime_visible_is_accepted() {
    let mut cf = make_cf();
    let mut cp = ConstantPool::new();
    // Add this_class to satisfy basic structure
    let this = cp.add_class("T");
    cf.this_class = this;

    // Build a RuntimeVisibleTypeAnnotations attribute with a TYPE_USE target and Runtime retention
    let ann_type = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("LAnno;"));
    let entry = TypeAnnotationEntry { type_name: ann_type, retention: Some(RetentionPolicy::Runtime), targets: vec![AnnotationTarget::TypeUse] };
    let attr = RuntimeVisibleTypeAnnotationsAttribute { annotations: vec![entry] };
    let name_idx = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("RuntimeVisibleTypeAnnotations"));
    cf.attributes.push(NamedAttribute::new(name_idx, AttributeInfo::RuntimeVisibleTypeAnnotations(attr)));

    cf.constant_pool = cp;
    assert!(verify(&cf).is_ok());
}

#[test]
fn type_use_runtime_in_invisible_set_is_rejected() {
    let mut cf = make_cf();
    let mut cp = ConstantPool::new();
    let this = cp.add_class("T");
    cf.this_class = this;

    let ann_type = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("LAnno;"));
    let entry = TypeAnnotationEntry { type_name: ann_type, retention: Some(RetentionPolicy::Runtime), targets: vec![AnnotationTarget::TypeUse] };
    let attr = RuntimeInvisibleTypeAnnotationsAttribute { annotations: vec![entry] };
    let name_idx = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("RuntimeInvisibleTypeAnnotations"));
    cf.attributes.push(NamedAttribute::new(name_idx, AttributeInfo::RuntimeInvisibleTypeAnnotations(attr)));

    cf.constant_pool = cp;
    match verify(&cf) {
        Err(e) => assert!(e.to_string().contains("Runtime retention type-annotation in invisible set"), "{e}"),
        other => panic!("expected attributes error, got {:?}", other),
    }
}

#[test]
fn class_level_type_annotation_invalid_target_rejected() {
    let mut cf = make_cf();
    let mut cp = ConstantPool::new();
    let this = cp.add_class("T");
    cf.this_class = this;

    let ann_type = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("LAnno;"));
    // Use Field target which is not valid on class-level type-annotations per our verify rule
    let entry = TypeAnnotationEntry { type_name: ann_type, retention: Some(RetentionPolicy::Class), targets: vec![AnnotationTarget::Field] };
    let attr = RuntimeVisibleTypeAnnotationsAttribute { annotations: vec![entry] };
    let name_idx = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("RuntimeVisibleTypeAnnotations"));
    cf.attributes.push(NamedAttribute::new(name_idx, AttributeInfo::RuntimeVisibleTypeAnnotations(attr)));

    cf.constant_pool = cp;
    match verify(&cf) {
        Err(e) => assert!(e.to_string().contains("Type annotation target not valid on class"), "{e}"),
        other => panic!("expected attributes error, got {:?}", other),
    }
}

#[test]
fn class_level_type_annotation_valid_targets_ok() {
    let mut cf = make_cf();
    let mut cp = ConstantPool::new();
    let this = cp.add_class("T");
    cf.this_class = this;

    let ann_type = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("LAnno;"));
    // Allow Type, TypeParameter, TypeUse targets
    let entry1 = TypeAnnotationEntry { type_name: ann_type.clone(), retention: Some(RetentionPolicy::Class), targets: vec![AnnotationTarget::Type] };
    let entry2 = TypeAnnotationEntry { type_name: ann_type.clone(), retention: Some(RetentionPolicy::Class), targets: vec![AnnotationTarget::TypeParameter] };
    let entry3 = TypeAnnotationEntry { type_name: ann_type, retention: Some(RetentionPolicy::Class), targets: vec![AnnotationTarget::TypeUse] };
    let attr = RuntimeVisibleTypeAnnotationsAttribute { annotations: vec![entry1, entry2, entry3] };
    let name_idx = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("RuntimeVisibleTypeAnnotations"));
    cf.attributes.push(NamedAttribute::new(name_idx, AttributeInfo::RuntimeVisibleTypeAnnotations(attr)));

    cf.constant_pool = cp;
    assert!(verify(&cf).is_ok());
}


