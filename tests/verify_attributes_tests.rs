use tolc::codegen::class::ClassFile;
use tolc::codegen::constpool::ConstantPool;
use tolc::codegen::attribute::{NamedAttribute, AttributeInfo, AnnotationEntry, ConstUtf8Info};
use tolc::verify::{verify, VerifyError};
use tolc::codegen::typed_index::ConstPoolIndex;

#[test]
fn innerclasses_indices_and_linkage_validated() {
    let mut cp = ConstantPool::new();
    let outer = cp.add_class("Outer");
    let inner = cp.add_class("Outer$Inner");
    let inner_name = cp.add_utf8("Inner");
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = inner;

    // Build InnerClasses attribute with one entry for this class
    let entry = tolc::codegen::attribute::InnerClassInfo {
        inner_class: ConstPoolIndex::from(inner),
        outer_class: ConstPoolIndex::from(outer),
        inner_name: ConstPoolIndex::from(inner_name),
        inner_class_access_flags: 0,
    };
    let ic = tolc::codegen::attribute::InnerClassesAttribute { classes: vec![entry] };
    let ic_attr = NamedAttribute::new(ConstPoolIndex::<ConstUtf8Info>::from(cf.constant_pool.add_utf8("InnerClasses")), AttributeInfo::InnerClasses(ic));
    cf.attributes.push(ic_attr);
    assert!(verify(&cf).is_ok());
}

#[test]
fn innerclasses_name_owner_mismatch_is_rejected() {
    let mut cp = ConstantPool::new();
    let outer = cp.add_class("pkg/Outer");
    let wrong_outer = cp.add_class("pkg/Wrong");
    let inner = cp.add_class("pkg/Outer$Inner");
    let inner_name = cp.add_utf8("Inner");
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = outer; // arbitrary

    // InnerClasses entry says owner is Wrong but inner name suggests Outer$Inner -> mismatch
    let entry = tolc::codegen::attribute::InnerClassInfo {
        inner_class: ConstPoolIndex::from(inner),
        outer_class: ConstPoolIndex::from(wrong_outer),
        inner_name: ConstPoolIndex::from(inner_name),
        inner_class_access_flags: 0,
    };
    let ic = tolc::codegen::attribute::InnerClassesAttribute { classes: vec![entry] };
    let ic_attr = NamedAttribute::new(ConstPoolIndex::<ConstUtf8Info>::from(cf.constant_pool.add_utf8("InnerClasses")), AttributeInfo::InnerClasses(ic));
    cf.attributes.push(ic_attr);
    match verify(&cf) {
        Ok(_) => panic!("expected name/owner mismatch error"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("InnerClasses name/owner mismatch"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected: {}", other),
    }
}

#[test]
fn enclosingmethod_requires_matching_innerclasses_entry() {
    let mut cp = ConstantPool::new();
    let outer = cp.add_class("Outer");
    let inner = cp.add_class("Outer$Inner");
    let nm = cp.add_name_and_type("m", "()V");
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = inner;
    // EnclosingMethod without InnerClasses -> error
    let em = tolc::codegen::attribute::EnclosingMethodAttribute { class: ConstPoolIndex::from(outer), method: ConstPoolIndex::from(nm) };
    let em_attr = NamedAttribute::new(ConstPoolIndex::<ConstUtf8Info>::from(cf.constant_pool.add_utf8("EnclosingMethod")), AttributeInfo::EnclosingMethod(em));
    cf.attributes.push(em_attr);
    match verify(&cf) {
        Ok(_) => panic!("expected error"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("EnclosingMethod without InnerClasses")),
        Err(other) => panic!("unexpected: {}", other),
    }
}

#[test]
fn runtime_retention_must_be_in_visible_set() {
    let mut cp = ConstantPool::new();
    let this = cp.add_class("T");
    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this;
    let ann = AnnotationEntry { type_name: ConstPoolIndex::from(cf.constant_pool.add_utf8("LAnno;")), retention: Some(tolc::codegen::attribute::RetentionPolicy::Runtime), targets: vec![] };
    // Put runtime retention annotation into invisible set -> error
    let ria = tolc::codegen::attribute::RuntimeInvisibleAnnotationsAttribute { annotations: vec![ann] };
    let ria_attr = NamedAttribute::new(ConstPoolIndex::<ConstUtf8Info>::from(cf.constant_pool.add_utf8("RuntimeInvisibleAnnotations")), AttributeInfo::RuntimeInvisibleAnnotations(ria));
    cf.attributes.push(ria_attr);
    match verify(&cf) {
        Ok(_) => panic!("expected retention error"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("Runtime retention annotation in invisible set")),
        Err(other) => panic!("unexpected: {}", other),
    }
}

use tolc::codegen::attribute::{ModuleAttribute, ModuleRequires, ModuleExports, ModuleOpens, ModuleUses, ModuleProvides};

#[test]
fn verify_rejects_module_attribute_below_java9() {
    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");

    // Build a minimal Module attribute (fields can be empty vectors)
    let name = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("m"));
    let version = ConstPoolIndex::<ConstUtf8Info>::from(cp.add_utf8("1"));
    let module = ModuleAttribute {
        name,
        flags: 0,
        version,
        requires: Default::default(),
        exports: Default::default(),
        opens: Default::default(),
        uses: Default::default(),
        provides: Default::default(),
    };
    let mod_attr = NamedAttribute::new(ConstPoolIndex::from(cp.add_utf8("Module")), tolc::codegen::attribute::AttributeInfo::Module(module));

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.major_version = 52; // Java 8
    cf.attributes.push(mod_attr);

    match verify(&cf) {
        Ok(_) => panic!("verify should fail for Module attribute on Java < 9"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("requires class file version >= 53"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected error: {}", other),
    }
}

#[test]
fn verify_rejects_duplicate_bootstrapmethods_attribute() {
    use tolc::codegen::attribute::BootstrapMethodsAttribute;
    use tolc::codegen::attribute::BootstrapMethod;

    let mut cp = ConstantPool::new();
    let this_class = cp.add_class("T");

    // minimal BootstrapMethods with a class index
    let helper_class = cp.add_class("H");
    // helper_idx not needed; we validate indices shape in verifier
    // Our attribute types use ConstClassInfo for indices, adapt accordingly if different
    let bm = BootstrapMethod {
        bootstrap_method: tolc::codegen::typed_index::ConstPoolIndex::<tolc::codegen::typed_index::ConstClassInfo>::from(helper_class),
        bootstrap_arguments: vec![],
    };
    let bsm1 = NamedAttribute::new(ConstPoolIndex::from(cp.add_utf8("BootstrapMethods")), tolc::codegen::attribute::AttributeInfo::BootstrapMethods(BootstrapMethodsAttribute { bootstrap_methods: vec![bm.clone()] }));
    let bsm2 = NamedAttribute::new(ConstPoolIndex::from(cp.add_utf8("BootstrapMethods")), tolc::codegen::attribute::AttributeInfo::BootstrapMethods(BootstrapMethodsAttribute { bootstrap_methods: vec![bm] }));

    let mut cf = ClassFile::new();
    cf.constant_pool = cp;
    cf.this_class = this_class;
    cf.attributes.push(bsm1);
    cf.attributes.push(bsm2);

    match verify(&cf) {
        Ok(_) => panic!("verify should fail for duplicate BootstrapMethods"),
        Err(VerifyError::Internal(msg)) => assert!(msg.contains("Duplicate BootstrapMethods attribute"), "unexpected: {}", msg),
        Err(other) => panic!("unexpected error: {}", other),
    }
}


