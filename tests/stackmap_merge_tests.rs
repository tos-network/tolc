use tolc::codegen::frame::{VerificationType, FrameState, merge_type};
use tolc::codegen::constpool::ConstantPool;

#[test]
fn merge_object_and_null() {
    let mut cp = ConstantPool::new();
    let a_idx = cp.add_class("p/A");
    let a = VerificationType::Object(a_idx);
    let null = VerificationType::Null;
    let m = merge_type(&a, &null, &mut cp);
    match m { VerificationType::Object(x) => assert_eq!(x, a_idx), _ => panic!("expected Object(p/A)") }
}

#[test]
fn merge_two_distinct_objects_to_java_lang_object() {
    let mut cp = ConstantPool::new();
    let a_idx = cp.add_class("p/A");
    let b_idx = cp.add_class("q/B");
    let a = VerificationType::Object(a_idx);
    let b = VerificationType::Object(b_idx);
    let m = merge_type(&a, &b, &mut cp);
    match m { VerificationType::Object(x) => {
        let obj_idx = cp.add_class("java/lang/Object");
        assert_eq!(x, obj_idx);
    } _ => panic!("expected Object(java/lang/Object)") }
}

#[test]
fn merge_primitives_and_top() {
    let mut cp = ConstantPool::new();
    use VerificationType::*;
    assert!(matches!(merge_type(&Integer, &Long, &mut cp), Top));
    assert!(matches!(merge_type(&Float, &Double, &mut cp), Top));
    assert!(matches!(merge_type(&Top, &Integer, &mut cp), Top));
}

#[test]
fn merge_frame_states_element_wise() {
    let mut cp = ConstantPool::new();
    let a_idx = cp.add_class("p/A");
    let b_idx = cp.add_class("q/B");
    let s1 = FrameState::new(
        vec![VerificationType::Integer, VerificationType::Object(a_idx)],
        vec![VerificationType::Float]
    );
    let s2 = FrameState::new(
        vec![VerificationType::Integer, VerificationType::Object(b_idx)],
        vec![VerificationType::Float, VerificationType::Double]
    );
    let m = s1.merge_with(&s2, &mut cp);
    assert_eq!(m.locals.len(), 2);
    match &m.locals[0] { VerificationType::Integer => {}, _ => panic!("expected int") }
    match &m.locals[1] { VerificationType::Object(idx) => {
        let obj_idx = cp.add_class("java/lang/Object");
        assert_eq!(*idx, obj_idx);
    } _ => panic!("expected Object(java/lang/Object)") }
    assert_eq!(m.stack.len(), 2);
    match &m.stack[0] { VerificationType::Float => {}, _ => panic!("expected float") }
    match &m.stack[1] { VerificationType::Top => {}, _ => panic!("expected Top for missing slot merge") }
}


