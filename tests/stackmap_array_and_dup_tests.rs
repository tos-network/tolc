use tolc::codegen::frame::{FrameBuilder, VerificationType};
use tolc::codegen::constpool::ConstantPool;

#[test]
fn aaload_from_string_array_pushes_string_type() {
    // static method with one arg: String[] a
    let mut cp = ConstantPool::new();
    let _owner = cp.add_class("p/T");
    // code: aload_0; iconst_0; aaload; goto +3; return
    let code: Vec<u8> = vec![0x2a, 0x03, 0x32, 0xa7, 0x00, 0x03, 0xb1];
    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "([Ljava/lang/String;)V", &mut cp);
    assert_eq!(frames.frames.len(), 1);
    match &frames.frames[0] {
        tolc::codegen::frame::StackMapFrame::SameLocals1StackItem { stack, .. } => {
            match stack {
                VerificationType::Object(idx) => {
                    let str_idx = cp.add_class("java/lang/String");
                    assert_eq!(*idx, str_idx, "aaload should push element type java/lang/String");
                }
                other => panic!("expected Object on stack, got {:?}", other),
            }
        }
        f => panic!("unexpected frame variant: {:?}", f),
    }
}

#[test]
fn dup_x_patterns_with_category2_do_not_panic_and_produce_frames() {
    let mut cp = ConstantPool::new();
    let _owner = cp.add_class("p/T");
    // Build several short sequences separated by gotos to create leaders
    // 1) iconst_1, lconst_0, dup_x1, goto +3
    // 2) lconst_0, lconst_0, dup2_x2, goto +3
    // 3) iconst_1, lconst_0, dup2_x1, goto +3
    // 4) iconst_1, iconst_0, dup_x2, goto +3
    // tail: return
    let mut code: Vec<u8> = Vec::new();
    // 1
    code.extend_from_slice(&[0x04, 0x09, 0x5a, 0xa7, 0x00, 0x03]);
    // 2
    code.extend_from_slice(&[0x09, 0x09, 0x5e, 0xa7, 0x00, 0x03]);
    // 3
    code.extend_from_slice(&[0x04, 0x09, 0x5d, 0xa7, 0x00, 0x03]);
    // 4
    code.extend_from_slice(&[0x04, 0x03, 0x5b, 0xa7, 0x00, 0x03]);
    // return
    code.push(0xb1);

    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "()V", &mut cp);
    assert!(frames.frames.len() >= 1);
}


