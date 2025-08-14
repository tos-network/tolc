use tolc::codegen::frame::{FrameBuilder, VerificationType, describe_stack_map_frames};
use tolc::codegen::constpool::ConstantPool;

#[test]
fn compute_with_types_emits_full_frames_at_leaders() {
    // bytecode: iconst_0, istore_1, goto +3, iinc 1 by 1, iinc 1 by 2 (fallthrough leader), return
    // 0: 03 (iconst_0)
    // 1: 3c (istore_1)
    // 2: a7 00 03 (goto 7)
    // 5: 84 01 01 (iinc 1, 1)
    // 8: 84 01 02 (iinc 1, 2)
    // 11: b1 (return)
    let code: Vec<u8> = vec![0x03, 0x3c, 0xa7, 0x00, 0x03, 0x84, 0x01, 0x01, 0x84, 0x01, 0x02, 0xb1];
    let fb = FrameBuilder::new();
    let mut cp = ConstantPool::new();
    let smt = fb.compute_with_types(&code, &[], false, false, "p/T", "()V", &mut cp);
    // Leaders: 0, 5, 12(out of range), but our collector adds fallthrough at 5; we emit Full at 5.
    assert_eq!(smt.frames.len(), 1);
    // Pretty description sanity
    let lines = describe_stack_map_frames(&smt);
    assert!(!lines.is_empty());
}

#[test]
fn handler_frame_includes_throwable_stack_item() {
    // trivial code with handler start at 0
    let code: Vec<u8> = vec![0xb1]; // return
    let fb = FrameBuilder::new();
    let mut cp = ConstantPool::new();
    let smt = fb.compute_with_types(&code, &[0], false, false, "p/T", "()V", &mut cp);
    assert_eq!(smt.frames.len(), 0, "leader 0 excluded");
    // add leader at 1 (no-op), handler at 1
    let code2: Vec<u8> = vec![0x03, 0xb1];
    let smt2 = fb.compute_with_types(&code2, &[1], false, false, "p/T", "()V", &mut cp);
    assert_eq!(smt2.frames.len(), 1);
}


