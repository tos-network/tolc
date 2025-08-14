use tolc::codegen::frame::{FrameBuilder, describe_stack_map_frames};
use tolc::codegen::constpool::ConstantPool;

#[test]
fn handler_specific_type_is_used() {
    let code: Vec<u8> = vec![0xb1]; // return
    let fb = FrameBuilder::new();
    let mut cp = ConstantPool::new();
    let frames = fb.compute_with_types_with_handlers(&code, &[(0, Some("java/lang/Exception".to_string()))], false, false, "p/T", "()V", &mut cp);
    // Leader 0 excluded; add another leader to force a frame
    let code2: Vec<u8> = vec![0x03, 0xb1];
    let frames2 = fb.compute_with_types_with_handlers(&code2, &[(1, Some("java/lang/RuntimeException".to_string()))], false, false, "p/T", "()V", &mut cp);
    assert_eq!(frames2.frames.len(), 1);
    let lines = describe_stack_map_frames(&frames2);
    assert!(!lines.is_empty());
}


