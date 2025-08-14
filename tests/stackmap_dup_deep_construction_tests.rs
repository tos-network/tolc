use tolc::codegen::frame::{FrameBuilder, StackMapFrame, VerificationType};
use tolc::codegen::constpool::ConstantPool;

#[test]
fn store_before_init_converts_local_on_invokespecial() {
    // new A; dup; astore_1; dup; iconst_1; invokespecial <init>(I)V; aload_1; goto +3; return
    let mut cp = ConstantPool::new();
    let a_cls = cp.add_class("p/A");
    let init_mr = cp.add_method_ref("p/A", "<init>", "(I)V");
    let code: Vec<u8> = vec![
        0xbb, (a_cls >> 8) as u8, (a_cls & 0xFF) as u8,
        0x59, // dup (U)
        0x3a, 0x01, // astore_1 (locals[1] = U)
        0x59, // dup again to keep receiver for <init>
        0x04, // iconst_1
        0xb7, (init_mr >> 8) as u8, (init_mr & 0xFF) as u8, // invokespecial <init>(I)V converts locals/stack U->Object(A)
        0x19, 0x01, // aload_1 pushes converted Object(A)
        0xa7, 0x00, 0x03, // goto +3
        0xb1, // return
    ];
    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "()V", &mut cp);
    assert_eq!(frames.frames.len(), 1);
    match &frames.frames[0] {
        StackMapFrame::Full { stack, .. } => {
            assert!(stack.len() >= 1);
            match &stack[0] {
                VerificationType::Object(idx) => assert_eq!(*idx, a_cls, "top of stack should be Object(p/A) after <init> and aload_1"),
                other => panic!("expected Object(p/A) on stack, got {:?}", other),
            }
        }
        other => panic!("unexpected frame variant: {:?}", other),
    }
}

#[test]
fn deep_dup_chain_converts_remaining_stack_copies() {
    // new A; dup; dup_x1; dup_x2; iconst_1; invokespecial <init>(I)V; pop; goto +3; return
    // After <init>, all remaining copies of U on stack should be converted to Object(A). We pop one, leaving one.
    let mut cp = ConstantPool::new();
    let a_cls = cp.add_class("p/A");
    let init_mr = cp.add_method_ref("p/A", "<init>", "(I)V");
    let code: Vec<u8> = vec![
        0xbb, (a_cls >> 8) as u8, (a_cls & 0xFF) as u8, // new A -> U
        0x59,             // dup -> U,U
        0x5a,             // dup_x1 -> U,U,U
        0x5b,             // dup_x2 -> U,U,U,U
        0x04,             // iconst_1
        0xb7, (init_mr >> 8) as u8, (init_mr & 0xFF) as u8, // invokespecial <init>(I)V converts remaining Us
        0x57,             // pop one Object(A), leaving one
        0xa7, 0x00, 0x03, // goto +3
        0xb1,             // return
    ];
    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "()V", &mut cp);
    assert_eq!(frames.frames.len(), 1);
    match &frames.frames[0] {
        StackMapFrame::Full { stack, .. } => {
            assert_eq!(stack.len(), 2, "expect two remaining copies after pop");
            for s in stack {
                match s {
                    VerificationType::Object(idx) => assert_eq!(*idx, a_cls, "each remaining copy should be Object(p/A) after <init>"),
                    other => panic!("expected Object(p/A) on stack, got {:?}", other),
                }
            }
        }
        other => panic!("unexpected frame variant: {:?}", other),
    }
}


