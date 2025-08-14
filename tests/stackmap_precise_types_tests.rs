use tolc::codegen::frame::{FrameBuilder, VerificationType};
use tolc::codegen::constpool::ConstantPool;

// Test: new; dup; invokespecial <init> and verify the post-init stack type is the exact owner class
#[test]
fn new_dup_invokespecial_init_preserves_owner_type_on_stack() {
    let mut cp = ConstantPool::new();
    let a_cls = cp.add_class("p/A");
    let init_mr = cp.add_method_ref("p/A", "<init>", "(I)V");

    // 0: new #a_cls
    // 3: dup
    // 4: iconst_1
    // 5: invokespecial #init_mr
    // 8: goto +4 -> 12
    // 11: aconst_null (pad so target=12 is an instruction start)
    // 12: return
    let code: Vec<u8> = vec![
        0xbb, ((a_cls >> 8) & 0xFF) as u8, (a_cls & 0xFF) as u8,
        0x59,
        0x04,
        0xb7, ((init_mr >> 8) & 0xFF) as u8, (init_mr & 0xFF) as u8,
        0xa7, 0x00, 0x04,
        0x01,
        0xb1,
    ];

    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "()V", &mut cp);
    assert_eq!(frames.frames.len(), 1, "expected one leader frame after goto target");

    match &frames.frames[0] {
        tolc::codegen::frame::StackMapFrame::SameLocals1StackItem { stack, .. } => {
            match stack {
                VerificationType::Object(idx) => assert_eq!(*idx, a_cls, "expected stack to hold Object(p/A) after <init>"),
                _ => panic!("expected Object on stack after <init>"),
            }
        }
        other => panic!("unexpected frame variant: {:?}", other),
    }
}

// Test: chained getfield precise typing across blocks: A.f -> B, then B.g -> java/lang/String
#[test]
fn getfield_chain_propagates_precise_reference_types_across_blocks() {
    let mut cp = ConstantPool::new();
    let _a_cls = cp.add_class("p/A");
    let _b_cls = cp.add_class("q/B");
    let _str_cls = cp.add_class("java/lang/String");
    let f_ref = cp.add_field_ref("p/A", "f", "Lq/B;");
    let g_ref = cp.add_field_ref("q/B", "g", "Ljava/lang/String;");

    // Method descriptor seeds local0 as Object(p/A)
    // 0: aload_0
    // 1: getfield #f_ref  -> stack: q/B
    // 4: getfield #g_ref  -> stack: java/lang/String
    // 7: goto +3 -> 10
    // 10: return
    let code: Vec<u8> = vec![
        0x2a,
        0xb4, ((f_ref >> 8) & 0xFF) as u8, (f_ref & 0xFF) as u8,
        0xb4, ((g_ref >> 8) & 0xFF) as u8, (g_ref & 0xFF) as u8,
        0xa7, 0x00, 0x03,
        0xb1,
    ];

    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "(Lp/A;)V", &mut cp);
    assert_eq!(frames.frames.len(), 1, "expected one leader frame at join");

    match &frames.frames[0] {
        tolc::codegen::frame::StackMapFrame::SameLocals1StackItem { stack, .. } => {
            match stack {
                VerificationType::Object(idx) => {
                    // java/lang/String index as seen by the CP
                    let str_idx = cp.add_class("java/lang/String");
                    assert_eq!(*idx, str_idx, "expected stack to hold Object(java/lang/String) after chained getfield");
                }
                _ => panic!("expected Object on stack after getfield chain"),
            }
        }
        other => panic!("unexpected frame variant: {:?}", other),
    }
}


