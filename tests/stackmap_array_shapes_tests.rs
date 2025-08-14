use tolc::codegen::frame::{FrameBuilder, VerificationType, StackMapFrame};
use tolc::codegen::constpool::ConstantPool;

#[test]
fn aaload_from_multidim_int_array_pushes_int_array_ref() {
    // Method arg: int[][]
    // code: aload_0; iconst_0; aaload; goto +3; return
    let mut cp = ConstantPool::new();
    let _owner = cp.add_class("p/T");
    let code: Vec<u8> = vec![0x2a, 0x03, 0x32, 0xa7, 0x00, 0x03, 0xb1];
    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "([[I)V", &mut cp);
    assert_eq!(frames.frames.len(), 1);
    match &frames.frames[0] {
        StackMapFrame::SameLocals1StackItem { stack, .. } | StackMapFrame::SameLocals1StackItemExtended { stack, .. } => {
            match stack {
                VerificationType::Object(idx) => {
                    let int_arr = cp.add_class("[I");
                    assert_eq!(*idx, int_arr, "aaload on [[I should push [I reference type");
                }
                other => panic!("expected Object([I) on stack, got {:?}", other),
            }
        }
        f => panic!("unexpected frame variant: {:?}", f),
    }
}

#[test]
fn aaload_chain_from_multidim_string_array_pushes_string() {
    // arg: String[][], code: aload_0; iconst_0; aaload; iconst_0; aaload; goto +3; return
    let mut cp = ConstantPool::new();
    let _owner = cp.add_class("p/T");
    let code: Vec<u8> = vec![0x2a, 0x03, 0x32, 0x03, 0x32, 0xa7, 0x00, 0x03, 0xb1];
    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "([[Ljava/lang/String;)V", &mut cp);
    assert_eq!(frames.frames.len(), 1);
    match &frames.frames[0] {
        StackMapFrame::SameLocals1StackItem { stack, .. } | StackMapFrame::SameLocals1StackItemExtended { stack, .. } => {
            match stack {
                VerificationType::Object(idx) => {
                    let s = cp.add_class("java/lang/String");
                    assert_eq!(*idx, s, "aaload chain on [[Ljava/lang/String; should push java/lang/String");
                }
                other => panic!("expected Object(java/lang/String) on stack, got {:?}", other),
            }
        }
        f => panic!("unexpected frame variant: {:?}", f),
    }
}

#[test]
fn iaload_from_int_array_pushes_integer() {
    // arg: int[]
    // code: aload_0; iconst_0; iaload; goto +3; return
    let mut cp = ConstantPool::new();
    let _owner = cp.add_class("p/T");
    let code: Vec<u8> = vec![0x2a, 0x03, 0x2e, 0xa7, 0x00, 0x03, 0xb1];
    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "([I)V", &mut cp);
    assert_eq!(frames.frames.len(), 1);
    match &frames.frames[0] {
        StackMapFrame::SameLocals1StackItem { stack, .. } | StackMapFrame::SameLocals1StackItemExtended { stack, .. } => {
            assert!(matches!(stack, VerificationType::Integer));
        }
        f => panic!("unexpected frame variant: {:?}", f),
    }
}

#[test]
fn deep_init_with_dup_x1_converts_all_uninitialized_copies() {
    // new A; dup; dup_x1; iconst_1; invokespecial <init>(I)V; pop; goto +3; return
    let mut cp = ConstantPool::new();
    let a_cls = cp.add_class("p/A");
    let init = cp.add_method_ref("p/A", "<init>", "(I)V");
    let mut code: Vec<u8> = Vec::new();
    // new A
    code.extend_from_slice(&[0xbb, (a_cls >> 8) as u8, (a_cls & 0xFF) as u8]);
    // dup; dup_x1; iconst_1
    code.extend_from_slice(&[0x59, 0x5a, 0x04]);
    // invokespecial <init>(I)V
    code.extend_from_slice(&[0xb7, (init >> 8) as u8, (init & 0xFF) as u8]);
    // pop to leave one converted reference
    code.push(0x57);
    // goto +3; return
    code.extend_from_slice(&[0xa7, 0x00, 0x03, 0xb1]);

    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "()V", &mut cp);
    assert_eq!(frames.frames.len(), 1);
    match &frames.frames[0] {
        StackMapFrame::SameLocals1StackItem { stack, .. } | StackMapFrame::SameLocals1StackItemExtended { stack, .. } => {
            match stack {
                VerificationType::Object(idx) => assert_eq!(*idx, a_cls, "remaining ref after <init> must be converted to Object(p/A)"),
                other => panic!("expected Object(p/A) on stack, got {:?}", other),
            }
        }
        f => panic!("unexpected frame variant: {:?}", f),
    }
}


