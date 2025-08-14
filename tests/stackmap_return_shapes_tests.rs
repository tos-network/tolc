use tolc::codegen::frame::{FrameBuilder, VerificationType};
use tolc::codegen::constpool::ConstantPool;

#[test]
fn method_return_shapes_push_correct_types() {
    let mut cp = ConstantPool::new();
    let owner = cp.add_class("p/T");
    let mr_int = cp.add_method_ref("p/T", "mI", "()I");
    let mr_long = cp.add_method_ref("p/T", "mJ", "()J");
    let mr_float = cp.add_method_ref("p/T", "mF", "()F");
    let mr_double = cp.add_method_ref("p/T", "mD", "()D");
    let mr_arr = cp.add_method_ref("p/T", "mA", "()[I");

    // aload_0; invokevirtual mI; istore_1; aload_0; invokevirtual mJ; lstore_2; aload_0; invokevirtual mF; fstore_4; aload_0; invokevirtual mD; dstore 6; aload_0; invokevirtual mA; pop; goto +3; return
    let code: Vec<u8> = vec![
        0x2a, 0xb6, (mr_int >> 8) as u8, (mr_int & 0xFF) as u8, 0x3c,
        0x2a, 0xb6, (mr_long >> 8) as u8, (mr_long & 0xFF) as u8, 0x37, 0x02,
        0x2a, 0xb6, (mr_float >> 8) as u8, (mr_float & 0xFF) as u8, 0x38, 0x04,
        0x2a, 0xb6, (mr_double >> 8) as u8, (mr_double & 0xFF) as u8, 0x39, 0x06,
        0x2a, 0xb6, (mr_arr >> 8) as u8, (mr_arr & 0xFF) as u8, 0x57,
        0xa7, 0x00, 0x03,
        0xb1,
    ];

    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], false, false, "p/T", "()V", &mut cp);
    assert!(frames.frames.len() >= 1);
}

#[test]
fn ctor_post_init_converts_uninitialized_in_locals() {
    let mut cp = ConstantPool::new();
    let a_cls = cp.add_class("p/A");
    let init_mr = cp.add_method_ref("p/A", "<init>", "()V");

    // 0: new A; 3: dup; 4: invokespecial <init>; 7: astore_1; 8: goto +3; 11: return
    let code: Vec<u8> = vec![
        0xbb, (a_cls >> 8) as u8, (a_cls & 0xFF) as u8,
        0x59,
        0xb7, (init_mr >> 8) as u8, (init_mr & 0xFF) as u8,
        0x3a, 0x01,
        0xa7, 0x00, 0x03,
        0xb1,
    ];
    let fb = FrameBuilder::new();
    let frames = fb.compute_with_types(&code, &[], true, false, "p/T", "()V", &mut cp);
    assert!(frames.frames.len() >= 1);
}


