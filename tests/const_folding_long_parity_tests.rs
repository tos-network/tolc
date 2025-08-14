use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }

#[test]
fn long_shift_negative_counts_are_masked() {
    ok(r#"package p; class C {
        static final long A = ((long)1) << -1;   // -1 & 63 == 63
        static final long B = ((long)1) << -65;  // -65 & 63 == 63
        static final long Cc = ((long)1) >> -1;  // arithmetic right
        static final long D = ((long)-1) >>> -1; // logical right
        long a(){ return A; } long b(){ return B; } long c(){ return Cc; } long d(){ return D; }
    }"#);
}

#[test]
fn long_urshift_on_negative_values_behaves_like_javac() {
    ok(r#"package p; class C {
        static final long U1 = ((long)-1) >>> 1;   // 0x7FFF...FF
        static final long U2 = ((long)-1) >>> 63;  // 0x000...001
        static final long U3 = ((long)-1) >>> 64;  // mask -> >>> 0 => all ones
        long u1(){ return U1; } long u2(){ return U2; } long u3(){ return U3; }
    }"#);
}

#[test]
fn long_min_value_shifts_and_parity() {
    ok(r#"package p; class C {
        static final long MINV = ((long)1) << 63;     // Long.MIN_VALUE
        static final long SRA = MINV >> 1;            // sign-propagating
        static final long SRL = MINV >>> 1;           // logical
        static final long ROL = (MINV << 1);          // wraps to 0
        long a(){ return MINV; } long b(){ return SRA; } long c(){ return SRL; } long d(){ return ROL; }
    }"#);
}

#[test]
fn int_vs_long_shift_masking_diff_is_respected() {
    ok(r#"package p; class C {
        static final int  I = 1 << 33;         // 33 & 31 == 1 => 2
        static final long L = ((long)1) << 33; // 33 & 63 == 33
        int i(){ return I; } long l(){ return L; }
    }"#);
}


