use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }

// Mixed int/long relational comparisons near boundaries should fold correctly
#[test]
fn mixed_int_long_relops_at_boundaries_fold() {
    ok(r#"package p; class C {
        // int max vs long just above int max
        static final boolean B1 = 2147483647 < 2147483648L;
        // negative int vs large positive long
        static final boolean B2 = -1 < 4294967295L;
        // Long.MIN_VALUE vs int MIN
        static final boolean B3 = (1L<<63) < (1<<31);
        // Long.MAX_VALUE vs int MAX
        static final boolean B4 = ((1L<<63)-1L) > 2147483647;
        // int MIN vs 0L
        static final boolean B5 = (1<<31) < 0L;
        // equality across int/long zero
        static final boolean B6 = 0L >= 0;
        boolean b1(){ return B1; } boolean b2(){ return B2; } boolean b3(){ return B3; }
        boolean b4(){ return B4; } boolean b5(){ return B5; } boolean b6(){ return B6; }
    }"#);
}


