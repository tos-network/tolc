// Global safety caps to prevent pathological or infinite loops

// Parser: maximum iterations for any guarded loop or overall passes
pub const PARSER_MAX_LOOP_ITERS: usize = 200_000;
// Parser: global gas limit (upper bound on total parser steps across a file)
pub const PARSER_MAX_GAS: usize = 5_000_000;

// Review: BFS/DFS hierarchy walks and per-pass iteration caps
pub const REVIEW_MAX_BFS_STEPS: usize = 200_000;
pub const REVIEW_MAX_HIERARCHY_STEPS: usize = 200_000;

// Verify: signature parser and related scanners
pub const VERIFY_MAX_SIGNATURE_ITERS: usize = 200_000;

// Well-known simple names from java.lang that should map to java/lang/* in descriptors
pub const JAVA_LANG_SIMPLE_TYPES: &[&str] = &[
    "String", "Object", "Throwable", "Cloneable", "Serializable",
    "Integer", "Long", "Float", "Double", "Boolean", "Character",
    "Short", "Byte", "Void",
];


