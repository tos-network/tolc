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
    "String", "Object", "Throwable", "Cloneable", "Iterable",
    "Integer", "Long", "Float", "Double", "Boolean", "Character",
    "Short", "Byte", "Void", "Class", "RuntimeException", "Exception",
    "Error", "AssertionError", "UnsupportedOperationException",
];

// Centralized constants for review phase

// Base set of exceptions treated as unchecked in our analysis
// Note: AssertionError extends Error but is listed explicitly for clarity.
pub const UNCHECKED_BASE_EXCEPTIONS: &[&str] = &[
	"RuntimeException",
	"Error",
	"AssertionError",
];

// Common RuntimeException subclasses considered unchecked when compat mode is on
pub const UNCHECKED_COMMON_SUBCLASSES: &[&str] = &[
	"ArithmeticException",
	"NullPointerException",
	"ClassCastException",
	"ArrayIndexOutOfBoundsException",
	"IndexOutOfBoundsException",
	"ArrayStoreException",
	"NumberFormatException",
	"RevertException",
	"NegativeArraySizeException",
	"IllegalArgumentException",
	"IllegalStateException",
	"UnsupportedOperationException",
	"NoSuchElementException",
    "UnsupportedEncodingException",
    "StringIndexOutOfBoundsException",
    "SecurityException",
    "SecurityManager",
    "SecurityManager",
    "NoClassDefFoundError",
    "NoSuchMethodError",
    "NoSuchFieldError",
    "IncompatibleClassChangeError",
    "AbstractMethodError",
    "ClassCastException",
    "ClassNotFoundException",
    "ClassFormatError",
];

// Common checked exceptions
pub const CHECKED_EXCEPTIONS: &[&str] = &[
	"IOException",
	"FileNotFoundException",
	"ClassNotFoundException",
	"InstantiationException",
	"IllegalAccessException",
	"UnsupportedClassVersionError",
];

