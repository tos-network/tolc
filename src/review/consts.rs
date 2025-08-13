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
    // VM/linkage errors typically unchecked
    "NoClassDefFoundError",
];


