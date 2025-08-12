## Fine-grained comparison: tolc vs javac (Java 8 semantics)

Legend: ✓ Covered, ◐ Partially covered, ✗ Missing

Notes
- Scope is Java 8. Newer features (Java 9+) are version-gated or rejected in tolc.
- tolc splits checks between AST-level review (pre-ClassFile) and ClassFile-level verify.

| Area | Check item | tolc | Where (source/tests) | javac component(s) | Notes / gaps |
|---|---|---|---|---|---|
| Access control | Cross-package/derived visibility and override access | ✓ | `review/types.rs`, `review/methods.rs` | Check/Resolve | Fully aligned: consider only inherited methods (public/protected; package-private only within same package; private never). Enforce no static/instance override/hide; no visibility reduction; interface implementations must be public; final non-overridable; return covariance; throws narrowing; interface defaults/abstracts and diamond conflicts handled. |
| Annotations | Retention=Runtime must be in visible set | ✓ | `verify/attributes.rs`; tests | Attr | — |
| Annotations | Type-annotation targets/context legality | ✓ | `verify/attributes.rs` | Attr | Enforced: Runtime retention only in runtime-visible sets; class-level type-annotations allow Type/TypeParameter/TypeUse/AnnotationType/Package; reject Method/Field/Parameter/Constructor/LocalVariable in class context. |
| BootstrapMethods | At most one; indices sanity | ✓ | `verify/constant_pool.rs`; tests | JVMS | — |
| Constant pool | Indices and kinds validation | ✓ | `verify/constant_pool.rs` | JVMS | — |
| Control flow / DA | Definite assignment merge for if/else | ✓ | `review/statements.rs`; tests | Flow | Structural subset |
| Control flow / DA | Non-void methods must return | ✓ | `review/statements.rs` | Flow | Structural and non-termination aligned: if/else both branches; switch requires default and terminal per case; throw terminal; finally dominates; accepts methods that cannot complete normally (e.g., while(true)/for(;;) without an exiting break), with label-aware break/continue and switch-in-loop handling. |
| Control flow / DA | try/catch/finally DA merge | ✓ | `review/statements.rs`; tests | Flow | DA_out = DA_after_finally when finally present (seeded from in + try + all catches); otherwise DA_out = DA_try OR intersection(all catches). Matches javac for typical forms incl. TWR. |
| Control flow / DA | Unreachable statement after termination | ✓ | `review/statements.rs` | Flow | Basic |
| Control flow / DA | while/for (incl. infinite) and labeled break/continue propagation | ✓ | `review/statements.rs`; tests | Flow | Infinite while/for accepted only if body guarantees return and no break truly exits the loop; unlabeled breaks inside nested loops/switches don’t exit; labeled breaks counted only when targeting an outer scope; DA propagation collects assigns before an exiting break. Matches javac on typical patterns. |
| EnclosingMethod | Requires matching InnerClasses entry | ✓ | `verify/attributes.rs`; tests | JVMS | — |
| Exceptions | Checked exceptions report (throws/catch coverage) | ✓ | `review/statements.rs`; tests: `review_exceptions_tests.rs`, `review_try_with_resources_tests.rs`, `review_multicatch_generic_exceptions_tests.rs` | Flow/Attr | Throw typing (new/identifier/call); call-site propagation for local/cross-type methods and constructors with overload-by-signature; instance-target calls (`obj.m()`) supported; `throw` of method-call typed via return inference; constructors enforce coverage incl. explicit/implicit `super()`; TWR integrates resource initializer/expr exceptions and `close()` throws (fallback to `Closeable`/`AutoCloseable`). Supports multi-catch and precise rethrow. Generic/TypeVar-based throws are mapped to their upper bounds for coverage. |
| Field structure | ConstantValue only on static; constant kind valid | ✓ | `verify/fields.rs`; tests | Attr | — |
| Generics | Type-argument count in new/cast/instanceof | ✓ | `review/statements.rs`; tests | Attr | Strict match; in compat mode only raw (0 args) usage is tolerated |
| Generics | Upper-bound checks (local index/explicit imports) | ✓ | `review/types.rs`, `review/statements.rs`; tests: `review_generics_upper_bounds_tests.rs` | Attr | Bounds validated across new/cast/instanceof, including intersection (A & I) upper bounds; wildcard erasure subset: `? extends B` treated as `B`, bare `?` as `Object`; wildcards disallowed in constructor type args. Full capture/inference remains out of scope. |
| indy/dynamic | Dynamic/InvokeDynamic require BootstrapMethods | ✓ | `verify/constant_pool.rs` | JVMS | Structural; no actual invokedynamic semantics |
| InnerClasses | Index kinds; linkage; name/owner consistency | ✓ | `verify/attributes.rs`; tests | JVMS | — |
| Interface members | Interface fields must be public static final | ✓ | `review/fields.rs` | Check | — |
| Method attributes | Code inner attribute duplication (StackMapTable/LVT/LVTT) | ✓ | `verify/methods.rs` | Attr | — |
| Method attributes | Exceptions: duplicate entries/duplicate attribute blocks | ✓ | `verify/methods.rs`; tests | Attr | — |
| Method attributes | Parameter annotations count matches parameter count | ✓ | `verify/methods.rs`; tests | Attr | — |
| Method names/desc | Utf8 index validity | ✓ | `verify/methods.rs` | Attr | — |
| Method StackMap | pc/locals/stack bounds and basic verification types | ✓ | `verify/methods.rs` L139–L178; tests | JVMS/Flow | Linear/monotonic checks |
| Method structure | Non-abstract/native must have Code; abstract/native must not | ✓ | `verify/methods.rs` | Attr | — |
| Method structure | Return opcode matches descriptor | ✓ | `verify/methods.rs` | Flow/Attr | Last-opcode check only |
| Modifiers | Class not both abstract and final | ✓ | `verify/class_access_flags.rs`; `review/types.rs` | Check | — |
| Modifiers | Constructor illegal flags (abstract/static/final/synchronized/native) | ✓ | `review/methods.rs` | Check | — |
| Modifiers | Field cannot be both final and volatile | ✓ | `review/fields.rs` | Check | — |
| Modifiers | Field visibility exclusivity | ✓ | `verify/fields.rs` | Check | — |
| Modifiers | Interface method illegal flags (private/protected/final) | ✓ | `review/methods.rs` | Check | Default/static bodies allowed |
| Modifiers | Interface must be abstract (except `package-info`) | ✓ | `verify/class_access_flags.rs` | Check | — |
| Modifiers | Method visibility exclusivity (public/protected/private) | ✓ | `verify/method_access_flags.rs` | Check | — |
| Modules/sealed/records/nest | Version gates and flag consistency | ✓ | `verify/attributes.rs`; `class_access_flags.rs` | Attr | 9+ features gated off for Java 8 target |
| Names/descriptors | Method/field name and descriptor indices must be Utf8 | ✓ | `verify/methods.rs`; `verify/fields.rs` | Attr | — |
| Overload resolution | Ambiguity detection and “more specific” tie-break | ✓ | `review/statements.rs`; tests: `review_overload_resolution_tests.rs` | Resolve | Implemented tie-break order: exact conversions first; then minimal number of conversions; then minimal widening cost; then prefer fixed arity over varargs; otherwise report ambiguity. Reference assignability considered for specificity when available. |
| Overload resolution | Arity match; varargs minimum arity | ✓ | `review/statements.rs` | Resolve | Local class/static imports |
| Overload resolution | Literal-driven applicability (widen/box/unbox) and simple ranking | ◐ | `review/statements.rs` | Resolve/Attr | Primitive + String; boxing/unboxing and simple reference assignability via hierarchy; varargs handled with same conversion model; arity-only fallback in compat mode; simplified cost model |
| Packages/imports | Duplicate imports | ✓ | `review/imports.rs`; tests: `review_import_tests.rs` | Enter/Check | — |
| Packages/imports | Non-empty package name when present | ✓ | `review/package.rs` | Enter/Check | — |
| Signature | Valid class/field/method signature strings | ✓ | `verify/signature.rs`; `verify/attributes.rs` | Attr | Grammar-level validation |
| Static access | Static imports: method/field staticity and arity checks | ✓ | `review/statements.rs`, `review/fields.rs`; tests | Resolve/Enter | — |
| Static access | `TypeName.m(...)` / `TypeName.f` must be static | ✓ | `review/statements.rs`, `review/fields.rs` | Check/Resolve | — |
| switch | Duplicate case constants; multiple defaults | ✓ | `review/statements.rs`; tests | Flow | int constant folding subset |
| switch | Effect of default on DA | ◐ | `review/statements.rs`; tests | Flow | Subset |
| Types/declarations | Duplicate type names in a CU | ✓ | `review/types.rs` | Enter/Check | — |
| Types/declarations | Empty type names rejected | ✓ | `review/types.rs` | Check | — |
| Types/declarations | Interface cannot be final | ✓ | `verify/mod.rs` | Check | — |
| Types/declarations | Nested type declarations (inner classes/interfaces/enums) are reviewed | ✓ | `review/types.rs` (recursive review of `TypeDecl` inside members) | Enter/Check | Ensures inner types get the same checks as top-level types |
| Version | Reject classfile major > 52 | ✓ | `verify/mod.rs` | Target/Source | Locked to Java 8 |

Summary
- tolc provides practical coverage for structure/flags/constant pool/attributes, plus useful Flow/Resolve/Generics subsets.
- Remaining notable gaps vs javac: precise rethrow/multi-catch rules; nuanced protected/package rules across packages; complete overload ranking and generics inference/capture beyond the implemented subset.


