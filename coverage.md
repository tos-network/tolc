## Fine-grained comparison: tolc vs javac (Java 8 semantics)

Legend: ✓ Covered, ◐ Partially covered, ✗ Missing

Notes
- Scope is Java 8. Newer features (Java 9+) are version-gated or rejected in tolc.
- tolc splits checks between AST-level review (pre-ClassFile) and ClassFile-level verify.

| Area | Check item | tolc | Where (source/tests) | javac component(s) | Notes / gaps |
|---|---|---|---|---|---|
| Packages/imports | Duplicate imports | ✓ | `review/imports.rs`; tests: `review_import_tests.rs` | Enter/Check | — |
| Packages/imports | Non-empty package name when present | ✓ | `review/package.rs` | Enter/Check | — |
| Types/declarations | Duplicate type names in a CU | ✓ | `review/types.rs` | Enter/Check | — |
| Types/declarations | Empty type names rejected | ✓ | `review/types.rs` | Check | — |
| Types/declarations | Nested type declarations (inner classes/interfaces/enums) are reviewed | ✓ | `review/types.rs` (recursive review of `TypeDecl` inside members) | Enter/Check | Ensures inner types get the same checks as top-level types |
| Types/declarations | Interface cannot be final | ✓ | `verify/mod.rs` | Check | — |
| Modifiers | Class not both abstract and final | ✓ | `verify/class_access_flags.rs`; `review/types.rs` | Check | — |
| Modifiers | Interface must be abstract (except `package-info`) | ✓ | `verify/class_access_flags.rs` | Check | — |
| Modifiers | Interface method illegal flags (private/protected/final) | ✓ | `review/methods.rs` | Check | Default/static bodies allowed |
| Modifiers | Constructor illegal flags (abstract/static/final/synchronized/native) | ✓ | `review/methods.rs` | Check | — |
| Modifiers | Method visibility exclusivity (public/protected/private) | ✓ | `verify/method_access_flags.rs` | Check | — |
| Modifiers | Field visibility exclusivity | ✓ | `verify/fields.rs` | Check | — |
| Modifiers | Field cannot be both final and volatile | ✓ | `review/fields.rs` | Check | — |
| Interface members | Interface fields must be public static final | ✓ | `review/fields.rs` | Check | — |
| Method structure | Non-abstract/native must have Code; abstract/native must not | ✓ | `verify/methods.rs` | Attr | — |
| Method structure | Return opcode matches descriptor | ✓ | `verify/methods.rs` | Flow/Attr | Last-opcode check only |
| Method attributes | Code inner attribute duplication (StackMapTable/LVT/LVTT) | ✓ | `verify/methods.rs` | Attr | — |
| Method attributes | Exceptions: duplicate entries/duplicate attribute blocks | ✓ | `verify/methods.rs`; tests | Attr | — |
| Method attributes | Parameter annotations count matches parameter count | ✓ | `verify/methods.rs`; tests | Attr | — |
| Field structure | ConstantValue only on static; constant kind valid | ✓ | `verify/fields.rs`; tests | Attr | — |
| Names/descriptors | Method/field name and descriptor indices must be Utf8 | ✓ | `verify/methods.rs`; `verify/fields.rs` | Attr | — |
| Control flow / DA | Definite assignment merge for if/else | ✓ | `review/statements.rs`; tests | Flow | Structural subset |
| Control flow / DA | while/for (incl. infinite) and labeled break/continue propagation | ◐ | `review/statements.rs`; tests | Flow | Common patterns; complex nesting approximated |
| Control flow / DA | try/catch/finally DA merge | ◐ | `review/statements.rs`; tests | Flow | OR/AND subset rules |
| Control flow / DA | Unreachable statement after termination | ✓ | `review/statements.rs` | Flow | Basic |
| Control flow / DA | Non-void methods must return | ◐ | `review/statements.rs` | Flow | Basic structural coverage |
| switch | Duplicate case constants; multiple defaults | ✓ | `review/statements.rs`; tests | Flow | int constant folding subset |
| switch | Effect of default on DA | ◐ | `review/statements.rs`; tests | Flow | Subset |
| Overload resolution | Arity match; varargs minimum arity | ✓ | `review/statements.rs` | Resolve | Local class/static imports |
| Overload resolution | Literal-driven applicability (widen/box/unbox) and simple ranking | ◐ | `review/statements.rs` | Resolve/Attr | Primitive + String; boxing/unboxing and simple reference assignability via hierarchy; varargs handled with same conversion model; arity-only fallback in compat mode; simplified cost model |
| Overload resolution | Ambiguity detection and “more specific” tie-break | ◐ | `review/statements.rs`; tests | Resolve | Uses reference assignability for specificity; still a simplified rule set |
| Static access | Static imports: method/field staticity and arity checks | ✓ | `review/statements.rs`, `review/fields.rs`; tests | Resolve/Enter | — |
| Static access | `TypeName.m(...)` / `TypeName.f` must be static | ✓ | `review/statements.rs`, `review/fields.rs` | Check/Resolve | — |
| Generics | Type-argument count in new/cast/instanceof | ✓ | `review/statements.rs`; tests | Attr | Strict match; in compat mode only raw (0 args) usage is tolerated |
| Generics | Upper-bound checks (local index/explicit imports) | ◐ | `review/types.rs`, `review/statements.rs` | Attr | Bounds validated across new/cast/instanceof; wildcard capture subset: `? extends B` treated as `B`, bare `?` as `Object`; wildcards disallowed in constructor type args; full capture/inference out of scope |
| Exceptions | Checked exceptions report (throws/catch coverage) | ◐ | `review/statements.rs`; tests: `review_exceptions_tests.rs`, `review_try_with_resources_tests.rs` | Flow/Attr | Throw typing (new/identifier/call); call-site propagation for local and cross-type methods/ctors (incl. static-imported). Try-with-resources integrates `close()` declared throws from declared resource type; falls back to `Closeable`/`AutoCloseable` contracts. Gaps: precise rethrow/multi-catch rules. |
| Access control | Cross-package/derived visibility and override access | ◐ | `review/types.rs`, `review/methods.rs` | Check/Resolve | Enforced across full super chain and interfaces: no static/instance override/hide; no visibility reduction; interface implementations must be public; final cannot be overridden; interface defaults/abstracts and diamond conflicts handled. Gaps: nuanced protected/package edge cases across packages. |
| Overrides | Return/throws/override-consistency | ◐ | `review/types.rs`, `review/methods.rs` | Check/Resolve/Attr | Enforced across full super chain and interfaces: static/instance rules; final; visibility (incl. package); return covariance (incl. type-parameter super returns); throws narrowing (basic); conflicting interface defaults require class override. Gaps: full throws subtyping matrix and corner cases. |
| Annotations | Retention=Runtime must be in visible set | ✓ | `verify/attributes.rs`; tests | Attr | — |
| Annotations | Type-annotation targets/context legality | ◐ | `verify/attributes.rs` | Attr | Basic validation only |
| Constant pool | Indices and kinds validation | ✓ | `verify/constant_pool.rs` | JVMS | — |
| indy/dynamic | Dynamic/InvokeDynamic require BootstrapMethods | ✓ | `verify/constant_pool.rs` | JVMS | Structural; no actual invokedynamic semantics |
| BootstrapMethods | At most one; indices sanity | ✓ | `verify/constant_pool.rs`; tests | JVMS | — |
| InnerClasses | Index kinds; linkage; name/owner consistency | ✓ | `verify/attributes.rs`; tests | JVMS | — |
| EnclosingMethod | Requires matching InnerClasses entry | ✓ | `verify/attributes.rs`; tests | JVMS | — |
| Signature | Valid class/field/method signature strings | ✓ | `verify/signature.rs`; `verify/attributes.rs` | Attr | Grammar-level validation |
| Modules/sealed/records/nest | Version gates and flag consistency | ✓ | `verify/attributes.rs`; `class_access_flags.rs` | Attr | 9+ features gated off for Java 8 target |
| Method StackMap | pc/locals/stack bounds and basic verification types | ✓ | `verify/methods.rs` L139–L178; tests | JVMS/Flow | Linear/monotonic checks |
| Method names/desc | Utf8 index validity | ✓ | `verify/methods.rs` | Attr | — |
| Version | Reject classfile major > 52 | ✓ | `verify/mod.rs` | Target/Source | Locked to Java 8 |

Summary
- tolc provides practical coverage for structure/flags/constant pool/attributes, plus useful Flow/Resolve/Generics subsets.
- Remaining notable gaps vs javac: precise rethrow/multi-catch rules; nuanced protected/package rules across packages; complete overload ranking and generics inference/capture beyond the implemented subset.


