## Fine-grained comparison: tolc vs javac (Java 8 semantics)

Legend: ✓ Covered, ◐ Partially covered, ✗ Missing

Notes
- Scope is Java 8. Newer features (Java 9+) are version-gated or rejected in tolc.
- tolc splits checks between AST-level review (pre-ClassFile) and ClassFile-level verify.

| Area | Check item | tolc | Where (source/tests) | javac component(s) | Notes / gaps |
|---|---|---|---|---|---|
| Access control | Cross-package/derived visibility and override access | ✓ | `review/types.rs`, `review/methods.rs`, `review/statements.rs`; tests: `accessibility_cross_package_tests.rs` | Check/Resolve | Fully aligned (methods and instance fields). Same-package: protected/package-private allowed; private rejected. Cross-package protected: allowed only in subclass code and only via `this`/subclass qualifier; unrelated qualifier rejected. Also enforces: no static/instance override/hide; no visibility reduction; interface implementations must be public; final non-overridable; return covariance; throws narrowing; interface defaults/abstracts and diamond conflicts handled. |
| Annotations | Retention=Runtime must be in visible set | ✓ | `verify/attributes.rs`; tests | Attr | — |
| Annotations | Type-annotation targets/context legality | ✓ | `verify/attributes.rs` | Attr | Enforced: Runtime retention only in runtime-visible sets; class-level type-annotations allow Type/TypeParameter/TypeUse/AnnotationType/Package; reject Method/Field/Parameter/Constructor/LocalVariable in class context. |
| Annotations | Type-use annotations emission (visible/invisible) with retention inference | ✓ | Parser: `parser/parser.rs` (captures on `TypeRef`, arrays dims, generic args); Codegen: `codegen/class_writer.rs` (return/params/throws/field/extends/implements/type-bounds) with CU-level `@Retention` inference in `codegen/mod.rs`; tests: `type_use_annotation_parser_tests.rs`, `verify_type_annotations_tests.rs` | Attr | Emits `RuntimeVisibleTypeAnnotations`/`RuntimeInvisibleTypeAnnotations` based on parsed `@Retention` on annotation declarations; recursively visits generic args and wildcard bounds; parses array-dimension annotations. |
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
| indy/dynamic | Dynamic/InvokeDynamic require BootstrapMethods | ✗ | — | JVMS | Not supported: no `invokedynamic` emission or acceptance. Classfiles using `invokedynamic` are rejected. Lambdas are not lowered via `LambdaMetafactory`. |
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
| Overload resolution | Literal-driven applicability (widen/box/unbox) and simple ranking | ✓ | review/statements.rs<br/>tests: review_overload_resolution_tests.rs | Resolve/Attr | Literal inference for primitives/String/null; applicability via widening/boxing/unboxing with cumulative cost; ranking by exact → minimal conversions → minimal widening cost → fixed-arity over varargs; compat-mode arity fallback retained. |
| Packages/imports | Duplicate imports | ✓ | `review/imports.rs`; tests: `review_import_tests.rs` | Enter/Check | — |
| Packages/imports | Non-empty package name when present | ✓ | `review/package.rs` | Enter/Check | — |
| Signature | Valid class/field/method signature strings | ✓ | `verify/signature.rs`; `verify/attributes.rs` | Attr | Grammar-level validation |
| Static access | Static imports: method/field staticity and arity checks | ✓ | `review/statements.rs`, `review/fields.rs`; tests | Resolve/Enter | — |
| Static access | `TypeName.m(...)` / `TypeName.f` must be static | ✓ | `review/statements.rs`, `review/fields.rs` | Check/Resolve | — |
| Name resolution | Simple-name/type resolution precedence | ✓ | `review/types.rs` | Resolve | Precedence order: local types in CU → explicit single-type imports → same-package types → on-demand (wildcard) imports → `java.lang.*`. Fully qualified names resolved directly. |
| switch | Duplicate case constants; multiple defaults | ✓ | `review/statements.rs`; tests: `switch_tests.rs`, `switch_da_default_tests.rs`, `switch_string_enum_tests.rs` | Flow | Covered for int/String/enum labels, with constant folding for int/String and enum-ordinal mapping; multiple defaults rejected. |
| switch | Effect of default on DA | ✓ | `review/statements.rs` (walk_stmt_locals: Switch), tests: `switch_tests.rs`, `switch_da_default_tests.rs`, `switch_string_enum_tests.rs` | Flow | Default considered in DA merge: constant selector evaluates a single fallthrough path (int/String/enum); non-constant enumerates all case-entry fallthrough paths. If there is no default, include the no-match path, except for enum switches that exhaustively list all constants (treated as covered). DA after switch is the intersection across all normal exits. Extended string constant folding: constant concatenation of literal strings/chars/integers. |
| Types/declarations | Duplicate type names in a CU | ✓ | `review/types.rs` | Enter/Check | — |
| Types/declarations | Empty type names rejected | ✓ | `review/types.rs` | Check | — |
| Types/declarations | Interface cannot be final | ✓ | `verify/mod.rs` | Check | — |
| Types/declarations | Nested type declarations (inner classes/interfaces/enums) are reviewed | ✓ | `review/types.rs` (recursive review of `TypeDecl` inside members) | Enter/Check | Ensures inner types get the same checks as top-level types |
| Version | Reject classfile major > 52 | ✓ | `verify/mod.rs` | Target/Source | Locked to Java 8 |

### Gaps to 100% javac alignment (Java 8) and roadmap

Legend: ✓ Covered, ◐ Partially covered, ✗ Missing

| Area | Gap | Status | Notes / Target spec (javac/JLS) |
|---|---|---|---|
| Generics | Full capture conversion (wildcards), inference across expressions and calls | ✗ | JLS 4.10, 15.12.2.7; affects overload applicability and cast/instanceof/new consistency |
| Generics | Generic method type inference (poly expressions) | ✗ | JLS 15.12.2; most-specific selection with inferred type args |
| Generics | Wildcards in constructor type args and nested use sites | ◐ | Basic checks done; full legality and capture missing |
| Overload resolution | Complete JLS ordering (boxing/unboxing, varargs vs fixed, primitive promotions, most-specific by subtyping) | ◐ | Implemented simplified ranking; needs full tie-breaks (JLS 15.12) and applicability-by-subtyping pass |
| Lambdas/method refs | Parsing, target typing, inference, exception typing | ✗ | Unsupported: lambda `->` and method reference `::` syntax cause parser errors by design; no `invokedynamic`/`LambdaMetafactory` support. |
| Flow/DA | Blank final fields: definite assignment across constructors/this()/super() and DA/DU rules | ◐ | Implemented path-sensitive checks across constructors with this(...) delegation and instance initializer blocks in `review/methods.rs`: each constructor path must assign each instance final exactly once (or have an initializer), and multiple assignments are rejected; assignments after this(...) caller only. Heuristics cover unresolved delegate targets. Remaining: full JLS edge-cases and super() ordering nuances. |
| Flow/DA | Full reachability (DR) and DA for complex labeled break/continue with try/finally nesting | ◐ | Core cases handled; needs exhaustive label/try/finally interactions |
| Flow/DA | Switch analysis parity (strings/enums constant folding, exhaustive per-entry fallthrough paths) | ◐ | Added String/enum constant folding, duplicate-label checks, and enum-exhaustive coverage (no empty path when all constants are listed). Remaining: broader String constant detection beyond literals, and deeper per-label reachability nuances. |
| Exceptions | Precise throws for generic methods, multi-catch disjointness checks, lambda-related throws | ◐ | Typical throws coverage is present; generic/lambda precision pending |
| Attr/Annotations | Complete type-annotation coverage (arrays, nested, use-site targets), repeated annotations | ◐ | Parsing and emission wired for many TYPE_USE sites (return/params/throws/field/extends/implements/type bounds; array-dimension and generic-arg annotations), with retention-based visible/invisible split. Remaining: full JVMS target-path encoding breadth, additional TYPE_USE contexts, and repeated-annotation semantics. |
| Name resolution | Ambiguous/duplicate imports precedence, inner-class vs top-level shadowing, star-import specificity | ✓ | `review/types.rs`; tests: `review_import_precedence_tests.rs` | Resolution precedence aligned with javac; inner-class vs top-level shadowing and star-import specificity match common scenarios. |
| Overrides | Generic override checks with erasure/bridges, ACC_SYNTHETIC bridge expectations | ✓ | Visibility/covariant returns/throws narrowing enforced. Bridge methods synthesized at codegen: `Comparable<T>#compareTo(T)` → `compareTo(Object)`, `Comparator<T>#compare(T,T)` → `compare(Object,Object)`, `List<E>#get(I)` → `get(I)Ljava/lang/Object;`. Verification enforces `ACC_BRIDGE|ACC_SYNTHETIC` on bridges and checks erased-target descriptor exists in the declaring class. |
| Parser | Lambda/method ref grammar, full annotation positions, multi-resource TWR details | ◐/✗ | Parser covers most statements/expressions; lambda/mref not yet |
| Verify/StackMap | Full verification types and StackMap frame merging | ◐ | Minimal verification-type lattice and CFG-based StackMap computation implemented in `codegen/frame.rs`. Precise typing for `getfield`/`getstatic`/`invoke*`, `new`/`<init>` uninitialized-to-initialized transitions (converting matching locals/stack), and `aload` preserving locals' exact types. Category-2 handling (`long`/`double`), `WIDE` forms, and handler catch-type typing supported. Frame compression (Same/SameExt/SameLocals1*/Append/Chop/Full) emitted. Tests: `tests/stackmap_merge_tests.rs`, `tests/stackmap_compute_tests.rs`, `tests/stackmap_invoke_tests.rs`, `tests/stackmap_precise_types_tests.rs`. Remaining: broaden `dup*_x*` around object construction, richer array/object stores edge-cases, and integration into emitted `StackMapTable`. |
| Const exprs | Compile-time constant evaluation (folding), final fields inlining parity | ◐ | Advanced: long/double arithmetic; char→int promotions; mixed-type casts/widening; boolean `&`/`|`/`^`; relational comparisons; broadened String concatenation (either side String); shift masking parity (int 5-bit, long 6-bit when lhs effectively long). Guarded div/mod by zero (no fold) with review-time diagnostic. Where: `review/statements.rs` (switch folding + diagnostic), `codegen/class_writer.rs` (`eval_compile_time_constant`). Tests: `tests/const_folding_tests.rs`, `tests/review_flow_tests.rs`. Remaining: corner-case constant-expression shapes and documentation polish. |

Planned sequencing to reach 100%
- Short-term: switch DA parity (enum/String) ✓, name-resolution precedence ✓, override-bridge checks ✓, annotation placement matrix (declaration-site duplicates ✓; type-use matrix ✓ with retention-based visible/invisible emission), generics upper-bound corner cases ✓, full overload tie-breaks ✓.
- Mid-term: blank-final DA/DU ◐, generic method inference and capture conversion ✗, StackMap merge improvements ◐, constant expression evaluation ◐ (long/double/char folding; mixed-type casts/widening; boolean/shift folding; relational comparisons; String concat broadened; div/mod-by-zero guard + diagnostic; NaN/Infinity semantics done; long parity without explicit casts done; constant narrowing conversions done). 

#### Mid-term progress updates (Aug 2025)
- Constant expressions
  - Extended compile-time folding: long/double arithmetic; char→int promotions; mixed-type casts/widening; boolean `&`/`|`/`^`; relational comparisons; int/long shift masking parity; broadened String concatenation (including nested forms); long bitwise parity across `&`/`|`/`^` when lhs effectively long
  - Guarded div/mod by zero during folding (no fold) and review-time diagnostic for constant div/mod by zero
  - Implemented in `review/statements.rs` (switch folding + diagnostic) and `codegen/class_writer.rs` (field `ConstantValue` folding)
  - Tests added in `tests/const_folding_tests.rs` and diagnostic checks in `tests/review_flow_tests.rs`
  - Completed: JLS-consistent NaN/Infinity comparison semantics; long-shift parity without explicit casts (6-bit mask when lhs effectively long); long bitwise parity across `&`/`|`/`^`; constant narrowing conversions for byte/short/char with range checks in review; more String concat shapes (nested forms)
- Next: blank-final DA/DU constructor-path edge cases (this()/super() ordering nuances); integrate StackMap frames into emission; expand simulation for `dup*_x*` and arrays; generics capture conversion and poly-expression inference scaffolding

- Long-term:
  - Lambdas/method refs: explicitly unsupported by design; parser reports errors for `->` and `::`; no `invokedynamic`/`LambdaMetafactory` emission or acceptance. Revisit only if policy changes.
  - Full verification type lattice and StackMap frame merging per JVMS (beyond linear bounds checks).
  - Repeated annotations semantics across declaration and TYPE_USE positions.
  - Generics capture conversion and full generic method type inference (poly expressions) if scope expands.
  
### Next to do
- StackMap
  - Integrate `FrameBuilder` frames into bytecode emission for methods (`StackMapTable` generation)
  - Expand simulation coverage: `dup*_x*` object-construction patterns; array/object loads/stores edge-cases; additional category-2 stack ops
  - Add tests covering method returns of primitives/long/double/arrays; verify locals update post-`<init>` across multiple constructor paths
- Constant expressions
  - Audit parity for long-specific bitwise/shift without explicit casts; add missing corner cases
- Blank-final DA/DU
  - Expand constructor path analysis for strict `this()`/`super()` ordering and instance initializer nuances
- Generics
  - Add scaffolding for capture conversion and poly-expression inference (affects overload applicability)
