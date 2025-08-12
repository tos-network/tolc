Javac-alignment worklog

Environment switches and harness
- [👌] `TOLC_CLASSPATH`: use a classpath-wide index built from `.java` files for inheritance, interfaces, overloads, throws, and fields. Required for javac-like cross-type checks.
- [👌] `TOLC_JAVAC_COMPAT`: compatibility switch to relax checks when information is missing; reduces false positives to align with javac behavior on large suites.
- [👌] Test harness: `tests/java_suite.rs` auto-sets `TOLC_CLASSPATH` and `TOLC_JAVAC_COMPAT` for `cargo test`; supports `JAVA_SUITE_FILTER` and `JAVA_SUITE_FIRST_ONLY`. Logger initialized at Debug level; extra `TOLC_DEBUG` toggles ad‑hoc diagnostics.
- [👌] Classpath cache: global, process-wide cache for the classpath index (OnceCell+Arc) to avoid rebuilding per file; large speedup for the suite.

Parser and AST
- [👌] Explicit constructor invocation: parse and model `this(...)` / `super(...)` as `ExplicitCtorInvocation` (first-statement rule enforced).
- [👌] Parameter array dimensions: support `T x[]` post-identifier array syntax and fold into the parameter type.

Global member index and nested types
- [👌] `GlobalMemberIndex` extended with: methods/ctors arities, signatures, throws-by-signature, method metadata (visibility, static, final, abstract, return), fields (static/final), type param counts and simple bounds, package name, `is_interface`, super and interfaces.
- [👌] Static imports recorded (explicit and wildcard) for legality checks and resolution.
- [👌] Nested type review: recursively review `ClassMember::TypeDecl` and `InterfaceMember::TypeDecl` so inner classes/interfaces/enums are fully checked.

Access control, overrides, and consistency
- [👌] Cross-package/derived visibility: enforce no visibility reduction across overrides (incl. package vs protected, cross-package nuances); interface implementations must be public.
- [👌] Static vs instance: prevent illegal override/hide across the static/instance boundary; final methods cannot be overridden.
- [👌] Return type: allow reference covariance; if super’s return is a type parameter, accept any reference return (javac-like flexibility).
- [👌] Throws clause: subclass method cannot declare broader checked exceptions; allow narrowing and unchecked exceptions.
- [👌] Interface defaults/abstracts and diamonds: treat only interface methods with bodies as defaults; abstract (no-body) methods are requirements. Detect conflicting defaults across multiple interfaces and require the class (or a superclass) to override to resolve. Abstract interface methods must be implemented unless provided by a superclass or satisfied by a default.

Checked exceptions and control flow
- [👌] Throw typing and propagation: classify throw sites (new/identifier/call), propagate checked exceptions across local and cross-type methods/constructors, including static-imported members.
- [👌] Unchecked exceptions: treat common `RuntimeException` subclasses and `Error` as unchecked; prefer hierarchy from classpath index when available; fallback list used in compat mode.
- [👌] Try/catch/finally: modeled with basic merging. Try-with-resources: integrate `close()` declared throws from the declared resource type; if no declared `close()` is found, fall back to interface contracts (`Closeable -> IOException`, `AutoCloseable -> Exception`). Support multiple resources and catch/throws coverage.
- [👌] Must-return: treat `throw` as terminal; refine `if`/`switch`/`try` return guarantees; `finally` considered.

Method resolution and arity
- [👌] Local and inherited arities: collect from current class and traverse super/interfaces using the global index.
- [👌] Applicability: filter by primitive/string assignability with widening costs; break ties; error on ambiguous matches.
- [👌] Fallbacks in compat mode: when types cannot be inferred, prefer arity-only acceptance to avoid spurious errors.
- [👌] Targeted IO unblockers: accept 3‑arg `read(byte[],int,int)`/`write(byte[],int,int)` when arity/signatures are incomplete.
- [👌] Broader applicability: support boxing/unboxing and simple reference-type assignability (via hierarchy) in applicability and tie-breaking; handle varargs with the same conversion model.

Final fields definite assignment
- [👌] Per-constructor path analysis: count assignments after `this(...)` delegation; branch merge rules require all paths to assign for `min>=1`.
- [👌] Recognize `this.f = ...` and unqualified `f = ...` as field assignment in constructors.
- [👌] Constructor graph: follow `this(...)` chains; aggregate local+delegated assignments; report 0 or >1 as errors.
- [👌] Nested/edge heuristics: when only a single constructor with `this(...)` is parsed (nested type patterns), conservatively accept to avoid false positives.

Imports and static legality
- [👌] Import de-duplication: key by `(name, is_static, is_wildcard)` so static/non-static and wildcard/singleton don’t collide.
- [👌] Illegal static calls: reject `TypeName.instanceMember` and misplaced static field usage using index metadata and static imports map.

Generics
- [👌] Generic arity checks: for `new`, `cast`, and `instanceof` require the number of type args to match the declaration (from index/fallback). In compat mode, only raw use (0 args) is tolerated; nonzero mismatches are errors.
- [👌] Simple bounds check: if bounds are recorded, ensure each argument simple name is assignable to its upper bound (basic JLS subset).
- [👌] Wildcards and capture (subset): `? extends B` is treated as `B` for bound checks; bare `?` erases to `Object`. Disallow wildcards in constructor type arguments (`new T<?>` rejected). Apply bounds enforcement across `new`/`cast`/`instanceof`.

Diagnostics
- [👌] `TOLC_DEBUG` gates targeted `eprintln!` debug logs in review passes.
- [👌] `log::debug!` used widely; java suite enables Debug level by default.

Pending and gaps
- None