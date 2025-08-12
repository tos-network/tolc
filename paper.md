## Review/Verify paper

This document outlines of tolc's compilation
--target is Java 8 only (classfile major 52)

- review (AST-level, pre-ClassFile)
- verify (ClassFile-level, pre-emit)

### tolc layers

- PARSE (lexer+parser)
  - tolc: `parser` (existing)

- ENTER (symbols, scopes)
  - tolc-review: build lightweight per-type member tables; detect duplicates and visibility issues early

- ATTR (type attribution/type checking)
  - tolc-review: progressively add essential type-shape checks (assignability, method arity, return presence)

- FLOW (definite assignment, reachability)
  - tolc-review (later): minimal must-return checks for non-void methods; staged expansion to DA/DR

- JVM (lowering to classfile, conformance)
  - tolc-verify: structural and attribute-level checks before writing bytes

---

## Current status

- review (AST)
  - Package/imports: empty or duplicate checks
  - Types: duplicate type names within compilation unit
  - Per-kind stubs split into dedicated files (`class.rs`, `interface.rs`, `annotation.rs`, `enums.rs`)
  - Class: abstract ∧ final is rejected
  - Interface: final is rejected
  - Nested types: inner classes/interfaces/enums are recursively reviewed so they receive the same checks as top-level types
  - Access control and overrides:
    - Enforce no visibility reduction; interface implementations must be public
    - Forbid overriding final methods; prevent static/instance override/hide mismatches
    - Return covariance for references; if super return is a type parameter, accept any reference return (javac-like)
    - Throws clause narrowing enforced (basic subset)
  - Method resolution:
    - Arity and varargs minimum checks; primitive/String-based applicability with widening cost; ambiguity reporting
    - Cross-type calls/constructors resolved via classpath index (when enabled)
  - Generics:
    - Enforce type-argument count for new/cast/instanceof; minimal upper-bound checks when bounds are present
  - Checked exceptions (basic):
    - Classify throw sites and propagate checked exceptions across local and cross-type calls; supports static-imported members
    - Common RuntimeException subclasses treated as unchecked (fallback); prefer hierarchy via classpath index when available
  - Control flow:
    - Must-return: treats throw as terminal; refined if/switch/try/finally coverage
    - DA/DR seed: use-before-init for locals; branch and loop heuristics; final parameters/locals single-assignment

  - verify (ClassFile)
  - Constant pool: index/type checks; basic BootstrapMethods indices; no Module/Nest/Record/PermittedSubclasses in Java 8
  - Class access flags: interface/annotation rules; abstract/final combos per spec
  - this_class/super_class: refer to Class entries; super=0 only when not interface
  - Interfaces vector: each entry refers to Class
  - Fields: name/descriptor to Utf8; interface field flags; ConstantValue validation
  - Methods: access flags; name/descriptor to Utf8; Code/Exceptions/Signature; last return opcode matches descriptor

Compatibility and classpath
- `TOLC_CLASSPATH`: enables a classpath-wide `GlobalMemberIndex` from `.java` files for inheritance/interfaces/overloads/throws/fields; used by review checks to align with javac
- `TOLC_JAVAC_COMPAT`: relaxes certain checks when information is incomplete (e.g., tolerate raw types with 0 generic args; defer strict arity/type errors when unresolved)
- Java suite harness auto-sets the above and initializes logging; supports `JAVA_SUITE_FILTER` and `JAVA_SUITE_FIRST_ONLY`
- Classpath index global cache: built once per process and reused to speed up the suite

---

## Next milestones (short-term)

1) review (ENTER-lite)
   - Per-class member uniqueness
     - Fields: unique by name
     - Methods: unique by erased signature (phase 1: name+arity; phase 2: simple erasure)
   - Visibility/modifier constraints
     - Interface members default to public; reject illegal combos

2) review (ATTR-lite)
   - Broaden applicability beyond primitives/String (boxing/unboxing for references)
   - Non-void methods: continue increasing must-return precision in complex flows

3) verify (JVM conformance)
   - Maintain Java 8 attribute set coverage; incremental tightening as needed

---

## Detailed implementation plan (actionable)

### Sprint 1: Member tables + duplicate detection (review/ENTER-lite)

- Data structures
  - `review/types.rs` builds a per-type `MemberTable`:
    - `fields_by_name: HashMap<String, Span>`
    - `methods_by_name: HashMap<String, Vec<(MethodSig, Span)>>`
  - `MethodSig` (phase 1): `{ name: String, arity: usize }`

- Checks
  - Field duplicates: error on same name in single type
    - Maps to `Check.checkUnique` behavior
  - Method duplicates: error when name+arity matches existing (ignore return type for now)
    - Next phase: simple erasure (treat all reference params as `Object`, primitives as-is)

- API sketch
  - `review::types::review_types` constructs tables and emits diagnostics via `ReviewError::DuplicateType/...`

- Tests
  - Add unit tests under `tests/parser_tests.rs` and new `tests/review_member_tests.rs` covering duplicates

### Sprint 2: Modifiers and visibility rules (review/ENTER-lite)

- Interface members
  - Methods default to `public abstract`; fields to `public static final`
  - Reject illegal flags on interface members (map from `Check.java`)

- Class-level rules
  - Constructor rules: at most one varargs constructor; visibility consistency (basic subset)
  - Enum-specific: constructors cannot be `public` (subset)

- API changes
  - Extend `review/class.rs`, `review/interface.rs` with per-member iteration (requires walking AST bodies)

- Tests
  - Add `tests/review_modifier_tests.rs` with positive/negative cases

### Sprint 3: Arity and assignability (review/ATTR-lite)

- Method invocation arity
  - Validate `arguments.len() == parameters.len()` for simple calls
  - Support varargs: `>= params.len() - 1`

- Literal assignability
  - Allow int literals into byte/short/char when in range (subset of `Attr.checkType`)
  - Booleans only to boolean

- Must-return structural check
  - For non-void methods: verify there is at least one `return` on all terminal paths in a simple CFG
  - Start with linear blocks and simple `if/else`

  - Tests
  - `tests/review_arity_and_return_tests.rs`

### Sprint 4: Verify enhancements (JVM conformance)

- Parameter annotations count
  - In `verify/methods.rs`, ensure RVPA/RIPA param arrays sizes match descriptor parameter count

- Exceptions attribute content
  - Already validates indices; extend with duplicate exception removal warnings (optional)

- Code attribute sanity (optional)
  - If StackMapTable present, count frames <= u16; reserve further checks for debug mode

---

### Sprint A: Symbol table + overload selection (review)

- Build minimal per-class symbol table (fields/methods) with quick lookup by name
- Overload selection (phase 1): choose by arity and primitive widening (int→double, char→int)
- Enforce access modifier consistency for class members (subset of Check.java)

### Sprint B: Verify refinements (JVM)

- Enforce parameter annotations count equals descriptor parameter count
- Single Signature/Exceptions per method; validate content and dedupe
- Attribute checks limited to Java 8 attribute set

### Sprint C: Flow and numeric promotions (review)

- Numeric promotions for arithmetic/comparison (int/long/float/double ladder)
- Varargs materialization model for call sites (extra args form the array tail)
  - Begin DA/DR for simple patterns (initialized before use; finals assigned once)

### [~] Sprint 3 (in progress): Review-Core deepening and Verify refinements

- Overload resolution (more specific)
  - Primitive-widening cost model in place; ambiguity reported via `AmbiguousMethod` (done)
  - Next: stricter tie-break (prefer exact over widened; prefer fewer/cheaper conversions)
- Qualified/static context checks
  - Reject `TypeName.m(...)` when method not static (approx via member table) (done)
  - Reject `ClassName.m(...)` self-qualified non-static (done)
- Simple name resolution skeleton
  - Build per-compilation-unit GlobalMemberIndex (done) and use for cross-type calls/ctors (done)
  - Next: fold imports/package into type matching; basic aliasing for simple cases
- Must-return coverage
  - Extended to `switch` (done), `while(true)` (done), `for(;;)` heuristic (done), try-finally (done)
- DA/DR (definite assignment) seed
  - Use-before-init for locals in single-block heuristic (done)
  - Next: assign-to-final (parameters/locals) and branch merging
- Verify integration
  - Pre-emit verify integrated (done); optional StackMapTable sanity later (todo)

## Diagnostics & conventions

- Error types
  - review: `ReviewError::{DuplicateImport, DuplicateType, EmptyClassName, ClassAbstractAndFinal, InterfaceFinal, ...}`
  - verify: `VerifyError::Internal(String)` wraps low-level verifier messages; consider typed errors later

- Messages
  - Prefer concise, e.g., "duplicate method '{name}({arity})'", "incompatible literal: expected int, found long"
  - Use `TOLC_DEBUG=1` to enable targeted review debug logs; general logs via `RUST_LOG`

---

## API integration

- AST
  - `parser::parse_and_verify` already calls `review::review(&Ast)`
- ClassFile
  - Ensure `verify::verify(&ClassFile)` is invoked right before `writer` emits bytes (wire in call site)

---

## Acceptance criteria per sprint

- Sprint 1
  - Duplicate fields/methods detected with clear spans; tests pass
- Sprint 2
  - Illegal interface member flags rejected; tests pass
- Sprint 3
  - Arity and simple assignability enforced; non-void must-return basic checks green
- Sprint 4
  - Parameter annotation counts verified; verify suite remains green

## Mid-term

- review
  - Completed: Name resolution scaffolding (qualified types), static imports (explicit and wildcard) for unqualified calls with shadowing precedence
  - Completed: Overload most-specific tie-break refinements (prefer exact > fewer conversions > lower widening cost)
  - In progress: DA/DR prototype expansion (final locals, loop merges, break/continue reachability); static field imports (validation wired)
  - Completed: Simple generic arity checks on type usage
    - new Type<...>(...): enforce argument count vs declared type parameters
    - Casts and instanceof: enforce generic arity on qualified target types
    - Imported/extern types: validate arity for explicitly imported simple names and fully-qualified uses; reject zero-arity classes used with type args and vice versa

- verify
  - Completed: LocalVariableTable/LocalVariableTypeTable basic content checks (ranges and Utf8 indices); debug-only StackMapTable PC progression sanity
  - Skipped: Signature grammar full validation; class/field-level Signature deep rules (beyond Java 8 minimum)
  - Completed: Class/Field/Method Signature presence and basic content validation (Java 8 minimal)

---

## Long-term

- review
  - Definite assignment/definite unassignment (FLOW) – remaining scope
    - Precise merges across try/catch/finally and labeled break/continue
    - switch fall-through and coverage analysis
    - Field/instance finals (single-assignment), compound assignments, and DA/DU on exceptional paths
  - Generics – remaining scope
    - Multiple/interface upper bounds and arrays/interfaces assignability
    - Type-variable-to-type-variable constraints and capture conversion basics

- verify
  - JVMS attribute matrix validation – status and next
    - Completed: EnclosingMethod requires InnerClasses; BootstrapMethods CP index sanity; StackMapTable strict pc coverage vs code length
    - Completed: StackMapTable frame decoding and state validation (pc/locals/stack bounds)
    - Completed: Annotation Retention=Runtime must be in visible set (Java 8 scope)
    - Completed: InnerClasses entry indices validation and linkage to EnclosingMethod
    - Completed: Signature content validation strengthened (class/field/method minimal grammar + lexical)
    - Next: InnerClasses broader name/owner consistency across non-this entries (optional)

---

## Integration points

- Pre-ClassFile: `review::review(&Ast)` (called by `parser::parse_and_verify`)
- Pre-emit: `verify::verify(&ClassFile)` (call right before writing bytes)


---

## Changelog

- Added `coverage.md` documenting a fine-grained coverage comparison against javac (Java 8 semantics). This serves as a living checklist to guide review/verify parity work.
- review: Added a basic checked-exception analysis (throws/catch coverage) and tests; updated coverage table accordingly.

- review (recent):
  - Recursive nested-type review (inner classes/interfaces/enums)
  - Overrides/visibility: full-chain/interface checks, final/static rules, return covariance (incl. type-parameter super returns), throws narrowing
  - Method resolution: cross-type arity/signature lookup via classpath index; improved applicability and ambiguity detection; compat fallbacks
  - Generics: strict arity checks on new/cast/instanceof; minimal bounds enforcement; raw (0 args) tolerance only in compat mode
  - Must-return refinements and DA/DR seeds (throw terminal; try/finally; loop/branch heuristics; final locals/params)
  - Illegal static call checks for fields/methods with static imports awareness

- infrastructure:
  - Classpath-wide index (`TOLC_CLASSPATH`) with a global cache for performance
  - Compatibility switch (`TOLC_JAVAC_COMPAT`) to align behavior with javac on large suites
  - Java suite harness updated to auto-set env vars and provide focused filters/logging

- Sprint 1 (ENTER-lite) – initial landing
  - review (per AST, pre-ClassFile):
    - Per-class member uniqueness
      - Fields: unique by name
      - Methods: unique by (name, arity)
    - Interface field modifiers enforced as public static final
    - Fields cannot have type `void`
    - Method parameters must be unique by name (per method)
    - Basic must-return check for non-void methods (structural heuristic)
  - Integration:
    - `review::review(&Ast)` dispatches `package`, `imports`, `types`
    - `types` delegates to `class` / `interface` / `fields` / `methods` / `statements`
  - Structure:
    - New files: `review/{class,interface,fields,methods,statements}.rs`

- Sprint 2 (ATTR-lite) – begin arity checks
  - review:
    - Added unqualified method call arity validation using per-class arity table
    - Varargs constructor multiplicity check (at most one varargs constructor per class)
    - Visibility exclusivity for methods (public/protected/private)
    - Locals: duplicate per block; literal initializer compatibility (int/double/boolean/char/String/null)
    - Field modifiers: visibility exclusivity; reject final+volatile
    - Abstract method illegal combos: abstract with private/static/final/native/synchronized rejected
  - Structure:
    - `statements.rs`: `review_body_call_arity` walker; `walk_stmt`/`walk_expr` visitors; locals/inits checker
    - `methods.rs`: builds arity/varargs/signature tables and invokes checkers per method body
  - Enhancements:
     - Must-return check now requires both branches of if/else to return (basic structured guarantee)
  - New checks:
     - Duplicate local variables per block scope
     - Literal initializer compatibility for int/double/boolean/char/String/null
     - Basic expression type inference (literals, paren, casts, unary, binary, conditional, new String)
     - Method overload applicability on literals and simple expressions (primitive widening + String)

- Sprint 2.1 – constructors and conditional merge
  - review:
    - Constructor overload validation for `new` of current class: arity + varargs + applicability
    - Conditional expression type merge: numeric promotion, String|null, boolean
  - Structure:
    - `methods.rs`: constructor signature tables (`ctor_arities`, `ctor_varargs_min`, `ctor_signatures`)
    - `statements.rs`: `review_body_call_arity` Interface extension supports constructor verification
  - Overload resolution:
    - Added primitive-widening cost model; report `AmbiguousMethod` when multiple candidates tie on minimal cost

- Sprint 2.2 – cross-type calls (static) minimal support
  - review:
    - Qualified calls `TypeName.m(...)` checked via GlobalMemberIndex tables
    - Cross-type constructor `new TypeName(...)` validated with ctor tables
  - Structure:
    - `types.rs`: GlobalMemberIndex with per-type method/ctor arities, varargs mins, signatures
    - `statements.rs`: use global index in `walk_expr` for cross-type calls/ctors

- Sprint 3 – initial drop
  - review:
    - Illegal static call diagnostics (`IllegalStaticCall`)
    - Must-return: add `for(;;)` heuristic and try-finally propagation
    - DA/DR seed: `UseBeforeInit` for locals within basic scopes; assignment marks variable as definitely assigned
  - structure:
    - `types.rs`: `methods_static` recorded per method name
    - `statements.rs`: consumes local static flags and global index for checks
  - Completed (Review-Core deepening):
    - Overload resolution tie-break refined: prefer fewer conversions, then lower widening cost
    - Static context checks tightened: reject qualified non-static calls, reject self-qualified non-static
    - Name resolution skeleton: package/imports (including wildcard) integrated into lookup for cross-type calls
    - Name resolution enhancements: static imports (explicit and wildcard) for unqualified calls, with shadowing precedence (local > explicit import > wildcard)
    - Must-return coverage expanded: switch/while(true)/for(;;)/try-finally
    - DA/DR prototype: use-before-init, assign-to-final (including parameters and local variables), if-branch merging, loop-internal merges
    - DA/DR refinement: precise termination on break/continue within loops and return; improved assignment merge in non-constant loop conditions
    - Overload duplicates: switched duplicate detection to full parameter type signature (enables overloading with same arity)
    - Tests: import/wildcard cross-type calls, static context, DA/DR, must-return
    - Generic arity checks: new/cast/instanceof, imported types, and zero-arity misuse
    - Generic bounds checks (minimal): enforce upper bounds with simple-name equality or Object, and reference-type assignability via superclass chains, in new/cast/instanceof
    - DA/DU refinements: loop merges and break/continue reachability; local variable modifiers supported (final) with single-assignment enforcement (replaces `_final` convention)
  - verify:
    - Methods: Exceptions duplicate entries detection (DuplicateExceptions)
    - Methods: Parameter annotations count equals descriptor param count (RVPA/RIPA)
    - Methods: Single Signature/Exceptions per method (DuplicateMethodAttribute)
    - Methods: Code inner attributes: single StackMapTable per Code

### [x] Sprint 4 – Verify enhancements
  - verify:
    - Added tests for parameter annotation count mismatch (visible/invisible)
    - Added tests for duplicate Signature attribute
    - Added tests for Exceptions duplication (attribute duplication and entry duplication)
    - Enforced single StackMapTable inside Code attribute
    - EnclosingMethod/InnerClasses consistency check (EnclosingMethod requires InnerClasses)
    - BootstrapMethods stricter content checks (indices sanity)
    - StackMapTable strict pc coverage sanity vs code length (non-debug builds as well)

