# Codeing Guidelines
_For use with Cursor, ChatGPT, Claude, etc._

## 1) Purpose
Make this repository easy for AI tools to **index, retrieve, and reason about**. These rules align code/doc structure with how LLMs chunk and search content, so suggestions are more accurate and safer to apply.

---

## 2) Repository Map (CODEMAP)
High-level orientation based on your tree (Aug 2025). Keep this section updated when modules move/rename.

```
src/
  lib.rs                // crate root & public facade (+ prelude)
  bin/main.rs           // thin CLI entrypoint

  ast/                  // AST domain model (pure; no I/O)
    mod.rs              // facade: exports nodes/visitor/printer
    nodes.rs            // node types
    visitor.rs          // Visitor & traversal
    printer.rs          // pretty-printer

  parser/               // lexing & parsing to AST
    mod.rs              // facade: exports parse(), errors, span
    lexer.rs
    parser.rs
    error.rs
    span.rs

  review/               // post-parse checks/normalizations
    mod.rs              // facade: run_all(ast)
    ... (feature passes)

  verify/               // JVM classfile structural verification
    mod.rs              // facade: verify_class(...)
    verifier.rs
    ... (flags, signatures, pools, etc.)

  codegen/              // JVM classfile generation
    mod.rs              // facade: ClassWriter, MethodWriter, ConstPool, Opcode
    class_writer.rs
    method_writer.rs
    class.rs, method.rs, field.rs, attribute.rs
    constpool.rs, descriptor.rs, typed_index.rs
    opcodes.rs, frame.rs, flag.rs, defs.rs
    writer.rs
    error.rs
    (helpers: bytecode.rs, opcodor.rs, vec.rs -> keep pub(crate))

  config.rs, consts.rs  // top-level config/constants (if public, document why)
  error.rs              // crate-level Error mapping (Parse/Verify/Codegen -> Error)

tests/                  // integration tests by feature (teaching tests preferred)
  ... lots of focused suites
```
**Pipeline (conceptual):**
`parser::parse(src)` -> `ast::*` -> `review::run_all(ast)` -> `verify::verify_class(ast|class)` -> `codegen::ClassWriter::to_bytes()`

---

## 3) Public Surface & Facades
Expose a **minimal, stable** public API through `lib.rs` and each module's `mod.rs`. Hide internal details with `pub(crate)`.

### `src/lib.rs` (crate root & facade)
Provide a 20–60 line crate overview using `//!` and re-export stable items. Example scaffold:

```rust
//! # Terminos Language Compiler (`tolc`)
//! Pipeline:
//!   parse -> review -> verify -> codegen -> .class
//!
//! ## Modules
//! - `ast`: AST nodes & visitors (pure)
//! - `parser`: `parse(&str) -> Result<AST, ParseError>`
//! - `review`: `run_all(&mut AST)`
//! - `verify`: JVM-level checks `verify_class(..)`
//! - `codegen`: `ClassWriter`, `MethodWriter`, `ConstPool`, `Opcode`
//!
//! ## Error Model
//! All subsystem errors map to `tolc::Error`.

#![deny(missing_docs)]

pub mod ast;
pub mod parser;
pub mod review;
pub mod verify;
pub mod codegen;
pub mod error;

/// Frequently used items for quick importing.
pub mod prelude {
    pub use crate::ast::{nodes::*, visitor::*, printer::Printer};
    pub use crate::parser::{parse, error::ParseError, span::Span};
    pub use crate::review::run_all;
    pub use crate::verify::{verify_class, error::VerifyError};
    pub use crate::codegen::{ClassWriter, MethodWriter, ConstPool, Opcode};
    pub use crate::error::Error;
}
```

### `src/ast/mod.rs`
```rust
//! AST domain model. Pure; no I/O.
//! Called by: parser (produce), review/verify/codegen (consume).

pub mod nodes;
pub mod visitor;
pub mod printer;

pub use nodes::*;
pub use visitor::*;
pub use printer::Printer;
```

### `src/parser/mod.rs`
```rust
//! Lexing & parsing into AST. Returns spans and rich errors.

pub mod lexer;
pub mod parser;
pub mod error;
pub mod span;

pub use parser::parse;
pub use error::ParseError;
pub use span::{Span, Spanned};
```

### `src/review/mod.rs`
```rust
//! Post-parse checks and normalization passes.

mod annotation;
mod class;
mod consts;
mod enums;
mod fields;
mod imports;
mod interface;
mod methods;
mod package;
mod statements;
mod types;

use crate::error::Error;

pub fn run_all(ast: &mut crate::ast::CompilationUnit) -> Result<(), Error> {
    // call passes; convert pass-specific errors into crate::Error
    Ok(())
}
```

### `src/verify/mod.rs`
```rust
//! JVM classfile verification (flags, cp, signatures, methods).

pub mod attributes;
pub mod class_access_flags;
pub mod constant_pool;
pub mod fields;
pub mod interfaces;
pub mod method_access_flags;
pub mod methods;
pub mod signature;
pub mod verifier;
pub mod error;

pub use verifier::verify_class;
pub use error::VerifyError;
```

### `src/codegen/mod.rs`
```rust
//! Code generation for JVM classfiles.

pub mod annotation;
pub mod attribute;
pub mod class;
pub mod class_writer;
pub mod constpool;
pub mod defs;
pub mod descriptor;
pub mod error;
pub mod field;
pub mod flag;
pub mod frame;
pub mod method;
pub mod method_writer;
pub mod opcodes;
pub mod typed_index;
pub mod writer;

pub(crate) mod bytecode;
pub(crate) mod opcodor;
pub(crate) mod vec;

pub use class_writer::ClassWriter;
pub use method_writer::MethodWriter;
pub use constpool::ConstPool;
pub use opcodes::Opcode;
pub use error::CodegenError;
```

### `src/error.rs` (crate-level mapping)
```rust
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] crate::parser::error::ParseError),
    #[error(transparent)]
    Verify(#[from] crate::verify::error::VerifyError),
    #[error(transparent)]
    Codegen(#[from] crate::codegen::error::CodegenError),
}
```

---

## 4) Size Rules (Targets & Hard Caps)
| Item        | Ideal Range | Caution  | Hard Cap | Notes |
|-------------|-------------|----------|----------|-------|
| `.rs` file  | 200–500 LOC | ~800 LOC | ~1000 LOC| Split by responsibility/domain |
| Function    | 10–50 LOC   | ~80 LOC  | ~100 LOC | Extract helpers; make contracts explicit |
| Module public API | Small & stable | — | — | Re-export via `mod.rs`/`lib.rs` |

> Approaching the **caution** zone -> plan a split. Hitting the **cap** -> split immediately.

---

## 5) Documentation Layering (Required)
- **Crate overview in `lib.rs`** (20–60 lines, `//!`): pipeline, modules, dataflow, constraints.
- **Module header** (5–10 lines, `//!`): responsibility, inputs/outputs, who calls it.
- **Public API rustdoc** (`///`) with **`# Examples`**: AI learns usage from examples.

**Module header template**
```rust
//! Purpose: <one sentence>
//! Inputs: <types>
//! Outputs: <types>
//! Called by: <modules>
//! Notes: <constraints/side effects>
```

---

## 6) Decomposition Triggers
- File nears **~600–800 LOC**.
- Function > **~80 LOC**, **3+ nested blocks**, or **5+ branches**.
- Mixed concerns (pure logic + I/O) in one unit.
- Multiple audiences (domain + CLI + adapters) mixed together.

**Common splits**
- `types.rs` for data structures
- `logic.rs` for pure business logic
- `error.rs` or `errors/*.rs` for typed errors

---

## 7) Errors & Results
- Use **typed errors** (`thiserror`) per subsystem.
- Map all subsystem errors into **`tolc::Error`** (see `src/error.rs`).
- Convert external/adapter errors **early** at boundaries.

---

## 8) Tests as Executable Specs
- Keep **teaching-style tests** close to features in `tests/` (you already do this 👍).
- For each public API, include **one happy path + one failure** test.
- Name tests by behavior (e.g., `review_import_precedence_tests.rs`).

---

## 9) Prelude & Imports
- Provide `tolc::prelude::*` as a **stable import surface**.
- Prefer examples/binaries to import from `prelude` or top-level re-exports, not deep paths.
- Fewer stable entrypoints -> fewer wrong imports suggested by AI.

---

## 10) Linting & Formatting
- Enforce `rustfmt` and `clippy` in CI. If using `#[allow(...)]`, include a short **justification**.
- Keep a `Makefile` or `justfile` with tasks: `build`, `test`, `fmt`, `clippy`, `doc`.

---

## 11) “AI-Ready” Checklists
**New Module Checklist**
- [ ] Header (`//!`) explains role, inputs, outputs, callers.
- [ ] Public APIs have `///` docs with `# Examples`.
- [ ] Functions ≤ 50–80 LOC; pure logic separated from I/O.
- [ ] Errors mapped to `crate::error::Error` if crossing boundary.
- [ ] Tests cover happy path + one failure.

**PR Checklist**
- [ ] Any file > 600 LOC considered for split; none > 1000 LOC.
- [ ] No function > 100 LOC or with ≥ 3 nested blocks (refactor if so).
- [ ] `lib.rs` overview updated if boundaries changed.
- [ ] `clippy` & `rustfmt` pass; any `allow` has a reason.

---

## 12) Quick Start (binary uses the library)
```rust
use tolc::prelude::*;

fn main() -> Result<(), tolc::Error> {
    let src = std::fs::read_to_string("HelloWorld.java")?;
    let mut cu = parse(&src)?;
    run_all(&mut cu)?;
    // depending on verify API: pass AST or intermediate class model
    let class = verify_class(&cu)?;
    let bytes = ClassWriter::from(&class).to_bytes()?;
    std::fs::create_dir_all("out")?;
    std::fs::write("out/HelloWorld.class", bytes)?;
    Ok(())
}
```

---

## 13) Exceptions (When Big Files Are OK)
- Glue/index files (`mod.rs` with re-exports) can be longer.
- Large generated code or static tables are fine if **isolated** and clearly marked.

---

## 14) Rationale
- Smaller files improve **chunking and retrieval** for embedding-based tools.
- Layered docs give AI a **map before territory**.
- Consistent error and API surfaces reduce **wrong imports** and **type mismatches**.
- Teaching-style tests provide grounded **usage patterns** for safe code generation.
