# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**tolc** (Terminos Language Compiler) is a Java8-compatible compiler written in Rust that compiles `.tol` source files to Java bytecode (`.class` files). The project implements a complete JavaC-aligned compilation pipeline with seven distinct phases.

### Ultimate Goal
The ultimate goal of tolc is to implement a **Rust-based equivalent of javac** - providing the same functionality, compatibility, and behavior as Oracle's official Java compiler. When encountering parsing, AST generation, or class file generation issues, **always align with the official javac implementation** located at `../javac/src/share/classes/com/sun/tools/javac`.

### Reference Implementation
- **Official javac source**: `../javac/src/share/classes/com/sun/tools/javac`
- **Key javac modules to reference**:
  - `com.sun.tools.javac.parser` - For parsing and lexical analysis alignment
  - `com.sun.tools.javac.tree` - For AST structure and node definitions  
  - `com.sun.tools.javac.jvm` - For bytecode generation and class file format
  - `com.sun.tools.javac.comp` - For semantic analysis and type checking

## Compilation Pipeline

tolc follows the exact JavaC compilation flow with seven distinct phases:

```
//! Parser → Enter → Attr → Flow → TransTypes → Lower → CodeGen
//!        symbols  type-inf  flow-ana  generic-era  desugar  bytecode-gen
```

### Phase-by-Phase Breakdown

1. **Parser** (`src/parser/`)
   - **Input**: `.tol` source files
   - **Output**: Abstract Syntax Tree (AST)
   - **Purpose**: Lexical analysis and syntax parsing
   - **JavaC Equivalent**: `com.sun.tools.javac.parser.JavacParser`

2. **Enter** (`src/codegen/enter.rs`)
   - **Input**: Raw AST from parser
   - **Output**: AST with symbol tables
   - **Purpose**: Symbol table construction, import resolution, and scope establishment
   - **JavaC Equivalent**: `com.sun.tools.javac.comp.Enter`

3. **Attr** (`src/codegen/attr.rs`)
   - **Input**: AST with symbol tables
   - **Output**: AST with complete type information
   - **Purpose**: Type checking, method resolution, and type inference
   - **JavaC Equivalent**: `com.sun.tools.javac.comp.Attr`

4. **Flow** (`src/codegen/flow.rs`)
   - **Input**: Type-checked AST
   - **Output**: AST with flow analysis results
   - **Purpose**: Definite assignment analysis, reachability analysis, and exception flow
   - **JavaC Equivalent**: `com.sun.tools.javac.comp.Flow`

5. **TransTypes** (`src/codegen/trans_types.rs`)
   - **Input**: Flow-analyzed AST
   - **Output**: AST with erased generics
   - **Purpose**: Generic type erasure and bridge method generation
   - **JavaC Equivalent**: `com.sun.tools.javac.comp.TransTypes`

6. **Lower** (`src/codegen/lower.rs`)
   - **Input**: Type-erased AST
   - **Output**: Desugared AST
   - **Purpose**: Syntactic sugar desugaring (enhanced for loops, string concatenation, etc.)
   - **JavaC Equivalent**: `com.sun.tools.javac.comp.Lower`

7. **CodeGen** (`src/codegen/gen.rs`, `src/codegen/gen_visitor.rs`)
   - **Input**: Desugared AST
   - **Output**: Java bytecode (`.class` files)
   - **Purpose**: Bytecode generation with optimization passes
   - **JavaC Equivalent**: `com.sun.tools.javac.jvm.Gen`

### Pipeline Orchestration

The complete pipeline is orchestrated by the `SemanticAnalyzer` struct in `src/codegen/mod.rs`:

```rust
pub struct SemanticAnalyzer {
    pub enter: enter::Enter,           // Phase 2: Symbol tables
    pub attr: attr::Attr,             // Phase 3: Type checking
    pub flow: flow::Flow,             // Phase 4: Flow analysis
    pub trans_types: trans_types::TransTypes,  // Phase 5: Generic erasure
    pub lower: lower::Lower,          // Phase 6: Desugaring
}

impl SemanticAnalyzer {
    pub fn analyze(&mut self, mut ast: Ast) -> Result<Ast> {
        // Phase 2: Enter - Build symbol tables
        ast = self.enter.process(ast)?;
        let symbol_env = self.enter.get_symbol_environment();
        
        // Phase 3: Attr - Type checking and resolution
        ast = self.attr.process_with_symbols(ast, Some(symbol_env))?;
        
        // Phase 4: Flow - Definite assignment analysis
        ast = self.flow.process_with_symbols(ast, Some(symbol_env))?;
        
        // Phase 5: TransTypes - Generic type erasure
        ast = self.trans_types.process_with_types(ast, &self.attr.get_type_information())?;
        
        // Phase 6: Lower - Desugar syntax
        ast = self.lower.process_with_types(ast, &self.trans_types.get_erased_types())?;
        
        // Phase 7: CodeGen happens separately via ClassWriter
        Ok(ast)
    }
}
```

## Common Development Commands

### Building and Testing
```bash
# Build the project
cargo build

# Build release version
cargo build --release

# Run all tests
cargo test

# Run specific test file
cargo test stackmap_array_and_dup_tests

# Run tests with output
cargo test -- --nocapture

# Run java_suite bytecode comparison tests
cargo test --test java_suite

# Run java_suite with specific file filter
TOLC_FILTER_FILE=BitSet.java cargo test --test java_suite

# Run benchmarks
cargo bench
```

### Running tolc CLI
```bash
# Basic compilation
cargo run -- compile input.tol -o build/

# Compile with verbose output and Java 11 target
cargo run -- compile input.tol -o build/ -v --target-version 11

# Parse file and show AST
cargo run -- parse input.tol --detailed

# Lexical analysis
cargo run -- lex input.tol --locations

# Compile with debug frames
cargo run -- compile input.tol -o build/ --debug-frames

# Compile without StackMapTable frames
cargo run -- compile input.tol -o build/ --no-frames
```

### Testing with Java files
```bash
# Test compilation with Java files in tests/java/
cargo run -- compile tests/java/lang/String.java -o build/

# Run compiled Java bytecode
cd build && java com.example.ClassName

# Compare with javac reference implementation
javac tests/java/lang/String.java -d reference_build/
# Compare generated .class files for alignment verification
```

## Architecture Overview

### Module Structure
- **`src/parser/`** - Phase 1: Lexical analysis and parsing of `.tol` files into AST using logos and peg crates
- **`src/ast/`** - Abstract Syntax Tree representation with visitor pattern support
- **`src/codegen/`** - Phases 2-7: Complete semantic analysis pipeline and bytecode generation
  - **`enter.rs`** - Phase 2: Symbol table construction and import resolution
  - **`attr.rs`** - Phase 3: Type checking, method resolution, and type inference
  - **`flow.rs`** - Phase 4: Definite assignment and reachability analysis
  - **`trans_types.rs`** - Phase 5: Generic type erasure and bridge method generation
  - **`lower.rs`** - Phase 6: Syntactic sugar desugaring
  - **`gen.rs`, `gen_visitor.rs`** - Phase 7: Bytecode generation and optimization
  - **`class_writer.rs`** - Main class file generation orchestrator
  - **`mod.rs`** - `SemanticAnalyzer` pipeline orchestration
- **`src/verify/`** - JVM classfile verification and validation
- **`src/review/`** - Additional semantic analysis and type checking utilities
- **`src/bin/main.rs`** - CLI interface similar to javac with compile/parse/lex subcommands

### Key Components

#### Parser (`src/parser/`)
- `lexer.rs` - Tokenization using logos crate
- `parser.rs` - PEG-based parsing to AST
- `error.rs` - Error recovery mechanisms
- `span.rs` - Source location tracking

#### Semantic Analysis & Code Generation (`src/codegen/`)

**Semantic Analysis Pipeline (Phases 2-6):**
- `enter.rs` - Symbol table construction and import resolution
- `attr.rs` - Type checking, method resolution, and type inference
- `flow.rs` - Definite assignment and reachability analysis
- `trans_types.rs` - Generic type erasure and bridge method generation
- `lower.rs` - Syntactic sugar desugaring

**Code Generation (Phase 7):**
- `gen.rs` - Main bytecode generator (corresponds to javac Gen.java)
- `gen_visitor.rs` - Compiler-aligned visitor methods for bytecode generation
- `class_writer.rs` - Main class file generation orchestrator
- `constpool.rs` - Constant pool management
- `opcodes.rs` - JVM instruction definitions
- `items.rs` - Stack item management and bytecode emission
- `jump_chain_optimizer.rs` - Jump chain optimization (replaces old javac-prefixed system)
- Active optimizer modules: `branch_optimizer.rs`, `line_number_optimizer.rs`, `stack_map_optimizer.rs`, `register_alloc.rs`
- `stackmap_*` modules - StackMapTable frame generation
- `code.rs` - Compiler-aligned code buffer management

**Pipeline Orchestration:**
- `mod.rs` - `SemanticAnalyzer` struct that orchestrates phases 2-6

#### AST (`src/ast/`)
- `nodes.rs` - All AST node definitions
- `visitor.rs` - Visitor pattern implementation
- `printer.rs` - AST pretty printing

### Configuration System
The `Config` struct in `src/config.rs` controls:
- Target Java version (6-17, default: 8)
- StackMapTable frame generation options
- Debug output settings
- Optimization levels

### Test Organization
- Tests in `tests/` directory follow pattern `[feature]_tests.rs`
- Copyright-safe naming: Removed all `javac_*` prefixes from test file names
- Java source files in `tests/java/` mirror JDK package structure
- Compiled `.class` files in `tests/classes/` for reference
- Integration tests cover: stackmap computation, optimizations, parser edge cases, bytecode generation
- `java_suite.rs` - Comprehensive bytecode comparison framework against javac output

### Codebase Cleanup Status
- ✅ **Removed unused optimizers**: Cleaned up 13 unused optimizer modules that were never integrated
- ✅ **Copyright compliance**: All javac/JavaC/javaC prefixes removed from file names and class names
- ✅ **Consolidated architecture**: 4 active optimizers remain, integrated into the main compilation pipeline
- ✅ **Test suite integrity**: All 91 unit tests + integration tests passing after cleanup

## Development Patterns

### Error Handling
- Use `crate::error::Result<T>` for all fallible operations
- Error types defined in `src/error.rs` with thiserror
- Parser errors support recovery and continue parsing

### Adding New Features

Follow the complete 7-phase pipeline when adding new language features:

1. **First**: Reference official javac implementation in `../javac/src/share/classes/com/sun/tools/javac`

2. **Phase 1 - Parser** (`src/parser/parser.rs`):
   - Update lexer and parser rules (align with `com.sun.tools.javac.parser`)
   - Update AST nodes in `src/ast/nodes.rs` if needed (align with `com.sun.tools.javac.tree`)

3. **Phase 2 - Enter** (`src/codegen/enter.rs`):
   - Add symbol table entries for new constructs (align with `com.sun.tools.javac.comp.Enter`)

4. **Phase 3 - Attr** (`src/codegen/attr.rs`):
   - Add type checking and resolution logic (align with `com.sun.tools.javac.comp.Attr`)

5. **Phase 4 - Flow** (`src/codegen/flow.rs`):
   - Add flow analysis for new constructs (align with `com.sun.tools.javac.comp.Flow`)

6. **Phase 5 - TransTypes** (`src/codegen/trans_types.rs`):
   - Handle generic type erasure if applicable (align with `com.sun.tools.javac.comp.TransTypes`)

7. **Phase 6 - Lower** (`src/codegen/lower.rs`):
   - Add desugaring logic if needed (align with `com.sun.tools.javac.comp.Lower`)

8. **Phase 7 - CodeGen** (`src/codegen/gen.rs`, `src/codegen/gen_visitor.rs`):
   - Add bytecode generation (align with `com.sun.tools.javac.jvm.Gen`)

9. **Testing**: Add comprehensive tests in `tests/` covering all phases

### Bytecode Generation (Phase 7)
- All bytecode generation goes through `ClassWriter` which orchestrates the complete pipeline
- The `SemanticAnalyzer` processes phases 2-6 before CodeGen
- `Gen` and `GenVisitor` handle the actual bytecode emission (Phase 7)
- StackMapTable frames are computed automatically unless disabled
- Multiple optimization passes run in sequence during code generation
- Verification happens before writing `.class` files

### Pipeline Integration
For complete compilation, use the integrated pipeline:
```rust
// Complete compilation with all phases
let mut semantic_analyzer = SemanticAnalyzer::new();
let processed_ast = semantic_analyzer.analyze(ast)?;  // Phases 2-6
let bytecode = codegen::generate_bytecode(&processed_ast, output_dir, &config)?;  // Phase 7
```

### Testing Java Compatibility
- Place test `.java` files in `tests/java/` following package structure
- Compile to `tests/classes/` for reference comparison
- Test both parsing and bytecode generation stages

## Specialized Tooling

### StackMapTable Generation
Complex frame computation with multiple algorithms:
- `enhanced_stack_map_emitter.rs` - Primary frame generation
- `stackmap_*_tests.rs` - Comprehensive frame testing
- Debug frames can be enabled with `--debug-frames`

### Active Optimization Pipeline
4 core optimization modules (cleaned up from 20+ unused modules):
- **Branch optimization**: Conditional branch elimination and jump chain optimization
- **Line number optimization**: Debug information compression and line number table optimization  
- **Stack map optimization**: StackMapTable frame compression and optimization
- **Register allocation**: Local variable slot optimization and register allocation
- **Jump chain optimization**: Compiler-aligned jump resolution and dead code elimination

### Parser Error Recovery
- Lenient parsing mode continues after errors
- Detailed error location reporting with spans
- Multiple error collection and reporting

## JavaC Alignment Guidelines

### When to Reference Official JavaC

Each phase has a direct JavaC equivalent for alignment reference:

- **Phase 1 - Parser Issues**: Check `com.sun.tools.javac.parser.JavacParser` for precedence, associativity, and syntax rules
- **Phase 1 - AST Structure**: Align node definitions with `com.sun.tools.javac.tree.JCTree` hierarchy  
- **Phase 2 - Symbol Tables**: Reference `com.sun.tools.javac.comp.Enter` for symbol resolution and scope management
- **Phase 3 - Type System**: Follow `com.sun.tools.javac.comp.Attr` for type checking, inference, and method resolution
- **Phase 4 - Flow Analysis**: Align with `com.sun.tools.javac.comp.Flow` for definite assignment and reachability
- **Phase 5 - Generic Erasure**: Reference `com.sun.tools.javac.comp.TransTypes` for type erasure and bridge methods
- **Phase 6 - Desugaring**: Follow `com.sun.tools.javac.comp.Lower` for syntactic sugar removal
- **Phase 7 - Bytecode Generation**: Reference `com.sun.tools.javac.jvm.Gen` and `com.sun.tools.javac.jvm.ClassWriter`
- **Error Handling**: Align error messages and recovery with javac behavior across all phases

### Current Project Status (2025-08-25)
- ✅ **Core compilation pipeline**: All 7 phases implemented and functional
- ✅ **Parser**: Complete Java 8 syntax support with error recovery
- ✅ **Semantic analysis**: Symbol tables, type checking, flow analysis working
- ✅ **Bytecode generation**: Fully functional with 20+ optimization passes
- ✅ **Array assignments**: Fixed support for `array[index] = value` expressions
- ✅ **Copyright compliance**: Removed all javac/JavaC prefixes from file and class names
- ✅ **Test suite**: 91 unit tests + comprehensive integration tests all passing
- ✅ **Jump chain optimization**: Replaced old system with JavaC-aligned JumpChainOptimizer

### Current Alignment Issues
- **Java Suite bytecode differences**: Several files show instruction-level differences from javac output
- **Method invocation patterns**: Parameter ordering and instruction sequences may differ from javac
- **Constant pool organization**: Entry ordering and reference patterns need javac alignment
- **Array operations**: Some array access patterns may generate different bytecode than javac

### Verification Process
1. **Behavioral Compatibility**: Ensure tolc produces same results as javac for identical inputs
2. **Bytecode Compatibility**: Generated `.class` files should be functionally equivalent
3. **Error Compatibility**: Error messages and recovery should match javac patterns
4. **Feature Parity**: Support same Java language features in same manner as javac
5. **Instruction-level Alignment**: Use `javap -c -verbose` to compare tolc vs javac bytecode

### Common JavaC Reference Points

**Phase 1 - Parsing:**
- **Operator Precedence**: `com.sun.tools.javac.parser.JavacParser.precedence`
- **AST Nodes**: `com.sun.tools.javac.tree.JCTree` subclasses
- **Parser Methods**: `com.sun.tools.javac.parser.JavacParser.parseXxx`

**Phase 2 - Enter:**
- **Symbol Table**: `com.sun.tools.javac.comp.Enter.visitXxx`
- **Scope Management**: `com.sun.tools.javac.code.Scope`

**Phase 3 - Attr:**
- **Type Checking**: `com.sun.tools.javac.comp.Attr.visitXxx` methods
- **Symbol Resolution**: `com.sun.tools.javac.comp.Resolve`
- **Type System**: `com.sun.tools.javac.code.Type` and `com.sun.tools.javac.code.Types`

**Phase 4 - Flow:**
- **Flow Analysis**: `com.sun.tools.javac.comp.Flow.visitXxx`
- **Definite Assignment**: `com.sun.tools.javac.comp.Flow.AssignAnalyzer`

**Phase 5 - TransTypes:**
- **Type Erasure**: `com.sun.tools.javac.comp.TransTypes.visitXxx`
- **Bridge Methods**: `com.sun.tools.javac.comp.TransTypes.addBridge`

**Phase 6 - Lower:**
- **Desugaring**: `com.sun.tools.javac.comp.Lower.visitXxx`
- **Enhanced For**: `com.sun.tools.javac.comp.Lower.visitForeachLoop`

**Phase 7 - CodeGen:**
- **Bytecode Generation**: `com.sun.tools.javac.jvm.Gen.visitXxx`
- **Method Invocation**: `com.sun.tools.javac.jvm.Gen.visitApply`
- **Bytecode Instructions**: `com.sun.tools.javac.jvm.ByteCodes`

## Bytecode Alignment and Comparison

### Java Suite Testing Framework

The `java_suite` test is the primary framework for comparing class file differences between tolc and javac output. It systematically compiles Java files and compares the generated bytecode.

**Basic Usage:**
```bash
# Run full java_suite comparison
cargo test --test java_suite

# Filter to specific file for focused analysis
TOLC_FILTER_FILE=ArraysAbstractList.java cargo test --test java_suite

# Run with output to see detailed differences
TOLC_FILTER_FILE=BitSet.java cargo test --test java_suite -- --nocapture
```

### Bytecode Analysis Methodology

**1. Systematic Comparison Process:**
- Java files are processed according to the `file_list` order in `java_suite.rs`
- Each file is compiled by both javac and tolc
- Bytecode differences are identified using `javap -c -verbose` output comparison
- Differences are categorized by type: instruction sequences, constant pool, stack frames

**2. Analysis Workflow:**
```bash
# Step 1: Compile with javac (reference implementation)
javac -d reference_build/ tests/java/util/BitSet.java

# Step 2: Compile with tolc 
cargo run -- compile tests/java/util/BitSet.java -o tolc_build/

# Step 3: Compare bytecode using javap
javap -c -verbose reference_build/java/util/BitSet > javac_output.txt
javap -c -verbose tolc_build/java/util/BitSet > tolc_output.txt
diff javac_output.txt tolc_output.txt
```

**3. Priority Analysis Order:**
1. **First**: Analyze the javac implementation in `../javac/src/share/classes/com/sun/tools/javac`
2. **Second**: Examine the current tolc implementation 
3. **Third**: Align tolc behavior to match javac patterns exactly

### Common Bytecode Differences

**Instruction Sequence Variations:**
- Method call parameter ordering
- Stack manipulation patterns (dup, swap operations)
- Local variable slot allocation
- Branch target calculation methods

**Constant Pool Differences:**
- Entry ordering and indexing
- String literal pooling strategies
- Method/field reference organization
- UTF8 constant sharing patterns

**Stack Frame Variations:**
- StackMapTable frame generation timing
- Local variable type tracking
- Exception handler table organization

### JavaC Reference Analysis

**Key javac files for bytecode generation:**
- `com.sun.tools.javac.jvm.Gen` - Main bytecode generation logic
- `com.sun.tools.javac.jvm.Code` - Code buffer and instruction emission
- `com.sun.tools.javac.jvm.ClassWriter` - Class file structure and output
- `com.sun.tools.javac.jvm.Items` - Stack item management and optimization

**Critical alignment points:**
- Method invocation patterns in `Gen.visitApply()`
- Local variable allocation in `Code.newLocal()`
- Constant pool management in `ClassWriter`
- Jump instruction optimization in `Code` class

### File-Specific Analysis Guidelines

**When analyzing specific files from java_suite:**
```bash
# Focus on a single problematic file
TOLC_FILTER_FILE=ArraysAbstractList.java cargo test --test java_suite

# Generate detailed comparison reports  
TOLC_FILTER_FILE=Objects.java cargo test --test java_suite -- --nocapture > comparison_report.txt
```

**Files requiring priority attention:**
- `BitSet.java` - Complex bit manipulation, method calls
- `ArraysAbstractList.java` - Generic type handling, inheritance
- `HashMapMyIterator.java` - Iterator patterns, inner classes
- `Objects.java` - Static method calls, null handling

### Optimization Alignment Strategy

1. **Disable tolc optimizations** to match raw javac output first
2. **Enable optimizations incrementally** to maintain javac compatibility
3. **Verify each optimization** preserves javac behavioral equivalence
4. **Document deviations** where tolc intentionally improves upon javac

The goal is instruction-level alignment with javac while maintaining or improving performance characteristics.

## Documentation and Code Comment Standards

### English-Only Rule
**IMPORTANT**: All documentation and code comments in this project must be written in English only.

This includes:
- **Code comments**: All `//` and `/* */` comments in Rust source files
- **Documentation comments**: All `///` and `//!` rustdoc comments
- **Markdown files**: All `.md` documentation files (README, guides, plans, etc.)
- **Error messages**: All user-facing error messages and debug output
- **Variable and function names**: Use descriptive English names
- **Git commit messages**: All commit messages should be in English

### Rationale
- **International accessibility**: Enables global contributors to understand and contribute
- **Professional standards**: Follows open-source project conventions
- **Maintainability**: Ensures consistent documentation across the codebase
- **JavaC alignment**: Matches the documentation style of reference implementation

### Comment Writing Rules
**MANDATORY**: When writing any comments in code:
1. **Use English exclusively** - No Chinese, Japanese, or other non-English languages
2. **Be descriptive and clear** - Comments should help international developers understand the code
3. **Follow consistent style** - Use proper grammar and punctuation
4. **Technical terminology** - Use standard English technical terms that align with JavaC documentation

### Enforcement
- All new code must include English-only comments
- All documentation updates must be in English
- Code reviews should verify English-only compliance
- Translation of existing Chinese content to English is required when updating files
- **Zero tolerance policy**: Any non-English comments must be converted to English before merging