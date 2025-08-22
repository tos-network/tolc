# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**tolc** (Terminos Language Compiler) is a Java8-compatible compiler written in Rust that compiles `.tol` source files to Java bytecode (`.class` files). The project uses a three-stage compilation pipeline: lexical analysis → parsing → code generation.

### Ultimate Goal
The ultimate goal of tolc is to implement a **Rust-based equivalent of javac** - providing the same functionality, compatibility, and behavior as Oracle's official Java compiler. When encountering parsing, AST generation, or class file generation issues, **always align with the official javac implementation** located at `../javac/src/share/classes/com/sun/tools/javac`.

### Reference Implementation
- **Official javac source**: `../javac/src/share/classes/com/sun/tools/javac`
- **Key javac modules to reference**:
  - `com.sun.tools.javac.parser` - For parsing and lexical analysis alignment
  - `com.sun.tools.javac.tree` - For AST structure and node definitions  
  - `com.sun.tools.javac.jvm` - For bytecode generation and class file format
  - `com.sun.tools.javac.comp` - For semantic analysis and type checking

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
- **`src/parser/`** - Lexical analysis and parsing of `.tol` files into AST using logos and peg crates
- **`src/ast/`** - Abstract Syntax Tree representation with visitor pattern support
- **`src/codegen/`** - Bytecode generation from AST to Java `.class` files with extensive optimization passes
- **`src/verify/`** - JVM classfile verification and validation
- **`src/review/`** - Semantic analysis and type checking
- **`src/bin/main.rs`** - CLI interface similar to solc with compile/parse/lex subcommands

### Key Components

#### Parser (`src/parser/`)
- `lexer.rs` - Tokenization using logos crate
- `parser.rs` - PEG-based parsing to AST
- `error.rs` - Error recovery mechanisms
- `span.rs` - Source location tracking

#### Code Generation (`src/codegen/`)
- `class_writer.rs` - Main class file generation
- `method_writer.rs` - Method bytecode generation
- `constpool.rs` - Constant pool management
- `opcodes.rs` - JVM instruction definitions
- Multiple optimizer modules (20+ optimization passes)
- `stackmap_*` modules - StackMapTable frame generation
- `enhanced_stack_map_emitter.rs` - Advanced frame computation

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
- Java source files in `tests/java/` mirror JDK package structure
- Compiled `.class` files in `tests/classes/` for reference
- Integration tests cover: stackmap computation, optimizations, parser edge cases, bytecode generation

## Development Patterns

### Error Handling
- Use `crate::error::Result<T>` for all fallible operations
- Error types defined in `src/error.rs` with thiserror
- Parser errors support recovery and continue parsing

### Adding New Features
1. **First**: Reference official javac implementation in `../javac/src/share/classes/com/sun/tools/javac`
2. Update AST nodes in `src/ast/nodes.rs` if needed (align with `com.sun.tools.javac.tree`)
3. Extend parser in `src/parser/parser.rs` (align with `com.sun.tools.javac.parser`)
4. Add codegen support in appropriate `src/codegen/` modules (align with `com.sun.tools.javac.jvm`)
5. Add comprehensive tests in `tests/`

### Bytecode Generation
- All bytecode generation goes through `ClassWriter` and `MethodWriter`
- StackMapTable frames are computed automatically unless disabled
- Multiple optimization passes run in sequence
- Verification happens before writing `.class` files

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

### Optimization Pipeline
20+ optimization passes including:
- Constant folding and propagation
- Dead code elimination  
- String buffer optimization
- Method invocation optimization
- Type coercion optimization

### Parser Error Recovery
- Lenient parsing mode continues after errors
- Detailed error location reporting with spans
- Multiple error collection and reporting

## JavaC Alignment Guidelines

### When to Reference Official JavaC
- **Parser Issues**: Check `com.sun.tools.javac.parser.JavacParser` for precedence, associativity, and syntax rules
- **AST Structure**: Align node definitions with `com.sun.tools.javac.tree.JCTree` hierarchy
- **Bytecode Generation**: Reference `com.sun.tools.javac.jvm.Gen` and `com.sun.tools.javac.jvm.ClassWriter`
- **Type System**: Follow `com.sun.tools.javac.comp.Attr` for type checking and inference
- **Error Handling**: Align error messages and recovery with javac behavior

### Current Alignment Issues
- **BitSet.java bytecode differs from javac**: Method call patterns and instruction sequences need alignment
- **Math.max() static calls**: Ensure parameter order and invokestatic generation matches javac
- **Array.length access**: Verify arraylength instruction generation timing
- **Method invocation optimization**: May be generating different instruction patterns than javac

### Verification Process
1. **Behavioral Compatibility**: Ensure tolc produces same results as javac for identical inputs
2. **Bytecode Compatibility**: Generated `.class` files should be functionally equivalent
3. **Error Compatibility**: Error messages and recovery should match javac patterns
4. **Feature Parity**: Support same Java language features in same manner as javac
5. **Instruction-level Alignment**: Use `javap -c -verbose` to compare tolc vs javac bytecode

### Common JavaC Reference Points
- **Operator Precedence**: `com.sun.tools.javac.parser.JavacParser.precedence`
- **AST Nodes**: `com.sun.tools.javac.tree.JCTree` subclasses
- **Bytecode Instructions**: `com.sun.tools.javac.jvm.ByteCodes`
- **Type Checking**: `com.sun.tools.javac.comp.Attr.visitXxx` methods
- **Symbol Resolution**: `com.sun.tools.javac.comp.Resolve`
- **Method Invocation**: `com.sun.tools.javac.jvm.Gen.visitApply` for method call generation

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