# Terminos Language Compiler (tolc)

A fast, deterministic compiler for the **Terminos Language**—a Java8-compatible language that compiles `.tol` source files to Java bytecode (`.class` files).

---

## Overview

`tolc` parses Terminos source files (`.tol`), performs lexical analysis and parsing into an Abstract Syntax Tree (AST), and generates deployable **Java bytecode** that can run on any Java Virtual Machine (JVM). The language is designed to be fully compatible with Java 8 syntax and semantics while providing a modern compilation pipeline.

- **Target:** JVM bytecode (Java 8+ compatible)
- **Language:** Java8-compatible syntax with `.tol` file extension
- **Determinism:** No syscalls or nondeterministic I/O; deterministic compilation
- **Tooling:** Command-line interface with multiple compilation modes

---

## Key Features

- **Java8 Compatibility** - Full support for Java 8 language features including classes, interfaces, enums, annotations, generics, and all control flow constructs
- **Lexical Analysis** - Robust tokenization with detailed error reporting and location tracking
- **AST Generation** - Complete Abstract Syntax Tree representation with visitor pattern support
- **Bytecode Generation** - Direct compilation to JVM bytecode with StackMapTable support
- **Multiple Output Formats** - Generate `.class` files, parse trees, or lexical analysis results
- **Configurable Targets** - Support for Java classfile versions 6-17 (default: Java 8)
- **Debug Support** - Optional StackMapTable frame generation with diagnostics
- **Error Handling** - Comprehensive error reporting with source location information
- **Visitor Pattern** - Extensible AST traversal and manipulation capabilities

---

## Quick Start

### CLI

```bash
# Version and build info
tolc --version

# Compile .tol file to .class files
tolc compile Counter.tol -o build/

# Compile with verbose output and custom Java version
tolc compile Counter.tol -o build/ -v --target-version 11

# Parse .tol file and show AST
tolc parse Counter.tol --detailed

# Lexical analysis of .tol file
tolc lex Counter.tol --locations

# Compile with debug frame information
tolc compile Counter.tol -o build/ --debug-frames

# Compile without StackMapTable frames
tolc compile Counter.tol -o build/ --no-frames
```

### Compilation Options

```bash
# Basic compilation
tolc compile <input.tol> -o <output_dir>

# Advanced compilation with all options
tolc compile <input.tol> \
  -o <output_dir> \
  -v \
  --target-version 11 \
  --debug-frames
```



---

## Inputs, Imports, Remappings

- **Input files:** `.tol` files with Java8-compatible syntax
- **Output:** Java bytecode (`.class` files) in the specified output directory
- **Target version:** Configurable Java classfile version (6-17, default: 8)
- **Frame generation:** Optional StackMapTable generation for JVM compatibility

---

## Compilation Process

The compilation pipeline consists of three main stages:

1. **Lexical Analysis** - Tokenizes source code with location tracking
2. **Parsing** - Generates Abstract Syntax Tree (AST) from tokens
3. **Code Generation** - Emits JVM bytecode with optional optimizations

---

## Artifacts

- **`<ClassName>.class`** – Compiled Java bytecode files
- **AST Output** – Detailed parse tree representation (use `--detailed` flag)
- **Token Analysis** – Lexical analysis results with location information
- **Debug Information** – Optional StackMapTable frames for JVM verification

---

## Language Features

- **Classes & Interfaces** - Full Java 8 class and interface support
- **Generics** - Type parameter support with bounds
- **Annotations** - Custom annotation processing
- **Enums** - Enumeration type support
- **Control Flow** - All Java control structures (if, while, for, switch, try-catch)
- **Expressions** - Complete expression language with operator precedence
- **Modifiers** - Access control and method/field modifiers
- **Exceptions** - Exception handling and throwing

---

## Advanced Features

- **StackMapTable Generation** - Optional frame generation for JVM compatibility
- **Debug Frames** - Enhanced diagnostics for StackMapTable generation
- **Target Version Control** - Support for multiple Java classfile versions
- **Verbose Output** - Detailed compilation progress information
- **Error Location** - Precise source location reporting for compilation errors

---

## Security & Determinism

- No filesystem/network/clock access in compiled code
- Deterministic compilation process
- JVM bytecode verification compatible
- Standard Java security model

---

## Licensing

This project is licensed under Apache-2.0. See the LICENSE file for details.

---

### Example

**`Counter.tol`**
```java
package com.example;

public class Counter {
    private int value;
    
    public Counter() {
        this.value = 0;
    }
    
    public void increment() {
        value++;
    }
    
    public int getValue() {
        return value;
    }
    
    public static void main(String[] args) {
        Counter counter = new Counter();
        counter.increment();
        System.out.println("Value: " + counter.getValue());
    }
}
```

**Build**
```bash
mkdir -p build
tolc compile Counter.tol -o build/
```

**Run**
```bash
cd build
java com.example.Counter
```
