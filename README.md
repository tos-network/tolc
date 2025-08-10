# Terminos Language Compiler (tolc)

A fast, deterministic compiler for the **Terminos Language**—purpose-built for smart contracts on the **Terminos VM (TVM)** while remaining **EVM-ABI compatible** for tools, wallets, and event logs.

---

## Overview

`tolc` parses Terminos source, type-checks, runs safety and gas-aware optimizations, and emits deployable **constructor/runtime bytecode**, **ABI JSON**, **IR**, and metadata—much like `solc` does for Solidity.

- **Targets:** TVM (primary), optional experimental EVM output.
- **ABI compatibility:** Generates `abi.json` and error/event selectors compatible with common Ethereum tooling.
- **Determinism:** No syscalls or nondeterministic I/O; cost is gas-metered.
- **Tooling:** Standard JSON in/out, import remappings, source maps.

---

## Key Features

- **Standard JSON I/O** (`--standard-json`) for build systems.
- **Optimizer** with predictable passes (inlining, const folding, DCE, strength reduction).
- **IR pipeline** (`--ir`, `--ir-optimized`) for audits/formal analysis.
- **Source maps** and revert strings for debuggers.
- **Docs/metadata**: `devdoc`, `userdoc`, compiler/optimizer manifest.
- **SPDX license** line recognition (e.g., `// SPDX-License-Identifier: MIT`).
- **Version pragmas** (e.g., `pragma terminos ^0.3.0;`).

---

## Quick Start

### CLI

```bash
# Version and build info
tolc --version

# Compile to bytecode + ABI
tolc --bin --abi contracts/Counter.tol -o build/

# Enable optimizer (default runs=200)
tolc --bin --abi --optimize --optimize-runs 500 contracts/Counter.tol -o build/

# Emit IR for review
tolc --ir --ir-optimized contracts/Counter.tol -o build/

# Imports: base/include paths + remappings
tolc --base-path .      --include-path lib      --remap "@std=lib/std"      --bin --abi contracts/App.tol -o build/
```

### Standard JSON (recommended)

**Input example**
```json
{
  "language": "Terminos",
  "sources": {
    "Counter.tol": {
      "content": "// SPDX-License-Identifier: MIT\npragma terminos ^0.3.0;\ncontract Counter { uint256 private v; function inc() public { v += 1; } function get() public view returns(uint256){ return v; } }"
    }
  },
  "settings": {
    "optimizer": { "enabled": true, "runs": 500 },
    "outputSelection": {
      "*": {
        "*": [
          "abi",
          "evm.bytecode.object",
          "evm.deployedBytecode.object",
          "ir",
          "irOptimized",
          "metadata",
          "devdoc",
          "userdoc",
          "storageLayout",
          "sources",
          "sourcemap"
        ]
      }
    },
    "targets": ["TVM"]
  }
}
```

**CLI**
```bash
tolc --standard-json < in.json > out.json
```

---

## Inputs, Imports, Remappings

- **Base path:** `--base-path <dir>` sets the import root.
- **Include paths:** `--include-path <dir>` (repeatable) for library search.
- **Remappings:** `--remap "@alias=path/to/lib"` used by `import "@alias/…";`
- **CI hardening:** `--allow-paths <dir1>,<dir2>` restricts filesystem reads.

---

## Optimizer

Enable with `--optimize`; tune with `--optimize-runs <N>` (higher favors long-lived contracts). Passes are cost-aware and preserve observable behavior.

---

## Artifacts

- **`<Name>.bin`** – constructor/deployment bytecode  
- **`<Name>.bin-runtime`** – runtime bytecode  
- **`<Name>.abi`** – EVM-compatible ABI JSON  
- **`<Name>.meta.json`** – compiler/optimizer/source hashes  
- **`<Name>.ir` / `<Name>.ir.opt`** – IR (pre/post optimization)  
- **`storageLayout.json`** – slot/offset map for audits/upgrades

---

## Language Directives

- **Version:** `pragma terminos ^0.3.0;`
- **Experimental:** `pragma experimental ABIEncoderV2;` (example)
- **License:** `// SPDX-License-Identifier: <id or expression>`

---

## Advanced

- **Call graph / CFG:** `--emit-cfg`
- **Gas estimates:** `--gas-estimates`
- **Target EVM (experimental):** `--target EVM` (default: TVM)
- **Strict mode:** `--strict` (warnings → errors)
- **Reproducible builds:** `--deterministic` (locks timestamps/hashes in metadata)

---

## Security & Determinism

- No filesystem/network/clock access in compiled code.
- Deterministic arithmetic; explicit gas metering.
- Revert reasons and selectors follow EVM conventions for smooth tooling integration.

---

## Licensing

Put an SPDX line at the top of each source file. Common examples:

- `// SPDX-License-Identifier: MIT`
- `// SPDX-License-Identifier: (MIT OR Apache-2.0)`
- `// SPDX-License-Identifier: UNLICENSED`

---

## Versioning

`tolc` uses semantic versions: `MAJOR.MINOR.PATCH`. Pin compiler compatibility in contracts with `pragma terminos`.

---

### Example

**`contracts/Counter.tol`**
```
// SPDX-License-Identifier: MIT
pragma terminos ^0.3.0;

contract Counter {
    uint256 private value;

    event Increased(uint256 newValue);

    function inc() public {
        value = value + 1;
        emit Increased(value);
    }

    function get() public view returns (uint256) {
        return value;
    }
}
```

**Build**
```bash
mkdir -p build
tolc --bin --bin-runtime --abi --metadata --ir --ir-optimized contracts/Counter.tol -o build/
```
