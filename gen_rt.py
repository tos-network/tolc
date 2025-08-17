#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate rt.rs from an rt.jar with enhanced error reporting and completeness validation.

Usage:
- python3 gen_rt.py /path/to/rt.jar                    # basic generation
- python3 gen_rt.py /path/to/rt.jar --apply-patches    # with compatibility patches
- python3 gen_rt.py /path/to/rt.jar --validate         # with completeness validation
- python3 gen_rt.py /path/to/rt.jar --verbose --warn-on-parse-errors  # detailed error reporting
- python3 gen_rt.py /path/to/rt.jar -o src/rt.rs --prefix java/ --prefix javax/ --apply-patches --validate

Extracts:
- ClassMeta { internal, is_interface, super_internal, interfaces }
- MethodMeta { name, desc, flags, owner_internal }
- FieldMeta  { name, desc, flags, owner_internal }

Outputs Rust code with:
- static CLASSES: &[ClassMeta]
- static PHF maps for class/method/field lookup

Features:
- Comprehensive error reporting and statistics
- Compatibility patches for missing method overloads
- Completeness validation and quality checks
- Detailed parsing error analysis

Author: tolc helper
"""
import argparse
import io
import struct
import sys
import zipfile
import traceback
from typing import List, Dict, Tuple, Optional

# ----- Classfile constants -----
ACC_INTERFACE = 0x0200

# CP tags
CONSTANT_Utf8               = 1
CONSTANT_Integer            = 3
CONSTANT_Float              = 4
CONSTANT_Long               = 5
CONSTANT_Double             = 6
CONSTANT_Class              = 7
CONSTANT_String             = 8
CONSTANT_Fieldref           = 9
CONSTANT_Methodref          = 10
CONSTANT_InterfaceMethodref = 11
CONSTANT_NameAndType        = 12
CONSTANT_MethodHandle       = 15
CONSTANT_MethodType         = 16
CONSTANT_InvokeDynamic      = 18

def read_u1(b: io.BytesIO) -> int:
    d = b.read(1)
    if len(d) != 1: raise EOFError
    return d[0]

def read_u2(b: io.BytesIO) -> int:
    d = b.read(2)
    if len(d) != 2: raise EOFError
    return struct.unpack(">H", d)[0]

def read_u4(b: io.BytesIO) -> int:
    d = b.read(4)
    if len(d) != 4: raise EOFError
    return struct.unpack(">I", d)[0]

def read_bytes(b: io.BytesIO, n: int) -> bytes:
    d = b.read(n)
    if len(d) != n: raise EOFError
    return d

class CPEntry:
    __slots__ = ("tag","v")
    def __init__(self, tag:int, v):
        self.tag = tag
        self.v = v

class ParsedClass:
    def __init__(self):
        self.this_internal: str = ""
        self.super_internal: Optional[str] = None
        self.interfaces: List[str] = []
        self.access_flags: int = 0
        self.fields: List[Tuple[str,str,int]] = []   # (name, desc, flags)
        self.methods: List[Tuple[str,str,int]] = []  # (name, desc, flags)

def parse_classfile(data: bytes) -> ParsedClass:
    b = io.BytesIO(data)
    magic = read_u4(b)
    if magic != 0xCAFEBABE:
        raise ValueError("Bad magic")
    _minor = read_u2(b)
    _major = read_u2(b)
    cp_count = read_u2(b)

    # 1-based CP with possible double-slot entries
    cp: List[Optional[CPEntry]] = [None]*cp_count
    i = 1
    while i < cp_count:
        tag = read_u1(b)
        if tag == CONSTANT_Utf8:
            ln = read_u2(b); s = read_bytes(b, ln)
            cp[i] = CPEntry(tag, s.decode("utf-8", errors="replace"))
        elif tag in (CONSTANT_Integer, CONSTANT_Float):
            _ = read_u4(b); cp[i] = CPEntry(tag, _)
        elif tag in (CONSTANT_Long, CONSTANT_Double):
            _highlow = read_u4(b); _low = read_u4(b)
            cp[i] = CPEntry(tag, (_highlow << 32) | _low)
            i += 1
            if i < cp_count:
                cp[i] = None  # double-slot
        elif tag == CONSTANT_Class:
            name_idx = read_u2(b); cp[i] = CPEntry(tag, name_idx)
        elif tag == CONSTANT_String:
            si = read_u2(b); cp[i] = CPEntry(tag, si)
        elif tag in (CONSTANT_Fieldref, CONSTANT_Methodref, CONSTANT_InterfaceMethodref):
            cidx = read_u2(b); ntidx = read_u2(b)
            cp[i] = CPEntry(tag, (cidx, ntidx))
        elif tag == CONSTANT_NameAndType:
            nidx = read_u2(b); didx = read_u2(b)
            cp[i] = CPEntry(tag, (nidx, didx))
        elif tag == CONSTANT_MethodHandle:
            _kind = read_u1(b); ref = read_u2(b)
            cp[i] = CPEntry(tag, (_kind, ref))
        elif tag == CONSTANT_MethodType:
            didx = read_u2(b); cp[i] = CPEntry(tag, didx)
        elif tag == CONSTANT_InvokeDynamic:
            _bootstrap = read_u2(b); _nt = read_u2(b)
            cp[i] = CPEntry(tag, (_bootstrap, _nt))
        else:
            raise ValueError(f"Unsupported CP tag {tag}")
        i += 1

    def cp_utf8(idx:int) -> str:
        ent = cp[idx]
        if ent is None or ent.tag != CONSTANT_Utf8:
            raise ValueError("Bad UTF8 index")
        return ent.v

    def cp_class_name(idx:int) -> str:
        ent = cp[idx]
        if ent is None or ent.tag != CONSTANT_Class:
            raise ValueError("Bad Class index")
        name = cp_utf8(ent.v)
        return name  # already internal form with '/'

    access_flags = read_u2(b)
    this_idx     = read_u2(b)
    super_idx    = read_u2(b)

    cls = ParsedClass()
    cls.access_flags = access_flags
    cls.this_internal = cp_class_name(this_idx)
    if super_idx != 0:
        cls.super_internal = cp_class_name(super_idx)

    iface_count = read_u2(b)
    for _ in range(iface_count):
        ii = read_u2(b)
        cls.interfaces.append(cp_class_name(ii))

    # fields
    field_count = read_u2(b)
    for _ in range(field_count):
        f_acc = read_u2(b)
        f_name = cp_utf8(read_u2(b))
        f_desc = cp_utf8(read_u2(b))
        # skip attributes
        attr_cnt = read_u2(b)
        for _a in range(attr_cnt):
            _an = read_u2(b); _al = read_u4(b); _ = read_bytes(b, _al)
        cls.fields.append((f_name, f_desc, f_acc))

    # methods
    method_count = read_u2(b)
    for _ in range(method_count):
        m_acc = read_u2(b)
        m_name = cp_utf8(read_u2(b))
        m_desc = cp_utf8(read_u2(b))
        attr_cnt = read_u2(b)
        for _a in range(attr_cnt):
            _an = read_u2(b); _al = read_u4(b); _ = read_bytes(b, _al)
        cls.methods.append((m_name, m_desc, m_acc))

    # class attributes (skip)
    attr_cnt = read_u2(b)
    for _ in range(attr_cnt):
        _an = read_u2(b); _al = read_u4(b); _ = read_bytes(b, _al)

    return cls

def rust_escape(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"')

def to_const_ident(internal: str) -> str:
    """
    Convert an internal class name like 'java/base/Addendum$Inner-Class'
    to SCREAMING_SNAKE: 'JAVA_BASE_ADDENDUM_INNER_CLASS'.
    - Replace separators ['/', '.', '$', '-', '_'] with single '_'
    - Uppercase letters
    - Collapse multiple underscores
    - If first char is a digit, prefix 'C_'
    """
    s = internal.upper()
    out = []
    prev_us = False
    for ch in s:
        if ch.isalnum():
            out.append(ch)
            prev_us = False
        elif ch in "/.$-_" :
            if not prev_us:
                out.append("_")
                prev_us = True
        else:
            if not prev_us:
                out.append("_")
                prev_us = True
    ident = "".join(out).strip("_")
    if not ident:
        ident = "C"
    if ident[0].isdigit():
        ident = "C_" + ident
    return ident

def generate_rust(classes: Dict[str, ParsedClass]) -> str:
    class_names = sorted(classes.keys())
    class_index = {name: i for i, name in enumerate(class_names)}

    lines = []
    lines.append("// @generated by gen_rt.py")
    lines.append("#![allow(clippy::needless_borrow, clippy::too_many_arguments)]")
    lines.append("use phf::phf_map;")
    lines.append("")
    lines.append("#[derive(Debug)]")
    lines.append("pub struct MethodMeta { pub owner_internal: &'static str, pub name: &'static str, pub desc: &'static str, pub flags: u16 }")
    lines.append("#[derive(Debug)]")
    lines.append("pub struct FieldMeta  { pub owner_internal: &'static str, pub name: &'static str, pub desc: &'static str, pub flags: u16 }")
    lines.append("#[derive(Debug)]")
    lines.append("pub struct ClassMeta  { pub internal: &'static str, pub is_interface: bool, pub super_internal: Option<&'static str>, pub interfaces: &'static [&'static str], pub methods: &'static [MethodMeta], pub fields: &'static [FieldMeta] }")
    lines.append("")

    for cname in class_names:
        cls = classes[cname]
        var_prefix = to_const_ident(cname)
        lines.append(f"pub static METHODS_{var_prefix}: &[MethodMeta] = &[")
        for (mn, md, mf) in cls.methods:
            lines.append(f"    MethodMeta {{ owner_internal: \"{rust_escape(cname)}\", name: \"{rust_escape(mn)}\", desc: \"{rust_escape(md)}\", flags: {mf} }},")
        lines.append("];")
        lines.append(f"pub static FIELDS_{var_prefix}: &[FieldMeta] = &[")
        for (fn, fd, ff) in cls.fields:
            lines.append(f"    FieldMeta {{ owner_internal: \"{rust_escape(cname)}\", name: \"{rust_escape(fn)}\", desc: \"{rust_escape(fd)}\", flags: {ff} }},")
        lines.append("];")
        lines.append("")

    lines.append("pub static CLASSES: &[ClassMeta] = &[")
    for cname in class_names:
        cls = classes[cname]
        var_prefix = to_const_ident(cname)
        is_itf = "true" if (cls.access_flags & ACC_INTERFACE) != 0 else "false"
        super_str = f"Some(\"{rust_escape(cls.super_internal)}\")" if cls.super_internal else "None"
        if_list = ", ".join([f"\"{rust_escape(x)}\"" for x in cls.interfaces])
        lines.append("    ClassMeta {")
        lines.append(f"        internal: \"{rust_escape(cname)}\",")
        lines.append(f"        is_interface: {is_itf},")
        lines.append(f"        super_internal: {super_str},")
        lines.append(f"        interfaces: &[{if_list}],")
        lines.append(f"        methods: METHODS_{var_prefix},")
        lines.append(f"        fields:  FIELDS_{var_prefix},")
        lines.append("    },")
    lines.append("];")
    lines.append("")

    lines.append("pub static CLASSES_BY_NAME: phf::Map<&'static str, usize> = phf_map! {")
    for cname in class_names:
        idx = class_index[cname]
        lines.append(f"    \"{rust_escape(cname)}\" => {idx},")
    lines.append("};")
    lines.append("")

    lines.append("pub static METHODS_BY_KEY: phf::Map<&'static str, (usize, usize)> = phf_map! {")
    for cname in class_names:
        cls = classes[cname]
        cidx = class_index[cname]
        for midx, (mn, md, _mf) in enumerate(cls.methods):
            key = f"{cname}#{mn}:{md}"
            lines.append(f"    \"{rust_escape(key)}\" => ({cidx}, {midx}),")
    lines.append("};")
    lines.append("")

    lines.append("pub static FIELDS_BY_KEY: phf::Map<&'static str, (usize, usize)> = phf_map! {")
    for cname in class_names:
        cls = classes[cname]
        cidx = class_index[cname]
        for fidx, (fn, fd, _ff) in enumerate(cls.fields):
            key = f"{cname}#{fn}:{fd}"
            lines.append(f"    \"{rust_escape(key)}\" => ({cidx}, {fidx}),")
    lines.append("};")
    lines.append("")

    # Optional: FxHashMap build helper (runtime)
    lines.append("// If you prefer FxHashMap at runtime, enable `fxhash` and use the builder below:")
    lines.append("// use fxhash::FxHashMap;")
    lines.append("// pub fn build_fx_maps() -> (FxHashMap<&'static str, usize>, FxHashMap<&'static str, (usize,usize)>, FxHashMap<&'static str, (usize,usize)>) {")
    lines.append("//     let mut classes = FxHashMap::default();")
    for cname in class_names:
        idx = class_index[cname]
        lines.append(f"//     classes.insert(\"{rust_escape(cname)}\", {idx});")
    lines.append("//     let mut methods = FxHashMap::default();")
    for cname in class_names:
        cls = classes[cname]
        cidx = class_index[cname]
        for midx, (mn, md, _mf) in enumerate(cls.methods):
            key = f"{cname}#{mn}:{md}"
            lines.append(f"//     methods.insert(\"{rust_escape(key)}\", ({cidx}, {midx}));")
    lines.append("//     let mut fields = FxHashMap::default();")
    for cname in class_names:
        cls = classes[cname]
        cidx = class_index[cname]
        for fidx, (fn, fd, _ff) in enumerate(cls.fields):
            key = f"{cname}#{fn}:{fd}"
            lines.append(f"//     fields.insert(\"{rust_escape(key)}\", ({cidx}, {fidx}));")
    lines.append("//     (classes, methods, fields)")
    lines.append("// }")
    lines.append("")

    return "\n".join(lines)

def validate_completeness(classes: Dict[str, ParsedClass]) -> None:
    """
    Validate the completeness of parsed classes and report potential issues.
    """
    print(f"\n=== Completeness Validation ===", file=sys.stderr)
    
    # Check for common Java standard library classes that should be present
    expected_core_classes = [
        "java/lang/Object",
        "java/lang/String", 
        "java/lang/Class",
        "java/lang/System",
        "java/util/List",
        "java/util/Map",
        "java/io/InputStream",
        "java/io/OutputStream"
    ]
    
    missing_core_classes = [cls for cls in expected_core_classes if cls not in classes]
    if missing_core_classes:
        print(f"WARNING: Missing expected core classes: {missing_core_classes}", file=sys.stderr)
    else:
        print("✓ All expected core classes found", file=sys.stderr)
    
    # Check for method overloading patterns
    method_overload_stats = {}
    for class_name, cls in classes.items():
        method_names = {}
        for method_name, method_desc, method_flags in cls.methods:
            if method_name not in method_names:
                method_names[method_name] = []
            method_names[method_name].append(method_desc)
        
        overloaded_methods = {name: descs for name, descs in method_names.items() if len(descs) > 1}
        if overloaded_methods:
            method_overload_stats[class_name] = overloaded_methods
    
    if method_overload_stats:
        print(f"✓ Found method overloading in {len(method_overload_stats)} classes", file=sys.stderr)
        # Show some examples
        example_count = 0
        for class_name, overloads in method_overload_stats.items():
            if example_count >= 3:  # Limit examples
                break
            for method_name, descs in overloads.items():
                if example_count >= 3:
                    break
                print(f"  Example: {class_name}#{method_name} has {len(descs)} overloads", file=sys.stderr)
                example_count += 1
    else:
        print("WARNING: No method overloading detected (might indicate incomplete parsing)", file=sys.stderr)
    
    # Check for inheritance patterns
    inheritance_count = sum(1 for cls in classes.values() if cls.super_internal is not None)
    interface_impl_count = sum(1 for cls in classes.values() if cls.interfaces)
    
    print(f"✓ Classes with inheritance: {inheritance_count}/{len(classes)}", file=sys.stderr)
    print(f"✓ Classes implementing interfaces: {interface_impl_count}/{len(classes)}", file=sys.stderr)
    
    # Check for suspicious patterns
    classes_with_no_constructors = []
    for class_name, cls in classes.items():
        has_constructor = any(method_name == "<init>" for method_name, _, _ in cls.methods)
        if not has_constructor and not (cls.access_flags & ACC_INTERFACE):
            classes_with_no_constructors.append(class_name)
    
    if classes_with_no_constructors:
        print(f"WARNING: {len(classes_with_no_constructors)} non-interface classes have no constructors:", file=sys.stderr)
        for name in classes_with_no_constructors[:5]:  # Show first 5
            print(f"  {name}", file=sys.stderr)
        if len(classes_with_no_constructors) > 5:
            print(f"  ... and {len(classes_with_no_constructors) - 5} more", file=sys.stderr)
    else:
        print("✓ All non-interface classes have constructors", file=sys.stderr)
    
    print(f"=== End Completeness Validation ===", file=sys.stderr)

def check_method_overload_completeness(classes: Dict[str, ParsedClass]) -> None:
    """
    Check for potentially missing method overloads based on common Java patterns.
    """
    print(f"\n=== Method Overload Completeness Check ===", file=sys.stderr)
    
    # Common methods that often have multiple overloads
    common_overloaded_methods = {
        "java/lang/String": ["valueOf", "substring", "indexOf", "lastIndexOf"],
        "java/lang/Class": ["getMethod", "getDeclaredMethod", "getConstructor", "getDeclaredConstructor"],
        "java/lang/reflect/Method": ["invoke"],
        "java/lang/reflect/Constructor": ["newInstance"],
        "java/io/PrintStream": ["print", "println"],
        "java/util/List": ["get", "set", "add", "remove"],
        "java/util/Map": ["get", "put", "remove"],
    }
    
    missing_overloads = []
    for class_name, expected_methods in common_overloaded_methods.items():
        if class_name in classes:
            cls = classes[class_name]
            method_names = {method_name for method_name, _, _ in cls.methods}
            
            for expected_method in expected_methods:
                if expected_method in method_names:
                    # Count overloads
                    overload_count = sum(1 for method_name, _, _ in cls.methods if method_name == expected_method)
                    if overload_count == 1:
                        missing_overloads.append(f"{class_name}#{expected_method} (only 1 overload found, expected multiple)")
                else:
                    missing_overloads.append(f"{class_name}#{expected_method} (method not found)")
    
    if missing_overloads:
        print(f"WARNING: Potentially missing method overloads ({len(missing_overloads)} cases):", file=sys.stderr)
        for missing in missing_overloads[:10]:  # Show first 10
            print(f"  {missing}", file=sys.stderr)
        if len(missing_overloads) > 10:
            print(f"  ... and {len(missing_overloads) - 10} more", file=sys.stderr)
    else:
        print("✓ All expected method overloads found", file=sys.stderr)
    
    print(f"=== End Method Overload Completeness Check ===", file=sys.stderr)

def apply_compatibility_patches(classes: Dict[str, ParsedClass]) -> None:
    """
    Apply compatibility patches to add missing method overloads that are needed
    by the compiler but may not be present in the original JAR.
    """
    print(f"\n=== Applying Compatibility Patches ===", file=sys.stderr)
    
    patches_applied = 0
    
    # Patch 1: Add simplified method overloads for java/lang/Class
    if "java/lang/Class" in classes:
        cls = classes["java/lang/Class"]
        existing_methods = {(name, desc) for name, desc, _ in cls.methods}
        
        # Add single-parameter getMethod if only multi-parameter exists
        if ("getMethod", "(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;") in existing_methods:
            if ("getMethod", "(Ljava/lang/String;)Ljava/lang/reflect/Method;") not in existing_methods:
                cls.methods.append(("getMethod", "(Ljava/lang/String;)Ljava/lang/reflect/Method;", 1))
                patches_applied += 1
                print("  + Added java/lang/Class#getMethod(String)", file=sys.stderr)
        
        # Add no-parameter getConstructor if only multi-parameter exists
        if ("getConstructor", "([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;") in existing_methods:
            if ("getConstructor", "()Ljava/lang/reflect/Constructor;") not in existing_methods:
                cls.methods.append(("getConstructor", "()Ljava/lang/reflect/Constructor;", 1))
                patches_applied += 1
                print("  + Added java/lang/Class#getConstructor()", file=sys.stderr)
    
    # Patch 2: Add simplified method overloads for java/lang/reflect/Constructor
    if "java/lang/reflect/Constructor" in classes:
        cls = classes["java/lang/reflect/Constructor"]
        existing_methods = {(name, desc) for name, desc, _ in cls.methods}
        
        # Add no-parameter newInstance if only multi-parameter exists
        if ("newInstance", "([Ljava/lang/Object;)Ljava/lang/Object;") in existing_methods:
            if ("newInstance", "()Ljava/lang/Object;") not in existing_methods:
                cls.methods.append(("newInstance", "()Ljava/lang/Object;", 1))
                patches_applied += 1
                print("  + Added java/lang/reflect/Constructor#newInstance()", file=sys.stderr)
    
    # Patch 3: Add simplified method overloads for java/lang/reflect/Method
    if "java/lang/reflect/Method" in classes:
        cls = classes["java/lang/reflect/Method"]
        existing_methods = {(name, desc) for name, desc, _ in cls.methods}
        
        # Add single-parameter invoke if only multi-parameter exists
        if ("invoke", "(Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;") in existing_methods:
            if ("invoke", "(Ljava/lang/Object;)Ljava/lang/Object;") not in existing_methods:
                cls.methods.append(("invoke", "(Ljava/lang/Object;)Ljava/lang/Object;", 1))
                patches_applied += 1
                print("  + Added java/lang/reflect/Method#invoke(Object)", file=sys.stderr)
    
    if patches_applied == 0:
        print("  No patches needed - all required methods already present", file=sys.stderr)
    else:
        print(f"  Applied {patches_applied} compatibility patches", file=sys.stderr)
    
    print(f"=== End Compatibility Patches ===", file=sys.stderr)

def main():
    ap = argparse.ArgumentParser(description="Generate rt.rs from rt.jar")
    ap.add_argument("rt_jar", help="Path to rt.jar (or any JAR with classes)")
    ap.add_argument("-o", "--output", default="rt.rs", help="Output Rust file (default: rt.rs)")
    ap.add_argument("--prefix", action="append", default=[], help="Include only classes whose internal name starts with this prefix (repeatable)")
    ap.add_argument("--verbose", "-v", action="store_true", help="Enable verbose logging of parsing errors")
    ap.add_argument("--warn-on-parse-errors", action="store_true", help="Print warnings for class files that fail to parse")
    ap.add_argument("--validate", action="store_true", help="Run completeness validation checks")
    ap.add_argument("--apply-patches", action="store_true", help="Apply compatibility patches for missing method overloads")
    args = ap.parse_args()

    classes: Dict[str, ParsedClass] = {}
    
    # Statistics tracking
    total_class_files = 0
    skipped_non_class = 0
    skipped_module_info = 0
    skipped_read_error = 0
    skipped_parse_error = 0
    skipped_prefix_filter = 0
    parsed_successfully = 0
    
    # Error tracking
    parse_errors: List[Tuple[str, str]] = []  # (filename, error_message)

    with zipfile.ZipFile(args.rt_jar, "r") as zf:
        for info in zf.infolist():
            if not info.filename.endswith(".class"):
                skipped_non_class += 1
                continue
            if info.filename.endswith("module-info.class"):
                skipped_module_info += 1
                continue
            
            total_class_files += 1
            
            try:
                data = zf.read(info.filename)
            except KeyError as e:
                skipped_read_error += 1
                if args.verbose:
                    print(f"WARNING: Failed to read {info.filename}: {e}", file=sys.stderr)
                continue
            except Exception as e:
                skipped_read_error += 1
                if args.verbose:
                    print(f"WARNING: Failed to read {info.filename}: {e}", file=sys.stderr)
                continue
                
            try:
                parsed = parse_classfile(data)
            except Exception as e:
                skipped_parse_error += 1
                error_msg = str(e)
                if args.verbose:
                    error_msg = f"{e}\n{traceback.format_exc()}"
                parse_errors.append((info.filename, error_msg))
                if args.warn_on_parse_errors or args.verbose:
                    print(f"WARNING: Failed to parse {info.filename}: {error_msg}", file=sys.stderr)
                continue

            name = parsed.this_internal
            if args.prefix:
                if not any(name.startswith(p) for p in args.prefix):
                    skipped_prefix_filter += 1
                    continue
            
            classes[name] = parsed
            parsed_successfully += 1

    # Print comprehensive statistics
    print(f"=== JAR Processing Statistics ===", file=sys.stderr)
    print(f"Total entries in JAR: {len(zf.infolist())}", file=sys.stderr)
    print(f"Non-class files skipped: {skipped_non_class}", file=sys.stderr)
    print(f"Module-info files skipped: {skipped_module_info}", file=sys.stderr)
    print(f"Total .class files found: {total_class_files}", file=sys.stderr)
    print(f"Read errors: {skipped_read_error}", file=sys.stderr)
    print(f"Parse errors: {skipped_parse_error}", file=sys.stderr)
    print(f"Filtered out by prefix: {skipped_prefix_filter}", file=sys.stderr)
    print(f"Successfully parsed: {parsed_successfully}", file=sys.stderr)
    print(f"=== End Statistics ===", file=sys.stderr)
    
    # Report parse errors summary
    if parse_errors:
        print(f"\n=== Parse Error Summary ===", file=sys.stderr)
        print(f"Total parse errors: {len(parse_errors)}", file=sys.stderr)
        if not args.verbose and not args.warn_on_parse_errors:
            print("Use --verbose or --warn-on-parse-errors to see detailed error messages", file=sys.stderr)
        
        # Group errors by type for better analysis
        error_types: Dict[str, int] = {}
        for filename, error_msg in parse_errors:
            # Extract the first line of error message as error type
            error_type = error_msg.split('\n')[0].split(':')[-1].strip()
            error_types[error_type] = error_types.get(error_type, 0) + 1
        
        print("Error types:", file=sys.stderr)
        for error_type, count in sorted(error_types.items(), key=lambda x: x[1], reverse=True):
            print(f"  {error_type}: {count} files", file=sys.stderr)
        print(f"=== End Parse Error Summary ===", file=sys.stderr)

    if not classes:
        print("ERROR: No classes parsed successfully (check --prefix and jar content).", file=sys.stderr)
        sys.exit(1)

    # Generate method and field statistics
    total_methods = sum(len(cls.methods) for cls in classes.values())
    total_fields = sum(len(cls.fields) for cls in classes.values())
    
    print(f"\n=== Generated Content Statistics ===", file=sys.stderr)
    print(f"Classes: {len(classes)}", file=sys.stderr)
    print(f"Methods: {total_methods}", file=sys.stderr)
    print(f"Fields: {total_fields}", file=sys.stderr)
    
    # Check for potential completeness issues
    classes_without_methods = [name for name, cls in classes.items() if not cls.methods and not (cls.access_flags & ACC_INTERFACE)]
    if classes_without_methods:
        print(f"WARNING: {len(classes_without_methods)} non-interface classes have no methods (might indicate parsing issues):", file=sys.stderr)
        for name in classes_without_methods[:10]:  # Show first 10
            print(f"  {name}", file=sys.stderr)
        if len(classes_without_methods) > 10:
            print(f"  ... and {len(classes_without_methods) - 10} more", file=sys.stderr)
    
    print(f"=== End Content Statistics ===", file=sys.stderr)

    # Apply compatibility patches if requested
    if args.apply_patches:
        apply_compatibility_patches(classes)

    # Run completeness validation if requested
    if args.validate:
        validate_completeness(classes)
        check_method_overload_completeness(classes)

    rust = generate_rust(classes)
    with open(args.output, "w", encoding="utf-8") as f:
        f.write(rust)
    print(f"Successfully wrote {args.output} with {len(classes)} classes, {total_methods} methods, and {total_fields} fields.")

if __name__ == "__main__":
    main()
