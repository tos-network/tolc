#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Generate rt.rs from an rt.jar.

Usage:
- python3 gen_rt.py /path/to/rt.jar            # generate rt.rs
- python3 gen_rt.py /path/to/rt.jar -o rt.rs   # explicitly specify
- python3 gen_rt.py /path/to/rt.jar -o src/generated/rt.rs --prefix java/ --prefix javax/

Extracts:
- ClassMeta { internal, is_interface, super_internal, interfaces }
- MethodMeta { name, desc, flags, owner_internal }
- FieldMeta  { name, desc, flags, owner_internal }

Outputs Rust code with:
- static CLASSES: &[ClassMeta]
- static PHF maps for class/method/field lookup

Author: tolc helper
"""
import argparse
import io
import struct
import sys
import zipfile
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

def main():
    ap = argparse.ArgumentParser(description="Generate rt.rs from rt.jar")
    ap.add_argument("rt_jar", help="Path to rt.jar (or any JAR with classes)")
    ap.add_argument("-o", "--output", default="rt.rs", help="Output Rust file (default: rt.rs)")
    ap.add_argument("--prefix", action="append", default=[], help="Include only classes whose internal name starts with this prefix (repeatable)")
    args = ap.parse_args()

    classes: Dict[str, ParsedClass] = {}

    with zipfile.ZipFile(args.rt_jar, "r") as zf:
        for info in zf.infolist():
            if not info.filename.endswith(".class"):
                continue
            if info.filename.endswith("module-info.class"):
                continue
            try:
                data = zf.read(info.filename)
            except KeyError:
                continue
            try:
                parsed = parse_classfile(data)
            except Exception:
                continue

            name = parsed.this_internal
            if args.prefix:
                if not any(name.startswith(p) for p in args.prefix):
                    continue
            classes[name] = parsed

    if not classes:
        print("No classes parsed (check --prefix and jar content).", file=sys.stderr)
        sys.exit(1)

    rust = generate_rust(classes)
    with open(args.output, "w", encoding="utf-8") as f:
        f.write(rust)
    print(f"Wrote {args.output} with {len(classes)} classes.")

if __name__ == "__main__":
    main()
