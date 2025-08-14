//! StackMapTable frames and (future) frame computation utilities

use super::attribute::AttributeInfo;
use super::constpool::ConstantPool;
use super::opcodes;

/// VerificationTypeInfo as defined in JVMS 4.7.4
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationType {
    Top,
    Integer,
    Float,
    Double,
    Long,
    Null,
    UninitializedThis,
    Object(u16),        // cpool index to CONSTANT_Class
    Uninitialized(u16), // offset
}

impl VerificationType {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        match self {
            VerificationType::Top => bytes.push(0),
            VerificationType::Integer => bytes.push(1),
            VerificationType::Float => bytes.push(2),
            VerificationType::Double => bytes.push(3),
            VerificationType::Long => bytes.push(4),
            VerificationType::Null => bytes.push(5),
            VerificationType::UninitializedThis => bytes.push(6),
            VerificationType::Object(cp_index) => {
                bytes.push(7);
                bytes.extend_from_slice(&cp_index.to_be_bytes());
            }
            VerificationType::Uninitialized(offset) => {
                bytes.push(8);
                bytes.extend_from_slice(&offset.to_be_bytes());
            }
        }
        bytes
    }
}

/// StackMapFrame variants as defined in JVMS 4.7.4
#[derive(Debug, Clone)]
pub enum StackMapFrame {
    Same { offset_delta: u16 },
    SameLocals1StackItem { offset_delta: u16, stack: VerificationType },
    SameLocals1StackItemExtended { offset_delta: u16, stack: VerificationType },
    Chop { k: u8, offset_delta: u16 },          // k in {1,2,3}
    SameExtended { offset_delta: u16 },
    Append { k: u8, offset_delta: u16, locals: Vec<VerificationType> }, // k in {1,2,3}
    Full { offset_delta: u16, locals: Vec<VerificationType>, stack: Vec<VerificationType> },
}

impl StackMapFrame {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        match self {
            StackMapFrame::Same { offset_delta } => {
                if *offset_delta <= 63 {
                    bytes.push(*offset_delta as u8);
                } else {
                    bytes.push(251); // same_frame_extended
                    bytes.extend_from_slice(&offset_delta.to_be_bytes());
                }
            }
            StackMapFrame::SameLocals1StackItem { offset_delta, stack } => {
                if *offset_delta <= 63 {
                    bytes.push(64 + *offset_delta as u8);
                    bytes.extend_from_slice(&stack.to_bytes());
                } else {
                    bytes.push(247); // same_locals_1_stack_item_frame_extended
                    bytes.extend_from_slice(&offset_delta.to_be_bytes());
                    bytes.extend_from_slice(&stack.to_bytes());
                }
            }
            StackMapFrame::SameLocals1StackItemExtended { offset_delta, stack } => {
                bytes.push(247);
                bytes.extend_from_slice(&offset_delta.to_be_bytes());
                bytes.extend_from_slice(&stack.to_bytes());
            }
            StackMapFrame::Chop { k, offset_delta } => {
                // 248 + k where k in {1,2,3}
                let tag = 248u8 + (*k as u8);
                bytes.push(tag);
                bytes.extend_from_slice(&offset_delta.to_be_bytes());
            }
            StackMapFrame::SameExtended { offset_delta } => {
                bytes.push(251);
                bytes.extend_from_slice(&offset_delta.to_be_bytes());
            }
            StackMapFrame::Append { k, offset_delta, locals } => {
                // 251 + k where k in {1,2,3}
                let tag = 251u8 + (*k as u8);
                bytes.push(tag);
                bytes.extend_from_slice(&offset_delta.to_be_bytes());
                for l in locals {
                    bytes.extend_from_slice(&l.to_bytes());
                }
            }
            StackMapFrame::Full { offset_delta, locals, stack } => {
                bytes.push(255);
                bytes.extend_from_slice(&offset_delta.to_be_bytes());
                bytes.extend_from_slice(&(locals.len() as u16).to_be_bytes());
                for l in locals {
                    bytes.extend_from_slice(&l.to_bytes());
                }
                bytes.extend_from_slice(&(stack.len() as u16).to_be_bytes());
                for s in stack {
                    bytes.extend_from_slice(&s.to_bytes());
                }
            }
        }
        bytes
    }
}

#[derive(Debug, Default, Clone)]
pub struct StackMapTable {
    pub frames: Vec<StackMapFrame>,
}

impl StackMapTable {
    pub fn new() -> Self { Self { frames: Vec::new() } }
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&(self.frames.len() as u16).to_be_bytes());
        for f in &self.frames {
            bytes.extend_from_slice(&f.to_bytes());
        }
        bytes
    }
}

/// Minimal verification type lattice and merge utilities
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FrameState {
    pub locals: Vec<VerificationType>,
    pub stack: Vec<VerificationType>,
    // Map NEW-instruction bytecode offset -> Class cp index for precise owner typing
    pub new_types: std::collections::HashMap<u16, u16>,
}

impl FrameState {
    pub fn new(locals: Vec<VerificationType>, stack: Vec<VerificationType>) -> Self {
        Self { locals, stack, new_types: std::collections::HashMap::new() }
    }

    /// Merge two frame states element-wise using the minimal lattice.
    pub fn merge_with(&self, other: &FrameState, cp: &mut ConstantPool) -> FrameState {
        let merged_locals = merge_vec_types(&self.locals, &other.locals, cp);
        let merged_stack = merge_vec_types(&self.stack, &other.stack, cp);
        // Merge new_types maps: prefer existing entries; add missing from other
        let mut merged_map = self.new_types.clone();
        for (k, v) in &other.new_types { merged_map.entry(*k).or_insert(*v); }
        FrameState { locals: merged_locals, stack: merged_stack, new_types: merged_map }
    }
}

fn merge_vec_types(a: &Vec<VerificationType>, b: &Vec<VerificationType>, cp: &mut ConstantPool) -> Vec<VerificationType> {
    let max_len = a.len().max(b.len());
    let mut out: Vec<VerificationType> = Vec::with_capacity(max_len);
    for i in 0..max_len {
        let ta = a.get(i).cloned().unwrap_or(VerificationType::Top);
        let tb = b.get(i).cloned().unwrap_or(VerificationType::Top);
        out.push(merge_type(&ta, &tb, cp));
    }
    out
}

/// Merge two verification types to their least-upper-bound in a minimal lattice.
/// Rules:
/// - Equal => itself
/// - Primitive categories only merge with identical kind; otherwise Top
/// - Null with Object(C) => Object(C)
/// - Object(C1) with Object(C2) => Object(commonSuper(C1,C2)); minimal common super is java/lang/Object
/// - Any with Top => Top
/// - Uninitialized* only merge with identical; otherwise Top
pub fn merge_type(a: &VerificationType, b: &VerificationType, cp: &mut ConstantPool) -> VerificationType {
    use VerificationType::*;
    if a == b { return a.clone(); }
    match (a, b) {
        (Top, _) | (_, Top) => Top,
        (Integer, Integer) => Integer,
        (Float, Float) => Float,
        (Double, Double) => Double,
        (Long, Long) => Long,
        (Null, Object(c)) | (Object(c), Null) => Object(*c),
        (Object(c1), Object(c2)) => {
            if c1 == c2 { Object(*c1) } else {
                // Minimal common super: java/lang/Object
                let obj = cp.add_class("java/lang/Object");
                Object(obj)
            }
        }
        (UninitializedThis, UninitializedThis) => UninitializedThis,
        (Uninitialized(o1), Uninitialized(o2)) if o1 == o2 => Uninitialized(*o1),
        // Fallback for different primitives or any other combination
        _ => Top,
    }
}

/// Produce human-readable descriptions of frames with absolute bytecode offsets
pub fn describe_stack_map_frames(table: &StackMapTable) -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();
    let mut pc_accum: u32 = 0; // previous_frame_pc
    for frame in &table.frames {
        match frame {
            StackMapFrame::Same { offset_delta } => {
                pc_accum = pc_accum.saturating_add((*offset_delta as u32) + 1);
                lines.push(format!("@{:>4} SAME (delta={})", pc_accum, offset_delta));
            }
            StackMapFrame::SameLocals1StackItem { offset_delta, stack } => {
                pc_accum = pc_accum.saturating_add((*offset_delta as u32) + 1);
                lines.push(format!("@{:>4} SAME_LOCALS_1 (delta={}, stack={:?})", pc_accum, offset_delta, stack));
            }
            StackMapFrame::SameLocals1StackItemExtended { offset_delta, stack } => {
                pc_accum = pc_accum.saturating_add((*offset_delta as u32) + 1);
                lines.push(format!("@{:>4} SAME_LOCALS_1_EXT (delta={}, stack={:?})", pc_accum, offset_delta, stack));
            }
            StackMapFrame::Chop { k, offset_delta } => {
                pc_accum = pc_accum.saturating_add((*offset_delta as u32) + 1);
                lines.push(format!("@{:>4} CHOP{} (delta={})", pc_accum, k, offset_delta));
            }
            StackMapFrame::SameExtended { offset_delta } => {
                pc_accum = pc_accum.saturating_add((*offset_delta as u32) + 1);
                lines.push(format!("@{:>4} SAME_EXT (delta={})", pc_accum, offset_delta));
            }
            StackMapFrame::Append { k, offset_delta, locals } => {
                pc_accum = pc_accum.saturating_add((*offset_delta as u32) + 1);
                lines.push(format!("@{:>4} APPEND{} (delta={}, locals={:?})", pc_accum, k, offset_delta, locals));
            }
            StackMapFrame::Full { offset_delta, locals, stack } => {
                pc_accum = pc_accum.saturating_add((*offset_delta as u32) + 1);
                lines.push(format!("@{:>4} FULL (delta={}, locals={:?}, stack={:?})", pc_accum, offset_delta, locals, stack));
            }
        }
    }
    lines
}

/// Helper to build an AttributeInfo for StackMapTable
pub fn make_stack_map_attribute(constant_pool: &mut ConstantPool, table: &StackMapTable) -> AttributeInfo {
    let _ = constant_pool.try_add_utf8("StackMapTable");
    let info = table.to_bytes();
    AttributeInfo::new(0, info)
}

/// Placeholder for future frame computation from bytecode
#[derive(Debug, Default)]
pub struct FrameBuilder;

impl FrameBuilder {
    pub fn new() -> Self { Self }

    /// Compute a minimal StackMapTable by basic block partition using branch targets and fall-throughs.
    /// This creates Same/SameExtended frames at basic block starts (excluding 0), without locals/stack types.
    pub fn compute_from(&self, code: &[u8], exception_handler_starts: &[u16]) -> StackMapTable {
        let leaders = self.collect_basic_block_leaders(code, exception_handler_starts);
        let mut frames: Vec<StackMapFrame> = Vec::new();
        let mut prev_offset: i32 = -1;
        for &leader in leaders.iter().filter(|&&o| o != 0) {
            let offset = leader as i32;
            let delta = (offset - prev_offset - 1) as u16;
            // Prefer same_frame when delta<=63, else same_frame_extended
            if delta <= 63 {
                frames.push(StackMapFrame::Same { offset_delta: delta });
            } else {
                frames.push(StackMapFrame::SameExtended { offset_delta: delta });
            }
            prev_offset = offset;
        }
        StackMapTable { frames }
    }

    /// Compute StackMapTable with frames at basic block leaders using a minimal dataflow:
    /// - Seed entry frame from `this`/args
    /// - Forward-simulate basic blocks with a coarse stack/locals effect model
    /// - Merge at join points using the minimal verification-type lattice
    /// - Emit compressed frames (Same/SameExtended/SameLocals1*/Append/Chop/Full) relative to the previous
    pub fn compute_with_types(
        &self,
        code: &[u8],
        exception_handler_starts: &[u16],
        is_static: bool,
        is_constructor: bool,
        current_class_internal_name: &str,
        method_descriptor: &str,
        constant_pool: &mut ConstantPool,
    ) -> StackMapTable {
        self.compute_with_types_with_handlers(
            code,
            &exception_handler_starts.iter().map(|&pc| (pc, None)).collect::<Vec<_>>(),
            is_static,
            is_constructor,
            current_class_internal_name,
            method_descriptor,
            constant_pool,
        )
    }

    /// Variant that accepts handler start PCs with optional catch type internal names.
    pub fn compute_with_types_with_handlers(
        &self,
        code: &[u8],
        exception_handlers: &[(u16, Option<String>)],
        is_static: bool,
        is_constructor: bool,
        current_class_internal_name: &str,
        method_descriptor: &str,
        constant_pool: &mut ConstantPool,
    ) -> StackMapTable {
        use std::collections::{BTreeSet, VecDeque, HashMap};
        let handler_starts: Vec<u16> = exception_handlers.iter().map(|(pc, _)| *pc).collect();
        let leaders: Vec<u16> = self.collect_basic_block_leaders(code, &handler_starts);
        let handler_set: BTreeSet<u16> = handler_starts.iter().copied().collect();
        let mut handler_type_map: HashMap<u16, VerificationType> = HashMap::new();
        for (pc, ty_opt) in exception_handlers {
            let vt = if let Some(internal) = ty_opt {
                let idx = constant_pool.add_class(internal);
                VerificationType::Object(idx)
            } else {
                let thr = constant_pool.add_class("java/lang/Throwable");
                VerificationType::Object(thr)
            };
            handler_type_map.insert(*pc, vt);
        }
        let mut leader_index: HashMap<u16, usize> = HashMap::new();
        for (idx, off) in leaders.iter().enumerate() { leader_index.insert(*off, idx); }

        // Seed entry frame state
        let entry_locals = seed_entry_locals(
            is_static,
            is_constructor,
            current_class_internal_name,
            method_descriptor,
            constant_pool,
        );
        let mut in_states: Vec<Option<FrameState>> = vec![None; leaders.len()];
        if let Some(&zero_idx) = leader_index.get(&0) { in_states[zero_idx] = Some(FrameState::new(entry_locals.clone(), vec![])); }

        // Worklist over leader PCs
        let mut worklist: VecDeque<u16> = VecDeque::new();
        worklist.push_back(0);

        // Forward simulate per basic block
        while let Some(leader_pc) = worklist.pop_front() {
            let Some(&li) = leader_index.get(&leader_pc) else { continue; };
            let Some(mut state) = in_states[li].clone() else { continue; };
            // Determine block end
            let pos = leaders.iter().position(|&x| x == leader_pc).unwrap_or(0);
            let block_end: usize = if pos + 1 < leaders.len() { leaders[pos + 1] as usize } else { code.len() };
            let mut pc: usize = leader_pc as usize;
            while pc < block_end {
                let op = code[pc];
                let len = instruction_length(op, code, pc);
                // Compute successors for control-flow affecting ops
                let mut jumped: bool = false;
                match op {
                    opcodes::GOTO => {
                        if pc + 3 <= code.len() {
                            let off = i16::from_be_bytes([code[pc + 1], code[pc + 2]]) as i32;
                            let tgt = (pc as i32 + off) as i32;
                            if tgt >= 0 { merge_into_leader(tgt as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); }
                        }
                        jumped = true;
                    }
                    opcodes::GOTO_W => {
                        if pc + 5 <= code.len() {
                            let off = read_i32(code, pc + 1);
                            let tgt = (pc as i32 + off) as i32;
                            if tgt >= 0 { merge_into_leader(tgt as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); }
                        }
                        jumped = true;
                    }
                    0xaa /*tableswitch*/ => {
                        let pad = (4 - ((pc + 1) % 4)) % 4;
                        let mut idx = pc + 1 + pad;
                        if idx + 12 <= code.len() {
                            let default_off = read_i32(code, idx); idx += 4;
                            let low = read_i32(code, idx); idx += 4;
                            let high = read_i32(code, idx); idx += 4;
                            let count = if high >= low { (high - low + 1) as usize } else { 0 };
                            let def_tgt = (pc as i32 + default_off) as i32;
                            if def_tgt >= 0 { merge_into_leader(def_tgt as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); }
                            for _ in 0..count { if idx + 4 > code.len() { break; } let off = read_i32(code, idx); idx += 4; let tgt = (pc as i32 + off) as i32; if tgt >= 0 { merge_into_leader(tgt as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); } }
                        }
                        jumped = true;
                    }
                    0xab /*lookupswitch*/ => {
                        let pad = (4 - ((pc + 1) % 4)) % 4; let mut idx = pc + 1 + pad;
                        if idx + 8 <= code.len() {
                            let default_off = read_i32(code, idx); idx += 4; let npairs = read_i32(code, idx) as usize; idx += 4;
                            let def_tgt = (pc as i32 + default_off) as i32; if def_tgt >= 0 { merge_into_leader(def_tgt as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); }
                            for _ in 0..npairs { if idx + 8 > code.len() { break; } idx += 4; let off = read_i32(code, idx); idx += 4; let tgt = (pc as i32 + off) as i32; if tgt >= 0 { merge_into_leader(tgt as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); } }
                        }
                        jumped = true;
                    }
                    opcodes::IFEQ | opcodes::IFNE | opcodes::IFLT | opcodes::IFGE | opcodes::IFGT | opcodes::IFLE
                    | opcodes::IF_ICMPEQ | opcodes::IF_ICMPNE | opcodes::IF_ICMPLT | opcodes::IF_ICMPGE | opcodes::IF_ICMPGT | opcodes::IF_ICMPLE
                    | opcodes::IF_ACMPEQ | opcodes::IF_ACMPNE | opcodes::IFNULL | opcodes::IFNONNULL => {
                        if pc + 3 <= code.len() {
                            let off = i16::from_be_bytes([code[pc + 1], code[pc + 2]]) as i32;
                            let tgt = (pc as i32 + off) as i32;
                            if tgt >= 0 { merge_into_leader(tgt as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); }
                            let next = pc + len; if next < code.len() { merge_into_leader(next as u16, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); }
                        }
                        jumped = true;
                    }
                    opcodes::IRETURN | opcodes::LRETURN | opcodes::FRETURN | opcodes::DRETURN | opcodes::ARETURN | opcodes::RETURN | opcodes::ATHROW => {
                        jumped = true; // block terminator
                    }
                    _ => {}
                }

                // Simulate coarse stack/local effects (best-effort)
                simulate_effect(op, code, pc, &mut state, constant_pool);

                if jumped { break; }
                pc += len;
            }
            // Fall-through to next leader
            if !leaders.is_empty() {
                let next_leader = if pos + 1 < leaders.len() { leaders[pos + 1] } else { u16::MAX };
                if next_leader != u16::MAX { merge_into_leader(next_leader, &state, &leader_index, &mut in_states, &mut worklist, constant_pool); }
            }
        }

        // Emit frames for reachable leaders (excluding 0) using compression relative to previous
        let mut frames: Vec<StackMapFrame> = Vec::new();
        let mut prev_offset: i32 = -1;
        let mut prev_locals: Vec<VerificationType> = entry_locals;
        let mut prev_stack: Vec<VerificationType> = Vec::new();
        for &leader in leaders.iter().filter(|&&o| o != 0) {
            if let Some(&idx) = leader_index.get(&leader) {
                if let Some(mut st) = in_states[idx].clone() {
                    // Handler stack override
                    if handler_set.contains(&leader) {
                        if let Some(vt) = handler_type_map.get(&leader).cloned() {
                            st.stack = vec![vt];
                        }
                    }
                    let offset = leader as i32;
                    let delta = (offset - prev_offset - 1) as u16;
                    let frame = compress_frame(delta, &prev_locals, &prev_stack, &st.locals, &st.stack);
                    prev_offset = offset;
                    prev_locals = st.locals;
                    prev_stack = st.stack;
                    frames.push(frame);
                }
            }
        }
        StackMapTable { frames }
    }
    fn collect_basic_block_leaders(&self, code: &[u8], exception_handler_starts: &[u16]) -> Vec<u16> {
        use std::collections::BTreeSet;
        let mut leaders: BTreeSet<u16> = BTreeSet::new();
        leaders.insert(0);

        // Exception handler start PCs are leaders
        for &h in exception_handler_starts { leaders.insert(h); }

        let mut pc: usize = 0;
        while pc < code.len() {
            let op = code[pc];
            let len = instruction_length(op, code, pc);
            match op {
                opcodes::GOTO => {
                    if pc + 3 <= code.len() {
                        let off = i16::from_be_bytes([code[pc + 1], code[pc + 2]]) as i32;
                        let target = (pc as i32 + off) as i32;
                        if target >= 0 { leaders.insert(target as u16); }
                    }
                }
                opcodes::GOTO_W => {
                    if pc + 5 <= code.len() {
                        let off = read_i32(code, pc + 1);
                        let target = (pc as i32 + off) as i32;
                        if target >= 0 { leaders.insert(target as u16); }
                    }
                }
                0xa8 /*JSR*/ => {
                    if pc + 3 <= code.len() {
                        let off = i16::from_be_bytes([code[pc + 1], code[pc + 2]]) as i32;
                        let target = (pc as i32 + off) as i32;
                        if target >= 0 { leaders.insert(target as u16); }
                    }
                }
                0xc9 /*JSR_W*/ => {
                    if pc + 5 <= code.len() {
                        let off = read_i32(code, pc + 1);
                        let target = (pc as i32 + off) as i32;
                        if target >= 0 { leaders.insert(target as u16); }
                    }
                }
                opcodes::IFEQ | opcodes::IFNE | opcodes::IFLT | opcodes::IFGE | opcodes::IFGT | opcodes::IFLE
                | opcodes::IF_ICMPEQ | opcodes::IF_ICMPNE | opcodes::IF_ICMPLT | opcodes::IF_ICMPGE | opcodes::IF_ICMPGT | opcodes::IF_ICMPLE
                | opcodes::IF_ACMPEQ | opcodes::IF_ACMPNE
                | opcodes::IFNULL | opcodes::IFNONNULL => {
                    if pc + 3 <= code.len() {
                        let off = i16::from_be_bytes([code[pc + 1], code[pc + 2]]) as i32;
                        let target = (pc as i32 + off) as i32;
                        if target >= 0 { leaders.insert(target as u16); }
                        // fall-through is next instruction
                        let next = pc + len;
                        if next < code.len() { leaders.insert(next as u16); }
                    }
                }
                0xaa /*tableswitch*/ => {
                    // align to 4 bytes after opcode
                    let pad = (4 - ((pc + 1) % 4)) % 4;
                    let mut idx = pc + 1 + pad;
                    if idx + 12 <= code.len() {
                        let default_off = read_i32(code, idx); idx += 4;
                        let low = read_i32(code, idx); idx += 4;
                        let high = read_i32(code, idx); idx += 4;
                        let count = if high >= low { (high - low + 1) as usize } else { 0 };
                        // default target
                        let def_tgt = (pc as i32 + default_off) as i32;
                        if def_tgt >= 0 { leaders.insert(def_tgt as u16); }
                        for _ in 0..count {
                            if idx + 4 > code.len() { break; }
                            let off = read_i32(code, idx); idx += 4;
                            let tgt = (pc as i32 + off) as i32;
                            if tgt >= 0 { leaders.insert(tgt as u16); }
                        }
                        // next instruction after switch
                        let next = idx;
                        if next < code.len() { leaders.insert(next as u16); }
                    }
                }
                0xab /*lookupswitch*/ => {
                    // align to 4 bytes after opcode
                    let pad = (4 - ((pc + 1) % 4)) % 4;
                    let mut idx = pc + 1 + pad;
                    if idx + 8 <= code.len() {
                        let default_off = read_i32(code, idx); idx += 4;
                        let npairs = read_i32(code, idx) as usize; idx += 4;
                        let def_tgt = (pc as i32 + default_off) as i32;
                        if def_tgt >= 0 { leaders.insert(def_tgt as u16); }
                        for _ in 0..npairs {
                            if idx + 8 > code.len() { break; }
                            idx += 4; // skip match
                            let off = read_i32(code, idx); idx += 4;
                            let tgt = (pc as i32 + off) as i32;
                            if tgt >= 0 { leaders.insert(tgt as u16); }
                        }
                        // next instruction
                        let next = idx;
                        if next < code.len() { leaders.insert(next as u16); }
                    }
                }
                _ => {}
            }
            pc += len;
        }

        leaders.into_iter().collect()
    }

    /// Similar to compute_from, but emits a single stack item at handler starts (e.g., Throwable)
    pub fn compute_from_with_handler_stack(
        &self,
        code: &[u8],
        exception_handler_starts: &[u16],
        handler_stack_item: VerificationType,
    ) -> StackMapTable {
        use std::collections::BTreeSet;
        let handler_set: BTreeSet<u16> = exception_handler_starts.iter().copied().collect();
        let leaders = self.collect_basic_block_leaders(code, exception_handler_starts);
        let mut frames: Vec<StackMapFrame> = Vec::new();
        let mut prev_offset: i32 = -1;
        for &leader in leaders.iter().filter(|&&o| o != 0) {
            let offset = leader as i32;
            let delta = (offset - prev_offset - 1) as u16;
            if handler_set.contains(&leader) {
                if delta <= 63 {
                    frames.push(StackMapFrame::SameLocals1StackItem {
                        offset_delta: delta,
                        stack: handler_stack_item.clone(),
                    });
                } else {
                    frames.push(StackMapFrame::SameLocals1StackItemExtended {
                        offset_delta: delta,
                        stack: handler_stack_item.clone(),
                    });
                }
            } else {
                if delta <= 63 {
                    frames.push(StackMapFrame::Same { offset_delta: delta });
                } else {
                    frames.push(StackMapFrame::SameExtended { offset_delta: delta });
                }
            }
            prev_offset = offset;
        }
        StackMapTable { frames }
    }
}

fn instruction_length(op: u8, code: &[u8], pc: usize) -> usize {
    match op {
        // 1-byte opcodes (default fallback handled later)
        // Explicit small table for common multi-byte instructions
        0x10 => 2, // bipush
        0x11 => 3, // sipush
        0x12 => 2, // ldc
        0x13 | 0x14 => 3, // ldc_w, ldc2_w
        0x15..=0x19 => 2, // iload..aload
        0x36..=0x3a => 2, // istore..astore
        0x84 => 3, // iinc
        0x99..=0xa7 | 0xc6 | 0xc7 => 3, // if*, goto, ifnull, ifnonnull
        0xc8 | 0xc9 => 5, // goto_w, jsr_w
        0xb2..=0xb5 => 3, // get/put field/static
        0xb6..=0xb8 => 3, // invokevirtual/special/static
        0xb9 => 5, // invokeinterface
        0xba => 5, // invokedynamic
        0xbb => 3, // new
        0xbc => 2, // newarray
        0xbd => 3, // anewarray
        0xc5 => 4, // multianewarray
        0xc0 | 0xc1 => 3, // checkcast, instanceof
        0xaa => {
            // tableswitch: 0xaa [pad] default:4 low:4 high:4 jump offsets:4*(high-low+1)
            let pad = (4 - ((pc + 1) % 4)) % 4;
            let idx = pc + 1 + pad;
            if idx + 12 > code.len() {
                return code.len() - pc;
            }
            let low = read_i32(code, idx + 4);
            let high = read_i32(code, idx + 8);
            let count = if high >= low { (high - low + 1) as usize } else { 0 };
            1 + pad + 12 + count * 4
        }
        0xab => {
            // lookupswitch: 0xab [pad] default:4 npairs:4 (match:4 offset:4)*npairs
            let pad = (4 - ((pc + 1) % 4)) % 4;
            let idx = pc + 1 + pad;
            if idx + 8 > code.len() {
                return code.len() - pc;
            }
            let npairs = read_i32(code, idx + 4) as usize;
            1 + pad + 8 + npairs * 8
        }
        0xc4 => {
            // wide
            if pc + 1 < code.len() {
                let next = code[pc + 1];
                if next == 0x84 { 6 } else { 4 }
            } else { 1 }
        }
        _ => 1,
    }
}

fn read_i32(code: &[u8], idx: usize) -> i32 {
    i32::from_be_bytes([
        *code.get(idx).unwrap_or(&0),
        *code.get(idx + 1).unwrap_or(&0),
        *code.get(idx + 2).unwrap_or(&0),
        *code.get(idx + 3).unwrap_or(&0),
    ])
}

fn read_u16(code: &[u8], idx: usize) -> u16 {
    u16::from_be_bytes([
        *code.get(idx).unwrap_or(&0),
        *code.get(idx + 1).unwrap_or(&0),
    ])
}

fn simulate_effect(op: u8, code: &[u8], pc: usize, st: &mut FrameState, cp: &mut ConstantPool) {
    use VerificationType::*;
    let obj_idx = cp.add_class("java/lang/Object");
    match op {
        // iconst_*, bipush, sipush push Integer
        0x02..=0x08 | opcodes::BIPUSH | opcodes::SIPUSH => st.stack.push(Integer),
        opcodes::LCONST_0 | opcodes::LCONST_1 => st.stack.push(Long),
        opcodes::FCONST_0 | opcodes::FCONST_1 | opcodes::FCONST_2 => st.stack.push(Float),
        opcodes::DCONST_0 | opcodes::DCONST_1 => st.stack.push(Double),
        opcodes::ACONST_NULL => st.stack.push(Null),
        // loads with index
        opcodes::ILOAD => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; st.stack.push(get_local_type(st, idx)); },
        opcodes::LLOAD => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; let _ = get_local_type(st, idx+1); st.stack.push(Long); },
        opcodes::FLOAD => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; st.stack.push(get_local_type(st, idx)); },
        opcodes::DLOAD => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; let _ = get_local_type(st, idx+1); st.stack.push(Double); },
        opcodes::ALOAD => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; st.stack.push(get_local_type(st, idx)); },
        // loads fixed
        opcodes::ILOAD_0 => st.stack.push(get_local_type(st, 0)),
        opcodes::ILOAD_1 => st.stack.push(get_local_type(st, 1)),
        opcodes::ILOAD_2 => st.stack.push(get_local_type(st, 2)),
        opcodes::ILOAD_3 => st.stack.push(get_local_type(st, 3)),
        opcodes::LLOAD_0 | opcodes::LLOAD_1 | opcodes::LLOAD_2 | opcodes::LLOAD_3 => st.stack.push(Long),
        opcodes::FLOAD_0 => st.stack.push(get_local_type(st, 0)),
        opcodes::FLOAD_1 => st.stack.push(get_local_type(st, 1)),
        opcodes::FLOAD_2 => st.stack.push(get_local_type(st, 2)),
        opcodes::FLOAD_3 => st.stack.push(get_local_type(st, 3)),
        opcodes::DLOAD_0 | opcodes::DLOAD_1 | opcodes::DLOAD_2 | opcodes::DLOAD_3 => st.stack.push(Double),
        opcodes::ALOAD_0 => st.stack.push(get_local_type(st, 0)),
        opcodes::ALOAD_1 => st.stack.push(get_local_type(st, 1)),
        opcodes::ALOAD_2 => st.stack.push(get_local_type(st, 2)),
        opcodes::ALOAD_3 => st.stack.push(get_local_type(st, 3)),
        // stores with index: pop and record
        opcodes::ISTORE => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; let t = st.stack.pop().unwrap_or(Top); set_local_type(st, idx, t); },
        opcodes::LSTORE => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; let _ = st.stack.pop(); set_local_type(st, idx, Long); },
        opcodes::FSTORE => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; let t = st.stack.pop().unwrap_or(Top); set_local_type(st, idx, t); },
        opcodes::DSTORE => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; let _ = st.stack.pop(); set_local_type(st, idx, Double); },
        opcodes::ASTORE => { let idx = *code.get(pc+1).unwrap_or(&0) as usize; let t = st.stack.pop().unwrap_or(Object(obj_idx)); let class_idx = if let Object(i) = t { i } else { obj_idx }; set_local_type(st, idx, Object(class_idx)); },
        // stores fixed
        opcodes::ISTORE_0 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 0, t); },
        opcodes::ISTORE_1 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 1, t); },
        opcodes::ISTORE_2 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 2, t); },
        opcodes::ISTORE_3 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 3, t); },
        opcodes::LSTORE_0 => { let _ = st.stack.pop(); set_local_type(st, 0, Long); },
        opcodes::LSTORE_1 => { let _ = st.stack.pop(); set_local_type(st, 1, Long); },
        opcodes::LSTORE_2 => { let _ = st.stack.pop(); set_local_type(st, 2, Long); },
        opcodes::LSTORE_3 => { let _ = st.stack.pop(); set_local_type(st, 3, Long); },
        opcodes::FSTORE_0 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 0, t); },
        opcodes::FSTORE_1 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 1, t); },
        opcodes::FSTORE_2 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 2, t); },
        opcodes::FSTORE_3 => { let t = st.stack.pop().unwrap_or(Top); set_local_type(st, 3, t); },
        opcodes::DSTORE_0 => { let _ = st.stack.pop(); set_local_type(st, 0, Double); },
        opcodes::DSTORE_1 => { let _ = st.stack.pop(); set_local_type(st, 1, Double); },
        opcodes::DSTORE_2 => { let _ = st.stack.pop(); set_local_type(st, 2, Double); },
        opcodes::DSTORE_3 => { let _ = st.stack.pop(); set_local_type(st, 3, Double); },
        opcodes::ASTORE_0 => { let t = st.stack.pop().unwrap_or(Object(obj_idx)); set_local_type(st, 0, t); },
        opcodes::ASTORE_1 => { let t = st.stack.pop().unwrap_or(Object(obj_idx)); set_local_type(st, 1, t); },
        opcodes::ASTORE_2 => { let t = st.stack.pop().unwrap_or(Object(obj_idx)); set_local_type(st, 2, t); },
        opcodes::ASTORE_3 => { let t = st.stack.pop().unwrap_or(Object(obj_idx)); set_local_type(st, 3, t); },
        // array loads/stores
        opcodes::IALOAD => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Integer); },
        opcodes::LALOAD => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Long); },
        opcodes::FALOAD => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Float); },
        opcodes::DALOAD => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Double); },
        opcodes::AALOAD => {
            // Pop index and arrayref; push element type from array descriptor if known, else Object
            let _ = st.stack.pop();
            let arr = st.stack.pop();
            let vt = match arr {
                Some(VerificationType::Object(ci)) => {
                    // Snapshot class name
                    let arr_name_opt: Option<String> = match cp.constants.get((ci - 1) as usize) {
                        Some(super::constpool::Constant::Class(name_idx)) => {
                            match cp.constants.get((*name_idx - 1) as usize) {
                                Some(super::constpool::Constant::Utf8(s)) => Some(s.clone()),
                                _ => None,
                            }
                        }
                        _ => None,
                    };
                    if let Some(arr_name) = arr_name_opt {
                        if arr_name.starts_with('[') {
                            let rem = &arr_name[1..];
                            let b = rem.as_bytes();
                            match b.first().copied().unwrap_or(0) {
                                b'[' => {
                                    // Still an array
                                    VerificationType::Object(cp.add_class(rem))
                                }
                                b'L' => {
                                    // Reference element: Ljava/lang/String;
                                    let mut i = 1usize; let mut j = i;
                                    while j < b.len() && b[j] != b';' { j += 1; }
                                    let name = String::from_utf8_lossy(&b[i..j]).to_string();
                                    VerificationType::Object(cp.add_class(&name))
                                }
                                // Primitive element (should not happen for AALOAD, but be conservative)
                                b'B' | b'C' | b'I' | b'S' | b'Z' => VerificationType::Integer,
                                b'J' => VerificationType::Long,
                                b'F' => VerificationType::Float,
                                b'D' => VerificationType::Double,
                                _ => Object(obj_idx),
                            }
                        } else {
                            Object(obj_idx)
                        }
                    } else { Object(obj_idx) }
                }
                _ => Object(obj_idx),
            };
            st.stack.push(vt);
        },
        opcodes::BALOAD | opcodes::CALOAD | opcodes::SALOAD => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Integer); },
        opcodes::IASTORE | opcodes::LASTORE | opcodes::FASTORE | opcodes::DASTORE | opcodes::AASTORE | opcodes::BASTORE | opcodes::CASTORE | opcodes::SASTORE => {
            // Pop value, index, arrayref
            let _ = st.stack.pop();
            let _ = st.stack.pop();
            let _ = st.stack.pop();
        },
        // stack pop/dup
        opcodes::POP => { let _ = st.stack.pop(); },
        opcodes::POP2 => { let _ = st.stack.pop(); let _ = st.stack.pop(); },
        opcodes::SWAP => {
            // Only valid for two category-1 values
            let len = st.stack.len();
            if len >= 2 {
                let a = st.stack[len - 2].clone();
                let b = st.stack[len - 1].clone();
                let a_cat2 = matches!(a, VerificationType::Long | VerificationType::Double);
                let b_cat2 = matches!(b, VerificationType::Long | VerificationType::Double);
                if !a_cat2 && !b_cat2 {
                    st.stack[len - 2] = b;
                    st.stack[len - 1] = a;
                }
            }
        },
        opcodes::DUP => { if let Some(t) = st.stack.last().cloned() { st.stack.push(t); } },
        opcodes::DUP2 => {
            let len = st.stack.len();
            if len >= 2 {
                let a = st.stack[len - 2].clone();
                let b = st.stack[len - 1].clone();
                st.stack.push(a);
                st.stack.push(b);
            }
        },
        opcodes::DUP_X1 => {
            // Handle category-2: if top is category-2, behave like DUP2_X1
            let len = st.stack.len();
            if len >= 2 {
                let cat2 = matches!(st.stack[len - 1], VerificationType::Long | VerificationType::Double);
                if cat2 {
                    // [..., v3, v2v1(cat2)] -> [..., v2v1, v3, v2v1]
                    if len >= 2 {
                        let vcat2 = st.stack.remove(len - 1);
                        let v3 = st.stack.remove(len - 2);
                        st.stack.push(vcat2.clone());
                        st.stack.push(v3);
                        st.stack.push(vcat2);
                    }
                } else {
                    // category-1: [..., v2, v1] -> [..., v1, v2, v1]
                    let v1 = st.stack.remove(len - 1);
                    let v2 = st.stack.remove(len - 2);
                    st.stack.push(v1.clone());
                    st.stack.push(v2);
                    st.stack.push(v1);
                }
            }
        },
        opcodes::DUP_X2 => {
            // Cases (JVMS): handle cat-2 under top or next
            let len = st.stack.len();
            if len >= 2 {
                let top_cat2 = matches!(st.stack[len - 1], VerificationType::Long | VerificationType::Double);
                let next_cat2 = len >= 2 && matches!(st.stack[len - 2], VerificationType::Long | VerificationType::Double);
                if top_cat2 {
                    // [..., v2v1(cat2)] -> [..., v2v1, v2v1]
                    let v = st.stack.remove(len - 1);
                    st.stack.push(v.clone());
                    st.stack.push(v);
                } else if next_cat2 {
                    // [..., v2v1(cat2), v0] -> [..., v0, v2v1, v0]
                    let v0 = st.stack.remove(len - 1);
                    let vcat2 = st.stack.remove(len - 2);
                    st.stack.push(v0.clone());
                    st.stack.push(vcat2);
                    st.stack.push(v0);
                } else if len >= 3 {
                    // all cat-1: [..., v3, v2, v1] -> [..., v1, v3, v2, v1]
                    let v1 = st.stack.remove(len - 1);
                    let v2 = st.stack.remove(len - 2);
                    let v3 = st.stack.remove(len - 3);
                    st.stack.push(v1.clone());
                    st.stack.push(v3);
                    st.stack.push(v2);
                    st.stack.push(v1);
                }
            }
        },
        opcodes::DUP2_X1 => {
            let len = st.stack.len();
            if len >= 2 {
                let top_cat2 = matches!(st.stack[len - 1], VerificationType::Long | VerificationType::Double);
                if top_cat2 {
                    // [..., v3, v2v1(cat2)] -> [..., v2v1, v3, v2v1]
                    let vcat2 = st.stack.remove(len - 1);
                    let v3 = st.stack.remove(len - 2);
                    st.stack.push(vcat2.clone());
                    st.stack.push(v3);
                    st.stack.push(vcat2);
                } else if len >= 3 {
                    // cat-1 pair: [..., v3, v2, v1] -> [..., v2, v1, v3, v2, v1]
                    let v1 = st.stack.remove(len - 1);
                    let v2 = st.stack.remove(len - 2);
                    let v3 = st.stack.remove(len - 3);
                    st.stack.push(v2.clone());
                    st.stack.push(v1.clone());
                    st.stack.push(v3);
                    st.stack.push(v2);
                    st.stack.push(v1);
                }
            }
        },
        opcodes::DUP2_X2 => {
            let len = st.stack.len();
            if len >= 2 {
                let top_cat2 = matches!(st.stack[len - 1], VerificationType::Long | VerificationType::Double);
                let next_cat2 = len >= 2 && matches!(st.stack[len - 2], VerificationType::Long | VerificationType::Double);
                if top_cat2 && next_cat2 {
                    // [..., v2v1(cat2), v0v_1(cat2)] -> [..., v0v_1, v2v1, v0v_1]
                    let v0cat2 = st.stack.remove(len - 1);
                    let v2cat2 = st.stack.remove(len - 2);
                    st.stack.push(v0cat2.clone());
                    st.stack.push(v2cat2);
                    st.stack.push(v0cat2);
                } else if top_cat2 && len >= 3 {
                    // [..., v3, v2, v1v0(cat2)] -> [..., v1v0, v3, v2, v1v0]
                    let vcat2 = st.stack.remove(len - 1);
                    let v2 = st.stack.remove(len - 2);
                    let v3 = st.stack.remove(len - 3);
                    st.stack.push(vcat2.clone());
                    st.stack.push(v3);
                    st.stack.push(v2);
                    st.stack.push(vcat2);
                } else if next_cat2 && len >= 3 {
                    // [..., v2v1(cat2), v0, vm] -> treat similar to DUP2_X1 then X1
                    let v0 = st.stack.remove(len - 1);
                    let vcat2 = st.stack.remove(len - 2);
                    st.stack.push(v0.clone());
                    st.stack.push(vcat2);
                    st.stack.push(v0);
                } else if len >= 4 {
                    // all cat-1: [..., v4, v3, v2, v1] -> [..., v2, v1, v4, v3, v2, v1]
                    let v1 = st.stack.remove(len - 1);
                    let v2 = st.stack.remove(len - 2);
                    let v3 = st.stack.remove(len - 3);
                    let v4 = st.stack.remove(len - 4);
                    st.stack.push(v2.clone());
                    st.stack.push(v1.clone());
                    st.stack.push(v4);
                    st.stack.push(v3);
                    st.stack.push(v2);
                    st.stack.push(v1);
                }
            }
        },
        // arithmetic: pop 2 push 1
        opcodes::IADD | opcodes::ISUB | opcodes::IMUL | opcodes::IDIV | opcodes::IREM => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Integer); },
        opcodes::LADD | opcodes::LSUB | opcodes::LMUL | opcodes::LDIV | opcodes::LREM => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Long); },
        opcodes::FADD | opcodes::FSUB | opcodes::FMUL | opcodes::FDIV | opcodes::FREM => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Float); },
        opcodes::DADD | opcodes::DSUB | opcodes::DMUL | opcodes::DDIV | opcodes::DREM => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Double); },
        // comparisons consume stack; push int for cmp*
        opcodes::LCMP | opcodes::FCMPL | opcodes::FCMPG | opcodes::DCMPL | opcodes::DCMPG => { let _ = st.stack.pop(); let _ = st.stack.pop(); st.stack.push(Integer); },
        // field access using CP to get field descriptor
        opcodes::GETFIELD => {
            // Pop receiver, but preserve precise reference type on stack from field descriptor
            let _ = st.stack.pop();
            let cp_idx = read_u16(code, pc+1);
            let vt = field_ref_type(cp, cp_idx).unwrap_or(Object(obj_idx));
            st.stack.push(vt);
        },
        opcodes::PUTFIELD => { let _ = st.stack.pop(); let _ = st.stack.pop(); },
        opcodes::GETSTATIC => { let cp_idx = read_u16(code, pc+1); let vt = field_ref_type(cp, cp_idx).unwrap_or(Object(obj_idx)); st.stack.push(vt); },
        opcodes::PUTSTATIC => { let _ = st.stack.pop(); },
        // object creation and init
        opcodes::NEW => {
            // Operand: cp index of class
            let cp_idx = read_u16(code, pc+1);
            st.new_types.insert(pc as u16, cp_idx);
            st.stack.push(Uninitialized(pc as u16));
        },
        opcodes::INVOKEVIRTUAL | opcodes::INVOKESPECIAL | opcodes::INVOKESTATIC | opcodes::INVOKEINTERFACE => {
            let cp_idx = read_u16(code, pc+1);
            let (is_static_call, owner_idx, name_opt, desc, ret) = method_ref_sig(cp, op, cp_idx);
            // Pop args
            let argc = count_args_in_descriptor(&desc);
            for _ in 0..argc { let _ = st.stack.pop(); }
            // Pop receiver for non-static
            if !is_static_call {
                if let Some(top) = st.stack.pop() {
                    if name_opt.as_deref() == Some("<init>") {
                        // If the receiver was an Uninitialized(offset), map via new_types to owner class
                        let (recv_is_uninit_this, recv_off_opt) = match top {
                            UninitializedThis => (true, None),
                            Uninitialized(off) => (false, Some(off)),
                            _ => (false, None),
                        };
                        let owner = match top {
                            UninitializedThis => owner_idx.map(VerificationType::Object).unwrap_or(Object(obj_idx)),
                            Uninitialized(off) => {
                                if let Some(cls_cp) = st.new_types.get(&off).copied() { VerificationType::Object(cls_cp) } else { owner_idx.map(VerificationType::Object).unwrap_or(Object(obj_idx)) }
                            }
                            _ => owner_idx.map(VerificationType::Object).unwrap_or(Object(obj_idx)),
                        };
                        // Convert all matching uninitialized entries in locals
                        for l in &mut st.locals {
                            match l {
                                VerificationType::UninitializedThis if recv_is_uninit_this => { *l = owner.clone(); }
                                VerificationType::Uninitialized(off) if recv_off_opt == Some(*off) => { *l = owner.clone(); }
                                _ => {}
                            }
                        }
                        // Convert all matching uninitialized entries in the remaining stack
                        for s in &mut st.stack {
                            match s {
                                VerificationType::UninitializedThis if recv_is_uninit_this => { *s = owner.clone(); }
                                VerificationType::Uninitialized(off) if recv_off_opt == Some(*off) => { *s = owner.clone(); }
                                _ => {}
                            }
                        }
                        // Do NOT push anything for <init> (void). The remaining duplicate, if any, has been converted.
                    }
                }
            }
            if let Some(rvt) = ret { if !matches!(rvt, VerificationType::Top) { st.stack.push(rvt); } }
        }
        // returns/athrow clear stack (end of block)
        opcodes::IRETURN | opcodes::LRETURN | opcodes::FRETURN | opcodes::DRETURN | opcodes::ARETURN | opcodes::RETURN | opcodes::ATHROW => { st.stack.clear(); },
        _ => {}
    }
}

fn get_local_type(state: &FrameState, idx: usize) -> VerificationType {
    state.locals.get(idx).cloned().unwrap_or(VerificationType::Top)
}

fn set_local_type(state: &mut FrameState, idx: usize, t: VerificationType) {
    use VerificationType::{Top, Long, Double};
    if state.locals.len() <= idx { state.locals.resize(idx + 1, Top); }
    state.locals[idx] = t.clone();
    if matches!(t, Long | Double) {
        if state.locals.len() <= idx + 1 { state.locals.resize(idx + 2, Top); }
        state.locals[idx + 1] = Top;
    }
}

fn compress_frame(delta: u16, prev_locals: &Vec<VerificationType>, prev_stack: &Vec<VerificationType>, locals: &Vec<VerificationType>, stack: &Vec<VerificationType>) -> StackMapFrame {
    // Normalize trailing Top in locals to improve compression
    fn trim_tops(v: &Vec<VerificationType>) -> Vec<VerificationType> {
        use VerificationType::Top;
        let mut out = v.clone();
        while matches!(out.last(), Some(Top)) { out.pop(); }
        out
    }
    let pl = trim_tops(prev_locals);
    let cl = trim_tops(locals);
    // Same frame: locals identical and current stack empty (previous stack content is irrelevant)
    if cl == pl && stack.is_empty() {
        if delta <= 63 { return StackMapFrame::Same { offset_delta: delta }; }
        return StackMapFrame::SameExtended { offset_delta: delta };
    }
    // SameLocals1StackItem
    if cl == pl && stack.len() == 1 {
        if delta <= 63 { return StackMapFrame::SameLocals1StackItem { offset_delta: delta, stack: stack[0].clone() }; }
        return StackMapFrame::SameLocals1StackItemExtended { offset_delta: delta, stack: stack[0].clone() };
    }
    // Append/Chop up to 3 locals difference when stack empty
    if stack.is_empty() {
        if cl.len() > pl.len() && cl.len() - pl.len() <= 3 && pl == cl[0..pl.len()] {
            let k = (cl.len() - pl.len()) as u8;
            let tail: Vec<VerificationType> = cl[pl.len()..].to_vec();
            return StackMapFrame::Append { k, offset_delta: delta, locals: tail };
        }
        if pl.len() > cl.len() && pl.len() - cl.len() <= 3 && cl == pl[0..cl.len()] {
            let k = (pl.len() - cl.len()) as u8;
            return StackMapFrame::Chop { k, offset_delta: delta };
        }
    }
    // Fallback to Full
    StackMapFrame::Full { offset_delta: delta, locals: cl, stack: stack.clone() }
}

/// Seed entry locals from class and method descriptor.
fn seed_entry_locals(
    is_static: bool,
    is_constructor: bool,
    class_internal: &str,
    method_descriptor: &str,
    cp: &mut ConstantPool,
) -> Vec<VerificationType> {
    let mut locals: Vec<VerificationType> = Vec::new();
    if !is_static {
        // For constructors, spec uses UninitializedThis until invokespecial <init>; we approximate with it
        // to be conservative.
        if is_constructor {
            locals.push(VerificationType::UninitializedThis);
        } else {
            let cidx = cp.add_class(class_internal);
            locals.push(VerificationType::Object(cidx));
        }
    }
    locals.extend(parse_method_descriptor_args(method_descriptor, cp));
    locals
}

/// Parse method descriptor like (I[Ljava/lang/String;J)V and return arg verification types.
fn parse_method_descriptor_args(desc: &str, cp: &mut ConstantPool) -> Vec<VerificationType> {
    let mut out: Vec<VerificationType> = Vec::new();
    let bytes = desc.as_bytes();
    let mut i = 0usize;
    // find opening '('
    while i < bytes.len() && bytes[i] != b'(' { i += 1; }
    if i >= bytes.len() { return out; }
    i += 1; // skip '('
    while i < bytes.len() && bytes[i] != b')' {
        let (vt, ni) = parse_field_type(&bytes, i, cp);
        if let Some(t) = vt { out.push(t); }
        i = ni;
    }
    out
}
fn count_args_in_descriptor(desc: &str) -> usize {
    let mut count = 0usize;
    let bytes = desc.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() && bytes[i] != b'(' { i += 1; }
    if i >= bytes.len() { return 0; }
    i += 1;
    while i < bytes.len() && bytes[i] != b')' {
        match bytes[i] {
            b'B' | b'C' | b'D' | b'F' | b'I' | b'J' | b'S' | b'Z' => { count += 1; i += 1; }
            b'L' => { i += 1; while i < bytes.len() && bytes[i] != b';' { i += 1; } if i < bytes.len() { i += 1; count += 1; } }
            b'[' => { i += 1; while i < bytes.len() && bytes[i] == b'[' { i += 1; } if i < bytes.len() { if bytes[i] == b'L' { i += 1; while i < bytes.len() && bytes[i] != b';' { i += 1; } if i < bytes.len() { i += 1; } } else { i += 1; } count += 1; } }
            _ => { i += 1; }
        }
    }
    count
}

fn field_ref_type(cp: &mut ConstantPool, index: u16) -> Option<VerificationType> {
    use super::constpool::Constant;
    let pos = (index - 1) as usize;
    match cp.constants.get(pos) {
        Some(Constant::FieldRef(_c, nt)) => {
            let nt_pos = (*nt - 1) as usize;
            match cp.constants.get(nt_pos) {
                Some(Constant::NameAndType(_name_idx, desc_idx)) => {
                    if let Some(Constant::Utf8(desc)) = cp.constants.get((*desc_idx - 1) as usize) {
                        // Avoid mutable borrow on cp while reading constants
                        let desc_owned = desc.clone();
                        map_field_desc_to_vt(cp, &desc_owned)
                    } else { None }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn map_field_desc_to_vt(cp: &mut ConstantPool, desc: &str) -> Option<VerificationType> {
    use VerificationType::*;
    let b = desc.as_bytes();
    let mut i = 0usize;
    match b.get(i).copied()? {
        b'B' | b'C' | b'I' | b'S' | b'Z' => Some(Integer),
        b'J' => Some(Long),
        b'F' => Some(Float),
        b'D' => Some(Double),
        b'L' => {
            i += 1; let mut j = i; while j < b.len() && b[j] != b';' { j += 1; }
            let name = String::from_utf8_lossy(&b[i..j]).to_string(); Some(Object(cp.add_class(&name)))
        }
        b'[' => {
            // Preserve full array descriptor as class name (e.g., "[I", "[Ljava/lang/String;")
            let name = String::from_utf8_lossy(&b[i..]).to_string();
            Some(Object(cp.add_class(&name)))
        }
        _ => None,
    }
}

fn method_ref_sig(cp: &mut ConstantPool, op: u8, index: u16) -> (bool, Option<u16>, Option<String>, String, Option<VerificationType>) {
    use super::constpool::Constant;
    let pos = (index - 1) as usize;
    let (class_idx, nt_idx) = match cp.constants.get(pos) {
        Some(Constant::MethodRef(c, nt)) | Some(Constant::InterfaceMethodRef(c, nt)) => (*c, *nt),
        _ => return (op == opcodes::INVOKESTATIC, None, None, "()V".to_string(), None),
    };
    let nt_pos = (nt_idx - 1) as usize;
    let (name, desc) = match cp.constants.get(nt_pos) {
        Some(Constant::NameAndType(n, d)) => {
            let name_s = if let Some(Constant::Utf8(s)) = cp.constants.get((*n - 1) as usize) { s.clone() } else { "".to_string() };
            let desc_s = if let Some(Constant::Utf8(s)) = cp.constants.get((*d - 1) as usize) { s.clone() } else { "()V".to_string() };
            (name_s, desc_s)
        }
        _ => ("".to_string(), "()V".to_string()),
    };
    let is_static_call = op == opcodes::INVOKESTATIC;
    let ret = map_return_type_from_desc(cp, &desc);
    (is_static_call, Some(class_idx), Some(name), desc, ret)
}

fn map_return_type_from_desc(cp: &mut ConstantPool, desc: &str) -> Option<VerificationType> {
    use VerificationType::*;
    let b = desc.as_bytes();
    let mut i = 0usize;
    while i < b.len() && b[i] != b')' { i += 1; }
    if i >= b.len() { return None; }
    i += 1;
    if i >= b.len() { return None; }
    match b[i] {
        b'V' => None,
        b'B' | b'C' | b'I' | b'S' | b'Z' => Some(Integer),
        b'J' => Some(Long),
        b'F' => Some(Float),
        b'D' => Some(Double),
        b'L' => {
            i += 1; let mut j = i; while j < b.len() && b[j] != b';' { j += 1; }
            let name = String::from_utf8_lossy(&b[i..j]).to_string(); Some(Object(cp.add_class(&name)))
        }
        b'[' => Some(Object(cp.add_class("java/lang/Object"))),
        _ => None,
    }
}

fn parse_field_type(bytes: &[u8], mut i: usize, cp: &mut ConstantPool) -> (Option<VerificationType>, usize) {
    use VerificationType::*;
    if i >= bytes.len() { return (None, i); }
    match bytes[i] {
        b'B' => (Some(Integer), i + 1),  // treat byte as int category
        b'C' => (Some(Integer), i + 1),  // char as int category for verification
        b'D' => (Some(Double), i + 1),
        b'F' => (Some(Float), i + 1),
        b'I' => (Some(Integer), i + 1),
        b'J' => (Some(Long), i + 1),
        b'S' => (Some(Integer), i + 1),  // short as int category
        b'Z' => (Some(Integer), i + 1),  // boolean as int category
        b'L' => {
            // reference: L<classname>;
            let mut j = i + 1;
            while j < bytes.len() && bytes[j] != b';' { j += 1; }
            if j < bytes.len() {
                let name = String::from_utf8_lossy(&bytes[(i + 1)..j]).to_string();
                let idx = cp.add_class(&name);
                (Some(Object(idx)), j + 1)
            } else { (Some(Object(cp.add_class("java/lang/Object"))), j) }
        }
        b'[' => {
            // array type: capture full array descriptor as class name
            let mut j = i;
            while j < bytes.len() && bytes[j] == b'[' { j += 1; }
            let (_, nj) = parse_field_type(bytes, j, cp);
            let name = String::from_utf8_lossy(&bytes[i..nj]).to_string();
            let idx = cp.add_class(&name);
            (Some(Object(idx)), nj)
        }
        _ => (None, i + 1),
    }
}

use std::collections::{HashMap, VecDeque};
fn merge_into_leader(
    target: u16,
    st: &FrameState,
    leader_index: &HashMap<u16, usize>,
    in_states: &mut Vec<Option<FrameState>>,
    worklist: &mut VecDeque<u16>,
    cp: &mut ConstantPool,
) {
    if let Some(&ti) = leader_index.get(&target) {
        if let Some(existing) = &in_states[ti] {
            let merged = existing.merge_with(st, cp);
            if &merged != existing {
                in_states[ti] = Some(merged);
                worklist.push_back(target);
            }
        } else {
            in_states[ti] = Some(st.clone());
            worklist.push_back(target);
        }
    }
}


