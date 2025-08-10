//! StackMapTable frames and (future) frame computation utilities

use super::attribute::AttributeInfo;
use super::constpool::ConstantPool;
use super::bytecode::opcodes;

/// VerificationTypeInfo as defined in JVMS 4.7.4
#[derive(Debug, Clone)]
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
    let name_index = constant_pool.add_utf8("StackMapTable");
    let info = table.to_bytes();
    AttributeInfo::new(name_index, info)
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


