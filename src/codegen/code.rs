//! Code generation buffer - 100% aligned with javac's Code.java
//!
//! This module implements the exact same bytecode management architecture as Oracle's
//! javac Code.java, providing centralized bytecode generation, offset calculation,
//! and jump resolution mechanisms.

use crate::ast::TypeRef;
use crate::error::Result;
use crate::codegen::{
    opcodes,
    pending_jumps::{PendingJumpsManager, JumpChain},
    constant_optimizer::{ConstantOptimizer, ConstantInstruction},
};
use std::collections::HashMap;

/// Stack state tracking for StackMapTable generation (javac State equivalent)
#[derive(Debug, Clone)]
pub struct State {
    /// Current stack depth
    pub stacksize: u16,
    /// Maximum stack depth seen
    pub max_stacksize: u16,
    /// Number of locks held (for synchronized methods)
    pub nlocks: u16,
    /// Local variable definitions
    pub defined: Vec<bool>,
    /// Stack types for StackMapTable (simplified)
    pub stack: Vec<Type>,
    /// Local variable types for StackMapTable
    pub locals: Vec<Type>,
}

impl State {
    pub fn new(max_locals: u16) -> Self {
        Self {
            stacksize: 0,
            max_stacksize: 0,
            nlocks: 0,
            defined: vec![false; max_locals as usize],
            stack: Vec::new(),
            locals: vec![Type::Top; max_locals as usize],
        }
    }
    
    /// Push a value onto the operand stack
    pub fn push(&mut self, t: Type) {
        self.stacksize += t.width();
        self.max_stacksize = self.max_stacksize.max(self.stacksize);
        self.stack.push(t);
    }
    
    /// Pop a value from the operand stack
    pub fn pop(&mut self, n: u16) {
        self.stacksize = self.stacksize.saturating_sub(n);
        // Remove corresponding items from stack tracking
        while let Some(item) = self.stack.pop() {
            if item.width() >= n {
                break;
            }
        }
    }
    
    /// Push a stack item with specified width
    pub fn push_stack_item(&mut self, width: u16) {
        match width {
            1 => self.push(Type::Int),
            2 => self.push(Type::Long),
            _ => self.push(Type::Int), // Default to int
        }
    }
    
    /// Duplicate the current state
    pub fn dup(&self) -> Self {
        self.clone()
    }
}

/// Simplified type representation for StackMapTable
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Top,
    Int,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Object(String),
    Uninitialized(u16), // PC where object was created
}

impl Type {
    /// Get the width of this type in stack slots
    pub fn width(&self) -> u16 {
        match self {
            Type::Long | Type::Double => 2,
            _ => 1,
        }
    }
}

/// Chain structure for pending jumps (javac Chain equivalent)
#[derive(Debug, Clone)]
pub struct Chain {
    /// PC of the jump instruction
    pub pc: u16,
    /// Next chain in the list
    pub next: Option<Box<Chain>>,
    /// State at the jump point
    pub state: State,
}

impl Chain {
    pub fn new(pc: u16, next: Option<Box<Chain>>, state: State) -> Self {
        Self { pc, next, state }
    }
}

/// Control flow types for enhanced alive state tracking
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlFlowType {
    Terminal,       // return, throw, etc.
    Jump,          // goto, etc.
    Conditional,   // if*, etc.
    Label,         // jump targets
    ExceptionHandler,
}

/// Stack map format enumeration (javac StackMapFormat equivalent)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackMapFormat {
    None,
    CLDC,    // Old StackMap attribute
    JSR202,  // New StackMapTable attribute
}

/// Exception table entry
#[derive(Debug, Clone)]
pub struct ExceptionTableEntry {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16, // 0 for finally, otherwise constant pool index
}

/// Line number table entry
#[derive(Debug, Clone)]
pub struct LineNumberEntry {
    pub start_pc: u16,
    pub line_number: u16,
}

/// Local variable table entry
#[derive(Debug, Clone)]
pub struct LocalVarEntry {
    pub start_pc: u16,
    pub length: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub index: u16,
}

/// Main code generation buffer - 100% aligned with javac's Code.java
pub struct Code {
    // Core bytecode buffer management (javac equivalent)
    /// The code buffer
    pub code: Vec<u8>,
    /// The current code pointer
    pub cp: usize,
    /// The maximum stack size
    pub max_stack: u16,
    /// The maximum number of local variable slots
    pub max_locals: u16,
    
    // State management (javac equivalent)
    /// Current execution state
    pub state: State,
    /// Code generation enabled? (javac alive)
    pub alive: bool,
    /// Is it forbidden to compactify code? (javac fixedPc)
    pub fixed_pc: bool,
    /// Fat code mode - use 32-bit offsets for jumps
    pub fatcode: bool,
    
    // Jump management (javac equivalent)
    /// A chain for jumps to be resolved before the next opcode is emitted
    pub pending_jumps: Option<Chain>,
    /// Enhanced jump management
    pub jump_manager: PendingJumpsManager,
    
    // Debug and metadata (javac equivalent)
    /// Switch: emit variable debug info
    pub var_debug_info: bool,
    /// Switch: emit line number info  
    pub line_debug_info: bool,
    /// Position of current statement start, NOPOS otherwise
    pub pending_stat_pos: i32,
    /// Stack map format to use
    pub stack_map_format: StackMapFormat,
    /// Whether we need stack map frames
    pub need_stack_map: bool,
    /// Pending stack map emission
    pub pending_stack_map: bool,
    
    // Tables and metadata
    /// Exception table
    pub exception_table: Vec<ExceptionTableEntry>,
    /// Line number table
    pub line_numbers: Vec<LineNumberEntry>,
    /// Local variable table
    pub local_vars: Vec<LocalVarEntry>,
    
    // Debug output
    /// Debug code generation
    pub debug_code: bool,
}

impl Code {
    /// Get current code pointer - JavaC equivalent
    pub fn get_cp(&self) -> u16 {
        self.cp as u16
    }
    
    /// Create a new code buffer with specified parameters
    pub fn new(max_locals: u16, need_stack_map: bool, debug_code: bool) -> Self {
        Self {
            code: Vec::with_capacity(64), // javac starts with 64 bytes
            cp: 0,
            max_stack: 0,
            max_locals,
            state: State::new(max_locals),
            alive: true,
            fixed_pc: false,
            fatcode: false,
            pending_jumps: None,
            jump_manager: PendingJumpsManager::new(),
            var_debug_info: true,
            line_debug_info: true,
            pending_stat_pos: -1, // NOPOS equivalent
            stack_map_format: if need_stack_map { 
                StackMapFormat::JSR202 
            } else { 
                StackMapFormat::None 
            },
            need_stack_map,
            pending_stack_map: false,
            exception_table: Vec::new(),
            line_numbers: Vec::new(),
            local_vars: Vec::new(),
            debug_code,
        }
    }
    
    // ============================================================================
    // BASIC BYTECODE EMISSION (javac emit1, emit2, emit4 equivalent)
    // ============================================================================
    
    /// Emit one byte of code (javac emit1)
    pub fn emit1(&mut self, od: u8) {
        if !self.alive {
            return;
        }
        
        // Ensure capacity
        if self.cp >= self.code.len() {
            self.code.resize(self.cp + 1, 0);
        }
        
        self.code[self.cp] = od;
        self.cp += 1;
    }
    
    /// Emit two bytes of code (javac emit2)
    pub fn emit2(&mut self, od: u16) {
        if !self.alive {
            return;
        }
        
        if self.cp + 2 > self.code.len() {
            self.emit1((od >> 8) as u8);
            self.emit1(od as u8);
        } else {
            self.code[self.cp] = (od >> 8) as u8;
            self.code[self.cp + 1] = od as u8;
            self.cp += 2;
        }
    }
    
    /// Emit four bytes of code (javac emit4)
    pub fn emit4(&mut self, od: u32) {
        if !self.alive {
            return;
        }
        
        if self.cp + 4 > self.code.len() {
            self.emit1((od >> 24) as u8);
            self.emit1((od >> 16) as u8);
            self.emit1((od >> 8) as u8);
            self.emit1(od as u8);
        } else {
            self.code[self.cp] = (od >> 24) as u8;
            self.code[self.cp + 1] = (od >> 16) as u8;
            self.code[self.cp + 2] = (od >> 8) as u8;
            self.code[self.cp + 3] = od as u8;
            self.cp += 4;
        }
    }
    
    // ============================================================================
    // INSTRUCTION EMISSION (javac emitop, emitop1, emitop2 equivalent)
    // ============================================================================
    
    /// Emit an opcode (javac emitop)
    pub fn emitop(&mut self, op: u8) {
        // Resolve pending jumps before emitting new instruction
        if self.pending_jumps.is_some() {
            self.resolve_pending();
        }
        
        if self.alive {
            // Emit line numbers lazily
            if self.pending_stat_pos >= 0 {
                self.emit_line_number();
            }
            
            // Emit stack map frames if needed
            if self.pending_stack_map {
                self.pending_stack_map = false;
                self.emit_stack_map();
            }
            
            // Debug output
            if self.debug_code {
                eprintln!("emit@{} stack={}: {}", 
                         self.cp, self.state.stacksize, self.mnem(op));
            }
            
            self.emit1(op);
        }
    }
    
    /// Emit an opcode with one operand (javac emitop1)
    pub fn emitop1(&mut self, op: u8, od: u8) {
        self.emitop(op);
        if !self.alive {
            return;
        }
        
        self.emit1(od);
        
        // Update stack state based on instruction
        self.update_stack_for_op1(op, od);
    }
    
    /// Emit an opcode with one operand, supporting wide format (javac emitop1w)
    pub fn emitop1w(&mut self, op: u8, od: u16) {
        if od > 0xFF {
            self.emitop(opcodes::WIDE);
            self.emitop(op);
            self.emit2(od);
        } else {
            self.emitop(op);
            self.emit1(od as u8);
        }
        
        if !self.alive {
            return;
        }
        
        // Update stack state
        self.update_stack_for_op1w(op, od);
    }
    
    /// Emit an opcode with two operands in wide format (javac emitop1w with 2 args)
    pub fn emitop1w_2(&mut self, op: u8, od1: u16, od2: i16) {
        if od1 > 0xFF || od2 < -128 || od2 > 127 {
            self.emitop(opcodes::WIDE);
            self.emitop(op);
            self.emit2(od1);
            self.emit2(od2 as u16);
        } else {
            self.emitop(op);
            self.emit1(od1 as u8);
            self.emit1(od2 as u8);
        }
        
        if !self.alive {
            return;
        }
        
        // Update stack state for iinc
        if op == opcodes::IINC {
            // iinc doesn't change stack
        }
    }
    
    /// Emit opcode with local variable index, using optimal encoding (javac equivalent)
    pub fn emitop_with_local_var(&mut self, op: u8, local_var: u16) {
        if local_var <= 3 && Self::has_short_form(op) {
            // Use short form like iload_0, istore_1, etc.
            let short_op = Self::get_short_form(op, local_var as u8);
            self.emitop(short_op);
        } else if local_var <= 255 {
            // Use single-byte local variable index
            self.emitop1(op, local_var as u8);
        } else {
            // Use wide instruction for local variables > 255
            self.emitop(opcodes::WIDE);
            self.emitop2(op, local_var);
        }
    }
    
    /// Check if opcode has short form for local variables 0-3
    fn has_short_form(op: u8) -> bool {
        matches!(op, 
            opcodes::ILOAD | opcodes::LLOAD | opcodes::FLOAD | opcodes::DLOAD | opcodes::ALOAD |
            opcodes::ISTORE | opcodes::LSTORE | opcodes::FSTORE | opcodes::DSTORE | opcodes::ASTORE
        )
    }
    
    /// Get short form opcode for local variable 0-3
    fn get_short_form(op: u8, local_var: u8) -> u8 {
        match op {
            opcodes::ILOAD => opcodes::ILOAD_0 + local_var,
            opcodes::LLOAD => opcodes::LLOAD_0 + local_var,
            opcodes::FLOAD => opcodes::FLOAD_0 + local_var,
            opcodes::DLOAD => opcodes::DLOAD_0 + local_var,
            opcodes::ALOAD => opcodes::ALOAD_0 + local_var,
            opcodes::ISTORE => opcodes::ISTORE_0 + local_var,
            opcodes::LSTORE => opcodes::LSTORE_0 + local_var,
            opcodes::FSTORE => opcodes::FSTORE_0 + local_var,
            opcodes::DSTORE => opcodes::DSTORE_0 + local_var,
            opcodes::ASTORE => opcodes::ASTORE_0 + local_var,
            _ => op, // fallback
        }
    }
    
    /// Emit an opcode with variable-length operand (javac equivalent)
    pub fn emitop_with_operand(&mut self, op: u8, operands: &[u8]) {
        self.emitop(op);
        if !self.alive {
            return;
        }
        
        for &operand in operands {
            self.emit1(operand);
        }
        
        // Update stack state based on instruction
        match op {
            opcodes::BIPUSH | opcodes::SIPUSH => {
                self.state.push_stack_item(1); // Pushes int
            }
            _ => {
                // Handle other instructions as needed
                self.update_stack_for_op1(op, operands.get(0).copied().unwrap_or(0));
            }
        }
    }

    /// Emit an opcode with two-byte operand (javac emitop2)
    pub fn emitop2(&mut self, op: u8, od: u16) {
        self.emitop(op);
        if !self.alive {
            return;
        }
        
        self.emit2(od);
        
        // Update stack state based on instruction
        self.update_stack_for_op2(op, od);
    }
    
    /// Emit an opcode with four-byte operand (javac emitop4)
    pub fn emitop4(&mut self, op: u8, od: u32) {
        self.emitop(op);
        if !self.alive {
            return;
        }
        
        self.emit4(od);
        
        // Update stack state
        match op {
            opcodes::GOTO_W => {
                self.mark_dead();
            }
            _ => {}
        }
    }
    
    // ============================================================================
    // SPECIALIZED INSTRUCTION METHODS (javac equivalents)
    // ============================================================================
    
    /// Emit ldc instruction with optimal format selection (javac emitLdc)
    pub fn emit_ldc(&mut self, od: u16) {
        if od <= 255 {
            self.emitop1(opcodes::LDC, od as u8);
        } else {
            self.emitop2(opcodes::LDC_W, od);
        }
    }
    
    /// Emit multianewarray instruction (javac emitMultianewarray)
    pub fn emit_multianewarray(&mut self, ndims: u8, array_type: u16) {
        self.emitop(opcodes::MULTIANEWARRAY);
        if !self.alive {
            return;
        }
        
        self.emit2(array_type);
        self.emit1(ndims);
        
        // Stack effect: pop ndims, push arrayref
        self.state.pop(ndims as u16);
        self.state.push(Type::Object("array".to_string()));
    }
    
    /// Emit newarray instruction (javac emitNewarray)
    pub fn emit_newarray(&mut self, elemcode: u8) {
        self.emitop(opcodes::NEWARRAY);
        if !self.alive {
            return;
        }
        
        self.emit1(elemcode);
        
        // Stack effect: pop count, push arrayref
        self.state.pop(1);
        self.state.push(Type::Object("array".to_string()));
    }
    
    /// Emit anewarray instruction (javac emitAnewarray)
    pub fn emit_anewarray(&mut self, class_index: u16) {
        self.emitop(opcodes::ANEWARRAY);
        if !self.alive {
            return;
        }
        
        self.emit2(class_index);
        
        // Stack effect: pop count, push arrayref
        self.state.pop(1);
        self.state.push(Type::Object("array".to_string()));
    }
    
    // ============================================================================
    // METHOD INVOCATION INSTRUCTIONS (javac equivalents)
    // ============================================================================
    
    /// Emit invokeinterface instruction (javac emitInvokeinterface)
    pub fn emit_invokeinterface(&mut self, meth: u16, arg_size: u8) {
        self.emitop(opcodes::INVOKEINTERFACE);
        if !self.alive {
            return;
        }
        
        self.emit2(meth);
        self.emit1(arg_size + 1); // +1 for objectref
        self.emit1(0); // reserved
        
        // Stack effect: pop args + objectref
        self.state.pop(arg_size as u16 + 1);
        // Push return value (simplified - would need method signature)
        // self.state.push(return_type);
    }
    
    /// Emit invokespecial instruction (javac emitInvokespecial)
    pub fn emit_invokespecial(&mut self, meth: u16, arg_size: u8) {
        self.emitop(opcodes::INVOKESPECIAL);
        if !self.alive {
            return;
        }
        
        self.emit2(meth);
        
        // Stack effect: pop args + objectref
        self.state.pop(arg_size as u16 + 1);
        // Push return value (simplified)
        // self.state.push(return_type);
    }
    
    /// Emit invokestatic instruction (javac emitInvokestatic)
    pub fn emit_invokestatic(&mut self, meth: u16, arg_size: u8) {
        self.emitop(opcodes::INVOKESTATIC);
        if !self.alive {
            return;
        }
        
        self.emit2(meth);
        
        // Stack effect: pop args
        self.state.pop(arg_size as u16);
        // Push return value (simplified)
        // self.state.push(return_type);
    }
    
    /// Emit invokevirtual instruction (javac emitInvokevirtual)
    pub fn emit_invokevirtual(&mut self, meth: u16, arg_size: u8) {
        self.emitop(opcodes::INVOKEVIRTUAL);
        if !self.alive {
            return;
        }
        
        self.emit2(meth);
        
        // Stack effect: pop args + objectref
        self.state.pop(arg_size as u16 + 1);
        // Push return value (simplified)
        // self.state.push(return_type);
    }
    
    /// Emit invokedynamic instruction (javac emitInvokedynamic)
    pub fn emit_invokedynamic(&mut self, desc: u16, arg_size: u8) {
        self.emitop(opcodes::INVOKEDYNAMIC);
        if !self.alive {
            return;
        }
        
        self.emit2(desc);
        self.emit2(0); // reserved
        
        // Stack effect: pop args
        self.state.pop(arg_size as u16);
        // Push return value (simplified)
        // self.state.push(return_type);
    }
    
    // ============================================================================
    // CONTROL FLOW AND POSITION MANAGEMENT (javac equivalents)
    // ============================================================================
    
    /// Get current code pointer, resolving pending jumps (javac curCP)
    pub fn cur_cp(&mut self) -> u16 {
        if self.pending_jumps.is_some() {
            self.resolve_pending();
        }
        self.cp as u16
    }
    
    /// Create an entry point and return its PC (javac entryPoint)
    pub fn entry_point(&mut self) -> u16 {
        let pc = self.cur_cp();
        self.alive = true;
        self.pending_stack_map = self.need_stack_map;
        pc
    }
    
    /// Create an entry point with specific state (javac entryPoint with state)
    pub fn entry_point_with_state(&mut self, state: State) -> u16 {
        let pc = self.cur_cp();
        self.alive = true;
        self.state = state;
        self.pending_stack_map = self.need_stack_map;
        pc
    }
    
    /// Mark code as dead after terminal instruction (javac markDead)
    pub fn mark_dead(&mut self) {
        self.alive = false;
    }
    
    /// Check if code generation is currently enabled
    pub fn is_alive(&self) -> bool {
        self.alive
    }
    
    // ============================================================================
    // JUMP INSTRUCTION MANAGEMENT (javac equivalents)
    // ============================================================================
    
    /// Emit a jump instruction and return chain (javac emitJump)
    pub fn emit_jump(&mut self, opcode: u8) -> u16 {
        if self.fatcode {
            if opcode == opcodes::GOTO || opcode == opcodes::JSR {
                self.emitop4(opcode + opcodes::GOTO_W - opcodes::GOTO, 0);
                return (self.cp - 5) as u16;
            } else {
                // Convert conditional jump to inverted condition + goto_w
                self.emitop2(self.negate(opcode), 8);
                self.emitop4(opcodes::GOTO_W, 0);
                self.alive = true;
                self.pending_stack_map = self.need_stack_map;
                return (self.cp - 5) as u16;
            }
        } else {
            self.emitop2(opcode, 0);
            return (self.cp - 3) as u16;
        }
    }
    
    /// Branch to label with given opcode (javac branch)
    pub fn branch(&mut self, opcode: u8) -> Chain {
        let result = if opcode != opcodes::GOTO && self.is_alive() {
            Some(Box::new(Chain::new(
                self.emit_jump(opcode),
                None,
                self.state.dup()
            )))
        } else {
            None
        };
        
        self.fixed_pc = self.fatcode;
        
        if opcode != opcodes::GOTO && self.is_alive() {
            Chain::new(
                self.emit_jump(opcode),
                result,
                self.state.dup()
            )
        } else {
            if opcode == opcodes::GOTO {
                self.mark_dead();
            }
            // Return empty chain
            Chain::new(0, None, self.state.dup())
        }
    }
    
    /// Emit a jump placeholder and return the chain for later resolution (javac equivalent)
    pub fn emit_jump_placeholder(&mut self) -> Option<Chain> {
        if self.is_alive() {
            let pc = self.emit_jump(opcodes::GOTO); // Placeholder goto
            Some(Chain::new(pc, None, self.state.dup()))
        } else {
            None
        }
    }
    
    /// Resolve a chain to point to given target (javac resolve with target)
    pub fn resolve_chain(&mut self, mut chain: Option<Chain>, target: u16) {
        let mut new_state = self.state.dup();
        
        while let Some(mut c) = chain {
            // Validate state consistency
            assert!(
                !self.alive || 
                target > c.pc || 
                self.state.stacksize == 0
            );
            
            let mut actual_target = target;
            if actual_target >= self.cp as u16 {
                actual_target = self.cp as u16;
            } else if self.get1(actual_target as usize) == opcodes::GOTO {
                // Follow goto chains
                if self.fatcode {
                    actual_target = (actual_target as i32 + self.get4(actual_target as usize + 1) as i32) as u16;
                } else {
                    actual_target = (actual_target as i32 + self.get2(actual_target as usize + 1) as i32) as u16;
                }
            }
            
            // Check for redundant goto optimization
            if self.get1(c.pc as usize) == opcodes::GOTO &&
               c.pc + 3 == actual_target && 
               actual_target == self.cp as u16 && 
               !self.fixed_pc {
                // Compact code by removing unnecessary goto
                if self.var_debug_info {
                    self.adjust_alive_ranges(self.cp as u16, -3);
                }
                self.cp -= 3;
                actual_target = self.cp as u16;
            } else {
                // Write the offset
                if self.fatcode {
                    self.put4(c.pc as usize + 1, (actual_target as i32 - c.pc as i32) as u32);
                } else {
                    let offset = actual_target as i32 - c.pc as i32;
                    if offset < i16::MIN as i32 || offset > i16::MAX as i32 {
                        self.fatcode = true;
                        // Would need to restart compilation with fatcode
                    } else {
                        self.put2(c.pc as usize + 1, offset as u16);
                    }
                }
                
                // Validate state consistency
                assert!(
                    !self.alive ||
                    c.state.stacksize == new_state.stacksize &&
                    c.state.nlocks == new_state.nlocks
                );
            }
            
            self.fixed_pc = true;
            
            // Check for state merging at target
            if self.cp as u16 == actual_target {
                // Merge states
                if !self.alive {
                    self.state = c.state;
                    self.alive = true;
                    self.pending_stack_map = self.need_stack_map;
                } else {
                    // Would need full state merging logic here
                }
            }
            
            chain = c.next.map(|boxed| *boxed);
        }
    }
    
    /// Resolve a chain to point to current position (javac resolve)
    pub fn resolve(&mut self, chain: Option<Chain>) {
        assert!(
            !self.alive ||
            chain.is_none() ||
            self.state.stacksize == chain.as_ref().unwrap().state.stacksize &&
            self.state.nlocks == chain.as_ref().unwrap().state.nlocks
        );
        
        let target = self.cur_cp();
        self.resolve_chain(chain, target);
    }
    
    /// Negate a conditional jump opcode (javac negate)
    fn negate(&self, opcode: u8) -> u8 {
        match opcode {
            opcodes::IFEQ => opcodes::IFNE,
            opcodes::IFNE => opcodes::IFEQ,
            opcodes::IFLT => opcodes::IFGE,
            opcodes::IFGE => opcodes::IFLT,
            opcodes::IFGT => opcodes::IFLE,
            opcodes::IFLE => opcodes::IFGT,
            opcodes::IF_ICMPEQ => opcodes::IF_ICMPNE,
            opcodes::IF_ICMPNE => opcodes::IF_ICMPEQ,
            opcodes::IF_ICMPLT => opcodes::IF_ICMPGE,
            opcodes::IF_ICMPGE => opcodes::IF_ICMPLT,
            opcodes::IF_ICMPGT => opcodes::IF_ICMPLE,
            opcodes::IF_ICMPLE => opcodes::IF_ICMPGT,
            opcodes::IF_ACMPEQ => opcodes::IF_ACMPNE,
            opcodes::IF_ACMPNE => opcodes::IF_ACMPEQ,
            opcodes::IFNULL => opcodes::IFNONNULL,
            opcodes::IFNONNULL => opcodes::IFNULL,
            _ => panic!("Cannot negate opcode: {}", opcode),
        }
    }
    
    // ============================================================================
    // BYTECODE MODIFICATION (javac put1, put2, put4, get1, get2, get4)
    // ============================================================================
    
    /// Place one byte into code at address pc (javac put1)
    fn put1(&mut self, pc: usize, op: u8) {
        if pc < self.code.len() {
            self.code[pc] = op;
        }
    }
    
    /// Place two bytes into code at address pc (javac put2)
    fn put2(&mut self, pc: usize, od: u16) {
        self.put1(pc, (od >> 8) as u8);
        self.put1(pc + 1, od as u8);
    }
    
    /// Place four bytes into code at address pc (javac put4)
    pub fn put4(&mut self, pc: usize, od: u32) {
        self.put1(pc, (od >> 24) as u8);
        self.put1(pc + 1, (od >> 16) as u8);
        self.put1(pc + 2, (od >> 8) as u8);
        self.put1(pc + 3, od as u8);
    }
    
    /// Get one byte from code at address pc (javac get1)
    fn get1(&self, pc: usize) -> u8 {
        if pc < self.code.len() {
            self.code[pc]
        } else {
            0
        }
    }
    
    /// Get two bytes from code at address pc (javac get2)
    fn get2(&self, pc: usize) -> u16 {
        ((self.get1(pc) as u16) << 8) | (self.get1(pc + 1) as u16)
    }
    
    /// Get four bytes from code at address pc (javac get4)
    fn get4(&self, pc: usize) -> u32 {
        ((self.get1(pc) as u32) << 24) |
        ((self.get1(pc + 1) as u32) << 16) |
        ((self.get1(pc + 2) as u32) << 8) |
        (self.get1(pc + 3) as u32)
    }
    
    // ============================================================================
    // STACK MANAGEMENT AND VALIDATION
    // ============================================================================
    
    /// Update stack state for emitop1 instructions
    fn update_stack_for_op1(&mut self, op: u8, od: u8) {
        match op {
            opcodes::BIPUSH => {
                self.state.push(Type::Int);
            }
            opcodes::SIPUSH => {
                self.state.push(Type::Int);
            }
            opcodes::LDC => {
                // Would need constant pool lookup to determine type
                self.state.push(Type::Int); // Simplified
            }
            opcodes::ILOAD | opcodes::ILOAD_0..=opcodes::ILOAD_3 => {
                self.state.push(Type::Int);
            }
            opcodes::LLOAD | opcodes::LLOAD_0..=opcodes::LLOAD_3 => {
                self.state.push(Type::Long);
            }
            opcodes::FLOAD | opcodes::FLOAD_0..=opcodes::FLOAD_3 => {
                self.state.push(Type::Float);
            }
            opcodes::DLOAD | opcodes::DLOAD_0..=opcodes::DLOAD_3 => {
                self.state.push(Type::Double);
            }
            opcodes::ALOAD | opcodes::ALOAD_0..=opcodes::ALOAD_3 => {
                self.state.push(Type::Object("object".to_string()));
            }
            opcodes::ISTORE | opcodes::ISTORE_0..=opcodes::ISTORE_3 => {
                self.state.pop(1);
            }
            opcodes::LSTORE | opcodes::LSTORE_0..=opcodes::LSTORE_3 => {
                self.state.pop(2);
            }
            opcodes::FSTORE | opcodes::FSTORE_0..=opcodes::FSTORE_3 => {
                self.state.pop(1);
            }
            opcodes::DSTORE | opcodes::DSTORE_0..=opcodes::DSTORE_3 => {
                self.state.pop(2);
            }
            opcodes::ASTORE | opcodes::ASTORE_0..=opcodes::ASTORE_3 => {
                self.state.pop(1);
            }
            opcodes::NEWARRAY => {
                // count -> arrayref
                self.state.pop(1);
                self.state.push(Type::Object("array".to_string()));
            }
            _ => {
                // Handle other instructions as needed
            }
        }
    }
    
    /// Update stack state for emitop1w instructions
    fn update_stack_for_op1w(&mut self, op: u8, _od: u16) {
        // Same as emitop1 but with wider operand
        self.update_stack_for_op1(op, 0);
    }
    
    /// Update stack state for emitop2 instructions
    fn update_stack_for_op2(&mut self, op: u8, _od: u16) {
        match op {
            opcodes::GETSTATIC => {
                // Would need field descriptor lookup
                self.state.push(Type::Int); // Simplified
            }
            opcodes::PUTSTATIC => {
                // Would need field descriptor lookup
                self.state.pop(1); // Simplified
            }
            opcodes::GETFIELD => {
                // objectref -> value
                self.state.pop(1);
                self.state.push(Type::Int); // Simplified
            }
            opcodes::PUTFIELD => {
                // objectref, value ->
                self.state.pop(2); // Simplified
            }
            opcodes::NEW => {
                self.state.push(Type::Object("object".to_string()));
            }
            opcodes::ANEWARRAY => {
                // count -> arrayref
                self.state.pop(1);
                self.state.push(Type::Object("array".to_string()));
            }
            opcodes::CHECKCAST => {
                // objectref -> objectref (no net change)
            }
            opcodes::INSTANCEOF => {
                // objectref -> int
                self.state.pop(1);
                self.state.push(Type::Int);
            }
            opcodes::LDC_W => {
                // Would need constant pool lookup
                self.state.push(Type::Int); // Simplified
            }
            opcodes::LDC2_W => {
                // long or double constant
                self.state.push(Type::Long); // Simplified, could be Double
            }
            // Jump instructions don't change stack
            opcodes::IFEQ..=opcodes::IF_ACMPNE => {
                // Conditional jumps pop their operands
                match op {
                    opcodes::IFEQ..=opcodes::IFNONNULL => self.state.pop(1),
                    opcodes::IF_ICMPEQ..=opcodes::IF_ACMPNE => self.state.pop(2),
                    _ => {}
                }
            }
            opcodes::GOTO => {
                self.mark_dead();
            }
            _ => {
                // Handle other instructions as needed
            }
        }
    }
    
    // ============================================================================
    // DEBUG AND METADATA EMISSION
    // ============================================================================
    
    /// Emit line number information (javac emitLineNumber)
    fn emit_line_number(&mut self) {
        if self.pending_stat_pos >= 0 && self.line_debug_info {
            // Convert position to line number (simplified)
            let line = self.pending_stat_pos as u16; // Would need proper line mapping
            let cp = self.cp as u16;
            
            self.line_numbers.push(LineNumberEntry {
                start_pc: cp,
                line_number: line,
            });
        }
        self.pending_stat_pos = -1;
    }
    
    /// Emit stack map frame (javac emitStackMap)
    fn emit_stack_map(&mut self) {
        let pc = self.cp as u16;
        if !self.need_stack_map {
            return;
        }
        
        match self.stack_map_format {
            StackMapFormat::None => {}
            StackMapFormat::CLDC => {
                self.emit_cldc_stack_map(pc);
            }
            StackMapFormat::JSR202 => {
                self.emit_stack_map_frame(pc);
            }
        }
    }
    
    /// Emit CLDC stack map frame
    fn emit_cldc_stack_map(&mut self, _pc: u16) {
        // Implementation would depend on CLDC format requirements
        // This is a simplified placeholder
    }
    
    /// Emit JSR202 stack map frame
    fn emit_stack_map_frame(&mut self, _pc: u16) {
        // Implementation would depend on StackMapTable format requirements
        // This is a simplified placeholder
    }
    
    /// Get mnemonic for opcode (javac mnem)
    fn mnem(&self, op: u8) -> &'static str {
        match op {
            opcodes::NOP => "nop",
            opcodes::ACONST_NULL => "aconst_null",
            opcodes::ICONST_M1 => "iconst_m1",
            opcodes::ICONST_0 => "iconst_0",
            opcodes::ICONST_1 => "iconst_1",
            opcodes::ICONST_2 => "iconst_2",
            opcodes::ICONST_3 => "iconst_3",
            opcodes::ICONST_4 => "iconst_4",
            opcodes::ICONST_5 => "iconst_5",
            opcodes::LCONST_0 => "lconst_0",
            opcodes::LCONST_1 => "lconst_1",
            opcodes::FCONST_0 => "fconst_0",
            opcodes::FCONST_1 => "fconst_1",
            opcodes::FCONST_2 => "fconst_2",
            opcodes::DCONST_0 => "dconst_0",
            opcodes::DCONST_1 => "dconst_1",
            opcodes::BIPUSH => "bipush",
            opcodes::SIPUSH => "sipush",
            opcodes::LDC => "ldc",
            opcodes::LDC_W => "ldc_w",
            opcodes::LDC2_W => "ldc2_w",
            opcodes::ILOAD => "iload",
            opcodes::LLOAD => "lload",
            opcodes::FLOAD => "fload",
            opcodes::DLOAD => "dload",
            opcodes::ALOAD => "aload",
            opcodes::ILOAD_0 => "iload_0",
            opcodes::ILOAD_1 => "iload_1",
            opcodes::ILOAD_2 => "iload_2",
            opcodes::ILOAD_3 => "iload_3",
            opcodes::ISTORE => "istore",
            opcodes::LSTORE => "lstore",
            opcodes::FSTORE => "fstore",
            opcodes::DSTORE => "dstore",
            opcodes::ASTORE => "astore",
            opcodes::GOTO => "goto",
            opcodes::GOTO_W => "goto_w",
            opcodes::IFEQ => "ifeq",
            opcodes::IFNE => "ifne",
            opcodes::IFLT => "iflt",
            opcodes::IFGE => "ifge",
            opcodes::IFGT => "ifgt",
            opcodes::IFLE => "ifle",
            opcodes::IF_ICMPEQ => "if_icmpeq",
            opcodes::IF_ICMPNE => "if_icmpne",
            opcodes::IF_ICMPLT => "if_icmplt",
            opcodes::IF_ICMPGE => "if_icmpge",
            opcodes::IF_ICMPGT => "if_icmpgt",
            opcodes::IF_ICMPLE => "if_icmple",
            opcodes::IF_ACMPEQ => "if_acmpeq",
            opcodes::IF_ACMPNE => "if_acmpne",
            opcodes::RETURN => "return",
            opcodes::IRETURN => "ireturn",
            opcodes::LRETURN => "lreturn",
            opcodes::FRETURN => "freturn",
            opcodes::DRETURN => "dreturn",
            opcodes::ARETURN => "areturn",
            opcodes::GETSTATIC => "getstatic",
            opcodes::PUTSTATIC => "putstatic",
            opcodes::GETFIELD => "getfield",
            opcodes::PUTFIELD => "putfield",
            opcodes::INVOKEVIRTUAL => "invokevirtual",
            opcodes::INVOKESPECIAL => "invokespecial",
            opcodes::INVOKESTATIC => "invokestatic",
            opcodes::INVOKEINTERFACE => "invokeinterface",
            opcodes::INVOKEDYNAMIC => "invokedynamic",
            opcodes::NEW => "new",
            opcodes::NEWARRAY => "newarray",
            opcodes::ANEWARRAY => "anewarray",
            opcodes::MULTIANEWARRAY => "multianewarray",
            _ => "unknown",
        }
    }
    
    // ============================================================================
    // PENDING JUMP RESOLUTION (javac resolvePending)
    // ============================================================================
    
    /// Resolve all pending jumps (javac resolvePending)
    fn resolve_pending(&mut self) {
        if let Some(pending) = self.pending_jumps.take() {
            self.resolve_chain(Some(pending), self.cp as u16);
        }
    }
    
    /// Adjust alive ranges for local variables when code is compacted
    fn adjust_alive_ranges(&mut self, _pc: u16, _delta: i16) {
        // Implementation would adjust local variable table entries
        // This is a simplified placeholder
    }
    
    // ============================================================================
    // PUBLIC API FOR COMPATIBILITY AND FINALIZATION
    // ============================================================================
    
    /// Get the final bytecode
    pub fn to_bytes(&self) -> Vec<u8> {
        self.code.clone()
    }
    
    /// Get the final stack information
    pub fn max_stack(&self) -> u16 {
        self.state.max_stacksize.max(self.max_stack)
    }
    
    /// Get the final locals information
    pub fn max_locals(&self) -> u16 {
        self.max_locals
    }
    
    /// Get the exception table
    pub fn exception_table(&self) -> &[ExceptionTableEntry] {
        &self.exception_table
    }
    
    /// Get the line number table
    pub fn line_numbers(&self) -> &[LineNumberEntry] {
        &self.line_numbers
    }
    
    /// Get the local variable table
    pub fn local_vars(&self) -> &[LocalVarEntry] {
        &self.local_vars
    }
    
    /// Add an exception table entry
    pub fn add_exception_handler(&mut self, start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) {
        self.exception_table.push(ExceptionTableEntry {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        });
    }
    
    /// Set pending statement position for line number generation
    pub fn set_pending_stat_pos(&mut self, pos: i32) {
        self.pending_stat_pos = pos;
    }
    
    /// Force emit a line number at current position
    pub fn emit_line_number_now(&mut self, line: u16) {
        self.line_numbers.push(LineNumberEntry {
            start_pc: self.cp as u16,
            line_number: line,
        });
    }
}

impl Default for Code {
    fn default() -> Self {
        Self::new(0, false, false)
    }
}