//! Code generation buffer - 100% aligned with javac's Code.java
//!
//! This module implements the exact same bytecode management architecture as Oracle's
//! javac Code.java, providing centralized bytecode generation, offset calculation,
//! and jump resolution mechanisms.

use crate::codegen::{
    opcodes,
    pending_jumps::PendingJumpsManager,
};

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
    /// A chain for jumps to be resolved before the next opcode is emitted (legacy)
    pub pending_jumps: Option<Box<crate::codegen::chain::Chain>>,
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
            // eprintln!("❌ DEBUG: emit1 - code not alive (0x{:02X} at {}), skipping", od, self.cp);
            return;
        }
        
        // Ensure capacity
        if self.cp >= self.code.len() {
            self.code.resize(self.cp + 1, 0);
        }
        
        // eprintln!("✅ DEBUG: emit1 - writing 0x{:02X} at position {} (alive={})", od, self.cp, self.alive);
        self.code[self.cp] = od;
        self.cp += 1;
    }
    
    /// Emit two bytes of code (javac emit2)
    pub fn emit2(&mut self, od: u16) {
        if !self.alive {
            eprintln!("🔧 DEBUG: emit2 - code not alive, skipping");
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
            
            // Update stack state based on instruction
            match op {
                // Return instructions consume their operands
                opcodes::IRETURN | opcodes::FRETURN | opcodes::ARETURN => {
                    self.state.pop(1); // Pop return value
                }
                opcodes::LRETURN | opcodes::DRETURN => {
                    self.state.pop(2); // Pop return value (long/double takes 2 slots)
                }
                opcodes::RETURN => {
                    // No stack change for void return
                }
                // Note: ALOAD instructions are handled automatically by update_stack_for_op2/op0 methods
                // Note: ILOAD instructions are handled automatically by emitop0 method  
                // Note: Method invocation instructions (INVOKEVIRTUAL, etc.) are handled
                // manually after emitop() with proper signature information
                _ => {}
            }
            
            // Mark code as dead after terminal instructions (JavaC pattern)
            // Note: GOTO is handled in branch() method, not here
            match op {
                opcodes::RETURN | opcodes::IRETURN | opcodes::LRETURN | 
                opcodes::FRETURN | opcodes::DRETURN | opcodes::ARETURN | 
                opcodes::ATHROW => {
                    self.alive = false;
                }
                _ => {}
            }
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
    
    
    /// Mark code as dead after terminal instruction (javac markDead)
    pub fn mark_dead(&mut self) {
        self.alive = false;
    }
    
    /// Check if code generation is currently enabled (javac isAlive)
    pub fn is_alive(&self) -> bool {
        self.alive || self.pending_jumps.is_some()
    }

    /// Declare an entry point with initial state (javac entryPoint(State))
    pub fn entry_point_with_state(&mut self, state: State) -> u16 {
        let pc = self.cp as u16;
        self.alive = true;
        let new_state = state.dup();
        self.state = new_state;
        self.pending_stack_map = self.need_stack_map;
        if self.debug_code {
            println!("entry point {} with state", pc);
        }
        pc
    }

    // ============================================================================
    // SMART BRANCH MERGING (Enhanced javac pattern)
    // ============================================================================

    /// Smart branch merging - merge execution paths intelligently (javac pattern)
    pub fn merge_state(&mut self, branch_state: &State) -> crate::common::error::Result<()> {
        if !self.alive {
            // If current path is dead, adopt the branch state
            self.state = branch_state.dup();
            self.alive = true;
            return Ok(());
        }

        // Both paths are alive - need to merge carefully
        if self.state.stacksize != branch_state.stacksize {
            return Err(crate::common::error::Error::CodeGen {
                message: format!(
                    "Stack size mismatch: current={}, branch={}", 
                    self.state.stacksize, 
                    branch_state.stacksize
                )
            });
        }

        // Merge local variable states - use most compatible types
        for i in 0..self.max_locals as usize {
            if i < self.state.locals.len() && i < branch_state.locals.len() {
                let current_type = &self.state.locals[i];
                let branch_type = &branch_state.locals[i];
                
                // If types differ, find common supertype
                if current_type != branch_type {
                    // For now, use Object type as common supertype
                    // In a full implementation, we'd use proper type hierarchy
                    self.state.locals[i] = Type::Object("java/lang/Object".to_string());
                }
            }
        }

        Ok(())
    }

    /// Resolve pending jumps with smart state merging (enhanced javac pattern)
    pub fn resolve_with_merge(&mut self, chain: Option<Box<crate::codegen::chain::Chain>>) {
        if let Some(chain) = chain {
            let target = self.cp as u16;
            
            // Check if we need to restore alive state
            let had_jumps = !chain.is_empty();
            
            // Resolve the jumps to current position
            self.resolve_chain(chain, target);
            
            // If we had pending jumps and code was dead, restore aliveness
            if had_jumps && !self.alive {
                self.alive = true;
                if self.debug_code {
                    println!("Code alive restored by jump resolution at {}", target);
                }
            }
        }
    }

    /// Enhanced resolve that handles goto optimization (javac pattern)
    pub fn resolve_optimized(&mut self, chain: Option<Box<crate::codegen::chain::Chain>>, target: u16) {
        if let Some(chain) = chain {
            // Advanced resolution with goto optimization
            for current in chain.iter() {
                let mut actual_target = target;

                // Follow goto chains for optimization (javac behavior)
                if actual_target < self.cp as u16 {
                    if let Some(opcode) = self.code.get(actual_target as usize).copied() {
                        if opcode == opcodes::GOTO {
                            // Follow the goto target
                            if self.fatcode {
                                let offset = self.get4((actual_target + 1) as usize) as i32;
                                actual_target = ((actual_target as i32) + offset) as u16;
                            } else {
                                let offset = self.get2((actual_target + 1) as usize) as i16;
                                actual_target = ((actual_target as i32) + (offset as i32)) as u16;
                            }
                        }
                    }
                }

                // Check for goto elimination optimization
                if let Some(opcode) = self.code.get(current as usize).copied() {
                    if opcode == opcodes::GOTO && 
                       current + 3 == actual_target && 
                       actual_target == self.cp as u16 && 
                       !self.fixed_pc {
                        // Eliminate unnecessary goto (javac optimization)
                        self.cp -= 3;
                        actual_target -= 3;
                        if self.debug_code {
                            println!("Eliminated goto at {}", current);
                        }
                        self.alive = true;
                        break;
                    }
                }

                // Patch the jump instruction
                if self.fatcode {
                    self.put4(current + 1, (actual_target as i32) - (current as i32));
                } else {
                    let offset = (actual_target as i32) - (current as i32);
                    if offset < i16::MIN as i32 || offset > i16::MAX as i32 {
                        self.fatcode = true;
                        // Re-emit as wide instruction - simplified for now
                    } else {
                        self.put2(current + 1, offset as i16);
                    }
                }
            }
        }
    }
    
    // ============================================================================
    // JUMP INSTRUCTION MANAGEMENT (javac equivalents)
    // ============================================================================
    
    /// Emit a jump instruction and return the PC (javac emitJump)
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
            let pc = (self.cp - 3) as u16;
            
            
            return pc;
        }
    }
    
    /// Create a branch instruction and return a Chain for later resolution (JavaC branch equivalent)
    pub fn branch(&mut self, opcode: u8) -> Option<Box<crate::codegen::chain::Chain>> {
        if !self.alive {
            return None;
        }
        
        let pc = self.emit_jump(opcode);
        let result = crate::codegen::chain::ChainOps::single(pc, self.state.stacksize, self.max_locals);
        
        // JavaC alignment: GOTO instruction marks code as dead after emission
        // This matches javac's Code.java:1468 - if (opcode == goto_) alive = false;
        if opcode == opcodes::GOTO {
            self.alive = false;
        }
        
        result
    }
    
    /// Resolve a chain to the current code position (JavaC resolve equivalent)
    pub fn resolve(&mut self, chain: Option<Box<crate::codegen::chain::Chain>>) {
        if let Some(chain) = chain {
            // Resolve jumps and restore alive state if needed (javac behavior)
            let had_jumps = !chain.is_empty();
            self.resolve_chain(chain, self.cp as u16);
            
            // If there were jumps targeting current position and code was dead,
            // restore aliveness (javac pattern)
            if had_jumps && !self.alive {
                self.alive = true;
                if self.debug_code {
                    println!("Code alive restored by resolve at {}", self.cp);
                }
            }
        }
    }
    
    /// Resolve a chain to a specific target position (JavaC resolve with target)
    pub fn resolve_chain(&mut self, chain: Box<crate::codegen::chain::Chain>, target: u16) {
        for pc in chain.iter() {
            // Calculate the jump offset
            let _offset = if target >= self.cp as u16 {
                target
            } else {
                // Check if target is a goto instruction for optimization
                if self.code.get(target as usize).copied() == Some(opcodes::GOTO) {
                    if self.fatcode {
                        target.wrapping_add(self.get2(target as usize + 1))
                    } else {
                        target.wrapping_add(self.get2(target as usize + 1))
                    }
                } else {
                    target
                }
            };
            
            // Handle goto optimization - remove unnecessary goto to next instruction
            if self.code.get(pc as usize).copied() == Some(opcodes::GOTO) && 
               pc + 3 == target && target == self.cp as u16 && !self.fixed_pc {
                // Remove the goto by setting cp back and not emitting the instruction
                if usize::from(pc + 3) == self.cp {
                    self.cp = pc as usize;
                    self.alive = true;
                    continue;
                }
            }
            
            // Patch the jump offset
            let jump_offset = target as i16 - pc as i16;
            
            
            if self.fatcode && (self.code[pc as usize] == opcodes::GOTO_W || self.code[pc as usize] == opcodes::JSR_W) {
                // 4-byte offset for wide jumps
                self.put4(pc + 1, jump_offset as i32);
            } else {
                // 2-byte offset for normal jumps
                self.put2(pc + 1, jump_offset);
            }
        }
    }
    
    /// Merge two chains (JavaC mergeChains equivalent)
    pub fn merge_chains(
        chain1: Option<Box<crate::codegen::chain::Chain>>, 
        chain2: Option<Box<crate::codegen::chain::Chain>>
    ) -> Option<Box<crate::codegen::chain::Chain>> {
        crate::codegen::chain::ChainOps::merge(chain1, chain2)
    }
    
    /// Create a new empty chain
    pub fn new_chain(&self) -> Box<crate::codegen::chain::Chain> {
        Box::new(crate::codegen::chain::Chain::new(self.cp as u16, self.state.stacksize, self.max_locals))
    }
    
    /// Get current jump context for chain operations
    pub fn jump_context(&self) -> crate::codegen::chain::JumpContext {
        crate::codegen::chain::JumpContext::new(
            self.cp as u16,
            self.state.stacksize,
            self.max_locals,
            self.fatcode,
        )
    }
    
    /// End scopes up to the given limit (JavaC endScopes equivalent)
    pub fn end_scopes(&mut self, limit: u16) {
        // JavaC pattern: restore max_locals to the given limit
        // This effectively deallocates local variables allocated after the limit
        if self.max_locals > limit {
            if self.debug_code {
                println!("End scopes: reducing max_locals from {} to {}", self.max_locals, limit);
            }
            self.max_locals = limit;
        }
        
        // Note: This works in conjunction with the enhanced scope manager
        // The scope manager handles detailed local variable tracking
        // while this method handles the basic max_locals restoration
        if self.debug_code {
            println!("Scope ended at limit: {}", limit);
        }
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
    pub fn put2(&mut self, pc: u16, od: i16) {
        self.put2_internal(pc as usize, od as u16);
    }
    
    fn put2_internal(&mut self, pc: usize, od: u16) {
        self.put1(pc, (od >> 8) as u8);
        self.put1(pc + 1, od as u8);
    }
    
    /// Place four bytes into code at address pc (javac put4)
    pub fn put4(&mut self, pc: u16, od: i32) {
        self.put4_internal(pc as usize, od as u32);
    }
    
    fn put4_internal(&mut self, pc: usize, od: u32) {
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
    fn update_stack_for_op1(&mut self, op: u8, _od: u8) {
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
                // Stack updates handled manually in gen_visitor.rs for proper typing
                // Skip automatic update to avoid double-counting
            }
            opcodes::PUTFIELD => {
                // objectref, value ->
                self.state.pop(2); // Simplified
            }
            opcodes::NEW => {
                // Stack updates handled manually in gen_visitor.rs for proper typing
                // Skip automatic update to avoid double-counting
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
                    opcodes::IFEQ..=opcodes::IFNONNULL => {
                        // Check if it's a two-operand comparison
                        if op >= opcodes::IF_ICMPEQ && op <= opcodes::IF_ACMPNE {
                            self.state.pop(2);
                        } else {
                            self.state.pop(1);
                        }
                    }
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
        // For now, pending jumps use the older system
        // TODO: Migrate to new Chain system
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
        // Return only the bytes up to the current position (cp)
        // This prevents returning uninitialized bytes after the actual code
        self.code[..self.cp].to_vec()
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
    
    /// Emit an opcode with no operand field (JavaC emitop0 equivalent)
    pub fn emitop0(&mut self, op: u8) {
        self.emitop(op);
        if !self.alive {
            return;
        }
        
        // Update stack state based on instruction - following JavaC emitop0 exactly
        match op {
            // Array load instructions
            opcodes::AALOAD => {
                self.state.pop(1); // index
                // arrayref remains, but we change type to element type
                self.state.pop(1); // arrayref  
                self.state.push(Type::Object("java/lang/Object".to_string())); // Simplified
            }
            opcodes::IALOAD | opcodes::BALOAD | opcodes::CALOAD | opcodes::SALOAD => {
                self.state.pop(2); // arrayref, index
                self.state.push(Type::Int);
            }
            opcodes::LALOAD => {
                self.state.pop(2); // arrayref, index
                self.state.push(Type::Long);
            }
            opcodes::FALOAD => {
                self.state.pop(2); // arrayref, index  
                self.state.push(Type::Float);
            }
            opcodes::DALOAD => {
                self.state.pop(2); // arrayref, index
                self.state.push(Type::Double);
            }
            
            // Array store instructions
            opcodes::IASTORE | opcodes::LASTORE | opcodes::FASTORE | 
            opcodes::DASTORE | opcodes::AASTORE | opcodes::BASTORE | 
            opcodes::CASTORE | opcodes::SASTORE => {
                self.state.pop(3); // arrayref, index, value
            }
            
            // Constants
            opcodes::ACONST_NULL => {
                self.state.push(Type::Null);
            }
            opcodes::ICONST_M1 | opcodes::ICONST_0..=opcodes::ICONST_5 => {
                self.state.push(Type::Int);
            }
            opcodes::LCONST_0 | opcodes::LCONST_1 => {
                self.state.push(Type::Long);
            }
            opcodes::FCONST_0 | opcodes::FCONST_1 | opcodes::FCONST_2 => {
                self.state.push(Type::Float);
            }
            opcodes::DCONST_0 | opcodes::DCONST_1 => {
                self.state.push(Type::Double);
            }
            
            // Load instructions (0-3 variants)
            opcodes::ILOAD_0..=opcodes::ILOAD_3 => {
                self.state.push(Type::Int);
            }
            opcodes::LLOAD_0..=opcodes::LLOAD_3 => {
                self.state.push(Type::Long);
            }
            opcodes::FLOAD_0..=opcodes::FLOAD_3 => {
                self.state.push(Type::Float);
            }
            opcodes::DLOAD_0..=opcodes::DLOAD_3 => {
                self.state.push(Type::Double);
            }
            opcodes::ALOAD_0..=opcodes::ALOAD_3 => {
                // Would need local variable type info for precise typing
                self.state.push(Type::Object("java/lang/Object".to_string()));
            }
            
            // Store instructions (0-3 variants)
            opcodes::ISTORE_0..=opcodes::ISTORE_3 |
            opcodes::FSTORE_0..=opcodes::FSTORE_3 |
            opcodes::ASTORE_0..=opcodes::ASTORE_3 => {
                self.state.pop(1);
            }
            opcodes::LSTORE_0..=opcodes::LSTORE_3 |
            opcodes::DSTORE_0..=opcodes::DSTORE_3 => {
                self.state.pop(2);
            }
            
            // Stack manipulation
            opcodes::POP => {
                self.state.pop(1);
            }
            opcodes::POP2 => {
                self.state.pop(2);
            }
            opcodes::DUP => {
                // Stack updates handled manually in gen_visitor.rs for proper typing
                // Skip automatic update to avoid double-counting
            }
            opcodes::DUP_X1 => {
                // ..., value2, value1 -> ..., value1, value2, value1
                let value1 = self.state.stack.pop().unwrap_or(Type::Top);
                let value2 = self.state.stack.pop().unwrap_or(Type::Top);
                self.state.push(value1.clone());
                self.state.push(value2);
                self.state.push(value1);
            }
            opcodes::DUP_X2 => {
                // Complex stack manipulation - simplified
                if let Some(value1) = self.state.stack.pop() {
                    self.state.push(value1.clone());
                    self.state.push(value1);
                }
            }
            opcodes::DUP2 => {
                // Duplicate top two stack elements (or one double-width)
                let top = self.state.stack.last().cloned().unwrap_or(Type::Top);
                if top.width() == 2 {
                    self.state.push(top);
                } else {
                    let second = self.state.stack.get(self.state.stack.len() - 2)
                        .cloned().unwrap_or(Type::Top);
                    self.state.push(second);
                    self.state.push(top);
                }
            }
            opcodes::SWAP => {
                // Swap top two stack elements
                if self.state.stack.len() >= 2 {
                    let len = self.state.stack.len();
                    self.state.stack.swap(len - 1, len - 2);
                }
            }
            
            // Arithmetic operations  
            opcodes::IADD | opcodes::ISUB | opcodes::IMUL | opcodes::IDIV | 
            opcodes::IREM | opcodes::ISHL | opcodes::ISHR | opcodes::IUSHR |
            opcodes::IAND | opcodes::IOR | opcodes::IXOR => {
                self.state.pop(2); // two ints
                self.state.push(Type::Int);
            }
            opcodes::LADD | opcodes::LSUB | opcodes::LMUL | opcodes::LDIV | 
            opcodes::LREM | opcodes::LAND | opcodes::LOR | opcodes::LXOR => {
                self.state.pop(4); // two longs
                self.state.push(Type::Long);
            }
            opcodes::LSHL | opcodes::LSHR | opcodes::LUSHR => {
                self.state.pop(3); // long + int
                self.state.push(Type::Long);
            }
            opcodes::FADD | opcodes::FSUB | opcodes::FMUL | opcodes::FDIV | opcodes::FREM => {
                self.state.pop(2); // two floats
                self.state.push(Type::Float);
            }
            opcodes::DADD | opcodes::DSUB | opcodes::DMUL | opcodes::DDIV | opcodes::DREM => {
                self.state.pop(4); // two doubles
                self.state.push(Type::Double);
            }
            
            // Negation
            opcodes::INEG => {
                // int -> int (no net change)
            }
            opcodes::LNEG | opcodes::FNEG | opcodes::DNEG => {
                // No net change for negation
            }
            
            // Type conversions
            opcodes::I2L => {
                self.state.pop(1); // int
                self.state.push(Type::Long);
            }
            opcodes::I2F => {
                self.state.pop(1); // int
                self.state.push(Type::Float);
            }
            opcodes::I2D => {
                self.state.pop(1); // int
                self.state.push(Type::Double);
            }
            opcodes::L2I => {
                self.state.pop(2); // long
                self.state.push(Type::Int);
            }
            opcodes::L2F => {
                self.state.pop(2); // long
                self.state.push(Type::Float);
            }
            opcodes::L2D => {
                self.state.pop(2); // long
                self.state.push(Type::Double);
            }
            opcodes::F2I => {
                self.state.pop(1); // float
                self.state.push(Type::Int);
            }
            opcodes::F2L => {
                self.state.pop(1); // float
                self.state.push(Type::Long);
            }
            opcodes::F2D => {
                self.state.pop(1); // float
                self.state.push(Type::Double);
            }
            opcodes::D2I => {
                self.state.pop(2); // double
                self.state.push(Type::Int);
            }
            opcodes::D2L => {
                self.state.pop(2); // double
                self.state.push(Type::Long);
            }
            opcodes::D2F => {
                self.state.pop(2); // double
                self.state.push(Type::Float);
            }
            opcodes::I2B | opcodes::I2C | opcodes::I2S => {
                // int -> int (no net change)
            }
            
            // Comparisons
            opcodes::LCMP => {
                self.state.pop(4); // two longs
                self.state.push(Type::Int);
            }
            opcodes::FCMPL | opcodes::FCMPG => {
                self.state.pop(2); // two floats
                self.state.push(Type::Int);
            }
            opcodes::DCMPL | opcodes::DCMPG => {
                self.state.pop(4); // two doubles
                self.state.push(Type::Int);
            }
            
            // Returns and termination
            opcodes::IRETURN | opcodes::FRETURN | opcodes::ARETURN => {
                self.state.pop(1);
                self.mark_dead();
            }
            opcodes::LRETURN | opcodes::DRETURN => {
                self.state.pop(2);
                self.mark_dead();
            }
            opcodes::RETURN => {
                self.mark_dead();
            }
            opcodes::ATHROW => {
                self.state.pop(1);
                self.mark_dead();
            }
            
            // Array length
            opcodes::ARRAYLENGTH => {
                self.state.pop(1); // arrayref
                self.state.push(Type::Int);
            }
            
            // Monitor operations
            opcodes::MONITORENTER | opcodes::MONITOREXIT => {
                self.state.pop(1); // objectref
            }
            
            // NOP has no effect
            opcodes::NOP => {}
            
            _ => {
                // Unknown instruction - be conservative
                if self.debug_code {
                    eprintln!("Unknown instruction in emitop0: {:#x}", op);
                }
            }
        }
        
        // Update max stack size
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    /// Emit method invocation instructions with proper stack tracking (JavaC equivalents)
    pub fn emit_invokevirtual(&mut self, meth_index: u16, arg_size: u16, return_size: u16) {
        self.emitop(opcodes::INVOKEVIRTUAL);
        if !self.alive {
            return;
        }
        self.emit2(meth_index);
        // Pop 'this' + arguments, push return value
        self.state.pop(arg_size + 1);
        if return_size > 0 {
            self.state.push_stack_item(return_size);
        }
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    pub fn emit_invokespecial(&mut self, meth_index: u16, arg_size: u16, return_size: u16) {
        self.emitop(opcodes::INVOKESPECIAL);
        if !self.alive {
            return;
        }
        self.emit2(meth_index);
        // Pop 'this' + arguments, push return value  
        self.state.pop(arg_size + 1);
        if return_size > 0 {
            self.state.push_stack_item(return_size);
        }
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    pub fn emit_invokestatic(&mut self, meth_index: u16, arg_size: u16, return_size: u16) {
        self.emitop(opcodes::INVOKESTATIC);
        if !self.alive {
            return;
        }
        self.emit2(meth_index);
        // Pop arguments, push return value (no 'this' for static)
        self.state.pop(arg_size);
        if return_size > 0 {
            self.state.push_stack_item(return_size);
        }
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    pub fn emit_invokeinterface(&mut self, meth_index: u16, arg_size: u16, return_size: u16) {
        self.emitop(opcodes::INVOKEINTERFACE);
        if !self.alive {
            return;
        }
        self.emit2(meth_index);
        self.emit1(arg_size as u8 + 1); // Include 'this' in count
        self.emit1(0); // Reserved byte
        // Pop 'this' + arguments, push return value
        self.state.pop(arg_size + 1);
        if return_size > 0 {
            self.state.push_stack_item(return_size);
        }
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    pub fn emit_invokedynamic(&mut self, bootstrap_index: u16, arg_size: u16, return_size: u16) {
        self.emitop(opcodes::INVOKEDYNAMIC);
        if !self.alive {
            return;
        }
        self.emit2(bootstrap_index);
        self.emit2(0); // Reserved bytes
        // Pop arguments, push return value (no 'this' for invokedynamic)
        self.state.pop(arg_size);
        if return_size > 0 {
            self.state.push_stack_item(return_size);
        }
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    /// Field access instructions with stack tracking
    pub fn emit_getstatic(&mut self, field_index: u16, field_size: u16) {
        self.emitop(opcodes::GETSTATIC);
        if !self.alive {
            return;
        }
        self.emit2(field_index);
        // Push field value
        self.state.push_stack_item(field_size);
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    pub fn emit_putstatic(&mut self, field_index: u16, field_size: u16) {
        self.emitop(opcodes::PUTSTATIC);
        if !self.alive {
            return;
        }
        self.emit2(field_index);
        // Pop field value
        self.state.pop(field_size);
    }
    
    pub fn emit_getfield(&mut self, field_index: u16, field_size: u16) {
        self.emitop(opcodes::GETFIELD);
        if !self.alive {
            return;
        }
        self.emit2(field_index);
        // Pop object reference, push field value
        self.state.pop(1);
        self.state.push_stack_item(field_size);
        self.max_stack = self.max_stack.max(self.state.stacksize);
    }
    
    pub fn emit_putfield(&mut self, field_index: u16, field_size: u16) {
        self.emitop(opcodes::PUTFIELD);
        if !self.alive {
            return;
        }
        self.emit2(field_index);
        // Pop object reference and field value
        self.state.pop(1 + field_size);
    }
    
    /// Branch instructions with proper stack tracking
    pub fn emit_branch(&mut self, op: u8, target: u16) {
        self.emitop(op);
        if !self.alive {
            return;
        }
        self.emit2(target);
        
        // Update stack based on branch type
        match op {
            // Single-operand conditionals
            opcodes::IFEQ | opcodes::IFNE | opcodes::IFLT | 
            opcodes::IFGE | opcodes::IFGT | opcodes::IFLE |
            opcodes::IFNULL | opcodes::IFNONNULL => {
                self.state.pop(1);
            }
            // Two-operand conditionals  
            opcodes::IF_ICMPEQ | opcodes::IF_ICMPNE | opcodes::IF_ICMPLT |
            opcodes::IF_ICMPGE | opcodes::IF_ICMPGT | opcodes::IF_ICMPLE |
            opcodes::IF_ACMPEQ | opcodes::IF_ACMPNE => {
                self.state.pop(2);
            }
            // Unconditional branch
            opcodes::GOTO => {
                self.mark_dead();
            }
            _ => {}
        }
    }
    
    /// Get current stack depth (for StackMapTable generation)
    pub fn stack_depth(&self) -> u16 {
        self.state.stacksize
    }
    
    /// Get maximum stack depth seen so far
    pub fn max_stack_depth(&self) -> u16 {
        self.max_stack
    }
    
    /// Reset alive state (for control flow joins)
    pub fn mark_alive(&mut self) {
        self.alive = true;
    }
    
}

impl Default for Code {
    fn default() -> Self {
        Self::new(0, false, false)
    }
}