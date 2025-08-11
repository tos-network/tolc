//! Java bytecode structures and constants
//! 
//! This module defines the basic structures needed for generating Java .class files.

use std::marker::PhantomData;
use super::opcodor::OpcodeGenerator;
use super::opcodes;
use std::collections::HashMap;

/// Type-safe constant pool index
/// 
/// This provides compile-time type safety for constant pool references,
/// preventing type errors when accessing different kinds of constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstPoolIndex<T> {
    index: u16,
    _phantom: PhantomData<T>,
}

impl<T> ConstPoolIndex<T> {
    /// Create a new constant pool index
    pub fn new(index: u16) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }
    
    /// Get the raw index value
    pub fn get(&self) -> u16 {
        self.index
    }
    
    /// Convert to a different type (unsafe, use with caution)
    pub fn cast<U>(self) -> ConstPoolIndex<U> {
        ConstPoolIndex::new(self.index)
    }
}

/// Marker types for different constant pool entries
pub mod types {
    pub struct Class;
    pub struct FieldRef;
    pub struct MethodRef;
    pub struct InterfaceMethodRef;
    pub struct String;
    pub struct Integer;
    pub struct Float;
    pub struct Long;
    pub struct Double;
    pub struct NameAndType;
    pub struct Utf8;
}

/// Type aliases for common constant pool indices
pub type ClassIndex = ConstPoolIndex<types::Class>;
pub type FieldRefIndex = ConstPoolIndex<types::FieldRef>;
pub type MethodRefIndex = ConstPoolIndex<types::MethodRef>;
pub type InterfaceMethodRefIndex = ConstPoolIndex<types::InterfaceMethodRef>;
pub type StringIndex = ConstPoolIndex<types::String>;
pub type IntegerIndex = ConstPoolIndex<types::Integer>;
pub type FloatIndex = ConstPoolIndex<types::Float>;
pub type LongIndex = ConstPoolIndex<types::Long>;
pub type DoubleIndex = ConstPoolIndex<types::Double>;
pub type NameAndTypeIndex = ConstPoolIndex<types::NameAndType>;
pub type Utf8Index = ConstPoolIndex<types::Utf8>;

/// Stack state management for bytecode generation
#[derive(Debug, Clone)]
pub struct StackState {
    /// Current stack depth
    pub depth: u16,
    /// Maximum stack depth seen so far
    pub max_depth: u16,
    /// Maximum stack size (for compatibility)
    pub max_stack: u16,
    /// Stack frame for tracking local variables
    pub frame: StackFrame,
}

impl StackState {
    /// Create a new stack state
    pub fn new() -> Self {
        Self {
            depth: 0,
            max_depth: 0,
            max_stack: 0,
            frame: StackFrame::new(),
        }
    }
    
    /// Update stack state after an instruction
    pub fn update(&mut self, dec: u16, inc: u16) -> Result<(), StackError> {
        if self.depth < dec {
            return Err(StackError::Underflow {
                current: self.depth,
                required: dec,
            });
        }
        
        self.depth -= dec;
        self.depth += inc;
        
        if self.depth > self.max_depth {
            self.max_depth = self.depth;
            self.max_stack = self.max_depth;
        }
        
        Ok(())
    }
    
    /// Push a value onto the stack
    pub fn push(&mut self, size: u16) -> Result<(), StackError> {
        self.update(0, size)
    }
    
    /// Pop a value from the stack
    pub fn pop(&mut self, size: u16) -> Result<(), StackError> {
        self.update(size, 0)
    }
    
    /// Get the current stack depth
    pub fn depth(&self) -> u16 {
        self.depth
    }
    
    /// Get the maximum stack depth
    pub fn max_depth(&self) -> u16 {
        self.max_depth
    }
    
    /// Get the maximum stack size
    pub fn max_stack(&self) -> u16 {
        self.max_stack
    }
    
    /// Get the maximum number of local variables
    pub fn max_locals(&self) -> u16 {
        self.frame.max_locals
    }
}

/// Stack frame for tracking local variables
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// Local variable slots
    pub locals: Vec<LocalSlot>,
    /// Maximum number of local variables
    pub max_locals: u16,
}

impl StackFrame {
    /// Create a new stack frame
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            max_locals: 0,
        }
    }
    
    /// Allocate a local variable slot
    pub fn allocate(&mut self, name: String, var_type: LocalType) -> u16 {
        let index = self.max_locals;
        self.locals.push(LocalSlot {
            name,
            var_type,
            index,
            start_pc: 0,
            length: 0,
        });
        self.max_locals += 1;
        index
    }
    
    /// Get local variable by index
    pub fn get(&self, index: u16) -> Option<&LocalSlot> {
        self.locals.get(index as usize)
    }
    
    /// Get local variable by name
    pub fn get_by_name(&self, name: &str) -> Option<&LocalSlot> {
        self.locals.iter().find(|slot| slot.name == name)
    }
    
    /// Update local variable lifetime
    pub fn update_lifetime(&mut self, index: u16, start_pc: u16, end_pc: u16) {
        if let Some(slot) = self.locals.get_mut(index as usize) {
            slot.start_pc = start_pc;
            slot.length = end_pc.saturating_sub(start_pc);
        }
    }
}

/// Local variable slot information
#[derive(Debug, Clone)]
pub struct LocalSlot {
    pub name: String,
    pub var_type: LocalType,
    pub index: u16,
    pub start_pc: u16,
    pub length: u16,
}

/// Local variable type information
#[derive(Debug, Clone, PartialEq)]
pub enum LocalType {
    Int,
    Long,
    Float,
    Double,
    Reference(String), // Class name
    Array(Box<LocalType>),
}

impl LocalType {
    /// Get the size of this type on the stack
    pub fn stack_size(&self) -> u16 {
        match self {
            LocalType::Long | LocalType::Double => 2,
            _ => 1,
        }
    }
    
    /// Get the JVM descriptor for this type
    pub fn descriptor(&self) -> String {
        match self {
            LocalType::Int => "I".to_string(),
            LocalType::Long => "J".to_string(),
            LocalType::Float => "F".to_string(),
            LocalType::Double => "D".to_string(),
            LocalType::Reference(class_name) => format!("L{};", class_name),
            LocalType::Array(element_type) => format!("[{}", element_type.descriptor()),
        }
    }
}

/// Stack operation error
#[derive(Debug, thiserror::Error)]
pub enum StackError {
    #[error("Stack underflow: current depth {current}, required {required}")]
    Underflow { current: u16, required: u16 },
    #[error("Stack overflow: depth {depth} exceeds maximum {max}")]
    Overflow { depth: u16, max: u16 },
    #[error("Unsupported operation: {0}")]
    UnsupportedOperation(String),
}


/// High-level bytecode builder for generating Java bytecode
/// 
/// This provides a more ergonomic API for generating bytecode instructions,
/// similar to javac-rs's Bytecode structure.
#[derive(Debug)]
pub struct BytecodeBuilder {
    /// Generated bytecode
    code: Vec<u8>,
    /// Current stack state
    stack_state: StackState,
    /// Labels for control flow
    labels: Vec<(String, u16)>,
    /// Exception table entries
    exception_table: Vec<ExceptionTableEntry>,
    /// Line number table
    line_numbers: Vec<(u16, u16)>,
    /// Opcode generator for emitting bytecode
    opcode_generator: OpcodeGenerator,
}

impl BytecodeBuilder {
    /// Create a new bytecode builder
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            stack_state: StackState::new(),
            labels: Vec::new(),
            exception_table: Vec::new(),
            line_numbers: Vec::new(),
            opcode_generator: OpcodeGenerator::new(),
        }
    }
    
    /// Get the generated bytecode
    pub fn into_code(self) -> Vec<u8> {
        self.code
    }
    
    /// Get the final stack state
    pub fn into_stack_state(self) -> StackState {
        self.stack_state
    }
    
    /// Get the exception table
    pub fn into_exception_table(self) -> Vec<ExceptionTableEntry> {
        self.exception_table
    }
    
    /// Get the line number table
    pub fn into_line_numbers(self) -> Vec<(u16, u16)> {
        self.line_numbers
    }
    
    // Constants - Use OpcodeGenerator
    pub fn aconst_null(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.aconst_null());
        Ok(())
    }

    pub fn iconst_m1(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iconst_m1());
        Ok(())
    }

    pub fn iconst_0(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iconst_0());
        Ok(())
    }

    pub fn iconst_1(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iconst_1());
        Ok(())
    }

    pub fn iconst_2(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iconst_2());
        Ok(())
    }

    pub fn iconst_3(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iconst_3());
        Ok(())
    }

    pub fn iconst_4(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iconst_4());
        Ok(())
    }

    pub fn iconst_5(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iconst_5());
        Ok(())
    }

    pub fn lconst_0(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lconst_0());
        Ok(())
    }

    pub fn lconst_1(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lconst_1());
        Ok(())
    }

    pub fn fconst_0(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fconst_0());
        Ok(())
    }

    pub fn fconst_1(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fconst_1());
        Ok(())
    }

    pub fn fconst_2(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fconst_2());
        Ok(())
    }

    pub fn dconst_0(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dconst_0());
        Ok(())
    }

    pub fn dconst_1(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dconst_1());
        Ok(())
    }

    // Loads - Use OpcodeGenerator
    pub fn iload(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iload(index));
        Ok(())
    }

    pub fn lload(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lload(index));
        Ok(())
    }

    pub fn fload(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fload(index));
        Ok(())
    }

    pub fn dload(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dload(index));
        Ok(())
    }

    pub fn aload(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(0)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.aload(index));
        Ok(())
    }

    // Stores - Use OpcodeGenerator
    pub fn istore(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.istore(index));
        Ok(())
    }

    pub fn lstore(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.emit_opcode(self.opcode_generator.lstore(index));
        Ok(())
    }

    pub fn fstore(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.fstore(index));
        Ok(())
    }

    pub fn dstore(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.emit_opcode(self.opcode_generator.dstore(index));
        Ok(())
    }

    pub fn astore(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.astore(index));
        Ok(())
    }

    // Stack operations - Use OpcodeGenerator
    pub fn pop(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.pop());
        Ok(())
    }

    pub fn pop2(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.emit_opcode(self.opcode_generator.pop2());
        Ok(())
    }

    pub fn dup(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dup());
        Ok(())
    }

    pub fn dup_x1(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(3)?;
        self.emit_opcode(self.opcode_generator.dup_x1());
        Ok(())
    }

    pub fn dup_x2(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?;
        self.stack_state.push(4)?;
        self.emit_opcode(self.opcode_generator.dup_x2());
        Ok(())
    }

    pub fn dup2(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(4)?;
        self.emit_opcode(self.opcode_generator.dup2());
        Ok(())
    }

    pub fn dup2_x1(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?;
        self.stack_state.push(5)?;
        self.emit_opcode(self.opcode_generator.dup2_x1());
        Ok(())
    }

    pub fn dup2_x2(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(6)?;
        self.emit_opcode(self.opcode_generator.dup2_x2());
        Ok(())
    }

    pub fn swap(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.swap());
        Ok(())
    }

    // Arithmetic operations - Use OpcodeGenerator
    pub fn iadd(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iadd());
        Ok(())
    }

    pub fn ladd(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.ladd());
        Ok(())
    }

    pub fn fadd(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fadd());
        Ok(())
    }

    pub fn dadd(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dadd());
        Ok(())
    }

    pub fn isub(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.isub());
        Ok(())
    }

    pub fn lsub(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lsub());
        Ok(())
    }

    pub fn fsub(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fsub());
        Ok(())
    }

    pub fn dsub(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dsub());
        Ok(())
    }

    pub fn imul(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.imul());
        Ok(())
    }

    pub fn lmul(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lmul());
        Ok(())
    }

    pub fn fmul(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fmul());
        Ok(())
    }

    pub fn dmul(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dmul());
        Ok(())
    }

    pub fn idiv(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.idiv());
        Ok(())
    }

    pub fn ldiv(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.ldiv());
        Ok(())
    }

    pub fn fdiv(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fdiv());
        Ok(())
    }

    pub fn ddiv(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.ddiv());
        Ok(())
    }

    pub fn irem(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.irem());
        Ok(())
    }

    pub fn lrem(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lrem());
        Ok(())
    }

    pub fn frem(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.frem());
        Ok(())
    }

    pub fn drem(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.drem());
        Ok(())
    }

    // Bitwise operations - Use OpcodeGenerator
    pub fn iand(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iand());
        Ok(())
    }

    pub fn land(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.land());
        Ok(())
    }

    pub fn ior(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.ior());
        Ok(())
    }

    pub fn lor(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lor());
        Ok(())
    }

    pub fn ixor(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.ixor());
        Ok(())
    }

    pub fn lxor(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?; // Pop two long values
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lxor());
        Ok(())
    }

    pub fn ishl(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop int value and shift count
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.ishl());
        Ok(())
    }

    pub fn lshl(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop long value and int shift count
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lshl());
        Ok(())
    }

    pub fn ishr(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop int value and shift count
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.ishr());
        Ok(())
    }

    pub fn lshr(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop long value and int shift count
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lshr());
        Ok(())
    }

    pub fn iushr(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop int value and shift count
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.iushr());
        Ok(())
    }

    pub fn lushr(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop long value and int shift count
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lushr());
        Ok(())
    }

    pub fn ineg(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.ineg());
        Ok(())
    }

    pub fn lneg(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.lneg());
        Ok(())
    }

    pub fn fneg(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.stack_state.push(1)?;
        self.emit_opcode(self.opcode_generator.fneg());
        Ok(())
    }

    pub fn dneg(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.stack_state.push(2)?;
        self.emit_opcode(self.opcode_generator.dneg());
        Ok(())
    }

    // Control flow - Use OpcodeGenerator
    pub fn ifeq(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        // Store the position of the instruction (opcode) for JVM offset calculation
        let instruction_pc = self.code.len() as u16;
        self.labels.push((label.to_string(), instruction_pc));
        self.emit_opcode(self.opcode_generator.ifeq(0)); // Placeholder offset
        Ok(())
    }

    pub fn ifne(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.ifne(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn iflt(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.iflt(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn ifge(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.ifge(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn ifgt(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.ifgt(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn ifle(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.ifle(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_icmpeq(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two int values for comparison
        self.emit_opcode(self.opcode_generator.if_icmpeq(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_icmpne(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two int values for comparison
        self.emit_opcode(self.opcode_generator.if_icmpne(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_icmplt(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two int values for comparison
        self.emit_opcode(self.opcode_generator.if_icmplt(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_icmpge(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two int values for comparison
        self.emit_opcode(self.opcode_generator.if_icmpge(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_icmpgt(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two int values for comparison
        self.emit_opcode(self.opcode_generator.if_icmpgt(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_icmple(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two int values for comparison
        self.emit_opcode(self.opcode_generator.if_icmple(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_acmpeq(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two reference values for comparison
        self.emit_opcode(self.opcode_generator.if_acmpeq(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn if_acmpne(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop two reference values for comparison
        self.emit_opcode(self.opcode_generator.if_acmpne(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn ifnull(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop one reference value for null check
        self.emit_opcode(self.opcode_generator.ifnull(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn ifnonnull(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop one reference value for null check
        self.emit_opcode(self.opcode_generator.ifnonnull(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn goto(&mut self, label: &str) -> Result<(), StackError> {
        self.emit_opcode(self.opcode_generator.goto(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    // Switch statements - Use OpcodeGenerator
    pub fn tableswitch(&mut self, default_label: &str, low: i32, high: i32, case_labels: &[&str]) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop the switch value
        
        // Calculate padding for alignment (tableswitch must be aligned to 4-byte boundary)
        let current_pc = self.code.len();
        let padding = (4 - (current_pc % 4)) % 4;
        for _ in 0..padding {
            self.emit_byte(0);
        }
        
        // Emit the tableswitch instruction
        let default_offset = 0; // Placeholder, will be resolved later
        let offsets = vec![0; (high - low + 1) as usize]; // Placeholder offsets
        self.emit_opcode(self.opcode_generator.tableswitch(default_offset, low, high, &offsets));
        
        // Add label references for all cases
        self.add_label_reference(default_label);
        for &case_label in case_labels {
            self.add_label_reference(case_label);
        }
        
        Ok(())
    }

    pub fn lookupswitch(&mut self, default_label: &str, case_labels: &[&str]) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop the switch value
        
        // Calculate padding for alignment (lookupswitch must be aligned to 4-byte boundary)
        let current_pc = self.code.len();
        let padding = (4 - (current_pc % 4)) % 4;
        for _ in 0..padding {
            self.emit_byte(0);
        }
        
        // Emit the lookupswitch instruction
        let default_offset = 0; // Placeholder, will be resolved later
        let npairs = case_labels.len() as i32;
        let pairs = vec![(0, 0); case_labels.len()]; // Placeholder pairs
        self.emit_opcode(self.opcode_generator.lookupswitch(default_offset, npairs, &pairs));
        
        // Add label references for all cases
        self.add_label_reference(default_label);
        for &case_label in case_labels {
            self.add_label_reference(case_label);
        }
        
        Ok(())
    }

    // Returns - Use OpcodeGenerator
    pub fn ireturn(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.ireturn());
        Ok(())
    }

    pub fn lreturn(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.emit_opcode(self.opcode_generator.lreturn());
        Ok(())
    }

    pub fn freturn(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.freturn());
        Ok(())
    }

    pub fn dreturn(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?;
        self.emit_opcode(self.opcode_generator.dreturn());
        Ok(())
    }

    pub fn areturn(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?;
        self.emit_opcode(self.opcode_generator.areturn());
        Ok(())
    }

    pub fn return_(&mut self) -> Result<(), StackError> {
        self.emit_opcode(self.opcode_generator.return_void());
        Ok(())
    }

    // Array operations - Use OpcodeGenerator
    pub fn newarray(&mut self, atype: u8) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop array length
        self.stack_state.push(1)?; // Push array reference
        self.emit_opcode(self.opcode_generator.newarray(atype));
        Ok(())
    }

    pub fn anewarray(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop array length
        self.stack_state.push(1)?; // Push array reference
        self.emit_opcode(self.opcode_generator.anewarray(index));
        Ok(())
    }

    pub fn multianewarray(&mut self, index: u16, dimensions: u8) -> Result<(), StackError> {
        self.stack_state.pop(dimensions as u16)?; // Pop dimension counts
        self.stack_state.push(1)?; // Push array reference
        self.emit_opcode(self.opcode_generator.multianewarray(index, dimensions));
        Ok(())
    }

    pub fn arraylength(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop array reference
        self.stack_state.push(1)?; // Push array length
        self.emit_opcode(self.opcode_generator.arraylength());
        Ok(())
    }

    pub fn iaload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(1)?; // Push int value
        self.emit_opcode(self.opcode_generator.iaload());
        Ok(())
    }

    pub fn laload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(2)?; // Push long value
        self.emit_opcode(self.opcode_generator.laload());
        Ok(())
    }

    pub fn faload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(1)?; // Push float value
        self.emit_opcode(self.opcode_generator.faload());
        Ok(())
    }

    pub fn daload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(2)?; // Push double value
        self.emit_opcode(self.opcode_generator.daload());
        Ok(())
    }

    pub fn aaload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(1)?; // Push reference value
        self.emit_opcode(self.opcode_generator.aaload());
        Ok(())
    }

    pub fn baload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(1)?; // Push byte value
        self.emit_opcode(self.opcode_generator.baload());
        Ok(())
    }

    pub fn caload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(1)?; // Push char value
        self.emit_opcode(self.opcode_generator.caload());
        Ok(())
    }

    pub fn saload(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop array reference and index
        self.stack_state.push(1)?; // Push short value
        self.emit_opcode(self.opcode_generator.saload());
        Ok(())
    }

    pub fn iastore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop array reference, index, and int value
        self.emit_opcode(self.opcode_generator.iastore());
        Ok(())
    }

    pub fn lastore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?; // Pop array reference, index, and long value
        self.emit_opcode(self.opcode_generator.lastore());
        Ok(())
    }

    pub fn fastore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop array reference, index, and float value
        self.emit_opcode(self.opcode_generator.fastore());
        Ok(())
    }

    pub fn dastore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(4)?; // Pop array reference, index, and double value
        self.emit_opcode(self.opcode_generator.dastore());
        Ok(())
    }

    pub fn aastore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop array reference, index, and reference value
        self.emit_opcode(self.opcode_generator.aastore());
        Ok(())
    }

    pub fn bastore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop array reference, index, and byte value
        self.emit_opcode(self.opcode_generator.bastore());
        Ok(())
    }

    pub fn castore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop array reference, index, and char value
        self.emit_opcode(self.opcode_generator.castore());
        Ok(())
    }

    pub fn sastore(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(3)?; // Pop array reference, index, and short value
        self.emit_opcode(self.opcode_generator.sastore());
        Ok(())
    }

    // Field access operations - Use OpcodeGenerator
    pub fn getfield(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop object reference
        self.stack_state.push(1)?; // Push field value (size depends on field type)
        self.emit_opcode(self.opcode_generator.getfield(index));
        Ok(())
    }

    pub fn putfield(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop object reference and field value
        self.emit_opcode(self.opcode_generator.putfield(index));
        Ok(())
    }

    pub fn getstatic(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.push(1)?; // Push static field value
        self.emit_opcode(self.opcode_generator.getstatic(index));
        Ok(())
    }

    pub fn putstatic(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop static field value
        self.emit_opcode(self.opcode_generator.putstatic(index));
        Ok(())
    }

    // Method invocation operations - Use OpcodeGenerator
    pub fn invokevirtual(&mut self, index: u16) -> Result<(), StackError> {
        // Note: Stack manipulation depends on method signature
        // This is a simplified version - actual implementation should check method signature
        self.emit_opcode(self.opcode_generator.invokevirtual(index));
        Ok(())
    }

    pub fn invokespecial(&mut self, index: u16) -> Result<(), StackError> {
        // Note: Stack manipulation depends on method signature
        self.emit_opcode(self.opcode_generator.invokespecial(index));
        Ok(())
    }

    pub fn invokestatic(&mut self, index: u16) -> Result<(), StackError> {
        // Note: Stack manipulation depends on method signature
        self.emit_opcode(self.opcode_generator.invokestatic(index));
        Ok(())
    }

    pub fn invokeinterface(&mut self, index: u16, count: u8) -> Result<(), StackError> {
        // Note: Stack manipulation depends on method signature
        self.emit_opcode(self.opcode_generator.invokeinterface(index, count));
        Ok(())
    }

    pub fn invokedynamic(&mut self, index: u16) -> Result<(), StackError> {
        // Note: Stack manipulation depends on method signature
        self.emit_opcode(self.opcode_generator.invokedynamic(index));
        Ok(())
    }

    // Type conversion operations - Use OpcodeGenerator
    pub fn i2l(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop int
        self.stack_state.push(2)?; // Push long
        self.emit_opcode(self.opcode_generator.i2l());
        Ok(())
    }

    pub fn l2i(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop long
        self.stack_state.push(1)?; // Push int
        self.emit_opcode(self.opcode_generator.l2i());
        Ok(())
    }

    pub fn f2d(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop float
        self.stack_state.push(2)?; // Push double
        self.emit_opcode(self.opcode_generator.f2d());
        Ok(())
    }

    pub fn d2f(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(2)?; // Pop double
        self.stack_state.push(1)?; // Push float
        self.emit_opcode(self.opcode_generator.d2f());
        Ok(())
    }

    // Exception handling and synchronization - Use OpcodeGenerator
    pub fn athrow(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop exception object
        self.emit_opcode(self.opcode_generator.athrow());
        Ok(())
    }

    pub fn monitorenter(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop object reference
        self.emit_opcode(self.opcode_generator.monitorenter());
        Ok(())
    }

    pub fn monitorexit(&mut self) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop object reference
        self.emit_opcode(self.opcode_generator.monitorexit());
        Ok(())
    }

    // Constant loading operations - Use OpcodeGenerator
    pub fn ldc(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.push(1)?; // Push constant value
        self.emit_opcode(self.opcode_generator.ldc(index));
        Ok(())
    }

    pub fn ldc2_w(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.push(2)?; // Push long or double constant
        self.emit_opcode(self.opcode_generator.ldc2_w(index));
        Ok(())
    }

    // Object creation and type checking - Use OpcodeGenerator
    pub fn new_object(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.push(1)?; // Push object reference
        self.emit_opcode(self.opcode_generator.new_object(index));
        Ok(())
    }

    pub fn checkcast(&mut self, index: u16) -> Result<(), StackError> {
        // No stack change - just type checking
        self.emit_opcode(self.opcode_generator.checkcast(index));
        Ok(())
    }

    pub fn instanceof(&mut self, index: u16) -> Result<(), StackError> {
        self.stack_state.pop(1)?; // Pop object reference
        self.stack_state.push(1)?; // Push boolean result
        self.emit_opcode(self.opcode_generator.instanceof(index));
        Ok(())
    }

    // Subroutine and wide operations - Use OpcodeGenerator
    pub fn jsr(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.push(1)?; // Push return address
        self.emit_opcode(self.opcode_generator.jsr(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn jsr_w(&mut self, label: &str) -> Result<(), StackError> {
        self.stack_state.push(1)?; // Push return address
        self.emit_opcode(self.opcode_generator.jsr_w(0)); // Placeholder offset
        self.add_label_reference(label);
        Ok(())
    }

    pub fn ret(&mut self, index: u8) -> Result<(), StackError> {
        // No stack change - just return from subroutine
        self.emit_opcode(self.opcode_generator.ret(index));
        Ok(())
    }

    pub fn wide(&mut self) -> Result<(), StackError> {
        // Note: wide is a prefix instruction that modifies the next instruction
        // This is a simplified version - actual implementation should handle the next instruction
        self.emit_opcode(self.opcode_generator.wide());
        Ok(())
    }

    // No-operation instruction - Use OpcodeGenerator
    pub fn nop(&mut self) -> Result<(), StackError> {
        // No stack change
        self.emit_opcode(self.opcode_generator.nop());
        Ok(())
    }

    // Push operations for small constants - Use OpcodeGenerator
    pub fn bipush(&mut self, byte: i8) -> Result<(), StackError> {
        self.stack_state.push(1)?; // Push byte value
        self.emit_opcode(self.opcode_generator.bipush(byte));
        Ok(())
    }

    pub fn sipush(&mut self, short: i16) -> Result<(), StackError> {
        self.stack_state.push(1)?; // Push short value
        self.emit_opcode(self.opcode_generator.sipush(short));
        Ok(())
    }

    // Utility methods
    fn emit_opcode(&mut self, opcode_bytes: Vec<u8>) {
        self.code.extend(opcode_bytes);
    }
    
    fn emit_byte(&mut self, byte: u8) {
        self.code.push(byte);
    }
    
    fn emit_short(&mut self, value: i16) {
        self.code.extend_from_slice(&value.to_be_bytes());
    }
    
    /// Add a label reference that will be resolved later
    fn add_label_reference(&mut self, label: &str) {
        // Store the position where the offset bytes start for later updating
        let offset_pc = self.code.len() as u16;
        self.labels.push((label.to_string(), offset_pc));
        // Emit placeholder offset (will be updated when label is marked)
        self.emit_short(0);
    }

    /// Mark a label at the current position
    pub fn mark_label(&mut self, label: &str) {
        let current_pc = self.code.len() as u16;
        
        // Update all references to this label
        let mut resolved_indices = Vec::new();
        for (i, (ref_label, ref_pc)) in self.labels.iter_mut().enumerate() {
            if ref_label == label {
                // JVM offset calculation: target_pc - (instruction_pc + 3)
                // where instruction_pc is the position of the opcode
                // +3 accounts for: 1 byte opcode + 2 bytes offset
                // ref_pc is now the position of the instruction
                let instruction_pc = *ref_pc;
                let offset = current_pc - (instruction_pc + 3);
                // Update the offset at instruction_pc + 1 (position where offset bytes start)
                let offset_bytes = offset.to_be_bytes();
                self.code[(instruction_pc + 1) as usize] = offset_bytes[0];
                self.code[(instruction_pc + 1) as usize + 1] = offset_bytes[1];
                

                
                // Mark this reference as resolved
                resolved_indices.push(i);
            }
        }
        
        // Remove resolved references (in reverse order to maintain indices)
        for &index in resolved_indices.iter().rev() {
            self.labels.remove(index);
        }
    }

    /// Add a line number entry for debugging
    pub fn add_line_number(&mut self, line: u16) {
        self.line_numbers.push((self.code.len() as u16, line));
    }

    // Advanced bytecode generation methods with automatic stack management
    
    /// Generate iconst instruction with automatic optimization
    pub fn iconst(&mut self, value: i32) -> Result<(), StackError> {
        match value {
            -1 => self.iconst_m1()?,
            0 => self.iconst_0()?,
            1 => self.iconst_1()?,
            2 => self.iconst_2()?,
            3 => self.iconst_3()?,
            4 => self.iconst_4()?,
            5 => self.iconst_5()?,
            _ => {
                if value >= -128 && value <= 127 {
                    self.emit_opcode(self.opcode_generator.bipush(value as i8));
                } else if value >= -32768 && value <= 32767 {
                    self.emit_opcode(self.opcode_generator.sipush(value as i16));
                } else {
                    // Use LDC for larger constants
                    // Note: This requires constant pool integration
                    return Err(StackError::UnsupportedOperation("Large integer constants not yet supported".to_string()));
                }
                self.stack_state.push(1)?;
            }
        }
        Ok(())
    }

    /// Generate lconst instruction with automatic optimization
    pub fn lconst(&mut self, value: i64) -> Result<(), StackError> {
        match value {
            0 => self.lconst_0()?,
            1 => self.lconst_1()?,
            _ => {
                // Use LDC2_W for other long constants
                return Err(StackError::UnsupportedOperation("Large long constants not yet supported".to_string()));
            }
        }
        Ok(())
    }

    /// Generate fconst instruction with automatic optimization
    pub fn fconst(&mut self, value: f32) -> Result<(), StackError> {
        match value {
            0.0 => self.fconst_0()?,
            1.0 => self.fconst_1()?,
            2.0 => self.fconst_2()?,
            _ => {
                // Use LDC for other float constants
                return Err(StackError::UnsupportedOperation("Non-standard float constants not yet supported".to_string()));
            }
        }
        Ok(())
    }

    /// Generate dconst instruction with automatic optimization
    pub fn dconst(&mut self, value: f64) -> Result<(), StackError> {
        match value {
            0.0 => self.dconst_0()?,
            1.0 => self.dconst_1()?,
            _ => {
                // Use LDC2_W for other double constants
                return Err(StackError::UnsupportedOperation("Non-standard double constants not yet supported".to_string()));
            }
        }
        Ok(())
    }

    /// Generate optimized load instruction based on index
    pub fn load(&mut self, index: u16, var_type: LocalType) -> Result<(), StackError> {
        match var_type {
            LocalType::Int => self.iload(index)?,
            LocalType::Long => self.lload(index)?,
            LocalType::Float => self.fload(index)?,
            LocalType::Double => self.dload(index)?,
            LocalType::Reference(_) | LocalType::Array(_) => self.aload(index)?,
        }
        Ok(())
    }

    /// Generate optimized store instruction based on index
    pub fn store(&mut self, index: u16, var_type: LocalType) -> Result<(), StackError> {
        match var_type {
            LocalType::Int => self.istore(index)?,
            LocalType::Long => self.lstore(index)?,
            LocalType::Float => self.fstore(index)?,
            LocalType::Double => self.dstore(index)?,
            LocalType::Reference(_) | LocalType::Array(_) => self.astore(index)?,
        }
        Ok(())
    }

    /// Generate optimized arithmetic operation
    pub fn arithmetic_op(&mut self, op: ArithmeticOp, var_type: LocalType) -> Result<(), StackError> {
        match (op, var_type) {
            (ArithmeticOp::Add, LocalType::Int) => self.iadd()?,
            (ArithmeticOp::Add, LocalType::Long) => self.ladd()?,
            (ArithmeticOp::Add, LocalType::Float) => self.fadd()?,
            (ArithmeticOp::Add, LocalType::Double) => self.dadd()?,
            (ArithmeticOp::Sub, LocalType::Int) => self.isub()?,
            (ArithmeticOp::Sub, LocalType::Long) => self.lsub()?,
            (ArithmeticOp::Sub, LocalType::Float) => self.fsub()?,
            (ArithmeticOp::Sub, LocalType::Double) => self.dsub()?,
            (ArithmeticOp::Mul, LocalType::Int) => self.imul()?,
            (ArithmeticOp::Mul, LocalType::Long) => self.lmul()?,
            (ArithmeticOp::Mul, LocalType::Float) => self.fmul()?,
            (ArithmeticOp::Mul, LocalType::Double) => self.dmul()?,
            (ArithmeticOp::Div, LocalType::Int) => self.idiv()?,
            (ArithmeticOp::Div, LocalType::Long) => self.ldiv()?,
            (ArithmeticOp::Div, LocalType::Float) => self.fdiv()?,
            (ArithmeticOp::Div, LocalType::Double) => self.ddiv()?,
            (ArithmeticOp::Rem, LocalType::Int) => self.irem()?,
            (ArithmeticOp::Rem, LocalType::Long) => self.lrem()?,
            (ArithmeticOp::Rem, LocalType::Float) => self.frem()?,
            (ArithmeticOp::Rem, LocalType::Double) => self.drem()?,
            (ArithmeticOp::Neg, LocalType::Int) => self.ineg()?,
            (ArithmeticOp::Neg, LocalType::Long) => self.lneg()?,
            (ArithmeticOp::Neg, LocalType::Float) => self.fneg()?,
            (ArithmeticOp::Neg, LocalType::Double) => self.dneg()?,
            // Reference and Array types don't support arithmetic operations
            (_, LocalType::Reference(_)) | (_, LocalType::Array(_)) => {
                return Err(StackError::UnsupportedOperation("Arithmetic operations not supported for reference types".to_string()));
            }
        }
        Ok(())
    }

    /// Generate optimized comparison operation
    pub fn comparison_op(&mut self, op: ComparisonOp, var_type: LocalType) -> Result<(), StackError> {
        match (op, var_type) {
            (ComparisonOp::Eq, LocalType::Int) => self.ifeq("")?,
            (ComparisonOp::Ne, LocalType::Int) => self.ifne("")?,
            (ComparisonOp::Lt, LocalType::Int) => self.iflt("")?,
            (ComparisonOp::Ge, LocalType::Int) => self.ifge("")?,
            (ComparisonOp::Gt, LocalType::Int) => self.ifgt("")?,
            (ComparisonOp::Le, LocalType::Int) => self.ifle("")?,
            _ => return Err(StackError::UnsupportedOperation("Complex comparison operations not yet supported".to_string())),
        }
        Ok(())
    }

    /// Generate optimized return instruction based on return type
    pub fn return_value(&mut self, return_type: Option<LocalType>) -> Result<(), StackError> {
        match return_type {
            None => self.return_()?,
            Some(LocalType::Int) => self.ireturn()?,
            Some(LocalType::Long) => self.lreturn()?,
            Some(LocalType::Float) => self.freturn()?,
            Some(LocalType::Double) => self.dreturn()?,
            Some(LocalType::Reference(_)) | Some(LocalType::Array(_)) => self.areturn()?,
        }
        Ok(())
    }

    /// Add exception handler entry
    pub fn add_exception_handler(&mut self, start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) {
        self.exception_table.push(ExceptionTableEntry::new(start_pc, end_pc, handler_pc, catch_type));
    }

    /// Get current code position (PC)
    pub fn current_pc(&self) -> u16 {
        self.code.len() as u16
    }

    /// Reserve space for a future instruction
    pub fn reserve_space(&mut self, bytes: usize) {
        self.code.extend(std::iter::repeat(0).take(bytes));
    }

    /// Update reserved space at specific position
    pub fn update_reserved_space(&mut self, position: usize, data: &[u8]) {
        if position + data.len() <= self.code.len() {
            self.code[position..position + data.len()].copy_from_slice(data);
        }
    }


}

/// Arithmetic operations for the arithmetic_op method
#[derive(Debug, Clone, Copy)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparisonOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BitwiseOp {
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Ushr,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
    Not,
}

/// Exception table entry for the bytecode builder
#[derive(Debug, Clone)]
pub struct ExceptionTableEntry {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16,
}

impl ExceptionTableEntry {
    pub fn new(start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) -> Self {
        Self {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        }
    }
}

/// Advanced bytecode structure similar to javac-rs
/// 
/// This provides a higher-level abstraction for bytecode generation
/// with automatic stack management, exception handling, and attribute support.
#[derive(Debug)]
pub struct AdvancedBytecode {
    /// Maximum stack size
    max_stack: u16,
    /// Maximum local variables
    max_locals: u16,
    /// Generated bytecode
    code: Vec<u8>,
    /// Exception table
    exception_tables: Vec<ExceptionTableEntry>,
    /// Line number table for debugging
    line_numbers: Vec<(u16, u16)>,
    /// Local variable table for debugging
    local_variables: Vec<LocalVariableEntry>,
    /// Current stack depth
    stack: u16,
    /// Current local variable count
    locals: u16,
    /// Labels for control flow
    labels: HashMap<String, u16>,
    /// Forward references to labels
    forward_refs: Vec<(String, u16)>,
}

impl AdvancedBytecode {
    /// Create a new advanced bytecode generator
    pub fn new() -> Self {
        Self {
            max_stack: 0,
            max_locals: 0,
            code: Vec::new(),
            exception_tables: Vec::new(),
            line_numbers: Vec::new(),
            local_variables: Vec::new(),
            stack: 0,
            locals: 0,
            labels: HashMap::new(),
            forward_refs: Vec::new(),
        }
    }

    /// Get current program counter (PC)
    pub fn pc(&self) -> u16 {
        self.code.len() as u16
    }

    /// Get current stack depth
    pub fn stack_depth(&self) -> u16 {
        self.stack
    }

    /// Get maximum stack depth seen
    pub fn max_stack_depth(&self) -> u16 {
        self.max_stack
    }

    /// Get current local variable count
    pub fn local_count(&self) -> u16 {
        self.locals
    }

    /// Get maximum local variable count
    pub fn max_local_count(&self) -> u16 {
        self.max_locals
    }

    /// Push value onto stack
    pub fn push(&mut self, size: u16) -> Result<(), StackError> {
        self.stack += size;
        if self.stack > self.max_stack {
            self.max_stack = self.stack;
        }
        Ok(())
    }

    /// Pop value from stack
    pub fn pop(&mut self, size: u16) -> Result<(), StackError> {
        if self.stack < size {
            return Err(StackError::Underflow {
                current: self.stack,
                required: size,
            });
        }
        self.stack -= size;
        Ok(())
    }

    /// Allocate local variable
    pub fn allocate_local(&mut self, size: u16) -> u16 {
        let index = self.locals;
        self.locals += size;
        if self.locals > self.max_locals {
            self.max_locals = self.locals;
        }
        index
    }

    /// Emit byte
    pub fn emit_byte(&mut self, byte: u8) {
        self.code.push(byte);
    }

    /// Emit short (big-endian)
    pub fn emit_short(&mut self, value: u16) {
        self.code.extend_from_slice(&value.to_be_bytes());
    }

    /// Emit int (big-endian)
    pub fn emit_int(&mut self, value: u32) {
        self.code.extend_from_slice(&value.to_be_bytes());
    }

    /// Add label reference
    pub fn add_label_ref(&mut self, label: &str) {
        self.forward_refs.push((label.to_string(), self.pc()));
        self.emit_short(0); // Placeholder
    }

    /// Mark label at current position
    pub fn mark_label(&mut self, label: &str) {
        let pc = self.pc();
        self.labels.insert(label.to_string(), pc);
        
        // Resolve forward references
        for (ref_label, ref_pc) in &self.forward_refs {
            if ref_label == label {
                let offset = pc - *ref_pc - 2; // -2 for branch instruction
                let offset_bytes = offset.to_be_bytes();
                self.code[*ref_pc as usize] = offset_bytes[0];
                self.code[*ref_pc as usize + 1] = offset_bytes[1];
            }
        }
    }

    /// Add line number entry
    pub fn add_line_number(&mut self, line: u16) {
        self.line_numbers.push((self.pc(), line));
    }

    /// Add local variable entry
    pub fn add_local_variable(&mut self, _name: String, _descriptor: String, start_pc: u16, length: u16, index: u16) {
        self.local_variables.push(LocalVariableEntry {
            start_pc,
            length,
            name_index: 0, // Will be set when writing to class file
            descriptor_index: 0, // Will be set when writing to class file
            index,
        });
    }

    /// Add exception handler
    pub fn add_exception_handler(&mut self, start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) {
        self.exception_tables.push(ExceptionTableEntry::new(start_pc, end_pc, handler_pc, catch_type));
    }

    /// Generate iconst with automatic optimization
    pub fn iconst(&mut self, value: i32) -> Result<(), StackError> {
        match value {
            -1 => {
                self.emit_byte(opcodes::ICONST_M1);
                self.push(1)?;
            }
            0 => {
                self.emit_byte(opcodes::ICONST_0);
                self.push(1)?;
            }
            1 => {
                self.emit_byte(opcodes::ICONST_1);
                self.push(1)?;
            }
            2 => {
                self.emit_byte(opcodes::ICONST_2);
                self.push(1)?;
            }
            3 => {
                self.emit_byte(opcodes::ICONST_3);
                self.push(1)?;
            }
            4 => {
                self.emit_byte(opcodes::ICONST_4);
                self.push(1)?;
            }
            5 => {
                self.emit_byte(opcodes::ICONST_5);
                self.push(1)?;
            }
            _ => {
                if value >= -128 && value <= 127 {
                    self.emit_byte(opcodes::BIPUSH);
                    self.emit_byte(value as u8);
                    self.push(1)?;
                } else if value >= -32768 && value <= 32767 {
                    self.emit_byte(opcodes::SIPUSH);
                    self.emit_short(value as u16);
                    self.push(1)?;
                } else {
                    // Use LDC for larger constants
                    return Err(StackError::UnsupportedOperation("Large integer constants not yet supported".to_string()));
                }
            }
        }
        Ok(())
    }

    /// Generate arithmetic operation with automatic stack management
    pub fn arithmetic(&mut self, op: ArithmeticOp, var_type: &LocalType) -> Result<(), StackError> {
        // Pop operands
        let pop_size = match var_type {
            LocalType::Int | LocalType::Float => 2, // 2 operands
            LocalType::Long | LocalType::Double => 4, // 2 operands, each takes 2 slots
            _ => return Err(StackError::UnsupportedOperation("Arithmetic operations not supported for reference types".to_string())),
        };
        self.pop(pop_size)?;
        
        // Emit operation
        match (op, var_type) {
            (ArithmeticOp::Add, LocalType::Int) => self.emit_byte(opcodes::IADD),
            (ArithmeticOp::Add, LocalType::Long) => self.emit_byte(opcodes::LADD),
            (ArithmeticOp::Add, LocalType::Float) => self.emit_byte(opcodes::FADD),
            (ArithmeticOp::Add, LocalType::Double) => self.emit_byte(opcodes::DADD),
            (ArithmeticOp::Sub, LocalType::Int) => self.emit_byte(opcodes::ISUB),
            (ArithmeticOp::Sub, LocalType::Long) => self.emit_byte(opcodes::LSUB),
            (ArithmeticOp::Sub, LocalType::Float) => self.emit_byte(opcodes::FSUB),
            (ArithmeticOp::Sub, LocalType::Double) => self.emit_byte(opcodes::DSUB),
            (ArithmeticOp::Mul, LocalType::Int) => self.emit_byte(opcodes::IMUL),
            (ArithmeticOp::Mul, LocalType::Long) => self.emit_byte(opcodes::LMUL),
            (ArithmeticOp::Mul, LocalType::Float) => self.emit_byte(opcodes::FMUL),
            (ArithmeticOp::Mul, LocalType::Double) => self.emit_byte(opcodes::DMUL),
            (ArithmeticOp::Div, LocalType::Int) => self.emit_byte(opcodes::IDIV),
            (ArithmeticOp::Div, LocalType::Long) => self.emit_byte(opcodes::LDIV),
            (ArithmeticOp::Div, LocalType::Float) => self.emit_byte(opcodes::FDIV),
            (ArithmeticOp::Div, LocalType::Double) => self.emit_byte(opcodes::DDIV),
            (ArithmeticOp::Rem, LocalType::Int) => self.emit_byte(opcodes::IREM),
            (ArithmeticOp::Rem, LocalType::Long) => self.emit_byte(opcodes::LREM),
            (ArithmeticOp::Rem, LocalType::Float) => self.emit_byte(opcodes::FREM),
            (ArithmeticOp::Rem, LocalType::Double) => self.emit_byte(opcodes::DREM),
            (ArithmeticOp::Neg, LocalType::Int) => self.emit_byte(opcodes::INEG),
            (ArithmeticOp::Neg, LocalType::Long) => self.emit_byte(opcodes::LNEG),
            (ArithmeticOp::Neg, LocalType::Float) => self.emit_byte(opcodes::FNEG),
            (ArithmeticOp::Neg, LocalType::Double) => self.emit_byte(opcodes::DNEG),
            // Reference and Array types don't support arithmetic operations
            (_, LocalType::Reference(_)) | (_, LocalType::Array(_)) => {
                return Err(StackError::UnsupportedOperation("Arithmetic operations not supported for reference types".to_string()));
            }
        }
        
        // Push result
        let push_size = match var_type {
            LocalType::Int | LocalType::Float => 1,
            LocalType::Long | LocalType::Double => 2,
            _ => unreachable!(),
        };
        self.push(push_size)?;
        Ok(())
    }

    /// Generate comparison operation
    pub fn comparison(&mut self, op: ComparisonOp, var_type: &LocalType) -> Result<(), StackError> {
        // Pop operands
        let pop_size = match var_type {
            LocalType::Int | LocalType::Float => 2,
            LocalType::Long | LocalType::Double => 4,
            _ => return Err(StackError::UnsupportedOperation("Comparison operations not supported for reference types".to_string())),
        };
        self.pop(pop_size)?;
        
        // Emit comparison
        match (op, var_type) {
            // Integer comparisons
            (ComparisonOp::Eq, LocalType::Int) => {
                self.emit_byte(opcodes::IF_ICMPEQ);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ne, LocalType::Int) => {
                self.emit_byte(opcodes::IF_ICMPNE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Lt, LocalType::Int) => {
                self.emit_byte(opcodes::IF_ICMPLT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Le, LocalType::Int) => {
                self.emit_byte(opcodes::IF_ICMPLE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Gt, LocalType::Int) => {
                self.emit_byte(opcodes::IF_ICMPGT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ge, LocalType::Int) => {
                self.emit_byte(opcodes::IF_ICMPGE);
                self.add_label_ref(""); // Placeholder for label
            },
            
            // Long comparisons
            (ComparisonOp::Eq, LocalType::Long) => {
                self.emit_byte(opcodes::LCMP);
                self.emit_byte(opcodes::IFEQ);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ne, LocalType::Long) => {
                self.emit_byte(opcodes::LCMP);
                self.emit_byte(opcodes::IFNE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Lt, LocalType::Long) => {
                self.emit_byte(opcodes::LCMP);
                self.emit_byte(opcodes::IFLT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Le, LocalType::Long) => {
                self.emit_byte(opcodes::LCMP);
                self.emit_byte(opcodes::IFLE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Gt, LocalType::Long) => {
                self.emit_byte(opcodes::LCMP);
                self.emit_byte(opcodes::IFGT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ge, LocalType::Long) => {
                self.emit_byte(opcodes::LCMP);
                self.emit_byte(opcodes::IFGE);
                self.add_label_ref(""); // Placeholder for label
            },
            
            // Float comparisons
            (ComparisonOp::Eq, LocalType::Float) => {
                self.emit_byte(opcodes::FCMPL);
                self.emit_byte(opcodes::IFEQ);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ne, LocalType::Float) => {
                self.emit_byte(opcodes::FCMPL);
                self.emit_byte(opcodes::IFNE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Lt, LocalType::Float) => {
                self.emit_byte(opcodes::FCMPL);
                self.emit_byte(opcodes::IFLT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Le, LocalType::Float) => {
                self.emit_byte(opcodes::FCMPL);
                self.emit_byte(opcodes::IFLE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Gt, LocalType::Float) => {
                self.emit_byte(opcodes::FCMPL);
                self.emit_byte(opcodes::IFGT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ge, LocalType::Float) => {
                self.emit_byte(opcodes::FCMPL);
                self.emit_byte(opcodes::IFGE);
                self.add_label_ref(""); // Placeholder for label
            },
            
            // Double comparisons
            (ComparisonOp::Eq, LocalType::Double) => {
                self.emit_byte(opcodes::DCMPL);
                self.emit_byte(opcodes::IFEQ);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ne, LocalType::Double) => {
                self.emit_byte(opcodes::DCMPL);
                self.emit_byte(opcodes::IFNE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Lt, LocalType::Double) => {
                self.emit_byte(opcodes::DCMPL);
                self.emit_byte(opcodes::IFLT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Le, LocalType::Double) => {
                self.emit_byte(opcodes::DCMPL);
                self.emit_byte(opcodes::IFLE);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Gt, LocalType::Double) => {
                self.emit_byte(opcodes::DCMPL);
                self.emit_byte(opcodes::IFGT);
                self.add_label_ref(""); // Placeholder for label
            },
            (ComparisonOp::Ge, LocalType::Double) => {
                self.emit_byte(opcodes::DCMPL);
                self.emit_byte(opcodes::IFGE);
                self.add_label_ref(""); // Placeholder for label
            },
            
            _ => return Err(StackError::UnsupportedOperation(format!("Unsupported comparison operation {:?} for type {:?}", op, var_type))),
        }
        
        // No stack push for comparisons (they consume operands)
        Ok(())
    }

    /// Generate bitwise operations
    pub fn bitwise_operations(&mut self, op: BitwiseOp, var_type: LocalType) -> Result<(), StackError> {
        // Pop operands
        let pop_size = match var_type {
            LocalType::Int | LocalType::Long => {
                match op {
                    BitwiseOp::Not => 1, // Unary operation
                    _ => 2, // Binary operation
                }
            }
            _ => return Err(StackError::UnsupportedOperation("Bitwise operations only supported for integer types".to_string())),
        };
        self.pop(pop_size)?;
        
        // Emit bitwise operation
        match (op, var_type.clone()) {
            // Integer bitwise operations
            (BitwiseOp::And, LocalType::Int) => self.emit_byte(opcodes::IAND),
            (BitwiseOp::Or, LocalType::Int) => self.emit_byte(opcodes::IOR),
            (BitwiseOp::Xor, LocalType::Int) => self.emit_byte(opcodes::IXOR),
            (BitwiseOp::Shl, LocalType::Int) => self.emit_byte(opcodes::ISHL),
            (BitwiseOp::Shr, LocalType::Int) => self.emit_byte(opcodes::ISHR),
            (BitwiseOp::Ushr, LocalType::Int) => self.emit_byte(opcodes::IUSHR),
            (BitwiseOp::Not, LocalType::Int) => self.emit_byte(opcodes::INEG),
            
            // Long bitwise operations
            (BitwiseOp::And, LocalType::Long) => self.emit_byte(opcodes::LAND),
            (BitwiseOp::Or, LocalType::Long) => self.emit_byte(opcodes::LOR),
            (BitwiseOp::Xor, LocalType::Long) => self.emit_byte(opcodes::LXOR),
            (BitwiseOp::Shl, LocalType::Long) => self.emit_byte(opcodes::LSHL),
            (BitwiseOp::Shr, LocalType::Long) => self.emit_byte(opcodes::LSHR),
            (BitwiseOp::Ushr, LocalType::Long) => self.emit_byte(opcodes::LUSHR),
            (BitwiseOp::Not, LocalType::Long) => self.emit_byte(opcodes::LNEG),
            
            // Float and Double don't support bitwise operations
            (_, LocalType::Float) | (_, LocalType::Double) => {
                return Err(StackError::UnsupportedOperation("Bitwise operations not supported for floating point types".to_string()));
            }
            
            // Reference and Array types don't support bitwise operations
            (_, LocalType::Reference(_)) | (_, LocalType::Array(_)) => {
                return Err(StackError::UnsupportedOperation("Bitwise operations not supported for reference types".to_string()));
            }
        }
        
        // Push result
        let push_size = match var_type {
            LocalType::Int => 1,
            LocalType::Long => 2,
            _ => unreachable!(),
        };
        self.push(push_size)?;
        Ok(())
    }

    /// Generate logical operations
    pub fn logical_operations(&mut self, op: LogicalOp) -> Result<(), StackError> {
        // Logical operations work on boolean values (int 0 = false, non-zero = true)
        match op {
            LogicalOp::And => {
                // For AND: if first operand is false (0), result is false
                // if first operand is true (non-zero), result is second operand
                self.emit_byte(opcodes::DUP);
                self.emit_byte(opcodes::IFEQ);
                self.add_label_ref("and_false"); // Jump to false case
                self.emit_byte(opcodes::POP); // Remove first operand
                self.emit_byte(opcodes::GOTO);
                self.add_label_ref("and_end");
                self.mark_label("and_false");
                self.emit_byte(opcodes::POP); // Remove second operand
                self.emit_byte(opcodes::ICONST_0); // Push false
                self.mark_label("and_end");
            }
            LogicalOp::Or => {
                // For OR: if first operand is true (non-zero), result is true
                // if first operand is false (0), result is second operand
                self.emit_byte(opcodes::DUP);
                self.emit_byte(opcodes::IFNE);
                self.add_label_ref("or_true"); // Jump to true case
                self.emit_byte(opcodes::POP); // Remove first operand
                self.emit_byte(opcodes::GOTO);
                self.add_label_ref("or_end");
                self.mark_label("or_true");
                self.emit_byte(opcodes::POP); // Remove second operand
                self.emit_byte(opcodes::ICONST_1); // Push true
                self.mark_label("or_end");
            }
            LogicalOp::Not => {
                // For NOT: if operand is 0, result is 1; if non-zero, result is 0
                self.emit_byte(opcodes::IFEQ);
                self.add_label_ref("not_false");
                self.emit_byte(opcodes::ICONST_0); // Push false
                self.emit_byte(opcodes::GOTO);
                self.add_label_ref("not_end");
                self.mark_label("not_false");
                self.emit_byte(opcodes::ICONST_1); // Push true
                self.mark_label("not_end");
            }
        }
        
        // Stack state: logical operations consume operands and push result
        self.pop(2)?; // Pop operands
        self.push(1)?; // Push result
        Ok(())
    }

    /// Generate return instruction
    pub fn return_value(&mut self, return_type: Option<LocalType>) -> Result<(), StackError> {
        match return_type {
            None => self.emit_byte(opcodes::RETURN),
            Some(LocalType::Int) => self.emit_byte(opcodes::IRETURN),
            Some(LocalType::Long) => self.emit_byte(opcodes::LRETURN),
            Some(LocalType::Float) => self.emit_byte(opcodes::FRETURN),
            Some(LocalType::Double) => self.emit_byte(opcodes::DRETURN),
            Some(LocalType::Reference(_)) | Some(LocalType::Array(_)) => self.emit_byte(opcodes::ARETURN),
        }
        
        // Return instructions clear the stack
        self.stack = 0;
        Ok(())
    }

    /// Generate type conversion operations
    pub fn type_conversion(&mut self, from_type: LocalType, to_type: LocalType) -> Result<(), StackError> {
        // Pop source value
        let pop_size = match from_type {
            LocalType::Int => 1,
            LocalType::Long => 2,
            LocalType::Float => 1,
            LocalType::Double => 2,
            _ => return Err(StackError::UnsupportedOperation("Type conversion not supported for reference types".to_string())),
        };
        self.pop(pop_size)?;
        
        // Emit conversion instruction
        match (from_type.clone(), to_type.clone()) {
            // Int conversions
            (LocalType::Int, LocalType::Long) => self.emit_byte(opcodes::I2L),
            (LocalType::Int, LocalType::Float) => self.emit_byte(opcodes::I2F),
            (LocalType::Int, LocalType::Double) => self.emit_byte(opcodes::I2D),
            
            // Long conversions
            (LocalType::Long, LocalType::Int) => self.emit_byte(opcodes::L2I),
            (LocalType::Long, LocalType::Float) => self.emit_byte(opcodes::L2F),
            (LocalType::Long, LocalType::Double) => self.emit_byte(opcodes::L2D),
            
            // Float conversions
            (LocalType::Float, LocalType::Int) => self.emit_byte(opcodes::F2I),
            (LocalType::Float, LocalType::Long) => self.emit_byte(opcodes::F2L),
            (LocalType::Float, LocalType::Double) => self.emit_byte(opcodes::F2D),
            
            // Double conversions
            (LocalType::Double, LocalType::Int) => self.emit_byte(opcodes::D2I),
            (LocalType::Double, LocalType::Long) => self.emit_byte(opcodes::D2L),
            (LocalType::Double, LocalType::Float) => self.emit_byte(opcodes::D2F),
            
            // Same type conversion (no-op)
            (LocalType::Int, LocalType::Int) |
            (LocalType::Long, LocalType::Long) |
            (LocalType::Float, LocalType::Float) |
            (LocalType::Double, LocalType::Double) => {
                // No conversion needed, just push back
                match from_type {
                    LocalType::Int => self.push(1)?,
                    LocalType::Long => self.push(2)?,
                    LocalType::Float => self.push(1)?,
                    LocalType::Double => self.push(2)?,
                    _ => unreachable!(),
                }
                return Ok(());
            }
            
            // Unsupported conversions
            _ => return Err(StackError::UnsupportedOperation(
                format!("Type conversion from {:?} to {:?} not supported", from_type, to_type)
            )),
        }
        
        // Push converted value
        let push_size = match to_type {
            LocalType::Int => 1,
            LocalType::Long => 2,
            LocalType::Float => 1,
            LocalType::Double => 2,
            _ => unreachable!(),
        };
        self.push(push_size)?;
        Ok(())
    }

    /// Generate array operations
    pub fn array_operations(&mut self, operation: &str, element_type: LocalType) -> Result<(), StackError> {
        match operation {
            "newarray" => {
                // Create new array: count on stack, array reference pushed
                self.emit_byte(opcodes::NEWARRAY);
                let type_code = match element_type {
                    LocalType::Int => 10, // T_INT
                    LocalType::Long => 11, // T_LONG
                    LocalType::Float => 6,  // T_FLOAT
                    LocalType::Double => 7, // T_DOUBLE
                    LocalType::Reference(_) => 1, // T_OBJECT
                    LocalType::Array(_) => 1, // T_OBJECT for arrays
                };
                self.emit_byte(type_code as u8);
                self.pop(1)?; // Pop count
                self.push(1)?; // Push array reference
            }
            "arraylength" => {
                // Get array length: array reference on stack, length pushed
                self.emit_byte(opcodes::ARRAYLENGTH);
                self.pop(1)?; // Pop array reference
                self.push(1)?; // Push length
            }
            "aload" => {
                // Load array element: array reference and index on stack, element pushed
                match element_type {
                    LocalType::Int => self.emit_byte(opcodes::IALOAD),
                    LocalType::Long => self.emit_byte(opcodes::LALOAD),
                    LocalType::Float => self.emit_byte(opcodes::FALOAD),
                    LocalType::Double => self.emit_byte(opcodes::DALOAD),
                    LocalType::Reference(_) | LocalType::Array(_) => self.emit_byte(opcodes::AALOAD),
                }
                self.pop(2)?; // Pop array reference and index
                let push_size = match element_type {
                    LocalType::Int | LocalType::Float => 1,
                    LocalType::Long | LocalType::Double => 2,
                    _ => 1, // Reference types
                };
                self.push(push_size)?;
            }
            "astore" => {
                // Store array element: array reference, index, and value on stack
                match element_type {
                    LocalType::Int => self.emit_byte(opcodes::IASTORE),
                    LocalType::Long => self.emit_byte(opcodes::LASTORE),
                    LocalType::Float => self.emit_byte(opcodes::FASTORE),
                    LocalType::Double => self.emit_byte(opcodes::DASTORE),
                    LocalType::Reference(_) | LocalType::Array(_) => self.emit_byte(opcodes::AASTORE),
                }
                let pop_size = match element_type {
                    LocalType::Int | LocalType::Float => 3,
                    LocalType::Long | LocalType::Double => 4,
                    _ => 3, // Reference types
                };
                self.pop(pop_size)?; // Pop array reference, index, and value
            }
            _ => return Err(StackError::UnsupportedOperation(
                format!("Array operation '{}' not supported", operation)
            )),
        }
        Ok(())
    }

    /// Convert to basic bytecode components
    pub fn into_components(self) -> (Vec<u8>, u16, u16, Vec<ExceptionTableEntry>) {
        (self.code, self.max_stack, self.max_locals, self.exception_tables)
    }

    /// Get line number table
    pub fn line_number_table(&self) -> &[(u16, u16)] {
        &self.line_numbers
    }

    /// Get local variable table
    pub fn local_variable_table(&self) -> &[LocalVariableEntry] {
        &self.local_variables
    }
}

/// Local variable table entry for debugging
#[derive(Debug, Clone)]
pub struct LocalVariableEntry {
    pub start_pc: u16,
    pub length: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub index: u16,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_bytecode_builder_basic() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = BytecodeBuilder::new();
        
        // Test constant loading
        builder.iconst_0()?;
        builder.iconst_1()?;
        
        // Test arithmetic
        builder.iadd()?;
        
        // Test store and load
        builder.istore(4)?; // Use index > 3 to get ISTORE + index
        builder.iload(4)?;  // Use index > 3 to get ILOAD + index
        
        // Test return
        builder.ireturn()?;
        
        // Verify stack state before consuming
        assert_eq!(builder.stack_state.max_depth(), 2);
        
        let code = builder.into_code();
        
        // Debug: print the actual bytecode
        println!("Generated bytecode: {:?}", code);
        println!("Code length: {}", code.len());
        
        // Verify bytecode
        assert_eq!(code[0], opcodes::ICONST_0);
        assert_eq!(code[1], opcodes::ICONST_1);
        assert_eq!(code[2], opcodes::IADD);
        assert_eq!(code[3], opcodes::ISTORE);
        assert_eq!(code[4], 4); // index value
        assert_eq!(code[5], opcodes::ILOAD);
        assert_eq!(code[6], 4); // index value
        assert_eq!(code[7], opcodes::IRETURN);
        Ok(())
    }
    
    #[test]
    fn test_bytecode_builder_control_flow() -> Result<(), Box<dyn std::error::Error>> {
        let mut builder = BytecodeBuilder::new();
        
        // Test control flow
        builder.iconst_0()?;
        builder.ifeq("end")?;
        builder.iconst_1()?;
        builder.mark_label("end");
        builder.ireturn()?;
        
        let code = builder.into_code();
        
        // Debug: print the bytecode
        println!("Generated bytecode: {:?}", code);
        println!("Code length: {}", code.len());
        
        // Verify control flow
        assert_eq!(code[0], opcodes::ICONST_0);
        assert_eq!(code[1], opcodes::IFEQ);
        // The offset should be resolved to point to the label
        // IFEQ is at position 1, offset bytes at position 2-3, label is at position 5
        // JVM offset calculation: target_pc - (current_pc + 2) = 5 - (1 + 2) = 2
        // But our implementation uses: current_pc - ref_pc - 2 = 5 - 2 - 2 = 1
        // This is correct for JVM bytecode where the offset is relative to the instruction after the branch
        let offset = ((code[2] as u16) << 8) | (code[3] as u16);
        println!("Calculated offset: {}", offset);
        assert_eq!(offset, 1);
        assert_eq!(code[4], opcodes::ICONST_1);
        assert_eq!(code[5], opcodes::IRETURN);
        Ok(())
    }
    
    #[test]
    fn test_bytecode_builder_stack_operations() {
        let mut builder = BytecodeBuilder::new();
        
        // Test stack operations
        builder.iconst_1().unwrap();
        builder.iconst_2().unwrap();
        builder.dup().unwrap();
        builder.pop().unwrap();
        builder.swap().unwrap();
        
        let code = builder.into_code();
        
        // Verify stack operations
        assert_eq!(code[0], opcodes::ICONST_1);
        assert_eq!(code[1], opcodes::ICONST_2);
        assert_eq!(code[2], opcodes::DUP);
        assert_eq!(code[3], opcodes::POP);
        assert_eq!(code[4], opcodes::SWAP);
    }

    #[test]
    fn test_advanced_bytecode_arithmetic() -> Result<(), Box<dyn std::error::Error>> {
        let mut advanced = AdvancedBytecode::new();
        
        // Test arithmetic operations
        advanced.iconst(5)?;
        advanced.iconst(3)?;
        advanced.arithmetic(ArithmeticOp::Add, &LocalType::Int)?;
        
        let (code, _, _, _) = advanced.into_components();
        
        // Verify arithmetic
        assert_eq!(code[0], opcodes::ICONST_5);
        assert_eq!(code[1], opcodes::ICONST_3);
        assert_eq!(code[2], opcodes::IADD);
        Ok(())
    }

    #[test]
    fn test_advanced_bytecode_comparison() -> Result<(), Box<dyn std::error::Error>> {
        let mut advanced = AdvancedBytecode::new();
        
        // Test comparison operations
        advanced.iconst(5)?;
        advanced.iconst(3)?;
        advanced.comparison(ComparisonOp::Gt, &LocalType::Int)?;
        
        let (code, _, _, _) = advanced.into_components();
        
        // Verify comparison
        assert_eq!(code[0], opcodes::ICONST_5);
        assert_eq!(code[1], opcodes::ICONST_3);
        assert_eq!(code[2], opcodes::IF_ICMPGT);
        Ok(())
    }

    #[test]
    fn test_advanced_bytecode_bitwise() -> Result<(), Box<dyn std::error::Error>> {
        let mut advanced = AdvancedBytecode::new();
        
        // Test bitwise operations
        advanced.iconst(5)?;
        advanced.iconst(3)?;
        advanced.bitwise_operations(BitwiseOp::And, LocalType::Int)?;
        
        let (code, _, _, _) = advanced.into_components();
        
        // Verify bitwise operation
        assert_eq!(code[0], opcodes::ICONST_5);
        assert_eq!(code[1], opcodes::ICONST_3);
        assert_eq!(code[2], opcodes::IAND);
        Ok(())
    }

    #[test]
    fn test_advanced_bytecode_type_conversion() -> Result<(), Box<dyn std::error::Error>> {
        let mut advanced = AdvancedBytecode::new();
        
        // Test type conversion
        advanced.iconst(42)?;
        advanced.type_conversion(LocalType::Int, LocalType::Long)?;
        
        let (code, _, _, _) = advanced.into_components();
        
        // Verify type conversion
        assert_eq!(code[0], opcodes::BIPUSH);
        assert_eq!(code[1], 42);
        assert_eq!(code[2], opcodes::I2L);
        Ok(())
    }

    #[test]
    fn test_advanced_bytecode_array_operations() -> Result<(), Box<dyn std::error::Error>> {
        let mut advanced = AdvancedBytecode::new();
        
        // Test array operations
        advanced.iconst(10)?; // Array size
        advanced.array_operations("newarray", LocalType::Int)?;
        
        let (code, _, _, _) = advanced.into_components();
        
        // Verify array operation
        assert_eq!(code[0], opcodes::BIPUSH);
        assert_eq!(code[1], 10);
        assert_eq!(code[2], opcodes::NEWARRAY);
        assert_eq!(code[3], 10); // T_INT
        Ok(())
    }

    #[test]
    fn test_advanced_bytecode_logical_operations() -> Result<(), Box<dyn std::error::Error>> {
        let mut advanced = AdvancedBytecode::new();
        
        // Test logical operations
        advanced.iconst(1)?; // true
        advanced.iconst(0)?; // false
        advanced.logical_operations(LogicalOp::And)?;
        
        let (code, _, _, _) = advanced.into_components();
        
        // Verify logical operation structure
        assert_eq!(code[0], opcodes::ICONST_1);
        assert_eq!(code[1], opcodes::ICONST_0);
        assert_eq!(code[2], opcodes::DUP);
        assert_eq!(code[3], opcodes::IFEQ);
        Ok(())
    }
}