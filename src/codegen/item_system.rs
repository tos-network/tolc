use crate::ast::*;
use crate::codegen::opcodes;
use crate::codegen::chain::{Chain, StackState};
use crate::error::Result;

/// Enhanced Item system for representing addressable entities in bytecode
/// 
/// This module implements sophisticated Item abstraction that represents
/// different kinds of values and their efficient bytecode generation patterns:
/// - LocalItem: local variables with iinc optimization
/// - StackItem: values on the operand stack
/// - MemberItem: field and method references
/// - StaticItem: static field references
/// - IndexedItem: array elements
/// - ImmediateItem: compile-time constants
#[derive(Debug, Clone)]
pub enum Item {
    /// Local variable item (javac LocalItem)
    Local(LocalItem),
    /// Stack item (javac StackItem)
    Stack(StackItem),
    /// Instance field item (javac MemberItem)
    Member(MemberItem),
    /// Static field item (javac StaticItem)
    Static(StaticItem),
    /// Array element item (javac IndexedItem)
    Indexed(IndexedItem),
    /// Immediate constant item (javac ImmediateItem)
    Immediate(ImmediateItem),
    /// Conditional item (javac CondItem)
    Conditional(ConditionalItem),
    /// Assignment item (javac AssignItem)
    Assignment(AssignmentItem),
    /// Self item (this/super) (javac SelfItem)
    SelfRef(SelfItem),
    /// Void item (javac voidItem)
    Void(VoidItem),
    /// Chain item for chained method calls (javac ChainItem)
    Chain(ChainItem),
    /// Select item for field/method selection (javac SelectItem)
    Select(SelectItem),
    /// Apply item for method application (javac ApplyItem)
    Apply(ApplyItem),
}

impl Item {
    /// Load this item onto the stack (javac Item.load())
    pub fn load(&self) -> Result<Vec<u8>> {
        match self {
            Item::Local(item) => item.load(),
            Item::Stack(item) => item.load(),
            Item::Member(item) => item.load(),
            Item::Static(item) => item.load(),
            Item::Indexed(item) => item.load(),
            Item::Immediate(item) => item.load(),
            Item::Conditional(item) => item.load(),
            Item::Assignment(item) => item.load(),
            Item::SelfRef(item) => item.load(),
            Item::Void(item) => item.load(),
            Item::Chain(item) => item.load(),
            Item::Select(item) => item.load(),
            Item::Apply(item) => item.load(),
        }
    }
    
    /// Store top of stack into this item (javac Item.store())
    pub fn store(&self) -> Result<Vec<u8>> {
        match self {
            Item::Local(item) => item.store(),
            Item::Member(item) => item.store(),
            Item::Static(item) => item.store(),
            Item::Indexed(item) => item.store(),
            Item::Assignment(item) => item.store(),
            Item::Chain(item) => item.store(),
            Item::Select(item) => item.store(),
            Item::Apply(item) => item.store(),
            _ => Err(crate::error::Error::codegen_error(&format!("Store operation not supported for {:?}", self))),
        }
    }
    
    /// Duplicate this item on stack (javac Item.duplicate())
    pub fn duplicate(&self) -> Result<Vec<u8>> {
        match self {
            Item::Stack(item) => item.duplicate(),
            Item::Indexed(_) => Ok(vec![opcodes::DUP2]), // Array index needs dup2
            Item::Assignment(item) => item.duplicate(),
            _ => Err(crate::error::Error::codegen_error(&format!("Duplicate operation not supported for {:?}", self))),
        }
    }
    
    /// Drop this item from stack (javac Item.drop())
    pub fn drop(&self) -> Result<Vec<u8>> {
        match self {
            Item::Stack(item) => item.drop(),
            Item::Indexed(_) => Ok(vec![opcodes::POP2]), // Array index needs pop2
            _ => Err(crate::error::Error::codegen_error(&format!("Drop operation not supported for {:?}", self))),
        }
    }
    
    /// Get the width of this item (javac Item.width())
    pub fn width(&self) -> u8 {
        match self {
            Item::Local(item) => item.width(),
            Item::Stack(item) => item.width(),
            Item::Member(_) => 1, // Object references are 1 slot
            Item::Static(_) => 1, // Static references are 1 slot  
            Item::Indexed(_) => 1, // Array elements are 1 slot
            Item::Immediate(item) => item.width(),
            Item::Conditional(_) => 1, // Conditional items have width 1
            Item::Assignment(_) => 1, // Assignment result is 1 slot
            Item::SelfRef(item) => item.width(),
            Item::Void(item) => item.width(),
            Item::Chain(item) => item.width(),
            Item::Select(item) => item.width(),
            Item::Apply(item) => item.width(),
        }
    }
    
    /// Convert to conditional item (javac Item.mkCond())
    pub fn mk_cond(&self) -> Result<Item> {
        // Convert any item to a conditional item by comparing with zero
        match self {
            Item::Conditional(_) => Ok(self.clone()),
            _ => {
                // Generate comparison with zero
                let true_jumps = crate::codegen::chain::Chain::new(0, None, crate::codegen::chain::StackState::new());
                let false_jumps = crate::codegen::chain::Chain::new(0, None, crate::codegen::chain::StackState::new());
                Ok(Item::Conditional(ConditionalItem::new(
                    opcodes::IFNE, // Jump if not equal to zero
                    true_jumps,
                    false_jumps,
                )))
            }
        }
    }
    
    /// Convert to assignment item (javac Item.mkAssign())
    pub fn mk_assign(&self, needs_dup: bool) -> Item {
        Item::Assignment(AssignmentItem {
            target: Box::new(self.clone()),
            needs_dup,
        })
    }
    
    /// Get type code of this item
    pub fn type_code(&self) -> TypeCode {
        match self {
            Item::Local(item) => item.type_code,
            Item::Stack(item) => item.type_code,
            Item::Member(_) => TypeCode::Object, // Member access returns object
            Item::Static(_) => TypeCode::Object, // Static access returns object
            Item::Indexed(item) => item.element_type_code, // Array element type
            Item::Immediate(item) => item.type_code,
            Item::Conditional(_) => TypeCode::Int, // Conditional items are boolean (int)
            Item::Assignment(_) => TypeCode::Object, // Assignment result type
            Item::SelfRef(item) => item.type_code,
            Item::Void(item) => item.type_code,
            Item::Chain(item) => item.type_code,
            Item::Select(item) => item.type_code,
            Item::Apply(item) => item.type_code,
        }
    }
}

/// Local variable item with iinc optimization support
#[derive(Debug, Clone)]
pub struct LocalItem {
    /// Local variable index
    pub index: u16,
    /// Variable type code (INT, LONG, FLOAT, DOUBLE, OBJECT)
    pub type_code: TypeCode,
    /// Variable type for type checking
    pub var_type: String,
    /// Whether this variable can use iinc instruction
    pub can_use_iinc: bool,
}

/// Stack item representing a value on the operand stack
#[derive(Debug, Clone)]
pub struct StackItem {
    /// Type code of the value on stack
    pub type_code: TypeCode,
    /// Stack depth where this item resides
    pub stack_depth: u16,
}

/// Member item for instance fields and methods
#[derive(Debug, Clone)]
pub struct MemberItem {
    /// Owner class internal name
    pub owner: String,
    /// Field/method name
    pub name: String,
    /// Field/method descriptor
    pub descriptor: String,
    /// Whether this is a field (true) or method (false)
    pub is_field: bool,
    /// Constant pool index
    pub cp_index: u16,
}

/// Static item for static fields and methods
#[derive(Debug, Clone)]
pub struct StaticItem {
    /// Owner class internal name
    pub owner: String,
    /// Field/method name
    pub name: String,
    /// Field/method descriptor
    pub descriptor: String,
    /// Whether this is a field (true) or method (false)
    pub is_field: bool,
    /// Constant pool index
    pub cp_index: u16,
}

/// Indexed item for array elements
#[derive(Debug, Clone)]
pub struct IndexedItem {
    /// Array type code
    pub array_type_code: TypeCode,
    /// Element type code
    pub element_type_code: TypeCode,
}

/// Immediate constant item
#[derive(Debug, Clone)]
pub struct ImmediateItem {
    /// Constant value
    pub value: ConstantValue,
    /// Type code
    pub type_code: TypeCode,
}

/// Conditional item for boolean expressions
#[derive(Debug, Clone)]
pub struct ConditionalItem {
    /// Jump opcode for true condition
    pub opcode: u8,
    /// True jump chain
    pub true_jumps: Option<Chain>,
    /// False jump chain
    pub false_jumps: Option<Chain>,
}

impl ConditionalItem {
    /// Create a new ConditionalItem
    pub fn new(opcode: u8, true_jumps: Chain, false_jumps: Chain) -> Self {
        Self {
            opcode,
            true_jumps: Some(true_jumps),
            false_jumps: Some(false_jumps),
        }
    }
    
    /// Load conditional item (placeholder)
    pub fn load(&self) -> Result<Vec<u8>> {
        // Conditional items don't directly load values
        Ok(vec![])
    }
}

/// Assignment item for assignment expressions
#[derive(Debug, Clone)]
pub struct AssignmentItem {
    /// Target item being assigned to
    pub target: Box<Item>,
    /// Whether this assignment needs duplication
    pub needs_dup: bool,
}

impl AssignmentItem {
    /// Load assignment item (javac AssignItem.load)
    pub fn load(&self) -> Result<Vec<u8>> {
        // Load target, duplicate if needed, then store
        let mut bytecode = self.target.load()?;
        if self.needs_dup {
            bytecode.extend_from_slice(&self.target.duplicate()?);
        }
        bytecode.extend_from_slice(&self.target.store()?);
        Ok(bytecode)
    }
    
    /// Store to assignment item (not supported)
    pub fn store(&self) -> Result<Vec<u8>> {
        Err(crate::error::Error::codegen_error("Cannot store to assignment item"))
    }
    
    /// Duplicate assignment item
    pub fn duplicate(&self) -> Result<Vec<u8>> {
        // Load and duplicate the result
        let mut bytecode = self.load()?;
        bytecode.extend_from_slice(&vec![opcodes::DUP]);
        Ok(bytecode)
    }
}

/// Type codes for JVM types (javac-style)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeCode {
    Void = 0,
    Byte = 1,
    Short = 2,
    Char = 3,
    Int = 4,
    Long = 5,
    Float = 6,
    Double = 7,
    Object = 8,
    Array = 9,
}

/// Constant values for immediate items
#[derive(Debug, Clone)]
pub enum ConstantValue {
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Null,
}


impl LocalItem {
    /// Create a new local item
    pub fn new(index: u16, type_code: TypeCode, var_type: String) -> Self {
        let can_use_iinc = matches!(type_code, TypeCode::Int | TypeCode::Byte | TypeCode::Short | TypeCode::Char);
        Self {
            index,
            type_code,
            var_type,
            can_use_iinc,
        }
    }
    
    /// Load local variable onto stack (javac LocalItem.load)
    pub fn load(&self) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        match self.type_code {
            TypeCode::Int | TypeCode::Byte | TypeCode::Short | TypeCode::Char => {
                if self.index <= 3 {
                    // Use iload_0, iload_1, iload_2, iload_3
                    bytecode.push(opcodes::ILOAD_0 + self.index as u8);
                } else if self.index <= 255 {
                    bytecode.push(opcodes::ILOAD);
                    bytecode.push(self.index as u8);
                } else {
                    // Wide instruction
                    bytecode.push(opcodes::WIDE);
                    bytecode.push(opcodes::ILOAD);
                    bytecode.extend_from_slice(&self.index.to_be_bytes());
                }
            }
            TypeCode::Long => {
                if self.index <= 3 {
                    bytecode.push(opcodes::LLOAD_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::LLOAD);
                    bytecode.push(self.index as u8);
                }
            }
            TypeCode::Float => {
                if self.index <= 3 {
                    bytecode.push(opcodes::FLOAD_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::FLOAD);
                    bytecode.push(self.index as u8);
                }
            }
            TypeCode::Double => {
                if self.index <= 3 {
                    bytecode.push(opcodes::DLOAD_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::DLOAD);
                    bytecode.push(self.index as u8);
                }
            }
            TypeCode::Object | TypeCode::Array => {
                if self.index <= 3 {
                    bytecode.push(opcodes::ALOAD_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::ALOAD);
                    bytecode.push(self.index as u8);
                }
            }
            _ => return Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Invalid type code for local variable"
            ))),
        }
        
        Ok(bytecode)
    }
    
    /// Store value from stack into local variable (javac LocalItem.store)
    pub fn store(&self) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        match self.type_code {
            TypeCode::Int | TypeCode::Byte | TypeCode::Short | TypeCode::Char => {
                if self.index <= 3 {
                    bytecode.push(opcodes::ISTORE_0 + self.index as u8);
                } else if self.index <= 255 {
                    bytecode.push(opcodes::ISTORE);
                    bytecode.push(self.index as u8);
                } else {
                    bytecode.push(opcodes::WIDE);
                    bytecode.push(opcodes::ISTORE);
                    bytecode.extend_from_slice(&self.index.to_be_bytes());
                }
            }
            TypeCode::Long => {
                if self.index <= 3 {
                    bytecode.push(opcodes::LSTORE_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::LSTORE);
                    bytecode.push(self.index as u8);
                }
            }
            TypeCode::Float => {
                if self.index <= 3 {
                    bytecode.push(opcodes::FSTORE_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::FSTORE);
                    bytecode.push(self.index as u8);
                }
            }
            TypeCode::Double => {
                if self.index <= 3 {
                    bytecode.push(opcodes::DSTORE_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::DSTORE);
                    bytecode.push(self.index as u8);
                }
            }
            TypeCode::Object | TypeCode::Array => {
                if self.index <= 3 {
                    bytecode.push(opcodes::ASTORE_0 + self.index as u8);
                } else {
                    bytecode.push(opcodes::ASTORE);
                    bytecode.push(self.index as u8);
                }
            }
            _ => return Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Invalid type code for local variable"
            ))),
        }
        
        Ok(bytecode)
    }
    
    /// Duplicate local variable (load twice)
    pub fn duplicate(&self) -> Result<Vec<u8>> {
        let mut bytecode = self.load()?;
        bytecode.extend_from_slice(&self.load()?);
        Ok(bytecode)
    }
    
    /// Increment local variable using iinc (javac LocalItem.incr)
    pub fn increment(&self, amount: i16) -> Result<Vec<u8>> {
        if !self.can_use_iinc {
            return Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Cannot use iinc on this variable type"
            )));
        }
        
        let mut bytecode = Vec::new();
        
        if self.index <= 255 && amount >= -128 && amount <= 127 {
            // Standard iinc
            bytecode.push(opcodes::IINC);
            bytecode.push(self.index as u8);
            bytecode.push(amount as i8 as u8);
        } else {
            // Wide iinc
            bytecode.push(opcodes::WIDE);
            bytecode.push(opcodes::IINC);
            bytecode.extend_from_slice(&self.index.to_be_bytes());
            bytecode.extend_from_slice(&amount.to_be_bytes());
        }
        
        Ok(bytecode)
    }
    
    /// Get stack width of this local variable
    pub fn width(&self) -> u8 {
        match self.type_code {
            TypeCode::Long | TypeCode::Double => 2,
            _ => 1,
        }
    }
}

impl StackItem {
    /// Create a new stack item
    pub fn new(type_code: TypeCode, stack_depth: u16) -> Self {
        Self {
            type_code,
            stack_depth,
        }
    }
    
    /// Load stack item (no-op since it's already on stack)
    pub fn load(&self) -> Result<Vec<u8>> {
        Ok(Vec::new()) // Already on stack
    }
    
    /// Duplicate stack item
    pub fn duplicate(&self) -> Result<Vec<u8>> {
        let opcode = match self.type_code {
            TypeCode::Long | TypeCode::Double => opcodes::DUP2,
            _ => opcodes::DUP,
        };
        Ok(vec![opcode])
    }
    
    /// Drop stack item
    pub fn drop(&self) -> Result<Vec<u8>> {
        let opcode = match self.type_code {
            TypeCode::Long | TypeCode::Double => opcodes::POP2,
            _ => opcodes::POP,
        };
        Ok(vec![opcode])
    }
    
    /// Get stack width
    pub fn width(&self) -> u8 {
        match self.type_code {
            TypeCode::Long | TypeCode::Double => 2,
            _ => 1,
        }
    }
}

impl ImmediateItem {
    /// Create a new immediate item
    pub fn new(value: ConstantValue, type_code: TypeCode) -> Self {
        Self { value, type_code }
    }
    
    /// Load immediate constant
    pub fn load(&self) -> Result<Vec<u8>> {
        match &self.value {
            ConstantValue::Int(value) => {
                match *value {
                    -1 => Ok(vec![opcodes::ICONST_M1]),
                    0 => Ok(vec![opcodes::ICONST_0]),
                    1 => Ok(vec![opcodes::ICONST_1]),
                    2 => Ok(vec![opcodes::ICONST_2]),
                    3 => Ok(vec![opcodes::ICONST_3]),
                    4 => Ok(vec![opcodes::ICONST_4]),
                    5 => Ok(vec![opcodes::ICONST_5]),
                    v if v >= -128 && v <= 127 => {
                        Ok(vec![opcodes::BIPUSH, v as i8 as u8])
                    }
                    v if v >= -32768 && v <= 32767 => {
                        let mut bytecode = vec![opcodes::SIPUSH];
                        bytecode.extend_from_slice(&(v as i16).to_be_bytes());
                        Ok(bytecode)
                    }
                    _ => {
                        // Use LDC for larger constants
                        Ok(vec![opcodes::LDC, 0]) // Index will be patched
                    }
                }
            }
            ConstantValue::Long(value) => {
                match *value {
                    0 => Ok(vec![opcodes::LCONST_0]),
                    1 => Ok(vec![opcodes::LCONST_1]),
                    _ => Ok(vec![opcodes::LDC2_W, 0, 0]), // Index will be patched
                }
            }
            ConstantValue::Float(value) => {
                if *value == 0.0 {
                    Ok(vec![opcodes::FCONST_0])
                } else if *value == 1.0 {
                    Ok(vec![opcodes::FCONST_1])
                } else if *value == 2.0 {
                    Ok(vec![opcodes::FCONST_2])
                } else {
                    Ok(vec![opcodes::LDC, 0]) // Index will be patched
                }
            }
            ConstantValue::Double(value) => {
                if *value == 0.0 {
                    Ok(vec![opcodes::DCONST_0])
                } else if *value == 1.0 {
                    Ok(vec![opcodes::DCONST_1])
                } else {
                    Ok(vec![opcodes::LDC2_W, 0, 0]) // Index will be patched
                }
            }
            ConstantValue::String(_) => {
                Ok(vec![opcodes::LDC, 0]) // Index will be patched
            }
            ConstantValue::Null => {
                Ok(vec![opcodes::ACONST_NULL])
            }
        }
    }
    
    /// Duplicate immediate constant (load twice)
    pub fn duplicate(&self) -> Result<Vec<u8>> {
        let mut bytecode = self.load()?;
        bytecode.extend_from_slice(&self.load()?);
        Ok(bytecode)
    }
    
    /// Get stack width
    pub fn width(&self) -> u8 {
        match self.type_code {
            TypeCode::Long | TypeCode::Double => 2,
            _ => 1,
        }
    }
}

impl MemberItem {
    /// Load instance field
    pub fn load(&self) -> Result<Vec<u8>> {
        if self.is_field {
            Ok(vec![opcodes::GETFIELD, 0, 0]) // Index will be patched
        } else {
            Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Cannot load method as value"
            )))
        }
    }
    
    /// Store into instance field
    pub fn store(&self) -> Result<Vec<u8>> {
        if self.is_field {
            Ok(vec![opcodes::PUTFIELD, 0, 0]) // Index will be patched
        } else {
            Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Cannot store into method"
            )))
        }
    }
}

impl StaticItem {
    /// Load static field
    pub fn load(&self) -> Result<Vec<u8>> {
        if self.is_field {
            Ok(vec![opcodes::GETSTATIC, 0, 0]) // Index will be patched
        } else {
            Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Cannot load method as value"
            )))
        }
    }
    
    /// Store into static field
    pub fn store(&self) -> Result<Vec<u8>> {
        if self.is_field {
            Ok(vec![opcodes::PUTSTATIC, 0, 0]) // Index will be patched
        } else {
            Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Cannot store into method"
            )))
        }
    }
}

impl IndexedItem {
    /// Load array element
    pub fn load(&self) -> Result<Vec<u8>> {
        let opcode = match self.element_type_code {
            TypeCode::Byte => opcodes::BALOAD,
            TypeCode::Short => opcodes::SALOAD,
            TypeCode::Char => opcodes::CALOAD,
            TypeCode::Int => opcodes::IALOAD,
            TypeCode::Long => opcodes::LALOAD,
            TypeCode::Float => opcodes::FALOAD,
            TypeCode::Double => opcodes::DALOAD,
            TypeCode::Object | TypeCode::Array => opcodes::AALOAD,
            _ => return Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Invalid array element type"
            ))),
        };
        Ok(vec![opcode])
    }
    
    /// Store into array element
    pub fn store(&self) -> Result<Vec<u8>> {
        let opcode = match self.element_type_code {
            TypeCode::Byte => opcodes::BASTORE,
            TypeCode::Short => opcodes::SASTORE,
            TypeCode::Char => opcodes::CASTORE,
            TypeCode::Int => opcodes::IASTORE,
            TypeCode::Long => opcodes::LASTORE,
            TypeCode::Float => opcodes::FASTORE,
            TypeCode::Double => opcodes::DASTORE,
            TypeCode::Object | TypeCode::Array => opcodes::AASTORE,
            _ => return Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Invalid array element type"
            ))),
        };
        Ok(vec![opcode])
    }
}

/// Item factory for creating different types of items (javac Items equivalent)
pub struct ItemFactory;

impl ItemFactory {
    /// Create a local variable item
    pub fn make_local_item(index: u16, type_code: TypeCode, var_type: String) -> Item {
        Item::Local(LocalItem::new(index, type_code, var_type))
    }
    
    /// Create a stack item
    pub fn make_stack_item(type_code: TypeCode, stack_depth: u16) -> Item {
        Item::Stack(StackItem::new(type_code, stack_depth))
    }
    
    /// Create an immediate constant item
    pub fn make_immediate_item(value: ConstantValue, type_code: TypeCode) -> Item {
        Item::Immediate(ImmediateItem::new(value, type_code))
    }
    
    /// Create a conditional item
    pub fn make_conditional_item(opcode: u8, true_jumps: Option<Chain>, false_jumps: Option<Chain>) -> Item {
        Item::Conditional(ConditionalItem {
            opcode,
            true_jumps,
            false_jumps,
        })
    }
    
    /// Create an assignment item
    pub fn make_assignment_item(target: Item, needs_dup: bool) -> Item {
        Item::Assignment(AssignmentItem {
            target: Box::new(target),
            needs_dup,
        })
    }
    
    /// Create 'this' item (javac makeThisItem)
    pub fn make_this_item() -> Item {
        Item::SelfRef(SelfItem::new(false))
    }
    
    /// Create 'super' item (javac makeSuperItem)
    pub fn make_super_item() -> Item {
        Item::SelfRef(SelfItem::new(true))
    }
    
    /// Create void item (javac makeVoidItem)
    pub fn make_void_item() -> Item {
        Item::Void(VoidItem::new())
    }
    

}

/// Self item representing 'this' or 'super' (javac SelfItem)
#[derive(Debug, Clone)]
pub struct SelfItem {
    /// Whether this represents 'super' (true) or 'this' (false)
    pub is_super: bool,
    /// Type code (always OBJECT)
    pub type_code: TypeCode,
}

impl SelfItem {
    /// Create a new SelfItem
    pub fn new(is_super: bool) -> Self {
        Self {
            is_super,
            type_code: TypeCode::Object,
        }
    }
    
    /// Load 'this' or 'super' onto stack (javac-style)
    pub fn load(&self) -> Result<Vec<u8>> {
        // Always load from local variable 0 (this/super)
        Ok(vec![opcodes::ALOAD_0])
    }
    
    /// Get width (always 1 for object references)
    pub fn width(&self) -> u8 {
        1
    }
}

/// Void item representing void type (javac voidItem)
#[derive(Debug, Clone)]
pub struct VoidItem {
    /// Type code (always VOID)
    pub type_code: TypeCode,
}

impl VoidItem {
    /// Create a new VoidItem
    pub fn new() -> Self {
        Self {
            type_code: TypeCode::Void,
        }
    }
    
    /// Load void (no-op, javac-style)
    pub fn load(&self) -> Result<Vec<u8>> {
        // Void items don't generate any bytecode
        Ok(vec![])
    }
    
    /// Get width (always 0 for void)
    pub fn width(&self) -> u8 {
        0
    }
}



/// Chain item for chained method calls (javac ChainItem)
#[derive(Debug, Clone)]
pub struct ChainItem {
    /// Base item being chained
    pub base_item: Box<Item>,
    /// Chain of method calls
    pub chain: Vec<String>,
    /// Result type of the chain
    pub type_code: TypeCode,
}

impl ChainItem {
    /// Create a new ChainItem
    pub fn new(base_item: Item, chain: Vec<String>, type_code: TypeCode) -> Self {
        Self {
            base_item: Box::new(base_item),
            chain,
            type_code,
        }
    }
    
    /// Load the chained result (javac-style)
    pub fn load(&self) -> Result<Vec<u8>> {
        let mut bytecode = self.base_item.load()?;
        
        // Generate method calls for the chain
        for _method_name in &self.chain {
            // This is a simplified implementation
            // In a full implementation, we'd need method resolution
            bytecode.extend_from_slice(&[opcodes::INVOKEVIRTUAL]);
            // Add method reference (placeholder)
            bytecode.extend_from_slice(&[0, 1]); // Placeholder method ref
        }
        
        Ok(bytecode)
    }
    
    /// Store is not supported for chain items
    pub fn store(&self) -> Result<Vec<u8>> {
        Err(crate::error::Error::codegen_error("Store operation not supported for ChainItem"))
    }
    
    /// Get width based on result type
    pub fn width(&self) -> u8 {
        match self.type_code {
            TypeCode::Long | TypeCode::Double => 2,
            TypeCode::Void => 0,
            _ => 1,
        }
    }
}

/// Select item for field/method selection (javac SelectItem)
#[derive(Debug, Clone)]
pub struct SelectItem {
    /// Selected member name
    pub member_name: String,
    /// Owner type
    pub owner_type: String,
    /// Member type
    pub member_type: String,
    /// Whether this is a static member
    pub is_static: bool,
    /// Type code for the member
    pub type_code: TypeCode,
}

impl SelectItem {
    /// Create a new SelectItem
    pub fn new(member_name: String, owner_type: String, member_type: String, is_static: bool, type_code: TypeCode) -> Self {
        Self {
            member_name,
            owner_type,
            member_type,
            is_static,
            type_code,
        }
    }
    
    /// Load the selected member (javac-style)
    pub fn load(&self) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        if self.is_static {
            // Static field access
            bytecode.push(opcodes::GETSTATIC);
        } else {
            // Instance field access (assumes receiver is on stack)
            bytecode.push(opcodes::GETFIELD);
        }
        
        // Add field reference (placeholder)
        bytecode.extend_from_slice(&[0, 1]); // Placeholder field ref
        
        Ok(bytecode)
    }
    
    /// Store into the selected member (javac-style)
    pub fn store(&self) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        if self.is_static {
            // Static field store
            bytecode.push(opcodes::PUTSTATIC);
        } else {
            // Instance field store
            bytecode.push(opcodes::PUTFIELD);
        }
        
        // Add field reference (placeholder)
        bytecode.extend_from_slice(&[0, 1]); // Placeholder field ref
        
        Ok(bytecode)
    }
    
    /// Get width based on member type
    pub fn width(&self) -> u8 {
        match self.type_code {
            TypeCode::Long | TypeCode::Double => 2,
            TypeCode::Void => 0,
            _ => 1,
        }
    }
}

/// Apply item for method application (javac ApplyItem)
#[derive(Debug, Clone)]
pub struct ApplyItem {
    /// Method name
    pub method_name: String,
    /// Method descriptor
    pub method_descriptor: String,
    /// Owner type
    pub owner_type: String,
    /// Whether this is a static method
    pub is_static: bool,
    /// Whether this is an interface method
    pub is_interface: bool,
    /// Return type code
    pub type_code: TypeCode,
}

impl ApplyItem {
    /// Create a new ApplyItem
    pub fn new(method_name: String, method_descriptor: String, owner_type: String, is_static: bool, is_interface: bool, type_code: TypeCode) -> Self {
        Self {
            method_name,
            method_descriptor,
            owner_type,
            is_static,
            is_interface,
            type_code,
        }
    }
    
    /// Load (invoke) the method (javac-style)
    pub fn load(&self) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        if self.is_static {
            bytecode.push(opcodes::INVOKESTATIC);
        } else if self.is_interface {
            bytecode.push(opcodes::INVOKEINTERFACE);
        } else {
            bytecode.push(opcodes::INVOKEVIRTUAL);
        }
        
        // Add method reference (placeholder)
        bytecode.extend_from_slice(&[0, 1]); // Placeholder method ref
        
        if self.is_interface {
            // Interface methods need argument count
            bytecode.push(1); // Placeholder arg count
            bytecode.push(0); // Reserved
        }
        
        Ok(bytecode)
    }
    
    /// Store is not supported for method application
    pub fn store(&self) -> Result<Vec<u8>> {
        Err(crate::error::Error::codegen_error("Store operation not supported for ApplyItem"))
    }
    
    /// Get width based on return type
    pub fn width(&self) -> u8 {
        match self.type_code {
            TypeCode::Long | TypeCode::Double => 2,
            TypeCode::Void => 0,
            _ => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_local_item_creation() {
        let item = LocalItem::new(1, TypeCode::Int, "int".to_string());
        assert_eq!(item.index, 1);
        assert_eq!(item.type_code, TypeCode::Int);
        assert!(item.can_use_iinc);
    }
    
    #[test]
    fn test_local_item_load() {
        let item = LocalItem::new(0, TypeCode::Int, "int".to_string());
        let bytecode = item.load().unwrap();
        assert_eq!(bytecode[0], opcodes::ILOAD_0);
        
        let item = LocalItem::new(5, TypeCode::Int, "int".to_string());
        let bytecode = item.load().unwrap();
        assert_eq!(bytecode[0], opcodes::ILOAD);
        assert_eq!(bytecode[1], 5);
    }
    
    #[test]
    fn test_immediate_item_constants() {
        let item = ImmediateItem::new(ConstantValue::Int(0), TypeCode::Int);
        let bytecode = item.load().unwrap();
        assert_eq!(bytecode[0], opcodes::ICONST_0);
        
        let item = ImmediateItem::new(ConstantValue::Int(100), TypeCode::Int);
        let bytecode = item.load().unwrap();
        assert_eq!(bytecode[0], opcodes::BIPUSH);
        assert_eq!(bytecode[1], 100);
    }
    
    #[test]
    fn test_stack_item_operations() {
        let item = StackItem::new(TypeCode::Int, 1);
        
        let load_bytecode = item.load().unwrap();
        assert!(load_bytecode.is_empty()); // Already on stack
        
        let dup_bytecode = item.duplicate().unwrap();
        assert_eq!(dup_bytecode[0], opcodes::DUP);
        
        let drop_bytecode = item.drop().unwrap();
        assert_eq!(drop_bytecode[0], opcodes::POP);
    }
    
    #[test]
    fn test_item_width() {
        let int_item = LocalItem::new(0, TypeCode::Int, "int".to_string());
        assert_eq!(int_item.width(), 1);
        
        let long_item = LocalItem::new(0, TypeCode::Long, "long".to_string());
        assert_eq!(long_item.width(), 2);
    }
    
    #[test]
    fn test_iinc_optimization() {
        let item = LocalItem::new(1, TypeCode::Int, "int".to_string());
        let bytecode = item.increment(1).unwrap();
        assert_eq!(bytecode[0], opcodes::IINC);
        assert_eq!(bytecode[1], 1);
        assert_eq!(bytecode[2], 1);
    }
    
    #[test]
    fn test_item_factory() {
        let item = ItemFactory::make_local_item(1, TypeCode::Int, "int".to_string());
        assert!(matches!(item, Item::Local(_)));
        
        let item = ItemFactory::make_immediate_item(ConstantValue::Int(42), TypeCode::Int);
        assert!(matches!(item, Item::Immediate(_)));
    }
}
