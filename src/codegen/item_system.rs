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

/// Assignment item for assignment expressions
#[derive(Debug, Clone)]
pub struct AssignmentItem {
    /// Target item being assigned to
    pub target: Box<Item>,
    /// Whether this assignment needs duplication
    pub needs_dup: bool,
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

impl Item {
    /// Load this item onto the operand stack (javac Item.load equivalent)
    pub fn load(&self) -> Result<Vec<u8>> {
        match self {
            Item::Local(local) => local.load(),
            Item::Stack(stack) => stack.load(),
            Item::Member(member) => member.load(),
            Item::Static(static_item) => static_item.load(),
            Item::Indexed(indexed) => indexed.load(),
            Item::Immediate(immediate) => immediate.load(),
            Item::Conditional(conditional) => conditional.load(),
            Item::Assignment(assignment) => assignment.load(),
        }
    }
    
    /// Store a value from stack into this item (javac Item.store equivalent)
    pub fn store(&self) -> Result<Vec<u8>> {
        match self {
            Item::Local(local) => local.store(),
            Item::Member(member) => member.store(),
            Item::Static(static_item) => static_item.store(),
            Item::Indexed(indexed) => indexed.store(),
            _ => Err(crate::error::Error::from(std::io::Error::new(
                std::io::ErrorKind::InvalidInput, 
                "Cannot store into this item type"
            ))),
        }
    }
    
    /// Duplicate this item on the stack (javac Item.duplicate equivalent)
    pub fn duplicate(&self) -> Result<Vec<u8>> {
        match self {
            Item::Local(local) => local.duplicate(),
            Item::Stack(stack) => stack.duplicate(),
            Item::Immediate(immediate) => immediate.duplicate(),
            _ => {
                // For complex items, load twice
                let mut bytecode = self.load()?;
                bytecode.extend_from_slice(&self.load()?);
                Ok(bytecode)
            }
        }
    }
    
    /// Drop this item from the stack (javac Item.drop equivalent)
    pub fn drop(&self) -> Result<Vec<u8>> {
        match self {
            Item::Stack(stack) => stack.drop(),
            _ => Ok(vec![opcodes::POP]), // Default drop behavior
        }
    }
    
    /// Get the width of this item on the stack (javac Item.width equivalent)
    pub fn width(&self) -> u8 {
        match self {
            Item::Local(local) => local.width(),
            Item::Stack(stack) => stack.width(),
            Item::Immediate(immediate) => immediate.width(),
            _ => 1, // Default width
        }
    }
    
    /// Get the type code of this item
    pub fn type_code(&self) -> TypeCode {
        match self {
            Item::Local(local) => local.type_code,
            Item::Stack(stack) => stack.type_code,
            Item::Immediate(immediate) => immediate.type_code,
            _ => TypeCode::Object, // Default type
        }
    }
    
    /// Convert this item to a conditional item (javac Item.mkCond equivalent)
    pub fn make_conditional(&self) -> Result<Item> {
        match self {
            Item::Conditional(cond) => Ok(Item::Conditional(cond.clone())),
            _ => {
                // Convert to conditional by testing against zero
                Ok(Item::Conditional(ConditionalItem {
                    opcode: opcodes::IFNE,
                    true_jumps: None,
                    false_jumps: None,
                }))
            }
        }
    }
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

impl ConditionalItem {
    /// Load conditional as boolean value
    pub fn load(&self) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        // Pattern: condition_check, ifeq false_label, iconst_1, goto end_label, false_label: iconst_0, end_label:
        bytecode.push(self.opcode);
        bytecode.extend_from_slice(&[0, 7]); // Jump to false_label
        bytecode.push(opcodes::ICONST_1);
        bytecode.push(opcodes::GOTO);
        bytecode.extend_from_slice(&[0, 4]); // Jump to end_label
        bytecode.push(opcodes::ICONST_0);
        
        Ok(bytecode)
    }
}

impl AssignmentItem {
    /// Load assignment result
    pub fn load(&self) -> Result<Vec<u8>> {
        if self.needs_dup {
            // Duplicate the assigned value
            let mut bytecode = self.target.load()?;
            bytecode.extend_from_slice(&self.target.duplicate()?);
            Ok(bytecode)
        } else {
            self.target.load()
        }
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
