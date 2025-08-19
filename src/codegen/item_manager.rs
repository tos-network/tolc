/// Item management system (javac-style)
/// Implements javac's sophisticated Item-based code generation system

use crate::codegen::opcodes;
use crate::ast::TypeRef;

/// Item types representing different kinds of values (javac Items pattern)
#[derive(Debug, Clone)]
pub enum Item {
    /// Value on the operand stack
    Stack(StackItem),
    /// Local variable
    Local(LocalItem),
    /// Immediate constant value
    Immediate(ImmediateItem),
    /// Class member (field or method)
    Member(MemberItem),
    /// Array element
    Indexed(IndexedItem),
    /// Conditional item for boolean expressions
    Condition(ConditionItem),
    /// Assignment target
    Assignment(AssignmentItem),
    /// 'this' reference
    This(ThisItem),
    /// 'super' reference
    Super(SuperItem),
    /// Static member
    Static(StaticItem),
}

/// Stack item - value currently on operand stack (javac StackItem)
#[derive(Debug, Clone)]
pub struct StackItem {
    pub type_ref: TypeRef,
    pub stack_depth: usize,
}

/// Local variable item (javac LocalItem)
#[derive(Debug, Clone)]
pub struct LocalItem {
    pub var_index: u16,
    pub type_ref: TypeRef,
    pub is_parameter: bool,
}

/// Immediate constant item (javac ImmediateItem)
#[derive(Debug, Clone)]
pub struct ImmediateItem {
    pub value: ConstantValue,
    pub type_ref: TypeRef,
}

/// Member item - field or method (javac MemberItem)
#[derive(Debug, Clone)]
pub struct MemberItem {
    pub owner_type: TypeRef,
    pub member_name: String,
    pub member_type: TypeRef,
    pub is_static: bool,
    pub is_private: bool,
}

/// Indexed item - array element (javac IndexedItem)
#[derive(Debug, Clone)]
pub struct IndexedItem {
    pub array_type: TypeRef,
    pub element_type: TypeRef,
}

/// Condition item for boolean expressions (javac CondItem)
#[derive(Debug, Clone)]
pub struct ConditionItem {
    pub opcode: u8,
    pub true_jumps: Vec<u16>,
    pub false_jumps: Vec<u16>,
    pub is_constant: Option<bool>,
}

/// Assignment item (javac AssignItem)
#[derive(Debug, Clone)]
pub struct AssignmentItem {
    pub target: Box<Item>,
}

/// This item (javac ThisItem)
#[derive(Debug, Clone)]
pub struct ThisItem {
    pub type_ref: TypeRef,
}

/// Super item (javac SuperItem)
#[derive(Debug, Clone)]
pub struct SuperItem {
    pub type_ref: TypeRef,
}

/// Static item (javac StaticItem)
#[derive(Debug, Clone)]
pub struct StaticItem {
    pub owner_type: TypeRef,
    pub member_name: String,
    pub member_type: TypeRef,
}

/// Constant value types
#[derive(Debug, Clone)]
pub enum ConstantValue {
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Boolean(bool),
    Null,
}

/// Item manager - factory for creating items (javac Items class)
pub struct ItemManager;

impl ItemManager {
    /// Create stack item (javac makeStackItem)
    pub fn make_stack_item(type_ref: TypeRef) -> Item {
        Item::Stack(StackItem {
            type_ref,
            stack_depth: 1,
        })
    }
    
    /// Create local item (javac makeLocalItem)
    pub fn make_local_item(var_index: u16, type_ref: TypeRef, is_parameter: bool) -> Item {
        Item::Local(LocalItem {
            var_index,
            type_ref,
            is_parameter,
        })
    }
    
    /// Create immediate item (javac makeImmediateItem)
    pub fn make_immediate_item(value: ConstantValue, type_ref: TypeRef) -> Item {
        Item::Immediate(ImmediateItem {
            value,
            type_ref,
        })
    }
    
    /// Create member item (javac makeMemberItem)
    pub fn make_member_item(
        owner_type: TypeRef,
        member_name: String,
        member_type: TypeRef,
        is_static: bool,
        is_private: bool,
    ) -> Item {
        Item::Member(MemberItem {
            owner_type,
            member_name,
            member_type,
            is_static,
            is_private,
        })
    }
    
    /// Create indexed item (javac makeIndexedItem)
    pub fn make_indexed_item(array_type: TypeRef, element_type: TypeRef) -> Item {
        Item::Indexed(IndexedItem {
            array_type,
            element_type,
        })
    }
    
    /// Create condition item (javac makeCondItem)
    pub fn make_condition_item(opcode: u8) -> Item {
        Item::Condition(ConditionItem {
            opcode,
            true_jumps: Vec::new(),
            false_jumps: Vec::new(),
            is_constant: None,
        })
    }
    
    /// Create assignment item (javac makeAssignItem)
    pub fn make_assignment_item(target: Item) -> Item {
        Item::Assignment(AssignmentItem {
            target: Box::new(target),
        })
    }
    
    /// Create this item (javac makeThisItem)
    pub fn make_this_item(type_ref: TypeRef) -> Item {
        Item::This(ThisItem { type_ref })
    }
    
    /// Create super item (javac makeSuperItem)
    pub fn make_super_item(type_ref: TypeRef) -> Item {
        Item::Super(SuperItem { type_ref })
    }
    
    /// Create static item (javac makeStaticItem)
    pub fn make_static_item(owner_type: TypeRef, member_name: String, member_type: TypeRef) -> Item {
        Item::Static(StaticItem {
            owner_type,
            member_name,
            member_type,
        })
    }
}

/// Item operations (javac Item methods)
impl Item {
    /// Load item onto stack (javac Item.load)
    pub fn load(&self) -> Vec<u8> {
        match self {
            Item::Stack(_) => {
                // Already on stack
                Vec::new()
            }
            
            Item::Local(local) => {
                Self::generate_local_load(local)
            }
            
            Item::Immediate(immediate) => {
                Self::generate_immediate_load(immediate)
            }
            
            Item::Member(member) => {
                Self::generate_member_load(member)
            }
            
            Item::Indexed(indexed) => {
                Self::generate_indexed_load(indexed)
            }
            
            Item::This(_) => {
                vec![opcodes::ALOAD_0]
            }
            
            Item::Super(_) => {
                vec![opcodes::ALOAD_0] // Super is also loaded as this
            }
            
            Item::Static(static_item) => {
                Self::generate_static_load(static_item)
            }
            
            _ => Vec::new(), // Other items don't have simple load operations
        }
    }
    
    /// Store value into item (javac Item.store)
    pub fn store(&self) -> Vec<u8> {
        match self {
            Item::Local(local) => {
                Self::generate_local_store(local)
            }
            
            Item::Member(member) => {
                Self::generate_member_store(member)
            }
            
            Item::Indexed(indexed) => {
                Self::generate_indexed_store(indexed)
            }
            
            Item::Static(static_item) => {
                Self::generate_static_store(static_item)
            }
            
            _ => Vec::new(), // Other items don't support store
        }
    }
    
    /// Duplicate item on stack (javac Item.duplicate)
    pub fn duplicate(&self) -> Vec<u8> {
        match self {
            Item::Stack(stack) => {
                if Self::is_wide_type(&stack.type_ref) {
                    vec![opcodes::DUP2]
                } else {
                    vec![opcodes::DUP]
                }
            }
            
            Item::Local(_local) => {
                // Load twice
                let mut bytecode = self.load();
                bytecode.extend_from_slice(&self.load());
                bytecode
            }
            
            _ => {
                // For other items, load and duplicate
                let mut bytecode = self.load();
                if self.is_wide() {
                    bytecode.push(opcodes::DUP2);
                } else {
                    bytecode.push(opcodes::DUP);
                }
                bytecode
            }
        }
    }
    
    /// Drop item from stack (javac Item.drop)
    pub fn drop(&self) -> Vec<u8> {
        if self.is_wide() {
            vec![opcodes::POP2]
        } else {
            vec![opcodes::POP]
        }
    }
    
    /// Convert to condition item (javac Item.mkCond)
    pub fn mk_cond(&self) -> Item {
        match self {
            Item::Condition(_) => self.clone(),
            _ => {
                // Convert to boolean condition
                ItemManager::make_condition_item(opcodes::IFNE)
            }
        }
    }
    
    /// Coerce item to target type (javac Item.coerce)
    pub fn coerce(&self, target_type: &TypeRef) -> Vec<u8> {
        let source_type = self.get_type();
        
        if Self::types_equal(&source_type, target_type) {
            return Vec::new(); // No coercion needed
        }
        
        Self::generate_type_coercion(&source_type, target_type)
    }
    
    /// Check if item represents a wide type (long/double)
    pub fn is_wide(&self) -> bool {
        let type_ref = self.get_type();
        Self::is_wide_type(&type_ref)
    }
    
    /// Get the type of this item
    pub fn get_type(&self) -> TypeRef {
        match self {
            Item::Stack(stack) => stack.type_ref.clone(),
            Item::Local(local) => local.type_ref.clone(),
            Item::Immediate(immediate) => immediate.type_ref.clone(),
            Item::Member(member) => member.member_type.clone(),
            Item::Indexed(indexed) => indexed.element_type.clone(),
            Item::This(this) => this.type_ref.clone(),
            Item::Super(super_item) => super_item.type_ref.clone(),
            Item::Static(static_item) => static_item.member_type.clone(),
            _ => TypeRef {
                name: "void".to_string(),
                type_args: Vec::new(),
                annotations: Vec::new(),
                array_dims: 0,
                span: crate::ast::Span::from_to(0, 0, 0, 0),
            },
        }
    }
    
    /// Generate local variable load bytecode
    fn generate_local_load(local: &LocalItem) -> Vec<u8> {
        match local.type_ref.name.as_str() {
            "int" | "boolean" | "byte" | "char" | "short" => {
                match local.var_index {
                    0 => vec![opcodes::ILOAD_0],
                    1 => vec![opcodes::ILOAD_1],
                    2 => vec![opcodes::ILOAD_2],
                    3 => vec![opcodes::ILOAD_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::ILOAD, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::ILOAD, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            "long" => {
                match local.var_index {
                    0 => vec![opcodes::LLOAD_0],
                    1 => vec![opcodes::LLOAD_1],
                    2 => vec![opcodes::LLOAD_2],
                    3 => vec![opcodes::LLOAD_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::LLOAD, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::LLOAD, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            "float" => {
                match local.var_index {
                    0 => vec![opcodes::FLOAD_0],
                    1 => vec![opcodes::FLOAD_1],
                    2 => vec![opcodes::FLOAD_2],
                    3 => vec![opcodes::FLOAD_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::FLOAD, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::FLOAD, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            "double" => {
                match local.var_index {
                    0 => vec![opcodes::DLOAD_0],
                    1 => vec![opcodes::DLOAD_1],
                    2 => vec![opcodes::DLOAD_2],
                    3 => vec![opcodes::DLOAD_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::DLOAD, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::DLOAD, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            _ => {
                // Reference type
                match local.var_index {
                    0 => vec![opcodes::ALOAD_0],
                    1 => vec![opcodes::ALOAD_1],
                    2 => vec![opcodes::ALOAD_2],
                    3 => vec![opcodes::ALOAD_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::ALOAD, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::ALOAD, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
        }
    }
    
    /// Generate local variable store bytecode
    fn generate_local_store(local: &LocalItem) -> Vec<u8> {
        match local.type_ref.name.as_str() {
            "int" | "boolean" | "byte" | "char" | "short" => {
                match local.var_index {
                    0 => vec![opcodes::ISTORE_0],
                    1 => vec![opcodes::ISTORE_1],
                    2 => vec![opcodes::ISTORE_2],
                    3 => vec![opcodes::ISTORE_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::ISTORE, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::ISTORE, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            "long" => {
                match local.var_index {
                    0 => vec![opcodes::LSTORE_0],
                    1 => vec![opcodes::LSTORE_1],
                    2 => vec![opcodes::LSTORE_2],
                    3 => vec![opcodes::LSTORE_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::LSTORE, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::LSTORE, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            "float" => {
                match local.var_index {
                    0 => vec![opcodes::FSTORE_0],
                    1 => vec![opcodes::FSTORE_1],
                    2 => vec![opcodes::FSTORE_2],
                    3 => vec![opcodes::FSTORE_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::FSTORE, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::FSTORE, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            "double" => {
                match local.var_index {
                    0 => vec![opcodes::DSTORE_0],
                    1 => vec![opcodes::DSTORE_1],
                    2 => vec![opcodes::DSTORE_2],
                    3 => vec![opcodes::DSTORE_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::DSTORE, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::DSTORE, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
            _ => {
                // Reference type
                match local.var_index {
                    0 => vec![opcodes::ASTORE_0],
                    1 => vec![opcodes::ASTORE_1],
                    2 => vec![opcodes::ASTORE_2],
                    3 => vec![opcodes::ASTORE_3],
                    _ => {
                        if local.var_index <= 255 {
                            vec![opcodes::ASTORE, local.var_index as u8]
                        } else {
                            vec![opcodes::WIDE, opcodes::ASTORE, 
                                 (local.var_index >> 8) as u8, 
                                 (local.var_index & 0xFF) as u8]
                        }
                    }
                }
            }
        }
    }
    
    /// Generate immediate value load bytecode (javac ImmediateItem.load)
    fn generate_immediate_load(immediate: &ImmediateItem) -> Vec<u8> {
        match &immediate.value {
            ConstantValue::Integer(i) => {
                match *i {
                    -1 => vec![opcodes::ICONST_M1],
                    0 => vec![opcodes::ICONST_0],
                    1 => vec![opcodes::ICONST_1],
                    2 => vec![opcodes::ICONST_2],
                    3 => vec![opcodes::ICONST_3],
                    4 => vec![opcodes::ICONST_4],
                    5 => vec![opcodes::ICONST_5],
                    -128..=127 => vec![opcodes::BIPUSH, *i as u8],
                    -32768..=32767 => {
                        let bytes = (*i as i16).to_be_bytes();
                        vec![opcodes::SIPUSH, bytes[0], bytes[1]]
                    }
                    _ => {
                        // Use ldc
                        vec![opcodes::LDC, 1] // Placeholder constant pool index
                    }
                }
            }
            
            ConstantValue::Long(l) => {
                match *l {
                    0 => vec![opcodes::LCONST_0],
                    1 => vec![opcodes::LCONST_1],
                    _ => vec![opcodes::LDC2_W, 0, 1], // Placeholder constant pool index
                }
            }
            
            ConstantValue::Float(f) => {
                if *f == 0.0 {
                    vec![opcodes::FCONST_0]
                } else if *f == 1.0 {
                    vec![opcodes::FCONST_1]
                } else if *f == 2.0 {
                    vec![opcodes::FCONST_2]
                } else {
                    vec![opcodes::LDC, 1] // Placeholder constant pool index
                }
            }
            
            ConstantValue::Double(d) => {
                if *d == 0.0 {
                    vec![opcodes::DCONST_0]
                } else if *d == 1.0 {
                    vec![opcodes::DCONST_1]
                } else {
                    vec![opcodes::LDC2_W, 0, 1] // Placeholder constant pool index
                }
            }
            
            ConstantValue::String(_) => {
                vec![opcodes::LDC, 1] // Placeholder constant pool index
            }
            
            ConstantValue::Boolean(b) => {
                if *b {
                    vec![opcodes::ICONST_1]
                } else {
                    vec![opcodes::ICONST_0]
                }
            }
            
            ConstantValue::Null => {
                vec![opcodes::ACONST_NULL]
            }
        }
    }
    
    /// Generate member access bytecode (placeholder)
    fn generate_member_load(_member: &MemberItem) -> Vec<u8> {
        vec![opcodes::GETFIELD, 0, 1] // Placeholder
    }
    
    /// Generate member store bytecode (placeholder)
    fn generate_member_store(_member: &MemberItem) -> Vec<u8> {
        vec![opcodes::PUTFIELD, 0, 1] // Placeholder
    }
    
    /// Generate indexed load bytecode
    fn generate_indexed_load(indexed: &IndexedItem) -> Vec<u8> {
        match indexed.element_type.name.as_str() {
            "int" => vec![opcodes::IALOAD],
            "long" => vec![opcodes::LALOAD],
            "float" => vec![opcodes::FALOAD],
            "double" => vec![opcodes::DALOAD],
            "boolean" | "byte" => vec![opcodes::BALOAD],
            "char" => vec![opcodes::CALOAD],
            "short" => vec![opcodes::SALOAD],
            _ => vec![opcodes::AALOAD], // Reference type
        }
    }
    
    /// Generate indexed store bytecode
    fn generate_indexed_store(indexed: &IndexedItem) -> Vec<u8> {
        match indexed.element_type.name.as_str() {
            "int" => vec![opcodes::IASTORE],
            "long" => vec![opcodes::LASTORE],
            "float" => vec![opcodes::FASTORE],
            "double" => vec![opcodes::DASTORE],
            "boolean" | "byte" => vec![opcodes::BASTORE],
            "char" => vec![opcodes::CASTORE],
            "short" => vec![opcodes::SASTORE],
            _ => vec![opcodes::AASTORE], // Reference type
        }
    }
    
    /// Generate static member load bytecode (placeholder)
    fn generate_static_load(_static_item: &StaticItem) -> Vec<u8> {
        vec![opcodes::GETSTATIC, 0, 1] // Placeholder
    }
    
    /// Generate static member store bytecode (placeholder)
    fn generate_static_store(_static_item: &StaticItem) -> Vec<u8> {
        vec![opcodes::PUTSTATIC, 0, 1] // Placeholder
    }
    
    /// Check if type is wide (long/double)
    fn is_wide_type(type_ref: &TypeRef) -> bool {
        matches!(type_ref.name.as_str(), "long" | "double")
    }
    
    /// Check if two types are equal
    fn types_equal(type1: &TypeRef, type2: &TypeRef) -> bool {
        type1.name == type2.name && type1.array_dims == type2.array_dims
    }
    
    /// Generate type coercion bytecode
    fn generate_type_coercion(source: &TypeRef, target: &TypeRef) -> Vec<u8> {
        // Simplified type coercion - would need full type system
        match (source.name.as_str(), target.name.as_str()) {
            ("int", "long") => vec![opcodes::I2L],
            ("int", "float") => vec![opcodes::I2F],
            ("int", "double") => vec![opcodes::I2D],
            ("long", "int") => vec![opcodes::L2I],
            ("long", "float") => vec![opcodes::L2F],
            ("long", "double") => vec![opcodes::L2D],
            ("float", "int") => vec![opcodes::F2I],
            ("float", "long") => vec![opcodes::F2L],
            ("float", "double") => vec![opcodes::F2D],
            ("double", "int") => vec![opcodes::D2I],
            ("double", "long") => vec![opcodes::D2L],
            ("double", "float") => vec![opcodes::D2F],
            _ => Vec::new(), // No coercion needed or not supported
        }
    }
}

/// Local variable increment optimization (javac LocalItem.incr)
impl LocalItem {
    /// Generate increment bytecode (javac LocalItem.incr)
    pub fn incr(&self, increment: i16) -> Vec<u8> {
        if increment >= -128 && increment <= 127 && self.var_index <= 255 {
            // Use iinc instruction
            vec![opcodes::IINC, self.var_index as u8, increment as u8]
        } else if self.var_index > 255 || increment < -128 || increment > 127 {
            // Use wide iinc
            vec![
                opcodes::WIDE,
                opcodes::IINC,
                (self.var_index >> 8) as u8,
                (self.var_index & 0xFF) as u8,
                (increment >> 8) as u8,
                (increment & 0xFF) as u8,
            ]
        } else {
            // Fallback to load, increment, store
            let mut bytecode = Vec::new();
            bytecode.extend_from_slice(&Item::Local(self.clone()).load());
            
            if increment > 0 {
                if increment <= 5 {
                    bytecode.push(opcodes::ICONST_0 + increment as u8);
                } else if increment <= 127 {
                    bytecode.push(opcodes::BIPUSH);
                    bytecode.push(increment as u8);
                } else {
                    bytecode.push(opcodes::SIPUSH);
                    bytecode.extend_from_slice(&increment.to_be_bytes());
                }
                bytecode.push(opcodes::IADD);
            } else {
                let abs_increment = (-increment) as u8;
                if abs_increment <= 5 {
                    bytecode.push(opcodes::ICONST_0 + abs_increment);
                } else if abs_increment <= 127 {
                    bytecode.push(opcodes::BIPUSH);
                    bytecode.push(abs_increment);
                } else {
                    bytecode.push(opcodes::SIPUSH);
                    bytecode.extend_from_slice(&(-increment).to_be_bytes());
                }
                bytecode.push(opcodes::ISUB);
            }
            
            bytecode.extend_from_slice(&Item::Local(self.clone()).store());
            bytecode
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    
    fn create_int_type() -> TypeRef {
        TypeRef {
            name: "int".to_string(),
            type_args: Vec::new(),
            annotations: Vec::new(),
            array_dims: 0,
            span: Span::from_to(0, 0, 0, 0),
        }
    }
    
    #[test]
    fn test_local_item_load() {
        let local = LocalItem {
            var_index: 1,
            type_ref: create_int_type(),
            is_parameter: false,
        };
        
        let item = Item::Local(local);
        let bytecode = item.load();
        
        assert_eq!(bytecode, vec![opcodes::ILOAD_1]);
    }
    
    #[test]
    fn test_immediate_item_load() {
        let immediate = ImmediateItem {
            value: ConstantValue::Integer(42),
            type_ref: create_int_type(),
        };
        
        let item = Item::Immediate(immediate);
        let bytecode = item.load();
        
        assert_eq!(bytecode, vec![opcodes::BIPUSH, 42]);
    }
    
    #[test]
    fn test_local_item_incr() {
        let local = LocalItem {
            var_index: 5,
            type_ref: create_int_type(),
            is_parameter: false,
        };
        
        let bytecode = local.incr(1);
        assert_eq!(bytecode, vec![opcodes::IINC, 5, 1]);
        
        let bytecode = local.incr(-10);
        assert_eq!(bytecode, vec![opcodes::IINC, 5, 246]); // -10 as u8 = 246
    }
    
    #[test]
    fn test_wide_type_detection() {
        let long_type = TypeRef {
            name: "long".to_string(),
            type_args: Vec::new(),
            annotations: Vec::new(),
            array_dims: 0,
            span: Span::from_to(0, 0, 0, 0),
        };
        
        assert!(Item::is_wide_type(&long_type));
        assert!(!Item::is_wide_type(&create_int_type()));
    }
    
    #[test]
    fn test_item_manager_factory_methods() {
        let stack_item = ItemManager::make_stack_item(create_int_type());
        assert!(matches!(stack_item, Item::Stack(_)));
        
        let local_item = ItemManager::make_local_item(1, create_int_type(), false);
        assert!(matches!(local_item, Item::Local(_)));
        
        let immediate_item = ItemManager::make_immediate_item(
            ConstantValue::Integer(42),
            create_int_type(),
        );
        assert!(matches!(immediate_item, Item::Immediate(_)));
    }
}
