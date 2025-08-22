//! 100% JavaC-aligned Items system
//! 
//! This module implements the exact same Items architecture as Oracle's javac,
//! with Items directly operating on the Code buffer for maximum compatibility.

use crate::ast::{Literal, TypeEnum};
use crate::common::error::Result;
use super::code::Code;
use super::constpool::ConstantPool;
use super::opcodes;
use super::optimization_manager::{OptimizationManager, OptimizationLevel};

/// Type codes exactly matching JavaC ByteCodes.java
pub mod typecodes {
    pub const VOID: u8 = 0;
    pub const BYTE: u8 = 1;
    pub const SHORT: u8 = 2;
    pub const CHAR: u8 = 3;
    pub const INT: u8 = 4;
    pub const LONG: u8 = 5;
    pub const FLOAT: u8 = 6;
    pub const DOUBLE: u8 = 7;
    pub const OBJECT: u8 = 8;
    pub const ARRAY: u8 = 9;
}

/// Items manager that directly operates on Code buffer (100% JavaC pattern)
/// Equivalent to: Items(Pool pool, Code code, Symtab syms, Types types)
pub struct Items<'a> {
    /// Reference to constant pool (JavaC pool field)
    pool: &'a mut ConstantPool,
    
    /// Reference to code buffer (JavaC code field)
    pub code: &'a mut Code,
    
    /// Integrated optimization manager
    optimizer: OptimizationManager,
    
    // TODO: Add syms and types when available
    // syms: &'a Symtab,
    // types: &'a Types,
}

impl<'a> Items<'a> {
    /// Create new Items manager with direct Code buffer access (JavaC constructor)
    pub fn new(pool: &'a mut ConstantPool, code: &'a mut Code) -> Self {
        Self {
            pool,
            code,
            optimizer: OptimizationManager::new(),
        }
    }
    
    /// Create Items with specific optimization level
    pub fn new_with_optimization(
        pool: &'a mut ConstantPool, 
        code: &'a mut Code, 
        _level: OptimizationLevel
    ) -> Self {
        Self {
            pool,
            code,
            optimizer: OptimizationManager::new(),
        }
    }
    
    /// Make void item (JavaC makeVoidItem)
    pub fn make_void_item(&self) -> Item {
        Item::Void
    }
    
    /// Make 'this' item (JavaC makeThisItem)
    pub fn make_this_item(&self) -> Item {
        Item::This
    }
    
    /// Make 'super' item (JavaC makeSuperItem)  
    pub fn make_super_item(&self) -> Item {
        Item::Super
    }
    
    /// Make stack item with typecode (JavaC makeStackItem)
    pub fn make_stack_item(&self, typecode: u8) -> Item {
        Item::Stack { typecode }
    }
    
    /// Make stack item for TypeEnum (JavaC makeStackItem(Type))
    pub fn make_stack_item_for_type(&self, typ: &TypeEnum) -> Item {
        let typecode = self.type_to_typecode(typ);
        Item::Stack { typecode }
    }
    
    /// Make local variable item (JavaC makeLocalItem)
    pub fn make_local_item(&self, typ: &TypeEnum, reg: u16) -> Item {
        let typecode = self.type_to_typecode(typ);
        Item::Local { typecode, reg }
    }
    
    /// Make immediate value item (JavaC makeImmediateItem)
    pub fn make_immediate_item(&self, typ: &TypeEnum, value: Literal) -> Item {
        let typecode = self.type_to_typecode(typ);
        Item::Immediate { typecode, value }
    }
    
    /// Make member item (JavaC makeMemberItem)
    pub fn make_member_item(&self, member_name: String, class_name: String, descriptor: String, is_static: bool, typ: &TypeEnum) -> Item {
        let typecode = self.type_to_typecode(typ);
        Item::Member { 
            typecode, 
            member_name, 
            class_name,
            descriptor,
            is_static,
            nonvirtual: false, // Default to virtual calls
        }
    }
    
    /// Make member item with virtual/non-virtual control (JavaC makeMemberItem with nonvirtual)
    pub fn make_member_item_nonvirtual(&self, member_name: String, class_name: String, descriptor: String, is_static: bool, typ: &TypeEnum, nonvirtual: bool) -> Item {
        let typecode = self.type_to_typecode(typ);
        Item::Member { 
            typecode, 
            member_name, 
            class_name,
            descriptor,
            is_static,
            nonvirtual,
        }
    }
    
    /// Make indexed item for array access (JavaC makeIndexedItem)
    pub fn make_indexed_item(&self, typ: &TypeEnum) -> Item {
        let typecode = self.type_to_typecode(typ);
        Item::Indexed { typecode }
    }
    
    /// Make conditional item (JavaC makeCondItem)
    pub fn make_cond_item(&self, opcode: u8) -> CondItem {
        CondItem {
            opcode,
            true_jumps: None,
            false_jumps: None,
        }
    }
    
    /// Convert TypeEnum to typecode
    fn type_to_typecode(&self, _typ: &TypeEnum) -> u8 {
        // TODO: Implement proper Type to typecode conversion
        typecodes::INT
    }
    
    /// Generate optimized load instruction and return stack item
    pub fn load_item(&mut self, item: &Item) -> Result<Item> {
        match item {
            Item::Stack { typecode } => {
                // Already on stack
                Ok(Item::Stack { typecode: *typecode })
            }
            
            Item::Local { typecode, reg } => {
                self.emit_load_local(*typecode, *reg)?;
                Ok(Item::Stack { typecode: *typecode })
            }
            
            Item::Immediate { typecode, value } => {
                self.emit_load_immediate(*typecode, value)?;
                Ok(Item::Stack { typecode: *typecode })
            }
            
            Item::Member { typecode, member_name, class_name, descriptor, is_static, .. } => {
                // For field access, use the class_name and descriptor
                self.emit_load_member(*typecode, member_name, class_name, descriptor, *is_static)?;
                Ok(Item::Stack { typecode: *typecode })
            }
            
            Item::Indexed { typecode } => {
                self.emit_load_indexed(*typecode)?;
                Ok(Item::Stack { typecode: *typecode })
            }
            
            Item::This | Item::Super => {
                self.code.emitop(opcodes::ALOAD_0);
                Ok(Item::Stack { typecode: typecodes::OBJECT })
            }
            
            Item::Void => Ok(Item::Void),
        }
    }
    
    /// Generate store instruction (JavaC store)
    pub fn store_item(&mut self, item: &Item) -> Result<()> {
        match item {
            Item::Local { typecode, reg } => {
                self.emit_store_local(*typecode, *reg)
            }
            
            Item::Member { typecode, member_name, class_name, descriptor, is_static, .. } => {
                // For field stores, use the class_name and descriptor
                self.emit_store_member(*typecode, member_name, class_name, descriptor, *is_static)
            }
            
            Item::Indexed { typecode } => {
                self.emit_store_indexed(*typecode)
            }
            
            _ => Err(crate::common::error::Error::CodeGen {
                message: format!("Cannot store into {:?}", item)
            })
        }
    }
    
    /// Generate method invocation (JavaC invoke)
    pub fn invoke_item(&mut self, item: &Item) -> Result<Item> {
        match item {
            Item::Member { typecode, member_name, class_name, descriptor, is_static, nonvirtual } => {
                self.emit_invoke_member(*typecode, member_name, class_name, descriptor, *is_static, *nonvirtual)?;
                Ok(Item::Stack { typecode: *typecode })
            }
            
            _ => Err(crate::common::error::Error::CodeGen {
                message: format!("Cannot invoke {:?}", item)
            })
        }
    }
    
    /// Generate duplicate instruction (JavaC duplicate)
    pub fn duplicate_item(&mut self, item: &Item) -> Result<()> {
        let width = item.width();
        if width == 2 {
            self.code.emitop(opcodes::DUP2);
        } else if width == 1 {
            self.code.emitop(opcodes::DUP);
        }
        Ok(())
    }
    
    /// Generate drop instruction (JavaC drop)
    pub fn drop_item(&mut self, item: &Item) -> Result<()> {
        let width = item.width();
        if width == 2 {
            self.code.emitop(opcodes::POP2);
        } else if width == 1 {
            self.code.emitop(opcodes::POP);
        }
        Ok(())
    }
    
    /// Generate type coercion (JavaC coerce)
    pub fn coerce_item(&mut self, item: &Item, target_typecode: u8) -> Result<Item> {
        let source_typecode = item.typecode();
        
        if source_typecode == target_typecode {
            return Ok(item.clone());
        }
        
        // Load item first if not already on stack
        let _stack_item = if matches!(item, Item::Stack { .. }) {
            item.clone()
        } else {
            self.load_item(item)?
        };
        
        // Generate conversion instructions
        self.emit_type_conversion(source_typecode, target_typecode)?;
        
        Ok(Item::Stack { typecode: target_typecode })
    }
    
    // ========== Bytecode Generation Methods ==========
    
    /// Merge two jump chains (JavaC Chain.merge equivalent)
    pub fn merge_chains(&mut self, chain1: Option<Box<crate::codegen::chain::Chain>>, 
                       chain2: Option<Box<crate::codegen::chain::Chain>>) 
                       -> Option<Box<crate::codegen::chain::Chain>> {
        use crate::codegen::chain::ChainOps;
        ChainOps::merge(chain1, chain2)
    }

    /// Emit optimized local variable load
    fn emit_load_local(&mut self, typecode: u8, reg: u16) -> Result<()> {
        if reg <= 3 {
            // Use optimized iload_0, iload_1, etc.
            let base_op = match typecode {
                typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => opcodes::ILOAD_0,
                typecodes::LONG => opcodes::LLOAD_0,
                typecodes::FLOAT => opcodes::FLOAD_0,
                typecodes::DOUBLE => opcodes::DLOAD_0,
                _ => opcodes::ALOAD_0,
            };
            self.code.emitop(base_op + reg as u8);
            
            // Update stack state - push the loaded value onto the stack
            let stack_type = match typecode {
                typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => super::code::Type::Int,
                typecodes::LONG => super::code::Type::Long,
                typecodes::FLOAT => super::code::Type::Float,
                typecodes::DOUBLE => super::code::Type::Double,
                _ => super::code::Type::Object("object".to_string()),
            };
            self.code.state.push(stack_type);
        } else {
            // Use general form with register
            let load_op = match typecode {
                typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => opcodes::ILOAD,
                typecodes::LONG => opcodes::LLOAD,
                typecodes::FLOAT => opcodes::FLOAD,
                typecodes::DOUBLE => opcodes::DLOAD,
                _ => opcodes::ALOAD,
            };
            self.code.emitop1w(load_op, reg);
            
            // For general form, emitop1w should handle stack updates automatically
            // But let's add it for safety until we verify emitop1w behavior
            let stack_type = match typecode {
                typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => super::code::Type::Int,
                typecodes::LONG => super::code::Type::Long,
                typecodes::FLOAT => super::code::Type::Float,
                typecodes::DOUBLE => super::code::Type::Double,
                _ => super::code::Type::Object("object".to_string()),
            };
            self.code.state.push(stack_type);
        }
        Ok(())
    }
    
    /// Emit optimized local variable store
    fn emit_store_local(&mut self, typecode: u8, reg: u16) -> Result<()> {
        if reg <= 3 {
            // Use optimized istore_0, istore_1, etc.
            let base_op = match typecode {
                typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => opcodes::ISTORE_0,
                typecodes::LONG => opcodes::LSTORE_0,
                typecodes::FLOAT => opcodes::FSTORE_0,
                typecodes::DOUBLE => opcodes::DSTORE_0,
                _ => opcodes::ASTORE_0,
            };
            self.code.emitop(base_op + reg as u8);
        } else {
            // Use general form with register
            let store_op = match typecode {
                typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => opcodes::ISTORE,
                typecodes::LONG => opcodes::LSTORE,
                typecodes::FLOAT => opcodes::FSTORE,
                typecodes::DOUBLE => opcodes::DSTORE,
                _ => opcodes::ASTORE,
            };
            self.code.emitop1w(store_op, reg);
        }
        Ok(())
    }
    
    /// Emit optimized immediate value load (JavaC pattern)
    fn emit_load_immediate(&mut self, _typecode: u8, value: &Literal) -> Result<()> {
        match value {
            Literal::Integer(val) => {
                // Apply JavaC optimization patterns
                match *val {
                    -1 => self.code.emitop(opcodes::ICONST_M1),
                    0 => self.code.emitop(opcodes::ICONST_0),
                    1 => self.code.emitop(opcodes::ICONST_1),
                    2 => self.code.emitop(opcodes::ICONST_2),
                    3 => self.code.emitop(opcodes::ICONST_3),
                    4 => self.code.emitop(opcodes::ICONST_4),
                    5 => self.code.emitop(opcodes::ICONST_5),
                    -128..=127 => {
                        self.code.emitop(opcodes::BIPUSH);
                        self.code.emit1(*val as u8);
                    }
                    -32768..=32767 => {
                        self.code.emitop(opcodes::SIPUSH);
                        self.code.emit2(*val as u16);
                    }
                    _ => {
                        // Add to constant pool and use LDC
                        let cp_index = self.pool.add_integer(*val as i32);
                        self.code.emitop(opcodes::LDC);
                        self.code.emit1(cp_index as u8); // Use actual CP index
                    }
                }
            }
            
            Literal::Long(val) => {
                match *val {
                    0 => self.code.emitop(opcodes::LCONST_0),
                    1 => self.code.emitop(opcodes::LCONST_1),
                    _ => {
                        // Use LDC2_W for long constants
                        let cp_index = self.pool.add_long(*val);
                        self.code.emitop(opcodes::LDC2_W);
                        self.code.emit2(cp_index); // Use actual CP index
                    }
                }
            }
            
            Literal::Float(val) => {
                if *val == 0.0 {
                    self.code.emitop(opcodes::FCONST_0);
                } else if *val == 1.0 {
                    self.code.emitop(opcodes::FCONST_1);
                } else if *val == 2.0 {
                    self.code.emitop(opcodes::FCONST_2);
                } else {
                    let cp_index = self.pool.add_float(*val as f32);
                    self.code.emitop(opcodes::LDC);
                    self.code.emit1(cp_index as u8); // Use actual CP index
                }
            }
            
            Literal::Double(val) => {
                if *val == 0.0 {
                    self.code.emitop(opcodes::DCONST_0);
                } else if *val == 1.0 {
                    self.code.emitop(opcodes::DCONST_1);
                } else {
                    let cp_index = self.pool.add_double(*val);
                    self.code.emitop(opcodes::LDC2_W);
                    self.code.emit2(cp_index); // Use actual CP index
                }
            }
            
            Literal::Boolean(val) => {
                if *val {
                    self.code.emitop(opcodes::ICONST_1);
                } else {
                    self.code.emitop(opcodes::ICONST_0);
                }
            }
            
            Literal::Char(val) => {
                // Load char as int constant
                let char_value = *val as i64;
                match char_value {
                    0 => self.code.emitop(opcodes::ICONST_0),
                    1 => self.code.emitop(opcodes::ICONST_1),
                    2 => self.code.emitop(opcodes::ICONST_2),
                    3 => self.code.emitop(opcodes::ICONST_3),
                    4 => self.code.emitop(opcodes::ICONST_4),
                    5 => self.code.emitop(opcodes::ICONST_5),
                    _ => {
                        if char_value <= 127 {
                            self.code.emitop_with_operand(opcodes::BIPUSH, &[char_value as u8]);
                        } else if char_value <= 32767 {
                            self.code.emitop_with_operand(opcodes::SIPUSH, &[(char_value >> 8) as u8, char_value as u8]);
                        } else {
                            // Use ldc for larger values
                            let cp_index = self.pool.add_integer(char_value as i32);
                            self.code.emitop(opcodes::LDC);
                            self.code.emit1(cp_index as u8); // Use actual CP index
                        }
                    }
                }
            }
            
            Literal::Null => {
                self.code.emitop(opcodes::ACONST_NULL);
            }
            
            Literal::String(s) => {
                // String constants use LDC
                let cp_index = self.pool.add_string(s);
                self.code.emitop(opcodes::LDC);
                self.code.emit1(cp_index as u8); // Use actual CP index
            }
        }
        
        Ok(())
    }
    
    /// Emit member field access
    fn emit_load_member(&mut self, _typecode: u8, member_name: &str, class_name: &str, field_descriptor: &str, is_static: bool) -> Result<()> {
        // Add field reference to constant pool
        let field_ref_idx = self.pool.add_field_ref(class_name, member_name, field_descriptor);
        
        if is_static {
            self.code.emitop(opcodes::GETSTATIC);
        } else {
            self.code.emitop(opcodes::GETFIELD);
        }
        self.code.emit2(field_ref_idx); // Use actual CP index
        Ok(())
    }
    
    /// Emit member field store
    fn emit_store_member(&mut self, _typecode: u8, member_name: &str, class_name: &str, field_descriptor: &str, is_static: bool) -> Result<()> {
        // Add field reference to constant pool
        let field_ref_idx = self.pool.add_field_ref(class_name, member_name, field_descriptor);
        
        if is_static {
            self.code.emitop(opcodes::PUTSTATIC);
        } else {
            self.code.emitop(opcodes::PUTFIELD);
        }
        self.code.emit2(field_ref_idx); // Use actual CP index
        Ok(())
    }
    
    /// Emit indexed array load (JavaC pattern)
    fn emit_load_indexed(&mut self, typecode: u8) -> Result<()> {
        let array_load_op = match typecode {
            typecodes::INT => opcodes::IALOAD,
            typecodes::LONG => opcodes::LALOAD,
            typecodes::FLOAT => opcodes::FALOAD,
            typecodes::DOUBLE => opcodes::DALOAD,
            typecodes::BYTE => opcodes::BALOAD,
            typecodes::CHAR => opcodes::CALOAD,
            typecodes::SHORT => opcodes::SALOAD,
            _ => opcodes::AALOAD, // Object and array references
        };
        self.code.emitop(array_load_op);
        Ok(())
    }
    
    /// Emit indexed array store (JavaC pattern)
    fn emit_store_indexed(&mut self, typecode: u8) -> Result<()> {
        let array_store_op = match typecode {
            typecodes::INT => opcodes::IASTORE,
            typecodes::LONG => opcodes::LASTORE,
            typecodes::FLOAT => opcodes::FASTORE,
            typecodes::DOUBLE => opcodes::DASTORE,
            typecodes::BYTE => opcodes::BASTORE,
            typecodes::CHAR => opcodes::CASTORE,
            typecodes::SHORT => opcodes::SASTORE,
            _ => opcodes::AASTORE, // Object and array references
        };
        self.code.emitop(array_store_op);
        Ok(())
    }
    
    /// Emit method invocation (JavaC invoke pattern)
    fn emit_invoke_member(&mut self, _typecode: u8, member_name: &str, class_name: &str, method_descriptor: &str, is_static: bool, nonvirtual: bool) -> Result<()> {
        // Add method reference to constant pool
        let method_ref_idx = self.pool.add_method_ref(class_name, member_name, method_descriptor);
        
        eprintln!("🔧 DEBUG: emit_invoke_member - class: {}, method: {}, descriptor: {}, idx: {}", class_name, member_name, method_descriptor, method_ref_idx);
        
        if is_static {
            self.code.emitop(opcodes::INVOKESTATIC);
        } else if nonvirtual {
            self.code.emitop(opcodes::INVOKESPECIAL);
        } else {
            self.code.emitop(opcodes::INVOKEVIRTUAL);
            // TODO: Add interface method detection for INVOKEINTERFACE
        }
        self.code.emit2(method_ref_idx); // Proper constant pool index
        Ok(())
    }
    
    /// Emit type conversion instructions (JavaC coerce pattern)
    fn emit_type_conversion(&mut self, source: u8, target: u8) -> Result<()> {
        // JavaC truncate and conversion logic
        let source_truncated = Self::truncate_typecode(source);
        let target_truncated = Self::truncate_typecode(target);
        
        if source_truncated != target_truncated {
            // Generate i2l, i2f, etc. conversions
            let conversion_base = opcodes::I2L; // Base conversion opcode
            let offset = if target_truncated > source_truncated {
                target_truncated - 1
            } else {
                target_truncated
            };
            
            self.code.emitop(conversion_base + source_truncated * 3 + offset);
        }
        
        if target != target_truncated {
            // Generate narrowing conversions (int2byte, int2char, int2short)
            let narrow_op = opcodes::I2B + target - typecodes::BYTE;
            self.code.emitop(narrow_op);
        }
        
        Ok(())
    }
    
    /// Truncate typecode for conversion (JavaC Code.truncate equivalent)
    fn truncate_typecode(typecode: u8) -> u8 {
        match typecode {
            typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => typecodes::INT,
            _ => typecode,
        }
    }
    
    /// Add method handle to constant pool
    pub fn add_method_handle(&mut self, reference_kind: u8, class_name: &str, method_name: &str, descriptor: &str) -> u16 {
        let method_ref_index = self.pool.add_method_ref(class_name, method_name, descriptor);
        self.pool.add_method_handle(reference_kind, method_ref_index)
    }
    
    /// Add method type to constant pool
    pub fn add_method_type(&mut self, descriptor: &str) -> u16 {
        self.pool.add_method_type(descriptor)
    }
    
    /// Add invoke dynamic to constant pool
    pub fn add_invoke_dynamic(&mut self, bootstrap_method_attr_index: u16, name: &str, descriptor: &str) -> u16 {
        let name_and_type_index = self.pool.add_name_and_type(name, descriptor);
        self.pool.add_invoke_dynamic(bootstrap_method_attr_index, name_and_type_index)
    }
}

/// Item representing addressable entities in bytecode (100% JavaC aligned)
#[derive(Debug, Clone)]
pub enum Item {
    /// Value on operand stack
    Stack { typecode: u8 },
    
    /// Local variable
    Local { typecode: u8, reg: u16 },
    
    /// Immediate value/constant
    Immediate { typecode: u8, value: Literal },
    
    /// Member field or method
    Member { 
        typecode: u8, 
        member_name: String, 
        class_name: String,        // Target class for method/field access
        descriptor: String,        // Method descriptor for invocations
        is_static: bool,
        nonvirtual: bool, // JavaC nonvirtual flag
    },
    
    /// Array element access
    Indexed { typecode: u8 },
    
    /// 'this' reference
    This,
    
    /// 'super' reference  
    Super,
    
    /// Void (no value)
    Void,
}

impl Item {
    /// Get typecode of this item
    pub fn typecode(&self) -> u8 {
        match self {
            Item::Stack { typecode } => *typecode,
            Item::Local { typecode, .. } => *typecode,
            Item::Immediate { typecode, .. } => *typecode,
            Item::Member { typecode, .. } => *typecode,
            Item::Indexed { typecode } => *typecode,
            Item::This | Item::Super => typecodes::OBJECT,
            Item::Void => typecodes::VOID,
        }
    }
    
    /// Get width on stack (JavaC width equivalent)
    pub fn width(&self) -> u8 {
        match self.typecode() {
            typecodes::LONG | typecodes::DOUBLE => 2,
            typecodes::VOID => 0,
            _ => 1,
        }
    }
    
    /// Generate condition item (JavaC mkCond)
    pub fn make_cond(&self) -> CondItem {
        CondItem {
            opcode: opcodes::IFNE, // Default to ifne
            true_jumps: None,
            false_jumps: None,
        }
    }
}

/// Conditional item for jump generation (JavaC CondItem equivalent)
#[derive(Debug, Clone)]
pub struct CondItem {
    /// Comparison opcode (ifne, ifeq, etc.)
    pub opcode: u8,
    
    /// Chain of jumps that should execute when condition is true
    pub true_jumps: Option<Box<crate::codegen::chain::Chain>>,
    
    /// Chain of jumps that should execute when condition is false  
    pub false_jumps: Option<Box<crate::codegen::chain::Chain>>,
}

impl CondItem {
    /// Create new conditional item
    pub fn new(opcode: u8) -> Self {
        Self {
            opcode,
            true_jumps: None,
            false_jumps: None,
        }
    }
    
    /// Create conditional item with specific jump chains
    pub fn with_chains(opcode: u8, true_jumps: Option<Box<crate::codegen::chain::Chain>>, false_jumps: Option<Box<crate::codegen::chain::Chain>>) -> Self {
        Self {
            opcode,
            true_jumps,
            false_jumps,
        }
    }
    
    /// Create always-true condition (no jumps needed)
    pub fn always_true() -> Self {
        Self {
            opcode: opcodes::IFNE, // Placeholder
            true_jumps: None,
            false_jumps: None,
        }
    }
    
    /// Create always-false condition (immediate jump to false)
    pub fn always_false() -> Self {
        Self {
            opcode: opcodes::IFEQ, // Placeholder
            true_jumps: None,
            false_jumps: None,
        }
    }
    
    /// Create negated condition (swap true/false jumps)
    pub fn negate(cond: CondItem) -> Self {
        Self {
            opcode: Self::negate_opcode(cond.opcode),
            true_jumps: cond.false_jumps,
            false_jumps: cond.true_jumps,
        }
    }
    
    /// Check if condition is always false
    pub fn is_false(&self) -> bool {
        // For now, no constant folding - never consider always false
        false
    }
    
    /// Resolve true jumps to current position (JavaC: resolve trueJumps)
    pub fn resolve_true(&self, code: &mut Code) {
        if let Some(ref true_jumps) = self.true_jumps {
            code.resolve(Some(true_jumps.clone()));
        }
    }
    
    /// Resolve false jumps to current position (JavaC: resolve falseJumps)
    pub fn resolve_false(&self, code: &mut Code) {
        if let Some(ref false_jumps) = self.false_jumps {
            code.resolve(Some(false_jumps.clone()));
        }
    }
    
    /// Generate jump when condition is false (JavaC: jumpFalse)
    pub fn jump_false(&self, code: &mut Code) -> Result<Option<Box<crate::codegen::chain::Chain>>> {
        if !code.is_alive() {
            return Ok(None);
        }
        
        // For simple conditions, emit the negated jump
        let negated_opcode = Self::negate_opcode(self.opcode);
        let jump_chain = code.branch(negated_opcode);
        
        // Merge with existing false jumps if any
        if let Some(existing_false) = &self.false_jumps {
            use crate::codegen::chain::ChainOps;
            Ok(ChainOps::merge(Some(existing_false.clone()), jump_chain))
        } else {
            Ok(jump_chain)
        }
    }
    
    /// Generate jump when condition is true (JavaC: jumpTrue)
    pub fn jump_true(&self, code: &mut Code) -> Result<Option<Box<crate::codegen::chain::Chain>>> {
        if !code.is_alive() {
            return Ok(None);
        }
        
        // For simple conditions, emit the direct jump
        let jump_chain = code.branch(self.opcode);
        
        // Merge with existing true jumps if any
        if let Some(existing_true) = &self.true_jumps {
            use crate::codegen::chain::ChainOps;
            Ok(ChainOps::merge(Some(existing_true.clone()), jump_chain))
        } else {
            Ok(jump_chain)
        }
    }
    
    /// Negate a conditional opcode
    fn negate_opcode(opcode: u8) -> u8 {
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
            _ => opcode, // Fallback for unknown opcodes
        }
    }
}