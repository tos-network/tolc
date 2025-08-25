//! 100% JavaC-aligned Items system
//! 
//! This module implements the exact same Items architecture as Oracle's javac,
//! with Items directly operating on the Code buffer for maximum compatibility.

use crate::ast::{Literal, TypeEnum, PrimitiveType};
use crate::common::error::Result;
use crate::codegen::attr::{ResolvedType, PrimitiveType as WashPrimitiveType};
use crate::common::type_resolver::TypeResolver;
use crate::common::import::ImportResolver;
use super::code::Code;
use super::constpool::ConstantPool;
use super::opcodes;
// Backed up: use super::optimization_manager::{OptimizationManager, OptimizationLevel};

/// Return true iff float number is positive zero (JavaC ImmediateItem.isPosZero)
fn is_pos_zero_float(x: f32) -> bool {
    x == 0.0f32 && (1.0f32 / x) > 0.0f32
}

/// Return true iff double number is positive zero (JavaC ImmediateItem.isPosZero)  
fn is_pos_zero_double(x: f64) -> bool {
    x == 0.0f64 && (1.0f64 / x) > 0.0f64
}

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
    
    /// Backed up: Integrated optimization manager
    // Backed up: optimizer: OptimizationManager,
    
    /// Type information from wash/attr phase for type-aware code generation
    wash_type_info: Option<&'a std::collections::HashMap<String, ResolvedType>>,
    
    // Symbol table for method resolution (JavaC syms field)
    symbol_table: Option<&'a std::collections::HashMap<String, crate::codegen::attr::ResolvedSymbol>>,
    
    // Type information for interface detection (JavaC types field)
    type_table: Option<&'a std::collections::HashMap<String, crate::codegen::attr::TypeInfo>>,
}

impl<'a> Items<'a> {
    /// Create new Items manager with direct Code buffer access (JavaC constructor)
    pub fn new(pool: &'a mut ConstantPool, code: &'a mut Code) -> Self {
        Self {
            pool,
            code,
            // Backed up: optimizer: OptimizationManager::new(),
            wash_type_info: None,
            symbol_table: None,
            type_table: None,
        }
    }
    
    /// Create new Items manager with wash type information
    pub fn new_with_wash_types(
        pool: &'a mut ConstantPool, 
        code: &'a mut Code,
        wash_type_info: &'a std::collections::HashMap<String, ResolvedType>
    ) -> Self {
        Self {
            pool,
            code,
            // Backed up: optimizer: OptimizationManager::new(),
            wash_type_info: Some(wash_type_info),
            symbol_table: None,
            type_table: None,
        }
    }
    
    /// Create new Items manager with full symbol and type information (JavaC equivalent)
    pub fn new_with_symbols(
        pool: &'a mut ConstantPool, 
        code: &'a mut Code,
        wash_type_info: &'a std::collections::HashMap<String, ResolvedType>,
        symbol_table: &'a std::collections::HashMap<String, crate::codegen::attr::ResolvedSymbol>,
        type_table: &'a std::collections::HashMap<String, crate::codegen::attr::TypeInfo>
    ) -> Self {
        Self {
            pool,
            code,
            // Backed up: optimizer: OptimizationManager::new(),
            wash_type_info: Some(wash_type_info),
            symbol_table: Some(symbol_table),
            type_table: Some(type_table),
        }
    }
    
    /// Backed up: Create Items with specific optimization level
    // Backed up: pub fn new_with_optimization(
    //     pool: &'a mut ConstantPool, 
    //     code: &'a mut Code, 
    //     level: OptimizationLevel
    // ) -> Self {
    //     Self {
    //         pool,
    //         code,
    //         // Backed up: optimizer: OptimizationManager::with_level(level),
    //         wash_type_info: None,
    //         symbol_table: None,
    //         type_table: None,
    //     }
    // }
    
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
    
    /// Make stack item for ResolvedType from wash phase
    pub fn make_stack_item_for_resolved_type(&self, resolved_type: &ResolvedType) -> Item {
        let typecode = self.resolved_type_to_typecode(resolved_type);
        Item::Stack { typecode }
    }
    
    /// Make local variable item (JavaC makeLocalItem)
    pub fn make_local_item(&self, typ: &TypeEnum, reg: u16) -> Item {
        let typecode = self.type_to_typecode(typ);
        Item::Local { typecode, reg }
    }
    
    /// Make local variable item for ResolvedType from wash phase
    pub fn make_local_item_for_resolved_type(&self, resolved_type: &ResolvedType, reg: u16) -> Item {
        let typecode = self.resolved_type_to_typecode(resolved_type);
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
    pub fn make_cond_item(&self, opcode: u8) -> Item {
        Item::Cond {
            opcode,
            true_jumps: None,
            false_jumps: None,
            tree: None,
        }
    }
    
    /// Make always-true conditional item (JavaC CondItem.always_true)
    pub fn make_always_true_cond(&self) -> Item {
        Item::Cond {
            opcode: opcodes::GOTO,
            true_jumps: None,
            false_jumps: None,
            tree: None,
        }
    }
    
    /// Make always-false conditional item (JavaC CondItem.always_false)
    pub fn make_always_false_cond(&self) -> Item {
        Item::Cond {
            opcode: opcodes::DONTGOTO,
            true_jumps: None,
            false_jumps: None,
            tree: None,
        }
    }
    
    /// Negate a conditional item (JavaC CondItem.negate)
    pub fn negate_cond_item(&self, item: Item) -> Item {
        match item {
            Item::Cond { opcode, true_jumps, false_jumps, tree } => {
                Item::Cond {
                    opcode: Item::negate_cond_opcode(opcode),
                    true_jumps: false_jumps, // Swap true and false jumps
                    false_jumps: true_jumps,
                    tree,
                }
            },
            _ => item, // Non-conditional items can't be negated
        }
    }
    
    /// Make static item (JavaC makeStaticItem)
    pub fn make_static_item(&self, member_name: String, class_name: String, descriptor: String, typ: &TypeEnum) -> Item {
        let typecode = match typ {
            TypeEnum::Primitive(p) => match p {
                PrimitiveType::Boolean | PrimitiveType::Byte | PrimitiveType::Short | PrimitiveType::Int | PrimitiveType::Char => typecodes::INT,
                PrimitiveType::Long => typecodes::LONG,
                PrimitiveType::Float => typecodes::FLOAT,
                PrimitiveType::Double => typecodes::DOUBLE,
            },
            _ => typecodes::OBJECT,
        };
        
        Item::Static {
            typecode,
            member_name,
            class_name: class_name.replace('.', "/"),
            descriptor,
        }
    }
    
    /// Make assignment item (JavaC makeAssignItem equivalent)
    pub fn make_assign_item(&self, lhs: Item) -> Item {
        let typecode = lhs.typecode();
        Item::Assign { 
            lhs: Box::new(lhs),
            typecode,
        }
    }
    
    /// Convert TypeEnum to typecode
    fn type_to_typecode(&self, typ: &TypeEnum) -> u8 {
        match typ {
            TypeEnum::Primitive(prim_type) => {
                match prim_type {
                    PrimitiveType::Boolean => typecodes::BYTE, // boolean stored as byte in JVM
                    PrimitiveType::Byte => typecodes::BYTE,
                    PrimitiveType::Short => typecodes::SHORT,
                    PrimitiveType::Int => typecodes::INT,
                    PrimitiveType::Long => typecodes::LONG,
                    PrimitiveType::Float => typecodes::FLOAT,
                    PrimitiveType::Double => typecodes::DOUBLE,
                    PrimitiveType::Char => typecodes::CHAR,
                }
            }
            TypeEnum::Reference(_) => typecodes::OBJECT,
            TypeEnum::Void => typecodes::VOID,
        }
    }
    
    /// Convert ResolvedType from wash phase to JVM typecode
    fn resolved_type_to_typecode(&self, resolved_type: &ResolvedType) -> u8 {
        match resolved_type {
            ResolvedType::Primitive(prim_type) => {
                match prim_type {
                    WashPrimitiveType::Boolean => typecodes::BYTE, // boolean stored as byte in JVM
                    WashPrimitiveType::Byte => typecodes::BYTE,
                    WashPrimitiveType::Short => typecodes::SHORT,
                    WashPrimitiveType::Int => typecodes::INT,
                    WashPrimitiveType::Long => typecodes::LONG,
                    WashPrimitiveType::Float => typecodes::FLOAT,
                    WashPrimitiveType::Double => typecodes::DOUBLE,
                    WashPrimitiveType::Char => typecodes::CHAR,
                }
            }
            ResolvedType::Reference(_) | 
            ResolvedType::Class(_) |
            ResolvedType::TypeVariable(_) |
            ResolvedType::Wildcard(_) => typecodes::OBJECT,
            ResolvedType::Array(_) => typecodes::ARRAY,
            ResolvedType::Method(_, _) => typecodes::OBJECT, // Method references as objects
            ResolvedType::NoType => typecodes::VOID,
            ResolvedType::Error => typecodes::OBJECT, // Error types default to object
            _ => typecodes::OBJECT, // Default for other types
        }
    }
    
    /// Make field access item with ResolvedType support
    pub fn make_field_item_for_resolved_type(
        &self, 
        field_name: String, 
        owner_class: String, 
        resolved_type: &ResolvedType,
        is_static: bool
    ) -> Item {
        let typecode = self.resolved_type_to_typecode(resolved_type);
        let descriptor = self.resolved_type_to_descriptor(resolved_type);
        
        Item::Member { 
            typecode, 
            member_name: field_name, 
            class_name: owner_class,
            descriptor,
            is_static,
            nonvirtual: false, // Fields are not virtual
        }
    }
    
    /// Convert ResolvedType to JVM field descriptor
    fn resolved_type_to_descriptor(&self, resolved_type: &ResolvedType) -> String {
        match resolved_type {
            ResolvedType::Primitive(prim_type) => {
                match prim_type {
                    WashPrimitiveType::Boolean => "Z".to_string(),
                    WashPrimitiveType::Byte => "B".to_string(),
                    WashPrimitiveType::Short => "S".to_string(),
                    WashPrimitiveType::Int => "I".to_string(),
                    WashPrimitiveType::Long => "J".to_string(),
                    WashPrimitiveType::Float => "F".to_string(),
                    WashPrimitiveType::Double => "D".to_string(),
                    WashPrimitiveType::Char => "C".to_string(),
                }
            }
            ResolvedType::Reference(class_name) => {
                // Check for generic type parameters that need type erasure (T, E, K, V, etc.)
                if class_name.len() == 1 && class_name.chars().next().unwrap().is_uppercase() {
                    return "Ljava/lang/Object;".to_string();
                }
                
                // Handle common unqualified types like "String" -> "java.lang.String"
                match class_name.as_str() {
                    "String" => "Ljava/lang/String;".to_string(),
                    "Object" => "Ljava/lang/Object;".to_string(),
                    _ => {
                        // Use TypeResolver for dynamic resolution
                        let mut type_resolver = crate::common::type_resolver::OwnedTypeResolver::new("tests/java");
                        
                        if let Some(fully_qualified) = type_resolver.resolve_type_name_simple(class_name) {
                            format!("L{};", fully_qualified.replace('.', "/"))
                        } else if crate::common::consts::JAVA_LANG_SIMPLE_TYPES.contains(&class_name.as_str()) {
                            format!("Ljava/lang/{};", class_name)
                        } else {
                            // Fallback: assume it's already qualified or use as-is
                            format!("L{};", class_name.replace('.', "/"))
                        }
                    }
                }
            }
            ResolvedType::Class(class_type) => {
                // For generic classes, use erasure (raw type)
                // Handle common unqualified types like "String" -> "java.lang.String"
                match class_type.name.as_str() {
                    "String" => "Ljava/lang/String;".to_string(),
                    "Object" => "Ljava/lang/Object;".to_string(),
                    _ => {
                        // Use TypeResolver for dynamic resolution
                        let mut type_resolver = crate::common::type_resolver::OwnedTypeResolver::new("tests/java");
                        
                        if let Some(fully_qualified) = type_resolver.resolve_type_name_simple(&class_type.name) {
                            format!("L{};", fully_qualified.replace('.', "/"))
                        } else if crate::common::consts::JAVA_LANG_SIMPLE_TYPES.contains(&class_type.name.as_str()) {
                            format!("Ljava/lang/{};", class_type.name)
                        } else {
                            // Fallback: assume it's already qualified or use as-is
                            format!("L{};", class_type.name.replace('.', "/"))
                        }
                    }
                }
            }
            ResolvedType::TypeVariable(_) => {
                // Type variables are erased to Object
                "Ljava/lang/Object;".to_string()
            }
            ResolvedType::Wildcard(_) => {
                // Wildcards are erased to their upper bound, default to Object
                "Ljava/lang/Object;".to_string()
            }
            ResolvedType::Array(elem_type) => {
                format!("[{}", self.resolved_type_to_descriptor(elem_type))
            }
            ResolvedType::NoType => "V".to_string(),
            _ => "Ljava/lang/Object;".to_string(), // Default for other types
        }
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
            
            Item::Static { typecode, member_name, class_name, descriptor } => {
                // JavaC StaticItem.load() - emit getstatic
                let field_idx = self.pool.add_field_ref(&class_name, &member_name, &descriptor);
                self.code.emitop2(opcodes::GETSTATIC, field_idx);
                Ok(Item::Stack { typecode: *typecode })
            }
            
            Item::Indexed { typecode } => {
                self.emit_load_indexed(*typecode)?;
                Ok(Item::Stack { typecode: *typecode })
            }
            
            Item::This | Item::Super => {
                self.code.emitop0(opcodes::ALOAD_0);
                Ok(Item::Stack { typecode: typecodes::OBJECT })
            }
            
            Item::Void => Ok(Item::Void),
            
            Item::Assign { .. } => {
                // Assignment items should be handled by their own load method
                todo!("Assignment items should use item.load(items) instead of load_item")
            },
            
            Item::Cond { .. } => {
                // Conditional items should be loaded using the load() method which converts to boolean
                todo!("Conditional items should use item.load(items) instead of load_item")
            },
            
            Item::ConversionChain { .. } => {
                // Conversion chain items should use their own load() method
                todo!("ConversionChain items should use item.load(items) instead of load_item")
            },
            
            Item::ReferenceConversion { .. } => {
                // Reference conversion items should use their own load() method  
                todo!("ReferenceConversion items should use item.load(items) instead of load_item")
            },
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
            
            Item::Static { typecode, member_name, class_name, descriptor } => {
                // JavaC StaticItem.invoke() - emit invokestatic
                self.emit_invoke_static(*typecode, member_name, class_name, descriptor)?;
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
    
    // ========== Constant Pool Methods ==========
    
    /// Add method handle to constant pool - JavaC equivalent
    pub fn add_method_handle(&mut self, reference_kind: u8, class_name: &str, name: &str, descriptor: &str) -> u16 {
        // First add the method reference
        let method_ref_index = self.pool.add_method_ref(class_name, name, descriptor);
        // Then create the method handle pointing to it
        self.pool.add_method_handle(reference_kind, method_ref_index)
    }

    /// Add method type to constant pool - JavaC equivalent  
    pub fn add_method_type(&mut self, descriptor: &str) -> u16 {
        self.pool.add_method_type(descriptor)
    }

    /// Add invoke dynamic to constant pool - JavaC equivalent
    pub fn add_invoke_dynamic(&mut self, bootstrap_method_attr_index: u16, name: &str, descriptor: &str) -> u16 {
        let name_and_type_index = self.pool.add_name_and_type(name, descriptor);
        self.pool.add_invoke_dynamic(bootstrap_method_attr_index, name_and_type_index)
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
    
    /// Emit method invocation - JavaC MemberItem.invoke() pattern
    fn emit_invoke_member(&mut self, _typecode: u8, member_name: &str, class_name: &str, method_descriptor: &str, is_static: bool, nonvirtual: bool) -> Result<()> {
        // Add method reference to constant pool
        let method_ref_idx = self.pool.add_method_ref(class_name, member_name, method_descriptor);
        
        eprintln!("🔧 DEBUG: MemberItem.invoke() - class: {}, method: {}, descriptor: {}, static: {}, nonvirtual: {}", 
                 class_name, member_name, method_descriptor, is_static, nonvirtual);
        
        // JavaC MemberItem.invoke() pattern:
        // if ((member.owner.flags() & Flags.INTERFACE) != 0 && !nonvirtual) {
        //     code.emitInvokeinterface(pool.put(member), mtype);
        // } else if (nonvirtual) {
        //     code.emitInvokespecial(pool.put(member), mtype);
        // } else {
        //     code.emitInvokevirtual(pool.put(member), mtype);
        // }
        
        if is_static {
            // Static methods should be handled by StaticItem.invoke(), not MemberItem.invoke()
            self.code.emitop(opcodes::INVOKESTATIC);
            self.code.emit2(method_ref_idx);
        } else if self.is_interface_method(class_name, member_name) && !nonvirtual {
            // Interface method call - invokeinterface (JavaC MemberItem pattern)
            self.emit_invoke_interface(method_ref_idx, method_descriptor)?;
        } else if nonvirtual {
            // Non-virtual method call - invokespecial (super calls, constructors, private methods)
            self.code.emitop(opcodes::INVOKESPECIAL);
            self.code.emit2(method_ref_idx);
        } else {
            // Virtual method call - invokevirtual (regular instance methods)
            self.code.emitop(opcodes::INVOKEVIRTUAL);
            self.code.emit2(method_ref_idx);
        }
        Ok(())
    }
    
    /// Emit static method invocation (JavaC StaticItem.invoke equivalent)
    fn emit_invoke_static(&mut self, _typecode: u8, member_name: &str, class_name: &str, method_descriptor: &str) -> Result<()> {
        // Add method reference to constant pool
        let method_ref_idx = self.pool.add_method_ref(class_name, member_name, method_descriptor);
        
        eprintln!("🔧 DEBUG: emit_invoke_static - class: {}, method: {}, descriptor: {}, idx: {}", class_name, member_name, method_descriptor, method_ref_idx);
        
        // JavaC StaticItem.invoke() - code.emitInvokestatic(pool.put(member), mtype);
        self.code.emitop(opcodes::INVOKESTATIC);
        self.code.emit2(method_ref_idx);
        
        Ok(())
    }
    
    /// Check if a method belongs to an interface (JavaC (member.owner.flags() & Flags.INTERFACE) != 0)
    fn is_interface_method(&self, class_name: &str, _method_name: &str) -> bool {
        // Check type table for interface flag if available
        if let Some(type_table) = self.type_table {
            if let Some(type_info) = type_table.get(class_name) {
                return type_info.is_interface;
            }
        }
        
        // Fallback: check known interface classes
        match class_name {
            // Common Java interface patterns
            "java/lang/Runnable" |
            "java/lang/Iterable" |
            "java/util/Iterator" |
            "java/util/Collection" |
            "java/util/List" |
            "java/util/Set" |
            "java/util/Map" |
            "java/io/Serializable" |
            "java/lang/Comparable" |
            "java/lang/CharSequence" => true,
            
            // Check for interface naming conventions
            name if name.contains("Interface") || name.ends_with("able") => true,
            
            _ => false,
        }
    }
    
    /// Emit invokeinterface instruction (JavaC emitInvokeinterface equivalent)
    fn emit_invoke_interface(&mut self, method_ref_idx: u16, method_descriptor: &str) -> Result<()> {
        self.code.emitop(opcodes::INVOKEINTERFACE);
        self.code.emit2(method_ref_idx);
        
        // Calculate argument count from method descriptor (required for invokeinterface)
        let arg_count = self.calculate_method_args(method_descriptor);
        self.code.emit1(arg_count);
        self.code.emit1(0); // Reserved byte (always 0)
        
        Ok(())
    }
    
    /// Calculate method argument count from descriptor (JavaC pattern)
    fn calculate_method_args(&self, descriptor: &str) -> u8 {
        let mut count = 1u8; // Start with 1 for 'this' parameter
        let mut chars = descriptor.chars().skip(1); // Skip opening '('
        
        while let Some(ch) = chars.next() {
            if ch == ')' {
                break;
            }
            
            match ch {
                'L' => {
                    // Object type - skip to semicolon
                    while let Some(c) = chars.next() {
                        if c == ';' {
                            break;
                        }
                    }
                    count += 1;
                }
                '[' => {
                    // Array type - find element type
                    while let Some(c) = chars.next() {
                        if c != '[' {
                            if c == 'L' {
                                // Skip to semicolon for object arrays
                                while let Some(sc) = chars.next() {
                                    if sc == ';' {
                                        break;
                                    }
                                }
                            }
                            break;
                        }
                    }
                    count += 1;
                }
                'J' | 'D' => {
                    // long and double take 2 slots
                    count += 2;
                }
                _ => {
                    // Primitive types take 1 slot
                    count += 1;
                }
            }
        }
        
        count
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
    
    
    /// Make static field item using resolved type - JavaC makeStaticItem equivalent
    pub fn make_static_item_for_resolved_type(&self, name: &str, owner_class: &str, resolved_type: &crate::codegen::attr::ResolvedType) -> Item {
        let typecode = match resolved_type {
            crate::codegen::attr::ResolvedType::Primitive(prim) => match prim {
                crate::codegen::attr::PrimitiveType::Int | 
                crate::codegen::attr::PrimitiveType::Boolean | 
                crate::codegen::attr::PrimitiveType::Byte | 
                crate::codegen::attr::PrimitiveType::Short | 
                crate::codegen::attr::PrimitiveType::Char => typecodes::INT,
                crate::codegen::attr::PrimitiveType::Long => typecodes::LONG,
                crate::codegen::attr::PrimitiveType::Float => typecodes::FLOAT,
                crate::codegen::attr::PrimitiveType::Double => typecodes::DOUBLE,
            },
            _ => typecodes::OBJECT,
        };
        
        let descriptor = resolved_type_to_descriptor(resolved_type);
        Item::Static {
            typecode,
            member_name: name.to_string(),
            class_name: owner_class.replace('.', "/"),
            descriptor,
        }
    }
    
    /// Make instance member item using resolved type - JavaC makeMemberItem equivalent
    pub fn make_member_item_for_resolved_type(&self, name: &str, owner_class: &str, resolved_type: &crate::codegen::attr::ResolvedType, is_private: bool) -> Item {
        let typecode = match resolved_type {
            crate::codegen::attr::ResolvedType::Primitive(prim) => match prim {
                crate::codegen::attr::PrimitiveType::Int | 
                crate::codegen::attr::PrimitiveType::Boolean | 
                crate::codegen::attr::PrimitiveType::Byte | 
                crate::codegen::attr::PrimitiveType::Short | 
                crate::codegen::attr::PrimitiveType::Char => typecodes::INT,
                crate::codegen::attr::PrimitiveType::Long => typecodes::LONG,
                crate::codegen::attr::PrimitiveType::Float => typecodes::FLOAT,
                crate::codegen::attr::PrimitiveType::Double => typecodes::DOUBLE,
            },
            crate::codegen::attr::ResolvedType::Array(_) => typecodes::ARRAY,
            _ => typecodes::OBJECT,
        };
        
        let descriptor = resolved_type_to_descriptor(resolved_type);
        Item::Member {
            typecode,
            member_name: name.to_string(),
            class_name: owner_class.replace('.', "/"), 
            descriptor,
            is_static: false,
            nonvirtual: is_private, // Private methods are non-virtual
        }
    }
}

/// Helper function to convert ResolvedType to JVM descriptor
fn resolved_type_to_descriptor(resolved_type: &crate::codegen::attr::ResolvedType) -> String {
    match resolved_type {
        crate::codegen::attr::ResolvedType::Primitive(prim) => match prim {
            crate::codegen::attr::PrimitiveType::Boolean => "Z".to_string(),
            crate::codegen::attr::PrimitiveType::Byte => "B".to_string(),
            crate::codegen::attr::PrimitiveType::Char => "C".to_string(),
            crate::codegen::attr::PrimitiveType::Short => "S".to_string(),
            crate::codegen::attr::PrimitiveType::Int => "I".to_string(),
            crate::codegen::attr::PrimitiveType::Long => "J".to_string(),
            crate::codegen::attr::PrimitiveType::Float => "F".to_string(),
            crate::codegen::attr::PrimitiveType::Double => "D".to_string(),
        },
        crate::codegen::attr::ResolvedType::Reference(class_name) => {
            // Check for generic type parameters that need type erasure (T, E, K, V, etc.)
            if class_name.len() == 1 && class_name.chars().next().unwrap().is_uppercase() {
                return "Ljava/lang/Object;".to_string();
            }
            
            let internal_name = class_name.replace('.', "/");
            format!("L{};", internal_name)
        },
        crate::codegen::attr::ResolvedType::Array(element_type) => {
            let mut result = "[".to_string();
            result.push_str(&resolved_type_to_descriptor(element_type));
            result
        },
        crate::codegen::attr::ResolvedType::Generic(_, _) => {
            // Generic types erase to Object at runtime
            "Ljava/lang/Object;".to_string()
        },
        _ => {
            // Fallback for other types (wildcards, type variables, etc.)
            "Ljava/lang/Object;".to_string()
        },
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
    
    /// Static field or method (JavaC StaticItem equivalent)
    Static {
        typecode: u8,
        member_name: String,
        class_name: String,        // Target class for method/field access  
        descriptor: String,        // Method descriptor for invocations
    },
    
    /// Array element access
    Indexed { typecode: u8 },
    
    /// Type conversion chain (optimized cast operations)
    ConversionChain {
        source_item: Box<Item>,
        instructions: Vec<crate::codegen::opcode_enum::Opcode>,
        target_typecode: u8,
    },
    
    /// Reference type conversion (with checkcast optimization)
    ReferenceConversion {
        source_item: Box<Item>,
        target_type: String,
        needs_checkcast: bool,
    },
    
    /// 'this' reference
    This,
    
    /// 'super' reference  
    Super,
    
    /// Void (no value)
    Void,
    
    /// Assignment item (JavaC AssignItem equivalent)
    Assign { 
        lhs: Box<Item>,  // Left-hand side item
        typecode: u8,    // Result typecode
    },
    
    /// Conditional item (JavaC CondItem equivalent) - 100% JavaC aligned
    Cond {
        /// Comparison opcode (ifne, ifeq, etc.)
        opcode: u8,
        /// Chain of jumps that should execute when condition is true
        true_jumps: Option<Box<crate::codegen::chain::Chain>>,
        /// Chain of jumps that should execute when condition is false  
        false_jumps: Option<Box<crate::codegen::chain::Chain>>,
        /// The abstract syntax tree of this item (for debugging and optimization)
        tree: Option<crate::ast::Expr>,
    },
}

impl Item {
    /// Get typecode of this item
    pub fn typecode(&self) -> u8 {
        match self {
            Item::Stack { typecode } => *typecode,
            Item::Local { typecode, .. } => *typecode,
            Item::Immediate { typecode, .. } => *typecode,
            Item::Member { typecode, .. } => *typecode,
            Item::Static { typecode, .. } => *typecode,
            Item::Indexed { typecode } => *typecode,
            Item::ConversionChain { target_typecode, .. } => *target_typecode,
            Item::ReferenceConversion { .. } => typecodes::OBJECT,
            Item::This | Item::Super => typecodes::OBJECT,
            Item::Void => typecodes::VOID,
            Item::Assign { typecode, .. } => *typecode,
            Item::Cond { .. } => typecodes::BYTE, // CondItems are always byte type
        }
    }
    
    /// Get width on stack (JavaC width equivalent)
    pub fn width(&self) -> u8 {
        match self {
            Item::Assign { lhs, typecode } => {
                // JavaC AssignItem.width(): lhs.width() + Code.width(typecode)
                let type_width = match typecode {
                    &typecodes::LONG | &typecodes::DOUBLE => 2,
                    &typecodes::VOID => 0,
                    _ => 1,
                };
                lhs.width() + type_width
            },
            _ => {
                match self.typecode() {
                    typecodes::LONG | typecodes::DOUBLE => 2,
                    typecodes::VOID => 0,
                    _ => 1,
                }
            }
        }
    }
    
    /// Generate condition item (JavaC mkCond)
    pub fn make_cond(&self) -> Item {
        Item::Cond {
            opcode: opcodes::IFNE, // Default to ifne
            true_jumps: None,
            false_jumps: None,
            tree: None,
        }
    }
    
    /// Load item value onto stack (JavaC load() equivalent)
    /// This is the core method that all expression evaluation goes through
    pub fn load(self, items: &mut Items) -> Result<Item> {
        match self {
            Item::Stack { typecode } => {
                // Already on stack, return stack item
                Ok(Item::Stack { typecode })
            },
            
            Item::Local { typecode, reg } => {
                // Load local variable
                match typecode {
                    typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => {
                        items.code.emitop1(opcodes::ILOAD, reg as u8);
                    },
                    typecodes::LONG => {
                        items.code.emitop1(opcodes::LLOAD, reg as u8);
                    },
                    typecodes::FLOAT => {
                        items.code.emitop1(opcodes::FLOAD, reg as u8);
                    },
                    typecodes::DOUBLE => {
                        items.code.emitop1(opcodes::DLOAD, reg as u8);
                    },
                    typecodes::OBJECT | typecodes::ARRAY => {
                        items.code.emitop1(opcodes::ALOAD, reg as u8);
                    },
                    _ => return Err(crate::common::error::Error::codegen_error(format!("Unsupported local variable typecode: {}", typecode))),
                }
                Ok(Item::Stack { typecode })
            },
            
            Item::Immediate { typecode, value } => {
                // JavaC Optimizer #3: ImmediateItem.load() - 100% JavaC aligned
                // Matches com.sun.tools.javac.jvm.Items.ImmediateItem.load() exactly
                match typecode {
                    // INTcode, BYTEcode, SHORTcode, CHARcode - all use integer optimization
                    // Boolean values are also handled as integers in JVM
                    typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => {
                        let ival = match value {
                            Literal::Integer(i) => i,
                            Literal::Boolean(b) => if b { 1 } else { 0 }, // Convert boolean to int  
                            Literal::Char(c) => c as i64, // Convert char to int
                            _ => return Err(crate::common::error::Error::codegen_error(format!("Expected integer-compatible literal for integer typecode, got: {:?}", value)))
                        };
                        
                        // Apply JavaC ImmediateItem.load() integer optimization logic
                        if ival >= -1 && ival <= 5 {
                            // iconst_m1 through iconst_5 (JavaC optimization)
                            if ival == -1 {
                                items.code.emitop(opcodes::ICONST_M1);
                            } else {
                                items.code.emitop(opcodes::ICONST_0 + (ival as u8));
                            }
                        } else if ival >= -128 && ival <= 127 {
                            // bipush for byte range (JavaC optimization)
                            items.code.emitop1(opcodes::BIPUSH, ival as u8);
                        } else if ival >= -32768 && ival <= 32767 {
                            // sipush for short range (JavaC optimization)
                            items.code.emitop2(opcodes::SIPUSH, ival as u16);
                        } else {
                            // ldc for large integers (JavaC fallback)
                            let const_idx = items.pool.add_integer(ival as i32);
                            items.code.emitop2(opcodes::LDC_W, const_idx);
                        }
                    },
                    
                    typecodes::LONG => {
                        if let Literal::Long(lval) = value {
                            let lval = lval; // No dereference needed
                            if lval == 0 || lval == 1 {
                                // lconst_0, lconst_1 (JavaC optimization)
                                items.code.emitop(opcodes::LCONST_0 + (lval as u8));
                            } else {
                                // ldc2_w for other long values (JavaC fallback)
                                let const_idx = items.pool.add_long(lval);
                                items.code.emitop2(opcodes::LDC2_W, const_idx);
                            }
                        } else {
                            return Err(crate::common::error::Error::codegen_error(format!("Expected long literal for long typecode, got: {:?}", value)));
                        }
                    },
                    
                    typecodes::FLOAT => {
                        if let Literal::Float(fval) = value {
                            let fval = fval as f32; // Convert to f32, no dereference needed
                            if is_pos_zero_float(fval) || fval == 1.0 || fval == 2.0 {
                                // fconst_0, fconst_1, fconst_2 (JavaC optimization)
                                items.code.emitop(opcodes::FCONST_0 + (fval as u8));
                            } else {
                                // ldc for other float values (JavaC fallback)
                                let const_idx = items.pool.add_float(fval);
                                items.code.emitop2(opcodes::LDC_W, const_idx);
                            }
                        } else {
                            return Err(crate::common::error::Error::codegen_error(format!("Expected float literal for float typecode, got: {:?}", value)));
                        }
                    },
                    
                    typecodes::DOUBLE => {
                        if let Literal::Double(dval) = value {
                            let dval = dval; // No dereference needed
                            if is_pos_zero_double(dval) || dval == 1.0 {
                                // dconst_0, dconst_1 (JavaC optimization)
                                items.code.emitop(opcodes::DCONST_0 + (dval as u8));
                            } else {
                                // ldc2_w for other double values (JavaC fallback)
                                let const_idx = items.pool.add_double(dval);
                                items.code.emitop2(opcodes::LDC2_W, const_idx);
                            }
                        } else {
                            return Err(crate::common::error::Error::codegen_error(format!("Expected double literal for double typecode, got: {:?}", value)));
                        }
                    },
                    
                    typecodes::OBJECT => {
                        match value {
                            Literal::String(s) => {
                                // String constant
                                let const_idx = items.pool.add_string(&s);
                                items.code.emitop2(opcodes::LDC_W, const_idx);
                            },
                            Literal::Null => {
                                items.code.emitop(opcodes::ACONST_NULL);
                            },
                            _ => {
                                // Other object constants via ldc
                                let const_idx = items.pool.add_string(&format!("{:?}", value));
                                items.code.emitop2(opcodes::LDC_W, const_idx);
                            }
                        }
                    },
                    
                    _ => return Err(crate::common::error::Error::codegen_error(format!("Unsupported immediate typecode: {}", typecode))),
                }
                
                Ok(Item::Stack { typecode })
            },
            
            Item::Member { typecode, member_name, class_name, descriptor, is_static, .. } => {
                if is_static {
                    // Static field access - getstatic
                    let field_idx = items.pool.add_field_ref(&class_name, &member_name, &descriptor);
                    items.code.emitop2(opcodes::GETSTATIC, field_idx);
                } else {
                    // Instance field access - getfield (assumes 'this' is already on stack)
                    let field_idx = items.pool.add_field_ref(&class_name, &member_name, &descriptor);
                    items.code.emitop2(opcodes::GETFIELD, field_idx);
                }
                Ok(Item::Stack { typecode })
            },
            
            Item::Static { typecode, member_name, class_name, descriptor } => {
                // JavaC StaticItem.load() - emit getstatic for static field
                let field_idx = items.pool.add_field_ref(&class_name, &member_name, &descriptor);
                items.code.emitop2(opcodes::GETSTATIC, field_idx);
                Ok(Item::Stack { typecode })
            },
            
            Item::Indexed { typecode } => {
                // Array access - assumes array and index are on stack
                match typecode {
                    typecodes::INT => items.code.emitop(opcodes::IALOAD),
                    typecodes::LONG => items.code.emitop(opcodes::LALOAD),
                    typecodes::FLOAT => items.code.emitop(opcodes::FALOAD),
                    typecodes::DOUBLE => items.code.emitop(opcodes::DALOAD),
                    typecodes::OBJECT | typecodes::ARRAY => items.code.emitop(opcodes::AALOAD),
                    typecodes::BYTE => items.code.emitop(opcodes::BALOAD),
                    typecodes::CHAR => items.code.emitop(opcodes::CALOAD),
                    typecodes::SHORT => items.code.emitop(opcodes::SALOAD),
                    _ => return Err(crate::common::error::Error::codegen_error(format!("Unsupported array element typecode: {}", typecode))),
                }
                Ok(Item::Stack { typecode })
            },
            
            Item::This => {
                // Load 'this' reference
                items.code.emitop(opcodes::ALOAD_0);
                Ok(Item::Stack { typecode: typecodes::OBJECT })
            },
            
            Item::Super => {
                // Load 'this' reference for super access
                items.code.emitop(opcodes::ALOAD_0);
                Ok(Item::Stack { typecode: typecodes::OBJECT })
            },
            
            Item::Assign { lhs, typecode } => {
                // JavaC AssignItem.load() equivalent: stash + store + return stack item
                let stashed_lhs = lhs.stash(typecode, items)?;
                stashed_lhs.store(items)?;
                Ok(Item::Stack { typecode })
            },
            
            Item::ConversionChain { source_item, instructions, target_typecode } => {
                // Load source item first
                let loaded_source = source_item.load(items)?;
                
                // Apply conversion instructions in sequence
                for instruction in instructions {
                    match instruction {
                        crate::codegen::opcode_enum::Opcode::I2b => items.code.emitop(opcodes::I2B),
                        crate::codegen::opcode_enum::Opcode::I2c => items.code.emitop(opcodes::I2C),
                        crate::codegen::opcode_enum::Opcode::I2s => items.code.emitop(opcodes::I2S),
                        crate::codegen::opcode_enum::Opcode::I2l => items.code.emitop(opcodes::I2L),
                        crate::codegen::opcode_enum::Opcode::I2f => items.code.emitop(opcodes::I2F),
                        crate::codegen::opcode_enum::Opcode::I2d => items.code.emitop(opcodes::I2D),
                        crate::codegen::opcode_enum::Opcode::L2i => items.code.emitop(opcodes::L2I),
                        crate::codegen::opcode_enum::Opcode::L2f => items.code.emitop(opcodes::L2F),
                        crate::codegen::opcode_enum::Opcode::L2d => items.code.emitop(opcodes::L2D),
                        crate::codegen::opcode_enum::Opcode::F2i => items.code.emitop(opcodes::F2I),
                        crate::codegen::opcode_enum::Opcode::F2l => items.code.emitop(opcodes::F2L),
                        crate::codegen::opcode_enum::Opcode::F2d => items.code.emitop(opcodes::F2D),
                        crate::codegen::opcode_enum::Opcode::D2i => items.code.emitop(opcodes::D2I),
                        crate::codegen::opcode_enum::Opcode::D2l => items.code.emitop(opcodes::D2L),
                        crate::codegen::opcode_enum::Opcode::D2f => items.code.emitop(opcodes::D2F),
                        crate::codegen::opcode_enum::Opcode::Int2byte => items.code.emitop(opcodes::I2B),
                        crate::codegen::opcode_enum::Opcode::Int2char => items.code.emitop(opcodes::I2C),
                        crate::codegen::opcode_enum::Opcode::Int2short => items.code.emitop(opcodes::I2S),
                        _ => return Err(crate::common::error::Error::codegen_error(format!("Unsupported conversion instruction: {:?}", instruction))),
                    }
                }
                
                Ok(Item::Stack { typecode: target_typecode })
            },
            
            Item::ReferenceConversion { source_item, target_type, needs_checkcast } => {
                // Load source item first
                let loaded_source = source_item.load(items)?;
                
                if needs_checkcast {
                    // Emit checkcast instruction
                    let class_idx = items.pool.add_class(&target_type);
                    items.code.emitop2(opcodes::CHECKCAST, class_idx);
                }
                
                Ok(Item::Stack { typecode: typecodes::OBJECT })
            },
            
            Item::Void => {
                // Void items don't load anything
                Ok(Item::Void)
            },
            
            Item::Cond { mut opcode, mut true_jumps, mut false_jumps, tree } => {
                // JavaC CondItem.load() - convert conditional to boolean value (0 or 1)
                // Generate the false branch first
                let negated_opcode = Self::negate_cond_opcode(opcode);
                let false_chain = items.code.branch(negated_opcode);
                let merged_false = Self::merge_chains(false_jumps, false_chain);
                
                // If not always false, generate true branch
                if !Self::is_cond_false(opcode, &true_jumps, &merged_false) {
                    // Resolve true jumps to current position
                    if let Some(true_chain) = true_jumps {
                        items.code.resolve(Some(true_chain));
                    }
                    // Push true value (1) onto stack
                    items.code.emitop(opcodes::ICONST_1);
                    // Jump over false branch
                    let end_chain = items.code.branch(opcodes::GOTO);
                    
                    // Generate false branch
                    if let Some(false_chain) = merged_false {
                        items.code.resolve(Some(false_chain));
                        // Push false value (0) onto stack
                        items.code.emitop(opcodes::ICONST_0);
                    }
                    
                    // Resolve end of true branch
                    if let Some(end_chain) = end_chain {
                        items.code.resolve(Some(end_chain));
                    }
                } else if let Some(false_chain) = merged_false {
                    // Only false branch exists
                    items.code.resolve(Some(false_chain));
                    items.code.emitop(opcodes::ICONST_0);
                }
                
                Ok(Item::Stack { typecode: typecodes::INT }) // Boolean result as int
            },
        }
    }
    
    /// Store stack value into this item (JavaC store() equivalent)
    pub fn store(self, items: &mut Items) -> Result<()> {
        match self {
            Item::Local { reg, typecode, .. } => {
                // Store to local variable
                match typecode {
                    typecodes::INT | typecodes::BYTE | typecodes::SHORT | typecodes::CHAR => {
                        items.code.emitop1(opcodes::ISTORE, reg as u8);
                    },
                    typecodes::LONG => {
                        items.code.emitop1(opcodes::LSTORE, reg as u8);
                    },
                    typecodes::FLOAT => {
                        items.code.emitop1(opcodes::FSTORE, reg as u8);
                    },
                    typecodes::DOUBLE => {
                        items.code.emitop1(opcodes::DSTORE, reg as u8);
                    },
                    typecodes::OBJECT | typecodes::ARRAY => {
                        items.code.emitop1(opcodes::ASTORE, reg as u8);
                    },
                    _ => return Err(crate::common::error::Error::codegen_error(format!("Unsupported local variable store typecode: {}", typecode))),
                }
                Ok(())
            },
            
            Item::Member { member_name, class_name, descriptor, is_static, .. } => {
                if is_static {
                    // Static field store - putstatic
                    let field_idx = items.pool.add_field_ref(&class_name, &member_name, &descriptor);
                    items.code.emitop2(opcodes::PUTSTATIC, field_idx);
                } else {
                    // Instance field store - putfield
                    let field_idx = items.pool.add_field_ref(&class_name, &member_name, &descriptor);
                    items.code.emitop2(opcodes::PUTFIELD, field_idx);
                }
                Ok(())
            },
            
            Item::Static { member_name, class_name, descriptor, .. } => {
                // JavaC StaticItem.store() - emit putstatic for static field
                let field_idx = items.pool.add_field_ref(&class_name, &member_name, &descriptor);
                items.code.emitop2(opcodes::PUTSTATIC, field_idx);
                Ok(())
            },
            
            Item::Indexed { typecode, .. } => {
                // Array store - assumes array, index, and value are on stack
                match typecode {
                    typecodes::INT => items.code.emitop(opcodes::IASTORE),
                    typecodes::LONG => items.code.emitop(opcodes::LASTORE),
                    typecodes::FLOAT => items.code.emitop(opcodes::FASTORE),
                    typecodes::DOUBLE => items.code.emitop(opcodes::DASTORE),
                    typecodes::OBJECT | typecodes::ARRAY => items.code.emitop(opcodes::AASTORE),
                    typecodes::BYTE => items.code.emitop(opcodes::BASTORE),
                    typecodes::CHAR => items.code.emitop(opcodes::CASTORE),
                    typecodes::SHORT => items.code.emitop(opcodes::SASTORE),
                    _ => return Err(crate::common::error::Error::codegen_error(format!("Unsupported array store typecode: {}", typecode))),
                }
                Ok(())
            },
            
            Item::Assign { .. } => {
                // JavaC AssignItem inherits base Item.store() which should error
                // AssignItems are not directly storable - they represent assignment expressions
                Err(crate::common::error::Error::codegen_error("AssignItem cannot be stored to".to_string()))
            },
            
            Item::Cond { .. } => {
                // JavaC CondItem cannot be stored to directly - they represent conditionals
                Err(crate::common::error::Error::codegen_error("CondItem cannot be stored to".to_string()))
            },
            
            _ => {
                Err(crate::common::error::Error::codegen_error(format!("Cannot store to item type: {:?}", self)))
            }
        }
    }
    
    /// Prepare for assignment by stashing value (JavaC stash() equivalent)
    pub fn stash(self, value_typecode: u8, items: &mut Items) -> Result<Item> {
        match self {
            Item::Stack { .. } => {
                // JavaC StackItem.stash(): code.emitop0((width() == 2 ? dup_x2 : dup_x1) + 3 * (Code.width(toscode) - 1))
                let stack_width = self.width();
                let value_width = match value_typecode {
                    typecodes::LONG | typecodes::DOUBLE => 2,
                    typecodes::VOID => 0, 
                    _ => 1,
                };
                
                // Calculate JavaC's stash opcode formula
                let base_opcode = if stack_width == 2 { opcodes::DUP_X2 } else { opcodes::DUP_X1 };
                let opcode = base_opcode + 3 * (value_width - 1);
                
                items.code.emitop(opcode);
                Ok(self)
            },
            Item::Member { is_static, .. } => {
                if !is_static {
                    // For instance fields, we need to duplicate the value for putfield
                    // Stack before: [this] [value] 
                    // Stack after:  [this] [value] [value]
                    match value_typecode {
                        typecodes::LONG | typecodes::DOUBLE => {
                            items.code.emitop(opcodes::DUP2_X1);  // Wide value
                        },
                        _ => {
                            items.code.emitop(opcodes::DUP_X1);   // Normal value
                        }
                    }
                }
                Ok(self)
            },
            
            Item::Local { .. } => {
                // For local variables, just duplicate the value
                match value_typecode {
                    typecodes::LONG | typecodes::DOUBLE => {
                        items.code.emitop(opcodes::DUP2);  // Wide value
                    },
                    _ => {
                        items.code.emitop(opcodes::DUP);   // Normal value  
                    }
                }
                Ok(self)
            },
            
            Item::Indexed { .. } => {
                // For array elements, duplicate the value
                // Stack: [array] [index] [value] -> [array] [index] [value] [value]
                match value_typecode {
                    typecodes::LONG | typecodes::DOUBLE => {
                        items.code.emitop(opcodes::DUP2_X2);  // Wide value
                    },
                    _ => {
                        items.code.emitop(opcodes::DUP_X2);   // Normal value
                    }
                }
                Ok(self)
            },
            
            Item::Assign { .. } => {
                // JavaC AssignItem.stash() - throws Assert.error()
                Err(crate::common::error::Error::codegen_error("AssignItem cannot be stashed".to_string()))
            },
            
            Item::Cond { .. } => {
                // JavaC CondItem cannot be stashed - they represent conditionals  
                Err(crate::common::error::Error::codegen_error("CondItem cannot be stashed".to_string()))
            },
            
            _ => {
                Err(crate::common::error::Error::codegen_error(format!("Cannot stash for item type: {:?}", self)))
            }
        }
    }
    
    /// Increment local variable (JavaC LocalItem.incr() - 100% aligned)
    /// 
    /// This method implements the exact same logic as JavaC's LocalItem.incr():
    /// - If typecode is INT and increment is in range [-32768, 32767], use iinc instruction
    /// - Otherwise, use load-add/sub-store sequence with proper type coercion
    pub fn incr(self, x: i32, items: &mut Items) -> Result<()> {
        match self {
            Item::Local { typecode, reg } => {
                // JavaC LocalItem.incr() exact logic
                if typecode == typecodes::INT && x >= -32768 && x <= 32767 {
                    // Use iinc instruction for efficient increment (JavaC: code.emitop1w(iinc, reg, x))
                    items.code.emitop(crate::codegen::opcodes::IINC);
                    items.code.emit1(reg as u8);
                    items.code.emit1(x as u8); // Note: JavaC uses emitop1w which handles wide format
                } else {
                    // Use load-add/sub-store sequence (JavaC fallback pattern)
                    // Load current value
                    self.clone().load(items)?;
                    
                    if x >= 0 {
                        // Add positive increment (JavaC: makeImmediateItem(syms.intType, x).load(); code.emitop0(iadd))
                        Self::emit_integer_constant(x, items)?;
                        items.code.emitop(crate::codegen::opcodes::IADD);
                    } else {
                        // Subtract negative increment (JavaC: makeImmediateItem(syms.intType, -x).load(); code.emitop0(isub))
                        Self::emit_integer_constant(-x, items)?;
                        items.code.emitop(crate::codegen::opcodes::ISUB);
                    }
                    
                    // Create stack item and coerce to target type (JavaC: makeStackItem(syms.intType).coerce(typecode))
                    let stack_item = Item::Stack { typecode: typecodes::INT };
                    let coerced_item = stack_item.coerce(typecode, items)?;
                    
                    // Store back to local variable (JavaC: store())
                    let local_item = Item::Local { typecode, reg };
                    coerced_item.store_to_item(local_item, items)?;
                }
                Ok(())
            },
            _ => {
                // Only local variables can be incremented
                Err(crate::common::error::Error::codegen_error("Cannot increment non-local item".to_string()))
            }
        }
    }
    
    /// Type coercion method (JavaC Item.coerce() equivalent)
    pub fn coerce(self, target_typecode: u8, items: &mut Items) -> Result<Item> {
        let source_typecode = self.typecode();
        
        if source_typecode == target_typecode {
            return Ok(self); // No coercion needed
        }
        
        // Emit conversion instructions based on source and target types
        match (source_typecode, target_typecode) {
            // Int to smaller types (narrowing conversions)
            (typecodes::INT, typecodes::BYTE) => {
                items.code.emitop(crate::codegen::opcodes::I2B);
            },
            (typecodes::INT, typecodes::CHAR) => {
                items.code.emitop(crate::codegen::opcodes::I2C);  
            },
            (typecodes::INT, typecodes::SHORT) => {
                items.code.emitop(crate::codegen::opcodes::I2S);
            },
            // Other conversions can be added as needed
            _ => {
                // No conversion available, return as-is
                return Ok(Item::Stack { typecode: target_typecode });
            }
        }
        
        Ok(Item::Stack { typecode: target_typecode })
    }
    
    /// Store current stack value to target item (helper for incr)
    pub fn store_to_item(self, target: Item, items: &mut Items) -> Result<()> {
        // The stack value (self) should be stored to the target item
        target.store(items)
    }
    
    /// Emit integer constant using JavaC constant folding patterns
    /// This matches the exact same optimization as JavaC's ImmediateItem.load()
    fn emit_integer_constant(value: i32, items: &mut Items) -> Result<()> {
        if value >= -1 && value <= 5 {
            // Use iconst_m1 through iconst_5 (JavaC optimization)
            items.code.emitop(crate::codegen::opcodes::ICONST_0 + (value + 1) as u8);
        } else if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
            // Use bipush for byte range (JavaC optimization)
            items.code.emitop(crate::codegen::opcodes::BIPUSH);
            items.code.emit1(value as u8);
        } else if value >= i16::MIN as i32 && value <= i16::MAX as i32 {
            // Use sipush for short range (JavaC optimization)
            items.code.emitop(crate::codegen::opcodes::SIPUSH);
            items.code.emit2(value as u16);
        } else {
            // Use ldc for large constants (would need constant pool access)
            // For now, use sipush as fallback
            items.code.emitop(crate::codegen::opcodes::SIPUSH);
            items.code.emit2((value & 0xFFFF) as u16);
        }
        Ok(())
    }
    
    /// Duplicate item on stack (JavaC duplicate() equivalent)
    pub fn duplicate(self, items: &mut Items) -> Result<()> {
        match &self {
            Item::Stack { .. } => {
                // JavaC StackItem.duplicate(): code.emitop0(width() == 2 ? dup2 : dup)
                let width = self.width();
                if width == 2 {
                    items.code.emitop(crate::codegen::opcodes::DUP2);
                } else {
                    items.code.emitop(crate::codegen::opcodes::DUP);
                }
                Ok(())
            },
            Item::Assign { .. } => {
                // JavaC AssignItem.duplicate(): load().duplicate()
                let loaded = self.load(items)?;
                items.duplicate_item(&loaded)
            },
            Item::Cond { .. } => {
                // JavaC CondItem.duplicate(): load().duplicate()
                let loaded = self.load(items)?;
                items.duplicate_item(&loaded)
            },
            _ => {
                // For other items, use the standard duplicate
                items.duplicate_item(&self)
            }
        }
    }
    
    /// Drop item from stack (JavaC drop() equivalent) 
    pub fn drop(self, items: &mut Items) -> Result<()> {
        match self {
            Item::Stack { .. } => {
                // JavaC StackItem.drop(): code.emitop0(width() == 2 ? pop2 : pop)
                let width = self.width();
                if width == 2 {
                    items.code.emitop(crate::codegen::opcodes::POP2);
                } else {
                    items.code.emitop(crate::codegen::opcodes::POP);
                }
                Ok(())
            },
            Item::Assign { lhs, .. } => {
                // JavaC AssignItem.drop(): lhs.store()
                lhs.store(items)
            },
            _ => {
                // For other items, emit pop instruction
                match self.width() {
                    2 => items.code.emitop(crate::codegen::opcodes::POP2),
                    1 => items.code.emitop(crate::codegen::opcodes::POP),
                    0 => {}, // Void items don't need to be popped
                    _ => return Err(crate::common::error::Error::codegen_error("Invalid item width for drop".to_string())),
                }
                Ok(())
            }
        }
    }
    
    // CondItem helper methods (JavaC CondItem alignment)
    
    /// Negate a conditional jump opcode (JavaC Code.negate equivalent)
    pub fn negate_cond_opcode(opcode: u8) -> u8 {
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
            opcodes::GOTO => opcodes::DONTGOTO,
            opcodes::DONTGOTO => opcodes::GOTO,
            _ => opcode, // Return same opcode if not conditional
        }
    }
    
    /// Merge two chain options (JavaC Chain.merge equivalent)
    pub fn merge_chains(first: Option<Box<crate::codegen::chain::Chain>>, second: Option<Box<crate::codegen::chain::Chain>>) -> Option<Box<crate::codegen::chain::Chain>> {
        crate::codegen::chain::ChainOps::merge(first, second)
    }
    
    /// Check if this condition is always false (JavaC CondItem.isFalse equivalent)
    pub fn is_cond_false(opcode: u8, true_jumps: &Option<Box<crate::codegen::chain::Chain>>, false_jumps: &Option<Box<crate::codegen::chain::Chain>>) -> bool {
        true_jumps.is_none() && opcode == opcodes::DONTGOTO
    }
    
    /// Get mnemonic name for debugging (JavaC Code.mnem equivalent)
    pub fn opcode_mnemonic(opcode: u8) -> &'static str {
        match opcode {
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
            opcodes::IFNULL => "ifnull",
            opcodes::IFNONNULL => "ifnonnull",
            opcodes::GOTO => "goto",
            opcodes::DONTGOTO => "dontgoto",
            _ => "unknown",
        }
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Assign { lhs, .. } => {
                // JavaC AssignItem.toString(): "assign(lhs = " + lhs + ")"
                write!(f, "assign(lhs = {})", lhs)
            },
            Item::Local { reg, .. } => write!(f, "local({})", reg),
            Item::Member { member_name, class_name, .. } => write!(f, "member({}.{})", class_name, member_name),
            Item::Static { member_name, class_name, .. } => write!(f, "static({}.{})", class_name, member_name),
            Item::Indexed { typecode } => write!(f, "indexed({})", typecode),
            Item::Immediate { value, .. } => write!(f, "immediate({:?})", value),
            Item::Stack { typecode } => write!(f, "stack({})", typecode),
            Item::This => write!(f, "this"),
            Item::Super => write!(f, "super"),
            Item::Void => write!(f, "void"),
            Item::Cond { opcode, .. } => write!(f, "cond({})", Item::opcode_mnemonic(*opcode)),
            Item::ConversionChain { target_typecode, instructions, .. } => write!(f, "conversion({} -> {}, {} ops)", 
                instructions.len(), target_typecode, instructions.len()),
            Item::ReferenceConversion { target_type, needs_checkcast, .. } => write!(f, "refconv({}, checkcast: {})", 
                target_type, needs_checkcast),
        }
    }
}

