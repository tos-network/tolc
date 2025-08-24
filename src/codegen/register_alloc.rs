//! Register allocation for local variables (JavaC alignment)
//!
//! This module implements JavaC's register allocation strategy:
//! - nextreg tracks next available local variable slot
//! - newLocal() methods allocate slots with proper width handling
//! - makeTemp() creates temporary variables for intermediate computations
//! - Optimized slot reuse for better performance
//!
//! JavaC Reference: com.sun.tools.javac.jvm.Code.newLocal()

use crate::ast::{TypeEnum, PrimitiveType};
use crate::codegen::items::{Item, Items, typecodes};
use crate::codegen::code::Code;
use std::collections::HashMap;

/// Register allocation manager (JavaC alignment)
/// 
/// This struct manages local variable slot allocation following JavaC's exact pattern:
/// - Maintains nextreg counter for next available slot  
/// - Handles width requirements (long/double take 2 slots)
/// - Provides temporary variable creation
/// - Optimizes slot reuse when possible
pub struct RegisterAllocator {
    /// Next available register slot (JavaC nextreg equivalent)
    pub nextreg: u16,
    
    /// Temporary variable counter for unique names
    temp_counter: u32,
    
    /// Map of allocated variables by name for reuse optimization
    allocated_vars: HashMap<String, u16>,
}

impl RegisterAllocator {
    /// Create new register allocator (JavaC alignment)
    pub fn new() -> Self {
        Self {
            nextreg: 0,
            temp_counter: 0,
            allocated_vars: HashMap::new(),
        }
    }
    
    /// Create register allocator with initial offset for method parameters
    pub fn new_with_offset(initial_nextreg: u16) -> Self {
        Self {
            nextreg: initial_nextreg,
            temp_counter: 0,
            allocated_vars: HashMap::new(),
        }
    }
    
    /// Allocate new local variable slot by typecode (JavaC newLocal(int typecode))
    /// 
    /// JavaC Pattern:
    /// ```java
    /// private int newLocal(int typecode) {
    ///     int reg = nextreg;
    ///     int w = width(typecode);
    ///     nextreg = reg + w;
    ///     if (nextreg > max_locals) max_locals = nextreg;
    ///     return reg;
    /// }
    /// ```
    pub fn new_local_by_typecode(&mut self, typecode: u8, code: &mut Code) -> u16 {
        let reg = self.nextreg;
        let width = self.width_from_typecode(typecode);
        self.nextreg = reg + width;
        
        // Update max_locals if needed (JavaC pattern)
        if self.nextreg > code.max_locals {
            code.max_locals = self.nextreg;
        }
        
        reg
    }
    
    /// Allocate new local variable slot by type (JavaC newLocal(Type type))
    pub fn new_local_by_type(&mut self, typ: &TypeEnum, code: &mut Code) -> u16 {
        let typecode = self.type_to_typecode(typ);
        self.new_local_by_typecode(typecode, code)
    }
    
    /// Allocate named local variable slot with reuse optimization
    pub fn new_local_named(&mut self, name: String, typ: &TypeEnum, code: &mut Code) -> u16 {
        // Check if we can reuse an existing slot
        if let Some(&existing_reg) = self.allocated_vars.get(&name) {
            return existing_reg;
        }
        
        let reg = self.new_local_by_type(typ, code);
        self.allocated_vars.insert(name, reg);
        reg
    }
    
    /// Create temporary variable (JavaC makeTemp equivalent)
    /// 
    /// JavaC Pattern:
    /// ```java
    /// LocalItem makeTemp(Type type) {
    ///     VarSymbol v = new VarSymbol(Flags.SYNTHETIC,
    ///                                 names.empty,
    ///                                 type,
    ///                                 env.enclMethod.sym);
    ///     code.newLocal(v);
    ///     return items.makeLocalItem(v);
    /// }
    /// ```
    pub fn make_temp(&mut self, typ: &TypeEnum, items: &mut Items) -> Item {
        self.temp_counter += 1;
        let temp_name = format!("$temp_{}", self.temp_counter);
        let reg = self.new_local_named(temp_name, typ, items.code);
        items.make_local_item(typ, reg)
    }
    
    /// Start fresh register segment (JavaC newRegSegment)
    /// 
    /// JavaC Pattern:
    /// ```java
    /// public void newRegSegment() {
    ///     nextreg = max_locals;
    /// }
    /// ```
    pub fn new_reg_segment(&mut self, max_locals: u16) {
        self.nextreg = max_locals;
    }
    
    /// Reset register allocation state
    pub fn reset(&mut self) {
        self.nextreg = 0;
        self.temp_counter = 0;
        self.allocated_vars.clear();
    }
    
    /// Get current next register position
    pub fn current_nextreg(&self) -> u16 {
        self.nextreg
    }
    
    /// Reserve slots manually (for compiler-generated variables)
    pub fn reserve_slots(&mut self, count: u16, code: &mut Code) -> u16 {
        let start_reg = self.nextreg;
        self.nextreg += count;
        
        if self.nextreg > code.max_locals {
            code.max_locals = self.nextreg;
        }
        
        start_reg
    }
    
    /// Get width required for typecode (JavaC Code.width equivalent)
    pub fn width_from_typecode(&self, typecode: u8) -> u16 {
        match typecode {
            typecodes::LONG | typecodes::DOUBLE => 2,  // long and double take 2 slots
            typecodes::VOID => 0,                      // void takes no slots
            _ => 1,                                    // everything else takes 1 slot
        }
    }
    
    /// Convert TypeEnum to typecode for width calculation
    fn type_to_typecode(&self, typ: &TypeEnum) -> u8 {
        match typ {
            TypeEnum::Primitive(prim) => match prim {
                PrimitiveType::Boolean => typecodes::BYTE,  // boolean maps to byte
                PrimitiveType::Byte => typecodes::BYTE,
                PrimitiveType::Short => typecodes::SHORT,
                PrimitiveType::Char => typecodes::CHAR,
                PrimitiveType::Int => typecodes::INT,
                PrimitiveType::Long => typecodes::LONG,
                PrimitiveType::Float => typecodes::FLOAT,
                PrimitiveType::Double => typecodes::DOUBLE,
            },
            _ => typecodes::OBJECT,  // All reference types
        }
    }
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

/// Extension trait for Code to add register allocation methods (JavaC alignment)
pub trait CodeRegisterExt {
    /// Allocate new local variable slot by typecode (JavaC Code.newLocal)
    fn new_local_by_typecode(&mut self, typecode: u8, allocator: &mut RegisterAllocator) -> u16;
    
    /// Allocate new local variable slot by type
    fn new_local_by_type(&mut self, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> u16;
    
    /// Start fresh register segment (JavaC Code.newRegSegment)
    fn new_reg_segment(&mut self, allocator: &mut RegisterAllocator);
}

impl CodeRegisterExt for Code {
    fn new_local_by_typecode(&mut self, typecode: u8, allocator: &mut RegisterAllocator) -> u16 {
        allocator.new_local_by_typecode(typecode, self)
    }
    
    fn new_local_by_type(&mut self, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> u16 {
        allocator.new_local_by_type(typ, self)
    }
    
    fn new_reg_segment(&mut self, allocator: &mut RegisterAllocator) {
        allocator.new_reg_segment(self.max_locals)
    }
}

/// Extension trait for Items to add register allocation methods (JavaC alignment)
pub trait ItemsRegisterExt {
    /// Create temporary local variable item (JavaC Items.makeTemp)
    fn make_temp(&mut self, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> Item;
    
    /// Create local variable item with automatic slot allocation
    fn make_local_auto(&mut self, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> Item;
    
    /// Create named local variable item with reuse optimization
    fn make_local_named(&mut self, name: String, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> Item;
}

impl ItemsRegisterExt for Items<'_> {
    fn make_temp(&mut self, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> Item {
        allocator.make_temp(typ, self)
    }
    
    fn make_local_auto(&mut self, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> Item {
        let reg = allocator.new_local_by_type(typ, self.code);
        self.make_local_item(typ, reg)
    }
    
    fn make_local_named(&mut self, name: String, typ: &TypeEnum, allocator: &mut RegisterAllocator) -> Item {
        let reg = allocator.new_local_named(name, typ, self.code);
        self.make_local_item(typ, reg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::constpool::ConstantPool;

    #[test]
    fn test_register_allocator_basic() {
        let mut allocator = RegisterAllocator::new();
        let mut pool = ConstantPool::new();
        let mut code = Code::new(10, false, false);
        
        // Allocate INT slot
        let reg1 = allocator.new_local_by_typecode(typecodes::INT, &mut code);
        assert_eq!(reg1, 0);
        assert_eq!(allocator.nextreg, 1);
        
        // Allocate LONG slot (takes 2 slots)
        let reg2 = allocator.new_local_by_typecode(typecodes::LONG, &mut code);
        assert_eq!(reg2, 1);
        assert_eq!(allocator.nextreg, 3);
    }
    
    #[test]
    fn test_register_allocator_width() {
        let mut allocator = RegisterAllocator::new();
        let mut pool = ConstantPool::new();
        let mut code = Code::new(10, false, false);
        
        // Test width calculations
        assert_eq!(allocator.width_from_typecode(typecodes::INT), 1);
        assert_eq!(allocator.width_from_typecode(typecodes::LONG), 2);
        assert_eq!(allocator.width_from_typecode(typecodes::DOUBLE), 2);
        assert_eq!(allocator.width_from_typecode(typecodes::VOID), 0);
    }
}