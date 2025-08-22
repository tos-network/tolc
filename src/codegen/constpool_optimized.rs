//! Optimized Constant Pool Implementation
//! 
//! This module provides high-performance constant pool management with:
//! - String interning to reduce memory allocations
//! - Faster HashMap lookups with pre-hashed keys
//! - Optimized serialization with size pre-computation
//! - Cache-friendly data structures for hot paths

use thiserror::Error;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ConstPoolError {
    #[error("Constant pool size limit exceeded: current={current}, adding={adding}, max={max}")]
    SizeLimitExceeded { current: usize, adding: usize, max: usize },
}

/// Interned string for efficient storage and comparison
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternedString(Arc<String>);

impl InternedString {
    pub fn new(s: &str) -> Self {
        Self(Arc::new(s.to_string()))
    }
    
    pub fn as_str(&self) -> &str {
        &self.0
    }
    
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

/// Pre-hashed key for faster HashMap lookups
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreHashedKey<T> {
    key: T,
    hash: u64,
}

impl<T: Hash> PreHashedKey<T> {
    pub fn new(key: T) -> Self {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        Self {
            key,
            hash: hasher.finish(),
        }
    }
}

impl<T> Hash for PreHashedKey<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

/// Optimized constant pool with performance enhancements
#[derive(Debug, Clone)]
pub struct OptimizedConstantPool {
    /// Main constant storage
    constants: Vec<Constant>,
    
    /// String interning table
    string_interner: HashMap<PreHashedKey<String>, InternedString>,
    
    /// Fast lookup maps with pre-hashed keys
    utf8_map: HashMap<PreHashedKey<InternedString>, u16>,
    class_map: HashMap<PreHashedKey<InternedString>, u16>,
    name_and_type_map: HashMap<PreHashedKey<(InternedString, InternedString)>, u16>,
    string_map: HashMap<PreHashedKey<InternedString>, u16>,
    fieldref_map: HashMap<PreHashedKey<(InternedString, InternedString, InternedString)>, u16>,
    methodref_map: HashMap<PreHashedKey<(InternedString, InternedString, InternedString)>, u16>,
    
    /// Pre-computed serialization sizes for faster bytecode generation
    serialized_sizes: Vec<u32>,
    total_size: u32,
}

/// Optimized constant representation with size hints
#[derive(Debug, Clone)]
pub enum Constant {
    Utf8(InternedString),
    Integer(i32),
    Float(f32), 
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    FieldRef(u16, u16),
    MethodRef(u16, u16),
    InterfaceMethodRef(u16, u16),
    NameAndType(u16, u16),
    MethodHandle(u8, u16),
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16),
    Placeholder,
}

impl Constant {
    /// Get the serialized size of this constant (cached for performance)
    pub fn serialized_size(&self) -> u32 {
        match self {
            Constant::Utf8(s) => 1 + 2 + s.len() as u32, // tag + length + bytes
            Constant::Integer(_) => 1 + 4, // tag + 4 bytes
            Constant::Float(_) => 1 + 4,   // tag + 4 bytes
            Constant::Long(_) => 1 + 8,    // tag + 8 bytes
            Constant::Double(_) => 1 + 8,  // tag + 8 bytes
            Constant::Class(_) => 1 + 2,   // tag + name_index
            Constant::String(_) => 1 + 2,  // tag + string_index
            Constant::FieldRef(_, _) => 1 + 2 + 2, // tag + class_index + nat_index
            Constant::MethodRef(_, _) => 1 + 2 + 2, // tag + class_index + nat_index
            Constant::InterfaceMethodRef(_, _) => 1 + 2 + 2, // tag + class_index + nat_index
            Constant::NameAndType(_, _) => 1 + 2 + 2, // tag + name_index + desc_index
            Constant::MethodHandle(_, _) => 1 + 1 + 2, // tag + kind + ref_index
            Constant::MethodType(_) => 1 + 2, // tag + desc_index
            Constant::Dynamic(_, _) => 1 + 2 + 2, // tag + bsm_index + nat_index
            Constant::InvokeDynamic(_, _) => 1 + 2 + 2, // tag + bsm_index + nat_index
            Constant::Module(_) => 1 + 2, // tag + name_index
            Constant::Package(_) => 1 + 2, // tag + name_index
            Constant::Placeholder => 0, // Not serialized
        }
    }
    
    /// Fast serialization with pre-allocated buffer
    pub fn serialize_to(&self, buffer: &mut Vec<u8>) {
        // JVM constant pool tag constants (from JVM spec)
        const CONSTANT_UTF8: u8 = 1;
        const CONSTANT_INTEGER: u8 = 3;
        const CONSTANT_FLOAT: u8 = 4;
        const CONSTANT_LONG: u8 = 5;
        const CONSTANT_DOUBLE: u8 = 6;
        const CONSTANT_CLASS: u8 = 7;
        const CONSTANT_STRING: u8 = 8;
        const CONSTANT_FIELDREF: u8 = 9;
        const CONSTANT_METHODREF: u8 = 10;
        const CONSTANT_INTERFACEMETHODREF: u8 = 11;
        const CONSTANT_NAMEANDTYPE: u8 = 12;
        const CONSTANT_METHODHANDLE: u8 = 15;
        const CONSTANT_METHODTYPE: u8 = 16;
        const CONSTANT_DYNAMIC: u8 = 17;
        const CONSTANT_INVOKEDYNAMIC: u8 = 18;
        const CONSTANT_MODULE: u8 = 19;
        const CONSTANT_PACKAGE: u8 = 20;
        
        match self {
            Constant::Utf8(s) => {
                buffer.push(CONSTANT_UTF8);
                let len = s.len() as u16;
                buffer.extend_from_slice(&len.to_be_bytes());
                buffer.extend_from_slice(s.as_str().as_bytes());
            }
            Constant::Integer(val) => {
                buffer.push(CONSTANT_INTEGER);
                buffer.extend_from_slice(&val.to_be_bytes());
            }
            Constant::Float(val) => {
                buffer.push(CONSTANT_FLOAT);
                buffer.extend_from_slice(&val.to_be_bytes());
            }
            Constant::Long(val) => {
                buffer.push(CONSTANT_LONG);
                buffer.extend_from_slice(&val.to_be_bytes());
            }
            Constant::Double(val) => {
                buffer.push(CONSTANT_DOUBLE);
                buffer.extend_from_slice(&val.to_be_bytes());
            }
            Constant::Class(name_index) => {
                buffer.push(CONSTANT_CLASS);
                buffer.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::String(string_index) => {
                buffer.push(CONSTANT_STRING);
                buffer.extend_from_slice(&string_index.to_be_bytes());
            }
            Constant::FieldRef(class_index, nat_index) => {
                buffer.push(CONSTANT_FIELDREF);
                buffer.extend_from_slice(&class_index.to_be_bytes());
                buffer.extend_from_slice(&nat_index.to_be_bytes());
            }
            Constant::MethodRef(class_index, nat_index) => {
                buffer.push(CONSTANT_METHODREF);
                buffer.extend_from_slice(&class_index.to_be_bytes());
                buffer.extend_from_slice(&nat_index.to_be_bytes());
            }
            Constant::InterfaceMethodRef(class_index, nat_index) => {
                buffer.push(CONSTANT_INTERFACEMETHODREF);
                buffer.extend_from_slice(&class_index.to_be_bytes());
                buffer.extend_from_slice(&nat_index.to_be_bytes());
            }
            Constant::NameAndType(name_index, desc_index) => {
                buffer.push(CONSTANT_NAMEANDTYPE);
                buffer.extend_from_slice(&name_index.to_be_bytes());
                buffer.extend_from_slice(&desc_index.to_be_bytes());
            }
            Constant::MethodHandle(kind, ref_index) => {
                buffer.push(CONSTANT_METHODHANDLE);
                buffer.push(*kind);
                buffer.extend_from_slice(&ref_index.to_be_bytes());
            }
            Constant::MethodType(desc_index) => {
                buffer.push(CONSTANT_METHODTYPE);
                buffer.extend_from_slice(&desc_index.to_be_bytes());
            }
            Constant::Dynamic(bsm_index, nat_index) => {
                buffer.push(CONSTANT_DYNAMIC);
                buffer.extend_from_slice(&bsm_index.to_be_bytes());
                buffer.extend_from_slice(&nat_index.to_be_bytes());
            }
            Constant::InvokeDynamic(bsm_index, nat_index) => {
                buffer.push(CONSTANT_INVOKEDYNAMIC);
                buffer.extend_from_slice(&bsm_index.to_be_bytes());
                buffer.extend_from_slice(&nat_index.to_be_bytes());
            }
            Constant::Module(name_index) => {
                buffer.push(CONSTANT_MODULE);
                buffer.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::Package(name_index) => {
                buffer.push(CONSTANT_PACKAGE);
                buffer.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::Placeholder => {
                // Placeholders are not serialized
            }
        }
    }
}

impl OptimizedConstantPool {
    /// Create new optimized constant pool
    pub fn new() -> Self {
        Self {
            constants: Vec::with_capacity(256), // Pre-allocate for common case
            string_interner: HashMap::with_capacity(128),
            utf8_map: HashMap::with_capacity(128),
            class_map: HashMap::with_capacity(64),
            name_and_type_map: HashMap::with_capacity(64),
            string_map: HashMap::with_capacity(32),
            fieldref_map: HashMap::with_capacity(32),
            methodref_map: HashMap::with_capacity(32),
            serialized_sizes: Vec::with_capacity(256),
            total_size: 0,
        }
    }
    
    /// Intern a string for efficient reuse
    fn intern_string(&mut self, s: &str) -> InternedString {
        let key = PreHashedKey::new(s.to_string());
        if let Some(interned) = self.string_interner.get(&key) {
            interned.clone()
        } else {
            let interned = InternedString::new(s);
            self.string_interner.insert(key, interned.clone());
            interned
        }
    }
    
    /// Add UTF8 constant with optimized string handling
    pub fn add_utf8_optimized(&mut self, value: &str) -> Result<u16, ConstPoolError> {
        let interned = self.intern_string(value);
        let key = PreHashedKey::new(interned.clone());
        
        if let Some(&idx) = self.utf8_map.get(&key) {
            return Ok(idx);
        }
        
        self.ensure_space(1)?;
        
        let constant = Constant::Utf8(interned.clone());
        let size = constant.serialized_size();
        
        self.constants.push(constant);
        self.serialized_sizes.push(size);
        self.total_size += size;
        
        let idx = self.constants.len() as u16;
        self.utf8_map.insert(key, idx);
        
        Ok(idx)
    }
    
    /// Add class constant with optimized lookups
    pub fn add_class_optimized(&mut self, name: &str) -> Result<u16, ConstPoolError> {
        let interned_name = self.intern_string(name);
        let key = PreHashedKey::new(interned_name.clone());
        
        if let Some(&idx) = self.class_map.get(&key) {
            return Ok(idx);
        }
        
        self.ensure_space(2)?; // UTF8 + Class in worst case
        
        // Add UTF8 for class name
        let name_utf8_idx = self.add_utf8_optimized(name)?;
        
        // Add Class constant
        let constant = Constant::Class(name_utf8_idx);
        let size = constant.serialized_size();
        
        self.constants.push(constant);
        self.serialized_sizes.push(size);
        self.total_size += size;
        
        let idx = self.constants.len() as u16;
        self.class_map.insert(key, idx);
        
        Ok(idx)
    }
    
    /// Fast serialization with pre-computed sizes
    pub fn serialize_optimized(&self) -> Vec<u8> {
        let mut buffer = Vec::with_capacity(self.total_size as usize + 4); // +4 for count
        
        // Write constant pool count
        let count = (self.constants.len() + 1) as u16;
        buffer.extend_from_slice(&count.to_be_bytes());
        
        // Serialize constants using optimized method
        for constant in &self.constants {
            constant.serialize_to(&mut buffer);
        }
        
        buffer
    }
    
    fn ensure_space(&self, adding: usize) -> Result<(), ConstPoolError> {
        let count_after = self.constants.len() + adding + 1;
        if count_after > u16::MAX as usize {
            return Err(ConstPoolError::SizeLimitExceeded { 
                current: self.constants.len(), 
                adding, 
                max: (u16::MAX as usize) - 1 
            });
        }
        Ok(())
    }
    
    /// Get total serialized size (cached)
    pub fn total_serialized_size(&self) -> u32 {
        self.total_size + 2 // +2 for count field
    }
    
    /// Get number of constants
    pub fn len(&self) -> usize {
        self.constants.len()
    }
    
    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.constants.is_empty()
    }
}

impl Default for OptimizedConstantPool {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_string_interning() {
        let mut pool = OptimizedConstantPool::new();
        
        // Same strings should return same interned instance
        let s1 = pool.intern_string("test");
        let s2 = pool.intern_string("test");
        
        assert_eq!(s1, s2);
        assert_eq!(Arc::ptr_eq(&s1.0, &s2.0), true); // Same Arc instance
    }
    
    #[test]
    fn test_utf8_optimization() {
        let mut pool = OptimizedConstantPool::new();
        
        // First add should create new constant
        let idx1 = pool.add_utf8_optimized("hello").unwrap();
        assert_eq!(idx1, 1);
        
        // Second add of same string should return same index
        let idx2 = pool.add_utf8_optimized("hello").unwrap();
        assert_eq!(idx2, 1);
        
        // Different string should get new index
        let idx3 = pool.add_utf8_optimized("world").unwrap();
        assert_eq!(idx3, 2);
    }
    
    #[test]
    fn test_serialization_size_caching() {
        let mut pool = OptimizedConstantPool::new();
        
        pool.add_utf8_optimized("test").unwrap();
        pool.add_class_optimized("java/lang/Object").unwrap();
        
        let total_size = pool.total_serialized_size();
        assert!(total_size > 0);
        
        // Size should be cached and consistent
        assert_eq!(total_size, pool.total_serialized_size());
    }
    
    #[test]
    fn test_fast_serialization() {
        let mut pool = OptimizedConstantPool::new();
        
        pool.add_utf8_optimized("hello").unwrap();
        pool.add_utf8_optimized("world").unwrap();
        pool.add_class_optimized("Test").unwrap();
        
        let serialized = pool.serialize_optimized();
        
        // Should have proper constant pool format
        let count = u16::from_be_bytes([serialized[0], serialized[1]]);
        assert_eq!(count, pool.len() as u16 + 1);
    }
}