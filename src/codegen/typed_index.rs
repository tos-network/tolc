//! Typed constant pool indices for better type safety

use std::marker::PhantomData;
use std::ops::Deref;
use super::constpool::Constant;

/// Raw, untyped constant pool index
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct RawConstPoolIndex(u16);

impl RawConstPoolIndex {
    /// Get the raw u16 value
    pub fn as_u16(&self) -> u16 {
        self.0
    }
}

impl Deref for RawConstPoolIndex {
    type Target = u16;
    
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<u16> for RawConstPoolIndex {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl Into<usize> for RawConstPoolIndex {
    fn into(self) -> usize {
        self.0 as usize
    }
}

/// Typed index for a specific constant pool entry type
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct ConstPoolIndex<T: ConstPoolEntryInfo>(RawConstPoolIndex, PhantomData<T>);

impl<T: ConstPoolEntryInfo> ConstPoolIndex<T> {
    /// Create a raw index from this typed one
    pub fn as_raw(&self) -> RawConstPoolIndex {
        self.0
    }
    
    /// Get the raw u16 value
    pub fn as_u16(&self) -> u16 {
        self.0.as_u16()
    }
    
    /// Convert to bytes for serialization
    pub fn to_bytes(&self) -> [u8; 2] {
        self.0.as_u16().to_be_bytes()
    }
}

impl<T: ConstPoolEntryInfo> From<RawConstPoolIndex> for ConstPoolIndex<T> {
    fn from(raw: RawConstPoolIndex) -> Self {
        Self(raw, PhantomData)
    }
}

impl<T: ConstPoolEntryInfo> From<u16> for ConstPoolIndex<T> {
    fn from(value: u16) -> Self {
        Self(RawConstPoolIndex::from(value), PhantomData)
    }
}

impl<T: ConstPoolEntryInfo> Into<u16> for ConstPoolIndex<T> {
    fn into(self) -> u16 {
        self.0.as_u16()
    }
}

impl<T: ConstPoolEntryInfo> Into<usize> for ConstPoolIndex<T> {
    fn into(self) -> usize {
        self.0.into()
    }
}

impl<T: ConstPoolEntryInfo> Into<RawConstPoolIndex> for ConstPoolIndex<T> {
    fn into(self) -> RawConstPoolIndex {
        self.0
    }
}

/// Marker trait for constant pool entry types
pub trait ConstPoolEntryInfo: PartialEq + Eq {
    /// Check if a constant pool entry is valid for this type
    fn is_valid_entry(entry: &Constant) -> bool;
}

/// Type-safe indices for different constant pool entry types
pub type ClassIndex = ConstPoolIndex<ConstClassInfo>;
pub type StringIndex = ConstPoolIndex<ConstStringInfo>;
pub type NameAndTypeIndex = ConstPoolIndex<ConstNameAndTypeInfo>;
pub type FieldRefIndex = ConstPoolIndex<ConstFieldRefInfo>;
pub type MethodRefIndex = ConstPoolIndex<ConstMethodRefInfo>;
pub type InterfaceMethodRefIndex = ConstPoolIndex<ConstInterfaceMethodRefInfo>;
pub type MethodHandleIndex = ConstPoolIndex<ConstMethodHandleInfo>;
pub type MethodTypeIndex = ConstPoolIndex<ConstMethodTypeInfo>;
pub type DynamicIndex = ConstPoolIndex<ConstDynamicInfo>;
pub type InvokeDynamicIndex = ConstPoolIndex<ConstInvokeDynamicInfo>;

/// Marker types for constant pool entries
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstClassInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstStringInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstNameAndTypeInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstFieldRefInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstMethodRefInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstInterfaceMethodRefInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstMethodHandleInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstMethodTypeInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstDynamicInfo;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstInvokeDynamicInfo;

// Implement ConstPoolEntryInfo for each marker type
impl ConstPoolEntryInfo for ConstClassInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::Class(_))
    }
}

impl ConstPoolEntryInfo for ConstStringInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::String(_))
    }
}

impl ConstPoolEntryInfo for ConstNameAndTypeInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::NameAndType(_, _))
    }
}

impl ConstPoolEntryInfo for ConstFieldRefInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::FieldRef(_, _))
    }
}

impl ConstPoolEntryInfo for ConstMethodRefInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::MethodRef(_, _))
    }
}

impl ConstPoolEntryInfo for ConstInterfaceMethodRefInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::InterfaceMethodRef(_, _))
    }
}

impl ConstPoolEntryInfo for ConstMethodHandleInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::MethodHandle(_, _))
    }
}

impl ConstPoolEntryInfo for ConstMethodTypeInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::MethodType(_))
    }
}

impl ConstPoolEntryInfo for ConstDynamicInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::Dynamic(_, _))
    }
}

impl ConstPoolEntryInfo for ConstInvokeDynamicInfo {
    fn is_valid_entry(entry: &Constant) -> bool {
        matches!(entry, Constant::InvokeDynamic(_, _))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::constpool::Constant;
    
    #[test]
    fn test_typed_index_creation() {
        let raw_index = RawConstPoolIndex::from(42);
        let class_index: ClassIndex = raw_index.into();
        
        assert_eq!(class_index.as_u16(), 42);
        assert_eq!(class_index.as_raw(), raw_index);
    }
    
    #[test]
    fn test_typed_index_conversion() {
        let class_index = ClassIndex::from(100);
        let raw_index: RawConstPoolIndex = class_index.as_raw();
        let u16_value: u16 = class_index.as_u16();
        
        assert_eq!(raw_index.as_u16(), 100);
        assert_eq!(u16_value, 100);
    }
    
    #[test]
    fn test_const_pool_entry_info() {
        let class_constant = Constant::Class(42);
        let string_constant = Constant::String(100);
        
        assert!(ConstClassInfo::is_valid_entry(&class_constant));
        assert!(!ConstClassInfo::is_valid_entry(&string_constant));
        
        assert!(ConstStringInfo::is_valid_entry(&string_constant));
        assert!(!ConstStringInfo::is_valid_entry(&class_constant));
    }
}
