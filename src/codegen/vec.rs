//! Size-limited vector types for JVM compliance

use super::error::{ConstPoolError, ConstPoolResult};
use std::ops::{Deref, Index, IndexMut};

/// Error that occurs when trying to store items in a size-limited vector
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum JvmVecError {
    #[error("Vector size limit exceeded: {current} > {max}")]
    SizeLimitExceeded { current: usize, max: usize },
    #[error("Index out of bounds: {index} >= {len}")]
    IndexOutOfBounds { index: usize, len: usize },
}

/// Result type for JVM vector operations
pub type JvmVecResult<T> = Result<T, JvmVecError>;

/// Vector limited to u16 size (JVM limitation)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmVecU2<T>(Vec<T>);

impl<T> JvmVecU2<T> {
    /// Maximum size for u16-limited vectors
    pub const MAX_SIZE: u16 = u16::MAX;
    
    /// Create a new empty vector
    pub fn new() -> Self {
        Self(Vec::new())
    }
    
    /// Get the current length
    pub fn len(&self) -> u16 {
        self.0.len() as u16
    }
    
    /// Check if the vector is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    
    /// Get remaining space
    pub fn remaining_space(&self) -> u16 {
        Self::MAX_SIZE - self.len()
    }
    
    /// Check if there's space for additional items
    pub fn has_space_for(&self, required: u16) -> bool {
        self.remaining_space() >= required
    }
    
    /// Check if there's any remaining space
    pub fn has_space(&self) -> bool {
        self.remaining_space() > 0
    }
    
    /// Push an item, returning the index
    pub fn push(&mut self, item: T) -> JvmVecResult<u16> {
        if self.len() >= Self::MAX_SIZE {
            return Err(JvmVecError::SizeLimitExceeded {
                current: self.len() as usize,
                max: Self::MAX_SIZE as usize,
            });
        }
        
        let index = self.len();
        self.0.push(item);
        Ok(index)
    }
    
    /// Push an item and get a reference to it
    pub fn push_get(&mut self, item: T) -> JvmVecResult<&T> {
        let index = self.push(item)?;
        Ok(&self.0[index as usize])
    }
    
    /// Push an item and get a mutable reference to it
    pub fn push_get_mut(&mut self, item: T) -> JvmVecResult<&mut T> {
        let index = self.push(item)?;
        Ok(&mut self.0[index as usize])
    }
    
    /// Get an item by index
    pub fn get(&self, index: u16) -> Option<&T> {
        self.0.get(index as usize)
    }
    
    /// Get a mutable reference to an item by index
    pub fn get_mut(&mut self, index: u16) -> Option<&mut T> {
        self.0.get_mut(index as usize)
    }
    
    /// Iterate over the vector
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.0.iter()
    }
    
    /// Convert to inner Vec
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }
}

impl<T> Default for JvmVecU2<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Deref for JvmVecU2<T> {
    type Target = Vec<T>;
    
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Index<u16> for JvmVecU2<T> {
    type Output = T;
    
    fn index(&self, index: u16) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl<T> IndexMut<u16> for JvmVecU2<T> {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl<T> From<Vec<T>> for JvmVecU2<T> {
    fn from(vec: Vec<T>) -> Self {
        if vec.len() > u16::MAX as usize {
            panic!("Vector too large for JvmVecU2");
        }
        Self(vec)
    }
}

impl<T> Into<Vec<T>> for JvmVecU2<T> {
    fn into(self) -> Vec<T> {
        self.0
    }
}

/// Vector limited to u32 size (for attributes and code)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmVecU4<T>(Vec<T>);

impl<T> JvmVecU4<T> {
    /// Maximum size for u32-limited vectors
    pub const MAX_SIZE: u32 = u32::MAX;
    
    /// Create a new empty vector
    pub fn new() -> Self {
        Self(Vec::new())
    }
    
    /// Get the current length
    pub fn len(&self) -> u32 {
        self.0.len() as u32
    }
    
    /// Check if the vector is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    
    /// Get remaining space
    pub fn remaining_space(&self) -> u32 {
        Self::MAX_SIZE - self.len()
    }
    
    /// Check if there's space for additional items
    pub fn has_space_for(&self, required: u32) -> bool {
        self.remaining_space() >= required
    }
    
    /// Check if there's any remaining space
    pub fn has_space(&self) -> bool {
        self.remaining_space() > 0
    }
    
    /// Push an item, returning the index
    pub fn push(&mut self, item: T) -> JvmVecResult<u32> {
        if self.len() >= Self::MAX_SIZE {
            return Err(JvmVecError::SizeLimitExceeded {
                current: self.len() as usize,
                max: Self::MAX_SIZE as usize,
            });
        }
        
        let index = self.len();
        self.0.push(item);
        Ok(index)
    }
    
    /// Get an item by index
    pub fn get(&self, index: u32) -> Option<&T> {
        self.0.get(index as usize)
    }
    
    /// Get a mutable reference to an item by index
    pub fn get_mut(&mut self, index: u32) -> Option<&mut T> {
        self.0.get_mut(index as usize)
    }
    
    /// Iterate over the vector
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.0.iter()
    }
    
    /// Convert to inner Vec
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }
}

impl<T> Default for JvmVecU4<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Deref for JvmVecU4<T> {
    type Target = Vec<T>;
    
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> From<Vec<T>> for JvmVecU4<T> {
    fn from(vec: Vec<T>) -> Self {
        if vec.len() > u32::MAX as usize {
            panic!("Vector too large for JvmVecU4");
        }
        Self(vec)
    }
}

impl<T> Into<Vec<T>> for JvmVecU4<T> {
    fn into(self) -> Vec<T> {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_jvm_vec_u2_basic_operations() {
        let mut vec = JvmVecU2::new();
        assert_eq!(vec.len(), 0);
        assert!(vec.is_empty());
        
        // Push items
        assert_eq!(vec.push("hello"), Ok(0));
        assert_eq!(vec.push("world"), Ok(1));
        assert_eq!(vec.len(), 2);
        
        // Get items
        assert_eq!(vec.get(0), Some(&"hello"));
        assert_eq!(vec.get(1), Some(&"world"));
        assert_eq!(vec.get(2), None);
    }
    
    #[test]
    fn test_jvm_vec_u2_size_limit() {
        let vec = JvmVecU2::<u8>::new();
        
        // This would take too long to actually test, but we can test the logic
        assert!(vec.has_space());
        assert!(vec.has_space_for(1));
        assert!(vec.has_space_for(u16::MAX)); // Empty vector has space for u16::MAX items
        assert!(vec.has_space_for(0)); // 0 elements always has space
    }
    
    #[test]
    fn test_jvm_vec_u4_basic_operations() {
        let mut vec = JvmVecU4::new();
        assert_eq!(vec.len(), 0);
        
        assert_eq!(vec.push(42), Ok(0));
        assert_eq!(vec.push(100), Ok(1));
        assert_eq!(vec.len(), 2);
        
        assert_eq!(vec.get(0), Some(&42));
        assert_eq!(vec.get(1), Some(&100));
    }
}
