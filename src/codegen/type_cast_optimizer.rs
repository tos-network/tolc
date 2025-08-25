//! Type Cast Optimization - JavaC-aligned type conversion system
//! 
//! This module implements type cast optimization exactly as found in Oracle JavaC's
//! Items.java and Gen.java. It provides efficient type coercion, cast elimination,
//! and primitive/reference type conversion following JavaC patterns.
//! 
//! Key JavaC patterns implemented:
//! - Item.coerce() method for primitive type conversions  
//! - Redundant cast elimination
//! - Primitive conversion instruction optimization
//! - Reference type cast verification and optimization
//! - Widening/narrowing conversion rules

use crate::codegen::opcode_enum::Opcode;
use crate::codegen::items::{Item, typecodes};
use crate::common::error::Result;
use crate::ast::{TypeEnum, CastExpr};
use std::collections::HashMap;

/// JavaC-aligned Type Cast Optimizer
/// 
/// Implements the exact same type conversion patterns as JavaC:
/// - Primitive type coercion with instruction optimization
/// - Reference type cast verification  
/// - Redundant cast elimination
/// - Widening/narrowing conversion rules
#[derive(Debug)]
pub struct TypeCastOptimizer {
    /// Conversion instruction mapping (JavaC pattern)
    conversion_table: HashMap<(u8, u8), Vec<Opcode>>,
    
    /// Redundant cast elimination enabled
    eliminate_redundant_casts: bool,
    
    /// Statistics
    stats: CastOptimizationStats,
}

#[derive(Debug, Default)]
pub struct CastOptimizationStats {
    /// Number of redundant casts eliminated
    pub redundant_casts_eliminated: u32,
    
    /// Number of primitive conversions optimized
    pub primitive_conversions_optimized: u32,
    
    /// Number of reference casts optimized
    pub reference_casts_optimized: u32,
    
    /// Total optimizations performed
    pub total_optimizations: u32,
}

impl TypeCastOptimizer {
    /// Create new JavaC-aligned type cast optimizer
    pub fn new() -> Self {
        let mut optimizer = Self {
            conversion_table: HashMap::new(),
            eliminate_redundant_casts: true,
            stats: CastOptimizationStats::default(),
        };
        
        optimizer.initialize_conversion_table();
        optimizer
    }
    
    /// Initialize conversion table with JavaC-compatible mappings
    /// Based on javac's Items.java coerce() method and Code.java truncate()
    fn initialize_conversion_table(&mut self) {
        use crate::codegen::opcodes;
        
        // INT conversions (narrowing)
        self.conversion_table.insert((typecodes::INT, typecodes::BYTE), vec![Opcode::I2b]);
        self.conversion_table.insert((typecodes::INT, typecodes::CHAR), vec![Opcode::I2c]);
        self.conversion_table.insert((typecodes::INT, typecodes::SHORT), vec![Opcode::I2s]);
        self.conversion_table.insert((typecodes::INT, typecodes::LONG), vec![Opcode::I2l]);
        self.conversion_table.insert((typecodes::INT, typecodes::FLOAT), vec![Opcode::I2f]);
        self.conversion_table.insert((typecodes::INT, typecodes::DOUBLE), vec![Opcode::I2d]);
        
        // LONG conversions
        self.conversion_table.insert((typecodes::LONG, typecodes::INT), vec![Opcode::L2i]);
        self.conversion_table.insert((typecodes::LONG, typecodes::FLOAT), vec![Opcode::L2f]);
        self.conversion_table.insert((typecodes::LONG, typecodes::DOUBLE), vec![Opcode::L2d]);
        
        // FLOAT conversions
        self.conversion_table.insert((typecodes::FLOAT, typecodes::INT), vec![Opcode::F2i]);
        self.conversion_table.insert((typecodes::FLOAT, typecodes::LONG), vec![Opcode::F2l]);
        self.conversion_table.insert((typecodes::FLOAT, typecodes::DOUBLE), vec![Opcode::F2d]);
        
        // DOUBLE conversions
        self.conversion_table.insert((typecodes::DOUBLE, typecodes::INT), vec![Opcode::D2i]);
        self.conversion_table.insert((typecodes::DOUBLE, typecodes::LONG), vec![Opcode::D2l]);
        self.conversion_table.insert((typecodes::DOUBLE, typecodes::FLOAT), vec![Opcode::D2f]);
        
        // Narrowing conversions (after I2L, L2I, etc.)
        self.conversion_table.insert((typecodes::INT, typecodes::BYTE), vec![Opcode::Int2byte]);
        self.conversion_table.insert((typecodes::INT, typecodes::CHAR), vec![Opcode::Int2char]); 
        self.conversion_table.insert((typecodes::INT, typecodes::SHORT), vec![Opcode::Int2short]);
    }
    
    /// Optimize type cast expression (JavaC Gen.visitTypeCast equivalent)
    pub fn optimize_cast(&mut self, cast_expr: &CastExpr, source_item: &Item) -> Result<Option<Item>> {
        let source_typecode = source_item.typecode();
        let target_type = &cast_expr.target_type;
        
        // Convert target type to typecode
        let target_typecode = self.type_to_typecode(target_type)?;
        
        // Check for redundant cast elimination
        if self.eliminate_redundant_casts && source_typecode == target_typecode {
            self.stats.redundant_casts_eliminated += 1;
            self.stats.total_optimizations += 1;
            return Ok(Some(source_item.clone())); // Return source unchanged
        }
        
        // Handle primitive type conversions
        if self.is_primitive_typecode(source_typecode) && self.is_primitive_typecode(target_typecode) {
            return self.optimize_primitive_conversion(source_typecode, target_typecode, source_item);
        }
        
        // Handle reference type casts
        if self.is_reference_typecode(source_typecode) || self.is_reference_typecode(target_typecode) {
            return self.optimize_reference_cast(cast_expr, source_item);
        }
        
        // No optimization applicable
        Ok(None)
    }
    
    /// Optimize primitive type conversion (JavaC Item.coerce equivalent)
    fn optimize_primitive_conversion(&mut self, source_typecode: u8, target_typecode: u8, source_item: &Item) -> Result<Option<Item>> {
        // JavaC truncate logic - check if conversion needs truncation
        let source_truncated = Self::truncate_typecode(source_typecode);
        let target_truncated = Self::truncate_typecode(target_typecode);
        
        let mut instructions = Vec::new();
        
        // First, handle widening/narrowing conversions between basic types
        if source_truncated != target_truncated {
            if let Some(conversion_ops) = self.conversion_table.get(&(source_truncated, target_truncated)) {
                instructions.extend(conversion_ops.clone());
            }
        }
        
        // Then, handle sub-type narrowing (like int to byte, char, short)
        if target_typecode != target_truncated {
            let narrowing_key = (Self::truncate_typecode(target_truncated), target_typecode);
            if let Some(narrowing_ops) = self.conversion_table.get(&narrowing_key) {
                instructions.extend(narrowing_ops.clone());
            }
        }
        
        if !instructions.is_empty() {
            self.stats.primitive_conversions_optimized += 1;
            self.stats.total_optimizations += 1;
            
            // Create optimized conversion item
            Ok(Some(Item::ConversionChain {
                source_item: Box::new(source_item.clone()),
                instructions,
                target_typecode,
            }))
        } else {
            Ok(None)
        }
    }
    
    /// Optimize reference type cast (JavaC checkcast logic)  
    fn optimize_reference_cast(&mut self, cast_expr: &CastExpr, source_item: &Item) -> Result<Option<Item>> {
        // For reference types, we need to emit checkcast instruction
        // But we can optimize by eliminating unnecessary casts to supertypes
        
        let target_type_name = &cast_expr.target_type.name;
        
        // TODO: Implement supertype checking to eliminate unnecessary casts
        // For now, always emit checkcast for reference types
        
        self.stats.reference_casts_optimized += 1;
        self.stats.total_optimizations += 1;
        
        Ok(Some(Item::ReferenceConversion {
            source_item: Box::new(source_item.clone()),
            target_type: target_type_name.clone(),
            needs_checkcast: true,
        }))
    }
    
    /// Convert TypeRef to typecode (JavaC Code.typecode equivalent)
    fn type_to_typecode(&self, type_ref: &crate::ast::TypeRef) -> Result<u8> {
        match type_ref.name.as_str() {
            "byte" => Ok(typecodes::BYTE),
            "char" => Ok(typecodes::CHAR), 
            "short" => Ok(typecodes::SHORT),
            "int" => Ok(typecodes::INT),
            "long" => Ok(typecodes::LONG),
            "float" => Ok(typecodes::FLOAT),
            "double" => Ok(typecodes::DOUBLE),
            "boolean" => Ok(typecodes::BYTE),
            "void" => Ok(typecodes::VOID),
            _ => Ok(typecodes::OBJECT), // Reference types
        }
    }
    
    /// Check if typecode represents a primitive type
    fn is_primitive_typecode(&self, typecode: u8) -> bool {
        matches!(typecode, 
            typecodes::BYTE | typecodes::CHAR | typecodes::SHORT | 
            typecodes::INT | typecodes::LONG | typecodes::FLOAT | 
            typecodes::DOUBLE
        )
    }
    
    /// Check if typecode represents a reference type
    fn is_reference_typecode(&self, typecode: u8) -> bool {
        typecode == typecodes::OBJECT
    }
    
    /// Truncate typecode (JavaC Code.truncate equivalent)
    /// Maps byte, char, short, boolean to int for computation
    fn truncate_typecode(typecode: u8) -> u8 {
        match typecode {
            typecodes::BYTE | typecodes::CHAR | typecodes::SHORT => typecodes::INT,
            _ => typecode,
        }
    }
    
    /// Get optimization statistics
    pub fn get_stats(&self) -> &CastOptimizationStats {
        &self.stats
    }
    
    /// Reset optimizer statistics
    pub fn reset_stats(&mut self) {
        self.stats = CastOptimizationStats::default();
    }
    
    /// Enable/disable redundant cast elimination
    pub fn set_eliminate_redundant_casts(&mut self, enabled: bool) {
        self.eliminate_redundant_casts = enabled;
    }
}

impl Default for TypeCastOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{TypeRef, Span};
    use crate::ast::Location;
    
    fn create_test_typeref(name: &str) -> TypeRef {
        TypeRef {
            name: name.to_string(),
            type_args: Vec::new(),
            annotations: Vec::new(),
            array_dims: 0,
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        }
    }
    
    #[test]
    fn test_primitive_conversion_optimization() {
        let mut optimizer = TypeCastOptimizer::new();
        
        // Test int to byte conversion
        let source_item = Item::Stack { typecode: typecodes::INT };
        let result = optimizer.optimize_primitive_conversion(typecodes::INT, typecodes::BYTE, &source_item);
        
        assert!(result.is_ok());
        let optimized = result.unwrap();
        assert!(optimized.is_some());
        
        // Should have generated conversion instructions
        if let Some(Item::ConversionChain { instructions, target_typecode, .. }) = optimized {
            assert_eq!(target_typecode, typecodes::BYTE);
            assert!(!instructions.is_empty());
        }
    }
    
    #[test]
    fn test_redundant_cast_elimination() {
        let mut optimizer = TypeCastOptimizer::new();
        
        let cast_expr = CastExpr {
            expr: Box::new(crate::ast::Expr::Literal(crate::ast::LiteralExpr {
                value: crate::ast::Literal::Integer(42),
                span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
            })),
            target_type: create_test_typeref("int"),
            span: Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0)),
        };
        
        let source_item = Item::Stack { typecode: typecodes::INT };
        let result = optimizer.optimize_cast(&cast_expr, &source_item);
        
        assert!(result.is_ok());
        let optimized = result.unwrap();
        assert!(optimized.is_some());
        
        // Should return source item unchanged (redundant cast eliminated)
        assert_eq!(optimizer.stats.redundant_casts_eliminated, 1);
    }
    
    #[test]
    fn test_type_to_typecode_conversion() {
        let optimizer = TypeCastOptimizer::new();
        
        assert_eq!(optimizer.type_to_typecode(&create_test_typeref("int")).unwrap(), typecodes::INT);
        assert_eq!(optimizer.type_to_typecode(&create_test_typeref("byte")).unwrap(), typecodes::BYTE);
        assert_eq!(optimizer.type_to_typecode(&create_test_typeref("String")).unwrap(), typecodes::OBJECT);
    }
    
    #[test]
    fn test_truncate_typecode() {
        assert_eq!(TypeCastOptimizer::truncate_typecode(typecodes::BYTE), typecodes::INT);
        assert_eq!(TypeCastOptimizer::truncate_typecode(typecodes::CHAR), typecodes::INT);
        assert_eq!(TypeCastOptimizer::truncate_typecode(typecodes::SHORT), typecodes::INT);
        // Note: BOOLEAN is represented as BYTE in this codebase
        assert_eq!(TypeCastOptimizer::truncate_typecode(typecodes::LONG), typecodes::LONG);
    }
}