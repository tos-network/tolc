/// String concatenation optimization (javac-style)
/// Optimizes string concatenation using StringBuilder pattern

use crate::codegen::opcodes;
use crate::ast::{Expr, BinaryExpr, BinaryOp, LiteralExpr, Literal, IdentifierExpr};

#[derive(Debug, Clone)]
pub struct StringConcatenation {
    pub expressions: Vec<StringExpr>,
    pub estimated_length: usize,
}

#[derive(Debug, Clone)]
pub enum StringExpr {
    Literal(String),
    Variable(String),
    Expression(Box<Expr>),
}

pub struct StringOptimizer;

impl StringOptimizer {
    /// Detect string concatenation patterns (javac algorithm)
    pub fn analyze_string_concat(expr: &Expr) -> Option<StringConcatenation> {
        match expr {
            Expr::Binary(binary_expr) if binary_expr.operator == BinaryOp::Add => {
                // Check if this is string concatenation
                if Self::is_string_type(&binary_expr.left) || Self::is_string_type(&binary_expr.right) {
                    let mut expressions = Vec::new();
                    let mut estimated_length = 0;
                    
                    Self::collect_string_parts(&binary_expr.left, &mut expressions, &mut estimated_length);
                    Self::collect_string_parts(&binary_expr.right, &mut expressions, &mut estimated_length);
                    
                    Some(StringConcatenation {
                        expressions,
                        estimated_length,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    
    /// Collect string parts recursively (javac pattern)
    fn collect_string_parts(expr: &Expr, parts: &mut Vec<StringExpr>, estimated_length: &mut usize) {
        match expr {
            Expr::Binary(binary_expr) if binary_expr.operator == BinaryOp::Add => {
                // Recursively collect from both sides
                Self::collect_string_parts(&binary_expr.left, parts, estimated_length);
                Self::collect_string_parts(&binary_expr.right, parts, estimated_length);
            }
            Expr::Literal(literal_expr) => {
                if let Literal::String(s) = &literal_expr.value {
                    *estimated_length += s.len();
                    parts.push(StringExpr::Literal(s.clone()));
                }
            }
            Expr::Identifier(id) => {
                *estimated_length += 16; // Estimated average string length
                parts.push(StringExpr::Variable(id.name.clone()));
            }
            _ => {
                *estimated_length += 16; // Estimated average for expressions
                parts.push(StringExpr::Expression(Box::new(expr.clone())));
            }
        }
    }
    
    /// Check if expression is string type
    fn is_string_type(expr: &Expr) -> bool {
        match expr {
            Expr::Literal(literal_expr) => matches!(literal_expr.value, Literal::String(_)),
            Expr::Binary(binary_expr) if binary_expr.operator == BinaryOp::Add => {
                Self::is_string_type(&binary_expr.left) || Self::is_string_type(&binary_expr.right)
            }
            // TODO: Add more sophisticated type checking
            _ => false,
        }
    }
    
    /// Generate optimized bytecode for string concatenation (javac pattern)
    pub fn generate_optimized_concat(concat: &StringConcatenation) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // 1. Create StringBuilder with estimated capacity
        bytecode.extend_from_slice(&Self::create_string_builder(concat.estimated_length));
        
        // 2. Append all parts
        for expr in &concat.expressions {
            bytecode.extend_from_slice(&Self::append_to_builder(expr));
        }
        
        // 3. Convert to string
        bytecode.extend_from_slice(&Self::builder_to_string());
        
        bytecode
    }
    
    /// Create StringBuilder with capacity (javac pattern)
    fn create_string_builder(capacity: usize) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // new StringBuilder
        bytecode.push(opcodes::NEW);
        bytecode.extend_from_slice(&Self::string_builder_class_index().to_be_bytes());
        
        // dup
        bytecode.push(opcodes::DUP);
        
        // Load capacity
        if capacity <= 5 {
            bytecode.push(opcodes::ICONST_0 + capacity as u8);
        } else if capacity <= 127 {
            bytecode.push(opcodes::BIPUSH);
            bytecode.push(capacity as u8);
        } else if capacity <= 32767 {
            bytecode.push(opcodes::SIPUSH);
            bytecode.extend_from_slice(&(capacity as i16).to_be_bytes());
        } else {
            // Use ldc for larger values
            bytecode.push(opcodes::LDC);
            bytecode.push(1); // Placeholder constant pool index
        }
        
        // invokespecial StringBuilder.<init>(I)V
        bytecode.push(opcodes::INVOKESPECIAL);
        bytecode.extend_from_slice(&Self::string_builder_init_index().to_be_bytes());
        
        bytecode
    }
    
    /// Append expression to StringBuilder (javac pattern)
    fn append_to_builder(expr: &StringExpr) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        match expr {
            StringExpr::Literal(_s) => {
                // Load string literal
                bytecode.push(opcodes::LDC);
                bytecode.push(1); // Placeholder constant pool index
                
                // invokevirtual StringBuilder.append(String)StringBuilder
                bytecode.push(opcodes::INVOKEVIRTUAL);
                bytecode.extend_from_slice(&Self::string_builder_append_string_index().to_be_bytes());
            }
            StringExpr::Variable(_name) => {
                // Load variable (placeholder - would need actual local variable index)
                bytecode.push(opcodes::ALOAD_0); // Placeholder
                
                // invokevirtual StringBuilder.append(Object)StringBuilder
                bytecode.push(opcodes::INVOKEVIRTUAL);
                bytecode.extend_from_slice(&Self::string_builder_append_object_index().to_be_bytes());
            }
            StringExpr::Expression(_expr) => {
                // Generate code for expression (placeholder)
                // This would need full expression generation
                
                // invokevirtual StringBuilder.append(Object)StringBuilder
                bytecode.push(opcodes::INVOKEVIRTUAL);
                bytecode.extend_from_slice(&Self::string_builder_append_object_index().to_be_bytes());
            }
        }
        
        bytecode
    }
    
    /// Convert StringBuilder to String (javac pattern)
    fn builder_to_string() -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // invokevirtual StringBuilder.toString()String
        bytecode.push(opcodes::INVOKEVIRTUAL);
        bytecode.extend_from_slice(&Self::string_builder_to_string_index().to_be_bytes());
        
        bytecode
    }
    
    /// Placeholder constant pool indices (would be provided by caller)
    fn string_builder_class_index() -> u16 { 1 }
    fn string_builder_init_index() -> u16 { 2 }
    fn string_builder_append_string_index() -> u16 { 3 }
    fn string_builder_append_object_index() -> u16 { 4 }
    fn string_builder_to_string_index() -> u16 { 5 }
    
    /// Calculate the benefit of optimization
    pub fn calculate_optimization_benefit(concat: &StringConcatenation) -> OptimizationBenefit {
        let part_count = concat.expressions.len();
        
        // javac heuristics
        let unoptimized_instructions = part_count * 8; // Rough estimate
        let optimized_instructions = 5 + part_count * 3; // StringBuilder pattern
        
        let memory_savings = if concat.estimated_length > 50 {
            concat.estimated_length / 2 // Avoid multiple string allocations
        } else {
            0
        };
        
        OptimizationBenefit {
            instruction_savings: unoptimized_instructions.saturating_sub(optimized_instructions),
            memory_savings,
            recommended: part_count >= 3 || concat.estimated_length > 50,
        }
    }
}

#[derive(Debug, Clone)]
pub struct OptimizationBenefit {
    pub instruction_savings: usize,
    pub memory_savings: usize,
    pub recommended: bool,
}

/// Advanced string concatenation patterns (javac-style)
pub struct AdvancedStringOptimizer;

impl AdvancedStringOptimizer {
    /// Optimize string concatenation in loops (javac pattern)
    pub fn optimize_loop_concatenation(expressions: &[Expr]) -> Option<LoopStringOptimization> {
        // Detect patterns like: s += expr in loops
        for expr in expressions {
            if let Expr::Binary(binary_expr) = expr {
                if binary_expr.operator == BinaryOp::Add && Self::is_string_accumulator(&binary_expr.left) {
                    return Some(LoopStringOptimization {
                        use_string_builder: true,
                        estimated_iterations: 10, // Default estimate
                        buffer_size: 256, // Default buffer size
                    });
                }
            }
        }
        None
    }
    
    /// Check if expression is a string accumulator pattern
    fn is_string_accumulator(expr: &Expr) -> bool {
        matches!(expr, Expr::Identifier(_))
    }
    
    /// Optimize constant string concatenation at compile time
    pub fn optimize_constant_concat(expressions: &[StringExpr]) -> Option<String> {
        let mut result = String::new();
        
        for expr in expressions {
            match expr {
                StringExpr::Literal(s) => result.push_str(s),
                _ => return None, // Can't optimize if not all literals
            }
        }
        
        Some(result)
    }
}

#[derive(Debug, Clone)]
pub struct LoopStringOptimization {
    pub use_string_builder: bool,
    pub estimated_iterations: usize,
    pub buffer_size: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Literal, IdentifierExpr, BinaryExpr, BinaryOp, LiteralExpr, Span};
    
    #[test]
    fn test_string_concat_detection() {
        // Create a simple string concatenation: "Hello" + " World"
        let left = Expr::Literal(LiteralExpr {
            value: Literal::String("Hello".to_string()),
            span: Span::from_to(0, 0, 0, 0),
        });
        let right = Expr::Literal(LiteralExpr {
            value: Literal::String(" World".to_string()),
            span: Span::from_to(0, 0, 0, 0),
        });
        let concat = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: BinaryOp::Add,
            right: Box::new(right),
            span: Span::from_to(0, 0, 0, 0),
        });
        
        let result = StringOptimizer::analyze_string_concat(&concat);
        assert!(result.is_some());
        
        let concat_info = result.unwrap();
        assert_eq!(concat_info.expressions.len(), 2);
        assert_eq!(concat_info.estimated_length, 11); // "Hello" + " World"
    }
    
    #[test]
    fn test_optimization_benefit() {
        let concat = StringConcatenation {
            expressions: vec![
                StringExpr::Literal("Hello".to_string()),
                StringExpr::Literal(" ".to_string()),
                StringExpr::Variable("name".to_string()),
                StringExpr::Literal("!".to_string()),
            ],
            estimated_length: 20,
        };
        
        let benefit = StringOptimizer::calculate_optimization_benefit(&concat);
        assert!(benefit.recommended); // 4 parts should be optimized
        assert!(benefit.instruction_savings > 0);
    }
    
    #[test]
    fn test_constant_concat_optimization() {
        let expressions = vec![
            StringExpr::Literal("Hello".to_string()),
            StringExpr::Literal(" ".to_string()),
            StringExpr::Literal("World".to_string()),
        ];
        
        let result = AdvancedStringOptimizer::optimize_constant_concat(&expressions);
        assert_eq!(result, Some("Hello World".to_string()));
    }
    
    #[test]
    fn test_mixed_concat_no_optimization() {
        let expressions = vec![
            StringExpr::Literal("Hello".to_string()),
            StringExpr::Variable("name".to_string()),
            StringExpr::Literal("!".to_string()),
        ];
        
        let result = AdvancedStringOptimizer::optimize_constant_concat(&expressions);
        assert_eq!(result, None); // Can't optimize mixed expressions
    }
}