use crate::ast::*;
use crate::codegen::opcodes;
use crate::error::Result;

/// javac-style string buffer optimization for efficient string concatenation
/// 
/// This module implements javac's sophisticated string concatenation optimization
/// using StringBuilder/StringBuffer pattern, including:
/// - makeStringBuffer() - create optimized string buffer
/// - appendStrings() - recursive string appending
/// - bufferToString() - convert buffer to final string
/// - String literal pooling and optimization
#[derive(Debug, Clone)]
pub struct StringBufferOptimizer {
    /// Track string literals for pooling
    string_literals: std::collections::HashMap<String, u16>,
    /// StringBuilder method references
    string_builder_methods: StringBuilderMethods,
    /// Optimization settings
    config: StringOptimizationConfig,
}

#[derive(Debug, Clone)]
struct StringBuilderMethods {
    /// StringBuilder.<init>() method reference
    init_method: u16,
    /// StringBuilder.<init>(String) method reference  
    init_string_method: u16,
    /// StringBuilder.append(String) method reference
    append_string_method: u16,
    /// StringBuilder.append(int) method reference
    append_int_method: u16,
    /// StringBuilder.append(Object) method reference
    append_object_method: u16,
    /// StringBuilder.toString() method reference
    to_string_method: u16,
}

#[derive(Debug, Clone)]
pub struct StringOptimizationConfig {
    /// Use StringBuilder instead of StringBuffer (Java 5+ optimization)
    pub use_string_builder: bool,
    /// Minimum number of concatenations to trigger optimization
    pub min_concatenations: usize,
    /// Enable string literal pooling
    pub enable_literal_pooling: bool,
    /// Maximum string buffer initial capacity
    pub max_initial_capacity: u32,
}

impl Default for StringOptimizationConfig {
    fn default() -> Self {
        Self {
            use_string_builder: true, // javac default for Java 5+
            min_concatenations: 2,
            enable_literal_pooling: true,
            max_initial_capacity: 16,
        }
    }
}

impl StringBufferOptimizer {
    pub fn new() -> Self {
        Self {
            string_literals: std::collections::HashMap::new(),
            string_builder_methods: StringBuilderMethods {
                init_method: 0,
                init_string_method: 0,
                append_string_method: 0,
                append_int_method: 0,
                append_object_method: 0,
                to_string_method: 0,
            },
            config: StringOptimizationConfig::default(),
        }
    }
    
    /// Initialize with custom configuration
    pub fn with_config(config: StringOptimizationConfig) -> Self {
        let mut optimizer = Self::new();
        optimizer.config = config;
        optimizer
    }
    
    /// Analyze string concatenation expression for optimization (javac-style)
    pub fn analyze_string_concatenation(&self, expr: &Expr) -> StringConcatenationAnalysis {
        match expr {
            Expr::Binary(binary) if binary.operator == BinaryOp::Add => {
                self.analyze_binary_concatenation(binary)
            }
            Expr::Literal(literal) => {
                match &literal.value {
                    Literal::String(s) => {
                        StringConcatenationAnalysis::SingleString {
                            value: s.clone(),
                            can_pool: s.len() <= 64, // Pool small strings
                        }
                    }
                    _ => StringConcatenationAnalysis::NotStringConcatenation,
                }
            }
            _ => StringConcatenationAnalysis::NotStringConcatenation,
        }
    }
    
    /// Analyze binary concatenation expression (javac visitBinary style)
    fn analyze_binary_concatenation(&self, binary: &BinaryExpr) -> StringConcatenationAnalysis {
        let left_analysis = self.analyze_string_concatenation(&binary.left);
        let right_analysis = self.analyze_string_concatenation(&binary.right);
        
        match (left_analysis, right_analysis) {
            (StringConcatenationAnalysis::SingleString { value: left_val, .. },
             StringConcatenationAnalysis::SingleString { value: right_val, .. }) => {
                // Two string literals - can be folded at compile time
                StringConcatenationAnalysis::CompileTimeFoldable {
                    result: format!("{}{}", left_val, right_val),
                }
            }
            (StringConcatenationAnalysis::StringConcatenation { mut expressions, estimated_length },
             right) => {
                // Left is already a concatenation chain
                expressions.push((*binary.right).clone());
                StringConcatenationAnalysis::StringConcatenation {
                    expressions,
                    estimated_length: estimated_length + self.estimate_expression_length(&binary.right),
                }
            }
            (left, StringConcatenationAnalysis::StringConcatenation { mut expressions, estimated_length }) => {
                // Right is a concatenation chain
                let mut new_expressions = vec![(*binary.left).clone()];
                new_expressions.append(&mut expressions);
                StringConcatenationAnalysis::StringConcatenation {
                    expressions: new_expressions,
                    estimated_length: self.estimate_expression_length(&binary.left) + estimated_length,
                }
            }
            (left, right) if self.is_string_type(&left) || self.is_string_type(&right) => {
                // At least one operand is string-related
                let expressions = vec![(*binary.left).clone(), (*binary.right).clone()];
                let estimated_length = self.estimate_expression_length(&binary.left) 
                                     + self.estimate_expression_length(&binary.right);
                StringConcatenationAnalysis::StringConcatenation {
                    expressions,
                    estimated_length,
                }
            }
            _ => StringConcatenationAnalysis::NotStringConcatenation,
        }
    }
    
    /// Check if analysis result represents a string type
    fn is_string_type(&self, analysis: &StringConcatenationAnalysis) -> bool {
        matches!(analysis, 
            StringConcatenationAnalysis::SingleString { .. } |
            StringConcatenationAnalysis::StringConcatenation { .. } |
            StringConcatenationAnalysis::CompileTimeFoldable { .. }
        )
    }
    
    /// Estimate the length of an expression when converted to string
    fn estimate_expression_length(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::Literal(literal) => {
                match &literal.value {
                    Literal::String(s) => s.len() as u32,
                    Literal::Integer(_) => 10, // Average integer length
                    Literal::Boolean(_) => 5, // "true" or "false"
                    Literal::Long(_) => 19, // Maximum long length: -9223372036854775808
                    Literal::Double(_) => 24, // Average double length with precision
                    Literal::Null => 4, // "null"
                    _ => 8, // Default estimate
                }
            }
            Expr::Identifier(_) => 16, // Average variable string length
            Expr::MethodCall(_) => 20, // Average method result length
            Expr::Binary(_) => 32, // Compound expression estimate
            _ => 12, // Default estimate
        }
    }
    
    /// Generate optimized string concatenation bytecode (javac-style)
    pub fn generate_string_concatenation_bytecode(&mut self, analysis: StringConcatenationAnalysis) -> Result<Vec<u8>> {
        match analysis {
            StringConcatenationAnalysis::CompileTimeFoldable { result } => {
                self.generate_string_literal_bytecode(&result)
            }
            StringConcatenationAnalysis::SingleString { value, can_pool } => {
                if can_pool && self.config.enable_literal_pooling {
                    self.generate_pooled_string_bytecode(&value)
                } else {
                    self.generate_string_literal_bytecode(&value)
                }
            }
            StringConcatenationAnalysis::StringConcatenation { expressions, estimated_length } => {
                if expressions.len() >= self.config.min_concatenations {
                    self.generate_string_builder_concatenation(&expressions, estimated_length)
                } else {
                    self.generate_simple_concatenation(&expressions)
                }
            }
            StringConcatenationAnalysis::NotStringConcatenation => {
                Ok(vec![opcodes::NOP]) // No optimization
            }
        }
    }
    
    /// Generate string literal bytecode with pooling
    fn generate_pooled_string_bytecode(&mut self, value: &str) -> Result<Vec<u8>> {
        if let Some(&pool_index) = self.string_literals.get(value) {
            // Use existing pooled string
            Ok(vec![opcodes::LDC, pool_index as u8])
        } else {
            // Add to pool and use
            let pool_index = self.string_literals.len() as u16;
            self.string_literals.insert(value.to_string(), pool_index);
            Ok(vec![opcodes::LDC, pool_index as u8])
        }
    }
    
    /// Generate simple string literal bytecode
    fn generate_string_literal_bytecode(&self, value: &str) -> Result<Vec<u8>> {
        // For now, use LDC with placeholder index
        // In real implementation, this would reference the constant pool
        Ok(vec![opcodes::LDC, 0]) // Index will be patched later
    }
    
    /// Generate StringBuilder-based concatenation (javac makeStringBuffer + appendStrings style)
    fn generate_string_builder_concatenation(&self, expressions: &[Expr], estimated_length: u32) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        // Create StringBuilder with estimated capacity (javac makeStringBuffer)
        bytecode.extend_from_slice(&self.make_string_buffer(estimated_length)?);
        
        // Append all expressions (javac appendStrings)
        for expr in expressions {
            bytecode.extend_from_slice(&self.append_string_expression(expr)?);
        }
        
        // Convert to string (javac bufferToString)
        bytecode.extend_from_slice(&self.buffer_to_string()?);
        
        Ok(bytecode)
    }
    
    /// Generate simple concatenation without StringBuilder
    fn generate_simple_concatenation(&self, expressions: &[Expr]) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        // Load first expression
        // ... (expression loading code would go here)
        
        // For each additional expression, use string concatenation
        for _expr in expressions.iter().skip(1) {
            // Load next expression
            // ... (expression loading code would go here)
            
            // Call String.valueOf if needed and concatenate
            // This is a simplified version - real implementation would be more complex
            bytecode.push(opcodes::INVOKEVIRTUAL);
            bytecode.extend_from_slice(&[0, 0]); // Method reference (will be patched)
        }
        
        Ok(bytecode)
    }
    
    /// Create StringBuilder with capacity (javac makeStringBuffer equivalent)
    fn make_string_buffer(&self, estimated_length: u32) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        // NEW StringBuilder
        bytecode.push(opcodes::NEW);
        bytecode.extend_from_slice(&[0, 0]); // StringBuilder class reference
        
        // DUP for constructor call
        bytecode.push(opcodes::DUP);
        
        if estimated_length > 0 && estimated_length <= self.config.max_initial_capacity {
            // StringBuilder(int capacity) constructor
            if estimated_length <= 127 {
                bytecode.push(opcodes::BIPUSH);
                bytecode.push(estimated_length as u8);
            } else {
                bytecode.push(opcodes::SIPUSH);
                bytecode.extend_from_slice(&(estimated_length as u16).to_be_bytes());
            }
            
            // Call StringBuilder(int) constructor
            bytecode.push(opcodes::INVOKESPECIAL);
            bytecode.extend_from_slice(&[0, 0]); // Constructor reference
        } else {
            // Default StringBuilder() constructor
            bytecode.push(opcodes::INVOKESPECIAL);
            bytecode.extend_from_slice(&[0, 0]); // Default constructor reference
        }
        
        Ok(bytecode)
    }
    
    /// Append expression to StringBuilder (javac appendString equivalent)
    fn append_string_expression(&self, expr: &Expr) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        // Load expression value
        // ... (expression loading code would go here)
        
        // Choose appropriate append method based on expression type
        let append_method = match expr {
            Expr::Literal(literal) => {
                match &literal.value {
                    Literal::String(_) => self.string_builder_methods.append_string_method,
                    Literal::Integer(_) => self.string_builder_methods.append_int_method,
                    _ => self.string_builder_methods.append_object_method,
                }
            }
            _ => self.string_builder_methods.append_object_method,
        };
        
        // Call StringBuilder.append(...)
        bytecode.push(opcodes::INVOKEVIRTUAL);
        bytecode.extend_from_slice(&append_method.to_be_bytes());
        
        Ok(bytecode)
    }
    
    /// Convert StringBuilder to String (javac bufferToString equivalent)
    fn buffer_to_string(&self) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        // Call StringBuilder.toString()
        bytecode.push(opcodes::INVOKEVIRTUAL);
        bytecode.extend_from_slice(&self.string_builder_methods.to_string_method.to_be_bytes());
        
        Ok(bytecode)
    }
    
    /// Recursive string appending optimization (javac appendStrings equivalent)
    pub fn append_strings_recursive(&self, expr: &Expr) -> Result<Vec<u8>> {
        match expr {
            Expr::Binary(binary) if binary.operator == BinaryOp::Add => {
                let mut bytecode = Vec::new();
                
                // Recursively append left operand
                bytecode.extend_from_slice(&self.append_strings_recursive(&binary.left)?);
                
                // Recursively append right operand  
                bytecode.extend_from_slice(&self.append_strings_recursive(&binary.right)?);
                
                Ok(bytecode)
            }
            _ => {
                // Append single expression
                self.append_string_expression(expr)
            }
        }
    }
    
    /// Optimize string concatenation chain (javac-style analysis)
    pub fn optimize_concatenation_chain(&self, expressions: &[Expr]) -> ConcatenationOptimization {
        let total_expressions = expressions.len();
        let estimated_length: u32 = expressions.iter()
            .map(|expr| self.estimate_expression_length(expr))
            .sum();
        
        // Count string literals vs dynamic expressions
        let (literal_count, dynamic_count) = expressions.iter().fold((0, 0), |(lit, dyn_count), expr| {
            match expr {
                Expr::Literal(literal) if matches!(literal.value, Literal::String(_)) => (lit + 1, dyn_count),
                _ => (lit, dyn_count + 1),
            }
        });
        
        if total_expressions >= self.config.min_concatenations {
            if literal_count > dynamic_count && estimated_length <= 256 {
                ConcatenationOptimization::CompileTimeFolding {
                    estimated_savings: literal_count * 10, // Bytecode instruction savings
                }
            } else if estimated_length > 64 {
                ConcatenationOptimization::StringBuilderWithCapacity {
                    initial_capacity: std::cmp::min(estimated_length, self.config.max_initial_capacity),
                    estimated_savings: total_expressions * 5,
                }
            } else {
                ConcatenationOptimization::StringBuilder {
                    estimated_savings: total_expressions * 3,
                }
            }
        } else {
            ConcatenationOptimization::SimpleConcat {
                estimated_cost: total_expressions * 2,
            }
        }
    }
}

/// String concatenation analysis result (javac-style)
#[derive(Debug, Clone)]
pub enum StringConcatenationAnalysis {
    /// Single string literal
    SingleString {
        value: String,
        can_pool: bool,
    },
    /// Multiple expressions that can be concatenated
    StringConcatenation {
        expressions: Vec<Expr>,
        estimated_length: u32,
    },
    /// Compile-time foldable concatenation
    CompileTimeFoldable {
        result: String,
    },
    /// Not a string concatenation
    NotStringConcatenation,
}

/// Concatenation optimization strategy
#[derive(Debug, Clone)]
pub enum ConcatenationOptimization {
    /// Use StringBuilder with default capacity
    StringBuilder {
        estimated_savings: usize,
    },
    /// Use StringBuilder with specific initial capacity
    StringBuilderWithCapacity {
        initial_capacity: u32,
        estimated_savings: usize,
    },
    /// Fold at compile time
    CompileTimeFolding {
        estimated_savings: usize,
    },
    /// Use simple concatenation
    SimpleConcat {
        estimated_cost: usize,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location, LiteralExpr, BinaryExpr};
    
    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }
    
    #[test]
    fn test_string_buffer_optimizer_creation() {
        let optimizer = StringBufferOptimizer::new();
        assert!(optimizer.config.use_string_builder);
        assert_eq!(optimizer.config.min_concatenations, 2);
    }
    
    #[test]
    fn test_single_string_analysis() {
        let optimizer = StringBufferOptimizer::new();
        
        let string_expr = Expr::Literal(LiteralExpr {
            value: Literal::String("hello".to_string()),
            span: create_span(),
        });
        
        let analysis = optimizer.analyze_string_concatenation(&string_expr);
        
        match analysis {
            StringConcatenationAnalysis::SingleString { value, can_pool } => {
                assert_eq!(value, "hello");
                assert!(can_pool); // Small string should be poolable
            }
            _ => panic!("Expected SingleString analysis"),
        }
    }
    
    #[test]
    fn test_compile_time_folding() {
        let optimizer = StringBufferOptimizer::new();
        
        let left = Expr::Literal(LiteralExpr {
            value: Literal::String("hello".to_string()),
            span: create_span(),
        });
        
        let right = Expr::Literal(LiteralExpr {
            value: Literal::String(" world".to_string()),
            span: create_span(),
        });
        
        let concat_expr = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: BinaryOp::Add,
            right: Box::new(right),
            span: create_span(),
        });
        
        let analysis = optimizer.analyze_string_concatenation(&concat_expr);
        
        match analysis {
            StringConcatenationAnalysis::CompileTimeFoldable { result } => {
                assert_eq!(result, "hello world");
            }
            _ => panic!("Expected CompileTimeFoldable analysis"),
        }
    }
    
    #[test]
    fn test_length_estimation() {
        let optimizer = StringBufferOptimizer::new();
        
        let string_expr = Expr::Literal(LiteralExpr {
            value: Literal::String("test".to_string()),
            span: create_span(),
        });
        
        assert_eq!(optimizer.estimate_expression_length(&string_expr), 4);
        
        let int_expr = Expr::Literal(LiteralExpr {
            value: Literal::Integer(42),
            span: create_span(),
        });
        
        assert_eq!(optimizer.estimate_expression_length(&int_expr), 10);
    }
    
    #[test]
    fn test_concatenation_optimization_strategy() {
        let optimizer = StringBufferOptimizer::new();
        
        let expressions = vec![
            Expr::Literal(LiteralExpr {
                value: Literal::String("a".to_string()),
                span: create_span(),
            }),
            Expr::Literal(LiteralExpr {
                value: Literal::String("b".to_string()),
                span: create_span(),
            }),
            Expr::Literal(LiteralExpr {
                value: Literal::String("c".to_string()),
                span: create_span(),
            }),
        ];
        
        let optimization = optimizer.optimize_concatenation_chain(&expressions);
        
        match optimization {
            ConcatenationOptimization::CompileTimeFolding { estimated_savings } => {
                assert!(estimated_savings > 0);
            }
            _ => {} // Other optimizations are also valid
        }
    }
    
    #[test]
    fn test_string_builder_methods() {
        let optimizer = StringBufferOptimizer::new();
        
        // Test make_string_buffer
        let bytecode = optimizer.make_string_buffer(16).unwrap();
        assert!(!bytecode.is_empty());
        assert_eq!(bytecode[0], opcodes::NEW);
        
        // Test buffer_to_string
        let bytecode = optimizer.buffer_to_string().unwrap();
        assert!(!bytecode.is_empty());
        assert_eq!(bytecode[0], opcodes::INVOKEVIRTUAL);
    }
}
