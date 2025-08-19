use crate::ast::*;
use crate::codegen::opcode_enum::Opcode;
use std::collections::{HashMap, VecDeque};

/// Enhanced JavaC-style string optimization for StringBuilder/StringBuffer operations
/// 
/// This implements comprehensive string concatenation optimization similar to javac's
/// string handling, including automatic StringBuilder usage, constant folding,
/// and efficient bytecode generation patterns.
#[derive(Debug, Clone)]
pub struct EnhancedStringOptimizer {
    /// String builder method references for efficient calls
    pub string_builder_methods: StringBuilderMethods,
    
    /// String concatenation analysis and optimization
    pub concat_analyzer: StringConcatenationAnalyzer,
    
    /// Constant string pool for folding
    pub constant_strings: HashMap<String, u16>,
    
    /// Statistics for optimization tracking
    pub stats: StringOptimizationStats,
    
    /// Configuration options
    pub config: StringOptimizationConfig,
}

#[derive(Debug, Clone)]
pub struct StringBuilderMethods {
    /// StringBuilder class reference
    pub string_builder_class: u16,
    
    /// StringBuilder.<init>() method reference
    pub init_method: u16,
    
    /// StringBuilder.<init>(String) method reference  
    pub init_string_method: u16,
    
    /// StringBuilder.<init>(int) method reference for capacity
    pub init_capacity_method: u16,
    
    /// StringBuilder.append(String) method reference
    pub append_string_method: u16,
    
    /// StringBuilder.append(int) method reference
    pub append_int_method: u16,
    
    /// StringBuilder.append(long) method reference
    pub append_long_method: u16,
    
    /// StringBuilder.append(float) method reference
    pub append_float_method: u16,
    
    /// StringBuilder.append(double) method reference
    pub append_double_method: u16,
    
    /// StringBuilder.append(boolean) method reference
    pub append_boolean_method: u16,
    
    /// StringBuilder.append(char) method reference
    pub append_char_method: u16,
    
    /// StringBuilder.append(Object) method reference
    pub append_object_method: u16,
    
    /// StringBuilder.toString() method reference
    pub to_string_method: u16,
}

#[derive(Debug, Clone)]
pub struct StringConcatenationAnalyzer {
    /// Current concatenation chain being analyzed
    pub current_chain: VecDeque<ConcatenationElement>,
    
    /// Estimated final string length
    pub estimated_length: usize,
    
    /// Whether the chain contains only constants
    pub is_constant_only: bool,
    
    /// Complexity score for optimization decisions
    pub complexity_score: u32,
}

#[derive(Debug, Clone)]
pub struct ConcatenationElement {
    /// Type of element in the concatenation
    pub element_type: ConcatenationType,
    
    /// Estimated contribution to final string length
    pub estimated_length: usize,
    
    /// Whether this element is a compile-time constant
    pub is_constant: bool,
    
    /// Constant value if known at compile time
    pub constant_value: Option<String>,
    
    /// Expression that generates this element
    pub source_expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConcatenationType {
    /// String literal
    StringLiteral,
    
    /// String variable or expression
    StringExpression,
    
    /// Integer value
    Integer,
    
    /// Long value
    Long,
    
    /// Float value
    Float,
    
    /// Double value
    Double,
    
    /// Boolean value
    Boolean,
    
    /// Character value
    Character,
    
    /// Object (requires toString())
    Object,
    
    /// Null value
    Null,
}

#[derive(Debug, Clone)]
pub struct StringOptimizationConfig {
    /// Minimum chain length to trigger StringBuilder optimization
    pub min_chain_length: usize,
    
    /// Maximum estimated length for constant folding
    pub max_constant_fold_length: usize,
    
    /// Whether to use StringBuilder capacity optimization
    pub use_capacity_optimization: bool,
    
    /// Whether to prefer StringBuffer over StringBuilder (thread safety)
    pub prefer_string_buffer: bool,
    
    /// Whether to inline small string operations
    pub inline_small_operations: bool,
}

#[derive(Debug, Clone, Default)]
pub struct StringOptimizationStats {
    /// Total string concatenations analyzed
    pub total_concatenations: u32,
    
    /// Number of concatenations optimized with StringBuilder
    pub string_builder_optimizations: u32,
    
    /// Number of constant strings folded
    pub constant_folds: u32,
    
    /// Number of capacity optimizations applied
    pub capacity_optimizations: u32,
    
    /// Estimated bytes saved through optimization
    pub bytes_saved: u32,
    
    /// Number of method calls eliminated
    pub calls_eliminated: u32,
}

impl EnhancedStringOptimizer {
    /// Create a new enhanced string optimizer
    pub fn new() -> Self {
        Self {
            string_builder_methods: StringBuilderMethods::default(),
            concat_analyzer: StringConcatenationAnalyzer::new(),
            constant_strings: HashMap::new(),
            stats: StringOptimizationStats::default(),
            config: StringOptimizationConfig::default(),
        }
    }
    
    /// Analyze a binary expression for string concatenation patterns
    pub fn analyze_string_concatenation(&mut self, binary_expr: &BinaryExpr) -> Result<StringConcatenationOptimization, String> {
        // Reset analyzer for new chain
        self.concat_analyzer.reset();
        
        // Analyze the concatenation chain
        self.analyze_concatenation_recursive(binary_expr)?;
        
        // Make optimization decision
        let optimization = self.decide_concatenation_optimization()?;
        
        self.stats.total_concatenations += 1;
        
        eprintln!("🔍 DEBUG: EnhancedStringOptimizer: Analyzed concatenation - {} elements, estimated length: {}, strategy: {:?}", 
                 self.concat_analyzer.current_chain.len(), 
                 self.concat_analyzer.estimated_length,
                 optimization.strategy);
        
        Ok(optimization)
    }
    
    /// Recursively analyze a concatenation expression
    fn analyze_concatenation_recursive(&mut self, expr: &BinaryExpr) -> Result<(), String> {
        if expr.operator != BinaryOp::Add {
            return Ok(());
        }
        
        // Analyze left operand
        self.analyze_concatenation_operand(&expr.left)?;
        
        // Analyze right operand
        self.analyze_concatenation_operand(&expr.right)?;
        
        Ok(())
    }
    
    /// Analyze a single operand in concatenation
    fn analyze_concatenation_operand(&mut self, expr: &Expr) -> Result<(), String> {
        let element = match expr {
            Expr::Literal(literal) => {
                match &literal.value {
                    Literal::String(s) => ConcatenationElement {
                        element_type: ConcatenationType::StringLiteral,
                        estimated_length: s.len(),
                        is_constant: true,
                        constant_value: Some(s.clone()),
                        source_expr: Some(expr.clone()),
                    },
                    Literal::Integer(i) => ConcatenationElement {
                        element_type: ConcatenationType::Integer,
                        estimated_length: i.to_string().len(),
                        is_constant: true,
                        constant_value: Some(i.to_string()),
                        source_expr: Some(expr.clone()),
                    },
                    Literal::Long(l) => ConcatenationElement {
                        element_type: ConcatenationType::Long,
                        estimated_length: l.to_string().len(),
                        is_constant: true,
                        constant_value: Some(l.to_string()),
                        source_expr: Some(expr.clone()),
                    },
                    Literal::Float(f) => ConcatenationElement {
                        element_type: ConcatenationType::Float,
                        estimated_length: f.to_string().len(),
                        is_constant: true,
                        constant_value: Some(f.to_string()),
                        source_expr: Some(expr.clone()),
                    },
                    Literal::Double(d) => ConcatenationElement {
                        element_type: ConcatenationType::Double,
                        estimated_length: d.to_string().len(),
                        is_constant: true,
                        constant_value: Some(d.to_string()),
                        source_expr: Some(expr.clone()),
                    },
                    Literal::Boolean(b) => ConcatenationElement {
                        element_type: ConcatenationType::Boolean,
                        estimated_length: if *b { 4 } else { 5 }, // "true" or "false"
                        is_constant: true,
                        constant_value: Some(b.to_string()),
                        source_expr: Some(expr.clone()),
                    },
                    Literal::Char(c) => ConcatenationElement {
                        element_type: ConcatenationType::Character,
                        estimated_length: 1,
                        is_constant: true,
                        constant_value: Some(c.to_string()),
                        source_expr: Some(expr.clone()),
                    },

                    Literal::Null => ConcatenationElement {
                        element_type: ConcatenationType::Null,
                        estimated_length: 4, // "null"
                        is_constant: true,
                        constant_value: Some("null".to_string()),
                        source_expr: Some(expr.clone()),
                    },
                }
            },
            Expr::Binary(binary) if binary.operator == BinaryOp::Add => {
                // Recursive concatenation
                self.analyze_concatenation_recursive(binary)?;
                return Ok(());
            },
            _ => {
                // Non-constant expression
                ConcatenationElement {
                    element_type: ConcatenationType::Object, // Default to Object
                    estimated_length: 10, // Rough estimate
                    is_constant: false,
                    constant_value: None,
                    source_expr: Some(expr.clone()),
                }
            }
        };
        
        self.concat_analyzer.add_element(element);
        Ok(())
    }
    
    /// Decide the best optimization strategy for the current concatenation
    pub fn decide_concatenation_optimization(&mut self) -> Result<StringConcatenationOptimization, String> {
        let chain_length = self.concat_analyzer.current_chain.len();
        let estimated_length = self.concat_analyzer.estimated_length;
        let is_constant_only = self.concat_analyzer.is_constant_only;
        
        let strategy = if is_constant_only && estimated_length <= self.config.max_constant_fold_length {
            // Fold constants at compile time
            OptimizationStrategy::ConstantFold
        } else if chain_length >= self.config.min_chain_length {
            // Use StringBuilder for long chains
            OptimizationStrategy::StringBuilder
        } else if chain_length == 2 && self.config.inline_small_operations {
            // Inline simple two-element concatenations
            OptimizationStrategy::InlineSimple
        } else {
            // Use default string concatenation
            OptimizationStrategy::Default
        };
        
        // Calculate estimated performance improvement
        let performance_gain = match strategy {
            OptimizationStrategy::ConstantFold => 90, // Very high gain
            OptimizationStrategy::StringBuilder => {
                // Gain increases with chain length
                std::cmp::min(80, (chain_length * 10) as u32)
            },
            OptimizationStrategy::InlineSimple => 20,
            OptimizationStrategy::Default => 0,
        };
        
        // Calculate estimated size change
        let size_change = match strategy {
            OptimizationStrategy::ConstantFold => -(chain_length as i32 * 5), // Saves instructions
            OptimizationStrategy::StringBuilder => {
                // StringBuilder setup overhead vs concatenation savings
                10 - (chain_length as i32 * 3)
            },
            OptimizationStrategy::InlineSimple => -2,
            OptimizationStrategy::Default => 0,
        };
        
        Ok(StringConcatenationOptimization {
            strategy,
            chain_length,
            estimated_final_length: estimated_length,
            performance_gain,
            size_change,
            constant_value: if is_constant_only {
                Some(self.concat_analyzer.fold_constants())
            } else {
                None
            },
        })
    }
    
    /// Generate optimized bytecode for string concatenation
    pub fn generate_optimized_concatenation(&mut self, optimization: &StringConcatenationOptimization) -> Result<Vec<u8>, String> {
        match optimization.strategy {
            OptimizationStrategy::ConstantFold => {
                self.generate_constant_fold_bytecode(optimization)
            },
            OptimizationStrategy::StringBuilder => {
                self.generate_string_builder_bytecode(optimization)
            },
            OptimizationStrategy::InlineSimple => {
                self.generate_inline_simple_bytecode(optimization)
            },
            OptimizationStrategy::Default => {
                self.generate_default_concatenation_bytecode(optimization)
            },
        }
    }
    
    /// Generate bytecode for constant folding
    fn generate_constant_fold_bytecode(&mut self, optimization: &StringConcatenationOptimization) -> Result<Vec<u8>, String> {
        let mut bytecode = Vec::new();
        
        if let Some(constant_value) = &optimization.constant_value {
            // Load the folded constant string
            // LDC instruction with string constant
            bytecode.push(Opcode::Ldc.to_byte());
            
            // Add constant to pool and get index (simplified)
            let string_index = self.add_string_constant(constant_value.clone());
            bytecode.push(string_index as u8);
            
            self.stats.constant_folds += 1;
            self.stats.calls_eliminated += optimization.chain_length as u32 - 1;
            
            eprintln!("🔍 DEBUG: EnhancedStringOptimizer: Generated constant fold for: '{}'", constant_value);
        }
        
        Ok(bytecode)
    }
    
    /// Generate bytecode for StringBuilder pattern
    fn generate_string_builder_bytecode(&mut self, optimization: &StringConcatenationOptimization) -> Result<Vec<u8>, String> {
        let mut bytecode = Vec::new();
        
        // NEW StringBuilder
        bytecode.push(Opcode::New.to_byte());
        bytecode.extend_from_slice(&self.string_builder_methods.string_builder_class.to_be_bytes());
        
        // DUP for constructor call
        bytecode.push(Opcode::Dup.to_byte());
        
        // Constructor call with capacity if optimization enabled
        if self.config.use_capacity_optimization && optimization.estimated_final_length > 16 {
            // Load capacity
            bytecode.push(Opcode::Bipush.to_byte());
            bytecode.push(optimization.estimated_final_length as u8);
            
            // INVOKESPECIAL StringBuilder.<init>(int)
            bytecode.push(Opcode::Invokespecial.to_byte());
            bytecode.extend_from_slice(&self.string_builder_methods.init_capacity_method.to_be_bytes());
            
            self.stats.capacity_optimizations += 1;
        } else {
            // INVOKESPECIAL StringBuilder.<init>()
            bytecode.push(Opcode::Invokespecial.to_byte());
            bytecode.extend_from_slice(&self.string_builder_methods.init_method.to_be_bytes());
        }
        
        // Generate append calls for each element
        for element in &self.concat_analyzer.current_chain {
            bytecode.extend(self.generate_append_bytecode(element)?);
        }
        
        // INVOKEVIRTUAL StringBuilder.toString()
        bytecode.push(Opcode::Invokevirtual.to_byte());
        bytecode.extend_from_slice(&self.string_builder_methods.to_string_method.to_be_bytes());
        
        self.stats.string_builder_optimizations += 1;
        self.stats.bytes_saved += (optimization.chain_length as u32 * 5).saturating_sub(20);
        
        eprintln!("🔍 DEBUG: EnhancedStringOptimizer: Generated StringBuilder pattern for {} elements", 
                 optimization.chain_length);
        
        Ok(bytecode)
    }
    
    /// Generate append bytecode for a concatenation element
    fn generate_append_bytecode(&self, element: &ConcatenationElement) -> Result<Vec<u8>, String> {
        let mut bytecode = Vec::new();
        
        // Load the value onto stack (simplified - would need full expression generation)
        if let Some(_constant_value) = &element.constant_value {
            // Load constant
            bytecode.push(Opcode::Ldc.to_byte());
            bytecode.push(0); // Placeholder for constant pool index
        } else {
            // Load variable/expression result (placeholder)
            bytecode.push(Opcode::Aload.to_byte());
            bytecode.push(0); // Placeholder for local variable index
        }
        
        // Call appropriate append method
        let append_method = match element.element_type {
            ConcatenationType::StringLiteral | ConcatenationType::StringExpression => {
                self.string_builder_methods.append_string_method
            },
            ConcatenationType::Integer => self.string_builder_methods.append_int_method,
            ConcatenationType::Long => self.string_builder_methods.append_long_method,
            ConcatenationType::Float => self.string_builder_methods.append_float_method,
            ConcatenationType::Double => self.string_builder_methods.append_double_method,
            ConcatenationType::Boolean => self.string_builder_methods.append_boolean_method,
            ConcatenationType::Character => self.string_builder_methods.append_char_method,
            ConcatenationType::Object | ConcatenationType::Null => {
                self.string_builder_methods.append_object_method
            },
        };
        
        bytecode.push(Opcode::Invokevirtual.to_byte());
        bytecode.extend_from_slice(&append_method.to_be_bytes());
        
        Ok(bytecode)
    }
    
    /// Generate bytecode for inline simple concatenation
    fn generate_inline_simple_bytecode(&mut self, _optimization: &StringConcatenationOptimization) -> Result<Vec<u8>, String> {
        let mut bytecode = Vec::new();
        
        // For simple two-element concatenation, use String.valueOf + StringBuilder pattern
        // This is a simplified implementation
        
        // Load first element
        bytecode.push(Opcode::Aload.to_byte());
        bytecode.push(0); // Placeholder
        
        // Load second element  
        bytecode.push(Opcode::Aload.to_byte());
        bytecode.push(1); // Placeholder
        
        // Call String concatenation helper (simplified)
        bytecode.push(Opcode::Invokestatic.to_byte());
        bytecode.extend_from_slice(&[0, 1]); // Placeholder for method reference
        
        eprintln!("🔍 DEBUG: EnhancedStringOptimizer: Generated inline simple concatenation");
        
        Ok(bytecode)
    }
    
    /// Generate default concatenation bytecode
    fn generate_default_concatenation_bytecode(&self, _optimization: &StringConcatenationOptimization) -> Result<Vec<u8>, String> {
        let mut bytecode = Vec::new();
        
        // Default string concatenation using + operator
        // This would generate the standard pattern without optimization
        
        bytecode.push(Opcode::Nop.to_byte()); // Placeholder
        
        Ok(bytecode)
    }
    
    /// Add a string constant to the pool
    fn add_string_constant(&mut self, value: String) -> u16 {
        let next_index = self.constant_strings.len() as u16 + 1;
        *self.constant_strings.entry(value).or_insert(next_index)
    }
    
    /// Get optimization statistics
    pub fn get_stats(&self) -> &StringOptimizationStats {
        &self.stats
    }
    
    /// Reset for new method compilation
    pub fn reset(&mut self) {
        self.concat_analyzer.reset();
        self.constant_strings.clear();
    }
    
    /// Generate optimization report
    pub fn generate_report(&self) -> String {
        let mut report = String::new();
        report.push_str("=== Enhanced String Optimization Report ===\n");
        report.push_str(&format!("Total Concatenations: {}\n", self.stats.total_concatenations));
        report.push_str(&format!("StringBuilder Optimizations: {}\n", self.stats.string_builder_optimizations));
        report.push_str(&format!("Constant Folds: {}\n", self.stats.constant_folds));
        report.push_str(&format!("Capacity Optimizations: {}\n", self.stats.capacity_optimizations));
        report.push_str(&format!("Bytes Saved: {}\n", self.stats.bytes_saved));
        report.push_str(&format!("Calls Eliminated: {}\n", self.stats.calls_eliminated));
        
        let optimization_rate = if self.stats.total_concatenations > 0 {
            ((self.stats.string_builder_optimizations + self.stats.constant_folds) * 100) / self.stats.total_concatenations
        } else {
            0
        };
        report.push_str(&format!("Optimization Rate: {}%\n", optimization_rate));
        
        report
    }
}

impl StringConcatenationAnalyzer {
    /// Create a new concatenation analyzer
    pub fn new() -> Self {
        Self {
            current_chain: VecDeque::new(),
            estimated_length: 0,
            is_constant_only: true,
            complexity_score: 0,
        }
    }
    
    /// Add an element to the current concatenation chain
    pub fn add_element(&mut self, element: ConcatenationElement) {
        self.estimated_length += element.estimated_length;
        
        if !element.is_constant {
            self.is_constant_only = false;
        }
        
        // Update complexity score
        self.complexity_score += match element.element_type {
            ConcatenationType::StringLiteral => 1,
            ConcatenationType::Integer | ConcatenationType::Boolean | ConcatenationType::Character => 2,
            ConcatenationType::Long | ConcatenationType::Float | ConcatenationType::Double => 3,
            ConcatenationType::Object => 5,
            _ => 2,
        };
        
        self.current_chain.push_back(element);
    }
    
    /// Fold all constant elements into a single string
    pub fn fold_constants(&self) -> String {
        let mut result = String::new();
        
        for element in &self.current_chain {
            if let Some(constant_value) = &element.constant_value {
                result.push_str(constant_value);
            }
        }
        
        result
    }
    
    /// Reset the analyzer for a new concatenation
    pub fn reset(&mut self) {
        self.current_chain.clear();
        self.estimated_length = 0;
        self.is_constant_only = true;
        self.complexity_score = 0;
    }
}

impl StringBuilderMethods {
    /// Create default StringBuilder method references (would be populated from constant pool)
    pub fn default() -> Self {
        Self {
            string_builder_class: 1,
            init_method: 2,
            init_string_method: 3,
            init_capacity_method: 4,
            append_string_method: 5,
            append_int_method: 6,
            append_long_method: 7,
            append_float_method: 8,
            append_double_method: 9,
            append_boolean_method: 10,
            append_char_method: 11,
            append_object_method: 12,
            to_string_method: 13,
        }
    }
}

impl StringOptimizationConfig {
    /// Create default configuration
    pub fn default() -> Self {
        Self {
            min_chain_length: 3,
            max_constant_fold_length: 1000,
            use_capacity_optimization: true,
            prefer_string_buffer: false,
            inline_small_operations: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StringConcatenationOptimization {
    /// Optimization strategy to apply
    pub strategy: OptimizationStrategy,
    
    /// Length of the concatenation chain
    pub chain_length: usize,
    
    /// Estimated final string length
    pub estimated_final_length: usize,
    
    /// Expected performance gain (0-100)
    pub performance_gain: u32,
    
    /// Expected size change in bytes (negative = smaller)
    pub size_change: i32,
    
    /// Constant folded value if applicable
    pub constant_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OptimizationStrategy {
    /// Fold constants at compile time
    ConstantFold,
    
    /// Use StringBuilder pattern
    StringBuilder,
    
    /// Inline simple two-element concatenation
    InlineSimple,
    
    /// Use default concatenation
    Default,
}

impl Default for EnhancedStringOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_constant_folding() {
        let mut optimizer = EnhancedStringOptimizer::new();
        
        // Create a simple string literal concatenation
        let left = Box::new(Expr::Literal(LiteralExpr {
            value: Literal::String("Hello".to_string()),
            span: crate::ast::Span::from_to(0, 0, 0, 0),
        }));
        let right = Box::new(Expr::Literal(LiteralExpr {
            value: Literal::String(" World".to_string()),
            span: crate::ast::Span::from_to(0, 0, 0, 0),
        }));
        
        let binary_expr = BinaryExpr {
            left,
            right,
            operator: BinaryOp::Add,
            span: crate::ast::Span::from_to(0, 0, 0, 0),
        };
        
        let optimization = optimizer.analyze_string_concatenation(&binary_expr).unwrap();
        
        assert_eq!(optimization.strategy, OptimizationStrategy::ConstantFold);
        assert_eq!(optimization.constant_value, Some("Hello World".to_string()));
        assert_eq!(optimization.estimated_final_length, 11);
    }
    
    #[test]
    fn test_string_builder_optimization() {
        let mut optimizer = EnhancedStringOptimizer::new();
        
        // Simulate a longer concatenation chain that should trigger StringBuilder
        optimizer.concat_analyzer.add_element(ConcatenationElement {
            element_type: ConcatenationType::StringLiteral,
            estimated_length: 5,
            is_constant: false, // Make it non-constant to avoid folding
            constant_value: None,
            source_expr: None,
        });
        
        optimizer.concat_analyzer.add_element(ConcatenationElement {
            element_type: ConcatenationType::Integer,
            estimated_length: 3,
            is_constant: false,
            constant_value: None,
            source_expr: None,
        });
        
        optimizer.concat_analyzer.add_element(ConcatenationElement {
            element_type: ConcatenationType::StringLiteral,
            estimated_length: 4,
            is_constant: false,
            constant_value: None,
            source_expr: None,
        });
        
        let optimization = optimizer.decide_concatenation_optimization().unwrap();
        
        assert_eq!(optimization.strategy, OptimizationStrategy::StringBuilder);
        assert_eq!(optimization.chain_length, 3);
        assert!(optimization.performance_gain > 0);
    }
}
