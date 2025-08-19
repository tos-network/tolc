use crate::ast::*;
use crate::codegen::opcodes;
use crate::error::Result;

/// Enhanced assignment optimization
/// 
/// This module implements sophisticated assignment optimizations including:
/// - iinc instruction optimization for local variable increments
/// - dup_x1 optimization for compound assignments
/// - Constant assignment folding
/// - Assignment chain optimization
#[derive(Debug, Clone)]
pub struct AssignmentOptimizer {
    /// Track local variable information for iinc optimization
    local_vars: std::collections::HashMap<String, LocalVarInfo>,
}

#[derive(Debug, Clone)]
struct LocalVarInfo {
    index: u16,
    var_type: String,
    is_int_compatible: bool,
}

impl AssignmentOptimizer {
    pub fn new() -> Self {
        Self {
            local_vars: std::collections::HashMap::new(),
        }
    }
    
    /// Register a local variable for optimization tracking
    pub fn register_local_var(&mut self, name: String, index: u16, var_type: String) {
        let is_int_compatible = matches!(var_type.as_str(), "int" | "byte" | "short" | "char");
        self.local_vars.insert(name, LocalVarInfo {
            index,
            var_type,
            is_int_compatible,
        });
    }
    
    /// Analyze assignment expression for optimization opportunities (javac-style)
    pub fn analyze_assignment(&self, assignment: &AssignmentExpr) -> AssignmentOptimization {
        match assignment.operator {
            AssignmentOp::AddAssign | AssignmentOp::SubAssign => {
                self.analyze_compound_assignment(assignment)
            }
            AssignmentOp::Assign => {
                self.analyze_simple_assignment(assignment)
            }
            _ => AssignmentOptimization::Standard,
        }
    }
    
    /// Analyze compound assignment for iinc optimization (javac visitAssignop style)
    fn analyze_compound_assignment(&self, assignment: &AssignmentExpr) -> AssignmentOptimization {
        // Check if this is a local variable increment/decrement that can use iinc
        if let Expr::Identifier(ident) = &*assignment.target {
            if let Some(var_info) = self.local_vars.get(&ident.name) {
                if var_info.is_int_compatible {
                    // Check if right side is a constant that fits in iinc range
                    if let Some(constant_value) = self.extract_constant_int(&assignment.value) {
                        let increment = match assignment.operator {
                            AssignmentOp::AddAssign => constant_value,
                            AssignmentOp::SubAssign => -constant_value,
                            _ => return AssignmentOptimization::Standard,
                        };
                        
                        // javac iinc range check: -32768 to +32767
                        if increment >= -32768 && increment <= 32767 {
                            return AssignmentOptimization::IincOptimization {
                                var_index: var_info.index,
                                increment: increment as i16,
                            };
                        }
                    }
                }
            }
        }
        
        // Check for dup_x1 optimization opportunity
        if self.can_use_dup_x1_optimization(assignment) {
            AssignmentOptimization::DupX1Optimization {
                needs_wide_instruction: false, // Will be determined during generation
            }
        } else {
            AssignmentOptimization::Standard
        }
    }
    
    /// Analyze simple assignment for optimization
    fn analyze_simple_assignment(&self, assignment: &AssignmentExpr) -> AssignmentOptimization {
        // Check for constant assignment
        if let Some(constant_value) = self.extract_constant_int(&assignment.value) {
            return AssignmentOptimization::ConstantAssignment {
                value: constant_value,
                use_optimized_load: self.can_use_optimized_constant_load(constant_value),
            };
        }
        
        // Check for assignment chain optimization
        if self.is_assignment_chain(&assignment.value) {
            return AssignmentOptimization::AssignmentChain;
        }
        
        AssignmentOptimization::Standard
    }
    
    /// Extract constant integer value from expression (javac constValue equivalent)
    fn extract_constant_int(&self, expr: &Expr) -> Option<i32> {
        match expr {
            Expr::Literal(literal) => {
                match &literal.value {
                    Literal::Integer(value) => Some(*value as i32),
                    _ => None,
                }
            }
            Expr::Unary(unary) => {
                if matches!(unary.operator, UnaryOp::Minus) {
                    self.extract_constant_int(&unary.operand).map(|v| -v)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    
    /// Check if expression can use dup_x1 optimization (javac-style)
    fn can_use_dup_x1_optimization(&self, assignment: &AssignmentExpr) -> bool {
        // dup_x1 is beneficial when we need to duplicate the target for compound assignment
        matches!(&*assignment.target, Expr::Identifier(_) | Expr::FieldAccess(_) | Expr::ArrayAccess(_))
    }
    
    /// Check if constant can use optimized loading instructions
    fn can_use_optimized_constant_load(&self, value: i32) -> bool {
        // javac constant loading optimization ranges
        matches!(value, -1..=5) || // iconst_m1 to iconst_5
        (value >= -128 && value <= 127) || // bipush range
        (value >= -32768 && value <= 32767) // sipush range
    }
    
    /// Check if expression is part of an assignment chain
    fn is_assignment_chain(&self, expr: &Expr) -> bool {
        matches!(expr, Expr::Assignment(_))
    }
    
    /// Generate optimized bytecode for assignment (javac-style)
    pub fn generate_assignment_bytecode(&self, assignment: &AssignmentExpr) -> Result<Vec<u8>> {
        let optimization = self.analyze_assignment(assignment);
        
        match optimization {
            AssignmentOptimization::IincOptimization { var_index, increment } => {
                self.generate_iinc_bytecode(var_index, increment)
            }
            AssignmentOptimization::DupX1Optimization { needs_wide_instruction } => {
                self.generate_dup_x1_bytecode(assignment, needs_wide_instruction)
            }
            AssignmentOptimization::ConstantAssignment { value, use_optimized_load } => {
                self.generate_constant_assignment_bytecode(assignment, value, use_optimized_load)
            }
            AssignmentOptimization::AssignmentChain => {
                self.generate_assignment_chain_bytecode(assignment)
            }
            AssignmentOptimization::Standard => {
                self.generate_standard_assignment_bytecode(assignment)
            }
        }
    }
    
    /// Generate iinc instruction bytecode (javac LocalItem.incr equivalent)
    fn generate_iinc_bytecode(&self, var_index: u16, increment: i16) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        if var_index <= 255 && increment >= -128 && increment <= 127 {
            // Standard iinc instruction
            bytecode.push(opcodes::IINC);
            bytecode.push(var_index as u8);
            bytecode.push(increment as u8);
        } else {
            // Wide iinc instruction
            bytecode.push(opcodes::WIDE);
            bytecode.push(opcodes::IINC);
            bytecode.extend_from_slice(&(var_index).to_be_bytes());
            bytecode.extend_from_slice(&(increment).to_be_bytes());
        }
        
        Ok(bytecode)
    }
    
    /// Generate dup_x1 optimized bytecode (javac-style)
    fn generate_dup_x1_bytecode(&self, assignment: &AssignmentExpr, _needs_wide: bool) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        // Load target address
        // ... (target loading code would go here)
        
        // Duplicate target for compound assignment (javac dup_x1 pattern)
        bytecode.push(opcodes::DUP_X1);
        
        // Load current value
        // ... (value loading code would go here)
        
        // Load right operand
        // ... (right operand code would go here)
        
        // Perform operation
        match assignment.operator {
            AssignmentOp::AddAssign => bytecode.push(opcodes::IADD),
            AssignmentOp::SubAssign => bytecode.push(opcodes::ISUB),
            AssignmentOp::MulAssign => bytecode.push(opcodes::IMUL),
            AssignmentOp::DivAssign => bytecode.push(opcodes::IDIV),
            AssignmentOp::ModAssign => bytecode.push(opcodes::IREM),
            _ => return Err(crate::error::Error::from(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Unsupported compound assignment operator"))),
        }
        
        // Store result
        // ... (store instruction would go here)
        
        Ok(bytecode)
    }
    
    /// Generate optimized constant assignment bytecode
    fn generate_constant_assignment_bytecode(&self, assignment: &AssignmentExpr, value: i32, use_optimized_load: bool) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        if use_optimized_load {
            // Use optimized constant loading (javac-style)
            match value {
                -1 => bytecode.push(opcodes::ICONST_M1),
                0 => bytecode.push(opcodes::ICONST_0),
                1 => bytecode.push(opcodes::ICONST_1),
                2 => bytecode.push(opcodes::ICONST_2),
                3 => bytecode.push(opcodes::ICONST_3),
                4 => bytecode.push(opcodes::ICONST_4),
                5 => bytecode.push(opcodes::ICONST_5),
                v if v >= -128 && v <= 127 => {
                    bytecode.push(opcodes::BIPUSH);
                    bytecode.push(v as u8);
                }
                v if v >= -32768 && v <= 32767 => {
                    bytecode.push(opcodes::SIPUSH);
                    bytecode.extend_from_slice(&(v as i16).to_be_bytes());
                }
                _ => {
                    // Use LDC for larger constants
                    bytecode.push(opcodes::LDC);
                    bytecode.push(0); // Constant pool index (will be patched)
                }
            }
        } else {
            // Standard LDC instruction
            bytecode.push(opcodes::LDC);
            bytecode.push(0); // Constant pool index (will be patched)
        }
        
        // Generate store instruction based on target
        match &*assignment.target {
            Expr::Identifier(_) => {
                // Local variable store
                bytecode.push(opcodes::ISTORE);
                bytecode.push(0); // Variable index (will be patched)
            }
            Expr::FieldAccess(_) => {
                // Field store
                bytecode.push(opcodes::PUTFIELD);
                bytecode.extend_from_slice(&[0, 0]); // Field reference (will be patched)
            }
            Expr::ArrayAccess(_) => {
                // Array store
                bytecode.push(opcodes::IASTORE);
            }
            _ => return Err(crate::error::Error::from(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Unsupported assignment target"))),
        }
        
        Ok(bytecode)
    }
    
    /// Generate assignment chain bytecode
    fn generate_assignment_chain_bytecode(&self, _assignment: &AssignmentExpr) -> Result<Vec<u8>> {
        // Assignment chains require careful handling of evaluation order
        // This is a placeholder for the complex logic
        Ok(vec![opcodes::NOP])
    }
    
    /// Generate standard assignment bytecode
    fn generate_standard_assignment_bytecode(&self, _assignment: &AssignmentExpr) -> Result<Vec<u8>> {
        // Standard assignment without special optimizations
        // This is a placeholder for the standard logic
        Ok(vec![opcodes::NOP])
    }
}

/// Assignment optimization strategies (javac-style)
#[derive(Debug, Clone)]
pub enum AssignmentOptimization {
    /// Use iinc instruction for local variable increment/decrement
    IincOptimization {
        var_index: u16,
        increment: i16,
    },
    /// Use dup_x1 for efficient compound assignment
    DupX1Optimization {
        needs_wide_instruction: bool,
    },
    /// Optimized constant assignment
    ConstantAssignment {
        value: i32,
        use_optimized_load: bool,
    },
    /// Assignment chain optimization
    AssignmentChain,
    /// Standard assignment (no special optimization)
    Standard,
}

/// Increment/Decrement expression analyzer (javac visitUnary PREINC/POSTINC style)
pub struct IncrementAnalyzer;

impl IncrementAnalyzer {
    /// Analyze increment/decrement expression for iinc optimization
    pub fn analyze_increment(expr: &UnaryExpr) -> IncrementOptimization {
        match expr.operator {
            UnaryOp::PreInc | UnaryOp::PostInc => {
                IncrementOptimization::Increment { is_prefix: matches!(expr.operator, UnaryOp::PreInc) }
            }
            UnaryOp::PreDec | UnaryOp::PostDec => {
                IncrementOptimization::Decrement { is_prefix: matches!(expr.operator, UnaryOp::PreDec) }
            }
            _ => IncrementOptimization::NotIncrement,
        }
    }
    
    /// Check if increment can use iinc instruction (javac LocalItem check)
    pub fn can_use_iinc(expr: &UnaryExpr, local_vars: &std::collections::HashMap<String, LocalVarInfo>) -> bool {
        if let Expr::Identifier(ident) = &*expr.operand {
            if let Some(var_info) = local_vars.get(&ident.name) {
                return var_info.is_int_compatible;
            }
        }
        false
    }
    
    /// Generate optimized increment bytecode (javac-style)
    pub fn generate_increment_bytecode(
        expr: &UnaryExpr,
        local_vars: &std::collections::HashMap<String, LocalVarInfo>,
    ) -> Result<Vec<u8>> {
        let mut bytecode = Vec::new();
        
        if Self::can_use_iinc(expr, local_vars) {
            // Use iinc instruction
            if let Expr::Identifier(ident) = &*expr.operand {
                if let Some(var_info) = local_vars.get(&ident.name) {
                    let increment = match expr.operator {
                        UnaryOp::PreInc | UnaryOp::PostInc => 1,
                        UnaryOp::PreDec | UnaryOp::PostDec => -1,
                        _ => return Err(crate::error::Error::from(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Invalid increment operator"))),
                    };
                    
                    // For postfix operations, we need to load the old value first
                    if matches!(expr.operator, UnaryOp::PostInc | UnaryOp::PostDec) {
                        bytecode.push(opcodes::ILOAD);
                        bytecode.push(var_info.index as u8);
                    }
                    
                    // Generate iinc instruction
                    bytecode.push(opcodes::IINC);
                    bytecode.push(var_info.index as u8);
                    bytecode.push(increment as i8 as u8);
                    
                    // For prefix operations, load the new value
                    if matches!(expr.operator, UnaryOp::PreInc | UnaryOp::PreDec) {
                        bytecode.push(opcodes::ILOAD);
                        bytecode.push(var_info.index as u8);
                    }
                }
            }
        } else {
            // Use standard increment pattern
            // Load variable, duplicate, increment, store
            bytecode.extend_from_slice(&[
                opcodes::DUP,
                opcodes::ICONST_1,
                match expr.operator {
                    UnaryOp::PreInc | UnaryOp::PostInc => opcodes::IADD,
                    UnaryOp::PreDec | UnaryOp::PostDec => opcodes::ISUB,
                    _ => return Err(crate::error::Error::from(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Invalid increment operator"))),
                },
            ]);
        }
        
        Ok(bytecode)
    }
}

/// Increment optimization strategies
#[derive(Debug, Clone)]
pub enum IncrementOptimization {
    Increment { is_prefix: bool },
    Decrement { is_prefix: bool },
    NotIncrement,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location, IdentifierExpr};
    
    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }
    
    #[test]
    fn test_assignment_optimizer_creation() {
        let optimizer = AssignmentOptimizer::new();
        assert!(optimizer.local_vars.is_empty());
    }
    
    #[test]
    fn test_local_var_registration() {
        let mut optimizer = AssignmentOptimizer::new();
        optimizer.register_local_var("x".to_string(), 1, "int".to_string());
        
        assert!(optimizer.local_vars.contains_key("x"));
        let var_info = &optimizer.local_vars["x"];
        assert_eq!(var_info.index, 1);
        assert!(var_info.is_int_compatible);
    }
    
    #[test]
    fn test_constant_extraction() {
        let optimizer = AssignmentOptimizer::new();
        
        let int_literal = Expr::Literal(LiteralExpr {
            value: Literal::Integer(42),
            span: create_span(),
        });
        
        assert_eq!(optimizer.extract_constant_int(&int_literal), Some(42));
        
        let minus_expr = Expr::Unary(UnaryExpr {
            operator: UnaryOp::Minus,
            operand: Box::new(int_literal),
            span: create_span(),
        });
        
        assert_eq!(optimizer.extract_constant_int(&minus_expr), Some(-42));
    }
    
    #[test]
    fn test_optimized_constant_load() {
        let optimizer = AssignmentOptimizer::new();
        
        // Test iconst range
        assert!(optimizer.can_use_optimized_constant_load(0));
        assert!(optimizer.can_use_optimized_constant_load(5));
        assert!(optimizer.can_use_optimized_constant_load(-1));
        
        // Test bipush range
        assert!(optimizer.can_use_optimized_constant_load(100));
        assert!(optimizer.can_use_optimized_constant_load(-100));
        
        // Test sipush range
        assert!(optimizer.can_use_optimized_constant_load(1000));
        assert!(optimizer.can_use_optimized_constant_load(-1000));
    }
    
    #[test]
    fn test_increment_analysis() {
        let pre_inc = UnaryExpr {
            operator: UnaryOp::PreInc,
            operand: Box::new(Expr::Identifier(IdentifierExpr {
                name: "x".to_string(),
                span: create_span(),
            })),
            span: create_span(),
        };
        
        let analysis = IncrementAnalyzer::analyze_increment(&pre_inc);
        assert!(matches!(analysis, IncrementOptimization::Increment { is_prefix: true }));
        
        let post_dec = UnaryExpr {
            operator: UnaryOp::PostDec,
            operand: Box::new(Expr::Identifier(IdentifierExpr {
                name: "x".to_string(),
                span: create_span(),
            })),
            span: create_span(),
        };
        
        let analysis = IncrementAnalyzer::analyze_increment(&post_dec);
        assert!(matches!(analysis, IncrementOptimization::Decrement { is_prefix: false }));
    }
    
    #[test]
    fn test_iinc_bytecode_generation() {
        let optimizer = AssignmentOptimizer::new();
        
        // Test standard iinc
        let bytecode = optimizer.generate_iinc_bytecode(1, 1).unwrap();
        assert_eq!(bytecode, vec![opcodes::IINC, 1, 1]);
        
        // Test wide iinc (large variable index)
        let bytecode = optimizer.generate_iinc_bytecode(300, 1).unwrap();
        assert_eq!(bytecode[0], opcodes::WIDE);
        assert_eq!(bytecode[1], opcodes::IINC);
    }
}
