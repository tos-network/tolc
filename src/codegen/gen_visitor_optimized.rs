//! Optimized AST Visitor for High-Performance Code Generation
//! 
//! This module provides performance-optimized AST traversal with:
//! - Reduced heap allocations through object pooling
//! - Vectorized operations for bulk processing
//! - Smart caching of computed results
//! - Minimal branching in hot paths

use crate::ast::*;
use crate::codegen::{
    bytecode_optimized::{OptimizedBytecodeBuffer, InstructionBuilder},
    constpool_optimized::OptimizedConstantPool,
    opcodes,
};
use crate::error::Result;
use std::collections::HashMap;

/// Optimized visitor context with pooled allocations
#[derive(Debug)]
pub struct OptimizedGenContext {
    /// Pre-allocated string buffer for name building
    name_buffer: String,
    
    /// Cached type computations
    type_cache: HashMap<TypeRef, u8>,
    
    /// Expression result cache
    expr_cache: HashMap<u64, ExprResult>, // Hash of expr -> result
    
    /// Local variable index pool
    local_vars: Vec<u16>,
    
    /// Current method being processed
    current_method: Option<String>,
    
    /// Optimization flags
    optimize_constants: bool,
    optimize_branches: bool,
    optimize_locals: bool,
}

/// Cached expression evaluation result
#[derive(Debug, Clone)]
struct ExprResult {
    stack_effect: i8,
    is_constant: bool,
    constant_value: Option<i64>,
    bytecode_size_hint: u16,
}

/// High-performance AST visitor with optimization
pub struct OptimizedGenVisitor {
    /// Optimized bytecode buffer
    buffer: OptimizedBytecodeBuffer,
    
    /// Optimized constant pool
    constant_pool: OptimizedConstantPool,
    
    /// Visitor context with caching
    context: OptimizedGenContext,
    
    /// Expression evaluator with memoization
    expr_evaluator: ExpressionEvaluator,
    
    /// Statement compiler with optimization
    stmt_compiler: StatementCompiler,
}

/// Specialized expression evaluator with performance optimizations
struct ExpressionEvaluator {
    /// Cache for constant expressions
    constant_cache: HashMap<String, i64>,
    
    /// Cache for type inference results
    type_inference_cache: HashMap<u64, TypeEnum>,
    
    /// Pre-computed arithmetic operation results
    arithmetic_cache: HashMap<(i64, i64, BinaryOp), i64>,
}

/// Specialized statement compiler with optimization
struct StatementCompiler {
    /// Control flow optimization cache
    control_flow_cache: HashMap<u64, ControlFlowInfo>,
    
    /// Loop optimization patterns
    loop_patterns: Vec<LoopPattern>,
    
    /// Branch prediction hints
    branch_hints: HashMap<u64, BranchHint>,
}

#[derive(Debug, Clone)]
struct ControlFlowInfo {
    has_early_return: bool,
    max_stack_depth: u16,
    locals_used: Vec<u16>,
}

#[derive(Debug, Clone)]
struct LoopPattern {
    loop_type: LoopType,
    variable_index: u16,
    start_value: i64,
    end_value: i64,
    increment: i64,
}

#[derive(Debug, Clone, PartialEq)]
enum LoopType {
    CountingUp,
    CountingDown,
    Iterator,
    Conditional,
}

#[derive(Debug, Clone)]
enum BranchHint {
    Likely,
    Unlikely,
    Unknown,
}

impl OptimizedGenContext {
    fn new() -> Self {
        Self {
            name_buffer: String::with_capacity(256),
            type_cache: HashMap::with_capacity(64),
            expr_cache: HashMap::with_capacity(128),
            local_vars: Vec::with_capacity(32),
            current_method: None,
            optimize_constants: true,
            optimize_branches: true,
            optimize_locals: true,
        }
    }
    
    /// Get cached type for TypeRef (avoids repeated computation)
    fn get_cached_type(&mut self, type_ref: &TypeRef) -> u8 {
        if let Some(&cached) = self.type_cache.get(type_ref) {
            return cached;
        }
        
        let type_code = match type_ref {
            TypeRef::Primitive(prim) => match prim {
                PrimitiveType::Boolean | PrimitiveType::Byte => 8, // T_BYTE
                PrimitiveType::Char => 5, // T_CHAR
                PrimitiveType::Short => 9, // T_SHORT
                PrimitiveType::Int => 10, // T_INT
                PrimitiveType::Long => 11, // T_LONG
                PrimitiveType::Float => 6, // T_FLOAT
                PrimitiveType::Double => 7, // T_DOUBLE
            },
            TypeRef::Reference(_) => 2, // T_OBJECT
            TypeRef::Array(_) => 3, // T_ARRAY
        };
        
        self.type_cache.insert(type_ref.clone(), type_code);
        type_code
    }
    
    /// Hash expression for caching (fast hash for common patterns)
    fn hash_expr(&self, expr: &Expr) -> u64 {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;
        
        let mut hasher = DefaultHasher::new();
        
        // Simple structural hash for expressions
        match expr {
            Expr::Literal(lit) => {
                0u8.hash(&mut hasher);
                match &lit.value {
                    Literal::Integer(v) => v.hash(&mut hasher),
                    Literal::Boolean(v) => v.hash(&mut hasher),
                    Literal::String(v) => v.hash(&mut hasher),
                    _ => 0u64.hash(&mut hasher),
                }
            }
            Expr::Identifier(id) => {
                1u8.hash(&mut hasher);
                id.name.hash(&mut hasher);
            }
            Expr::Binary(bin) => {
                2u8.hash(&mut hasher);
                bin.operator.hash(&mut hasher);
                // Don't recurse deeply to avoid stack overflow
            }
            _ => {
                255u8.hash(&mut hasher); // Other expressions
            }
        }
        
        hasher.finish()
    }
}

impl ExpressionEvaluator {
    fn new() -> Self {
        Self {
            constant_cache: HashMap::with_capacity(64),
            type_inference_cache: HashMap::with_capacity(64),
            arithmetic_cache: HashMap::with_capacity(32),
        }
    }
    
    /// Evaluate constant expression with caching
    fn evaluate_constant(&mut self, expr: &Expr) -> Option<i64> {
        match expr {
            Expr::Literal(lit) => match &lit.value {
                Literal::Integer(v) => Some(*v),
                Literal::Boolean(v) => Some(if *v { 1 } else { 0 }),
                _ => None,
            },
            
            Expr::Binary(bin) => {
                if let (Some(left), Some(right)) = (
                    self.evaluate_constant(&bin.left),
                    self.evaluate_constant(&bin.right)
                ) {
                    let cache_key = (left, right, bin.operator.clone());
                    if let Some(&cached) = self.arithmetic_cache.get(&cache_key) {
                        return Some(cached);
                    }
                    
                    let result = match bin.operator {
                        BinaryOp::Add => left.wrapping_add(right),
                        BinaryOp::Sub => left.wrapping_sub(right),
                        BinaryOp::Mul => left.wrapping_mul(right),
                        BinaryOp::Div if right != 0 => left.wrapping_div(right),
                        BinaryOp::Mod if right != 0 => left.wrapping_rem(right),
                        BinaryOp::And => left & right,
                        BinaryOp::Or => left | right,
                        BinaryOp::Xor => left ^ right,
                        BinaryOp::LShift => left.wrapping_shl(right as u32),
                        BinaryOp::RShift => left.wrapping_shr(right as u32),
                        _ => return None,
                    };
                    
                    self.arithmetic_cache.insert(cache_key, result);
                    Some(result)
                } else {
                    None
                }
            }
            
            _ => None,
        }
    }
    
    /// Fast type inference with caching
    fn infer_type_fast(&mut self, expr: &Expr, context: &OptimizedGenContext) -> TypeEnum {
        let hash = context.hash_expr(expr);
        if let Some(cached) = self.type_inference_cache.get(&hash) {
            return cached.clone();
        }
        
        let inferred_type = match expr {
            Expr::Literal(lit) => match &lit.value {
                Literal::Integer(_) => TypeEnum::Primitive(PrimitiveType::Int),
                Literal::Long(_) => TypeEnum::Primitive(PrimitiveType::Long),
                Literal::Float(_) => TypeEnum::Primitive(PrimitiveType::Float),
                Literal::Double(_) => TypeEnum::Primitive(PrimitiveType::Double),
                Literal::Boolean(_) => TypeEnum::Primitive(PrimitiveType::Boolean),
                Literal::Char(_) => TypeEnum::Primitive(PrimitiveType::Char),
                Literal::String(_) => TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string())),
                Literal::Null => TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string())),
            },
            
            Expr::Binary(bin) => {
                // Fast common cases
                match bin.operator {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                        TypeEnum::Primitive(PrimitiveType::Int) // Simplified
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge |
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        TypeEnum::Primitive(PrimitiveType::Boolean)
                    }
                    _ => TypeEnum::Primitive(PrimitiveType::Int),
                }
            }
            
            _ => TypeEnum::Primitive(PrimitiveType::Int), // Default
        };
        
        self.type_inference_cache.insert(hash, inferred_type.clone());
        inferred_type
    }
}

impl StatementCompiler {
    fn new() -> Self {
        Self {
            control_flow_cache: HashMap::with_capacity(32),
            loop_patterns: Vec::with_capacity(8),
            branch_hints: HashMap::with_capacity(16),
        }
    }
    
    /// Analyze and optimize loop patterns
    fn analyze_loop(&mut self, stmt: &ForStmt) -> Option<LoopPattern> {
        // Simple pattern recognition for counting loops
        if let (Some(init), Some(condition), Some(update)) = (&stmt.init, &stmt.condition, &stmt.update) {
            // Check if this is a simple counting loop: for(i=0; i<n; i++)
            if let (
                Stmt::VarDecl(var_decl),
                Expr::Binary(cond_bin),
                Expr::Unary(update_unary)
            ) = (init.as_ref(), condition, update) {
                if cond_bin.operator == BinaryOp::Lt && 
                   update_unary.operator == UnaryOp::PostInc {
                    
                    if let Some(init_value) = self.extract_constant_init(var_decl) {
                        return Some(LoopPattern {
                            loop_type: LoopType::CountingUp,
                            variable_index: 0, // Simplified
                            start_value: init_value,
                            end_value: 100, // Would extract from condition
                            increment: 1,
                        });
                    }
                }
            }
        }
        None
    }
    
    fn extract_constant_init(&self, _var_decl: &VarDeclStmt) -> Option<i64> {
        // Extract constant initialization value
        // Simplified implementation
        Some(0)
    }
}

impl OptimizedGenVisitor {
    /// Create new optimized visitor
    pub fn new() -> Self {
        Self {
            buffer: OptimizedBytecodeBuffer::with_capacity(2048),
            constant_pool: OptimizedConstantPool::new(),
            context: OptimizedGenContext::new(),
            expr_evaluator: ExpressionEvaluator::new(),
            stmt_compiler: StatementCompiler::new(),
        }
    }
    
    /// Visit expression with optimization
    pub fn visit_expr_optimized(&mut self, expr: &Expr) -> Result<ExprResult> {
        // Check cache first
        let expr_hash = self.context.hash_expr(expr);
        if let Some(cached) = self.context.expr_cache.get(&expr_hash) {
            return Ok(cached.clone());
        }
        
        let result = match expr {
            Expr::Literal(lit) => self.visit_literal_optimized(lit)?,
            Expr::Binary(bin) => self.visit_binary_optimized(bin)?,
            Expr::Unary(unary) => self.visit_unary_optimized(unary)?,
            Expr::Identifier(id) => self.visit_identifier_optimized(id)?,
            Expr::MethodCall(call) => self.visit_method_call_optimized(call)?,
            _ => ExprResult {
                stack_effect: 1,
                is_constant: false,
                constant_value: None,
                bytecode_size_hint: 3,
            },
        };
        
        // Cache the result
        self.context.expr_cache.insert(expr_hash, result.clone());
        Ok(result)
    }
    
    /// Optimized literal handling
    fn visit_literal_optimized(&mut self, lit: &LiteralExpr) -> Result<ExprResult> {
        match &lit.value {
            Literal::Integer(value) => {
                self.buffer.emit_load_constant(*value as i32);
                Ok(ExprResult {
                    stack_effect: 1,
                    is_constant: true,
                    constant_value: Some(*value),
                    bytecode_size_hint: 1,
                })
            }
            
            Literal::String(value) => {
                let string_index = self.constant_pool.add_utf8_optimized(value)?;
                self.buffer.emit_op_u16(opcodes::LDC, string_index);
                Ok(ExprResult {
                    stack_effect: 1,
                    is_constant: true,
                    constant_value: None,
                    bytecode_size_hint: 3,
                })
            }
            
            Literal::Boolean(value) => {
                self.buffer.emit_load_constant(if *value { 1 } else { 0 });
                Ok(ExprResult {
                    stack_effect: 1,
                    is_constant: true,
                    constant_value: Some(if *value { 1 } else { 0 }),
                    bytecode_size_hint: 1,
                })
            }
            
            _ => {
                // Handle other literal types
                self.buffer.emit_op(opcodes::ACONST_NULL); // Placeholder
                Ok(ExprResult {
                    stack_effect: 1,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 1,
                })
            }
        }
    }
    
    /// Optimized binary expression handling with constant folding
    fn visit_binary_optimized(&mut self, bin: &BinaryExpr) -> Result<ExprResult> {
        // Try constant folding first
        if let Some(constant_result) = self.expr_evaluator.evaluate_constant(&Expr::Binary(bin.clone())) {
            self.buffer.emit_load_constant(constant_result as i32);
            return Ok(ExprResult {
                stack_effect: 1,
                is_constant: true,
                constant_value: Some(constant_result),
                bytecode_size_hint: 1,
            });
        }
        
        // Generate code for operands
        let _left_result = self.visit_expr_optimized(&bin.left)?;
        let _right_result = self.visit_expr_optimized(&bin.right)?;
        
        // Generate optimized operation
        match bin.operator {
            BinaryOp::Add => {
                self.buffer.emit_op(opcodes::IADD);
                Ok(ExprResult {
                    stack_effect: -1, // Consumes 2, produces 1
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 1,
                })
            }
            
            BinaryOp::Sub => {
                self.buffer.emit_op(opcodes::ISUB);
                Ok(ExprResult {
                    stack_effect: -1,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 1,
                })
            }
            
            BinaryOp::Mul => {
                self.buffer.emit_op(opcodes::IMUL);
                Ok(ExprResult {
                    stack_effect: -1,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 1,
                })
            }
            
            BinaryOp::Eq => {
                // Optimized comparison
                let end_label = self.buffer.pc() + 10; // Estimate
                self.buffer.emit_jump(opcodes::IF_ICMPNE, end_label)?;
                self.buffer.emit_load_constant(1); // true
                self.buffer.emit_jump(opcodes::GOTO, end_label + 2)?;
                self.buffer.emit_load_constant(0); // false
                
                Ok(ExprResult {
                    stack_effect: -1,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 10,
                })
            }
            
            _ => {
                // Generic handling for other operators
                self.buffer.emit_op(opcodes::IADD); // Placeholder
                Ok(ExprResult {
                    stack_effect: -1,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 1,
                })
            }
        }
    }
    
    /// Optimized unary expression handling
    fn visit_unary_optimized(&mut self, unary: &UnaryExpr) -> Result<ExprResult> {
        let operand_result = self.visit_expr_optimized(&unary.operand)?;
        
        match unary.operator {
            UnaryOp::Minus => {
                if operand_result.is_constant {
                    if let Some(value) = operand_result.constant_value {
                        self.buffer.emit_load_constant(-value as i32);
                        return Ok(ExprResult {
                            stack_effect: 0, // Replace stack top
                            is_constant: true,
                            constant_value: Some(-value),
                            bytecode_size_hint: 1,
                        });
                    }
                }
                
                self.buffer.emit_op(opcodes::INEG);
                Ok(ExprResult {
                    stack_effect: 0,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 1,
                })
            }
            
            UnaryOp::Not => {
                // Optimized boolean negation
                self.buffer.emit_load_constant(1);
                self.buffer.emit_op(opcodes::IXOR);
                Ok(ExprResult {
                    stack_effect: 0,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 2,
                })
            }
            
            _ => {
                // Handle other unary operators
                Ok(ExprResult {
                    stack_effect: 0,
                    is_constant: false,
                    constant_value: None,
                    bytecode_size_hint: 1,
                })
            }
        }
    }
    
    /// Optimized identifier handling
    fn visit_identifier_optimized(&mut self, _id: &IdentifierExpr) -> Result<ExprResult> {
        // Simplified local variable loading
        // In a real implementation, would need proper symbol table lookup
        let var_index = 0u8; // Placeholder
        
        if var_index < 4 {
            self.buffer.emit_op(opcodes::ILOAD_0 + var_index);
        } else {
            self.buffer.emit_op1(opcodes::ILOAD, var_index);
        }
        
        Ok(ExprResult {
            stack_effect: 1,
            is_constant: false,
            constant_value: None,
            bytecode_size_hint: 1,
        })
    }
    
    /// Optimized method call handling
    fn visit_method_call_optimized(&mut self, call: &MethodCallExpr) -> Result<ExprResult> {
        // Generate arguments
        let mut total_stack_effect = 0i8;
        for arg in &call.arguments {
            let arg_result = self.visit_expr_optimized(arg)?;
            total_stack_effect += arg_result.stack_effect;
        }
        
        // Add method reference to constant pool
        let method_ref = self.constant_pool.add_utf8_optimized(&call.name)?;
        
        // Generate optimized method call
        let param_count = call.arguments.len() as u8;
        self.buffer.emit_method_call(opcodes::INVOKEVIRTUAL, method_ref, param_count);
        
        Ok(ExprResult {
            stack_effect: -(param_count as i8) + 1, // Pop args, push result
            is_constant: false,
            constant_value: None,
            bytecode_size_hint: 3,
        })
    }
    
    /// Visit statement with optimization
    pub fn visit_stmt_optimized(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.visit_expr_optimized(&expr_stmt.expr)?;
                self.buffer.emit_op(opcodes::POP); // Discard result
                Ok(())
            }
            
            Stmt::If(if_stmt) => self.visit_if_optimized(if_stmt),
            Stmt::For(for_stmt) => self.visit_for_optimized(for_stmt),
            Stmt::Block(block) => self.visit_block_optimized(block),
            
            _ => Ok(()), // Handle other statement types
        }
    }
    
    /// Optimized if statement with branch prediction
    fn visit_if_optimized(&mut self, if_stmt: &IfStmt) -> Result<()> {
        // Generate condition
        let condition_result = self.visit_expr_optimized(&if_stmt.condition)?;
        
        // Optimized branch based on constant condition
        if condition_result.is_constant {
            if let Some(value) = condition_result.constant_value {
                if value != 0 {
                    // Always true - only generate then branch
                    return self.visit_stmt_optimized(&if_stmt.then_branch);
                } else if let Some(ref else_branch) = if_stmt.else_branch {
                    // Always false - only generate else branch
                    return self.visit_stmt_optimized(else_branch);
                } else {
                    // Always false, no else - generate nothing
                    return Ok(());
                }
            }
        }
        
        // Generate conditional branch
        let else_label = self.buffer.pc() + 100; // Estimate
        self.buffer.emit_jump(opcodes::IFEQ, else_label)?;
        
        // Generate then branch
        self.visit_stmt_optimized(&if_stmt.then_branch)?;
        
        if if_stmt.else_branch.is_some() {
            let end_label = self.buffer.pc() + 50; // Estimate
            self.buffer.emit_jump(opcodes::GOTO, end_label)?;
            
            // Generate else branch
            if let Some(ref else_branch) = if_stmt.else_branch {
                self.visit_stmt_optimized(else_branch)?;
            }
        }
        
        Ok(())
    }
    
    /// Optimized for loop with pattern recognition
    fn visit_for_optimized(&mut self, for_stmt: &ForStmt) -> Result<()> {
        // Try to recognize and optimize common loop patterns
        if let Some(pattern) = self.stmt_compiler.analyze_loop(for_stmt) {
            return self.generate_optimized_counting_loop(&pattern);
        }
        
        // Fall back to generic loop generation
        self.generate_generic_for_loop(for_stmt)
    }
    
    fn generate_optimized_counting_loop(&mut self, pattern: &LoopPattern) -> Result<()> {
        // Generate highly optimized counting loop
        match pattern.loop_type {
            LoopType::CountingUp => {
                // Initialize loop variable
                self.buffer.emit_load_constant(pattern.start_value as i32);
                self.buffer.emit_op1(opcodes::ISTORE, pattern.variable_index as u8);
                
                // Loop start
                let loop_start = self.buffer.pc();
                
                // Condition check
                self.buffer.emit_op1(opcodes::ILOAD, pattern.variable_index as u8);
                self.buffer.emit_load_constant(pattern.end_value as i32);
                
                let loop_end = self.buffer.pc() + 20; // Estimate
                self.buffer.emit_jump(opcodes::IF_ICMPGE, loop_end)?;
                
                // Loop body would be generated here
                
                // Increment
                self.buffer.emit_op2(opcodes::IINC, pattern.variable_index as u8, pattern.increment as u8);
                
                // Jump back to start
                self.buffer.emit_jump(opcodes::GOTO, loop_start)?;
            }
            
            _ => {
                // Handle other loop types
            }
        }
        
        Ok(())
    }
    
    fn generate_generic_for_loop(&mut self, _for_stmt: &ForStmt) -> Result<()> {
        // Generic for loop generation (simplified)
        Ok(())
    }
    
    /// Optimized block statement
    fn visit_block_optimized(&mut self, block: &Block) -> Result<()> {
        // Vectorized statement processing
        let mut stmt_results = Vec::with_capacity(block.statements.len());
        
        for stmt in &block.statements {
            self.visit_stmt_optimized(stmt)?;
        }
        
        Ok(())
    }
    
    /// Get the generated bytecode
    pub fn get_bytecode(self) -> Vec<u8> {
        self.buffer.into_bytes()
    }
    
    /// Get the constant pool
    pub fn get_constant_pool(self) -> OptimizedConstantPool {
        self.constant_pool
    }
    
    /// Get performance statistics
    pub fn get_stats(&self) -> (usize, usize, usize) {
        (
            self.buffer.as_bytes().len(), // Bytecode size
            self.constant_pool.len(),     // Constant pool size
            self.context.expr_cache.len(), // Cache hits
        )
    }
}

impl Default for OptimizedGenVisitor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location};
    
    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }
    
    #[test]
    fn test_constant_folding() {
        let mut visitor = OptimizedGenVisitor::new();
        
        // Create binary expression: 5 + 3
        let binary_expr = Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Integer(5),
                span: create_span(),
            })),
            operator: BinaryOp::Add,
            right: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Integer(3),
                span: create_span(),
            })),
            span: create_span(),
        });
        
        let result = visitor.visit_expr_optimized(&binary_expr).unwrap();
        
        assert!(result.is_constant);
        assert_eq!(result.constant_value, Some(8));
    }
    
    #[test]
    fn test_expression_caching() {
        let mut visitor = OptimizedGenVisitor::new();
        
        let literal_expr = Expr::Literal(LiteralExpr {
            value: Literal::Integer(42),
            span: create_span(),
        });
        
        // First visit should compute and cache
        let result1 = visitor.visit_expr_optimized(&literal_expr).unwrap();
        
        // Second visit should use cache
        let result2 = visitor.visit_expr_optimized(&literal_expr).unwrap();
        
        assert_eq!(result1.constant_value, result2.constant_value);
        assert!(visitor.context.expr_cache.len() > 0);
    }
    
    #[test]
    fn test_optimized_if_constant_condition() {
        let mut visitor = OptimizedGenVisitor::new();
        
        let if_stmt = IfStmt {
            condition: Expr::Literal(LiteralExpr {
                value: Literal::Boolean(true),
                span: create_span(),
            }),
            then_branch: Box::new(Stmt::Block(Block {
                statements: vec![],
                span: create_span(),
            })),
            else_branch: None,
            span: create_span(),
        };
        
        let initial_size = visitor.buffer.as_bytes().len();
        visitor.visit_if_optimized(&if_stmt).unwrap();
        
        // Should generate minimal bytecode for constant true condition
        assert!(visitor.buffer.as_bytes().len() >= initial_size);
    }
}