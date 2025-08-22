//! Lower phase optimizer - JavaC Lower.java aligned
//!
//! This module implements optimizations that occur during the lowering phase,
//! following the exact same patterns as Oracle's javac Lower.java.

use crate::ast::*;
use crate::common::error::Result;
use super::const_fold_javac::ConstFoldJavaC;
use super::symtab::Symtab;
use super::types::Types;

/// Lower phase optimizer - JavaC Lower equivalent
/// Handles code lowering and transformations before bytecode generation
pub struct LowerOptimizer {
    /// Constant folding system
    cfolder: ConstFoldJavaC,
    
    /// Types system
    types: Types,
    
    /// Symbol table
    symtab: Symtab,
    
    /// Next synthetic variable ID
    next_synthetic_id: u32,
}

impl LowerOptimizer {
    /// Create new lower optimizer - JavaC Lower constructor equivalent
    pub fn new(symtab: Symtab, types: Types) -> Self {
        let cfolder = ConstFoldJavaC::new(symtab.clone());
        
        Self {
            cfolder,
            types,
            symtab,
            next_synthetic_id: 0,
        }
    }
    
    /// Lower and optimize expression - JavaC visitXxx equivalent
    pub fn lower_expr(&mut self, expr: &Expr) -> Result<Expr> {
        match expr {
            // String concatenation optimization
            Expr::Binary(bin) if bin.operator == BinaryOp::Add => {
                self.lower_string_concatenation(bin)
            }
            
            // Compound assignment optimization
            Expr::Assignment(assign) => self.lower_assignment(assign),
            
            // Method call optimization
            Expr::MethodCall(call) => self.lower_method_call(call),
            
            // Array initialization (using existing ArrayInitializer variant)
            Expr::ArrayInitializer(elements) => {
                let lowered_elements = elements.iter()
                    .map(|e| self.lower_expr(e))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Expr::ArrayInitializer(lowered_elements))
            }
            
            // Lambda expressions (if supported)
            // TODO: Add lambda lowering when lambdas are implemented
            
            // Binary expressions
            Expr::Binary(bin) => self.lower_binary(bin),
            
            // Unary expressions
            Expr::Unary(unary) => self.lower_unary(unary),
            
            // Literals and identifiers - no lowering needed
            Expr::Literal(_) | Expr::Identifier(_) => Ok(expr.clone()),
            
            _ => {
                // Default: recurse into sub-expressions
                self.lower_expr_default(expr)
            }
        }
    }
    
    /// Lower string concatenation - JavaC pattern
    fn lower_string_concatenation(&mut self, bin: &BinaryExpr) -> Result<Expr> {
        // JavaC: Check if this is string concatenation
        if !self.is_string_concatenation(bin) {
            return self.lower_binary(bin);
        }
        
        // JavaC: Collect all string concatenation operands
        let mut operands = Vec::new();
        self.collect_string_operands(&Expr::Binary(bin.clone()), &mut operands)?;
        
        // JavaC: Optimize based on operand count and types
        // Always use StringBuilder for string concatenation to avoid Object.append issues
        if operands.len() < 2 {
            // Not actually concatenation, fallback to binary
            return self.lower_binary(bin);
        }
        
        // JavaC: Complex string concatenation - use StringBuilder pattern
        self.create_string_builder_chain(operands, bin.span.clone())
    }
    
    /// Check if binary expression is string concatenation
    fn is_string_concatenation(&self, bin: &BinaryExpr) -> bool {
        if bin.operator != BinaryOp::Add {
            return false;
        }
        
        // Simple heuristic: check for string literals or known string types
        self.is_string_expression(&bin.left) || self.is_string_expression(&bin.right)
    }
    
    /// Check if expression produces a string
    fn is_string_expression(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(LiteralExpr { value: Literal::String(_), .. }) => true,
            Expr::MethodCall(call) if call.name.contains("toString") => true,
            Expr::MethodCall(call) if call.name.contains("String.valueOf") => true,
            _ => false, // TODO: Add type-based checking
        }
    }
    
    /// Collect all operands in a string concatenation chain
    fn collect_string_operands(&mut self, expr: &Expr, operands: &mut Vec<Expr>) -> Result<()> {
        match expr {
            Expr::Binary(bin) if bin.operator == BinaryOp::Add && self.is_string_concatenation(bin) => {
                self.collect_string_operands(&bin.left, operands)?;
                self.collect_string_operands(&bin.right, operands)?;
            }
            _ => {
                operands.push(self.lower_expr(expr)?);
            }
        }
        Ok(())
    }
    
    /// Create StringBuilder chain for complex string concatenation
    fn create_string_builder_chain(&mut self, operands: Vec<Expr>, span: Span) -> Result<Expr> {
        // JavaC pattern: new StringBuilder().append(a).append(b)...toString()
        
        // Estimate initial capacity (JavaC optimization)
        let estimated_capacity = self.estimate_string_capacity(&operands);
        
        // Create new StringBuilder(capacity) using proper object construction
        let stringbuilder_type = TypeRef {
            name: "StringBuilder".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: span.clone(),
        };
        
        let mut result = Expr::New(NewExpr {
            target_type: stringbuilder_type,
            arguments: if estimated_capacity > 16 {
                vec![Expr::Literal(LiteralExpr {
                    value: Literal::Integer(estimated_capacity as i64),
                    span: span.clone(),
                })]
            } else {
                vec![] // Use default capacity
            },
            anonymous_body: None,
            span: span.clone(),
        });
        
        // Chain append calls
        for operand in operands {
            result = Expr::MethodCall(MethodCallExpr {
                target: Some(Box::new(result)),
                name: "append".to_string(),
                arguments: vec![operand],
                span: span.clone(),
            });
        }
        
        // Final toString() call
        result = Expr::MethodCall(MethodCallExpr {
            target: Some(Box::new(result)),
            name: "toString".to_string(),
            arguments: vec![],
            span: span.clone(),
        });
        
        Ok(result)
    }
    
    /// Estimate string capacity for StringBuilder optimization
    fn estimate_string_capacity(&self, operands: &[Expr]) -> usize {
        let mut capacity = 0;
        
        for operand in operands {
            capacity += match operand {
                Expr::Literal(LiteralExpr { value: Literal::String(s), .. }) => s.len(),
                Expr::Literal(LiteralExpr { value: Literal::Integer(_), .. }) => 11, // Max int length
                Expr::Literal(LiteralExpr { value: Literal::Long(_), .. }) => 20,    // Max long length
                Expr::Literal(LiteralExpr { value: Literal::Boolean(_), .. }) => 5,  // "false"
                _ => 16, // Default estimate
            };
        }
        
        capacity
    }
    
    /// Lower assignment expression - JavaC pattern
    fn lower_assignment(&mut self, assign: &AssignmentExpr) -> Result<Expr> {
        let lowered_left = self.lower_expr(&assign.target)?;
        let lowered_right = self.lower_expr(&assign.value)?;
        
        // JavaC: Check for compound assignment optimization
        match &assign.operator {
            AssignmentOp::AddAssign => {
                // JavaC: x += y becomes x = x + y, then optimize the addition
                let addition = BinaryExpr {
                    left: Box::new(lowered_left.clone()),
                    operator: BinaryOp::Add,
                    right: Box::new(lowered_right),
                    span: assign.span.clone(),
                };
                
                let optimized_addition = self.lower_expr(&Expr::Binary(addition))?;
                
                Ok(Expr::Assignment(AssignmentExpr {
                    target: Box::new(lowered_left),
                    operator: AssignmentOp::Assign,
                    value: Box::new(optimized_addition),
                    span: assign.span.clone(),
                }))
            }
            
            // Other compound assignments
            AssignmentOp::SubAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::Sub, assign.span.clone()),
            AssignmentOp::MulAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::Mul, assign.span.clone()),
            AssignmentOp::DivAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::Div, assign.span.clone()),
            AssignmentOp::ModAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::Mod, assign.span.clone()),
            AssignmentOp::AndAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::And, assign.span.clone()),
            AssignmentOp::OrAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::Or, assign.span.clone()),
            AssignmentOp::XorAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::Xor, assign.span.clone()),
            AssignmentOp::LShiftAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::LShift, assign.span.clone()),
            AssignmentOp::RShiftAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::RShift, assign.span.clone()),
            AssignmentOp::URShiftAssign => self.expand_compound_assignment(&lowered_left, &lowered_right, BinaryOp::URShift, assign.span.clone()),
            
            // Simple assignment
            AssignmentOp::Assign => {
                Ok(Expr::Assignment(AssignmentExpr {
                    target: Box::new(lowered_left),
                    operator: assign.operator.clone(),
                    value: Box::new(lowered_right),
                    span: assign.span.clone(),
                }))
            }
        }
    }
    
    /// Expand compound assignment - JavaC pattern
    fn expand_compound_assignment(&mut self, left: &Expr, right: &Expr, op: BinaryOp, span: Span) -> Result<Expr> {
        // JavaC: x op= y becomes x = x op y
        let binary_expr = BinaryExpr {
            left: Box::new(left.clone()),
            operator: op,
            right: Box::new(right.clone()),
            span: span.clone(),
        };
        
        let optimized_binary = self.lower_expr(&Expr::Binary(binary_expr))?;
        
        Ok(Expr::Assignment(AssignmentExpr {
            target: Box::new(left.clone()),
            operator: AssignmentOp::Assign,
            value: Box::new(optimized_binary),
            span,
        }))
    }
    
    /// Lower method call - JavaC visitApply equivalent
    fn lower_method_call(&mut self, call: &MethodCallExpr) -> Result<Expr> {
        // Lower target and arguments
        let target = if let Some(ref t) = call.target {
            Some(Box::new(self.lower_expr(t)?))
        } else {
            None
        };
        
        let mut lowered_args = Vec::new();
        for arg in &call.arguments {
            lowered_args.push(self.lower_expr(arg)?);
        }
        
        // JavaC: Check for special method optimizations
        let optimized_call = self.optimize_special_methods(call, target, lowered_args)?;
        
        Ok(optimized_call)
    }
    
    /// Optimize special method calls - JavaC pattern
    fn optimize_special_methods(&mut self, call: &MethodCallExpr, target: Option<Box<Expr>>, args: Vec<Expr>) -> Result<Expr> {
        match call.name.as_str() {
            // JavaC: System.arraycopy optimization
            "System.arraycopy" if args.len() == 5 => {
                // Could be optimized for known array types and constant lengths
                Ok(Expr::MethodCall(MethodCallExpr {
                    target,
                    name: call.name.clone(),
                    arguments: args,
                    span: call.span.clone(),
                }))
            }
            
            // JavaC: Object.getClass() optimization
            "getClass" if args.is_empty() => {
                // Could be optimized if target type is known at compile time
                Ok(Expr::MethodCall(MethodCallExpr {
                    target,
                    name: call.name.clone(),
                    arguments: args,
                    span: call.span.clone(),
                }))
            }
            
            // JavaC: Autoboxing/unboxing optimization
            "valueOf" if args.len() == 1 => {
                // Check if this is a wrapper valueOf call
                self.optimize_autoboxing(&call.name, &args[0], call.span.clone())
            }
            
            "intValue" | "longValue" | "floatValue" | "doubleValue" | "booleanValue" if args.is_empty() => {
                // Check if this is an unboxing call
                if let Some(ref t) = target {
                    self.optimize_unboxing(&call.name, t, call.span.clone())
                } else {
                    Ok(Expr::MethodCall(MethodCallExpr {
                        target,
                        name: call.name.clone(),
                        arguments: args,
                        span: call.span.clone(),
                    }))
                }
            }
            
            _ => {
                // No special optimization
                Ok(Expr::MethodCall(MethodCallExpr {
                    target,
                    name: call.name.clone(),
                    arguments: args,
                    span: call.span.clone(),
                }))
            }
        }
    }
    
    /// Optimize autoboxing - JavaC pattern
    fn optimize_autoboxing(&mut self, method_name: &str, arg: &Expr, span: Span) -> Result<Expr> {
        // JavaC: For compile-time constants, we can cache common values
        if let Expr::Literal(lit) = arg {
            match (method_name, &lit.value) {
                ("Integer.valueOf", Literal::Integer(v)) if *v >= -128 && *v <= 127 => {
                    // JavaC: Use cached Integer instances for small values
                    return Ok(arg.clone()); // Keep as literal for now
                }
                ("Boolean.valueOf", Literal::Boolean(_)) => {
                    // JavaC: Boolean.TRUE and Boolean.FALSE are cached
                    return Ok(arg.clone());
                }
                _ => {}
            }
        }
        
        // Default: create method call
        Ok(Expr::MethodCall(MethodCallExpr {
            target: None,
            name: method_name.to_string(),
            arguments: vec![arg.clone()],
            span,
        }))
    }
    
    /// Optimize unboxing - JavaC pattern
    fn optimize_unboxing(&mut self, method_name: &str, target: &Expr, span: Span) -> Result<Expr> {
        // JavaC: If target is a known wrapper constant, extract the primitive value
        // For now, keep as method call
        Ok(Expr::MethodCall(MethodCallExpr {
            target: Some(Box::new(target.clone())),
            name: method_name.to_string(),
            arguments: vec![],
            span,
        }))
    }
    
    // Array creation and initialization will be handled through NewExpr and ArrayInitializer
    // for compatibility with existing AST structure
    
    /// Lower binary expression - JavaC pattern
    fn lower_binary(&mut self, bin: &BinaryExpr) -> Result<Expr> {
        let left = self.lower_expr(&bin.left)?;
        let right = self.lower_expr(&bin.right)?;
        
        Ok(Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: bin.operator.clone(),
            right: Box::new(right),
            span: bin.span.clone(),
        }))
    }
    
    /// Lower unary expression - JavaC pattern
    fn lower_unary(&mut self, unary: &UnaryExpr) -> Result<Expr> {
        let operand = self.lower_expr(&unary.operand)?;
        
        // JavaC: Special case for boolean NOT optimization
        if let (UnaryOp::Not, Expr::Literal(LiteralExpr { value: Literal::Boolean(b), span })) = (&unary.operator, &operand) {
            // JavaC: tree.type = cfolder.fold1(bool_not, tree.arg.type);
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::Boolean(!b),
                span: span.clone(),
            }));
        }
        
        Ok(Expr::Unary(UnaryExpr {
            operator: unary.operator.clone(),
            operand: Box::new(operand),
            span: unary.span.clone(),
        }))
    }
    
    /// Lower expression with default recursion
    fn lower_expr_default(&mut self, expr: &Expr) -> Result<Expr> {
        // Default implementation: recurse into sub-expressions
        match expr {
            Expr::Cast(cast) => {
                let lowered_expr = self.lower_expr(&cast.expr)?;
                Ok(Expr::Cast(CastExpr {
                    target_type: cast.target_type.clone(),
                    expr: Box::new(lowered_expr),
                    span: cast.span.clone(),
                }))
            }
            
            Expr::FieldAccess(field) => {
                let target = if let Some(ref t) = field.target {
                    Some(Box::new(self.lower_expr(t)?))
                } else {
                    None
                };
                
                Ok(Expr::FieldAccess(FieldAccessExpr {
                    target,
                    name: field.name.clone(),
                    span: field.span.clone(),
                }))
            }
            
            Expr::ArrayAccess(array) => {
                let array_expr = self.lower_expr(&array.array)?;
                let index_expr = self.lower_expr(&array.index)?;
                
                Ok(Expr::ArrayAccess(ArrayAccessExpr {
                    array: Box::new(array_expr),
                    index: Box::new(index_expr),
                    span: array.span.clone(),
                }))
            }
            
            _ => Ok(expr.clone()),
        }
    }
    
    /// Lower statement - JavaC visitXxx equivalent
    pub fn lower_stmt(&mut self, stmt: &Stmt) -> Result<Stmt> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                let lowered_expr = self.lower_expr(&expr_stmt.expr)?;
                Ok(Stmt::Expression(ExprStmt {
                    expr: lowered_expr,
                    span: expr_stmt.span.clone(),
                }))
            }
            
            Stmt::Block(block) => {
                let mut lowered_statements = Vec::new();
                for stmt in &block.statements {
                    lowered_statements.push(self.lower_stmt(stmt)?);
                }
                
                Ok(Stmt::Block(Block {
                    statements: lowered_statements,
                    span: block.span.clone(),
                }))
            }
            
            Stmt::If(if_stmt) => {
                let condition = self.lower_expr(&if_stmt.condition)?;
                let then_branch = self.lower_stmt(&if_stmt.then_branch)?;
                let else_branch = if let Some(ref else_stmt) = if_stmt.else_branch {
                    Some(Box::new(self.lower_stmt(else_stmt)?))
                } else {
                    None
                };
                
                Ok(Stmt::If(IfStmt {
                    condition,
                    then_branch: Box::new(then_branch),
                    else_branch,
                    span: if_stmt.span.clone(),
                }))
            }
            
            // Other statements - TODO: implement as needed
            _ => Ok(stmt.clone()),
        }
    }
    
    /// Generate synthetic variable name - JavaC pattern
    fn generate_synthetic_name(&mut self, prefix: &str) -> String {
        let name = format!("{}${}", prefix, self.next_synthetic_id);
        self.next_synthetic_id += 1;
        name
    }
}