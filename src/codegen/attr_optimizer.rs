//! Attribute phase optimizer - JavaC Attr.java aligned
//!
//! This module implements optimizations that occur during the attribute analysis phase,
//! following the exact same patterns as Oracle's javac Attr.java.

use crate::ast::*;
use crate::common::error::Result;
use super::const_fold_javac::ConstFoldJavaC;
use super::symtab::Symtab;
use super::types::Types;

/// Attribute phase optimizer - JavaC Attr equivalent
/// Handles type attribution and early optimizations during semantic analysis
pub struct AttrOptimizer {
    /// Constant folding system
    cfolder: ConstFoldJavaC,
    
    /// Types system
    types: Types,
    
    /// Symbol table
    symtab: Symtab,
}

impl AttrOptimizer {
    /// Create new attribute optimizer - JavaC Attr constructor equivalent
    pub fn new(symtab: Symtab, types: Types) -> Self {
        let cfolder = ConstFoldJavaC::new(symtab.clone());
        
        Self {
            cfolder,
            types,
            symtab,
        }
    }
    
    /// Attribute and optimize expression - JavaC attribExpr equivalent
    pub fn attrib_expr(&mut self, expr: &Expr) -> Result<Expr> {
        match expr {
            // Binary expressions with constant folding
            Expr::Binary(bin) => self.attrib_binary(bin),
            
            // Unary expressions with constant folding  
            Expr::Unary(unary) => self.attrib_unary(unary),
            
            // Conditional expressions (ternary operator)
            Expr::Conditional(cond) => self.attrib_conditional(cond),
            
            // Type cast expressions
            Expr::Cast(cast) => self.attrib_cast(cast),
            
            // Literal expressions (already constants)
            Expr::Literal(_lit) => Ok(expr.clone()),
            
            // Identifier expressions
            Expr::Identifier(id) => self.attrib_identifier(id),
            
            // Method call expressions
            Expr::MethodCall(call) => self.attrib_method_call(call),
            
            // Field access expressions
            Expr::FieldAccess(field) => self.attrib_field_access(field),
            
            // Array access expressions
            Expr::ArrayAccess(array) => self.attrib_array_access(array),
            
            _ => Ok(expr.clone()),
        }
    }
    
    /// Attribute binary expression with constant folding - JavaC visitBinary equivalent
    fn attrib_binary(&mut self, bin: &BinaryExpr) -> Result<Expr> {
        // First, attribute operands
        let left = self.attrib_expr(&bin.left)?;
        let right = self.attrib_expr(&bin.right)?;
        
        // Check for constant folding opportunity (JavaC pattern)
        if let (Expr::Literal(left_lit), Expr::Literal(right_lit)) = (&left, &right) {
            // JavaC: Type ctype = cfolder.fold2(opc, left, right);
            if let Some(folded_value) = self.cfolder.fold_binary_expr(&bin.operator, &left_lit.value, &right_lit.value) {
                // JavaC: if (ctype != null) owntype = cfolder.coerce(ctype, owntype);
                return Ok(Expr::Literal(LiteralExpr {
                    value: folded_value,
                    span: bin.span.clone(),
                }));
            }
        }
        
        // No constant folding possible - return optimized operands
        Ok(Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: bin.operator.clone(),
            right: Box::new(right),
            span: bin.span.clone(),
        }))
    }
    
    /// Attribute unary expression with constant folding - JavaC visitUnary equivalent
    fn attrib_unary(&mut self, unary: &UnaryExpr) -> Result<Expr> {
        // First, attribute operand
        let operand = self.attrib_expr(&unary.operand)?;
        
        // Check for constant folding opportunity (JavaC pattern)
        if let Expr::Literal(operand_lit) = &operand {
            // JavaC: Type ctype = cfolder.fold1(opc, argtype);
            if let Some(folded_value) = self.cfolder.fold_unary_expr(&unary.operator, &operand_lit.value) {
                // JavaC: if (ctype != null) owntype = cfolder.coerce(ctype, owntype);
                return Ok(Expr::Literal(LiteralExpr {
                    value: folded_value,
                    span: unary.span.clone(),
                }));
            }
        }
        
        // No constant folding possible
        Ok(Expr::Unary(UnaryExpr {
            operator: unary.operator.clone(),
            operand: Box::new(operand),
            span: unary.span.clone(),
        }))
    }
    
    /// Attribute conditional expression - JavaC visitConditional equivalent
    fn attrib_conditional(&mut self, cond: &ConditionalExpr) -> Result<Expr> {
        // Attribute condition
        let condition = self.attrib_expr(&cond.condition)?;
        
        // Check for constant condition (JavaC optimization)
        if let Expr::Literal(LiteralExpr { value: Literal::Boolean(cond_value), .. }) = &condition {
            // JavaC: owntype = cfolder.coerce(condtype.isTrue() ? truetype : falsetype, owntype);
            if *cond_value {
                // Condition is true - return true branch
                return self.attrib_expr(&cond.then_expr);
            } else {
                // Condition is false - return false branch
                return self.attrib_expr(&cond.else_expr);
            }
        }
        
        // Attribute both branches
        let true_expr = self.attrib_expr(&cond.then_expr)?;
        let false_expr = self.attrib_expr(&cond.else_expr)?;
        
        Ok(Expr::Conditional(ConditionalExpr {
            condition: Box::new(condition),
            then_expr: Box::new(true_expr),
            else_expr: Box::new(false_expr),
            span: cond.span.clone(),
        }))
    }
    
    /// Attribute cast expression - JavaC visitTypeCast equivalent
    fn attrib_cast(&mut self, cast: &CastExpr) -> Result<Expr> {
        let expr = self.attrib_expr(&cast.expr)?;
        
        // Check for constant cast optimization
        if let Expr::Literal(lit) = &expr {
            let target_type = TypeEnum::from(cast.target_type.clone());
            
            // JavaC: owntype = cfolder.coerce(exprtype, owntype);
            if let Some(coerced_value) = self.cfolder.coerce(&lit.value, &target_type) {
                return Ok(Expr::Literal(LiteralExpr {
                    value: coerced_value,
                    span: cast.span.clone(),
                }));
            }
        }
        
        Ok(Expr::Cast(CastExpr {
            target_type: cast.target_type.clone(),
            expr: Box::new(expr),
            span: cast.span.clone(),
        }))
    }
    
    /// Attribute identifier expression - JavaC visitIdent equivalent
    fn attrib_identifier(&mut self, id: &IdentifierExpr) -> Result<Expr> {
        // JavaC: Check if identifier refers to a constant field
        // For now, no optimization
        Ok(Expr::Identifier(id.clone()))
    }
    
    /// Attribute method call expression - JavaC visitApply equivalent
    fn attrib_method_call(&mut self, call: &MethodCallExpr) -> Result<Expr> {
        // Attribute arguments
        let mut optimized_args = Vec::new();
        for arg in &call.arguments {
            optimized_args.push(self.attrib_expr(arg)?);
        }
        
        // JavaC: Check for builtin method optimizations
        let optimized_call = self.optimize_builtin_method_call(call, &optimized_args)?;
        
        Ok(optimized_call)
    }
    
    /// Optimize builtin method calls - JavaC pattern
    fn optimize_builtin_method_call(&mut self, call: &MethodCallExpr, args: &[Expr]) -> Result<Expr> {
        // JavaC builtin method optimizations
        match call.name.as_str() {
            "Math.max" | "Math.min" if args.len() == 2 => {
                // Optimize Math.max/min with constants
                if let (Expr::Literal(left_lit), Expr::Literal(right_lit)) = (&args[0], &args[1]) {
                    if let (Literal::Integer(l), Literal::Integer(r)) = (&left_lit.value, &right_lit.value) {
                        let result = if call.name == "Math.max" {
                            std::cmp::max(*l, *r)
                        } else {
                            std::cmp::min(*l, *r)
                        };
                        
                        return Ok(Expr::Literal(LiteralExpr {
                            value: Literal::Integer(result),
                            span: call.span.clone(),
                        }));
                    }
                }
            }
            
            "String.valueOf" if args.len() == 1 => {
                // Optimize String.valueOf for constants
                if let Expr::Literal(lit) = &args[0] {
                    let string_value = match &lit.value {
                        Literal::Integer(i) => i.to_string(),
                        Literal::Long(l) => l.to_string(),
                        Literal::Float(f) => f.to_string(),
                        Literal::Double(d) => d.to_string(),
                        Literal::Boolean(b) => b.to_string(),
                        Literal::Char(c) => c.to_string(),
                        Literal::String(s) => s.clone(),
                        Literal::Null => "null".to_string(),
                    };
                    
                    return Ok(Expr::Literal(LiteralExpr {
                        value: Literal::String(string_value),
                        span: call.span.clone(),
                    }));
                }
            }
            
            "Integer.valueOf" | "Long.valueOf" | "Float.valueOf" | "Double.valueOf" if args.len() == 1 => {
                // Optimize wrapper valueOf for constants
                if let Expr::Literal(_lit) = &args[0] {
                    // For compile-time constants, keep as literals (autoboxing will handle at runtime)
                    return Ok(args[0].clone());
                }
            }
            
            _ => {}
        }
        
        // No optimization applied - return original call with optimized arguments
        Ok(Expr::MethodCall(MethodCallExpr {
            target: call.target.as_ref().map(|t| Box::new(self.attrib_expr(t).unwrap_or_else(|_| (**t).clone()))),
            name: call.name.clone(),
            arguments: args.to_vec(),
            span: call.span.clone(),
        }))
    }
    
    /// Attribute field access expression - JavaC visitSelect equivalent
    fn attrib_field_access(&mut self, field: &FieldAccessExpr) -> Result<Expr> {
        let target = if let Some(ref t) = field.target {
            Some(Box::new(self.attrib_expr(t)?))
        } else {
            None
        };
        
        // JavaC: Check for constant field access optimization
        // For now, no optimization
        Ok(Expr::FieldAccess(FieldAccessExpr {
            target,
            name: field.name.clone(),
            span: field.span.clone(),
        }))
    }
    
    /// Attribute array access expression - JavaC visitIndexed equivalent
    fn attrib_array_access(&mut self, array: &ArrayAccessExpr) -> Result<Expr> {
        let array_expr = self.attrib_expr(&array.array)?;
        let index_expr = self.attrib_expr(&array.index)?;
        
        Ok(Expr::ArrayAccess(ArrayAccessExpr {
            array: Box::new(array_expr),
            index: Box::new(index_expr),
            span: array.span.clone(),
        }))
    }
    
    /// Attribute statement - JavaC attribStat equivalent
    pub fn attrib_stat(&mut self, stmt: &Stmt) -> Result<Stmt> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                let optimized_expr = self.attrib_expr(&expr_stmt.expr)?;
                Ok(Stmt::Expression(ExprStmt {
                    expr: optimized_expr,
                    span: expr_stmt.span.clone(),
                }))
            }
            
            Stmt::If(if_stmt) => {
                let condition = self.attrib_expr(&if_stmt.condition)?;
                
                // JavaC: Constant condition optimization
                if let Expr::Literal(LiteralExpr { value: Literal::Boolean(cond_value), .. }) = &condition {
                    if *cond_value {
                        // Condition is true - return only then branch
                        return self.attrib_stat(&if_stmt.then_branch);
                    } else if let Some(ref else_branch) = if_stmt.else_branch {
                        // Condition is false - return only else branch
                        return self.attrib_stat(else_branch);
                    } else {
                        // Condition is false, no else - return empty statement
                        return Ok(Stmt::Block(Block {
                            statements: vec![],
                            span: if_stmt.span.clone(),
                        }));
                    }
                }
                
                let then_branch = self.attrib_stat(&if_stmt.then_branch)?;
                let else_branch = if let Some(ref else_stmt) = if_stmt.else_branch {
                    Some(Box::new(self.attrib_stat(else_stmt)?))
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
            
            Stmt::Block(block) => {
                let mut optimized_statements = Vec::new();
                for stmt in &block.statements {
                    let optimized_stmt = self.attrib_stat(stmt)?;
                    
                    // Filter out empty blocks (JavaC dead code elimination)
                    match optimized_stmt {
                        Stmt::Block(ref inner_block) if inner_block.statements.is_empty() => {
                            // Skip empty blocks
                        }
                        _ => optimized_statements.push(optimized_stmt),
                    }
                }
                
                Ok(Stmt::Block(Block {
                    statements: optimized_statements,
                    span: block.span.clone(),
                }))
            }
            
            // Other statements - recurse into sub-expressions
            _ => Ok(stmt.clone()), // TODO: Handle other statement types
        }
    }
    
    /// Check if expression is a compile-time constant - JavaC pattern
    pub fn is_constant_expression(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(lit) => self.cfolder.is_constant(&lit.value),
            
            Expr::Binary(bin) => {
                self.is_constant_expression(&bin.left) && self.is_constant_expression(&bin.right)
            }
            
            Expr::Unary(unary) => {
                self.is_constant_expression(&unary.operand)
            }
            
            Expr::Cast(cast) => {
                self.is_constant_expression(&cast.expr)
            }
            
            Expr::Conditional(cond) => {
                self.is_constant_expression(&cond.condition) &&
                self.is_constant_expression(&cond.then_expr) &&
                self.is_constant_expression(&cond.else_expr)
            }
            
            _ => false,
        }
    }
    
    /// Get constant value of expression if it's constant - JavaC pattern
    pub fn get_constant_value(&mut self, expr: &Expr) -> Option<Literal> {
        if !self.is_constant_expression(expr) {
            return None;
        }
        
        match self.attrib_expr(expr) {
            Ok(Expr::Literal(lit)) => Some(lit.value),
            _ => None,
        }
    }
}