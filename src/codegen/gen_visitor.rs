//! Simplified JavaC-style visitor methods for Gen - avoiding borrowing conflicts
//!
//! This module provides simplified implementations of all visitor methods
//! to resolve borrowing issues. Full implementations will be added later.

use crate::ast::*;
use crate::error::Result;
use super::gen::{Gen, GenContext};
use super::items_javac::{Item};
use super::opcodes;

impl Gen {
    /// Visit literal expression - JavaC Gen.visitLiteral equivalent
    pub fn visit_literal(&mut self, tree: &LiteralExpr, _env: &GenContext) -> Result<Item> {
        if let Some(code) = self.code_mut() {
            match &tree.value {
                Literal::Null => {
                    code.emitop(super::opcodes::ACONST_NULL);
                    code.state.push(super::code::Type::Null); // Push null reference on stack
                }
                Literal::Integer(val) => {
                    // Generate appropriate constant instruction
                    match *val {
                        -1 => code.emitop(super::opcodes::ICONST_M1),
                        0 => code.emitop(super::opcodes::ICONST_0),
                        1 => code.emitop(super::opcodes::ICONST_1),
                        2 => code.emitop(super::opcodes::ICONST_2),
                        3 => code.emitop(super::opcodes::ICONST_3),
                        4 => code.emitop(super::opcodes::ICONST_4),
                        5 => code.emitop(super::opcodes::ICONST_5),
                        -128..=127 => {
                            code.emitop(super::opcodes::BIPUSH);
                            code.emit1(*val as u8);
                        }
                        -32768..=32767 => {
                            code.emitop(super::opcodes::SIPUSH);
                            code.emit2(*val as u16);
                        }
                        _ => {
                            // Use LDC for larger constants
                            // TODO: Add to constant pool
                            code.emitop(super::opcodes::LDC);
                            code.emit1(0); // Placeholder constant pool index
                        }
                    }
                    code.state.push(super::code::Type::Int); // Push int on stack
                }
                Literal::Boolean(val) => {
                    if *val {
                        code.emitop(super::opcodes::ICONST_1);
                    } else {
                        code.emitop(super::opcodes::ICONST_0);
                    }
                    code.state.push(super::code::Type::Int); // Push int on stack
                }
                _ => {
                    // TODO: Implement other literal types
                }
            }
        }
        
        // Return appropriate item type based on literal type
        use super::items_javac::{Item, typecodes};
        Ok(Item::Immediate { 
            typecode: match &tree.value {
                Literal::Integer(_) => typecodes::INT,
                Literal::Boolean(_) => typecodes::INT,
                Literal::Null => typecodes::OBJECT,
                _ => typecodes::OBJECT,
            },
            value: tree.value.clone(),
        })
    }
    
    /// Visit identifier expression - simplified version
    pub fn visit_ident(&mut self, tree: &IdentifierExpr, _env: &GenContext) -> Result<Item> {
        if tree.name == "this" {
            self.with_items(|items| {
                Ok(items.make_this_item())
            })
        } else if tree.name == "super" {
            self.with_items(|items| {
                Ok(items.make_super_item())
            })
        } else {
            // Look up local variable or field
            let local_var = self.type_inference.lookup_local(&tree.name).cloned();
            
            if let Some(local_var) = local_var {
                self.with_items(|items| {
                    Ok(items.make_local_item(&local_var.typ, local_var.reg))
                })
            } else {
                // Try to resolve through symbol table
                let symbol = self.type_inference.types().symtab().lookup_symbol(&tree.name).cloned();
                
                self.with_items(|items| {
                    if let Some(symbol) = symbol {
                        let is_static = symbol.kind == super::symtab::SymbolKind::Field && 
                                       symbol.modifiers.contains(&"static".to_string());
                        Ok(items.make_member_item(tree.name.clone(), is_static, &symbol.typ))
                    } else {
                        eprintln!("⚠️  WARNING: Cannot resolve symbol '{}', defaulting to int", tree.name);
                        let typ = TypeEnum::Primitive(PrimitiveType::Int);
                        Ok(items.make_member_item(tree.name.clone(), false, &typ))
                    }
                })
            }
        }
    }
    
    /// Visit field access expression - simplified version
    pub fn visit_select(&mut self, tree: &FieldAccessExpr, env: &GenContext) -> Result<Item> {
        // Generate target expression if present
        if let Some(ref target) = tree.target {
            let _target_item = self.visit_expr(target, env)?;
        }
        
        // Create the member access item
        self.with_items(|items| {
            let typ = TypeEnum::Primitive(PrimitiveType::Int); // TODO: Lookup field type
            let is_static = false; // TODO: Determine if static
            Ok(items.make_member_item(tree.name.clone(), is_static, &typ))
        })
    }
    
    /// Visit method call expression - simplified version
    pub fn visit_apply(&mut self, tree: &MethodCallExpr, env: &GenContext) -> Result<Item> {
        // Generate target expression if present
        if let Some(ref target) = tree.target {
            let _target_item = self.visit_expr(target, env)?;
        }
        
        // Generate arguments
        for arg in &tree.arguments {
            let _arg_item = self.visit_expr(arg, env)?;
        }
        
        // Create method result item
        self.with_items(|items| {
            let return_type = TypeEnum::Void; // TODO: Get actual return type
            Ok(items.make_stack_item_for_type(&return_type))
        })
    }
    
    /// Visit binary expression - simplified version
    pub fn visit_binary(&mut self, tree: &BinaryExpr, env: &GenContext) -> Result<Item> {
        // Generate operands
        let _left_item = self.visit_expr(&tree.left, env)?;
        let _right_item = self.visit_expr(&tree.right, env)?;
        
        // Generate operation
        self.with_items(|items| {
            match tree.operator {
                BinaryOp::Add => {
                    items.code.emitop(opcodes::IADD); // Default to int add
                }
                BinaryOp::Sub => {
                    items.code.emitop(opcodes::ISUB);
                }
                BinaryOp::Mul => {
                    items.code.emitop(opcodes::IMUL);
                }
                BinaryOp::Div => {
                    items.code.emitop(opcodes::IDIV);
                }
                _ => {
                    // Default operation
                    items.code.emitop(opcodes::NOP);
                }
            }
            
            let result_type = TypeEnum::Primitive(PrimitiveType::Int); // TODO: Type inference
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Visit unary expression - simplified version
    pub fn visit_unary(&mut self, tree: &UnaryExpr, env: &GenContext) -> Result<Item> {
        // Generate operand
        let _operand_item = self.visit_expr(&tree.operand, env)?;
        
        // Generate operation
        self.with_items(|items| {
            match tree.operator {
                UnaryOp::Minus => {
                    items.code.emitop(opcodes::INEG);
                }
                UnaryOp::Not => {
                    // Boolean negation
                }
                _ => {
                    items.code.emitop(opcodes::NOP);
                }
            }
            
            let result_type = TypeEnum::Primitive(PrimitiveType::Int);
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Visit assignment expression - simplified version
    pub fn visit_assign(&mut self, tree: &AssignmentExpr, env: &GenContext) -> Result<Item> {
        // Generate target and value
        let _target = self.visit_expr(&tree.target, env)?;
        let _value = self.visit_expr(&tree.value, env)?;
        
        // Generate store instruction
        self.with_items(|items| {
            // TODO: Generate store instruction based on target
            let result_type = TypeEnum::Primitive(PrimitiveType::Int);
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Visit type cast expression - simplified version
    pub fn visit_type_cast(&mut self, tree: &CastExpr, env: &GenContext) -> Result<Item> {
        // Generate expression to cast
        let _expr_item = self.visit_expr(&tree.expr, env)?;
        
        // Generate cast instruction
        self.with_items(|items| {
            let target_type = TypeEnum::from(tree.target_type.clone());
            Ok(items.make_stack_item_for_type(&target_type))
        })
    }
    
    /// Visit array access expression - simplified version
    pub fn visit_indexed(&mut self, tree: &ArrayAccessExpr, env: &GenContext) -> Result<Item> {
        // Generate array and index
        let _array_item = self.visit_expr(&tree.array, env)?;
        let _index_item = self.visit_expr(&tree.index, env)?;
        
        // Create indexed item
        self.with_items(|items| {
            let element_type = TypeEnum::Primitive(PrimitiveType::Int);
            Ok(items.make_indexed_item(&element_type))
        })
    }
    
    /// Helper: Convert Literal to TypeEnum
    fn literal_to_type_enum(literal: &Literal) -> TypeEnum {
        match literal {
            Literal::Integer(_) => TypeEnum::Primitive(PrimitiveType::Int),
            Literal::Long(_) => TypeEnum::Primitive(PrimitiveType::Long),
            Literal::Float(_) => TypeEnum::Primitive(PrimitiveType::Float),
            Literal::Double(_) => TypeEnum::Primitive(PrimitiveType::Double),
            Literal::Boolean(_) => TypeEnum::Primitive(PrimitiveType::Boolean),
            Literal::Char(_) => TypeEnum::Primitive(PrimitiveType::Char),
            Literal::String(_) => TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string())),
            Literal::Null => TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string())),
        }
    }
}

/// Statement visitor methods - simplified versions
impl Gen {
    /// Visit if statement - simplified version
    pub fn visit_if(&mut self, tree: &IfStmt, env: &GenContext) -> Result<()> {
        // Generate condition
        let _cond_result = self.visit_expr(&tree.condition, env)?;
        
        // Generate then branch
        self.visit_stmt(&tree.then_branch, env)?;
        
        // Generate else branch if present
        if let Some(ref else_branch) = tree.else_branch {
            self.visit_stmt(else_branch, env)?;
        }
        
        Ok(())
    }
    
    /// Visit while loop - simplified version
    pub fn visit_while(&mut self, tree: &WhileStmt, env: &GenContext) -> Result<()> {
        // Generate condition
        let _cond_result = self.visit_expr(&tree.condition, env)?;
        
        // Generate body
        self.visit_stmt(&tree.body, env)?;
        
        Ok(())
    }
    
    /// Visit for loop - simplified version
    pub fn visit_for(&mut self, tree: &ForStmt, env: &GenContext) -> Result<()> {
        // Generate initializers
        for init_stmt in &tree.init {
            self.visit_stmt(init_stmt, env)?;
        }
        
        // Generate condition
        if let Some(ref condition) = tree.condition {
            let _cond_result = self.visit_expr(condition, env)?;
        }
        
        // Generate body
        self.visit_stmt(&tree.body, env)?;
        
        Ok(())
    }
    
    /// Visit return statement - JavaC Gen.visitReturn equivalent
    pub fn visit_return(&mut self, tree: &ReturnStmt, env: &GenContext) -> Result<()> {
        if let Some(ref expr) = tree.value {
            // Generate expression for return value
            let _result = self.visit_expr(expr, env)?;
            
            // Determine return instruction based on expression type
            // TODO: Get actual expression type for proper return instruction
            // For now, assume int return
            if let Some(code) = self.code_mut() {
                code.emitop(super::opcodes::IRETURN);
                code.alive = false;
            }
        } else {
            // Void return
            if let Some(code) = self.code_mut() {
                code.emitop(super::opcodes::RETURN);
                code.alive = false;
            }
        }
        Ok(())
    }
    
    /// Visit variable declaration - simplified version
    pub fn visit_var_def(&mut self, tree: &VarDeclStmt, env: &GenContext) -> Result<()> {
        for var in &tree.variables {
            if let Some(ref init) = var.initializer {
                let _init_result = self.visit_expr(init, env)?;
            }
        }
        Ok(())
    }
    
    /// Visit expression statement - simplified version
    pub fn visit_exec(&mut self, tree: &ExprStmt, env: &GenContext) -> Result<()> {
        let _result = self.visit_expr(&tree.expr, env)?;
        Ok(())
    }
    
    /// Visit block - simplified version
    pub fn visit_block(&mut self, tree: &Block, env: &GenContext) -> Result<()> {
        for stmt in &tree.statements {
            self.visit_stmt(stmt, env)?;
        }
        Ok(())
    }
    
    /// Visit try statement - simplified version
    pub fn visit_try(&mut self, tree: &TryStmt, env: &GenContext) -> Result<()> {
        self.visit_block(&tree.try_block, env)?;
        
        for catch_clause in &tree.catch_clauses {
            self.visit_block(&catch_clause.block, env)?;
        }
        
        if let Some(ref finally_block) = tree.finally_block {
            self.visit_block(finally_block, env)?;
        }
        
        Ok(())
    }
}