//! Simplified JavaC-style visitor methods for Gen - avoiding borrowing conflicts
//!
//! This module provides simplified implementations of all visitor methods
//! to resolve borrowing issues. Full implementations will be added later.

use crate::ast::*;
use crate::error::Result;
use super::gen::{Gen, GenContext};
use super::items_javac::{Item, CondItem, Items};
use super::chain::Chain;
use super::opcodes;

impl Gen {
    /// Visit literal expression - JavaC Gen.visitLiteral equivalent
    pub fn visit_literal(&mut self, tree: &LiteralExpr, _env: &GenContext) -> Result<Item> {
        // Handle constants that need constant pool first (to avoid borrowing conflicts)
        let pool_data = match &tree.value {
            Literal::String(s) => {
                Some((self.get_pool_mut().add_string(s), "string"))
            }
            Literal::Char(c) => {
                let char_val = *c as u32 as i32;
                if char_val > 32767 || char_val < -32768 {
                    Some((self.get_pool_mut().add_integer(char_val), "char"))
                } else {
                    None
                }
            }
            Literal::Long(val) if *val != 0 && *val != 1 => {
                Some((self.get_pool_mut().add_long(*val), "long"))
            }
            Literal::Float(val) => {
                let epsilon_check = (*val - 0.0).abs() > f64::EPSILON && 
                                   (*val - 1.0).abs() > f64::EPSILON && 
                                   (*val - 2.0).abs() > f64::EPSILON;
                if epsilon_check {
                    Some((self.get_pool_mut().add_float(*val as f32), "float"))
                } else {
                    None
                }
            }
            Literal::Double(val) => {
                let epsilon_check = (*val - 0.0).abs() > f64::EPSILON && 
                                   (*val - 1.0).abs() > f64::EPSILON;
                if epsilon_check {
                    Some((self.get_pool_mut().add_double(*val), "double"))
                } else {
                    None
                }
            }
            _ => None,
        };
        
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
                Literal::String(_) => {
                    if let Some((string_idx, _)) = pool_data {
                        code.emitop(super::opcodes::LDC);
                        code.emit1(string_idx as u8);
                        code.state.push(super::code::Type::Object("java/lang/String".to_string()));
                    }
                }
                Literal::Char(c) => {
                    let char_val = *c as u32 as i32;
                    if char_val >= 0 && char_val <= 5 {
                        match char_val {
                            0 => code.emitop(super::opcodes::ICONST_0),
                            1 => code.emitop(super::opcodes::ICONST_1),
                            2 => code.emitop(super::opcodes::ICONST_2),
                            3 => code.emitop(super::opcodes::ICONST_3),
                            4 => code.emitop(super::opcodes::ICONST_4),
                            5 => code.emitop(super::opcodes::ICONST_5),
                            _ => unreachable!(),
                        }
                    } else if char_val >= -128 && char_val <= 127 {
                        code.emitop(super::opcodes::BIPUSH);
                        code.emit1(char_val as u8);
                    } else if char_val >= -32768 && char_val <= 32767 {
                        code.emitop(super::opcodes::SIPUSH);
                        code.emit2(char_val as u16);
                    } else if let Some((int_idx, _)) = pool_data {
                        code.emitop(super::opcodes::LDC);
                        code.emit1(int_idx as u8);
                    }
                    code.state.push(super::code::Type::Int);
                }
                Literal::Long(val) => {
                    match *val {
                        0 => code.emitop(super::opcodes::LCONST_0),
                        1 => code.emitop(super::opcodes::LCONST_1),
                        _ => {
                            if let Some((long_idx, _)) = pool_data {
                                code.emitop(super::opcodes::LDC2_W);
                                code.emit2(long_idx);
                            }
                        }
                    }
                    code.state.push(super::code::Type::Long);
                }
                Literal::Float(val) => {
                    match val {
                        v if (*v - 0.0).abs() < f64::EPSILON => code.emitop(super::opcodes::FCONST_0),
                        v if (*v - 1.0).abs() < f64::EPSILON => code.emitop(super::opcodes::FCONST_1),
                        v if (*v - 2.0).abs() < f64::EPSILON => code.emitop(super::opcodes::FCONST_2),
                        _ => {
                            if let Some((float_idx, _)) = pool_data {
                                code.emitop(super::opcodes::LDC);
                                code.emit1(float_idx as u8);
                            }
                        }
                    }
                    code.state.push(super::code::Type::Float);
                }
                Literal::Double(val) => {
                    match val {
                        v if (*v - 0.0).abs() < f64::EPSILON => code.emitop(super::opcodes::DCONST_0),
                        v if (*v - 1.0).abs() < f64::EPSILON => code.emitop(super::opcodes::DCONST_1),
                        _ => {
                            if let Some((double_idx, _)) = pool_data {
                                code.emitop(super::opcodes::LDC2_W);
                                code.emit2(double_idx);
                            }
                        }
                    }
                    code.state.push(super::code::Type::Double);
                }
                _ => {
                    // Fallback for any unhandled literal types
                    eprintln!("⚠️  WARNING: Unhandled literal type: {:?}", tree.value);
                }
            }
        }
        
        // Return appropriate item type based on literal type
        use super::items_javac::{Item, typecodes};
        Ok(Item::Immediate { 
            typecode: match &tree.value {
                Literal::Integer(_) => typecodes::INT,
                Literal::Boolean(_) => typecodes::INT,
                Literal::Char(_) => typecodes::INT,
                Literal::Long(_) => typecodes::LONG,
                Literal::Float(_) => typecodes::FLOAT,
                Literal::Double(_) => typecodes::DOUBLE,
                Literal::String(_) => typecodes::OBJECT,
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
                    let local_item = items.make_local_item(&local_var.typ, local_var.reg);
                    items.load_item(&local_item)
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
    
    /// Visit binary expression - JavaC-aligned implementation
    pub fn visit_binary(&mut self, tree: &BinaryExpr, env: &GenContext) -> Result<Item> {
        use crate::ast::BinaryOp;
        
        // Generate operands
        let left_item = self.visit_expr(&tree.left, env)?;
        let right_item = self.visit_expr(&tree.right, env)?;
        
        // Determine result type based on operands and operator
        let result_type = self.infer_binary_result_type(&left_item, &right_item, &tree.operator);
        
        // Generate operation bytecode
        self.with_items(|items| {
            match tree.operator {
                // Arithmetic operators
                BinaryOp::Add => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Int) => items.code.emitop(opcodes::IADD),
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LADD),
                        TypeEnum::Primitive(PrimitiveType::Float) => items.code.emitop(opcodes::FADD),
                        TypeEnum::Primitive(PrimitiveType::Double) => items.code.emitop(opcodes::DADD),
                        _ => items.code.emitop(opcodes::IADD), // Default to int
                    }
                }
                BinaryOp::Sub => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Int) => items.code.emitop(opcodes::ISUB),
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LSUB),
                        TypeEnum::Primitive(PrimitiveType::Float) => items.code.emitop(opcodes::FSUB),
                        TypeEnum::Primitive(PrimitiveType::Double) => items.code.emitop(opcodes::DSUB),
                        _ => items.code.emitop(opcodes::ISUB),
                    }
                }
                BinaryOp::Mul => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Int) => items.code.emitop(opcodes::IMUL),
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LMUL),
                        TypeEnum::Primitive(PrimitiveType::Float) => items.code.emitop(opcodes::FMUL),
                        TypeEnum::Primitive(PrimitiveType::Double) => items.code.emitop(opcodes::DMUL),
                        _ => items.code.emitop(opcodes::IMUL),
                    }
                }
                BinaryOp::Div => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Int) => items.code.emitop(opcodes::IDIV),
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LDIV),
                        TypeEnum::Primitive(PrimitiveType::Float) => items.code.emitop(opcodes::FDIV),
                        TypeEnum::Primitive(PrimitiveType::Double) => items.code.emitop(opcodes::DDIV),
                        _ => items.code.emitop(opcodes::IDIV),
                    }
                }
                BinaryOp::Mod => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Int) => items.code.emitop(opcodes::IREM),
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LREM),
                        TypeEnum::Primitive(PrimitiveType::Float) => items.code.emitop(opcodes::FREM),
                        TypeEnum::Primitive(PrimitiveType::Double) => items.code.emitop(opcodes::DREM),
                        _ => items.code.emitop(opcodes::IREM),
                    }
                }
                
                // Bitwise operators (integer types only)
                BinaryOp::And => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LAND),
                        _ => items.code.emitop(opcodes::IAND),
                    }
                }
                BinaryOp::Or => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LOR),
                        _ => items.code.emitop(opcodes::IOR),
                    }
                }
                BinaryOp::Xor => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LXOR),
                        _ => items.code.emitop(opcodes::IXOR),
                    }
                }
                BinaryOp::LShift => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LSHL),
                        _ => items.code.emitop(opcodes::ISHL),
                    }
                }
                BinaryOp::RShift => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LSHR),
                        _ => items.code.emitop(opcodes::ISHR),
                    }
                }
                BinaryOp::URShift => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LUSHR),
                        _ => items.code.emitop(opcodes::IUSHR),
                    }
                }
                
                // Comparison operators
                BinaryOp::Eq | BinaryOp::Ne | 
                BinaryOp::Lt | BinaryOp::Le |
                BinaryOp::Gt | BinaryOp::Ge => {
                    // Generate comparison instructions
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Float) => {
                            items.code.emitop(opcodes::FCMPL);
                        }
                        TypeEnum::Primitive(PrimitiveType::Double) => {
                            items.code.emitop(opcodes::DCMPL);
                        }
                        TypeEnum::Primitive(PrimitiveType::Long) => {
                            items.code.emitop(opcodes::LCMP);
                        }
                        _ => {
                            // For integers, use subtract for basic comparison
                            items.code.emitop(opcodes::ISUB);
                        }
                    }
                    return Ok(items.make_stack_item_for_type(&TypeEnum::Primitive(PrimitiveType::Boolean)));
                }
                
                _ => {
                    eprintln!("⚠️  WARNING: Unsupported binary operator: {:?}", tree.operator);
                    items.code.emitop(opcodes::NOP);
                }
            }
            
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Infer binary operation result type based on operands and operator
    fn infer_binary_result_type(&self, left_item: &Item, right_item: &Item, operator: &BinaryOp) -> TypeEnum {
        use crate::ast::BinaryOp;
        use super::items_javac::typecodes;
        
        // Get type codes from items
        let left_type = left_item.typecode();
        let right_type = right_item.typecode();
        
        // Comparison operators always return boolean
        match operator {
            BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | 
            BinaryOp::Gt | BinaryOp::Ge | BinaryOp::And | BinaryOp::Or => {
                return TypeEnum::Primitive(PrimitiveType::Boolean);
            }
            _ => {}
        }
        
        // Type promotion rules (follow Java's rules)
        match (left_type, right_type) {
            // Double takes precedence
            (typecodes::DOUBLE, _) | (_, typecodes::DOUBLE) => TypeEnum::Primitive(PrimitiveType::Double),
            // Float takes precedence over integral types
            (typecodes::FLOAT, _) | (_, typecodes::FLOAT) => TypeEnum::Primitive(PrimitiveType::Float),
            // Long takes precedence over int
            (typecodes::LONG, _) | (_, typecodes::LONG) => TypeEnum::Primitive(PrimitiveType::Long),
            // Default to int for all integral operations
            _ => TypeEnum::Primitive(PrimitiveType::Int),
        }
    }
    
    /// Visit unary expression - JavaC-aligned implementation
    pub fn visit_unary(&mut self, tree: &UnaryExpr, env: &GenContext) -> Result<Item> {
        use crate::ast::UnaryOp;
        
        // Generate operand
        let operand_item = self.visit_expr(&tree.operand, env)?;
        
        // Determine result type based on operand and operator
        let result_type = self.infer_unary_result_type(&operand_item, &tree.operator);
        
        // Generate operation
        self.with_items(|items| {
            match tree.operator {
                UnaryOp::Minus => {
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Int) => items.code.emitop(opcodes::INEG),
                        TypeEnum::Primitive(PrimitiveType::Long) => items.code.emitop(opcodes::LNEG),
                        TypeEnum::Primitive(PrimitiveType::Float) => items.code.emitop(opcodes::FNEG),
                        TypeEnum::Primitive(PrimitiveType::Double) => items.code.emitop(opcodes::DNEG),
                        _ => items.code.emitop(opcodes::INEG), // Default to int
                    }
                }
                UnaryOp::Plus => {
                    // No operation needed for unary plus - just pass through
                }
                UnaryOp::Not => {
                    // Logical not - converts 0 to 1, non-zero to 0
                    items.code.emitop(opcodes::ICONST_0);
                    items.code.emitop(opcodes::IF_ICMPEQ);
                    items.code.emit2(7); // Jump to "push 1" if equal to 0
                    items.code.emitop(opcodes::ICONST_0); // Push 0 (false)
                    items.code.emitop(opcodes::GOTO);
                    items.code.emit2(4); // Jump to end
                    items.code.emitop(opcodes::ICONST_1); // Push 1 (true)
                }
                UnaryOp::BitNot => {
                    // Bitwise not - XOR with all 1s
                    match result_type {
                        TypeEnum::Primitive(PrimitiveType::Long) => {
                            items.code.emitop(opcodes::LCONST_1); 
                            items.code.emitop(opcodes::LNEG); // Load 1 and negate to get -1
                            items.code.emitop(opcodes::LXOR);
                        }
                        _ => {
                            items.code.emitop(opcodes::ICONST_M1); // -1 (all bits set)
                            items.code.emitop(opcodes::IXOR);
                        }
                    }
                }
                _ => {
                    eprintln!("⚠️  WARNING: Unsupported unary operator: {:?}", tree.operator);
                    items.code.emitop(opcodes::NOP);
                }
            }
            
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Infer unary operation result type based on operand and operator
    fn infer_unary_result_type(&self, operand_item: &Item, operator: &UnaryOp) -> TypeEnum {
        use crate::ast::UnaryOp;
        use super::items_javac::typecodes;
        
        let operand_type = operand_item.typecode();
        
        match operator {
            UnaryOp::Not => TypeEnum::Primitive(PrimitiveType::Boolean),
            UnaryOp::Plus | UnaryOp::Minus | UnaryOp::BitNot => {
                // Preserve the operand type for arithmetic operations
                match operand_type {
                    typecodes::DOUBLE => TypeEnum::Primitive(PrimitiveType::Double),
                    typecodes::FLOAT => TypeEnum::Primitive(PrimitiveType::Float),
                    typecodes::LONG => TypeEnum::Primitive(PrimitiveType::Long),
                    _ => TypeEnum::Primitive(PrimitiveType::Int),
                }
            }
            _ => TypeEnum::Primitive(PrimitiveType::Int), // Default
        }
    }
    
    /// Visit assignment expression - simplified version
    pub fn visit_assign(&mut self, tree: &AssignmentExpr, env: &GenContext) -> Result<Item> {
        // Check if target is a field access (this.field)
        if let Expr::FieldAccess(field_access) = tree.target.as_ref() {
            // Handle field assignment: this.field = value
            
            // Load 'this' reference first
            if let Some(code) = self.code_mut() {
                code.emitop(opcodes::ALOAD_0); // Load 'this' (parameter 0)
                code.state.push(super::code::Type::Object("java/lang/Object".to_string())); // Push object reference
            }
            
            // Generate the value to assign
            let _value_item = self.visit_expr(&tree.value, env)?;
            
            // Get field name and add field reference to constant pool first
            let field_name = &field_access.name;
            let field_ref_idx = match &env.clazz {
                Some(class) => {
                    // Find field type
                    let field_decl = class.body.iter().find_map(|member| {
                        if let ClassMember::Field(f) = member {
                            if f.name == *field_name {
                                Some(f)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    });
                    
                    if let Some(field) = field_decl {
                        let descriptor = self.type_ref_to_descriptor(&field.type_ref)?;
                        self.get_pool_mut().add_field_ref(&class.name, field_name, &descriptor)
                    } else {
                        // Default to int field if not found
                        self.get_pool_mut().add_field_ref(&class.name, field_name, "I")
                    }
                }
                None => {
                    // Default if no class context
                    self.get_pool_mut().add_field_ref("Object", field_name, "I")
                }
            };
            
            // Generate putfield instruction
            if let Some(code) = self.code_mut() {
                code.emitop(opcodes::PUTFIELD);
                code.emit2(field_ref_idx.into()); // Field reference index
                
                // Pop object reference and value from stack
                code.state.pop(1); // value
                code.state.pop(1); // object reference
            }
            
            // Return the assigned value
            self.with_items(|items| {
                let result_type = TypeEnum::Primitive(PrimitiveType::Int); // TODO: Proper type
                Ok(items.make_stack_item_for_type(&result_type))
            })
        } else {
            // Handle other assignment types (local variables, arrays, etc.)
            let _target = self.visit_expr(&tree.target, env)?;
            let _value = self.visit_expr(&tree.value, env)?;
            
            // Generate store instruction
            self.with_items(|items| {
                // TODO: Generate store instruction based on target type
                let result_type = TypeEnum::Primitive(PrimitiveType::Int);
                Ok(items.make_stack_item_for_type(&result_type))
            })
        }
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
    /// Visit if statement - JavaC pattern alignment (Gen.java:1758-1777)
    pub fn visit_if(&mut self, tree: &IfStmt, env: &GenContext) -> Result<()> {
        use crate::codegen::chain::Chain;
        
        // Store nextreg limit (javac pattern)
        let limit = self.with_items(|items| Ok(items.code.max_locals))?;
        let mut then_exit: Option<Box<Chain>> = None;
        
        // Generate condition using javac's genCond pattern
        let cond = self.gen_cond(&tree.condition, env)?;
        
        // Get false jump chain from condition
        let else_chain = self.with_items(|items| {
            cond.jump_false(&mut items.code)
        })?;
        
        // Generate then branch if condition is not always false
        if !self.is_cond_false(&cond) {
            // Resolve true jumps to current position
            self.with_items(|items| {
                if let Some(true_chain) = &cond.true_jumps {
                    items.code.resolve(Some(true_chain.clone()));
                }
                Ok(())
            })?;
            
            // Generate then statement
            self.visit_stmt(&tree.then_branch, env)?;
            
            // Create goto jump if code is still alive
            then_exit = self.with_items(|items| {
                Ok(if items.code.is_alive() {
                    items.code.branch(opcodes::GOTO)
                } else {
                    None
                })
            })?;
        }
        
        // Handle else branch if else_chain exists
        if let Some(else_chain) = else_chain {
            // Resolve else jumps to current position
            self.with_items(|items| {
                items.code.resolve(Some(else_chain));
                Ok(())
            })?;
            
            // Generate else statement if present
            if let Some(ref else_branch) = tree.else_branch {
                self.visit_stmt(else_branch, env)?;
            }
        }
        
        // Resolve then exit jumps to current position
        self.with_items(|items| {
            items.code.resolve(then_exit);
            Ok(())
        })?;
        
        // End scopes (javac pattern)
        self.with_items(|items| {
            items.code.end_scopes(limit);
            Ok(())
        })?;
        
        Ok(())
    }
    
    /// Generate condition item from expression (JavaC: genCond)
    fn gen_cond(&mut self, expr: &Expr, env: &GenContext) -> Result<CondItem> {
        use crate::ast::Expr;
        use super::items_javac::CondItem;
        
        match expr {
            // Binary operations - direct conditional generation
            Expr::Binary(bin_expr) => {
                use crate::ast::BinaryOp;
                match &bin_expr.operator {
                    // Comparison operators - generate direct conditional
                    BinaryOp::Eq => {
                        self.visit_expr(&bin_expr.left, env)?;
                        self.visit_expr(&bin_expr.right, env)?;
                        Ok(CondItem::new(opcodes::IF_ICMPEQ))
                    }
                    BinaryOp::Ne => {
                        self.visit_expr(&bin_expr.left, env)?;
                        self.visit_expr(&bin_expr.right, env)?;
                        Ok(CondItem::new(opcodes::IF_ICMPNE))
                    }
                    BinaryOp::Lt => {
                        self.visit_expr(&bin_expr.left, env)?;
                        self.visit_expr(&bin_expr.right, env)?;
                        Ok(CondItem::new(opcodes::IF_ICMPLT))
                    }
                    BinaryOp::Le => {
                        self.visit_expr(&bin_expr.left, env)?;
                        self.visit_expr(&bin_expr.right, env)?;
                        Ok(CondItem::new(opcodes::IF_ICMPLE))
                    }
                    BinaryOp::Gt => {
                        self.visit_expr(&bin_expr.left, env)?;
                        self.visit_expr(&bin_expr.right, env)?;
                        Ok(CondItem::new(opcodes::IF_ICMPGT))
                    }
                    BinaryOp::Ge => {
                        self.visit_expr(&bin_expr.left, env)?;
                        self.visit_expr(&bin_expr.right, env)?;
                        Ok(CondItem::new(opcodes::IF_ICMPGE))
                    }
                    
                    // Logical operators - simplified for now
                    BinaryOp::And => {
                        // For now, generate as regular expression and test != 0
                        // TODO: Implement proper short-circuit evaluation
                        self.visit_expr(expr, env)?;
                        Ok(CondItem::new(opcodes::IFNE))
                    }
                    
                    BinaryOp::Or => {
                        // For now, generate as regular expression and test != 0
                        // TODO: Implement proper short-circuit evaluation
                        self.visit_expr(expr, env)?;
                        Ok(CondItem::new(opcodes::IFNE))
                    }
                    
                    // For other binary ops, evaluate and test != 0
                    _ => {
                        self.visit_expr(expr, env)?;
                        Ok(CondItem::new(opcodes::IFNE))
                    }
                }
            }
            
            // Boolean literals - constant conditions
            Expr::Literal(lit_expr) => {
                if let Literal::Boolean(value) = &lit_expr.value {
                    if *value {
                        // Always true - no jumps needed, fall through to then
                        Ok(CondItem::always_true())
                    } else {
                        // Always false - immediate jump to else
                        Ok(CondItem::always_false())
                    }
                } else {
                    // Non-boolean literal - generate and test != 0
                    self.visit_expr(expr, env)?;
                    Ok(CondItem::new(opcodes::IFNE))
                }
            }
            
            // Unary not operation
            Expr::Unary(unary_expr) => {
                use crate::ast::UnaryOp;
                match &unary_expr.operator {
                    UnaryOp::Not => {
                        // Generate condition for operand and negate
                        let inner_cond = self.gen_cond(&unary_expr.operand, env)?;
                        Ok(CondItem::negate(inner_cond))
                    }
                    _ => {
                        // For other unary ops, evaluate and test != 0
                        self.visit_expr(expr, env)?;
                        Ok(CondItem::new(opcodes::IFNE))
                    }
                }
            }
            
            // For all other expressions, evaluate and test != 0
            _ => {
                self.visit_expr(expr, env)?;
                Ok(CondItem::new(opcodes::IFNE))
            }
        }
    }
    
    /// Check if condition is always false (JavaC: isFalse)
    fn is_cond_false(&self, cond: &CondItem) -> bool {
        cond.is_false()
    }
    
    /// Generate jump when condition is false (JavaC: jumpFalse)
    fn cond_jump_false(&self, cond: &CondItem, items: &mut Items) -> Result<Option<Box<crate::codegen::chain::Chain>>> {
        cond.jump_false(&mut items.code)
    }
    
    /// Generate jump when condition is true (JavaC: jumpTrue)
    fn cond_jump_true(&self, cond: &CondItem, items: &mut Items) -> Result<Option<Box<crate::codegen::chain::Chain>>> {
        cond.jump_true(&mut items.code)
    }
    
    /// Resolve true jumps to current position (JavaC: resolve trueJumps)
    fn cond_resolve_true(&self, cond: &CondItem, items: &mut Items) {
        if let Some(true_chain) = cond.true_jumps.as_ref() {
            items.code.resolve(Some(true_chain.clone()));
        }
        // For simple conditions without explicit true jumps, fall through naturally
    }
    
    /// Negate a conditional opcode (JavaC: negate)
    fn negate_opcode(&self, opcode: u8) -> u8 {
        match opcode {
            opcodes::IFEQ => opcodes::IFNE,
            opcodes::IFNE => opcodes::IFEQ,
            opcodes::IFLT => opcodes::IFGE,
            opcodes::IFGE => opcodes::IFLT,
            opcodes::IFGT => opcodes::IFLE,
            opcodes::IFLE => opcodes::IFGT,
            opcodes::IF_ICMPEQ => opcodes::IF_ICMPNE,
            opcodes::IF_ICMPNE => opcodes::IF_ICMPEQ,
            opcodes::IF_ICMPLT => opcodes::IF_ICMPGE,
            opcodes::IF_ICMPGE => opcodes::IF_ICMPLT,
            opcodes::IF_ICMPGT => opcodes::IF_ICMPLE,
            opcodes::IF_ICMPLE => opcodes::IF_ICMPGT,
            opcodes::IF_ACMPEQ => opcodes::IF_ACMPNE,
            opcodes::IF_ACMPNE => opcodes::IF_ACMPEQ,
            opcodes::IFNULL => opcodes::IFNONNULL,
            opcodes::IFNONNULL => opcodes::IFNULL,
            _ => opcode, // Fallback for unknown opcodes
        }
    }
    
    /// Visit while loop - JavaC pattern alignment (Gen.java:visitWhileLoop)
    pub fn visit_while(&mut self, tree: &WhileStmt, env: &GenContext) -> Result<()> {
        // JavaC: genLoop(tree, tree.body, tree.cond, List.<JCExpressionStatement>nil(), true);
        self.gen_loop(&tree.body, Some(&tree.condition), &[], true, env)
    }
    
    /// Visit do-while loop - JavaC pattern alignment (Gen.java:visitDoLoop)
    pub fn visit_do_while(&mut self, tree: &DoWhileStmt, env: &GenContext) -> Result<()> {
        // JavaC: genLoop(tree, tree.body, tree.cond, List.<JCExpressionStatement>nil(), false);
        // do-while loop - condition tested last
        self.gen_loop(&tree.body, Some(&tree.condition), &[], false, env)
    }
    
    /// Visit for loop - JavaC pattern alignment (Gen.java:visitForLoop)
    pub fn visit_for(&mut self, tree: &ForStmt, env: &GenContext) -> Result<()> {
        // Store nextreg limit (javac pattern)
        let limit = self.with_items(|items| Ok(items.code.max_locals))?;
        
        // Generate initializers (JavaC: genStats(tree.init, env))
        for init_stmt in &tree.init {
            self.visit_stmt(init_stmt, env)?;
        }
        
        // JavaC: genLoop(tree, tree.body, tree.cond, tree.step, true);
        // Convert ExprStmt to Stmt for the step parameter
        let step_stmts: Vec<Stmt> = tree.update.iter()
            .map(|expr_stmt| Stmt::Expression(expr_stmt.clone()))
            .collect();
        self.gen_loop(&tree.body, tree.condition.as_ref(), &step_stmts, true, env)?;
        
        // End scopes (javac pattern)
        self.with_items(|items| {
            items.code.end_scopes(limit);
            Ok(())
        })?;
        
        Ok(())
    }
    
    /// Generate loop - JavaC Gen.genLoop equivalent (lines 1189-1230)
    /// This method handles both while and for loops with proper javac alignment
    fn gen_loop(&mut self, 
                body: &Stmt, 
                condition: Option<&Expr>, 
                step: &[Stmt], 
                test_first: bool, 
                env: &GenContext) -> Result<()> {
        
        // JavaC: Env<GenContext> loopEnv = env.dup(loop, new GenContext());
        let mut loop_env = env.dup(); // Create new environment for loop scope
        
        // JavaC: int startpc = code.entryPoint();
        let start_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
        
        if test_first {
            // while or for loop - condition tested first
            
            // Generate condition
            let cond = if let Some(condition) = condition {
                // JavaC: c = genCond(TreeInfo.skipParens(cond), CRT_FLOW_CONTROLLER);
                self.gen_cond(condition, env)?
            } else {
                // JavaC: c = items.makeCondItem(goto_);
                CondItem {
                    opcode: opcodes::GOTO,
                    true_jumps: None,
                    false_jumps: None,
                }
            };
            
            // JavaC: Chain loopDone = c.jumpFalse();
            let loop_done = self.with_items(|items| {
                cond.jump_false(&mut items.code)
            })?;
            
            // JavaC: code.resolve(c.trueJumps);
            self.with_items(|items| {
                if let Some(true_jumps) = cond.true_jumps.as_ref() {
                    items.code.resolve(Some(true_jumps.clone()));
                }
                Ok(())
            })?;
            
            // Clear any previous break/continue chains for this loop
            self.current_break_chain = None;
            self.current_continue_chain = None;
            
            // JavaC: genStat(body, loopEnv, CRT_STATEMENT | CRT_FLOW_TARGET);
            self.visit_stmt(body, &loop_env)?;
            
            // Collect any break statements generated during body execution
            if let Some(break_chain) = self.current_break_chain.take() {
                loop_env.add_exit(Some(break_chain));
            }
            
            // Collect any continue statements generated during body execution
            if let Some(continue_chain) = self.current_continue_chain.take() {
                loop_env.add_cont(Some(continue_chain));
            }
            
            // JavaC: code.resolve(loopEnv.info.cont); - continue statements
            self.with_items(|items| {
                items.code.resolve(loop_env.cont.take());
                Ok(())
            })?;
            
            // JavaC: genStats(step, loopEnv); - for loop step statements
            for step_stmt in step {
                self.visit_stmt(step_stmt, &loop_env)?;
            }
            
            // JavaC: code.resolve(code.branch(goto_), startpc);
            self.with_items(|items| {
                let goto_branch = items.code.branch(opcodes::GOTO).ok_or_else(|| {
                    crate::error::Error::CodeGen { message: "Failed to create goto branch".to_string() }
                })?;
                items.code.resolve_chain(goto_branch, start_pc);
                Ok(())
            })?;
            
            // JavaC: code.resolve(loopDone);
            self.with_items(|items| {
                items.code.resolve(loop_done);
                Ok(())
            })?;
            
        } else {
            // do-while loop - condition tested last
            
            // Clear any previous break/continue chains for this loop
            self.current_break_chain = None;
            self.current_continue_chain = None;
            
            // JavaC: genStat(body, loopEnv, CRT_STATEMENT | CRT_FLOW_TARGET);
            self.visit_stmt(body, &loop_env)?;
            
            // Collect any break statements generated during body execution
            if let Some(break_chain) = self.current_break_chain.take() {
                loop_env.add_exit(Some(break_chain));
            }
            
            // Collect any continue statements generated during body execution
            if let Some(continue_chain) = self.current_continue_chain.take() {
                loop_env.add_cont(Some(continue_chain));
            }
            
            // JavaC: code.resolve(loopEnv.info.cont); - continue statements
            self.with_items(|items| {
                items.code.resolve(loop_env.cont.take());
                Ok(())
            })?;
            
            // JavaC: genStats(step, loopEnv);
            for step_stmt in step {
                self.visit_stmt(step_stmt, &loop_env)?;
            }
            
            // Generate condition
            let cond = if let Some(condition) = condition {
                // JavaC: c = genCond(TreeInfo.skipParens(cond), CRT_FLOW_CONTROLLER);
                self.gen_cond(condition, env)?
            } else {
                // JavaC: c = items.makeCondItem(goto_);
                CondItem {
                    opcode: opcodes::GOTO,
                    true_jumps: None,
                    false_jumps: None,
                }
            };
            
            // JavaC: code.resolve(c.jumpTrue(), startpc);
            let true_jump = self.with_items(|items| {
                cond.jump_true(&mut items.code)
            })?;
            if let Some(true_jump) = true_jump {
                self.with_items(|items| {
                    items.code.resolve_chain(true_jump, start_pc);
                    Ok(())
                })?;
            }
            
            // JavaC: code.resolve(c.falseJumps);
            self.with_items(|items| {
                if let Some(false_jumps) = cond.false_jumps.as_ref() {
                    items.code.resolve(Some(false_jumps.clone()));
                }
                Ok(())
            })?;
        }
        
        // JavaC: Chain exit = loopEnv.info.exit; - break statements  
        if let Some(exit_chain) = loop_env.exit.take() {
            // JavaC: code.resolve(exit);
            self.with_items(|items| {
                items.code.resolve(Some(exit_chain));
                Ok(())
            })?;
            
            // JavaC: exit.state.defined.excludeFrom(code.nextreg);
            // TODO: Implement proper register exclusion for local variable cleanup
        }
        
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
    
    /// Visit break statement - JavaC visitBreak equivalent (Gen.java:1793-1798)  
    pub fn visit_break(&mut self, tree: &crate::ast::BreakStmt, env: &GenContext) -> Result<()> {
        // JavaC: Assert.check(code.state.stacksize == 0);
        // Ensure stack is clear before break
        // TODO: Add stack size assertion when we have proper stack tracking
        
        // JavaC: targetEnv.info.addExit(code.branch(goto_));
        // Generate a goto and add to the exit chain of target environment
        if let Some(goto_chain) = self.with_items(|items| {
            Ok(items.code.branch(super::opcodes::GOTO))
        })? {
            // Store the break jump for the enclosing loop to resolve
            // For now, we'll need to handle this with a global break chain
            // In a full implementation, we'd find the target environment and add to its exit chain
            self.current_break_chain = crate::codegen::code::Code::merge_chains(
                Some(goto_chain), 
                self.current_break_chain.take()
            );
        }
        
        // Mark code as dead after break (JavaC behavior)
        self.with_items(|items| {
            items.code.mark_dead();
            Ok(())
        })?;
        
        Ok(())
    }
    
    /// Visit continue statement - JavaC visitContinue equivalent (Gen.java:1800-1805)
    pub fn visit_continue(&mut self, tree: &crate::ast::ContinueStmt, env: &GenContext) -> Result<()> {
        // For now, we need a mutable environment to track continue chains
        // In full implementation, we'd use unwind(tree.target, env) to find target environment
        
        // JavaC: Assert.check(code.state.stacksize == 0);
        // Ensure stack is clear before continue
        
        // JavaC: targetEnv.info.addCont(code.branch(goto_));
        // Generate a goto and add to the cont chain of target environment  
        if let Some(goto_chain) = self.with_items(|items| {
            Ok(items.code.branch(super::opcodes::GOTO))
        })? {
            // For now, we'll need to handle this at the loop level
            // In full implementation, this would be added to the appropriate environment's cont chain
            // Store the continue jump for the enclosing loop to resolve
            self.current_continue_chain = crate::codegen::code::Code::merge_chains(
                Some(goto_chain), 
                self.current_continue_chain.take()
            );
        }
        
        // Mark code as dead after continue (JavaC behavior)
        self.with_items(|items| {
            items.code.mark_dead();
            Ok(())
        })?;
        
        Ok(())
    }
}