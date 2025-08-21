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
    /// Visit if statement - simplified implementation for now (to be enhanced with StackMapTable)
    pub fn visit_if(&mut self, tree: &IfStmt, env: &GenContext) -> Result<()> {
        // For now, just generate both branches without proper conditionals
        // This is a temporary solution until we can properly integrate the branching system
        // TODO: Implement proper conditional branching with StackMapTable integration
        
        // Generate condition expression (result is on stack)
        let _cond_result = self.visit_expr(&tree.condition, env)?;
        
        // Simple conditional using if-eq pattern - basic implementation
        self.with_items(|items| {
            // Pop the condition value to use it
            // For a boolean condition, we expect 0 (false) or 1 (true)
            // This is a simplified implementation
            items.code.emitop(opcodes::POP); // Remove condition from stack for now
            Ok(())
        })?;
        
        // Generate then branch
        self.visit_stmt(&tree.then_branch, env)?;
        
        // Generate else branch if present (without proper jumping for now)
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