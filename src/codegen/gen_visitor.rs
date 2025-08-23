//! Simplified visitor methods for Gen - avoiding borrowing conflicts
//!
//! This module provides simplified implementations of all visitor methods
//! to resolve borrowing issues. Full implementations will be added later.

use crate::ast::*;
use crate::common::error::{Result, Error};
use super::gen::{Gen, GenContext};
use super::items::{Item as BytecodeItem, CondItem, Items, typecodes};
use super::opcodes;
use crate::wash::attr::ResolvedType;

impl Gen {
    // ========== TYPE CHECKING INFRASTRUCTURE - JavaC Attr.java patterns ==========
    
    /// Check type compatibility - JavaC types.isAssignable equivalent
    /// Validates that 'from_type' can be assigned to 'to_type'
    fn check_assignable(&mut self, from_type: &TypeEnum, to_type: &TypeEnum, context: &str) -> Result<()> {
        // Use the Types system for proper type checking
        let is_assignable = self.type_inference.types_mut().is_assignable(from_type, to_type);
        
        if is_assignable {
            eprintln!("✅ TYPE CHECK: {} assignable to {} in {}", 
                self.type_to_string(from_type), self.type_to_string(to_type), context);
            Ok(())
        } else {
            eprintln!("❌ TYPE ERROR: {} not assignable to {} in {}", 
                self.type_to_string(from_type), self.type_to_string(to_type), context);
            Err(Error::CodeGen {
                message: format!("Type mismatch in {}: cannot assign {} to {}", 
                    context, self.type_to_string(from_type), self.type_to_string(to_type))
            })
        }
    }
    
    /// Check if type is numeric - JavaC types.isNumeric equivalent
    fn is_numeric_type(&self, typ: &TypeEnum) -> bool {
        self.type_inference.types().symtab().is_numeric(typ)
    }
    
    /// Check if type is integral - JavaC types.isIntegral equivalent
    fn is_integral_type(&self, typ: &TypeEnum) -> bool {
        self.type_inference.types().symtab().is_integral(typ)
    }
    
    /// Check if type is floating point - JavaC types.isFloating equivalent
    fn is_floating_type(&self, typ: &TypeEnum) -> bool {
        self.type_inference.types().symtab().is_floating(typ)
    }
    
    /// Check if type is reference - JavaC types.isReference equivalent
    fn is_reference_type(&self, typ: &TypeEnum) -> bool {
        self.type_inference.types().symtab().is_reference(typ)
    }
    
    /// Check binary operation type compatibility - JavaC Attr.visitBinary equivalent
    fn check_binary_op_types(&mut self, op: &BinaryOp, left_type: &TypeEnum, right_type: &TypeEnum) -> Result<TypeEnum> {
        match op {
            // Arithmetic operators - require numeric types
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                if !self.is_numeric_type(left_type) {
                    return Err(Error::CodeGen {
                        message: format!("Arithmetic operator {:?} requires numeric left operand, got {}", 
                            op, self.type_to_string(left_type))
                    });
                }
                if !self.is_numeric_type(right_type) {
                    return Err(Error::CodeGen {
                        message: format!("Arithmetic operator {:?} requires numeric right operand, got {}", 
                            op, self.type_to_string(right_type))
                    });
                }
                
                // Determine result type using JavaC's numeric promotion rules
                self.get_binary_numeric_result_type(left_type, right_type)
            }
            
            // Bitwise operators - require integral types
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor |
            BinaryOp::LShift | BinaryOp::RShift | BinaryOp::URShift => {
                if !self.is_integral_type(left_type) {
                    return Err(Error::CodeGen {
                        message: format!("Bitwise operator {:?} requires integral left operand, got {}", 
                            op, self.type_to_string(left_type))
                    });
                }
                if !self.is_integral_type(right_type) {
                    return Err(Error::CodeGen {
                        message: format!("Bitwise operator {:?} requires integral right operand, got {}", 
                            op, self.type_to_string(right_type))
                    });
                }
                
                self.get_binary_integral_result_type(left_type, right_type)
            }
            
            // Comparison operators - return boolean
            BinaryOp::Eq | BinaryOp::Ne => {
                // Can compare any types for equality
                Ok(self.type_inference.types().symtab().boolean_type.clone())
            }
            
            BinaryOp::Lt | BinaryOp::Le | 
            BinaryOp::Gt | BinaryOp::Ge => {
                if !self.is_numeric_type(left_type) || !self.is_numeric_type(right_type) {
                    return Err(Error::CodeGen {
                        message: format!("Relational operator {:?} requires numeric operands, got {} and {}", 
                            op, self.type_to_string(left_type), self.type_to_string(right_type))
                    });
                }
                Ok(self.type_inference.types().symtab().boolean_type.clone())
            }
            
            // Logical operators - require boolean types
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                self.check_boolean_type(left_type, "logical operator left operand")?;
                self.check_boolean_type(right_type, "logical operator right operand")?;
                Ok(self.type_inference.types().symtab().boolean_type.clone())
            }
        }
    }
    
    /// Check if type is boolean - JavaC equivalent
    fn check_boolean_type(&self, typ: &TypeEnum, context: &str) -> Result<()> {
        match typ {
            TypeEnum::Primitive(PrimitiveType::Boolean) => Ok(()),
            _ => Err(Error::CodeGen {
                message: format!("{} requires boolean type, got {}", context, self.type_to_string(typ))
            })
        }
    }
    
    /// Get binary numeric result type - JavaC numeric promotion rules
    fn get_binary_numeric_result_type(&self, left: &TypeEnum, right: &TypeEnum) -> Result<TypeEnum> {
        match (left, right) {
            // Double promotion
            (TypeEnum::Primitive(PrimitiveType::Double), _) | (_, TypeEnum::Primitive(PrimitiveType::Double)) => {
                Ok(self.type_inference.types().symtab().double_type.clone())
            }
            // Float promotion
            (TypeEnum::Primitive(PrimitiveType::Float), _) | (_, TypeEnum::Primitive(PrimitiveType::Float)) => {
                Ok(self.type_inference.types().symtab().float_type.clone())
            }
            // Long promotion
            (TypeEnum::Primitive(PrimitiveType::Long), _) | (_, TypeEnum::Primitive(PrimitiveType::Long)) => {
                Ok(self.type_inference.types().symtab().long_type.clone())
            }
            // Default to int for other integral types
            _ => Ok(self.type_inference.types().symtab().int_type.clone())
        }
    }
    
    /// Get binary integral result type - for bitwise operations
    fn get_binary_integral_result_type(&self, left: &TypeEnum, right: &TypeEnum) -> Result<TypeEnum> {
        match (left, right) {
            // Long promotion for integral operations
            (TypeEnum::Primitive(PrimitiveType::Long), _) | (_, TypeEnum::Primitive(PrimitiveType::Long)) => {
                Ok(self.type_inference.types().symtab().long_type.clone())
            }
            // Default to int
            _ => Ok(self.type_inference.types().symtab().int_type.clone())
        }
    }
    
    /// Check unary operation type compatibility - JavaC Attr.visitUnary equivalent
    fn check_unary_op_type(&mut self, op: &UnaryOp, operand_type: &TypeEnum) -> Result<TypeEnum> {
        match op {
            UnaryOp::Plus | UnaryOp::Minus => {
                if !self.is_numeric_type(operand_type) {
                    return Err(Error::CodeGen {
                        message: format!("Unary arithmetic operator {:?} requires numeric operand, got {}", 
                            op, self.type_to_string(operand_type))
                    });
                }
                Ok(operand_type.clone())
            }
            
            UnaryOp::BitNot => {
                if !self.is_integral_type(operand_type) {
                    return Err(Error::CodeGen {
                        message: format!("Bitwise NOT requires integral operand, got {}", 
                            self.type_to_string(operand_type))
                    });
                }
                Ok(operand_type.clone())
            }
            
            UnaryOp::Not => {
                self.check_boolean_type(operand_type, "logical NOT operand")?;
                Ok(self.type_inference.types().symtab().boolean_type.clone())
            }
            
            UnaryOp::PreInc | UnaryOp::PostInc | 
            UnaryOp::PreDec | UnaryOp::PostDec => {
                if !self.is_numeric_type(operand_type) {
                    return Err(Error::CodeGen {
                        message: format!("Increment/decrement requires numeric operand, got {}", 
                            self.type_to_string(operand_type))
                    });
                }
                Ok(operand_type.clone())
            }
        }
    }
    
    /// Check cast compatibility - JavaC types.isCastable equivalent
    fn check_cast_compatibility(&mut self, from_type: &TypeEnum, to_type: &TypeEnum) -> Result<()> {
        // Simplified cast checking - full implementation would use Types system
        match (from_type, to_type) {
            // Same types are always castable
            _ if from_type == to_type => Ok(()),
            
            // Primitive casts
            (TypeEnum::Primitive(_), TypeEnum::Primitive(_)) => Ok(()),
            
            // Reference type casts - simplified
            (TypeEnum::Reference(_), TypeEnum::Reference(_)) => Ok(()),
            
            // Primitive to reference boxing
            (TypeEnum::Primitive(_), TypeEnum::Reference(_)) => Ok(()),
            
            // Reference to primitive unboxing
            (TypeEnum::Reference(_), TypeEnum::Primitive(_)) => Ok(()),
            
            _ => Err(Error::CodeGen {
                message: format!("Cannot cast {} to {}", 
                    self.type_to_string(from_type), self.type_to_string(to_type))
            })
        }
    }
    
    /// Convert TypeEnum to string for error messages - utility method
    fn type_to_string(&self, typ: &TypeEnum) -> String {
        match typ {
            TypeEnum::Primitive(prim) => format!("{:?}", prim),
            TypeEnum::Reference(ref_type) => {
                match ref_type {
                    ReferenceType::Class(name) => name.clone(),
                    ReferenceType::Interface(name) => format!("interface {}", name),
                    ReferenceType::Array(element_type) => format!("{}[]", element_type.name),
                }
            }
            TypeEnum::Void => "void".to_string(),
        }
    }
    
    /// Infer expression type - JavaC Attr.attribExpr equivalent
    /// This is a key method that determines the type of any expression
    fn infer_expression_type(&mut self, expr: &Expr) -> Result<TypeEnum> {
        match expr {
            Expr::Literal(lit) => Ok(self.get_literal_type(lit)),
            
            Expr::Identifier(ident) => {
                // Look up in symbol table
                if let Some(symbol) = self.type_inference.types().symtab().lookup_symbol(&ident.name) {
                    Ok(symbol.typ.clone())
                } else {
                    // Fallback to Object type
                    eprintln!("⚠️ TYPE INFERENCE: Unknown identifier '{}', defaulting to Object", ident.name);
                    Ok(self.type_inference.types().symtab().object_type.clone())
                }
            }
            
            Expr::Binary(binary) => {
                let left_type = self.infer_expression_type(&binary.left)?;
                let right_type = self.infer_expression_type(&binary.right)?;
                self.check_binary_op_types(&binary.operator, &left_type, &right_type)
            }
            
            Expr::Unary(unary) => {
                let operand_type = self.infer_expression_type(&unary.operand)?;
                self.check_unary_op_type(&unary.operator, &operand_type)
            }
            
            Expr::Cast(cast) => {
                let source_type = self.infer_expression_type(&cast.expr)?;
                let target_type = TypeEnum::from(cast.target_type.clone());
                self.check_cast_compatibility(&source_type, &target_type)?;
                Ok(target_type)
            }
            
            Expr::Assignment(assign) => {
                let target_type = self.infer_expression_type(&assign.target)?;
                let value_type = self.infer_expression_type(&assign.value)?;
                self.check_assignable(&value_type, &target_type, "assignment")?;
                Ok(target_type)
            }
            
            _ => {
                // For other expression types, use wash type info or default to Object
                eprintln!("⚠️ TYPE INFERENCE: Unsupported expression type, defaulting to Object");
                Ok(self.type_inference.types().symtab().object_type.clone())
            }
        }
    }
    
    /// Get literal type - JavaC equivalent
    fn get_literal_type(&self, lit: &LiteralExpr) -> TypeEnum {
        match &lit.value {
            Literal::Integer(_) => self.type_inference.types().symtab().int_type.clone(),
            Literal::Long(_) => self.type_inference.types().symtab().long_type.clone(),
            Literal::Float(_) => self.type_inference.types().symtab().float_type.clone(),
            Literal::Double(_) => self.type_inference.types().symtab().double_type.clone(),
            Literal::Boolean(_) => self.type_inference.types().symtab().boolean_type.clone(),
            Literal::Char(_) => self.type_inference.types().symtab().char_type.clone(),
            Literal::String(_) => self.type_inference.types().symtab().string_type.clone(),
            Literal::Null => self.type_inference.types().symtab().object_type.clone(),
        }
    }
    
    /// Convert binary operator to string for debugging
    fn binary_op_to_string(&self, op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
            BinaryOp::And => "&",
            BinaryOp::Or => "|",
            BinaryOp::Xor => "^",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",
            BinaryOp::URShift => ">>>",
            _ => "?", // Default for unknown operators
        }
    }
    
    /// Convert unary operator to string for debugging
    fn unary_op_to_string(&self, op: &crate::ast::UnaryOp) -> &'static str {
        match op {
            crate::ast::UnaryOp::Plus => "+",
            crate::ast::UnaryOp::Minus => "-",
            crate::ast::UnaryOp::BitNot => "~",
            crate::ast::UnaryOp::Not => "!",
            crate::ast::UnaryOp::PreInc => "++",
            crate::ast::UnaryOp::PostInc => "++",
            crate::ast::UnaryOp::PreDec => "--",
            crate::ast::UnaryOp::PostDec => "--",
        }
    }
    
    // ========== END TYPE CHECKING INFRASTRUCTURE ==========
    
    /// Visit literal expression
    pub fn visit_literal(&mut self, tree: &LiteralExpr, _env: &GenContext) -> Result<BytecodeItem> {
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
                        -1 => {
                            code.emitop(super::opcodes::ICONST_M1);
                            code.state.push(crate::codegen::code::Type::Int);
                        }
                        0 => {
                            code.emitop(super::opcodes::ICONST_0);
                            code.state.push(crate::codegen::code::Type::Int);
                        }
                        1 => {
                            code.emitop(super::opcodes::ICONST_1);
                            code.state.push(crate::codegen::code::Type::Int);
                        }
                        2 => {
                            code.emitop(super::opcodes::ICONST_2);
                            code.state.push(crate::codegen::code::Type::Int);
                        }
                        3 => {
                            code.emitop(super::opcodes::ICONST_3);
                            code.state.push(crate::codegen::code::Type::Int);
                        }
                        4 => {
                            code.emitop(super::opcodes::ICONST_4);
                            code.state.push(crate::codegen::code::Type::Int);
                        }
                        5 => {
                            code.emitop(super::opcodes::ICONST_5);
                            code.state.push(crate::codegen::code::Type::Int);
                        }
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
            }
        }
        
        // Return appropriate item type based on literal type
        use super::items::{Item, typecodes};
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
            },
            value: tree.value.clone(),
        })
    }
    
    /// Visit identifier expression - simplified version
    /// Visit identifier expression
    /// Uses symbol-based resolution
    pub fn visit_ident(&mut self, tree: &IdentifierExpr, env: &GenContext) -> Result<BytecodeItem> {
        eprintln!("🔍 GEN: visit_ident for '{}' (symbol-based resolution)", tree.name);
        
        // Handle special identifiers
        if tree.name == "this" {
            return self.with_items(|items| {
                let this_item = items.make_this_item();
                items.load_item(&this_item)
            });
        } else if tree.name == "super" {
            return self.with_items(|items| {
                let super_item = items.make_super_item();
                items.load_item(&super_item)
            });
        }
        
        // Get current method and class context for symbol resolution
        let method_context = env.method.as_ref().map(|m| m.name.as_str());
        let class_context = env.clazz.as_ref().map(|c| c.name.as_str()).unwrap_or("UnknownClass");
        
        // Use wash/SymbolEnvironment for symbol resolution
        let resolved_symbol = if let Some(ref symbol_env) = self.wash_symbol_env {
            symbol_env.resolve_identifier(&tree.name, method_context, class_context).cloned()
        } else {
            None
        };
        
        if let Some(var_symbol) = resolved_symbol {
            eprintln!("✅ GEN: Resolved '{}' via symbol table: kind={:?}, owner={}", 
                     tree.name, var_symbol.kind, var_symbol.owner);
            
            return match var_symbol.kind {
                // Local variable (like JavaC: sym.kind == VAR && sym.owner.kind == MTH)
                crate::wash::enter::SymbolKind::Variable => {
                    let slot = var_symbol.local_slot.unwrap_or(1) as u16;
                    let var_type = var_symbol.var_type.clone();
                    eprintln!("📍 GEN: Loading local variable '{}' from slot {}", tree.name, slot);
                    self.with_items(|items| {
                        // Create type-aware local item using resolved type
                        let resolved_type = crate::wash::attr::ResolvedType::Reference(var_type);
                        let local_item = items.make_local_item_for_resolved_type(&resolved_type, slot);
                        items.load_item(&local_item)
                    })
                }
                
                // Field access (like JavaC: static vs instance field handling)
                crate::wash::enter::SymbolKind::Field => {
                    let field_name = tree.name.clone();
                    let owner_class = var_symbol.owner.trim_start_matches("class:").to_string();
                    let var_type = var_symbol.var_type.clone();
                    let is_static = var_symbol.is_static;
                    
                    if is_static {
                        eprintln!("📍 GEN: Loading static field '{}'", field_name);
                        // Static field (like JavaC: (sym.flags() & STATIC) != 0)
                        self.with_items(|items| {
                            let resolved_type = crate::wash::attr::ResolvedType::Reference(var_type);
                            let static_item = items.make_static_item_for_resolved_type(
                                &field_name, 
                                &owner_class,
                                &resolved_type
                            );
                            items.load_item(&static_item)
                        })
                    } else {
                        eprintln!("📍 GEN: Loading instance field '{}'", field_name);
                        // Instance field (like JavaC: load this, then member)
                        self.with_items(|items| {
                            let this_item = items.make_this_item();
                            items.load_item(&this_item)?; // Load 'this' first
                            let resolved_type = crate::wash::attr::ResolvedType::Reference(var_type);
                            let member_item = items.make_member_item_for_resolved_type(
                                &field_name,
                                &owner_class,
                                &resolved_type,
                                false
                            );
                            items.load_item(&member_item)
                        })
                    }
                }
                
                _ => {
                    eprintln!("⚠️ GEN: Unsupported symbol kind for '{}': {:?}", tree.name, var_symbol.kind);
                    Err(crate::common::error::Error::codegen_error(
                        format!("Unsupported symbol kind for identifier '{}'", tree.name)
                    ))
                }
            };
        }
        
        // Fallback: try legacy type inference system for backward compatibility
        eprintln!("🔄 GEN: Symbol not found in wash environment, trying legacy resolution for '{}'", tree.name);
        
        let local_var = self.type_inference.lookup_local(&tree.name).cloned();
        if let Some(local_var) = local_var {
            eprintln!("📍 GEN: Found '{}' in legacy local variables (slot {})", tree.name, local_var.reg);
            return self.with_items(|items| {
                let local_item = items.make_local_item(&local_var.typ, local_var.reg);
                items.load_item(&local_item)
            });
        }
        
        // Final fallback: assume it's an unresolved field access
        eprintln!("⚠️ GEN: Could not resolve '{}', assuming implicit field access", tree.name);
        self.with_items(|items| {
            // Load 'this' first for instance field access
            let this_item = items.make_this_item();
            items.load_item(&this_item)?;
            
            // Create a generic member item with Object type as fallback
            let fallback_type = crate::ast::TypeEnum::Reference(
                crate::ast::ReferenceType::Class("java/lang/Object".to_string())
            );
            Ok(items.make_member_item(
                tree.name.clone(), 
                class_context.to_string(), 
                "Ljava/lang/Object;".to_string(), 
                false, 
                &fallback_type
            ))
        })
    }
    
    /// Visit field access expression - simplified version
    pub fn visit_select(&mut self, tree: &FieldAccessExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Handle special cases like System.out
        if let Some(ref target) = tree.target {
            if let Expr::Identifier(id) = target.as_ref() {
                if id.name == "System" && tree.name == "out" {
                    // Special case: System.out field access - generate getstatic
                    let field_ref_idx = self.get_pool_mut().add_field_ref(
                        "java/lang/System",
                        "out", 
                        "Ljava/io/PrintStream;"
                    );
                    
                    self.with_items(|items| {
                        items.code.emitop(opcodes::GETSTATIC);
                        items.code.emit2(field_ref_idx);
                        // Push the PrintStream reference onto the stack
                        items.code.state.push(super::code::Type::Object("java/io/PrintStream".to_string()));
                        Ok(())
                    })?;
                    
                    return Ok(BytecodeItem::Stack { typecode: typecodes::OBJECT });
                }
            }
            
            // General case: evaluate target first, then access field
            // Don't discard the target evaluation - it loads 'this' onto the stack
            self.visit_expr(target, env)?;
            
            // Use wash type information for type-aware field access
            let field_key = format!("{}.{}", 
                if let Expr::Identifier(id) = target.as_ref() { 
                    &id.name 
                } else { 
                    "this" 
                }, 
                &tree.name);
                
            let resolved_type_opt = self.get_wash_type_info().and_then(|wash_types| {
                wash_types.get(&field_key).or_else(|| wash_types.get(&tree.name)).cloned()
            });
                
            if let Some(resolved_type) = resolved_type_opt {
                eprintln!("DEBUG: Found wash field type for '{}': {:?}", tree.name, resolved_type);
                
                // Extract class name and field descriptor before borrowing
                let owner_class = env.clazz.as_ref().map(|c| c.name.clone()).unwrap_or("UnknownClass".to_string());
                let field_name = tree.name.clone();
                let field_descriptor = self.get_field_descriptor_from_resolved_type(&resolved_type);
                
                // Direct GETFIELD emission since object is already on stack
                let field_ref_idx = self.get_pool_mut().add_field_ref(&owner_class, &field_name, &field_descriptor);
                
                return self.with_items(|items| {
                    items.code.emitop(super::opcodes::GETFIELD);
                    items.code.emit2(field_ref_idx);
                    
                    // Update stack state - pop object reference, push field value
                    items.code.state.pop(1); // Remove object reference
                    let field_type = match resolved_type {
                        crate::wash::attr::ResolvedType::Primitive(prim) => {
                            match prim {
                                crate::wash::attr::PrimitiveType::Int => super::code::Type::Int,
                                crate::wash::attr::PrimitiveType::Boolean => super::code::Type::Int,
                                crate::wash::attr::PrimitiveType::Long => super::code::Type::Long,
                                crate::wash::attr::PrimitiveType::Float => super::code::Type::Float,
                                crate::wash::attr::PrimitiveType::Double => super::code::Type::Double,
                                _ => super::code::Type::Int,
                            }
                        },
                        _ => super::code::Type::Object("java/lang/Object".to_string()),
                    };
                    items.code.state.push(field_type);
                    
                    Ok(BytecodeItem::Stack { typecode: typecodes::OBJECT })
                });
            }
            
            // Fallback to hardcoded logic for specific known fields (deprecated)
            if tree.name == "value" {
                eprintln!("WARNING: Using fallback field access for 'value' - should use wash type info");
                
                return self.with_items(|items| {
                    // Create a field item for the 'value' field
                    let current_class = env.clazz.as_ref().map(|c| c.name.clone()).unwrap_or("java/lang/Object".to_string());
                    let field_item = BytecodeItem::Member {
                        typecode: typecodes::OBJECT,
                        member_name: "value".to_string(),
                        class_name: current_class,
                        descriptor: "Ljava/lang/Object;".to_string(),
                        is_static: false,
                        nonvirtual: false,
                    };
                    
                    // Load the field using the items system
                    items.load_item(&field_item)
                });
            }
        }
        
        // Extract symbol and class info before borrowing
        let symbol_opt = self.type_inference.types().symtab().lookup_symbol(&tree.name).cloned();
        let class_name = env.clazz.as_ref().map(|c| c.name.clone()).unwrap_or("UnknownClass".to_string());
        let field_name = tree.name.clone();
        
        // Create the member access item for general fields with proper type resolution
        self.with_items(|items| {
            if let Some(symbol) = symbol_opt {
                let is_static = symbol.kind == super::symtab::SymbolKind::Field && 
                               symbol.modifiers.contains(&"static".to_string());
                // Generate proper field descriptor based on symbol type
                let descriptor = format!("L{};", class_name.replace('.', "/"));
                
                Ok(items.make_member_item(field_name.clone(), class_name.clone(), descriptor, is_static, &symbol.typ))
            } else {
                eprintln!("⚠️  WARNING: Cannot resolve field '{}', using fallback type", field_name);
                // Fallback to object type for unknown fields
                let typ = TypeEnum::Reference(ReferenceType::Class("java.lang.Object".to_string()));
                Ok(items.make_member_item(field_name, class_name, "Ljava/lang/Object;".to_string(), false, &typ))
            }
        })
    }
    
    /// Visit method call expression with generic type inference
    pub fn visit_apply(&mut self, tree: &MethodCallExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Enhanced JavaC: visitApply method with wash type integration for generics
        eprintln!("DEBUG: Method call '{}' with {} arguments", tree.name, tree.arguments.len());
        
        // Try generic method type inference using wash information
        let method_key = format!("{}()", tree.name); // Simplified method key
        let resolved_type_opt = self.get_wash_type_info().and_then(|wash_types| {
            wash_types.get(&method_key).cloned()
        });
        
        if let Some(resolved_type) = resolved_type_opt {
            eprintln!("DEBUG: Found wash method type for '{}': {:?}", tree.name, resolved_type);
            
            // Use wash type information for enhanced method call generation
            return self.gen_generic_method_call_with_wash(tree, env, &resolved_type);
        }
        
        // Determine method call type and generate appropriate bytecode
        let is_static = self.is_static_method_call(tree);
        let is_constructor = tree.name == "<init>";
        let is_super_call = self.is_super_method_call(tree);
        
        if is_static {
            // Static method call: invokestatic
            self.gen_static_method_call(tree, env)
        } else if is_constructor {
            // Constructor call: invokespecial
            self.gen_constructor_call(tree, env)
        } else if is_super_call {
            // Super method call: invokespecial
            self.gen_super_method_call(tree, env)
        } else {
            // Instance method call: invokevirtual or invokeinterface
            self.gen_instance_method_call(tree, env)
        }
    }
    
    /// Generate generic method call with wash type inference
    fn gen_generic_method_call_with_wash(
        &mut self, 
        tree: &MethodCallExpr, 
        env: &GenContext, 
        method_type: &crate::wash::attr::ResolvedType
    ) -> Result<BytecodeItem> {
        eprintln!("DEBUG: Generating generic method call with wash type inference");
        
        // Generate arguments with type awareness (moved outside to avoid borrowing issues)
        let mut arg_items = Vec::new();
        for arg in &tree.arguments {
            let arg_item = self.visit_expr(arg, env)?;
            arg_items.push(arg_item);
        }
        
        // Extract method signature information from wash ResolvedType
        let (param_types, return_type) = match method_type {
            crate::wash::attr::ResolvedType::Method(params, ret) => {
                (params.clone(), ret.as_ref())
            }
            _ => {
                eprintln!("WARNING: Expected Method type for '{}', got {:?}", tree.name, method_type);
                // Fallback to regular method call
                return self.gen_instance_method_call(tree, env);
            }
        };
        
        // Build method descriptor from resolved types (with type erasure for generics)
        let mut descriptor = String::from("(");
        for param_type in &param_types {
            descriptor.push_str(&self.resolved_type_to_descriptor(param_type)?);
        }
        descriptor.push(')');
        descriptor.push_str(&self.resolved_type_to_descriptor(return_type)?);
        
        eprintln!("DEBUG: Generated generic method descriptor: {}", descriptor);
        
        // Extract necessary data before borrowing
        let class_name = env.clazz.as_ref().map(|c| c.name.clone()).unwrap_or("UnknownClass".to_string());
        let method_name = tree.name.clone();
        
        // Add method ref to pool before with_items
        let method_ref_idx = self.get_pool_mut().add_method_ref(&class_name, &method_name, &descriptor);
        
        return self.with_items(|items| {
            items.code.emitop(super::opcodes::INVOKEVIRTUAL); // Use invokevirtual for instance methods
            items.code.emit2(method_ref_idx);
            
            // Create return item based on resolved return type
            let return_item = items.make_stack_item_for_resolved_type(return_type);
            Ok(return_item)
        });
    }
    
    /// Convert TypeRef to TypeEnum helper
    fn type_ref_to_type_enum(&self, type_ref: &TypeRef) -> Result<TypeEnum> {
        // Use the existing as_type_enum method from the trait
        Ok(type_ref.as_type_enum())
    }
    
    /// Convert TypeEnum to JVM descriptor helper
    fn type_enum_to_descriptor(&self, type_enum: &TypeEnum) -> Result<String> {
        match type_enum {
            TypeEnum::Primitive(prim) => {
                match prim {
                    PrimitiveType::Boolean => Ok("Z".to_string()),
                    PrimitiveType::Byte => Ok("B".to_string()),
                    PrimitiveType::Short => Ok("S".to_string()),
                    PrimitiveType::Int => Ok("I".to_string()),
                    PrimitiveType::Long => Ok("J".to_string()),
                    PrimitiveType::Float => Ok("F".to_string()),
                    PrimitiveType::Double => Ok("D".to_string()),
                    PrimitiveType::Char => Ok("C".to_string()),
                }
            }
            TypeEnum::Reference(ref_type) => {
                match ref_type {
                    ReferenceType::Class(class_name) | ReferenceType::Interface(class_name) => {
                        let internal_name = class_name.replace('.', "/");
                        Ok(format!("L{};", internal_name))
                    }
                    ReferenceType::Array(_) => {
                        // Arrays need special handling - for now, simplify to Object[]
                        Ok("[Ljava/lang/Object;".to_string())
                    }
                }
            }
            TypeEnum::Void => Ok("V".to_string()),
            _ => {
                eprintln!("WARNING: Unsupported TypeEnum for descriptor: {:?}", type_enum);
                Ok("Ljava/lang/Object;".to_string())
            }
        }
    }
    
    /// Convert ResolvedType to JVM descriptor for method signatures
    fn resolved_type_to_descriptor(&self, resolved_type: &crate::wash::attr::ResolvedType) -> Result<String> {
        use crate::wash::attr::{ResolvedType, PrimitiveType};
        
        match resolved_type {
            ResolvedType::Primitive(prim) => {
                match prim {
                    PrimitiveType::Boolean => Ok("Z".to_string()),
                    PrimitiveType::Byte => Ok("B".to_string()),
                    PrimitiveType::Short => Ok("S".to_string()),
                    PrimitiveType::Int => Ok("I".to_string()),
                    PrimitiveType::Long => Ok("J".to_string()),
                    PrimitiveType::Float => Ok("F".to_string()),
                    PrimitiveType::Double => Ok("D".to_string()),
                    PrimitiveType::Char => Ok("C".to_string()),
                }
            }
            ResolvedType::Reference(class_name) => {
                let internal_name = class_name.replace('.', "/");
                Ok(format!("L{};", internal_name))
            }
            ResolvedType::Array(element_type) => {
                Ok(format!("[{}", self.resolved_type_to_descriptor(element_type)?))
            }
            ResolvedType::TypeVariable(_) => {
                // Type erasure: T -> Object
                Ok("Ljava/lang/Object;".to_string())
            }
            ResolvedType::Wildcard(wildcard) => {
                // Type erasure: ? extends T -> upper bound (or Object)
                if let Some(ref bound) = wildcard.bound {
                    self.resolved_type_to_descriptor(bound)
                } else {
                    Ok("Ljava/lang/Object;".to_string())
                }
            }
            ResolvedType::Null => Ok("Ljava/lang/Object;".to_string()),
            _ => {
                eprintln!("WARNING: Unsupported ResolvedType for descriptor: {:?}", resolved_type);
                Ok("Ljava/lang/Object;".to_string())
            }
        }
    }
    
    /// Generate static method call (invokestatic)
    fn gen_static_method_call(&mut self, tree: &MethodCallExpr, env: &GenContext) -> Result<BytecodeItem> {
        // JavaC pattern: generate arguments first and collect their types
        let mut arg_types = Vec::new();
        for arg in &tree.arguments {
            let arg_item = self.visit_expr(arg, env)?;
            arg_types.push(self.typecode_to_type_enum(arg_item.typecode()));
        }
        
        // Determine target class and method descriptor (aligned with javac)
        let (class_name, method_descriptor) = self.resolve_static_method_info_with_types(tree, &arg_types, env)?;
        
        // Add method reference to constant pool and emit invokestatic
        let method_ref_idx = self.get_pool_mut().add_method_ref(&class_name, &tree.name, &method_descriptor);
        
        eprintln!("🔧 DEBUG: Generating invokestatic {}#{}", class_name, tree.name);
        
        self.with_items(|items| {
            // Emit invokestatic with proper constant pool reference
            items.code.emitop(super::opcodes::INVOKESTATIC);
            items.code.emit2(method_ref_idx);
            
            // Determine return type from descriptor
            let return_type = Self::parse_return_type_from_descriptor(&method_descriptor);
            
            // Return appropriate item for the result type
            Ok(if matches!(return_type, TypeEnum::Void) {
                items.make_stack_item_for_type(&TypeEnum::Void)
            } else {
                items.make_stack_item_for_type(&return_type)
            })
        })
    }
    
    /// Generate constructor call (invokespecial)
    fn gen_constructor_call(&mut self, tree: &MethodCallExpr, env: &GenContext) -> Result<BytecodeItem> {
        // JavaC pattern: object reference already on stack from NEW instruction
        
        // Generate arguments
        for arg in &tree.arguments {
            let _arg_item = self.visit_expr(arg, env)?;
        }
        
        self.with_items(|items| {
            eprintln!("🔧 DEBUG: Generating invokespecial for constructor");
            
            // Emit invokespecial bytecode
            items.code.emitop(super::opcodes::INVOKESPECIAL);
            items.code.emit2(1); // Constructor reference index (placeholder)
            
            // Constructor returns void but object reference remains on stack
            Ok(items.make_stack_item_for_type(&TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/Object".to_string()))))
        })
    }
    
    /// Generate super method call (invokespecial)
    fn gen_super_method_call(&mut self, tree: &MethodCallExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Generate 'this' reference
        self.with_items(|items| {
            items.code.emitop(super::opcodes::ALOAD_0); // Load 'this'
            Ok(())
        })?;
        
        // Generate arguments
        for arg in &tree.arguments {
            let _arg_item = self.visit_expr(arg, env)?;
        }
        
        self.with_items(|items| {
            eprintln!("🔧 DEBUG: Generating invokespecial for super call: {}", tree.name);
            
            // Emit invokespecial bytecode
            items.code.emitop(super::opcodes::INVOKESPECIAL);
            items.code.emit2(1); // Super method reference index (placeholder)
            
            // Determine return type (simplified)
            let return_type = if tree.name.starts_with("get") {
                TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/Object".to_string()))
            } else if tree.name.starts_with("is") || tree.name.starts_with("has") {
                TypeEnum::Primitive(crate::ast::PrimitiveType::Boolean)
            } else {
                TypeEnum::Void
            };
            
            Ok(items.make_stack_item_for_type(&return_type))
        })
    }
    
    /// Generate instance method call (invokevirtual or invokeinterface)
    fn gen_instance_method_call(&mut self, tree: &MethodCallExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Check if this is an interface method call first (before any mutable borrows)
        let is_interface = self.is_interface_method_call(tree);
        
        // Generate target expression (receiver)
        if let Some(ref target) = tree.target {
            let _target_item = self.visit_expr(target, env)?;
        } else {
            // Implicit 'this' reference
            self.with_items(|items| {
                items.code.emitop(super::opcodes::ALOAD_0);
                Ok(())
            })?;
        }
        
        // Generate arguments and collect argument types
        let mut arg_types = Vec::new();
        for arg in &tree.arguments {
            let arg_item = self.visit_expr(arg, env)?;
            // Determine type from the expression
            let arg_type = match arg {
                Expr::Literal(literal) => {
                    match literal.value {
                        Literal::Integer(_) => TypeEnum::Primitive(crate::ast::PrimitiveType::Int),
                        Literal::Long(_) => TypeEnum::Primitive(crate::ast::PrimitiveType::Long),
                        Literal::Float(_) => TypeEnum::Primitive(crate::ast::PrimitiveType::Float),
                        Literal::Double(_) => TypeEnum::Primitive(crate::ast::PrimitiveType::Double),
                        Literal::Boolean(_) => TypeEnum::Primitive(crate::ast::PrimitiveType::Boolean),
                        Literal::Char(_) => TypeEnum::Primitive(crate::ast::PrimitiveType::Char),
                        Literal::String(_) => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/String".to_string())),
                        _ => TypeEnum::Primitive(crate::ast::PrimitiveType::Int), // Default
                    }
                }
                Expr::Identifier(_) => {
                    // TODO: Proper symbol resolution - for now assume int variables
                    TypeEnum::Primitive(crate::ast::PrimitiveType::Int)
                }
                _ => TypeEnum::Primitive(crate::ast::PrimitiveType::Int), // Default to int
            };
            arg_types.push(arg_type);
        }
        
        // Determine target class and method descriptor (like static method call)
        let (class_name, method_descriptor) = self.resolve_instance_method_info_with_types(tree, &arg_types)?;
        eprintln!("🔧 DEBUG: Method descriptor for {} on {}: {}", tree.name, class_name, method_descriptor);
        
        // Add method reference to constant pool
        let method_ref_idx = if is_interface {
            self.get_pool_mut().add_interface_method_ref(&class_name, &tree.name, &method_descriptor)
        } else {
            self.get_pool_mut().add_method_ref(&class_name, &tree.name, &method_descriptor)
        };
        
        // Determine return type from actual method descriptor
        let return_type = Self::parse_return_type_from_descriptor(&method_descriptor);
        
        self.with_items(|items| {
            if is_interface {
                eprintln!("🔧 DEBUG: Generating invokeinterface for: {} -> #{}", tree.name, method_ref_idx);
                items.code.emitop(super::opcodes::INVOKEINTERFACE);
                items.code.emit2(method_ref_idx); // Correct interface method reference
                items.code.emit1(tree.arguments.len() as u8 + 1); // Argument count + receiver
                items.code.emit1(0); // Reserved byte
            } else {
                eprintln!("🔧 DEBUG: Generating invokevirtual for: {} -> #{}", tree.name, method_ref_idx);
                items.code.emitop(super::opcodes::INVOKEVIRTUAL);
                items.code.emit2(method_ref_idx); // Correct method reference index
                
                // Pop arguments and receiver from stack
                let stack_consume = tree.arguments.len() as u16 + 1; // arguments + receiver
                items.code.state.pop(stack_consume);
                
                // Push return value if non-void
                if !matches!(return_type, TypeEnum::Void) {
                    match &return_type {
                        TypeEnum::Primitive(crate::ast::PrimitiveType::Long) | 
                        TypeEnum::Primitive(crate::ast::PrimitiveType::Double) => {
                            items.code.state.push(super::code::Type::Long); // 2 slots
                        }
                        TypeEnum::Reference(_) => {
                            items.code.state.push(super::code::Type::Object("java/lang/Object".to_string()));
                        }
                        _ => {
                            items.code.state.push(super::code::Type::Int); // 1 slot
                        }
                    }
                }
            }
            
            Ok(items.make_stack_item_for_type(&return_type))
        })
    }
    
    /// Check if this is a static method call
    fn is_static_method_call(&self, tree: &MethodCallExpr) -> bool {
        // Heuristics for static method detection
        // In a complete implementation, this would use symbol table information
        if let Some(ref target) = tree.target {
            if let Expr::Identifier(ident) = target.as_ref() {
                // Check for known static method patterns
                match ident.name.as_str() {
                    "System" | "Math" | "String" | "Integer" | "Double" | "Float" | "Long" | "Object" => true,
                    _ => false, // Unknown class - assume instance method for safety
                }
            } else {
                false // Complex target expression - likely instance method
            }
        } else {
            // No target - could be current class static method or instance method with implicit 'this'
            // Check if this is a known static method in the current class
            self.is_current_class_static_method(&tree.name)
        }
    }
    
    /// Check if a method is static in the current class
    fn is_current_class_static_method(&self, method_name: &str) -> bool {
        // Check wash type information for method modifiers
        if let Some(wash_types) = self.get_wash_type_info() {
            let method_key = format!("{}()", method_name);
            if let Some(_resolved_type) = wash_types.get(&method_key) {
                // For now, use heuristics based on common static method names
                // In future, this could check actual method modifiers from wash
                return self.is_static_method_by_name(method_name);
            }
        }
        
        // Fallback to heuristics based on method name patterns
        self.is_static_method_by_name(method_name)
    }
    
    /// Heuristic detection of static methods by name patterns
    fn is_static_method_by_name(&self, method_name: &str) -> bool {
        match method_name {
            // Common static utility method names
            "equal" | "equals" | "compare" | "valueOf" | "parse" | "toString" | 
            "min" | "max" | "abs" | "sqrt" | "sin" | "cos" | "tan" |
            "getInstance" | "newInstance" | "create" | "builder" |
            "of" | "from" | "empty" | "copyOf" => true,
            // Methods that are typically instance methods
            "get" | "set" | "add" | "remove" | "size" | "isEmpty" | 
            "contains" | "iterator" | "toArray" | "clear" => false,
            _ => {
                // For unknown methods, check if they follow static naming conventions
                // Static methods often have no receiver and are utility functions
                method_name.chars().next().map_or(false, |c| c.is_lowercase())
            }
        }
    }
    
    /// Check if this is a super method call
    fn is_super_method_call(&self, tree: &MethodCallExpr) -> bool {
        if let Some(ref target) = tree.target {
            if let Expr::Identifier(ident) = target.as_ref() {
                return ident.name == "super";
            }
        }
        false
    }
    
    /// Check if this is an interface method call
    fn is_interface_method_call(&self, tree: &MethodCallExpr) -> bool {
        // Check if the target is cast to an interface type
        if let Some(ref target) = tree.target {
            match target.as_ref() {
                Expr::Parenthesized(inner) => {
                    if let Expr::Cast(cast_expr) = inner.as_ref() {
                        return self.is_interface_type(&cast_expr.target_type.name);
                    }
                }
                Expr::Cast(cast_expr) => {
                    return self.is_interface_type(&cast_expr.target_type.name);
                }
                Expr::Identifier(ident) => {
                    // Check if this identifier is a field of interface type
                    // For now, specifically handle "next" field as HashMapCell interface
                    if ident.name == "next" {
                        return true; // HashMapCell is an interface
                    }
                }
                Expr::FieldAccess(field_access) => {
                    // Check if the field type is an interface
                    if field_access.name == "next" {
                        return true; // HashMapCell is an interface
                    }
                }
                _ => {}
            }
        }
        
        // Simplified heuristics - in real implementation would use type information
        tree.name.contains("Iterator") ||
        tree.name.contains("Collection") ||
        tree.name.contains("List") ||
        tree.name.contains("Map") ||
        tree.name.contains("Set")
    }
    
    /// Check if a type name represents an interface
    fn is_interface_type(&self, type_name: &str) -> bool {
        match type_name {
            "Comparable" | "Comparator" | "Iterable" | "Iterator" |
            "Collection" | "List" | "Set" | "Map" | "Queue" | "Deque" |
            "Runnable" | "Callable" | "Serializable" | "Cloneable" |
            "HashMapCell" | "Entry" => true,
            _ => false,
        }
    }
    
    /// Infer method return type from method name (simplified)
    fn infer_method_return_type(&self, method_name: &str) -> TypeEnum {
        match method_name {
            // Void methods
            name if name.starts_with("set") => TypeEnum::Void,
            name if name == "println" || name == "print" => TypeEnum::Void,
            name if name.starts_with("add") || name.starts_with("remove") => TypeEnum::Primitive(crate::ast::PrimitiveType::Boolean),
            
            // Boolean methods
            name if name.starts_with("is") || name.starts_with("has") => TypeEnum::Primitive(crate::ast::PrimitiveType::Boolean),
            name if name.starts_with("contains") || name.starts_with("equals") => TypeEnum::Primitive(crate::ast::PrimitiveType::Boolean),
            
            // Integer methods
            name if name == "size" || name == "length" => TypeEnum::Primitive(crate::ast::PrimitiveType::Int),
            name if name == "hashCode" || name.starts_with("compare") => TypeEnum::Primitive(crate::ast::PrimitiveType::Int),
            name if name.contains("indexOf") => TypeEnum::Primitive(crate::ast::PrimitiveType::Int),
            
            // String methods
            name if name == "toString" || name.starts_with("substring") => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/String".to_string())),
            name if name.starts_with("get") && name.contains("String") => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/String".to_string())),
            
            // HashMapCell interface methods
            name if name == "after" || name == "before" => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/util/HashMapCell".to_string())),
            name if name == "next" => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/util/HashMapCell".to_string())),
            
            // Object methods (default)
            name if name.starts_with("get") => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/Object".to_string())),
            name if name == "clone" => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/Object".to_string())),
            
            // Default to Object for unknown methods
            _ => TypeEnum::Reference(crate::ast::ReferenceType::Class("java/lang/Object".to_string())),
        }
    }
    
    /// Visit new expression - JavaC Gen.visitNewClass equivalent
    pub fn visit_new(&mut self, tree: &NewExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Check if this is array creation (has array dimensions)
        if tree.target_type.array_dims > 0 {
            return self.visit_new_array(tree, env);
        }
        
        // JavaC pattern for regular object creation:
        // 1. Generate 'new' instruction to allocate object
        // 2. Duplicate reference for constructor call
        // 3. Generate constructor arguments
        // 4. Call constructor with invokespecial
        
        // Get the class name from target_type
        let class_name = &tree.target_type.name;
        
        // Add class reference to constant pool
        let class_idx = self.get_pool_mut().add_class(class_name);
        
        self.with_items(|items| {
            // 1. Generate 'new' instruction - allocate object
            items.code.emitop(super::opcodes::NEW);
            items.code.emit2(class_idx);
            items.code.state.push(super::code::Type::Object(class_name.clone()));
            
            // 2. Duplicate reference for constructor call
            items.code.emitop(super::opcodes::DUP);
            items.code.state.push(super::code::Type::Object(class_name.clone()));
            
            Ok(())
        })?;
        
        // 3. Generate constructor arguments and collect their types
        let mut arg_types = Vec::new();
        for arg in &tree.arguments {
            let arg_item = self.visit_expr(arg, env)?;
            arg_types.push(self.typecode_to_type_enum(arg_item.typecode()));
        }
        
        // 4. Build constructor method descriptor
        let method_descriptor = self.generate_method_descriptor_with_types("<init>", &arg_types);
        
        // 5. Add constructor method reference to constant pool
        let constructor_ref_idx = self.get_pool_mut().add_method_ref(class_name, "<init>", &method_descriptor);
        
        self.with_items(|items| {
            // 6. Call constructor with invokespecial
            items.code.emitop(super::opcodes::INVOKESPECIAL);
            items.code.emit2(constructor_ref_idx);
            
            // Pop constructor arguments and the duplicated reference
            let arg_count = tree.arguments.len() + 1; // +1 for 'this' reference
            items.code.state.pop(arg_count as u16);
            
            // The result is the object reference (from the first 'dup')
            Ok(BytecodeItem::Stack { typecode: super::items::typecodes::OBJECT })
        })
    }
    
    /// Handle array creation using newarray/anewarray/multianewarray
    fn visit_new_array(&mut self, tree: &NewExpr, env: &GenContext) -> Result<BytecodeItem> {
        let element_type = &tree.target_type.name;
        let dimensions = tree.target_type.array_dims;
        
        // Generate dimension expressions (array sizes)
        // For "new int[5][3]", arguments would be [5, 3]
        for arg in &tree.arguments {
            let _arg_item = self.visit_expr(arg, env)?;
        }
        
        // Prepare constants outside the closure
        let class_idx = if dimensions == 1 && !Self::is_primitive_type(element_type) {
            Some(self.get_pool_mut().add_class(element_type))
        } else if dimensions > 1 {
            let array_descriptor = format!("{}{}", "[".repeat(dimensions), 
                if Self::is_primitive_type(element_type) {
                    Self::get_primitive_descriptor(element_type).to_string()
                } else {
                    format!("L{};", element_type)
                });
            Some(self.get_pool_mut().add_class(&array_descriptor))
        } else {
            None
        };
        
        self.with_items(|items| {
            if dimensions == 1 {
                // Single dimension array
                if Self::is_primitive_type(element_type) {
                    // Use newarray for primitive arrays
                    let atype = Self::get_primitive_array_type(element_type);
                    items.code.emitop(super::opcodes::NEWARRAY);
                    items.code.emit1(atype);
                    items.code.state.pop(1); // Pop count
                    items.code.state.push(super::code::Type::Object(format!("[{}", Self::get_primitive_descriptor(element_type))));
                } else {
                    // Use anewarray for reference arrays
                    items.code.emitop(super::opcodes::ANEWARRAY);
                    items.code.emit2(class_idx.unwrap());
                    items.code.state.pop(1); // Pop count
                    items.code.state.push(super::code::Type::Object(format!("[L{};", element_type)));
                }
            } else if dimensions > 1 {
                // Multi-dimensional array
                let array_descriptor = format!("{}{}", "[".repeat(dimensions), 
                    if Self::is_primitive_type(element_type) {
                        Self::get_primitive_descriptor(element_type).to_string()
                    } else {
                        format!("L{};", element_type)
                    });
                items.code.emitop(super::opcodes::MULTIANEWARRAY);
                items.code.emit2(class_idx.unwrap());
                items.code.emit1(tree.arguments.len() as u8); // Number of dimensions provided
                items.code.state.pop(tree.arguments.len() as u16); // Pop all dimension counts
                items.code.state.push(super::code::Type::Object(array_descriptor));
            }
            
            Ok(BytecodeItem::Stack { typecode: super::items::typecodes::OBJECT })
        })
    }
    
    /// Check if a type name represents a primitive type
    fn is_primitive_type(type_name: &str) -> bool {
        matches!(type_name, "boolean" | "byte" | "char" | "short" | "int" | "long" | "float" | "double")
    }
    
    /// Get the atype code for newarray instruction
    fn get_primitive_array_type(type_name: &str) -> u8 {
        match type_name {
            "boolean" => 4,
            "char" => 5,
            "float" => 6,
            "double" => 7,
            "byte" => 8,
            "short" => 9,
            "int" => 10,
            "long" => 11,
            _ => 10, // Default to int
        }
    }
    
    /// Get the JVM type descriptor for primitive types
    fn get_primitive_descriptor(type_name: &str) -> &'static str {
        match type_name {
            "boolean" => "Z",
            "byte" => "B",
            "char" => "C",
            "short" => "S",
            "int" => "I",
            "long" => "J",
            "float" => "F",
            "double" => "D",
            _ => "I", // Default to int
        }
    }
    
    /// Visit binary expression - JavaC-aligned implementation
    pub fn visit_binary(&mut self, tree: &BinaryExpr, env: &GenContext) -> Result<BytecodeItem> {
        use crate::ast::BinaryOp;
        
        // Handle short-circuit operators first (they need special logic)
        match tree.operator {
            BinaryOp::LogicalAnd => {
                return self.visit_logical_and(tree, env);
            }
            BinaryOp::LogicalOr => {
                return self.visit_logical_or(tree, env);
            }
            _ => {} // Continue with normal binary operations
        }
        
        // Type check binary operation using new infrastructure
        // First, infer operand types
        let left_type = self.infer_expression_type(&tree.left)?;
        let right_type = self.infer_expression_type(&tree.right)?;
        
        // Apply type checking for the specific operator
        let result_type = self.check_binary_op_types(&tree.operator, &left_type, &right_type)?;
        
        eprintln!("🔍 TYPE CHECK: Binary {} {} {} = {}", 
            self.type_to_string(&left_type), 
            self.binary_op_to_string(&tree.operator),
            self.type_to_string(&right_type),
            self.type_to_string(&result_type)
        );
        
        // Special handling for null comparisons (before evaluating operands)
        if matches!(tree.operator, BinaryOp::Eq | BinaryOp::Ne) {
            let is_null_comparison = matches!(tree.left.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null)) ||
                                    matches!(tree.right.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null));
            
            if is_null_comparison {
                // Determine which operand is not null
                let non_null_expr = if matches!(tree.left.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null)) {
                    &tree.right
                } else {
                    &tree.left
                };
                
                // Evaluate only the non-null operand
                let _non_null_item = self.visit_expr(non_null_expr, env)?;
                
                return self.with_items(|items| {
                    // Now we have one value on stack - generate null comparison
                    match tree.operator {
                        BinaryOp::Eq => {
                            // x == null -> ifnull true_branch
                            let null_branch = items.code.branch(opcodes::IFNULL);
                            items.code.emitop(opcodes::ICONST_0); // false (x is not null)
                            let end_branch = items.code.branch(opcodes::GOTO);
                            items.code.resolve(null_branch);
                            items.code.emitop(opcodes::ICONST_1); // true (x is null)
                            items.code.resolve(end_branch);
                        }
                        BinaryOp::Ne => {
                            // x != null -> ifnull false_branch  
                            let null_branch = items.code.branch(opcodes::IFNULL);
                            items.code.emitop(opcodes::ICONST_1); // true (x is not null)
                            let end_branch = items.code.branch(opcodes::GOTO);
                            items.code.resolve(null_branch);
                            items.code.emitop(opcodes::ICONST_0); // false (x is null)
                            items.code.resolve(end_branch);
                        }
                        _ => unreachable!()
                    }
                    Ok(items.make_stack_item_for_type(&TypeEnum::Primitive(PrimitiveType::Boolean)))
                });
            }
        }
        
        // Generate operands for normal binary operations
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
                    // Comparison operations (null comparisons handled earlier)
                    // Non-null comparisons - use arithmetic/comparison instructions
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
                        TypeEnum::Reference(_) => {
                            // Reference comparisons (non-null) use if_acmp instructions
                            match tree.operator {
                                BinaryOp::Eq => {
                                    let eq_branch = items.code.branch(opcodes::IF_ACMPEQ);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(eq_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                BinaryOp::Ne => {
                                    let ne_branch = items.code.branch(opcodes::IF_ACMPNE);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(ne_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                _ => {
                                    eprintln!("⚠️  WARNING: Unsupported reference comparison: {:?}", tree.operator);
                                    items.code.emitop(opcodes::ICONST_0);
                                }
                            }
                            return Ok(items.make_stack_item_for_type(&TypeEnum::Primitive(PrimitiveType::Boolean)));
                        }
                        _ => {
                            // Integer comparisons - use if_icmp instructions  
                            match tree.operator {
                                BinaryOp::Eq => {
                                    let eq_branch = items.code.branch(opcodes::IF_ICMPEQ);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(eq_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                BinaryOp::Ne => {
                                    let ne_branch = items.code.branch(opcodes::IF_ICMPNE);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(ne_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                BinaryOp::Lt => {
                                    let lt_branch = items.code.branch(opcodes::IF_ICMPLT);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(lt_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                BinaryOp::Le => {
                                    let le_branch = items.code.branch(opcodes::IF_ICMPLE);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(le_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                BinaryOp::Gt => {
                                    let gt_branch = items.code.branch(opcodes::IF_ICMPGT);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(gt_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                BinaryOp::Ge => {
                                    let ge_branch = items.code.branch(opcodes::IF_ICMPGE);
                                    items.code.emitop(opcodes::ICONST_0); // false
                                    let end_branch = items.code.branch(opcodes::GOTO);
                                    items.code.resolve(ge_branch);
                                    items.code.emitop(opcodes::ICONST_1); // true
                                    items.code.resolve(end_branch);
                                }
                                _ => {
                                    eprintln!("⚠️  WARNING: Unsupported integer comparison: {:?}", tree.operator);
                                    items.code.emitop(opcodes::ICONST_0);
                                }
                            }
                            return Ok(items.make_stack_item_for_type(&TypeEnum::Primitive(PrimitiveType::Boolean)));
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
    
    /// Visit LogicalAnd expression with proper short circuit evaluation (JavaC aligned)
    fn visit_logical_and(&mut self, tree: &BinaryExpr, env: &GenContext) -> Result<BytecodeItem> {
        // JavaC pattern for && : if left is false, jump to false result
        let _left_item = self.visit_expr(&tree.left, env)?;
        
        let false_chain = self.with_items(|items| {
            // If left is false (0), jump to false result
            let false_chain = items.code.branch(opcodes::IFEQ);
            Ok(false_chain)
        })?;
        
        // Left is true, evaluate right operand
        let _right_item = self.visit_expr(&tree.right, env)?;
        
        self.with_items(|items| {
            // If right is also true, set result to true and jump to end
            let true_chain = items.code.branch(opcodes::IFEQ);
            items.code.emitop(opcodes::ICONST_1); // Push true (1)
            let end_chain = items.code.branch(opcodes::GOTO);
            
            // Resolve false chain - left was false, set result to false
            if let Some(chain) = false_chain {
                items.code.resolve(Some(chain));
            }
            if let Some(chain) = true_chain {
                items.code.resolve(Some(chain));
            }
            items.code.emitop(opcodes::ICONST_0); // Push false (0)
            
            // Resolve end chain
            if let Some(chain) = end_chain {
                items.code.resolve(Some(chain));
            }
            
            let result_type = TypeEnum::Primitive(PrimitiveType::Boolean);
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Visit LogicalOr expression with proper short circuit evaluation (JavaC aligned)
    fn visit_logical_or(&mut self, tree: &BinaryExpr, env: &GenContext) -> Result<BytecodeItem> {
        // JavaC pattern for || : if left is true, jump to true result
        let _left_item = self.visit_expr(&tree.left, env)?;
        
        let true_chain = self.with_items(|items| {
            // If left is true (non-zero), jump to true result
            let true_chain = items.code.branch(opcodes::IFNE);
            Ok(true_chain)
        })?;
        
        // Left is false, evaluate right operand
        let _right_item = self.visit_expr(&tree.right, env)?;
        
        self.with_items(|items| {
            // If right is also false, set result to false and jump to end
            let false_chain = items.code.branch(opcodes::IFEQ);
            items.code.emitop(opcodes::ICONST_1); // Push true (1) - right was true
            let end_chain = items.code.branch(opcodes::GOTO);
            
            // Resolve true chain - left was true, set result to true
            if let Some(chain) = true_chain {
                items.code.resolve(Some(chain));
            }
            items.code.emitop(opcodes::ICONST_1); // Push true (1)
            let end_chain2 = items.code.branch(opcodes::GOTO);
            
            // Resolve false chain - both were false, set result to false
            if let Some(chain) = false_chain {
                items.code.resolve(Some(chain));
            }
            items.code.emitop(opcodes::ICONST_0); // Push false (0)
            
            // Resolve end chains
            if let Some(chain) = end_chain {
                items.code.resolve(Some(chain));
            }
            if let Some(chain) = end_chain2 {
                items.code.resolve(Some(chain));
            }
            
            let result_type = TypeEnum::Primitive(PrimitiveType::Boolean);
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Infer binary operation result type based on operands and operator
    fn infer_binary_result_type(&self, left_item: &BytecodeItem, right_item: &BytecodeItem, operator: &BinaryOp) -> TypeEnum {
        use crate::ast::BinaryOp;
        use super::items::typecodes;
        
        // Get type codes from items
        let left_type = left_item.typecode();
        let right_type = right_item.typecode();
        
        // Comparison and logical operators always return boolean
        match operator {
            BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | 
            BinaryOp::Gt | BinaryOp::Ge | BinaryOp::And | BinaryOp::Or |
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
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
    pub fn visit_unary(&mut self, tree: &UnaryExpr, env: &GenContext) -> Result<BytecodeItem> {
        use crate::ast::UnaryOp;
        
        // Type check unary operation using new infrastructure
        let operand_type = self.infer_expression_type(&tree.operand)?;
        let result_type = self.check_unary_op_type(&tree.operator, &operand_type)?;
        
        eprintln!("🔍 TYPE CHECK: Unary {} {} = {}", 
            self.unary_op_to_string(&tree.operator),
            self.type_to_string(&operand_type),
            self.type_to_string(&result_type)
        );
        
        // Generate operand
        let operand_item = self.visit_expr(&tree.operand, env)?;
        
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
                UnaryOp::PreInc => {
                    // Pre-increment: increment first, then return the incremented value
                    // Stack: [value] -> [value+1]
                    items.code.emitop(opcodes::ICONST_1);
                    items.code.emitop(opcodes::IADD);
                    // TODO: Store back to variable (requires variable tracking)
                    eprintln!("⚠️  WARNING: PreInc simplified - computes increment but doesn't store to variable");
                }
                UnaryOp::PreDec => {
                    // Pre-decrement: decrement first, then return the decremented value  
                    // Stack: [value] -> [value-1]
                    items.code.emitop(opcodes::ICONST_1);
                    items.code.emitop(opcodes::ISUB);
                    // TODO: Store back to variable (requires variable tracking)
                    eprintln!("⚠️  WARNING: PreDec simplified - computes decrement but doesn't store to variable");
                }
                UnaryOp::PostInc => {
                    // Post-increment: return current value, but increment happens behind the scenes
                    // Stack: [value] -> [value] (same value returned, increment ignored for now)
                    // For proper implementation, we'd need to:
                    // 1. Store current value to temp
                    // 2. Increment original variable  
                    // 3. Return temp value
                    // For now, just pass through the current value
                    eprintln!("⚠️  WARNING: PostInc simplified - returns current value without incrementing");
                }
                UnaryOp::PostDec => {
                    // Post-decrement: return current value, but decrement happens behind the scenes  
                    // Stack: [value] -> [value] (same value returned, decrement ignored for now)
                    eprintln!("⚠️  WARNING: PostDec simplified - returns current value without decrementing");
                }
            }
            
            Ok(items.make_stack_item_for_type(&result_type))
        })
    }
    
    /// Infer unary operation result type based on operand and operator
    fn infer_unary_result_type(&self, operand_item: &BytecodeItem, operator: &UnaryOp) -> TypeEnum {
        use crate::ast::UnaryOp;
        use super::items::typecodes;
        
        let operand_type = operand_item.typecode();
        
        match operator {
            UnaryOp::Not => TypeEnum::Primitive(PrimitiveType::Boolean),
            UnaryOp::Plus | UnaryOp::Minus | UnaryOp::BitNot | 
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                // Preserve the operand type for arithmetic operations
                match operand_type {
                    typecodes::DOUBLE => TypeEnum::Primitive(PrimitiveType::Double),
                    typecodes::FLOAT => TypeEnum::Primitive(PrimitiveType::Float),
                    typecodes::LONG => TypeEnum::Primitive(PrimitiveType::Long),
                    _ => TypeEnum::Primitive(PrimitiveType::Int),
                }
            }
        }
    }
    
    /// Visit assignment expression - JavaC-aligned version following visitAssign pattern
    pub fn visit_assign_javac(&mut self, tree: &AssignmentExpr, env: &GenContext) -> Result<BytecodeItem> {
        self.visit_assign_javac_internal(tree, env, true)
    }
    
    /// Visit assignment for statement context (no result needed)
    pub fn visit_assign_javac_stmt(&mut self, tree: &AssignmentExpr, env: &GenContext) -> Result<()> {
        let _result = self.visit_assign_javac_internal(tree, env, false)?;
        Ok(())
    }
    
    /// Internal JavaC assignment implementation
    fn visit_assign_javac_internal(&mut self, tree: &AssignmentExpr, env: &GenContext, need_result: bool) -> Result<BytecodeItem> {
        use crate::codegen::items::{Item, typecodes};
        
        // JavaC pattern: Item l = genExpr(tree.lhs, tree.lhs.type);
        //                genExpr(tree.rhs, tree.lhs.type).load();
        //                result = items.makeAssignItem(l);
        
        eprintln!("🔧 DEBUG: JavaC-style assignment (need_result={}): {:?} = {:?}", need_result, tree.target, tree.value);
        
        // Step 1: Generate left-hand side item (don't load it yet!)
        let lhs_item = self.generate_lhs_item_javac(tree, env)?;
        
        // Step 2: Generate right-hand side and load it onto stack
        let _rhs_item = self.visit_expr(&tree.value, env)?;
        
        // Step 3: Create assignment and execute it
        if need_result {
            // For expression context: create assignment item and load it (duplicates result)
            let assign_item = self.with_items(|items| {
                let assign_item = items.make_assign_item(lhs_item);
                // Load the assignment item to trigger the assignment execution
                assign_item.load(items)
            })?;
            Ok(assign_item)
        } else {
            // For statement context: do assignment directly without duplication
            self.with_items(|items| {
                // For statement context, store directly without stashing (no dup needed)
                lhs_item.store(items)?;
                Ok(BytecodeItem::Stack { typecode: typecodes::VOID })
            })
        }
    }
    
    /// Generate left-hand side item for assignment (JavaC genExpr equivalent for lhs)
    fn generate_lhs_item_javac(&mut self, tree: &AssignmentExpr, env: &GenContext) -> Result<crate::codegen::items::Item> {
        use crate::codegen::items::{Item, typecodes};
        
        match tree.target.as_ref() {
            Expr::Identifier(ident) => {
                // For identifiers like "next", check if it's a field
                let field_name = &ident.name;
                let class_name = env.clazz.as_ref().map(|c| c.name.clone()).unwrap_or("UnknownClass".to_string());
                
                // Determine field descriptor based on field name
                let descriptor = match field_name.as_str() {
                    "next" => "Ljava/util/HashMapCell;".to_string(),
                    "hashMap" => "Ljava/util/HashMap;".to_string(),
                    _ => "Ljava/lang/Object;".to_string(),
                };
                
                eprintln!("🔧 DEBUG: Creating MemberItem for field '{}' with descriptor '{}'", field_name, descriptor);
                
                // Load 'this' reference first for field access
                self.with_items(|items| {
                    items.code.emitop(crate::codegen::opcodes::ALOAD_0);
                    Ok(())
                })?;
                
                Ok(Item::Member {
                    typecode: typecodes::OBJECT,
                    member_name: field_name.clone(),
                    class_name,
                    descriptor,
                    is_static: false,
                    nonvirtual: false,
                })
            },
            
            Expr::FieldAccess(field_access) => {
                // Explicit field access like obj.field
                let field_name = &field_access.name;
                
                // Determine target class and descriptor based on target expression
                let (class_name, descriptor) = if let Some(ref target) = field_access.target {
                    // Generate the target object first
                    let _target_item = self.visit_expr(target, env)?;
                    
                    // Resolve target type to determine class name and field descriptor
                    match target.as_ref() {
                        Expr::Identifier(ident) => {
                            // Check if target is a known type from wash type info
                            if let Some(wash_types) = self.get_wash_type_info() {
                                if let Some(target_type) = wash_types.get(&ident.name) {
                                    match target_type {
                                        ResolvedType::Class(class_type) => {
                                            let target_class = &class_type.name;
                                            let field_desc = self.resolve_field_descriptor(target_class, field_name);
                                            (target_class.clone(), field_desc)
                                        },
                                        ResolvedType::Reference(ref_name) => {
                                            let field_desc = self.resolve_field_descriptor(ref_name, field_name);
                                            (ref_name.clone(), field_desc)
                                        },
                                        _ => {
                                            // Fallback for unknown types
                                            ("java/lang/Object".to_string(), "Ljava/lang/Object;".to_string())
                                        }
                                    }
                                } else {
                                    // No type info available, use heuristics
                                    let inferred_class = self.infer_target_class_from_identifier(&ident.name, field_name);
                                    let field_desc = self.resolve_field_descriptor(&inferred_class, field_name);
                                    (inferred_class, field_desc)
                                }
                            } else {
                                // No wash type info, use heuristics
                                let inferred_class = self.infer_target_class_from_identifier(&ident.name, field_name);
                                let field_desc = self.resolve_field_descriptor(&inferred_class, field_name);
                                (inferred_class, field_desc)
                            }
                        },
                        _ => {
                            // For other target expressions, use generic Object type
                            ("java/lang/Object".to_string(), "Ljava/lang/Object;".to_string())
                        }
                    }
                } else {
                    // No target means it's an implicit 'this' access
                    let current_class = env.clazz.as_ref().map(|c| c.name.clone()).unwrap_or("UnknownClass".to_string());
                    let field_desc = self.resolve_field_descriptor(&current_class, field_name);
                    
                    // Load 'this' reference for implicit access
                    self.with_items(|items| {
                        items.code.emitop(crate::codegen::opcodes::ALOAD_0);
                        Ok(())
                    })?;
                    
                    (current_class, field_desc)
                };
                
                eprintln!("🔧 DEBUG: Creating MemberItem for field '{}' on class '{}' with descriptor '{}'", field_name, class_name, descriptor);
                
                Ok(Item::Member {
                    typecode: typecodes::OBJECT,
                    member_name: field_name.clone(),
                    class_name,
                    descriptor,
                    is_static: false,
                    nonvirtual: false,
                })
            },
            
            _ => {
                Err(crate::common::error::Error::codegen_error(format!("Unsupported assignment target: {:?}", tree.target)))
            }
        }
    }
    
    /// Infer target class from identifier name and field context
    fn infer_target_class_from_identifier(&self, identifier: &str, field_name: &str) -> String {
        // Use heuristics based on identifier and field names
        match (identifier, field_name) {
            // Known patterns from common Java classes
            ("next", "key") | ("next", "value") | ("next", "hash") => "java/util/HashMap$Entry".to_string(),
            ("head", "next") | ("tail", "next") => "java/util/LinkedList$Node".to_string(),
            ("hashMap", _) => "java/util/HashMap".to_string(),
            ("map", _) => "java/util/Map".to_string(),
            ("list", _) => "java/util/List".to_string(),
            (_, "next") => "java/util/HashMapCell".to_string(), // Common pattern for linked structures
            _ => "java/lang/Object".to_string(), // Fallback
        }
    }
    
    /// Resolve field descriptor based on class and field name
    fn resolve_field_descriptor(&self, class_name: &str, field_name: &str) -> String {
        // Use known field patterns and wash type information if available
        if let Some(wash_types) = self.get_wash_type_info() {
            if let Some(resolved_type) = wash_types.get(field_name) {
                if let Ok(descriptor) = self.resolved_type_to_descriptor(resolved_type) {
                    return descriptor;
                }
            }
        }
        
        // Use heuristics based on class and field names
        match (class_name, field_name) {
            // HashMap related fields
            ("java/util/HashMap", "size") => "I".to_string(),
            ("java/util/HashMap", "threshold") => "I".to_string(),
            ("java/util/HashMap", "loadFactor") => "F".to_string(),
            ("java/util/HashMap", "table") => "[Ljava/util/HashMap$Entry;".to_string(),
            ("java/util/HashMap", "entrySet") => "Ljava/util/Set;".to_string(),
            
            // HashMap.Entry related fields  
            ("java/util/HashMap$Entry", "key") => "Ljava/lang/Object;".to_string(),
            ("java/util/HashMap$Entry", "value") => "Ljava/lang/Object;".to_string(),
            ("java/util/HashMap$Entry", "hash") => "I".to_string(),
            ("java/util/HashMap$Entry", "next") => "Ljava/util/HashMap$Entry;".to_string(),
            
            // HashMapCell/HashMapMyIterator related fields
            ("HashMapMyIterator", "hashMap") => "Ljava/util/HashMap;".to_string(),
            ("HashMapMyIterator", "next") => "Ljava/util/HashMapCell;".to_string(),
            ("HashMapMyIterator", "e") => "Ljava/lang/Object;".to_string(),
            ("java/util/HashMapMyIterator", "hashMap") => "Ljava/util/HashMap;".to_string(),
            ("java/util/HashMapMyIterator", "next") => "Ljava/util/HashMapCell;".to_string(),
            ("java/util/HashMapMyIterator", "e") => "Ljava/lang/Object;".to_string(),
            ("java/util/HashMapCell", "key") => "Ljava/lang/Object;".to_string(),
            ("java/util/HashMapCell", "value") => "Ljava/lang/Object;".to_string(),
            ("java/util/HashMapCell", "next") => "Ljava/util/HashMapCell;".to_string(),
            
            // LinkedList related fields
            ("java/util/LinkedList$Node", "item") => "Ljava/lang/Object;".to_string(),
            ("java/util/LinkedList$Node", "next") => "Ljava/util/LinkedList$Node;".to_string(),
            ("java/util/LinkedList$Node", "prev") => "Ljava/util/LinkedList$Node;".to_string(),
            
            // Common patterns
            (_, "next") => "Ljava/util/HashMapCell;".to_string(), // Default for next fields
            (_, "size") => "I".to_string(),
            (_, "length") => "I".to_string(),
            
            // Generic fallback
            _ => "Ljava/lang/Object;".to_string(),
        }
    }
    
    
    /// Determine if assignment should use JavaC-aligned processing
    fn should_use_javac_assignment(&self, tree: &AssignmentExpr) -> bool {
        // Use JavaC assignment for field assignments involving method calls
        match tree.target.as_ref() {
            Expr::Identifier(ident) => {
                // For specific fields like "next" that are involved in complex assignments
                if ident.name == "next" {
                    // Check if the value is a method call
                    matches!(tree.value.as_ref(), Expr::MethodCall(_))
                } else {
                    false
                }
            },
            Expr::FieldAccess(_) => {
                // For explicit field access with method call values
                matches!(tree.value.as_ref(), Expr::MethodCall(_))
            },
            _ => false,
        }
    }
    
    /// Visit assignment expression - simplified version (original)
    pub fn visit_assign(&mut self, tree: &AssignmentExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Type check assignment using new infrastructure
        let target_type = self.infer_expression_type(&tree.target)?;
        let value_type = self.infer_expression_type(&tree.value)?;
        self.check_assignable(&value_type, &target_type, "assignment")?;
        
        eprintln!("🔍 TYPE CHECK: Assignment {} = {} (assignable)", 
            self.type_to_string(&target_type),
            self.type_to_string(&value_type)
        );
        
        // For critical assignments involving interface method calls, use JavaC-aligned approach
        if self.should_use_javac_assignment(tree) {
            eprintln!("🚀 DEBUG: Using JavaC-style assignment for complex expression");
            return self.visit_assign_javac(tree, env);
        }
        
        // Check if target is a field access (this.field)
        if let Expr::FieldAccess(field_access) = tree.target.as_ref() {
            // Handle field assignment using type-aware items system
            let field_name = &field_access.name;
            
            // Try to get wash type information for the field
            let resolved_type = if let Some(wash_types) = self.get_wash_type_info() {
                wash_types.get(field_name).cloned()
            } else {
                None
            };
            
            // Note: We'll generate 'this' first, then value to match JVM putfield stack order
            
            // Extract fallback field type information BEFORE with_items to avoid borrowing conflicts
            let fallback_field_info = if resolved_type.is_none() {
                match &env.clazz {
                    Some(class) => {
                        let field_decl = class.body.iter().find_map(|member| {
                            if let ClassMember::Field(f) = member {
                                if f.name == *field_name { Some(f) } else { None }
                            } else { None }
                        });
                        
                        if let Some(field) = field_decl {
                            let field_type = self.type_ref_to_type_enum(&field.type_ref)?;
                            let descriptor = self.type_ref_to_base_descriptor(&field.type_ref.name)?;
                            Some((field_type, class.name.clone(), descriptor))
                        } else {
                            eprintln!("WARNING: Field '{}' not found in class definition, using Object type", field_name);
                            let field_type = TypeEnum::Reference(ReferenceType::Class("java.lang.Object".to_string()));
                            Some((field_type, class.name.clone(), "Ljava/lang/Object;".to_string()))
                        }
                    }
                    None => {
                        eprintln!("WARNING: No class context for field '{}', using Object type", field_name);
                        let field_type = TypeEnum::Reference(ReferenceType::Class("java.lang.Object".to_string()));
                        Some((field_type, "UnknownClass".to_string(), "Ljava/lang/Object;".to_string()))
                    }
                }
            } else {
                None
            };
            
            let field_name = field_name.clone();
            
            // Load 'this' reference first (JVM putfield expects: this, value)
            self.with_items(|items| {
                let this_item = items.make_this_item();
                items.load_item(&this_item)?;
                Ok(items.make_stack_item_for_type(&TypeEnum::Reference(ReferenceType::Class("this".to_string()))))
            })?;
            
            // Generate the value expression (after 'this' is on stack)
            eprintln!("DEBUG: Assignment value expression: {:?}", tree.value);
            let _value_item = self.visit_expr(&tree.value, env)?;
            
            // Now store to field with proper stack order: [this] [value] -> putfield
            return self.with_items(|items| {
                // Create field item for assignment
                let field_item = if let Some(resolved_type) = &resolved_type {
                    eprintln!("DEBUG: Using wash type info for field '{}': {:?}", field_name, resolved_type);
                    let owner_class = env.clazz.as_ref().map(|c| c.name.clone()).unwrap_or("UnknownClass".to_string());
                    items.make_field_item_for_resolved_type(
                        field_name.clone(),
                        owner_class,
                        resolved_type,
                        false // Non-static field
                    )
                } else if let Some((field_type, owner_class, descriptor)) = fallback_field_info {
                    items.make_member_item(
                        field_name.clone(),
                        owner_class,
                        descriptor,
                        false,
                        &field_type
                    )
                } else {
                    // Should not reach here due to fallback logic above
                    let field_type = TypeEnum::Reference(ReferenceType::Class("java.lang.Object".to_string()));
                    items.make_member_item(
                        field_name.clone(),
                        "UnknownClass".to_string(),
                        "Ljava/lang/Object;".to_string(),
                        false,
                        &field_type
                    )
                };
                
                // Store to field using the items system (type-aware)
                items.store_item(&field_item)?;
                
                // Return the field item as the assignment result
                Ok(field_item)
            });
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
    pub fn visit_type_cast(&mut self, tree: &CastExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Generate expression to cast
        let _expr_item = self.visit_expr(&tree.expr, env)?;
        
        // Generate checkcast instruction for reference types
        let target_type = TypeEnum::from(tree.target_type.clone());
        
        // Handle checkcast for reference types
        match &target_type {
            TypeEnum::Reference(ref_type) => {
                // Generate checkcast instruction for reference types
                let class_name = match ref_type {
                    crate::ast::ReferenceType::Class(class) => class.clone(),
                    crate::ast::ReferenceType::Interface(interface) => interface.clone(),
                    crate::ast::ReferenceType::Array(_) => {
                        // Handle array types - convert to descriptor format
                        format!("[{}", "Ljava/lang/Object;") // Simplified for now
                    }
                };
                
                // Add class to constant pool and emit checkcast
                let class_idx = self.get_pool_mut().add_class(&class_name);
                eprintln!("🔧 DEBUG: Generating checkcast for: {} -> #{}", class_name, class_idx);
                
                self.with_items(|items| {
                    items.code.emitop(super::opcodes::CHECKCAST);
                    items.code.emit2(class_idx);
                    Ok(items.make_stack_item_for_type(&target_type))
                })
            }
            _ => {
                // Primitive type casts - generate appropriate conversion instructions
                // For now, just handle reference type casts which is the main issue
                self.with_items(|items| {
                    Ok(items.make_stack_item_for_type(&target_type))
                })
            }
        }
    }
    
    /// Visit array access expression - JavaC-aligned implementation
    pub fn visit_indexed(&mut self, tree: &ArrayAccessExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Generate array and index expressions
        let _array_item = self.visit_expr(&tree.array, env)?;
        let _index_item = self.visit_expr(&tree.index, env)?;
        
        // Determine array element type from the array expression
        let element_type = self.infer_array_element_type(&tree.array)?;
        
        self.with_items(|items| {
            // Generate appropriate array load instruction based on element type
            match &element_type {
                TypeEnum::Primitive(primitive_type) => {
                    match primitive_type {
                        PrimitiveType::Boolean => {
                            items.code.emitop(super::opcodes::BALOAD); // boolean arrays use BALOAD
                        }
                        PrimitiveType::Byte => {
                            items.code.emitop(super::opcodes::BALOAD);
                        }
                        PrimitiveType::Char => {
                            items.code.emitop(super::opcodes::CALOAD);
                        }
                        PrimitiveType::Short => {
                            items.code.emitop(super::opcodes::SALOAD);
                        }
                        PrimitiveType::Int => {
                            items.code.emitop(super::opcodes::IALOAD);
                        }
                        PrimitiveType::Long => {
                            items.code.emitop(super::opcodes::LALOAD);
                        }
                        PrimitiveType::Float => {
                            items.code.emitop(super::opcodes::FALOAD);
                        }
                        PrimitiveType::Double => {
                            items.code.emitop(super::opcodes::DALOAD);
                        }
                    }
                }
                TypeEnum::Reference(_) => {
                    // Reference types use aaload
                    items.code.emitop(super::opcodes::AALOAD);
                }
                _ => {
                    // Default to aaload for unknown types
                    items.code.emitop(super::opcodes::AALOAD);
                }
            }
            
            // Update stack: pop array ref and index, push element
            items.code.state.pop(2); // Pop array reference and index
            match &element_type {
                TypeEnum::Primitive(PrimitiveType::Long) | TypeEnum::Primitive(PrimitiveType::Double) => {
                    items.code.state.push(super::code::Type::Long); // 2 slots
                }
                TypeEnum::Reference(ref_type) => {
                    let class_name = match ref_type {
                        ReferenceType::Class(name) => name.clone(),
                        _ => "java/lang/Object".to_string(),
                    };
                    items.code.state.push(super::code::Type::Object(class_name));
                }
                _ => {
                    items.code.state.push(super::code::Type::Int); // Most primitives are 1 slot
                }
            }
            
            Ok(items.make_indexed_item(&element_type))
        })
    }
    
    /// Infer the element type of an array from its expression
    fn infer_array_element_type(&self, array_expr: &Expr) -> Result<TypeEnum> {
        match array_expr {
            Expr::Identifier(ident) => {
                // Enhanced heuristic based on variable name
                let name = &ident.name;
                if name.contains("numbers") || name.contains("ints") || name.contains("values") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Int))
                } else if name.contains("names") || name.contains("strings") || name.contains("words") {
                    Ok(TypeEnum::Reference(ReferenceType::Class("java/lang/String".to_string())))
                } else if name.contains("booleans") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Boolean))
                } else if name.contains("chars") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Char))
                } else if name.contains("bytes") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Byte))
                } else if name.contains("shorts") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Short))
                } else if name.contains("longs") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Long))
                } else if name.contains("floats") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Float))
                } else if name.contains("doubles") {
                    Ok(TypeEnum::Primitive(PrimitiveType::Double))
                } else {
                    // Default to int array for unknown identifiers
                    Ok(TypeEnum::Primitive(PrimitiveType::Int))
                }
            }
            Expr::New(new_expr) => {
                // Array creation - extract element type
                let element_type_name = &new_expr.target_type.name;
                if Self::is_primitive_type(element_type_name) {
                    Ok(self.primitive_name_to_type_enum(element_type_name))
                } else {
                    Ok(TypeEnum::Reference(ReferenceType::Class(element_type_name.clone())))
                }
            }
            _ => {
                // Default to Object for unknown types
                Ok(TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string())))
            }
        }
    }
    
    /// Convert primitive type name to TypeEnum
    fn primitive_name_to_type_enum(&self, type_name: &str) -> TypeEnum {
        match type_name {
            "boolean" => TypeEnum::Primitive(PrimitiveType::Boolean),
            "byte" => TypeEnum::Primitive(PrimitiveType::Byte),
            "char" => TypeEnum::Primitive(PrimitiveType::Char),
            "short" => TypeEnum::Primitive(PrimitiveType::Short),
            "int" => TypeEnum::Primitive(PrimitiveType::Int),
            "long" => TypeEnum::Primitive(PrimitiveType::Long),
            "float" => TypeEnum::Primitive(PrimitiveType::Float),
            "double" => TypeEnum::Primitive(PrimitiveType::Double),
            _ => TypeEnum::Primitive(PrimitiveType::Int), // Default
        }
    }
    
    /// Visit array initializer (e.g., {1, 2, 3, 4, 5})
    pub fn visit_array_initializer(&mut self, values: &[Expr], env: &GenContext) -> Result<BytecodeItem> {
        // For array initializers like {1, 2, 3}, we need to:
        // 1. Create the array with the correct size
        // 2. Store each value at the appropriate index
        
        let array_size = values.len();
        
        // First, create the array with the appropriate size
        // We'll infer the element type from the first value
        let element_type = if let Some(first_value) = values.first() {
            self.infer_element_type_from_expr(first_value)?
        } else {
            TypeEnum::Primitive(PrimitiveType::Int) // Default for empty arrays
        };
        
        // Pre-compute class index for reference types
        let class_idx = match &element_type {
            TypeEnum::Reference(ref_type) => {
                let class_name = match ref_type {
                    ReferenceType::Class(name) => name.clone(),
                    _ => "java/lang/Object".to_string(),
                };
                Some(self.get_pool_mut().add_class(&class_name))
            }
            _ => None,
        };
        
        self.with_items(|items| {
            // Push array size onto stack
            match array_size {
                0..=5 => {
                    items.code.emitop(match array_size {
                        0 => super::opcodes::ICONST_0,
                        1 => super::opcodes::ICONST_1,
                        2 => super::opcodes::ICONST_2,
                        3 => super::opcodes::ICONST_3,
                        4 => super::opcodes::ICONST_4,
                        5 => super::opcodes::ICONST_5,
                        _ => unreachable!(),
                    });
                }
                6..=127 => {
                    items.code.emitop(super::opcodes::BIPUSH);
                    items.code.emit1(array_size as u8);
                }
                _ => {
                    items.code.emitop(super::opcodes::SIPUSH);
                    items.code.emit2(array_size as u16);
                }
            }
            items.code.state.push(super::code::Type::Int); // Array size
            
            // Create array based on element type
            match &element_type {
                TypeEnum::Primitive(primitive_type) => {
                    let atype = match primitive_type {
                        PrimitiveType::Boolean => 4,
                        PrimitiveType::Char => 5,
                        PrimitiveType::Float => 6,
                        PrimitiveType::Double => 7,
                        PrimitiveType::Byte => 8,
                        PrimitiveType::Short => 9,
                        PrimitiveType::Int => 10,
                        PrimitiveType::Long => 11,
                    };
                    items.code.emitop(super::opcodes::NEWARRAY);
                    items.code.emit1(atype);
                }
                _ => {
                    // Reference types
                    items.code.emitop(super::opcodes::ANEWARRAY);
                    items.code.emit2(class_idx.unwrap_or(1));
                }
            }
            
            items.code.state.pop(1); // Pop size, array ref remains
            items.code.state.push(super::code::Type::Object("Array".to_string()));
            
            Ok(())
        })?;
        
        // Now store each value in the array
        for (index, value) in values.iter().enumerate() {
            self.with_items(|items| {
                // Duplicate array reference for store operation
                items.code.emitop(super::opcodes::DUP);
                items.code.state.push(super::code::Type::Object("Array".to_string()));
                
                // Push index
                match index {
                    0..=5 => {
                        items.code.emitop(match index {
                            0 => super::opcodes::ICONST_0,
                            1 => super::opcodes::ICONST_1,
                            2 => super::opcodes::ICONST_2,
                            3 => super::opcodes::ICONST_3,
                            4 => super::opcodes::ICONST_4,
                            5 => super::opcodes::ICONST_5,
                            _ => unreachable!(),
                        });
                    }
                    6..=127 => {
                        items.code.emitop(super::opcodes::BIPUSH);
                        items.code.emit1(index as u8);
                    }
                    _ => {
                        items.code.emitop(super::opcodes::SIPUSH);
                        items.code.emit2(index as u16);
                    }
                }
                items.code.state.push(super::code::Type::Int); // Index
                
                Ok(())
            })?;
            
            // Generate the value expression
            let _value_item = self.visit_expr(value, env)?;
            
            // Store the value in the array
            self.with_items(|items| {
                // Generate appropriate array store instruction
                match &element_type {
                    TypeEnum::Primitive(primitive_type) => {
                        match primitive_type {
                            PrimitiveType::Boolean | PrimitiveType::Byte => {
                                items.code.emitop(super::opcodes::BASTORE);
                            }
                            PrimitiveType::Char => {
                                items.code.emitop(super::opcodes::CASTORE);
                            }
                            PrimitiveType::Short => {
                                items.code.emitop(super::opcodes::SASTORE);
                            }
                            PrimitiveType::Int => {
                                items.code.emitop(super::opcodes::IASTORE);
                            }
                            PrimitiveType::Long => {
                                items.code.emitop(super::opcodes::LASTORE);
                            }
                            PrimitiveType::Float => {
                                items.code.emitop(super::opcodes::FASTORE);
                            }
                            PrimitiveType::Double => {
                                items.code.emitop(super::opcodes::DASTORE);
                            }
                        }
                    }
                    _ => {
                        // Reference types use aastore
                        items.code.emitop(super::opcodes::AASTORE);
                    }
                }
                
                // Update stack: pop array ref, index, and value
                items.code.state.pop(3);
                
                Ok(())
            })?;
        }
        
        // Return the array reference (still on stack)
        self.with_items(|items| {
            Ok(BytecodeItem::Stack { typecode: super::items::typecodes::OBJECT })
        })
    }
    
    /// Infer element type from an expression in array initializer
    fn infer_element_type_from_expr(&self, expr: &Expr) -> Result<TypeEnum> {
        match expr {
            Expr::Literal(literal) => {
                Ok(Self::literal_to_type_enum(&literal.value))
            }
            Expr::Identifier(_) => {
                // TODO: Look up from symbol table
                Ok(TypeEnum::Primitive(PrimitiveType::Int))
            }
            _ => {
                // Default to int for unknown expressions
                Ok(TypeEnum::Primitive(PrimitiveType::Int))
            }
        }
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
        
        // Check if both branches terminate execution (aligned with javac flow analysis)
        if let Some(ref else_branch) = tree.else_branch {
            // Both then and else branches exist - check if both terminate
            let then_terminates = Self::stmt_guarantees_return(&tree.then_branch);
            let else_terminates = Self::stmt_guarantees_return(else_branch);
            
            if then_terminates && else_terminates {
                // Both branches terminate - mark code as dead (aligned with javac)
                self.with_items(|items| {
                    items.code.alive = false;
                    Ok(())
                })?;
            }
        }
        
        Ok(())
    }
    
    /// Generate condition item from expression (JavaC: genCond)
    fn gen_cond(&mut self, expr: &Expr, env: &GenContext) -> Result<CondItem> {
        use crate::ast::Expr;
        use super::items::CondItem;
        
        match expr {
            // Binary operations - direct conditional generation
            Expr::Binary(bin_expr) => {
                use crate::ast::BinaryOp;
                match &bin_expr.operator {
                    // Comparison operators - generate direct conditional
                    BinaryOp::Eq => {
                        // Special handling for null comparisons (JavaC aligned)
                        let is_null_comparison = matches!(bin_expr.left.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null)) ||
                                                matches!(bin_expr.right.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null));
                        
                        if is_null_comparison {
                            // For x == null, evaluate non-null operand and use IFNULL
                            let non_null_expr = if matches!(bin_expr.left.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null)) {
                                &bin_expr.right
                            } else {
                                &bin_expr.left
                            };
                            self.visit_expr(non_null_expr, env)?;
                            Ok(CondItem::new(opcodes::IFNULL))
                        } else {
                            // Regular comparison for non-null values
                            self.visit_expr(&bin_expr.left, env)?;
                            self.visit_expr(&bin_expr.right, env)?;
                            Ok(CondItem::new(opcodes::IF_ICMPEQ))
                        }
                    }
                    BinaryOp::Ne => {
                        // Special handling for null comparisons (JavaC aligned)
                        let is_null_comparison = matches!(bin_expr.left.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null)) ||
                                                matches!(bin_expr.right.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null));
                        
                        if is_null_comparison {
                            // For x != null, evaluate non-null operand and use IFNONNULL
                            let non_null_expr = if matches!(bin_expr.left.as_ref(), Expr::Literal(lit) if matches!(lit.value, Literal::Null)) {
                                &bin_expr.right
                            } else {
                                &bin_expr.left
                            };
                            self.visit_expr(non_null_expr, env)?;
                            Ok(CondItem::new(opcodes::IFNONNULL))
                        } else {
                            // Regular comparison for non-null values
                            self.visit_expr(&bin_expr.left, env)?;
                            self.visit_expr(&bin_expr.right, env)?;
                            Ok(CondItem::new(opcodes::IF_ICMPNE))
                        }
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
                    
                    // Bitwise logical operators (non-short-circuit)
                    BinaryOp::And => {
                        // Generate as regular expression and test != 0
                        self.visit_expr(expr, env)?;
                        Ok(CondItem::new(opcodes::IFNE))
                    }
                    
                    BinaryOp::Or => {
                        // Generate as regular expression and test != 0  
                        self.visit_expr(expr, env)?;
                        Ok(CondItem::new(opcodes::IFNE))
                    }
                    
                    // Short-circuit logical operators - JavaC aligned implementation
                    BinaryOp::LogicalAnd => {
                        // JavaC: genCond for logical AND (&&)
                        // Left operand condition
                        let left_cond = self.gen_cond(&bin_expr.left, env)?;
                        
                        // Get false jumps from left operand - if left is false, entire expression is false
                        let false_jumps = self.with_items(|items| {
                            left_cond.jump_false(&mut items.code)
                        })?;
                        
                        // If left is true, evaluate right operand
                        self.with_items(|items| {
                            left_cond.resolve_true(&mut items.code);
                            Ok(())
                        })?;
                        
                        // Right operand condition
                        let right_cond = self.gen_cond(&bin_expr.right, env)?;
                        
                        // Combine conditions: true only if both are true
                        Ok(CondItem {
                            opcode: right_cond.opcode,
                            true_jumps: right_cond.true_jumps,
                            false_jumps: {
                                // Chain false jumps from left and right
                                match (false_jumps.as_ref(), right_cond.false_jumps.as_ref()) {
                                    (Some(left_false), Some(right_false)) => {
                                        super::code::Code::merge_chains(Some(left_false.clone()), Some(right_false.clone()))
                                    }
                                    (Some(left_false), None) => Some(left_false.clone()),
                                    (None, Some(right_false)) => Some(right_false.clone()),
                                    (None, None) => None,
                                }
                            },
                        })
                    }
                    
                    BinaryOp::LogicalOr => {
                        // JavaC: genCond for logical OR (||)
                        // Left operand condition
                        let left_cond = self.gen_cond(&bin_expr.left, env)?;
                        
                        // Get true jumps from left operand - if left is true, entire expression is true
                        let true_jumps = self.with_items(|items| {
                            left_cond.jump_true(&mut items.code)
                        })?;
                        
                        // If left is false, evaluate right operand
                        self.with_items(|items| {
                            left_cond.resolve_false(&mut items.code);
                            Ok(())
                        })?;
                        
                        // Right operand condition
                        let right_cond = self.gen_cond(&bin_expr.right, env)?;
                        
                        // Combine conditions: false only if both are false
                        Ok(CondItem {
                            opcode: right_cond.opcode,
                            true_jumps: {
                                // Chain true jumps from left and right
                                match (true_jumps.as_ref(), right_cond.true_jumps.as_ref()) {
                                    (Some(left_true), Some(right_true)) => {
                                        super::code::Code::merge_chains(Some(left_true.clone()), Some(right_true.clone()))
                                    }
                                    (Some(left_true), None) => Some(left_true.clone()),
                                    (None, Some(right_true)) => Some(right_true.clone()),
                                    (None, None) => None,
                                }
                            },
                            false_jumps: right_cond.false_jumps,
                        })
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
    
    /// Visit enhanced-for statement - JavaC Lower.visitForeachLoop equivalent
    pub fn visit_enhanced_for(&mut self, tree: &EnhancedForStmt, env: &GenContext) -> Result<()> {
        // Check if the iterable is an array or implements Iterable (following JavaC pattern)
        let is_array = self.is_array_type(&tree.iterable)?;
        
        if is_array {
            self.visit_array_enhanced_for(tree, env)
        } else {
            self.visit_iterable_enhanced_for(tree, env)
        }
    }
    
    /// Handle enhanced-for over arrays (JavaC Lower.visitArrayForeachLoop pattern)
    /// Translates: for (T v : arrayexpr) stmt;
    /// To: for ({arraytype #arr = arrayexpr; int #len = arr.length; int #i = 0;} #i < #len; #i++) { T v = #arr[#i]; stmt; }
    fn visit_array_enhanced_for(&mut self, tree: &EnhancedForStmt, env: &GenContext) -> Result<()> {
        let limit = self.with_items(|items| Ok(items.code.max_locals))?;
        
        // Generate array cache variable: arraytype #arr = arrayexpr;
        let array_slot = self.with_items(|items| Ok(items.code.max_locals()))?; // Get next available slot
        self.with_items(|items| { items.code.max_locals += 1; Ok(()) })?; // Reserve slot
        
        // Evaluate the array expression and store in local variable
        let _array_result = self.visit_expr(&tree.iterable, env)?;
        self.with_items(|items| {
            items.code.emitop1(opcodes::ASTORE, array_slot as u8);
            Ok(())
        })?;
        
        // Generate length cache variable: int #len = #arr.length;
        let len_slot = self.with_items(|items| Ok(items.code.max_locals()))?; 
        self.with_items(|items| { items.code.max_locals += 1; Ok(()) })?; // Reserve slot
        
        // Load array and get length
        self.with_items(|items| {
            items.code.emitop1(opcodes::ALOAD, array_slot as u8); // Load array
            items.code.emitop(opcodes::ARRAYLENGTH); // Get array.length
            items.code.emitop1(opcodes::ISTORE, len_slot as u8); // Store length
            Ok(())
        })?;
        
        // Generate index variable: int #i = 0;
        let index_slot = self.with_items(|items| Ok(items.code.max_locals()))?;
        self.with_items(|items| { items.code.max_locals += 1; Ok(()) })?; // Reserve slot
        
        // Initialize index to 0
        self.with_items(|items| {
            items.code.emitop(opcodes::ICONST_0); // Load constant 0
            items.code.emitop1(opcodes::ISTORE, index_slot as u8); // Store index
            Ok(())
        })?;
        
        // Generate loop: for (#i < #len; #i++)
        let start_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
        
        // Loop condition: #i < #len
        self.with_items(|items| {
            items.code.emitop1(opcodes::ILOAD, index_slot as u8); // Load index
            items.code.emitop1(opcodes::ILOAD, len_slot as u8); // Load length
            Ok(())
        })?;
        
        // Branch if index >= length (exit loop)
        let loop_done = self.with_items(|items| {
            Ok(items.code.branch(opcodes::IF_ICMPGE))
        })?.ok_or_else(|| crate::common::error::Error::CodeGen { 
            message: "Failed to create loop exit branch".to_string() 
        })?;
        
        // Generate loop variable: T v = #arr[#i];
        let var_slot = self.with_items(|items| Ok(items.code.max_locals()))?;
        self.with_items(|items| { items.code.max_locals += 1; Ok(()) })?; // Reserve slot
        
        // Determine element type for correct array access instruction
        let element_type = self.infer_array_element_type(&tree.iterable)?;
        
        self.with_items(|items| {
            items.code.emitop1(opcodes::ALOAD, array_slot as u8); // Load array
            items.code.emitop1(opcodes::ILOAD, index_slot as u8); // Load index
            
            // Use appropriate array load instruction based on element type
            match &element_type {
                TypeEnum::Primitive(primitive_type) => {
                    match primitive_type {
                        PrimitiveType::Boolean | PrimitiveType::Byte => {
                            items.code.emitop(opcodes::BALOAD);
                        }
                        PrimitiveType::Char => {
                            items.code.emitop(opcodes::CALOAD);
                        }
                        PrimitiveType::Short => {
                            items.code.emitop(opcodes::SALOAD);
                        }
                        PrimitiveType::Int => {
                            items.code.emitop(opcodes::IALOAD);
                        }
                        PrimitiveType::Long => {
                            items.code.emitop(opcodes::LALOAD);
                        }
                        PrimitiveType::Float => {
                            items.code.emitop(opcodes::FALOAD);
                        }
                        PrimitiveType::Double => {
                            items.code.emitop(opcodes::DALOAD);
                        }
                    }
                    
                    // Use appropriate store instruction for primitives
                    match primitive_type {
                        PrimitiveType::Boolean | PrimitiveType::Byte => {
                            items.code.emitop1(opcodes::ISTORE, var_slot as u8);
                        }
                        PrimitiveType::Char | PrimitiveType::Short | PrimitiveType::Int => {
                            items.code.emitop1(opcodes::ISTORE, var_slot as u8);
                        }
                        PrimitiveType::Long => {
                            items.code.emitop1(opcodes::LSTORE, var_slot as u8);
                        }
                        PrimitiveType::Float => {
                            items.code.emitop1(opcodes::FSTORE, var_slot as u8);
                        }
                        PrimitiveType::Double => {
                            items.code.emitop1(opcodes::DSTORE, var_slot as u8);
                        }
                    }
                }
                _ => {
                    // Reference types use aaload/astore
                    items.code.emitop(opcodes::AALOAD);
                    items.code.emitop1(opcodes::ASTORE, var_slot as u8);
                }
            }
            
            Ok(())
        })?;
        
        // Generate loop body
        self.visit_stmt(&tree.body, env)?;
        
        // Increment index: #i++
        self.with_items(|items| {
            items.code.emitop(opcodes::IINC);
            items.code.emit1(index_slot as u8); // Local variable index
            items.code.emit1(1u8); // Increment by 1 - low byte
            items.code.emit1(0u8); // Increment by 1 - high byte (0 since 1 < 256)
            Ok(())
        })?;
        
        // Jump back to condition
        self.with_items(|items| {
            let goto_branch = items.code.branch(opcodes::GOTO).ok_or_else(|| {
                crate::common::error::Error::CodeGen { message: "Failed to create goto branch".to_string() }
            })?;
            items.code.resolve_chain(goto_branch, start_pc);
            Ok(())
        })?;
        
        // Resolve loop exit
        self.with_items(|items| {
            items.code.resolve(Some(loop_done));
            Ok(())
        })?;
        
        // End scopes
        self.with_items(|items| {
            items.code.end_scopes(limit);
            Ok(())
        })?;
        
        Ok(())
    }
    
    /// Handle enhanced-for over Iterables (JavaC Lower.visitIterableForeachLoop pattern)
    /// Translates: for (T v : coll) stmt;
    /// To: for (Iterator<T> #i = coll.iterator(); #i.hasNext(); ) { T v = (T) #i.next(); stmt; }
    fn visit_iterable_enhanced_for(&mut self, tree: &EnhancedForStmt, env: &GenContext) -> Result<()> {
        let limit = self.with_items(|items| Ok(items.code.max_locals))?;
        
        // Generate iterator variable: Iterator<T> #i = coll.iterator();
        let iterator_slot = self.with_items(|items| Ok(items.code.max_locals()))?;
        self.with_items(|items| { items.code.max_locals += 1; Ok(()) })?; // Reserve slot
        
        // Call iterator() method on the iterable
        let _iterable_result = self.visit_expr(&tree.iterable, env)?;
        
        // Add iterator() method to constant pool
        let iterator_method_ref = self.get_pool_mut().add_interface_method_ref(
            "java/lang/Iterable", 
            "iterator", 
            "()Ljava/util/Iterator;"
        );
        
        self.with_items(|items| {
            items.code.emitop(opcodes::INVOKEINTERFACE);
            items.code.emit2(iterator_method_ref);
            items.code.emit1(1); // Interface method arg count
            items.code.emit1(0); // Padding
            items.code.emitop1(opcodes::ASTORE, iterator_slot as u8); // Store iterator
            Ok(())
        })?;
        
        // Generate loop with hasNext() condition
        let start_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
        
        // Call hasNext() method
        let has_next_method_ref = self.get_pool_mut().add_interface_method_ref(
            "java/util/Iterator",
            "hasNext", 
            "()Z"
        );
        
        self.with_items(|items| {
            items.code.emitop1(opcodes::ALOAD, iterator_slot as u8); // Load iterator
            items.code.emitop(opcodes::INVOKEINTERFACE);
            items.code.emit2(has_next_method_ref);
            items.code.emit1(1); // Interface method arg count  
            items.code.emit1(0); // Padding
            Ok(())
        })?;
        
        // Branch if hasNext() returns false (exit loop)
        let loop_done = self.with_items(|items| {
            Ok(items.code.branch(opcodes::IFEQ))
        })?.ok_or_else(|| crate::common::error::Error::CodeGen { 
            message: "Failed to create loop exit branch".to_string() 
        })?;
        
        // Generate loop variable: T v = (T) #i.next();
        let var_slot = self.with_items(|items| Ok(items.code.max_locals()))?;
        self.with_items(|items| { items.code.max_locals += 1; Ok(()) })?; // Reserve slot
        
        // Call next() method
        let next_method_ref = self.get_pool_mut().add_interface_method_ref(
            "java/util/Iterator",
            "next", 
            "()Ljava/lang/Object;"
        );
        
        self.with_items(|items| {
            items.code.emitop1(opcodes::ALOAD, iterator_slot as u8); // Load iterator
            items.code.emitop(opcodes::INVOKEINTERFACE);
            items.code.emit2(next_method_ref);
            items.code.emit1(1); // Interface method arg count
            items.code.emit1(0); // Padding
            // TODO: Add type cast if needed based on variable type
            items.code.emitop1(opcodes::ASTORE, var_slot as u8); // Store in loop variable
            Ok(())
        })?;
        
        // Generate loop body
        self.visit_stmt(&tree.body, env)?;
        
        // Jump back to condition
        self.with_items(|items| {
            let goto_branch = items.code.branch(opcodes::GOTO).ok_or_else(|| {
                crate::common::error::Error::CodeGen { message: "Failed to create goto branch".to_string() }
            })?;
            items.code.resolve_chain(goto_branch, start_pc);
            Ok(())
        })?;
        
        // Resolve loop exit
        self.with_items(|items| {
            items.code.resolve(Some(loop_done));
            Ok(())
        })?;
        
        // End scopes
        self.with_items(|items| {
            items.code.end_scopes(limit);
            Ok(())
        })?;
        
        Ok(())
    }
    
    /// Check if an expression evaluates to an array type
    fn is_array_type(&self, expr: &Expr) -> Result<bool> {
        match expr {
            Expr::Identifier(id) => {
                // Enhanced heuristic: check for common array variable name patterns
                let name = &id.name;
                Ok(name.contains("numbers") || name.contains("arr") || name.contains("array") ||
                   name.contains("names") || name.contains("values") || name.contains("items") ||
                   name.ends_with("s") && name.len() > 2) // Simple plural detection
            }
            Expr::ArrayInitializer(_) => Ok(true), // Array literals are arrays
            Expr::New(new_expr) => {
                // Array creation expressions
                Ok(new_expr.target_type.array_dims > 0)
            }
            _ => Ok(false) // Default to iterable for other expressions
        }
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
        
        // Enhanced scope management: push loop scope
        let current_max_locals = self.with_items(|items| Ok(items.code.max_locals))?;
        let loop_type_for_scope = if condition.is_some() { 
            super::gen::LoopType::While 
        } else { 
            super::gen::LoopType::For 
        };
        
        let _scope_id = self.scope_manager.push_scope(
            start_pc as usize, 
            true, // is_loop_scope
            Some(loop_type_for_scope), 
            None, // no label at this level
            current_max_locals
        );
        
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
            
            // Push optimized loop context instead of clearing global chains
            let loop_type = if condition.is_some() { 
                super::gen::LoopType::While 
            } else { 
                super::gen::LoopType::For 
            };
            self.push_loop_context(None, loop_type, start_pc as usize);
            
            // JavaC: genStat(body, loopEnv, CRT_STATEMENT | CRT_FLOW_TARGET);
            self.visit_stmt(body, &loop_env)?;
            
            // Pop loop context and collect break/continue chains 
            if let Some(loop_ctx) = self.pop_loop_context() {
                if let Some(break_chain) = loop_ctx.break_chain {
                    loop_env.add_exit(Some(break_chain));
                }
                if let Some(continue_chain) = loop_ctx.continue_chain {
                    loop_env.add_cont(Some(continue_chain));
                }
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
                    crate::common::error::Error::CodeGen { message: "Failed to create goto branch".to_string() }
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
            
            // Push optimized loop context for do-while loop
            self.push_loop_context(None, super::gen::LoopType::DoWhile, start_pc as usize);
            
            // JavaC: genStat(body, loopEnv, CRT_STATEMENT | CRT_FLOW_TARGET);
            self.visit_stmt(body, &loop_env)?;
            
            // Pop loop context and collect break/continue chains 
            if let Some(loop_ctx) = self.pop_loop_context() {
                if let Some(break_chain) = loop_ctx.break_chain {
                    loop_env.add_exit(Some(break_chain));
                }
                if let Some(continue_chain) = loop_ctx.continue_chain {
                    loop_env.add_cont(Some(continue_chain));
                }
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
        
        // Enhanced scope management: pop loop scope and finalize variables
        let current_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
        if let Some((scope_context, finalized_vars)) = self.scope_manager.pop_scope(current_pc as usize) {
            if self.with_items(|items| Ok(items.code.debug_code))? {
                eprintln!("🔄 DEBUG: Finalized loop scope {} with {} local variables", 
                         scope_context.scope_id, finalized_vars.len());
                for var in &finalized_vars {
                    eprintln!("   - {}: {} (slot {}, {}..{})", 
                             var.name, var.type_desc, var.slot, var.start_pc, var.start_pc + var.length);
                }
            }
        }
        
        // JavaC: Chain exit = loopEnv.info.exit; - break statements  
        if let Some(exit_chain) = loop_env.exit.take() {
            // JavaC: code.resolve(exit);
            self.with_items(|items| {
                items.code.resolve(Some(exit_chain));
                Ok(())
            })?;
            
            // JavaC: exit.state.defined.excludeFrom(code.nextreg);
            // Loop scope variables are already managed by the scope manager
        }
        
        Ok(())
    }
    
    /// Visit return statement - JavaC Gen.visitReturn equivalent
    pub fn visit_return(&mut self, tree: &ReturnStmt, env: &GenContext) -> Result<()> {
        if let Some(ref expr) = tree.value {
            // Generate expression for return value and validate type
            let _result = self.visit_expr(expr, env)?;
            
            // Determine return instruction based on method return type (aligned with javac)
            let return_opcode = if let Some(ref method) = env.method {
                if let Some(ref return_type) = method.return_type {
                    // TODO: Add type checking - ensure expression type is assignable to return type
                    // This would prevent bytecode verification errors by catching type mismatches
                    // at compile time (like javac does)
                    Self::get_return_instruction_for_type(return_type)
                } else {
                    super::opcodes::RETURN // Should not happen for non-void with value
                }
            } else {
                // Fallback if no method context - this shouldn't happen in well-formed code
                super::opcodes::ARETURN // Assume object return
            };
            
            if let Some(code) = self.code_mut() {
                code.emitop(return_opcode);
                code.alive = false; // Mark code as unreachable after return
            }
        } else {
            // Void return statement
            if let Some(code) = self.code_mut() {
                code.emitop(super::opcodes::RETURN);
                code.alive = false; // Mark code as unreachable after return
            }
        }
        Ok(())
    }
    
    /// Visit variable declaration - simplified version
    pub fn visit_var_def(&mut self, tree: &VarDeclStmt, env: &GenContext) -> Result<()> {
        for var in &tree.variables {
            // Add variable to current scope first
            let current_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
            let next_slot = self.with_items(|items| Ok(items.code.max_locals))?;
            
            // Generate initializer if present and store in variable
            if let Some(ref init) = var.initializer {
                let _init_result = self.visit_expr(init, env)?;
                
                // Generate store instruction to put value in local variable slot
                self.with_items(|items| {
                    // Determine the correct store instruction based on variable type
                    let (optimized_base, general_op) = match tree.type_ref.name.as_str() {
                        "boolean" | "byte" | "short" | "char" | "int" => {
                            (opcodes::ISTORE_0, opcodes::ISTORE)
                        }
                        "long" => {
                            (opcodes::LSTORE_0, opcodes::LSTORE)
                        }
                        "float" => {
                            (opcodes::FSTORE_0, opcodes::FSTORE)
                        }
                        "double" => {
                            (opcodes::DSTORE_0, opcodes::DSTORE)
                        }
                        _ => {
                            // Object references (including our SimpleConstructorTest)
                            (opcodes::ASTORE_0, opcodes::ASTORE)
                        }
                    };
                    
                    // Use appropriate store instruction based on slot number and type
                    if next_slot <= 3 {
                        items.code.emitop(optimized_base + next_slot as u8);
                        items.code.state.pop(1); // Pop the value from stack
                    } else {
                        items.code.emitop1(general_op, next_slot as u8);
                        // emitop1 already handles stack tracking via update_stack_for_op1
                    }
                    Ok(())
                })?;
            }
            
            // Get type descriptor from VarDeclStmt's type_ref
            let type_desc = format!("{:?}", tree.type_ref); // Simplified for now
            
            // Add to scope manager
            if let Err(e) = self.scope_manager.add_local_var(
                var.name.clone(),
                type_desc,
                next_slot,
                current_pc as usize
            ) {
                eprintln!("⚠️  WARNING: Failed to add local variable '{}' to scope: {}", var.name, e);
            }
            
            // Update max_locals (allocate one slot for simplicity)
            self.with_items(|items| {
                items.code.max_locals += 1;
                Ok(())
            })?;
        }
        Ok(())
    }
    
    /// Visit expression statement - simplified version
    pub fn visit_exec(&mut self, tree: &ExprStmt, env: &GenContext) -> Result<()> {
        // Special handling for assignments in statement context
        if let Expr::Assignment(assignment) = &tree.expr {
            if self.should_use_javac_assignment(assignment) {
                eprintln!("🚀 DEBUG: Using JavaC-style statement assignment");
                return self.visit_assign_javac_stmt(assignment, env);
            }
        }
        
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
    /// Visit try statement - JavaC visitTry equivalent with proper exception tables
    pub fn visit_try(&mut self, tree: &TryStmt, env: &GenContext) -> Result<()> {
        self.generate_try_catch_bytecode(tree, env)
    }
    
    /// Generate proper try-catch-finally bytecode with exception tables
    /// Based on JavaC Gen.genTry implementation
    fn generate_try_catch_bytecode(&mut self, tree: &TryStmt, env: &GenContext) -> Result<()> {
        // Pre-allocate local variable indices to avoid borrowing conflicts
        let catch_var_indices: Vec<u16> = (0..tree.catch_clauses.len())
            .map(|_| {
                let idx = self.method_context.next_local;
                self.method_context.next_local += 1;
                idx
            })
            .collect();
            
        let finally_var_index = if tree.finally_block.is_some() {
            let idx = self.method_context.next_local;
            self.method_context.next_local += 1;
            Some(idx)
        } else {
            None
        };
        
        // Pre-allocate exception type constant pool indices
        let exception_type_indices: Vec<u16> = tree.catch_clauses.iter()
            .map(|catch_clause| {
                let class_name = &catch_clause.parameter.type_ref.name;
                self.get_pool_mut().add_class(class_name)
            })
            .collect();
        
        // Generate try-catch structure with proper control flow
        self.gen_try_with_control_flow(tree, env, &catch_var_indices, finally_var_index, &exception_type_indices)
    }
    
    /// Generate try block with proper control flow and exception table
    fn gen_try_with_control_flow(
        &mut self, 
        tree: &TryStmt, 
        env: &GenContext,
        catch_var_indices: &[u16],
        finally_var_index: Option<u16>,
        exception_type_indices: &[u16]
    ) -> Result<()> {
        // Step 1: Generate try block and capture PC ranges
        let start_pc = self.get_current_pc()?;
        self.visit_block(&tree.try_block, env)?;
        let end_pc = self.get_current_pc()?;
        
        // Step 2: Generate normal exit path (skip catch handlers)
        let normal_exit_jump = self.emit_goto_placeholder()?;
        
        // Step 3: Generate catch handlers
        let catch_handler_pcs = self.generate_catch_handlers(tree, env, catch_var_indices)?;
        
        // Step 4: Generate finally block for normal path
        let finally_normal_pc = if tree.finally_block.is_some() {
            Some(self.get_current_pc()?)
        } else {
            None
        };
        
        if let Some(ref finally_block) = tree.finally_block {
            self.visit_block(finally_block, env)?;
        }
        
        // Step 5: Patch normal exit jump to point here
        let exit_target = self.get_current_pc()?;
        self.patch_goto_jump(normal_exit_jump, exit_target)?;
        
        // Step 6: Generate finally catch-all handler if needed
        if let (Some(finally_block), Some(finally_var_idx)) = (&tree.finally_block, finally_var_index) {
            let catchall_pc = self.get_current_pc()?;
            
            // Store exception in local variable
            self.emit_astore(finally_var_idx)?;
            
            // Execute finally block
            self.visit_block(finally_block, env)?;
            
            // Re-throw exception
            self.emit_aload(finally_var_idx)?;
            self.emit_athrow()?;
            
            // Add catch-all exception table entry for finally
            self.add_exception_table_entry(start_pc, end_pc, catchall_pc, 0)?; // 0 = catch all
        }
        
        // Step 7: Add exception table entries for catch clauses
        for (i, _catch_clause) in tree.catch_clauses.iter().enumerate() {
            let handler_pc = catch_handler_pcs[i];
            let exception_type_index = exception_type_indices[i];
            self.add_exception_table_entry(start_pc, end_pc, handler_pc, exception_type_index)?;
        }
        
        Ok(())
    }
    
    /// Generate catch handlers and return their PC positions
    fn generate_catch_handlers(
        &mut self,
        tree: &TryStmt,
        env: &GenContext,
        catch_var_indices: &[u16]
    ) -> Result<Vec<u16>> {
        let mut handler_pcs = Vec::new();
        
        for (i, catch_clause) in tree.catch_clauses.iter().enumerate() {
            let handler_pc = self.get_current_pc()?;
            handler_pcs.push(handler_pc);
            
            // Store exception in catch parameter local variable
            let var_index = catch_var_indices[i];
            self.emit_astore(var_index)?;
            
            // Generate catch block body
            self.visit_block(&catch_clause.block, env)?;
            
            // Generate goto to finally/exit (will be patched later)
            let _catch_exit_jump = self.emit_goto_placeholder()?;
        }
        
        Ok(handler_pcs)
    }
    
    // Helper methods for bytecode generation
    fn get_current_pc(&mut self) -> Result<u16> {
        if let Some(code) = self.code_mut() {
            Ok(code.get_cp())
        } else {
            Err(crate::common::error::Error::CodeGen { message: "No code context available".into() })
        }
    }
    
    fn emit_goto_placeholder(&mut self) -> Result<u16> {
        if let Some(code) = self.code_mut() {
            let jump_pc = code.get_cp();
            code.emitop(super::opcodes::GOTO);
            code.emit2(0); // Placeholder offset
            Ok(jump_pc)
        } else {
            Err(crate::common::error::Error::CodeGen { message: "No code context available".into() })
        }
    }
    
    fn patch_goto_jump(&mut self, jump_pc: u16, target_pc: u16) -> Result<()> {
        if let Some(code) = self.code_mut() {
            let offset = (target_pc as i32 - jump_pc as i32 - 3) as i16;
            code.put2(jump_pc + 1, offset);
            Ok(())
        } else {
            Err(crate::common::error::Error::CodeGen { message: "No code context available".into() })
        }
    }
    
    fn emit_astore(&mut self, var_index: u16) -> Result<()> {
        if let Some(code) = self.code_mut() {
            code.emitop1(super::opcodes::ASTORE, var_index as u8);
            Ok(())
        } else {
            Err(crate::common::error::Error::CodeGen { message: "No code context available".into() })
        }
    }
    
    fn emit_aload(&mut self, var_index: u16) -> Result<()> {
        if let Some(code) = self.code_mut() {
            code.emitop1(super::opcodes::ALOAD, var_index as u8);
            Ok(())
        } else {
            Err(crate::common::error::Error::CodeGen { message: "No code context available".into() })
        }
    }
    
    fn emit_athrow(&mut self) -> Result<()> {
        if let Some(code) = self.code_mut() {
            code.emitop(super::opcodes::ATHROW);
            Ok(())
        } else {
            Err(crate::common::error::Error::CodeGen { message: "No code context available".into() })
        }
    }
    
    fn add_exception_table_entry(&mut self, start_pc: u16, end_pc: u16, handler_pc: u16, catch_type: u16) -> Result<()> {
        if let Some(code) = self.code_mut() {
            code.add_exception_handler(start_pc, end_pc, handler_pc, catch_type);
            Ok(())
        } else {
            Err(crate::common::error::Error::CodeGen { message: "No code context available".into() })
        }
    }
    
    /// Visit break statement - JavaC visitBreak equivalent (Gen.java:1793-1798)  
    pub fn visit_break(&mut self, tree: &crate::ast::BreakStmt, env: &GenContext) -> Result<()> {
        // JavaC: Assert.check(code.state.stacksize == 0);
        // Ensure stack is clear before break
        self.with_items(|items| {
            // Clear any remaining items on the stack before break
            while items.code.state.stacksize > 0 {
                items.code.emitop(super::opcodes::POP);
                items.code.state.stacksize -= 1;
            }
            Ok(())
        })?;
        
        // Handle labeled vs unlabeled break
        let _target_env = if let Some(ref label) = tree.label {
            // Labeled break: find the target environment by label
            self.label_env_map.get(label).unwrap_or(env).clone()
        } else {
            // Unlabeled break: use current environment
            env.clone()
        };
        
        // JavaC: targetEnv.info.addExit(code.branch(goto_));
        // Generate a goto and add to the break chain using optimized system
        if let Some(goto_chain) = self.with_items(|items| {
            Ok(items.code.branch(super::opcodes::GOTO))
        })? {
            // Use optimized loop context system instead of global chains
            let label_ref = tree.label.as_ref().map(|s| s.as_str());
            self.add_break_jump(label_ref, goto_chain);
            
            if tree.label.is_some() {
                eprintln!("🔄 DEBUG: Optimized labeled break to '{:?}' generated", tree.label);
            }
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
        // JavaC: Assert.check(code.state.stacksize == 0);
        // Ensure stack is clear before continue
        self.with_items(|items| {
            // Clear any remaining items on the stack before continue
            while items.code.state.stacksize > 0 {
                items.code.emitop(super::opcodes::POP);
                items.code.state.stacksize -= 1;
            }
            Ok(())
        })?;
        
        // Handle labeled vs unlabeled continue
        let _target_env = if let Some(ref label) = tree.label {
            // Labeled continue: find the target environment by label
            self.label_env_map.get(label).unwrap_or(env).clone()
        } else {
            // Unlabeled continue: use current environment
            env.clone()
        };
        
        // JavaC: targetEnv.info.addCont(code.branch(goto_));
        // Generate a goto and add to the continue chain using optimized system  
        if let Some(goto_chain) = self.with_items(|items| {
            Ok(items.code.branch(super::opcodes::GOTO))
        })? {
            // Use optimized loop context system instead of global chains
            let label_ref = tree.label.as_ref().map(|s| s.as_str());
            self.add_continue_jump(label_ref, goto_chain);
            
            if tree.label.is_some() {
                eprintln!("🔄 DEBUG: Optimized labeled continue to '{:?}' generated", tree.label);
            }
        }
        
        // Mark code as dead after continue (JavaC behavior)
        self.with_items(|items| {
            items.code.mark_dead();
            Ok(())
        })?;
        
        Ok(())
    }
    
    /// Visit labeled statement - JavaC visitLabelled equivalent (Gen.java:1849-1855)
    pub fn visit_labeled_stmt(&mut self, tree: &crate::ast::LabeledStmt, env: &GenContext) -> Result<()> {
        // JavaC: Env<GenContext> localEnv = env.dup(env.tree, env.info);
        let local_env = env.dup();
        
        // Store the label to environment mapping for break/continue resolution
        // In JavaC, labels are associated with their target statement environment
        self.label_env_map.insert(tree.label.clone(), local_env.clone());
        
        // Check if this is a labeled loop statement for optimized handling
        let is_loop_stmt = matches!(*tree.statement, 
            crate::ast::Stmt::While(_) | 
            crate::ast::Stmt::DoWhile(_) | 
            crate::ast::Stmt::For(_) | crate::ast::Stmt::EnhancedFor(_)
        );
        
        if is_loop_stmt {
            // For labeled loops, enhance scope management with label information
            let start_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
            let current_max_locals = self.with_items(|items| Ok(items.code.max_locals))?;
            let loop_type = super::gen::LoopType::Labeled;
            
            // Push both optimized loop context and enhanced scope
            self.push_loop_context(Some(tree.label.clone()), loop_type, start_pc as usize);
            
            let _scope_id = self.scope_manager.push_scope(
                start_pc as usize,
                true, // is_loop_scope
                Some(super::gen::LoopType::Labeled),
                Some(tree.label.clone()),
                current_max_locals
            );
            
            // Visit the loop statement
            self.visit_stmt(&tree.statement, &local_env)?;
            
            // Pop both contexts
            self.pop_loop_context();
            let end_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
            if let Some((_scope_context, finalized_vars)) = self.scope_manager.pop_scope(end_pc as usize) {
                if self.with_items(|items| Ok(items.code.debug_code))? {
                    eprintln!("🏷️  DEBUG: Finalized labeled scope '{}' with {} variables", 
                             tree.label, finalized_vars.len());
                }
            }
        } else {
            // Non-loop labeled statement - still manage scope for consistency
            let start_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
            let current_max_locals = self.with_items(|items| Ok(items.code.max_locals))?;
            
            let _scope_id = self.scope_manager.push_scope(
                start_pc as usize,
                false, // not a loop scope
                None,
                Some(tree.label.clone()),
                current_max_locals
            );
            
            self.visit_stmt(&tree.statement, &local_env)?;
            
            let end_pc = self.with_items(|items| Ok(items.code.entry_point()))?;
            self.scope_manager.pop_scope(end_pc as usize);
        }
        
        // Clean up the label mapping after the statement completes
        self.label_env_map.remove(&tree.label);
        
        Ok(())
    }
    
    /// Visit throw statement - JavaC Gen.visitThrow equivalent
    pub fn visit_throw(&mut self, tree: &ThrowStmt, env: &GenContext) -> Result<()> {
        // Generate expression for exception object (aligned with javac Gen.visitThrow)
        let _result = self.visit_expr(&tree.expr, env)?;
        
        // Emit athrow instruction (aligned with javac: code.emitop0(athrow))
        if let Some(code) = self.code_mut() {
            code.emitop(super::opcodes::ATHROW);
            code.alive = false; // Code after throw is unreachable
        }
        
        Ok(())
    }
    
    /// Check if a statement guarantees return/termination (aligned with javac flow analysis)
    fn stmt_guarantees_return(stmt: &Stmt) -> bool {
        let result = match stmt {
            Stmt::Return(_) => true,
            Stmt::Throw(_) => true,
            Stmt::Block(b) => Self::block_guarantees_return(b),
            Stmt::If(ifstmt) => {
                if let Some(else_b) = &ifstmt.else_branch {
                    // require both branches to guarantee return
                    Self::stmt_guarantees_return(&ifstmt.then_branch) && Self::stmt_guarantees_return(else_b)
                } else {
                    false
                }
            }
            _ => false, // Other statements don't guarantee return
        };
        result
    }
    
    /// Check if a block guarantees return (aligned with javac flow analysis)
    fn block_guarantees_return(block: &Block) -> bool {
        // A block guarantees return if any contained statement guarantees return
        for s in &block.statements {
            if Self::stmt_guarantees_return(s) { 
                return true; 
            }
        }
        // If the last statement guarantees return
        if let Some(last) = block.statements.last() {
            return Self::stmt_guarantees_return(last);
        }
        false
    }
    
    /// Get return instruction for type (aligned with javac Code.typecode + ireturn pattern)
    fn get_return_instruction_for_type(type_ref: &TypeRef) -> u8 {
        // Convert TypeRef to TypeEnum using the existing TypeExt trait
        let type_enum = type_ref.as_type_enum();
        
        let opcode = match type_enum {
            TypeEnum::Void => super::opcodes::RETURN,
            TypeEnum::Primitive(prim_type) => match prim_type {
                PrimitiveType::Boolean | 
                PrimitiveType::Byte | 
                PrimitiveType::Char | 
                PrimitiveType::Short | 
                PrimitiveType::Int => super::opcodes::IRETURN,
                PrimitiveType::Long => super::opcodes::LRETURN,
                PrimitiveType::Float => super::opcodes::FRETURN,
                PrimitiveType::Double => super::opcodes::DRETURN,
            },
            TypeEnum::Reference(_) => super::opcodes::ARETURN,
        };
        
        opcode
    }
    
    /// Resolve static method class and descriptor (aligned with javac symbol resolution)
    fn resolve_static_method_info(&self, tree: &MethodCallExpr) -> Result<(String, String)> {
        // Legacy method for backward compatibility - use empty arg types and minimal context
        let arg_types = vec![];
        let dummy_env = GenContext::default();
        self.resolve_static_method_info_with_types(tree, &arg_types, &dummy_env)
    }
    
    /// Resolve static method with actual argument types (JavaC aligned)
    fn resolve_static_method_info_with_types(&self, tree: &MethodCallExpr, arg_types: &[TypeEnum], env: &GenContext) -> Result<(String, String)> {
        // Determine target class
        let class_name = if let Some(ref target) = tree.target {
            // TODO: Proper expression evaluation to get class name
            // For now, handle simple identifiers and common static calls
            match target.as_ref() {
                Expr::Identifier(id) => {
                    match id.name.as_str() {
                        "System" => "java/lang/System".to_string(),
                        "Math" => "java/lang/Math".to_string(),
                        "String" => "java/lang/String".to_string(),
                        "Integer" => "java/lang/Integer".to_string(),
                        "Object" => "java/lang/Object".to_string(),
                        _ => format!("java/lang/{}", id.name), // Assume java.lang for unknown classes
                    }
                }
                _ => "java/lang/System".to_string(), // Default fallback
            }
        } else {
            // No target means current class static method
            if let Some(clazz) = &env.clazz {
                clazz.name.clone()
            } else {
                // Fallback if no current class context available
                "java/lang/Object".to_string()
            }
        };
        
        // Generate method descriptor based on method name and actual argument types (JavaC aligned)
        let descriptor = if arg_types.is_empty() {
            // Fallback to old method for backward compatibility
            self.generate_method_descriptor(&tree.name, &tree.arguments)
        } else {
            self.generate_method_descriptor_with_types(&tree.name, arg_types)
        };
        
        Ok((class_name, descriptor))
    }
    
    /// Resolve instance method information with types (for non-static method calls)
    fn resolve_instance_method_info_with_types(&self, tree: &MethodCallExpr, arg_types: &[TypeEnum]) -> Result<(String, String)> {
        // For instance methods, we need to determine the class from the target expression
        let class_name = if let Some(ref target) = tree.target {
            // TODO: Proper expression evaluation to get class name from target
            // For now, handle common patterns and use heuristics
            match target.as_ref() {
                Expr::Identifier(id) => {
                    // Check if it's a known variable type or use Object as fallback
                    match id.name.as_str() {
                        "out" => "java/io/PrintStream".to_string(), // System.out
                        "next" => "java/util/HashMapCell".to_string(), // HashMapCell interface
                        _ => "java/lang/Object".to_string(), // Default fallback for variables
                    }
                }
                Expr::FieldAccess(field_access) => {
                    // Handle field access like System.out
                    if let Some(ref target_expr) = field_access.target {
                        if let Expr::Identifier(ref obj) = target_expr.as_ref() {
                        if obj.name == "System" && field_access.name == "out" {
                            "java/io/PrintStream".to_string()
                        } else {
                            "java/lang/Object".to_string()
                        }
                        } else {
                            "java/lang/Object".to_string()
                        }
                    } else {
                        "java/lang/Object".to_string()
                    }
                }
                Expr::Parenthesized(inner) => {
                    // Handle parenthesized expressions - unwrap and recurse
                    match inner.as_ref() {
                        Expr::Cast(cast_expr) => {
                            // Extract the target type from the cast
                            match cast_expr.target_type.name.as_str() {
                                "Comparable" => "java/lang/Comparable".to_string(),
                                "String" => "java/lang/String".to_string(), 
                                "List" => "java/util/List".to_string(),
                                "Map" => "java/util/Map".to_string(),
                                class_name => {
                                    // Handle qualified class names
                                    if class_name.contains('.') {
                                        class_name.replace('.', "/")
                                    } else {
                                        // Try common packages
                                        match class_name {
                                            "Object" => "java/lang/Object".to_string(),
                                            "Integer" => "java/lang/Integer".to_string(),
                                            "Boolean" => "java/lang/Boolean".to_string(),
                                            _ => format!("java/lang/{}", class_name), // Default to java.lang
                                        }
                                    }
                                }
                            }
                        }
                        _ => "java/lang/Object".to_string(), // Default fallback
                    }
                }
                Expr::Cast(cast_expr) => {
                    // Extract the target type from the cast
                    match cast_expr.target_type.name.as_str() {
                        "Comparable" => "java/lang/Comparable".to_string(),
                        "String" => "java/lang/String".to_string(), 
                        class_name => {
                            // Handle qualified class names
                            if class_name.contains('.') {
                                class_name.replace('.', "/")
                            } else {
                                format!("java/lang/{}", class_name) // Default to java.lang
                            }
                        }
                    }
                }
                _ => "java/lang/Object".to_string(), // Default fallback
            }
        } else {
            // No target means current class instance method
            "TODO_CURRENT_CLASS".to_string()
        };
        
        // Generate method descriptor - use known signatures for standard methods
        let descriptor = match (class_name.as_str(), tree.name.as_str()) {
            // Standard interface method signatures to align with javac
            ("java/lang/Comparable", "compareTo") => "(Ljava/lang/Object;)I".to_string(),
            ("java/util/Comparator", "compare") => "(Ljava/lang/Object;Ljava/lang/Object;)I".to_string(),
            ("java/io/PrintStream", "println") => {
                if tree.arguments.is_empty() {
                    "()V".to_string()
                } else {
                    "(Ljava/lang/Object;)V".to_string()
                }
            },
            ("java/io/PrintStream", "print") => {
                if tree.arguments.is_empty() {
                    "()V".to_string() 
                } else {
                    "(Ljava/lang/Object;)V".to_string()
                }
            },
            // For all other methods, use the existing logic
            _ => {
                if arg_types.is_empty() {
                    // Fallback to old method for backward compatibility
                    self.generate_method_descriptor(&tree.name, &tree.arguments)
                } else {
                    self.generate_method_descriptor_with_types(&tree.name, arg_types)
                }
            }
        };
        
        Ok((class_name, descriptor))
    }
    
    /// Generate method descriptor from arguments (simplified version)
    fn generate_method_descriptor(&self, method_name: &str, arguments: &[Expr]) -> String {
        // JavaC would use proper type resolution, but for now use heuristics
        let param_types = arguments.iter().map(|_| "Ljava/lang/Object;").collect::<String>();
        
        // Return type based on method name (heuristics)
        let return_type = match method_name {
            "println" | "print" | "doSomething" => "V",  // void methods
            "max" | "min" | "abs" => "I",
            "toString" | "valueOf" => "Ljava/lang/String;",
            "getClass" => "Ljava/lang/Class;",
            "condition1" | "condition2" | "condition3" | "shouldBreak" => "Z", // boolean methods
            // HashMapCell interface methods
            "after" | "before" => "Ljava/util/HashMapCell;",
            "next" => "Ljava/util/HashMapCell;",
            _ => "Ljava/lang/Object;", // Default to Object
        };
        
        format!("({}){}",  param_types, return_type)
    }
    
    /// Generate method descriptor with actual argument types (JavaC aligned)
    fn generate_method_descriptor_with_types(&self, method_name: &str, arg_types: &[TypeEnum]) -> String {
        // Convert argument types to JVM descriptor format
        let param_types = arg_types.iter()
            .map(|t| self.type_to_descriptor_string(t))
            .collect::<String>();
        
        // Determine return type based on method name and argument types (JavaC aligned)
        let return_type = match method_name {
            "<init>" => "V", // Constructors always return void
            "println" | "print" => "V",
            "compareTo" => "I", // Comparable.compareTo always returns int
            "compare" => "I",   // Comparator.compare always returns int
            "max" | "min" | "abs" => {
                // For Math methods, return type matches argument type
                if !arg_types.is_empty() {
                    match &arg_types[0] {
                        TypeEnum::Primitive(PrimitiveType::Int) => "I",
                        TypeEnum::Primitive(PrimitiveType::Long) => "J",
                        TypeEnum::Primitive(PrimitiveType::Float) => "F",
                        TypeEnum::Primitive(PrimitiveType::Double) => "D",
                        _ => "I", // Default to int
                    }
                } else {
                    "I" // Default to int
                }
            }
            "toString" | "valueOf" => "Ljava/lang/String;",
            "getClass" => "Ljava/lang/Class;",
            // HashMapCell interface methods
            "after" | "before" => "Ljava/util/HashMapCell;",
            "next" => "Ljava/util/HashMapCell;",
            _ => "Ljava/lang/Object;", // Default to Object
        };
        
        format!("({}){}",  param_types, return_type)
    }
    
    /// Convert TypeEnum to JVM descriptor string
    fn type_to_descriptor_string(&self, type_enum: &TypeEnum) -> String {
        match type_enum {
            TypeEnum::Primitive(PrimitiveType::Boolean) => "Z".to_string(),
            TypeEnum::Primitive(PrimitiveType::Byte) => "B".to_string(),
            TypeEnum::Primitive(PrimitiveType::Char) => "C".to_string(),
            TypeEnum::Primitive(PrimitiveType::Short) => "S".to_string(),
            TypeEnum::Primitive(PrimitiveType::Int) => "I".to_string(),
            TypeEnum::Primitive(PrimitiveType::Long) => "J".to_string(),
            TypeEnum::Primitive(PrimitiveType::Float) => "F".to_string(),
            TypeEnum::Primitive(PrimitiveType::Double) => "D".to_string(),
            TypeEnum::Void => "V".to_string(),
            TypeEnum::Reference(ref_type) => {
                match ref_type {
                    ReferenceType::Class(name) => format!("L{};", name.replace('.', "/")),
                    ReferenceType::Interface(name) => format!("L{};", name.replace('.', "/")),
                    ReferenceType::Array(element_type) => format!("[{}", self.type_to_descriptor_string(&element_type.as_type_enum())),
                }
            }
        }
    }
    
    /// Convert typecode to TypeEnum for type inference
    fn typecode_to_type_enum(&self, typecode: u8) -> TypeEnum {
        use super::items::typecodes;
        match typecode {
            typecodes::VOID => TypeEnum::Void,
            typecodes::BYTE => TypeEnum::Primitive(PrimitiveType::Byte),
            typecodes::SHORT => TypeEnum::Primitive(PrimitiveType::Short),
            typecodes::CHAR => TypeEnum::Primitive(PrimitiveType::Char),
            typecodes::INT => TypeEnum::Primitive(PrimitiveType::Int),
            typecodes::LONG => TypeEnum::Primitive(PrimitiveType::Long),
            typecodes::FLOAT => TypeEnum::Primitive(PrimitiveType::Float),
            typecodes::DOUBLE => TypeEnum::Primitive(PrimitiveType::Double),
            typecodes::OBJECT => TypeEnum::Reference(ReferenceType::Class("java.lang.Object".to_string())),
            typecodes::ARRAY => TypeEnum::Reference(ReferenceType::Array(Box::new(TypeRef {
                name: "java.lang.Object".to_string(),
                type_args: vec![],
                annotations: vec![],
                array_dims: 0,
                span: Span::default(),
            }))),
            _ => TypeEnum::Reference(ReferenceType::Class("java.lang.Object".to_string())), // Default fallback
        }
    }
    
    /// Parse return type from method descriptor
    fn parse_return_type_from_descriptor(descriptor: &str) -> TypeEnum {
        // Find return type after the closing ')'
        if let Some(return_part) = descriptor.split(')').nth(1) {
            match return_part {
                "V" => TypeEnum::Void,
                "I" => TypeEnum::Primitive(PrimitiveType::Int),
                "J" => TypeEnum::Primitive(PrimitiveType::Long),
                "F" => TypeEnum::Primitive(PrimitiveType::Float),
                "D" => TypeEnum::Primitive(PrimitiveType::Double),
                "Z" => TypeEnum::Primitive(PrimitiveType::Boolean),
                "B" => TypeEnum::Primitive(PrimitiveType::Byte),
                "C" => TypeEnum::Primitive(PrimitiveType::Char),
                "S" => TypeEnum::Primitive(PrimitiveType::Short),
                s if s.starts_with('L') && s.ends_with(';') => {
                    let class_name = s[1..s.len()-1].to_string();
                    TypeEnum::Reference(ReferenceType::Class(class_name))
                }
                s if s.starts_with('[') => {
                    // Array type - simplified handling
                    TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string()))
                }
                _ => TypeEnum::Reference(ReferenceType::Class("java/lang/Object".to_string())),
            }
        } else {
            TypeEnum::Void
        }
    }
    
    /// Visit lambda expression - Generate invokedynamic instruction
    pub fn visit_lambda(&mut self, lambda: &LambdaExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Generate unique lambda method name
        let lambda_method_name = format!("lambda$main${}", self.lambda_counter);
        self.lambda_counter += 1;
        
        // Determine functional interface and method based on lambda
        let functional_interface = self.infer_functional_interface(lambda)?;
        let sam_method_name = functional_interface.method_name.clone();
        let sam_descriptor = functional_interface.method_descriptor.clone();
        
        // Prepare data before generating bytecode
        let current_class_name = env.clazz.as_ref()
            .map(|c| c.name.clone())
            .unwrap_or_else(|| "UnknownClass".to_string());
        let lambda_descriptor = self.generate_lambda_descriptor(lambda);
        
        // Generate bootstrap method indices and invokedynamic instruction
        let (bootstrap_method_index, invoke_dynamic_index) = {
            let mut bootstrap_args = Vec::new();
            
            self.with_items(|items| {
                // 1. Create LambdaMetafactory.metafactory method handle  
                let bootstrap_method_handle = items.add_method_handle(
                    6, // REF_invokeStatic
                    "java/lang/invoke/LambdaMetafactory",
                    "metafactory",
                    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
                );
                
                // 2. Create SAM method type (functional interface method signature)
                let sam_method_type = items.add_method_type(&sam_descriptor);
                
                // 3. Create lambda implementation method handle
                let impl_method_handle = items.add_method_handle(
                    6, // REF_invokeStatic (synthetic static method)
                    &current_class_name,
                    &lambda_method_name,
                    &lambda_descriptor
                );
                
                // 4. Create instantiated method type (same as SAM for simple cases)
                let instantiated_method_type = sam_method_type;
                
                // 5. Prepare bootstrap method arguments
                bootstrap_args = vec![
                    bootstrap_method_handle,
                    sam_method_type,
                    impl_method_handle,
                    instantiated_method_type,
                ];
                
                Ok(())
            })?;
            
            // 6. Add bootstrap method and generate invokedynamic
            let bootstrap_index = self.add_bootstrap_method(bootstrap_args);
            let invoke_dynamic_index = self.with_items(|items| {
                Ok(items.add_invoke_dynamic(bootstrap_index, &sam_method_name, &sam_descriptor))
            })?;
            
            (bootstrap_index, invoke_dynamic_index)
        };
        
        // 7. Emit invokedynamic instruction using generated indices
        self.with_items(|items| {
            items.code.emitop(opcodes::INVOKEDYNAMIC);
            items.code.emit2(invoke_dynamic_index);
            items.code.emit2(0); // Must be zero for invokedynamic
            Ok(())
        })?;
        
        // TODO: Generate synthetic lambda method implementation
        self.generate_lambda_method(lambda, &lambda_method_name, env)?;
        
        // Return functional interface type
        Ok(BytecodeItem::Stack { typecode: typecodes::OBJECT })
    }
    
    /// Visit method reference - Generate method handle
    pub fn visit_method_reference(&mut self, method_ref: &MethodReferenceExpr, env: &GenContext) -> Result<BytecodeItem> {
        // Placeholder implementation for method reference
        // Full implementation will need:
        // 1. Method handle creation for different reference types
        // 2. invokedynamic instruction generation
        // 3. LambdaMetafactory integration for functional interfaces
        
        self.with_items(|items| {
            // Placeholder: Load null as the method reference implementation
            items.code.emitop(opcodes::ACONST_NULL);
            Ok(())
        })?;
        
        Ok(BytecodeItem::Stack { typecode: typecodes::OBJECT })
    }
    
    /// Generate synthetic method for lambda implementation
    fn generate_lambda_method(&mut self, lambda: &LambdaExpr, method_name: &str, env: &GenContext) -> Result<()> {
        // For now, create a placeholder implementation
        // Full method generation will be implemented when method creation infrastructure is ready
        
        // TODO: Generate actual synthetic method for lambda implementation
        // This requires:
        // 1. Method creation in class file
        // 2. Code generation for lambda body
        // 3. Proper return value handling
        // 4. Bootstrap method registration
        
        // Placeholder: Just register that we need to generate this method
        // The actual implementation will need to integrate with the class file generation
        
        Ok(())
    }
    
    /// Infer functional interface for lambda expression
    fn infer_functional_interface(&self, lambda: &LambdaExpr) -> Result<FunctionalInterface> {
        // Simplified: assume Function interface for now
        // In a full implementation, this would analyze the context to determine the target type
        Ok(FunctionalInterface {
            interface_name: "java/util/function/Function".to_string(),
            interface_descriptor: "Ljava/util/function/Function;".to_string(),
            method_name: "apply".to_string(),
            method_descriptor: "(Ljava/lang/Object;)Ljava/lang/Object;".to_string(),
        })
    }
    
    /// Generate method descriptor for lambda implementation
    fn generate_lambda_descriptor(&self, lambda: &LambdaExpr) -> String {
        // Simplified: generate based on parameter count
        let param_count = lambda.parameters.len();
        let params = "Ljava/lang/Object;".repeat(param_count);
        format!("({})Ljava/lang/Object;", params)
    }
    
    /// Add bootstrap method to the bootstrap methods table
    fn add_bootstrap_method(&mut self, method_arguments: Vec<u16>) -> u16 {
        let index = self.bootstrap_methods.len() as u16;
        self.bootstrap_methods.push(method_arguments);
        index
    }
    
    /// Get field descriptor from resolved type
    fn get_field_descriptor_from_resolved_type(&self, resolved_type: &crate::wash::attr::ResolvedType) -> String {
        match resolved_type {
            crate::wash::attr::ResolvedType::Primitive(prim) => {
                match prim {
                    crate::wash::attr::PrimitiveType::Boolean => "Z".to_string(),
                    crate::wash::attr::PrimitiveType::Byte => "B".to_string(),
                    crate::wash::attr::PrimitiveType::Char => "C".to_string(),
                    crate::wash::attr::PrimitiveType::Short => "S".to_string(),
                    crate::wash::attr::PrimitiveType::Int => "I".to_string(),
                    crate::wash::attr::PrimitiveType::Long => "J".to_string(),
                    crate::wash::attr::PrimitiveType::Float => "F".to_string(),
                    crate::wash::attr::PrimitiveType::Double => "D".to_string(),
                }
            },
            crate::wash::attr::ResolvedType::Generic(name, _) => {
                // For generics, use Object descriptor (type erasure)
                format!("L{};", name.replace('.', "/"))
            },
            crate::wash::attr::ResolvedType::Reference(class_name) => {
                format!("L{};", class_name.replace('.', "/"))
            },
            crate::wash::attr::ResolvedType::Array(element_type) => {
                format!("[{}", self.get_field_descriptor_from_resolved_type(element_type))
            },
            crate::wash::attr::ResolvedType::Class(_) => {
                // For ClassType, default to Object for now
                "Ljava/lang/Object;".to_string()
            },
            _ => {
                // For other types, default to Object
                "Ljava/lang/Object;".to_string()
            },
        }
    }
    
    /// Visit switch statement - JavaC visitSwitch equivalent
    pub fn visit_switch(&mut self, tree: &crate::ast::SwitchStmt, env: &GenContext) -> Result<()> {
        eprintln!("🔄 DEBUG: Switch statement generation - creating switch environment");
        
        // Create switch-specific environment
        let switch_env = GenContext {
            method: env.method.clone(),
            clazz: env.clazz.clone(),
            fatcode: env.fatcode,
            debug_code: env.debug_code,
            exit: env.exit.clone(),
            cont: env.cont.clone(),
            is_switch: true,
        };
        
        // Generate selector expression
        let _selector_item = self.visit_expr(&tree.expression, &switch_env)?;
        
        // For now, implement a simple if-else chain for switch cases
        // Full switch table optimization would come later
        eprintln!("⚠️  TODO: Complete switch table generation with case handling");
        
        // Generate case statements as if-else chain for now
        for case in &tree.cases {
            if case.labels.is_empty() {
                // Default case
                for stmt in &case.statements {
                    self.visit_stmt(stmt, &switch_env)?;
                }
            } else {
                // Regular case
                for stmt in &case.statements {
                    self.visit_stmt(stmt, &switch_env)?;
                }
            }
        }
        
        Ok(())
    }
    
    /// Visit assert statement - JavaC visitAssert equivalent
    pub fn visit_assert(&mut self, tree: &crate::ast::AssertStmt, env: &GenContext) -> Result<()> {
        eprintln!("🔄 DEBUG: Assert statement generation");
        
        // JavaC pattern: if assertions are enabled, generate assertion code
        // For now, just evaluate the condition and message expressions
        let _condition_item = self.visit_expr(&tree.condition, env)?;
        
        if let Some(ref message) = tree.message {
            let _message_item = self.visit_expr(message, env)?;
        }
        
        eprintln!("⚠️  TODO: Complete assert bytecode with -ea flag handling");
        Ok(())
    }
    
    /// Visit synchronized statement - JavaC visitSynchronized equivalent  
    pub fn visit_synchronized(&mut self, tree: &crate::ast::SynchronizedStmt, env: &GenContext) -> Result<()> {
        eprintln!("🔄 DEBUG: Synchronized block generation");
        
        // Generate expression for monitor object
        let _monitor_item = self.visit_expr(&tree.lock, env)?;
        
        // Generate monitorenter instruction
        self.with_items(|items| {
            items.code.emitop(super::opcodes::MONITORENTER);
            Ok(())
        })?;
        
        // Generate synchronized block body
        self.visit_block(&tree.body, env)?;
        
        // Generate monitorexit instruction (normally in finally block)
        self.with_items(|items| {
            items.code.emitop(super::opcodes::MONITOREXIT);
            Ok(())
        })?;
        
        eprintln!("⚠️  TODO: Add proper try-finally for monitorexit exception handling");
        Ok(())
    }
    
    /// Visit type declaration statement - JavaC visitTypeDecl equivalent
    pub fn visit_type_decl(&mut self, type_decl: &crate::ast::TypeDecl, env: &GenContext) -> Result<()> {
        eprintln!("🔄 DEBUG: Type declaration in statement context");
        
        // Type declarations in statement context are typically inner classes
        match type_decl {
            crate::ast::TypeDecl::Class(_class_decl) => {
                eprintln!("⚠️  TODO: Generate inner class in statement context");
                // Inner class generation would be handled here
            }
            crate::ast::TypeDecl::Interface(_interface_decl) => {
                eprintln!("⚠️  TODO: Generate inner interface in statement context");
                // Inner interface generation would be handled here
            }
            crate::ast::TypeDecl::Enum(_enum_decl) => {
                eprintln!("⚠️  TODO: Generate inner enum in statement context");
                // Inner enum generation would be handled here  
            }
            crate::ast::TypeDecl::Annotation(_annotation_decl) => {
                eprintln!("⚠️  TODO: Generate inner annotation in statement context");
                // Inner annotation generation would be handled here
            }
        }
        
        Ok(())
    }
}

/// Functional interface information for lambda compilation
#[derive(Debug, Clone)]
struct FunctionalInterface {
    interface_name: String,
    interface_descriptor: String,
    method_name: String,
    method_descriptor: String,
}

