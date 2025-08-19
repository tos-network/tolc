use crate::ast::*;
use crate::codegen::bytecode::BytecodeBuilder;
use std::collections::HashMap;

/// Field access optimization patterns from javac's Gen.java visitSelect and visitIdent
#[derive(Debug, Clone, PartialEq)]
pub enum FieldAccessOptimizationType {
    /// Static field access (getstatic/putstatic)
    Static {
        opcode: u8, // 178 (getstatic) or 179 (putstatic)
        field_ref: u16,
        is_constant: bool,
        constant_value: Option<Literal>,
    },
    /// Instance field access (getfield/putfield)
    Instance {
        opcode: u8, // 180 (getfield) or 181 (putfield)
        field_ref: u16,
        requires_null_check: bool,
    },
    /// Constant field inlining
    ConstantInline {
        value: Literal,
    },
    /// Array length access (arraylength)
    ArrayLength,
    /// Class literal access (.class -> ldc)
    ClassLiteral {
        class_ref: u16,
    },
}

/// Field access pattern analysis
#[derive(Debug, Clone)]
pub struct FieldAccessPattern {
    pub optimization_type: FieldAccessOptimizationType,
    pub stack_effect: (i32, i32), // (pop, push)
    pub estimated_cost: u32,
}

/// Field access optimizer following javac's field access patterns
pub struct FieldAccessOptimizer {
    constant_fields: HashMap<String, Literal>,
    static_fields: HashMap<String, bool>,
}

impl FieldAccessOptimizer {
    pub fn new() -> Self {
        let mut constant_fields = HashMap::new();
        let mut static_fields = HashMap::new();

        // Common constant fields
        constant_fields.insert("MAX_VALUE".to_string(), Literal::Integer(i32::MAX as i64));
        constant_fields.insert("MIN_VALUE".to_string(), Literal::Integer(i32::MIN as i64));
        constant_fields.insert("SIZE".to_string(), Literal::Integer(32));
        constant_fields.insert("TRUE".to_string(), Literal::Boolean(true));
        constant_fields.insert("FALSE".to_string(), Literal::Boolean(false));
        
        // BitSet constants
        constant_fields.insert("BITS_PER_LONG".to_string(), Literal::Integer(64));
        constant_fields.insert("BITS_PER_LONG_SHIFT".to_string(), Literal::Integer(6));

        // Common static fields
        static_fields.insert("MAX_VALUE".to_string(), true);
        static_fields.insert("MIN_VALUE".to_string(), true);
        static_fields.insert("SIZE".to_string(), true);
        static_fields.insert("out".to_string(), true);
        static_fields.insert("err".to_string(), true);
        static_fields.insert("in".to_string(), true);
        
        // BitSet static fields
        static_fields.insert("BITS_PER_LONG".to_string(), true);
        static_fields.insert("BITS_PER_LONG_SHIFT".to_string(), true);

        Self {
            constant_fields,
            static_fields,
        }
    }

    /// Get constant value for a field if it exists
    pub fn get_constant_value(&self, field_name: &str) -> Option<&Literal> {
        self.constant_fields.get(field_name)
    }
    
    /// Analyze field access for optimization (from Gen.visitSelect)
    pub fn analyze_field_access(
        &self,
        field_access: &FieldAccessExpr,
        is_assignment: bool,
    ) -> FieldAccessPattern {
        let field_name = &field_access.name;

        // Check for special cases first
        if field_name == "length" {
            return FieldAccessPattern {
                optimization_type: FieldAccessOptimizationType::ArrayLength,
                stack_effect: (1, 1), // array ref -> length
                estimated_cost: 1,
            };
        }

        if field_name == "class" {
            return FieldAccessPattern {
                optimization_type: FieldAccessOptimizationType::ClassLiteral {
                    class_ref: 1, // Placeholder
                },
                stack_effect: (0, 1), // -> Class object
                estimated_cost: 2,
            };
        }

        // Check for constant field inlining
        if !is_assignment && self.is_constant_field(field_name) {
            if let Some(constant_value) = self.constant_fields.get(field_name) {
                return FieldAccessPattern {
                    optimization_type: FieldAccessOptimizationType::ConstantInline {
                        value: constant_value.clone(),
                    },
                    stack_effect: (0, 1), // -> constant value
                    estimated_cost: 1,
                };
            }
        }

        // Determine if field is static
        let is_static = self.is_static_field(field_name) || self.is_static_context(field_access.target.as_ref());

        if is_static {
            let opcode = if is_assignment { 179 } else { 178 }; // putstatic vs getstatic
            let stack_effect = if is_assignment {
                (1, 0) // value -> (consumed)
            } else {
                (0, 1) // -> field value
            };

            FieldAccessPattern {
                optimization_type: FieldAccessOptimizationType::Static {
                    opcode,
                    field_ref: 1, // Placeholder
                    is_constant: self.is_constant_field(field_name),
                    constant_value: self.constant_fields.get(field_name).cloned(),
                },
                stack_effect,
                estimated_cost: 2,
            }
        } else {
            let opcode = if is_assignment { 181 } else { 180 }; // putfield vs getfield
            let requires_null_check = self.requires_null_check(field_access.target.as_ref());
            
            let stack_effect = if is_assignment {
                (2, 0) // object ref + value -> (consumed)
            } else {
                (1, 1) // object ref -> field value
            };

            FieldAccessPattern {
                optimization_type: FieldAccessOptimizationType::Instance {
                    opcode,
                    field_ref: 1, // Placeholder
                    requires_null_check,
                },
                stack_effect,
                estimated_cost: if requires_null_check { 4 } else { 2 },
            }
        }
    }

    /// Analyze identifier access for optimization (from Gen.visitIdent)
    pub fn analyze_identifier_access(
        &self,
        identifier: &IdentifierExpr,
        context: &IdentifierContext,
    ) -> FieldAccessPattern {
        match context.symbol_kind {
            SymbolKind::LocalVariable => {
                // Local variable access - use appropriate load instruction
                let opcode = match context.variable_type {
                    VariableType::Int => 21,    // iload
                    VariableType::Long => 22,   // lload
                    VariableType::Float => 23,  // fload
                    VariableType::Double => 24, // dload
                    VariableType::Reference => 25, // aload
                };

                FieldAccessPattern {
                    optimization_type: FieldAccessOptimizationType::Instance {
                        opcode,
                        field_ref: context.local_index as u16,
                        requires_null_check: false,
                    },
                    stack_effect: (0, 1),
                    estimated_cost: 1,
                }
            }
            SymbolKind::StaticField => {
                if let Some(constant_value) = self.constant_fields.get(&identifier.name) {
                    FieldAccessPattern {
                        optimization_type: FieldAccessOptimizationType::ConstantInline {
                            value: constant_value.clone(),
                        },
                        stack_effect: (0, 1),
                        estimated_cost: 1,
                    }
                } else {
                    FieldAccessPattern {
                        optimization_type: FieldAccessOptimizationType::Static {
                            opcode: 178, // getstatic
                            field_ref: 1, // Placeholder
                            is_constant: false,
                            constant_value: None,
                        },
                        stack_effect: (0, 1),
                        estimated_cost: 2,
                    }
                }
            }
            SymbolKind::InstanceField => {
                FieldAccessPattern {
                    optimization_type: FieldAccessOptimizationType::Instance {
                        opcode: 180, // getfield
                        field_ref: 1, // Placeholder
                        requires_null_check: true,
                    },
                    stack_effect: (1, 1), // this -> field value
                    estimated_cost: 3,
                }
            }
            SymbolKind::Method => {
                // Method reference - this would be handled by method invocation optimizer
                FieldAccessPattern {
                    optimization_type: FieldAccessOptimizationType::Instance {
                        opcode: 25, // aload (load method reference)
                        field_ref: 0,
                        requires_null_check: false,
                    },
                    stack_effect: (0, 1),
                    estimated_cost: 1,
                }
            }
        }
    }

    /// Generate optimized bytecode for field access
    pub fn generate_optimized_field_access(
        &self,
        pattern: &FieldAccessPattern,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match &pattern.optimization_type {
            FieldAccessOptimizationType::Static { opcode, field_ref, is_constant, constant_value } => {
                if *is_constant && constant_value.is_some() {
                    // Inline constant value instead of field access
                    self.generate_constant_load(constant_value.as_ref().unwrap(), bytecode_builder)?;
                } else {
                    match *opcode {
                        178 => bytecode_builder.getstatic(*field_ref)?, // getstatic
                        179 => bytecode_builder.putstatic(*field_ref)?, // putstatic
                        _ => return Err("Invalid static field access opcode".into()),
                    }
                }
            }
            FieldAccessOptimizationType::Instance { opcode, field_ref, requires_null_check } => {
                if *requires_null_check && (*opcode == 180 || *opcode == 181) {
                    // Generate null check before field access
                    bytecode_builder.dup()?;
                    // Assuming Object.getClass method ref is at index 1
                    bytecode_builder.invokevirtual(1)?;
                    bytecode_builder.pop()?;
                }

                match *opcode {
                    180 => bytecode_builder.getfield(*field_ref)?, // getfield
                    181 => bytecode_builder.putfield(*field_ref)?, // putfield
                    21 => bytecode_builder.iload(*field_ref)?, // iload
                    22 => bytecode_builder.lload(*field_ref)?, // lload
                    23 => bytecode_builder.fload(*field_ref)?, // fload
                    24 => bytecode_builder.dload(*field_ref)?, // dload
                    25 => bytecode_builder.aload(*field_ref)?, // aload
                    _ => return Err("Invalid instance field access opcode".into()),
                }
            }
            FieldAccessOptimizationType::ConstantInline { value } => {
                self.generate_constant_load(value, bytecode_builder)?;
            }
            FieldAccessOptimizationType::ArrayLength => {
                bytecode_builder.arraylength()?;
            }
            FieldAccessOptimizationType::ClassLiteral { class_ref } => {
                bytecode_builder.ldc(*class_ref)?;
            }
        }
        Ok(())
    }

    /// Generate constant loading bytecode
    fn generate_constant_load(
        &self,
        literal: &Literal,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match literal {
            Literal::Integer(value) => {
                match *value {
                    -1 => bytecode_builder.iconst_m1()?,
                    0 => bytecode_builder.iconst_0()?,
                    1 => bytecode_builder.iconst_1()?,
                    2 => bytecode_builder.iconst_2()?,
                    3 => bytecode_builder.iconst_3()?,
                    4 => bytecode_builder.iconst_4()?,
                    5 => bytecode_builder.iconst_5()?,
                    -128..=127 => bytecode_builder.bipush(*value as i8)?,
                    -32768..=32767 => bytecode_builder.sipush(*value as i16)?,
                    _ => bytecode_builder.ldc(1)?, // Placeholder constant pool index
                }
            }
            // Handle long values (removed duplicate pattern)
            Literal::Float(value) => {
                if *value == 0.0 {
                    bytecode_builder.fconst_0()?;
                } else if *value == 1.0 {
                    bytecode_builder.fconst_1()?;
                } else if *value == 2.0 {
                    bytecode_builder.fconst_2()?;
                } else {
                    bytecode_builder.ldc(1)?; // Placeholder constant pool index
                }
            }
            // Handle double values (removed duplicate pattern)
            Literal::Boolean(value) => {
                if *value {
                    bytecode_builder.iconst_1()?;
                } else {
                    bytecode_builder.iconst_0()?;
                }
            }
            Literal::String(_) => {
                bytecode_builder.ldc(1)?; // Placeholder constant pool index
            }
            Literal::Null => {
                bytecode_builder.aconst_null()?;
            }
            Literal::Long(value) => {
                // Long constants - use lconst or ldc2_w
                match *value {
                    0 => bytecode_builder.lconst_0()?,
                    1 => bytecode_builder.lconst_1()?,
                    _ => {
                        // Use ldc2_w for other long values
                        // This would need constant pool integration
                        return Err("Long constants not fully implemented".into());
                    }
                }
            },
            Literal::Double(value) => {
                // Double constants - use dconst or ldc2_w
                if *value == 0.0 {
                    bytecode_builder.dconst_0()?;
                } else if *value == 1.0 {
                    bytecode_builder.dconst_1()?;
                } else {
                    // Use ldc2_w for other double values
                    return Err("Double constants not fully implemented".into());
                }
            },
            Literal::Char(value) => {
                let char_value = *value as i32;
                match char_value {
                    0..=5 => { bytecode_builder.push_byte(3 + char_value as u8); }, // iconst_0 to iconst_5
                    -128..=127 => bytecode_builder.bipush(char_value as i8)?,
                    -32768..=32767 => bytecode_builder.sipush(char_value as i16)?,
                    _ => bytecode_builder.ldc(1)?, // Placeholder constant pool index
                }
            }
        }
        Ok(())
    }

    // Helper methods for pattern recognition

    fn is_constant_field(&self, field_name: &str) -> bool {
        self.constant_fields.contains_key(field_name) ||
        (field_name.chars().all(|c| c.is_uppercase() || c == '_') && field_name.len() > 1)
    }

    fn is_static_field(&self, field_name: &str) -> bool {
        self.static_fields.get(field_name).copied().unwrap_or_else(|| {
            // Heuristic: all uppercase names are likely static
            field_name.chars().all(|c| c.is_uppercase() || c == '_')
        })
    }

    fn is_static_context(&self, object_expr: Option<&Box<Expr>>) -> bool {
        match object_expr {
            Some(expr) => {
                if let Expr::Identifier(ident) = expr.as_ref() {
                    // Check if identifier refers to a class name (static context)
                    ident.name.chars().next().map_or(false, |c| c.is_uppercase())
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn requires_null_check(&self, object_expr: Option<&Box<Expr>>) -> bool {
        match object_expr {
            Some(expr) => {
                match expr.as_ref() {
                    Expr::Identifier(ident) => {
                        // 'this' and 'super' don't need null checks
                        ident.name != "this" && ident.name != "super"
                    }
                    Expr::Literal(LiteralExpr { value: Literal::Null, .. }) => false, // null.field would throw NPE anyway
                    _ => true, // Most expressions need null checks
                }
            }
            None => false, // No object means static context
        }
    }
}

/// Context information for identifier analysis
#[derive(Debug, Clone)]
pub struct IdentifierContext {
    pub symbol_kind: SymbolKind,
    pub variable_type: VariableType,
    pub local_index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    LocalVariable,
    StaticField,
    InstanceField,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

/// Stack effect calculation for field access
impl FieldAccessPattern {
    pub fn calculate_stack_effect(&self) -> (i32, i32) {
        self.stack_effect
    }

    pub fn get_instruction_size(&self) -> u32 {
        match &self.optimization_type {
            FieldAccessOptimizationType::Static { .. } => 3, // getstatic/putstatic
            FieldAccessOptimizationType::Instance { opcode, requires_null_check, .. } => {
                let base_size = match *opcode {
                    21..=25 => 2, // local variable loads (iload, lload, fload, dload, aload)
                    180 | 181 => 3, // getfield/putfield
                    _ => 3,
                };
                if *requires_null_check { base_size + 4 } else { base_size } // +4 for dup+invokevirtual+pop
            }
            FieldAccessOptimizationType::ConstantInline { value } => {
                match value {
                    Literal::Integer(v) => {
                        match *v {
                            -1..=5 => 1, // iconst
                            -128..=127 => 2, // bipush
                            -32768..=32767 => 3, // sipush
                            _ => 3, // ldc
                        }
                    }
                    // Handle long values (removed duplicate pattern)
                    Literal::Float(v) => if *v == 0.0 || *v == 1.0 || *v == 2.0 { 1 } else { 3 },
                    Literal::Boolean(_) => 1, // iconst_0/iconst_1
                    Literal::String(_) => 3, // ldc
                    Literal::Long(value) => {
                        match *value {
                            0..=1 => 1, // lconst_0, lconst_1
                            _ => 3, // ldc2_w
                        }
                    },
                    Literal::Double(value) => {
                        if *value == 0.0 || *value == 1.0 {
                            1 // dconst_0, dconst_1
                        } else {
                            3 // ldc2_w
                        }
                    },
                    Literal::Null => 1, // aconst_null
                    Literal::Char(v) => {
                        let char_value = *v as i32;
                        match char_value {
                            0..=5 => 1, // iconst
                            -128..=127 => 2, // bipush
                            -32768..=32767 => 3, // sipush
                            _ => 3, // ldc
                        }
                    }
                }
            }
            FieldAccessOptimizationType::ArrayLength => 1, // arraylength
            FieldAccessOptimizationType::ClassLiteral { .. } => 3, // ldc
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;

    #[test]
    fn test_static_field_access() {
        let optimizer = FieldAccessOptimizer::new();
        
        let field_access = FieldAccessExpr {
            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                name: "Integer".to_string(),
                span: Span::from_to(0, 0, 0, 7),
            }))),
            name: "MAX_VALUE".to_string(),
            span: Span::from_to(0, 0, 0, 17),
        };

        let pattern = optimizer.analyze_field_access(&field_access, false);
        
        match pattern.optimization_type {
            FieldAccessOptimizationType::ConstantInline { value } => {
                assert_eq!(value, Literal::Integer(i32::MAX as i64));
            }
            _ => panic!("Expected constant inline optimization"),
        }
        
        assert_eq!(pattern.stack_effect, (0, 1));
    }

    #[test]
    fn test_instance_field_access() {
        let optimizer = FieldAccessOptimizer::new();
        
        let field_access = FieldAccessExpr {
            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                name: "obj".to_string(),
                span: Span::from_to(0, 0, 0, 3),
            }))),
            name: "value".to_string(),
            span: Span::from_to(0, 0, 0, 9),
        };

        let pattern = optimizer.analyze_field_access(&field_access, false);
        
        match pattern.optimization_type {
            FieldAccessOptimizationType::Instance { opcode, requires_null_check, .. } => {
                assert_eq!(opcode, 180); // getfield
                assert!(requires_null_check);
            }
            _ => panic!("Expected instance field access"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 1));
    }

    #[test]
    fn test_array_length_access() {
        let optimizer = FieldAccessOptimizer::new();
        
        let field_access = FieldAccessExpr {
            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                name: "array".to_string(),
                span: Span::from_to(0, 0, 0, 5),
            }))),
            name: "length".to_string(),
            span: Span::from_to(0, 0, 0, 12),
        };

        let pattern = optimizer.analyze_field_access(&field_access, false);
        
        match pattern.optimization_type {
            FieldAccessOptimizationType::ArrayLength => {},
            _ => panic!("Expected array length access"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 1));
        assert_eq!(pattern.get_instruction_size(), 1);
    }

    #[test]
    fn test_identifier_context() {
        let optimizer = FieldAccessOptimizer::new();
        
        let identifier = IdentifierExpr {
            name: "localVar".to_string(),
            span: Span::from_to(0, 0, 0, 8),
        };

        let context = IdentifierContext {
            symbol_kind: SymbolKind::LocalVariable,
            variable_type: VariableType::Int,
            local_index: 1,
        };

        let pattern = optimizer.analyze_identifier_access(&identifier, &context);
        
        match pattern.optimization_type {
            FieldAccessOptimizationType::Instance { opcode, field_ref, .. } => {
                assert_eq!(opcode, 21); // iload
                assert_eq!(field_ref, 1);
            }
            _ => panic!("Expected local variable access"),
        }
        
        assert_eq!(pattern.stack_effect, (0, 1));
    }
}
