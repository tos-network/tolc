use crate::ast::*;
use crate::codegen::bytecode::BytecodeBuilder;
use std::collections::HashMap;

/// Method invocation optimization patterns from javac's Gen.java
#[derive(Debug, Clone, PartialEq)]
pub enum InvocationOptimizationType {
    /// Direct method call (invokestatic, invokespecial)
    Direct {
        opcode: u8,
        method_ref: u16,
        is_interface: bool,
    },
    /// Virtual method call (invokevirtual, invokeinterface)
    Virtual {
        opcode: u8,
        method_ref: u16,
        is_interface: bool,
    },
    /// Dynamic method call (invokedynamic)
    Dynamic {
        bootstrap_method: u16,
        name_and_type: u16,
    },
    /// String concatenation optimization (string_add)
    StringConcatenation {
        expressions: Vec<Expr>,
    },
    /// Null check optimization (getClass + pop)
    NullCheck {
        expression: Box<Expr>,
    },
    /// Array length access (arraylength)
    ArrayLength {
        array_expr: Box<Expr>,
    },
    /// Class literal access (.class)
    ClassLiteral {
        class_type: TypeRef,
    },
}

/// Method invocation pattern analysis
#[derive(Debug, Clone)]
pub struct InvocationPattern {
    pub optimization_type: InvocationOptimizationType,
    pub stack_effect: (i32, i32), // (pop, push)
    pub estimated_cost: u32,
}

/// Method invocation optimizer following javac's visitApply, visitSelect, visitIdent patterns
pub struct MethodInvocationOptimizer {
    string_buffer_methods: HashMap<String, u16>,
}

impl MethodInvocationOptimizer {
    pub fn new() -> Self {
        Self {
            string_buffer_methods: HashMap::new(),
        }
    }

    /// Analyze method invocation for optimization opportunities (from Gen.visitApply)
    pub fn analyze_method_invocation(
        &self,
        method_call: &MethodCallExpr,
        receiver_type: Option<&TypeRef>,
    ) -> InvocationPattern {
        // Check for string concatenation (string_add operator)
        if self.is_string_concatenation(method_call) {
            return InvocationPattern {
                optimization_type: InvocationOptimizationType::StringConcatenation {
                    expressions: self.collect_string_expressions(method_call),
                },
                stack_effect: (method_call.arguments.len() as i32, 1),
                estimated_cost: 10 + method_call.arguments.len() as u32 * 5,
            };
        }

        // Check for array length access
        if self.is_array_length_access(method_call) {
            if let Some(array_expr) = method_call.arguments.first() {
                return InvocationPattern {
                    optimization_type: InvocationOptimizationType::ArrayLength {
                        array_expr: Box::new(array_expr.clone()),
                    },
                    stack_effect: (1, 1),
                    estimated_cost: 1,
                };
            }
        }

        // Check for class literal access
        if self.is_class_literal_access(method_call) {
            if let Some(type_ref) = receiver_type {
                return InvocationPattern {
                    optimization_type: InvocationOptimizationType::ClassLiteral {
                        class_type: type_ref.clone(),
                    },
                    stack_effect: (0, 1),
                    estimated_cost: 2,
                };
            }
        }

        // Determine invocation type based on method characteristics
        let is_static = self.is_static_method(method_call);
        let is_interface = self.is_interface_method(method_call, receiver_type);
        let is_constructor = self.is_constructor_call(method_call);
        let is_super_call = self.is_super_call(method_call);

        let opcode = if is_static {
            184 // invokestatic
        } else if is_constructor || is_super_call {
            183 // invokespecial
        } else if is_interface {
            185 // invokeinterface
        } else {
            182 // invokevirtual
        };

        let optimization_type = if is_static || is_constructor || is_super_call {
            InvocationOptimizationType::Direct {
                opcode,
                method_ref: 1, // Placeholder
                is_interface,
            }
        } else {
            InvocationOptimizationType::Virtual {
                opcode,
                method_ref: 1, // Placeholder
                is_interface,
            }
        };

        let arg_count = method_call.arguments.len() as i32;
        let receiver_count = if is_static { 0 } else { 1 };
        let return_count = if self.is_void_method(method_call) { 0 } else { 1 };

        InvocationPattern {
            optimization_type,
            stack_effect: (arg_count + receiver_count, return_count),
            estimated_cost: if is_interface { 5 } else { 3 },
        }
    }

    /// Analyze field access for optimization (from Gen.visitSelect, visitIdent)
    pub fn analyze_field_access(
        &self,
        field_access: &FieldAccessExpr,
    ) -> InvocationPattern {
        let is_static = self.is_static_field(&field_access.name);
        let is_constant = self.is_constant_field(&field_access.name);

        // Constant field optimization - inline the constant value
        if is_constant {
            return InvocationPattern {
                optimization_type: InvocationOptimizationType::Direct {
                    opcode: 18, // ldc
                    method_ref: 1, // Placeholder for constant pool index
                    is_interface: false,
                },
                stack_effect: (0, 1),
                estimated_cost: 1,
            };
        }

        let opcode = if is_static { 178 } else { 180 }; // getstatic vs getfield
        let stack_effect = if is_static { (0, 1) } else { (1, 1) };

        InvocationPattern {
            optimization_type: InvocationOptimizationType::Direct {
                opcode,
                method_ref: 1, // Placeholder
                is_interface: false,
            },
            stack_effect,
            estimated_cost: 2,
        }
    }

    /// Generate optimized bytecode for method invocation
    pub fn generate_optimized_invocation(
        &self,
        pattern: &InvocationPattern,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match &pattern.optimization_type {
            InvocationOptimizationType::StringConcatenation { expressions } => {
                self.generate_string_concatenation(expressions, bytecode_builder)?;
            }
            InvocationOptimizationType::ArrayLength { array_expr } => {
                // Array expression should already be on stack
                bytecode_builder.arraylength()?;
            }
            InvocationOptimizationType::ClassLiteral { class_type: _ } => {
                // Generate ldc for class literal
                bytecode_builder.ldc(1)?; // Placeholder constant pool index
            }
            InvocationOptimizationType::Direct { opcode, method_ref, is_interface } => {
                match *opcode {
                    183 => bytecode_builder.invokespecial(*method_ref)?, // invokespecial
                    184 => bytecode_builder.invokestatic(*method_ref)?, // invokestatic
                    178 => bytecode_builder.getstatic(*method_ref)?, // getstatic
                    180 => bytecode_builder.getfield(*method_ref)?, // getfield
                    18 => bytecode_builder.ldc(*method_ref)?, // ldc for constants
                    _ => return Err("Unsupported direct invocation opcode".into()),
                }
            }
            InvocationOptimizationType::Virtual { opcode, method_ref, is_interface } => {
                if *is_interface {
                    bytecode_builder.invokeinterface(*method_ref, pattern.stack_effect.0 as u8)?;
                } else {
                    bytecode_builder.invokevirtual(*method_ref)?;
                }
            }
            InvocationOptimizationType::Dynamic { bootstrap_method, name_and_type } => {
                // invokedynamic implementation
                bytecode_builder.invokedynamic(*bootstrap_method)?;
            }
            InvocationOptimizationType::NullCheck { expression: _ } => {
                // Generate getClass + pop for null check
                bytecode_builder.dup()?;
                // Assuming Object.getClass method ref is at index 1
                bytecode_builder.invokevirtual(1)?;
                bytecode_builder.pop()?;
            }
        }
        Ok(())
    }

    /// Generate string concatenation using StringBuilder (from Gen.makeStringBuffer, appendString, bufferToString)
    fn generate_string_concatenation(
        &self,
        expressions: &[Expr],
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // new StringBuilder
        bytecode_builder.new_object(1)?; // Placeholder for StringBuilder class ref
        bytecode_builder.dup()?;
        
        // StringBuilder.<init>()
        bytecode_builder.invokespecial(1)?; // Placeholder for StringBuilder constructor

        // Append each expression
        for expr in expressions {
            // Expression value should be on stack
            // StringBuilder.append(type)
            bytecode_builder.invokevirtual(1)?; // Placeholder for appropriate append method
        }

        // StringBuilder.toString()
        bytecode_builder.invokevirtual(1)?; // Placeholder for toString method

        Ok(())
    }

    // Helper methods for pattern recognition

    fn is_string_concatenation(&self, method_call: &MethodCallExpr) -> bool {
        // Check if this is a string concatenation operation
        method_call.name == "concat" || 
        (method_call.name == "append" && method_call.arguments.len() >= 2)
    }

    fn is_array_length_access(&self, method_call: &MethodCallExpr) -> bool {
        method_call.name == "length" && method_call.arguments.len() == 1
    }

    fn is_class_literal_access(&self, method_call: &MethodCallExpr) -> bool {
        method_call.name == "class"
    }

    fn is_static_method(&self, method_call: &MethodCallExpr) -> bool {
        // This would need to be determined from symbol table/type information
        // For now, use heuristics based on method name patterns
        method_call.name.starts_with("static") || 
        method_call.target.is_none()
    }

    fn is_interface_method(&self, method_call: &MethodCallExpr, receiver_type: Option<&TypeRef>) -> bool {
        // This would need type system information
        // For now, use heuristics
        if let Some(type_ref) = receiver_type {
            type_ref.name.ends_with("Interface") || type_ref.name.contains("I")
        } else {
            false
        }
    }

    fn is_constructor_call(&self, method_call: &MethodCallExpr) -> bool {
        method_call.name == "<init>" || method_call.name == "new"
    }

    fn is_super_call(&self, method_call: &MethodCallExpr) -> bool {
        if let Some(target) = &method_call.target {
            if let Expr::Identifier(ident) = target.as_ref() {
                return ident.name == "super";
            }
        }
        false
    }

    fn is_void_method(&self, method_call: &MethodCallExpr) -> bool {
        // This would need return type information from symbol table
        // For now, use heuristics
        method_call.name.starts_with("set") || 
        method_call.name == "println" ||
        method_call.name == "print"
    }

    fn is_static_field(&self, field_name: &str) -> bool {
        // This would need symbol table information
        // For now, use naming conventions
        field_name.chars().all(|c| c.is_uppercase() || c == '_')
    }

    fn is_constant_field(&self, field_name: &str) -> bool {
        // Constants are typically static final fields with all uppercase names
        self.is_static_field(field_name)
    }

    fn collect_string_expressions(&self, method_call: &MethodCallExpr) -> Vec<Expr> {
        // Collect all expressions that are part of string concatenation
        method_call.arguments.clone()
    }
}

/// Stack effect calculation for method invocations
impl InvocationPattern {
    pub fn calculate_stack_effect(&self) -> (i32, i32) {
        self.stack_effect
    }

    pub fn get_instruction_size(&self) -> u32 {
        match &self.optimization_type {
            InvocationOptimizationType::Direct { opcode, .. } => {
                match *opcode {
                    18 => 2,  // ldc
                    178 | 180 => 3, // getstatic, getfield
                    183 | 184 => 3, // invokespecial, invokestatic
                    _ => 3,
                }
            }
            InvocationOptimizationType::Virtual { is_interface, .. } => {
                if *is_interface { 5 } else { 3 } // invokeinterface vs invokevirtual
            }
            InvocationOptimizationType::Dynamic { .. } => 5, // invokedynamic
            InvocationOptimizationType::StringConcatenation { expressions } => {
                // Estimate: new + dup + init + (append * n) + toString
                5 + expressions.len() as u32 * 3
            }
            InvocationOptimizationType::ArrayLength { .. } => 1, // arraylength
            InvocationOptimizationType::ClassLiteral { .. } => 2, // ldc
            InvocationOptimizationType::NullCheck { .. } => 4, // dup + invokevirtual + pop
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;

    #[test]
    fn test_method_invocation_analysis() {
        let optimizer = MethodInvocationOptimizer::new();
        
        let method_call = MethodCallExpr {
            target: None,
            name: "println".to_string(),
            arguments: vec![
                Expr::Literal(LiteralExpr {
                    value: Literal::String("Hello".to_string()),
                    span: Span::from_to(0, 0, 0, 5),
                })
            ],
            span: Span::from_to(0, 0, 0, 10),
        };

        let pattern = optimizer.analyze_method_invocation(&method_call, None);
        
        match pattern.optimization_type {
            InvocationOptimizationType::Direct { opcode, .. } => {
                assert_eq!(opcode, 184); // invokestatic
            }
            _ => panic!("Expected direct invocation"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 0)); // 1 arg, void return
    }

    #[test]
    fn test_field_access_analysis() {
        let optimizer = MethodInvocationOptimizer::new();
        
        let field_access = FieldAccessExpr {
            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                name: "obj".to_string(),
                span: Span::from_to(0, 0, 0, 3),
            }))),
            name: "value".to_string(),
            span: Span::from_to(0, 0, 0, 9),
        };

        let pattern = optimizer.analyze_field_access(&field_access);
        
        match pattern.optimization_type {
            InvocationOptimizationType::Direct { opcode, .. } => {
                assert_eq!(opcode, 180); // getfield
            }
            _ => panic!("Expected direct field access"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 1)); // object ref in, field value out
    }

    #[test]
    fn test_stack_effects() {
        let optimizer = MethodInvocationOptimizer::new();
        
        // Test various invocation patterns
        let patterns = vec![
            InvocationPattern {
                optimization_type: InvocationOptimizationType::ArrayLength {
                                    array_expr: Box::new(Expr::Identifier(IdentifierExpr {
                    name: "arr".to_string(),
                    span: Span::from_to(0, 0, 0, 3),
                })),
                },
                stack_effect: (1, 1),
                estimated_cost: 1,
            },
            InvocationPattern {
                optimization_type: InvocationOptimizationType::ClassLiteral {
                    class_type: TypeRef {
                        name: "String".to_string(),

                        type_args: vec![],
                        array_dims: 0,
                        annotations: vec![],
                        span: Span::from_to(0, 0, 0, 6),
                    },
                },
                stack_effect: (0, 1),
                estimated_cost: 2,
            },
        ];

        for pattern in patterns {
            let (pop, push) = pattern.calculate_stack_effect();
            assert!(pop >= 0 && push >= 0);
            assert!(pattern.get_instruction_size() > 0);
        }
    }
}
