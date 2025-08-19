use crate::codegen::bytecode::BytecodeBuilder;

/// Type coercion optimization patterns from javac's Gen.java and Code.java
#[derive(Debug, Clone, PartialEq)]
pub enum CoercionOptimizationType {
    /// No coercion needed (types are compatible)
    NoCoercion,
    /// Primitive widening conversion (implicit)
    PrimitiveWidening {
        from_type: PrimitiveType,
        to_type: PrimitiveType,
        opcode: Option<u8>, // Some conversions are implicit (no opcode)
    },
    /// Primitive narrowing conversion (explicit cast)
    PrimitiveNarrowing {
        from_type: PrimitiveType,
        to_type: PrimitiveType,
        opcode: u8,
    },
    /// Reference type cast (checkcast)
    ReferenceCast {
        from_type: String,
        to_type: String,
        class_ref: u16,
        is_necessary: bool, // false if cast is redundant
    },
    /// Boxing conversion (primitive to wrapper)
    Boxing {
        primitive_type: PrimitiveType,
        wrapper_class: String,
        method_ref: u16, // valueOf method
    },
    /// Unboxing conversion (wrapper to primitive)
    Unboxing {
        wrapper_class: String,
        primitive_type: PrimitiveType,
        method_ref: u16, // xxxValue method
    },
    /// Array type coercion
    ArrayCoercion {
        from_element_type: String,
        to_element_type: String,
        dimensions: u8,
        requires_checkcast: bool,
    },
}

/// Type coercion pattern analysis
#[derive(Debug, Clone)]
pub struct CoercionPattern {
    pub optimization_type: CoercionOptimizationType,
    pub stack_effect: (i32, i32), // (pop, push)
    pub estimated_cost: u32,
}

/// Type coercion optimizer following javac's type conversion patterns
pub struct TypeCoercionOptimizer {
    primitive_hierarchy: Vec<PrimitiveType>,
    wrapper_classes: std::collections::HashMap<PrimitiveType, String>,
}

impl TypeCoercionOptimizer {
    pub fn new() -> Self {
        let primitive_hierarchy = vec![
            PrimitiveType::Byte,
            PrimitiveType::Short,
            PrimitiveType::Char,
            PrimitiveType::Int,
            PrimitiveType::Long,
            PrimitiveType::Float,
            PrimitiveType::Double,
        ];

        let mut wrapper_classes = std::collections::HashMap::new();
        wrapper_classes.insert(PrimitiveType::Boolean, "java/lang/Boolean".to_string());
        wrapper_classes.insert(PrimitiveType::Byte, "java/lang/Byte".to_string());
        wrapper_classes.insert(PrimitiveType::Char, "java/lang/Character".to_string());
        wrapper_classes.insert(PrimitiveType::Short, "java/lang/Short".to_string());
        wrapper_classes.insert(PrimitiveType::Int, "java/lang/Integer".to_string());
        wrapper_classes.insert(PrimitiveType::Long, "java/lang/Long".to_string());
        wrapper_classes.insert(PrimitiveType::Float, "java/lang/Float".to_string());
        wrapper_classes.insert(PrimitiveType::Double, "java/lang/Double".to_string());

        Self {
            primitive_hierarchy,
            wrapper_classes,
        }
    }

    /// Analyze type coercion requirements
    pub fn analyze_coercion(
        &self,
        from_type: &TypeInfo,
        to_type: &TypeInfo,
        is_explicit_cast: bool,
    ) -> CoercionPattern {
        // Handle same types
        if self.types_equal(from_type, to_type) {
            return CoercionPattern {
                optimization_type: CoercionOptimizationType::NoCoercion,
                stack_effect: (0, 0),
                estimated_cost: 0,
            };
        }

        // Handle primitive type conversions
        if let (TypeInfo::Primitive(from_prim), TypeInfo::Primitive(to_prim)) = (from_type, to_type) {
            return self.analyze_primitive_coercion(from_prim, to_prim, is_explicit_cast);
        }

        // Handle boxing/unboxing
        if let TypeInfo::Primitive(prim_type) = from_type {
            if let TypeInfo::Reference(ref_type) = to_type {
                if let Some(wrapper_class) = self.wrapper_classes.get(prim_type) {
                    if ref_type.name == *wrapper_class || ref_type.name == "java/lang/Object" {
                        return CoercionPattern {
                            optimization_type: CoercionOptimizationType::Boxing {
                                primitive_type: *prim_type,
                                wrapper_class: wrapper_class.clone(),
                                method_ref: 1, // Placeholder
                            },
                            stack_effect: (1, 1),
                            estimated_cost: 3,
                        };
                    }
                }
            }
        }

        if let TypeInfo::Reference(ref_type) = from_type {
            if let TypeInfo::Primitive(prim_type) = to_type {
                if let Some(wrapper_class) = self.wrapper_classes.get(prim_type) {
                    if ref_type.name == *wrapper_class {
                        return CoercionPattern {
                            optimization_type: CoercionOptimizationType::Unboxing {
                                wrapper_class: wrapper_class.clone(),
                                primitive_type: *prim_type,
                                method_ref: 1, // Placeholder
                            },
                            stack_effect: (1, 1),
                            estimated_cost: 3,
                        };
                    }
                }
            }
        }

        // Handle reference type casts
        if let (TypeInfo::Reference(from_ref), TypeInfo::Reference(to_ref)) = (from_type, to_type) {
            return self.analyze_reference_coercion(from_ref, to_ref, is_explicit_cast);
        }

        // Handle array coercions
        if let (TypeInfo::Array(from_array), TypeInfo::Array(to_array)) = (from_type, to_type) {
            return self.analyze_array_coercion(from_array, to_array);
        }

        // Default: no coercion possible
        CoercionPattern {
            optimization_type: CoercionOptimizationType::NoCoercion,
            stack_effect: (0, 0),
            estimated_cost: 0,
        }
    }

    /// Analyze primitive type coercion
    fn analyze_primitive_coercion(
        &self,
        from_type: &PrimitiveType,
        to_type: &PrimitiveType,
        is_explicit_cast: bool,
    ) -> CoercionPattern {
        if from_type == to_type {
            return CoercionPattern {
                optimization_type: CoercionOptimizationType::NoCoercion,
                stack_effect: (0, 0),
                estimated_cost: 0,
            };
        }

        let from_rank = self.get_primitive_rank(from_type);
        let to_rank = self.get_primitive_rank(to_type);

        if from_rank < to_rank || (from_rank == to_rank && self.is_widening_conversion(from_type, to_type)) {
            // Widening conversion
            let opcode = self.get_widening_opcode(from_type, to_type);
            CoercionPattern {
                optimization_type: CoercionOptimizationType::PrimitiveWidening {
                    from_type: *from_type,
                    to_type: *to_type,
                    opcode,
                },
                stack_effect: self.get_primitive_stack_effect(from_type, to_type),
                estimated_cost: if opcode.is_some() { 1 } else { 0 },
            }
        } else if is_explicit_cast {
            // Narrowing conversion (requires explicit cast)
            let opcode = self.get_narrowing_opcode(from_type, to_type);
            CoercionPattern {
                optimization_type: CoercionOptimizationType::PrimitiveNarrowing {
                    from_type: *from_type,
                    to_type: *to_type,
                    opcode,
                },
                stack_effect: self.get_primitive_stack_effect(from_type, to_type),
                estimated_cost: 1,
            }
        } else {
            // Implicit narrowing not allowed
            CoercionPattern {
                optimization_type: CoercionOptimizationType::NoCoercion,
                stack_effect: (0, 0),
                estimated_cost: 0,
            }
        }
    }

    /// Analyze reference type coercion
    fn analyze_reference_coercion(
        &self,
        from_ref: &ReferenceTypeInfo,
        to_ref: &ReferenceTypeInfo,
        is_explicit_cast: bool,
    ) -> CoercionPattern {
        // Check if cast is necessary
        let is_necessary = !self.is_assignable(&from_ref.name, &to_ref.name) || is_explicit_cast;

        if is_necessary {
            CoercionPattern {
                optimization_type: CoercionOptimizationType::ReferenceCast {
                    from_type: from_ref.name.clone(),
                    to_type: to_ref.name.clone(),
                    class_ref: 1, // Placeholder
                    is_necessary: true,
                },
                stack_effect: (1, 1),
                estimated_cost: 3,
            }
        } else {
            CoercionPattern {
                optimization_type: CoercionOptimizationType::NoCoercion,
                stack_effect: (0, 0),
                estimated_cost: 0,
            }
        }
    }

    /// Analyze array type coercion
    fn analyze_array_coercion(
        &self,
        from_array: &ArrayTypeInfo,
        to_array: &ArrayTypeInfo,
    ) -> CoercionPattern {
        if from_array.dimensions != to_array.dimensions {
            return CoercionPattern {
                optimization_type: CoercionOptimizationType::NoCoercion,
                stack_effect: (0, 0),
                estimated_cost: 0,
            };
        }

        let requires_checkcast = !self.is_assignable(&from_array.element_type, &to_array.element_type);

        CoercionPattern {
            optimization_type: CoercionOptimizationType::ArrayCoercion {
                from_element_type: from_array.element_type.clone(),
                to_element_type: to_array.element_type.clone(),
                dimensions: from_array.dimensions,
                requires_checkcast,
            },
            stack_effect: (1, 1),
            estimated_cost: if requires_checkcast { 3 } else { 0 },
        }
    }

    /// Generate optimized bytecode for type coercion
    pub fn generate_optimized_coercion(
        &self,
        pattern: &CoercionPattern,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match &pattern.optimization_type {
            CoercionOptimizationType::NoCoercion => {
                // No bytecode needed
            }
            CoercionOptimizationType::PrimitiveWidening { opcode, .. } => {
                if let Some(op) = opcode {
                    bytecode_builder.push_byte(*op);
                }
                // Some widening conversions are implicit (no bytecode)
            }
            CoercionOptimizationType::PrimitiveNarrowing { opcode, .. } => {
                bytecode_builder.push_byte(*opcode);
            }
            CoercionOptimizationType::ReferenceCast { class_ref, is_necessary, .. } => {
                if *is_necessary {
                    bytecode_builder.checkcast(*class_ref)?;
                }
            }
            CoercionOptimizationType::Boxing { method_ref, .. } => {
                // Call wrapper.valueOf(primitive)
                bytecode_builder.invokestatic(*method_ref)?;
            }
            CoercionOptimizationType::Unboxing { method_ref, .. } => {
                // Call wrapper.xxxValue()
                bytecode_builder.invokevirtual(*method_ref)?;
            }
            CoercionOptimizationType::ArrayCoercion { requires_checkcast, .. } => {
                if *requires_checkcast {
                    bytecode_builder.checkcast(1)?; // Placeholder class ref
                }
            }
        }
        Ok(())
    }

    // Helper methods

    fn types_equal(&self, type1: &TypeInfo, type2: &TypeInfo) -> bool {
        match (type1, type2) {
            (TypeInfo::Primitive(p1), TypeInfo::Primitive(p2)) => p1 == p2,
            (TypeInfo::Reference(r1), TypeInfo::Reference(r2)) => r1.name == r2.name,
            (TypeInfo::Array(a1), TypeInfo::Array(a2)) => {
                a1.dimensions == a2.dimensions && a1.element_type == a2.element_type
            }
            _ => false,
        }
    }

    fn get_primitive_rank(&self, prim_type: &PrimitiveType) -> usize {
        self.primitive_hierarchy.iter().position(|&t| t == *prim_type).unwrap_or(0)
    }

    fn is_widening_conversion(&self, from_type: &PrimitiveType, to_type: &PrimitiveType) -> bool {
        use PrimitiveType::*;
        matches!(
            (from_type, to_type),
            (Byte, Short) | (Byte, Int) | (Byte, Long) | (Byte, Float) | (Byte, Double) |
            (Short, Int) | (Short, Long) | (Short, Float) | (Short, Double) |
            (Char, Int) | (Char, Long) | (Char, Float) | (Char, Double) |
            (Int, Long) | (Int, Float) | (Int, Double) |
            (Long, Float) | (Long, Double) |
            (Float, Double)
        )
    }

    fn get_widening_opcode(&self, from_type: &PrimitiveType, to_type: &PrimitiveType) -> Option<u8> {
        use PrimitiveType::*;
        match (from_type, to_type) {
            (Int, Long) => Some(133), // i2l
            (Int, Float) => Some(134), // i2f
            (Int, Double) => Some(135), // i2d
            (Long, Float) => Some(136), // l2f
            (Long, Double) => Some(137), // l2d
            (Float, Double) => Some(141), // f2d
            _ => None, // Implicit conversion
        }
    }

    fn get_narrowing_opcode(&self, from_type: &PrimitiveType, to_type: &PrimitiveType) -> u8 {
        use PrimitiveType::*;
        match (from_type, to_type) {
            (Long, Int) => 136, // l2i
            (Float, Int) => 139, // f2i
            (Float, Long) => 140, // f2l
            (Double, Int) => 142, // d2i
            (Double, Long) => 143, // d2l
            (Double, Float) => 144, // d2f
            (Int, Byte) => 145, // i2b
            (Int, Char) => 146, // i2c
            (Int, Short) => 147, // i2s
            _ => 0, // Should not happen
        }
    }

    fn get_primitive_stack_effect(&self, from_type: &PrimitiveType, to_type: &PrimitiveType) -> (i32, i32) {
        use PrimitiveType::*;
        let from_size = match from_type {
            Long | Double => 2,
            _ => 1,
        };
        let to_size = match to_type {
            Long | Double => 2,
            _ => 1,
        };
        (from_size, to_size)
    }

    fn is_assignable(&self, from_type: &str, to_type: &str) -> bool {
        // Simplified assignability check
        if from_type == to_type {
            return true;
        }
        
        // Object is assignable from everything
        if to_type == "java/lang/Object" {
            return true;
        }
        
        // String is assignable from String
        if from_type == "java/lang/String" && to_type == "java/lang/String" {
            return true;
        }
        
        // More sophisticated type hierarchy checking would go here
        false
    }
}

/// Type information for coercion analysis
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Primitive(PrimitiveType),
    Reference(ReferenceTypeInfo),
    Array(ArrayTypeInfo),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum PrimitiveType {
    Boolean,
    Byte,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceTypeInfo {
    pub name: String,
    pub type_parameters: Vec<TypeInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayTypeInfo {
    pub element_type: String,
    pub dimensions: u8,
}

/// Stack effect calculation for type coercion
impl CoercionPattern {
    pub fn calculate_stack_effect(&self) -> (i32, i32) {
        self.stack_effect
    }

    pub fn get_instruction_size(&self) -> u32 {
        match &self.optimization_type {
            CoercionOptimizationType::NoCoercion => 0,
            CoercionOptimizationType::PrimitiveWidening { opcode, .. } => {
                if opcode.is_some() { 1 } else { 0 }
            }
            CoercionOptimizationType::PrimitiveNarrowing { .. } => 1,
            CoercionOptimizationType::ReferenceCast { is_necessary, .. } => {
                if *is_necessary { 3 } else { 0 } // checkcast
            }
            CoercionOptimizationType::Boxing { .. } => 3, // invokestatic
            CoercionOptimizationType::Unboxing { .. } => 3, // invokevirtual
            CoercionOptimizationType::ArrayCoercion { requires_checkcast, .. } => {
                if *requires_checkcast { 3 } else { 0 }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_widening() {
        let optimizer = TypeCoercionOptimizer::new();
        
        let from_type = TypeInfo::Primitive(PrimitiveType::Int);
        let to_type = TypeInfo::Primitive(PrimitiveType::Long);
        
        let pattern = optimizer.analyze_coercion(&from_type, &to_type, false);
        
        match pattern.optimization_type {
            CoercionOptimizationType::PrimitiveWidening { from_type, to_type, opcode } => {
                assert_eq!(from_type, PrimitiveType::Int);
                assert_eq!(to_type, PrimitiveType::Long);
                assert_eq!(opcode, Some(133)); // i2l
            }
            _ => panic!("Expected primitive widening"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 2)); // int -> long (2 slots)
    }

    #[test]
    fn test_primitive_narrowing() {
        let optimizer = TypeCoercionOptimizer::new();
        
        let from_type = TypeInfo::Primitive(PrimitiveType::Long);
        let to_type = TypeInfo::Primitive(PrimitiveType::Int);
        
        let pattern = optimizer.analyze_coercion(&from_type, &to_type, true);
        
        match pattern.optimization_type {
            CoercionOptimizationType::PrimitiveNarrowing { from_type, to_type, opcode } => {
                assert_eq!(from_type, PrimitiveType::Long);
                assert_eq!(to_type, PrimitiveType::Int);
                assert_eq!(opcode, 136); // l2i
            }
            _ => panic!("Expected primitive narrowing"),
        }
        
        assert_eq!(pattern.stack_effect, (2, 1)); // long -> int
    }

    #[test]
    fn test_boxing() {
        let optimizer = TypeCoercionOptimizer::new();
        
        let from_type = TypeInfo::Primitive(PrimitiveType::Int);
        let to_type = TypeInfo::Reference(ReferenceTypeInfo {
            name: "java/lang/Integer".to_string(),
            type_parameters: vec![],
        });
        
        let pattern = optimizer.analyze_coercion(&from_type, &to_type, false);
        
        match pattern.optimization_type {
            CoercionOptimizationType::Boxing { primitive_type, wrapper_class, .. } => {
                assert_eq!(primitive_type, PrimitiveType::Int);
                assert_eq!(wrapper_class, "java/lang/Integer");
            }
            _ => panic!("Expected boxing conversion"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 1));
    }

    #[test]
    fn test_unboxing() {
        let optimizer = TypeCoercionOptimizer::new();
        
        let from_type = TypeInfo::Reference(ReferenceTypeInfo {
            name: "java/lang/Integer".to_string(),
            type_parameters: vec![],
        });
        let to_type = TypeInfo::Primitive(PrimitiveType::Int);
        
        let pattern = optimizer.analyze_coercion(&from_type, &to_type, false);
        
        match pattern.optimization_type {
            CoercionOptimizationType::Unboxing { wrapper_class, primitive_type, .. } => {
                assert_eq!(wrapper_class, "java/lang/Integer");
                assert_eq!(primitive_type, PrimitiveType::Int);
            }
            _ => panic!("Expected unboxing conversion"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 1));
    }

    #[test]
    fn test_reference_cast() {
        let optimizer = TypeCoercionOptimizer::new();
        
        let from_type = TypeInfo::Reference(ReferenceTypeInfo {
            name: "java/lang/Object".to_string(),
            type_parameters: vec![],
        });
        let to_type = TypeInfo::Reference(ReferenceTypeInfo {
            name: "java/lang/String".to_string(),
            type_parameters: vec![],
        });
        
        let pattern = optimizer.analyze_coercion(&from_type, &to_type, true);
        
        match pattern.optimization_type {
            CoercionOptimizationType::ReferenceCast { from_type, to_type, is_necessary, .. } => {
                assert_eq!(from_type, "java/lang/Object");
                assert_eq!(to_type, "java/lang/String");
                assert!(is_necessary);
            }
            _ => panic!("Expected reference cast"),
        }
        
        assert_eq!(pattern.stack_effect, (1, 1));
    }

    #[test]
    fn test_no_coercion() {
        let optimizer = TypeCoercionOptimizer::new();
        
        let from_type = TypeInfo::Primitive(PrimitiveType::Int);
        let to_type = TypeInfo::Primitive(PrimitiveType::Int);
        
        let pattern = optimizer.analyze_coercion(&from_type, &to_type, false);
        
        match pattern.optimization_type {
            CoercionOptimizationType::NoCoercion => {},
            _ => panic!("Expected no coercion"),
        }
        
        assert_eq!(pattern.stack_effect, (0, 0));
        assert_eq!(pattern.estimated_cost, 0);
    }
}
