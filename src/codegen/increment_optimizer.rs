use crate::ast::*;
use crate::codegen::bytecode::BytecodeBuilder;

/// Increment/decrement optimization patterns from javac's Gen.java visitUnary and visitAssignop
#[derive(Debug, Clone, PartialEq)]
pub enum IncrementOptimizationType {
    /// Local variable increment using iinc instruction
    LocalIncrement {
        local_index: u8,
        increment: i16,
    },
    /// General increment using load + constant + add + store
    GeneralIncrement {
        load_opcode: u8,
        store_opcode: u8,
        add_opcode: u8,
        local_index: u8,
        increment: i32,
        requires_narrowing: bool,
        narrowing_opcode: Option<u8>,
    },
    /// Pre-increment optimization (++var)
    PreIncrement {
        optimization: Box<IncrementOptimizationType>,
    },
    /// Post-increment optimization (var++)
    PostIncrement {
        optimization: Box<IncrementOptimizationType>,
        requires_stash: bool,
    },
}

/// Increment pattern analysis
#[derive(Debug, Clone)]
pub struct IncrementPattern {
    pub optimization_type: IncrementOptimizationType,
    pub stack_effect: (i32, i32), // (pop, push)
    pub estimated_cost: u32,
}

/// Increment optimizer following javac's increment/decrement patterns
pub struct IncrementOptimizer;

impl IncrementOptimizer {
    pub fn new() -> Self {
        Self
    }

    /// Analyze unary increment/decrement expression (from Gen.visitUnary)
    pub fn analyze_unary_increment(
        &self,
        unary: &UnaryExpr,
        variable_info: &VariableInfo,
    ) -> IncrementPattern {
        let is_pre = matches!(unary.operator, UnaryOp::PreInc | UnaryOp::PreDec);
        let is_increment = matches!(unary.operator, UnaryOp::PreInc | UnaryOp::PostInc);
        let increment_value = if is_increment { 1 } else { -1 };

        let base_optimization = self.create_base_increment_optimization(
            variable_info,
            increment_value,
        );

        let optimization_type = if is_pre {
            IncrementOptimizationType::PreIncrement {
                optimization: Box::new(base_optimization),
            }
        } else {
            IncrementOptimizationType::PostIncrement {
                optimization: Box::new(base_optimization),
                requires_stash: !self.can_use_iinc(variable_info, increment_value),
            }
        };

        let stack_effect = if is_pre {
            (0, 1) // Push incremented value
        } else {
            (0, 1) // Push original value
        };

        let estimated_cost = self.calculate_increment_cost(&optimization_type);

        IncrementPattern {
            optimization_type,
            stack_effect,
            estimated_cost,
        }
    }

    /// Analyze assignment increment/decrement expression (from Gen.visitAssignop)
    pub fn analyze_assignment_increment(
        &self,
        assignment: &AssignmentExpr,
        variable_info: &VariableInfo,
    ) -> Option<IncrementPattern> {
        // Check if this is an increment/decrement assignment
        let increment_value = match assignment.operator {
            AssignmentOp::AddAssign => {
                if let Expr::Literal(LiteralExpr { value: Literal::Integer(val), .. }) = assignment.value.as_ref() {
                    *val as i32
                } else {
                    return None; // Not a constant increment
                }
            }
            AssignmentOp::SubAssign => {
                if let Expr::Literal(LiteralExpr { value: Literal::Integer(val), .. }) = assignment.value.as_ref() {
                    -(*val as i32)
                } else {
                    return None; // Not a constant decrement
                }
            }
            _ => return None, // Not an increment/decrement assignment
        };

        // Check if increment is within iinc range (-32768 to 32767)
        if increment_value < -32768 || increment_value > 32767 {
            return None; // Too large for iinc optimization
        }

        let optimization_type = self.create_base_increment_optimization(
            variable_info,
            increment_value,
        );

        let estimated_cost = self.calculate_increment_cost(&optimization_type);

        Some(IncrementPattern {
            optimization_type,
            stack_effect: (0, 1), // Push the result value
            estimated_cost,
        })
    }

    /// Create base increment optimization based on variable type and increment value
    fn create_base_increment_optimization(
        &self,
        variable_info: &VariableInfo,
        increment_value: i32,
    ) -> IncrementOptimizationType {
        // Check if we can use iinc instruction (local int variable, increment in range)
        if self.can_use_iinc(variable_info, increment_value) {
            return IncrementOptimizationType::LocalIncrement {
                local_index: variable_info.local_index as u8,
                increment: increment_value as i16,
            };
        }

        // Use general increment pattern
        let (load_opcode, store_opcode, add_opcode) = match variable_info.variable_type {
            VariableType::Int => (21, 54, 96),    // iload, istore, iadd
            VariableType::Long => (22, 55, 97),   // lload, lstore, ladd
            VariableType::Float => (23, 56, 98),  // fload, fstore, fadd
            VariableType::Double => (24, 57, 99), // dload, dstore, dadd
            VariableType::Reference => return IncrementOptimizationType::LocalIncrement {
                local_index: variable_info.local_index as u8,
                increment: 0, // Can't increment reference types
            },
        };

        // Check if narrowing conversion is needed (for byte, char, short)
        let (requires_narrowing, narrowing_opcode) = match variable_info.variable_subtype {
            Some(VariableSubtype::Byte) => (true, Some(145)), // i2b
            Some(VariableSubtype::Char) => (true, Some(146)), // i2c
            Some(VariableSubtype::Short) => (true, Some(147)), // i2s
            None => (false, None),
        };

        IncrementOptimizationType::GeneralIncrement {
            load_opcode,
            store_opcode,
            add_opcode,
            local_index: variable_info.local_index as u8,
            increment: increment_value,
            requires_narrowing,
            narrowing_opcode,
        }
    }

    /// Check if iinc instruction can be used
    fn can_use_iinc(&self, variable_info: &VariableInfo, increment_value: i32) -> bool {
        // iinc can only be used for int local variables with increment in range -32768..32767
        // However, if the variable has a subtype (byte, char, short), we need narrowing conversion
        // after the increment, so iinc is not suitable
        variable_info.variable_type == VariableType::Int &&
        variable_info.is_local &&
        variable_info.variable_subtype.is_none() && // No subtype means pure int
        increment_value >= -32768 &&
        increment_value <= 32767
    }

    /// Calculate estimated cost for increment operation
    fn calculate_increment_cost(&self, optimization_type: &IncrementOptimizationType) -> u32 {
        match optimization_type {
            IncrementOptimizationType::LocalIncrement { .. } => 3, // iinc instruction
            IncrementOptimizationType::GeneralIncrement { requires_narrowing, .. } => {
                let base_cost = 6; // load + const + add + store
                if *requires_narrowing { base_cost + 1 } else { base_cost }
            }
            IncrementOptimizationType::PreIncrement { optimization } => {
                self.calculate_increment_cost(optimization) + 1 // +1 for duplicate
            }
            IncrementOptimizationType::PostIncrement { optimization, requires_stash } => {
                let base_cost = self.calculate_increment_cost(optimization);
                if *requires_stash { base_cost + 3 } else { base_cost + 1 } // +3 for stash operations
            }
        }
    }

    /// Generate optimized bytecode for increment operation
    pub fn generate_optimized_increment(
        &self,
        pattern: &IncrementPattern,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match &pattern.optimization_type {
            IncrementOptimizationType::LocalIncrement { local_index, increment } => {
                bytecode_builder.iinc(*local_index, *increment)?;
            }
            IncrementOptimizationType::GeneralIncrement {
                load_opcode,
                store_opcode,
                add_opcode,
                local_index,
                increment,
                requires_narrowing,
                narrowing_opcode,
            } => {
                // Load variable
                self.emit_load_instruction(*load_opcode, *local_index, bytecode_builder)?;
                
                // Load increment constant
                self.emit_constant_load(*increment, bytecode_builder)?;
                
                // Add
                bytecode_builder.push_byte(*add_opcode);
                
                // Narrowing conversion if needed
                if *requires_narrowing {
                    if let Some(narrowing_op) = narrowing_opcode {
                        bytecode_builder.push_byte(*narrowing_op);
                    }
                }
                
                // Store result
                self.emit_store_instruction(*store_opcode, *local_index, bytecode_builder)?;
            }
            IncrementOptimizationType::PreIncrement { optimization } => {
                // For pre-increment: increment first, then duplicate result
                self.generate_base_increment(optimization, bytecode_builder)?;
                // Load the incremented value for the expression result
                if let IncrementOptimizationType::LocalIncrement { local_index, .. } = optimization.as_ref() {
                    bytecode_builder.iload(*local_index as u16)?;
                }
            }
            IncrementOptimizationType::PostIncrement { optimization, requires_stash } => {
                if *requires_stash {
                    // For post-increment with stash: load original, stash it, increment, return stashed value
                    if let IncrementOptimizationType::GeneralIncrement { load_opcode, local_index, .. } = optimization.as_ref() {
                        // Load original value
                        self.emit_load_instruction(*load_opcode, *local_index, bytecode_builder)?;
                        
                        // Stash original value (implementation depends on type)
                        self.emit_stash_operation(*load_opcode, bytecode_builder)?;
                        
                        // Perform increment
                        self.generate_base_increment(optimization, bytecode_builder)?;
                        
                        // The stashed value is now on top of stack (original value)
                    }
                } else {
                    // For post-increment with iinc: load original, then increment
                    if let IncrementOptimizationType::LocalIncrement { local_index, increment } = optimization.as_ref() {
                        // Load original value
                        bytecode_builder.iload(*local_index as u16)?;
                        
                        // Increment the variable
                        bytecode_builder.iinc(*local_index, *increment)?;
                        
                        // Original value is on stack
                    }
                }
            }
        }
        Ok(())
    }

    /// Generate base increment operation (without pre/post handling)
    fn generate_base_increment(
        &self,
        optimization: &IncrementOptimizationType,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match optimization {
            IncrementOptimizationType::LocalIncrement { local_index, increment } => {
                bytecode_builder.iinc(*local_index, *increment)?;
            }
            IncrementOptimizationType::GeneralIncrement {
                load_opcode,
                store_opcode,
                add_opcode,
                local_index,
                increment,
                requires_narrowing,
                narrowing_opcode,
            } => {
                // Load variable
                self.emit_load_instruction(*load_opcode, *local_index, bytecode_builder)?;
                
                // Load increment constant
                self.emit_constant_load(*increment, bytecode_builder)?;
                
                // Add
                bytecode_builder.push_byte(*add_opcode);
                
                // Narrowing conversion if needed
                if *requires_narrowing {
                    if let Some(narrowing_op) = narrowing_opcode {
                        bytecode_builder.push_byte(*narrowing_op);
                    }
                }
                
                // Store result
                self.emit_store_instruction(*store_opcode, *local_index, bytecode_builder)?;
            }
            _ => return Err("Invalid base increment optimization type".into()),
        }
        Ok(())
    }

    /// Emit load instruction based on opcode
    fn emit_load_instruction(
        &self,
        opcode: u8,
        local_index: u8,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match opcode {
            21 => bytecode_builder.iload(local_index as u16)?, // iload
            22 => bytecode_builder.lload(local_index as u16)?, // lload
            23 => bytecode_builder.fload(local_index as u16)?, // fload
            24 => bytecode_builder.dload(local_index as u16)?, // dload
            25 => bytecode_builder.aload(local_index as u16)?, // aload
            _ => return Err("Invalid load opcode".into()),
        }
        Ok(())
    }

    /// Emit store instruction based on opcode
    fn emit_store_instruction(
        &self,
        opcode: u8,
        local_index: u8,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match opcode {
            54 => bytecode_builder.istore(local_index as u16)?, // istore
            55 => bytecode_builder.lstore(local_index as u16)?, // lstore
            56 => bytecode_builder.fstore(local_index as u16)?, // fstore
            57 => bytecode_builder.dstore(local_index as u16)?, // dstore
            58 => bytecode_builder.astore(local_index as u16)?, // astore
            _ => return Err("Invalid store opcode".into()),
        }
        Ok(())
    }

    /// Emit constant loading instruction
    fn emit_constant_load(
        &self,
        value: i32,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match value {
            -1 => bytecode_builder.iconst_m1()?,
            0 => bytecode_builder.iconst_0()?,
            1 => bytecode_builder.iconst_1()?,
            2 => bytecode_builder.iconst_2()?,
            3 => bytecode_builder.iconst_3()?,
            4 => bytecode_builder.iconst_4()?,
            5 => bytecode_builder.iconst_5()?,
            -128..=127 => bytecode_builder.bipush(value as i8)?,
            -32768..=32767 => bytecode_builder.sipush(value as i16)?,
            _ => bytecode_builder.ldc(1)?, // Placeholder constant pool index
        }
        Ok(())
    }

    /// Emit stash operation for post-increment
    fn emit_stash_operation(
        &self,
        load_opcode: u8,
        bytecode_builder: &mut BytecodeBuilder,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match load_opcode {
            21 => { // int
                bytecode_builder.dup()?;
                // Additional stash logic would go here
            }
            22 => { // long
                bytecode_builder.dup2()?;
                // Additional stash logic would go here
            }
            23 => { // float
                bytecode_builder.dup()?;
                // Additional stash logic would go here
            }
            24 => { // double
                bytecode_builder.dup2()?;
                // Additional stash logic would go here
            }
            25 => { // reference
                bytecode_builder.dup()?;
                // Additional stash logic would go here
            }
            _ => return Err("Invalid load opcode for stash".into()),
        }
        Ok(())
    }
}

/// Variable information for increment optimization
#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub variable_type: VariableType,
    pub variable_subtype: Option<VariableSubtype>,
    pub local_index: usize,
    pub is_local: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableSubtype {
    Byte,
    Char,
    Short,
}

/// Stack effect calculation for increment operations
impl IncrementPattern {
    pub fn calculate_stack_effect(&self) -> (i32, i32) {
        self.stack_effect
    }

    pub fn get_instruction_size(&self) -> u32 {
        match &self.optimization_type {
            IncrementOptimizationType::LocalIncrement { .. } => 3, // iinc
            IncrementOptimizationType::GeneralIncrement { requires_narrowing, .. } => {
                let base_size = 6; // load + const + add + store (estimated)
                if *requires_narrowing { base_size + 1 } else { base_size }
            }
            IncrementOptimizationType::PreIncrement { optimization } => {
                self.calculate_base_size(optimization) + 2 // +2 for load result
            }
            IncrementOptimizationType::PostIncrement { optimization, requires_stash } => {
                let base_size = self.calculate_base_size(optimization);
                if *requires_stash { base_size + 4 } else { base_size + 2 } // +stash operations
            }
        }
    }

    fn calculate_base_size(&self, optimization: &IncrementOptimizationType) -> u32 {
        match optimization {
            IncrementOptimizationType::LocalIncrement { .. } => 3,
            IncrementOptimizationType::GeneralIncrement { requires_narrowing, .. } => {
                if *requires_narrowing { 7 } else { 6 }
            }
            _ => 3, // Fallback
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;

    #[test]
    fn test_iinc_optimization() {
        let optimizer = IncrementOptimizer::new();
        
        let unary = UnaryExpr {
            operator: UnaryOp::PreInc,
            operand: Box::new(Expr::Identifier(IdentifierExpr {
                name: "i".to_string(),
                span: Span::from_to(0, 0, 0, 1),
            })),
            span: Span::from_to(0, 0, 0, 3),
        };

        let variable_info = VariableInfo {
            variable_type: VariableType::Int,
            variable_subtype: None,
            local_index: 1,
            is_local: true,
        };

        let pattern = optimizer.analyze_unary_increment(&unary, &variable_info);
        
        match pattern.optimization_type {
            IncrementOptimizationType::PreIncrement { optimization } => {
                match optimization.as_ref() {
                    IncrementOptimizationType::LocalIncrement { local_index, increment } => {
                        assert_eq!(*local_index, 1);
                        assert_eq!(*increment, 1);
                    }
                    _ => panic!("Expected local increment optimization"),
                }
            }
            _ => panic!("Expected pre-increment optimization"),
        }
        
        assert_eq!(pattern.stack_effect, (0, 1));
    }

    #[test]
    fn test_general_increment() {
        let optimizer = IncrementOptimizer::new();
        
        let unary = UnaryExpr {
            operator: UnaryOp::PostDec,
            operand: Box::new(Expr::Identifier(IdentifierExpr {
                name: "f".to_string(),
                span: Span::from_to(0, 0, 0, 1),
            })),
            span: Span::from_to(0, 0, 0, 3),
        };

        let variable_info = VariableInfo {
            variable_type: VariableType::Float,
            variable_subtype: None,
            local_index: 2,
            is_local: true,
        };

        let pattern = optimizer.analyze_unary_increment(&unary, &variable_info);
        
        match pattern.optimization_type {
            IncrementOptimizationType::PostIncrement { optimization, requires_stash } => {
                assert!(requires_stash); // Float increment requires stash
                match optimization.as_ref() {
                    IncrementOptimizationType::GeneralIncrement { 
                        load_opcode, 
                        store_opcode, 
                        add_opcode, 
                        local_index,
                        increment,
                        .. 
                    } => {
                        assert_eq!(*load_opcode, 23); // fload
                        assert_eq!(*store_opcode, 56); // fstore
                        assert_eq!(*add_opcode, 98); // fadd
                        assert_eq!(*local_index, 2);
                        assert_eq!(*increment, -1);
                    }
                    _ => panic!("Expected general increment optimization"),
                }
            }
            _ => panic!("Expected post-increment optimization"),
        }
    }

    #[test]
    fn test_assignment_increment() {
        let optimizer = IncrementOptimizer::new();
        
        let assignment = AssignmentExpr {
            target: Box::new(Expr::Identifier(IdentifierExpr {
                name: "x".to_string(),
                span: Span::from_to(0, 0, 0, 1),
            })),
            operator: AssignmentOp::AddAssign,
            value: Box::new(Expr::Literal(LiteralExpr {
                value: Literal::Integer(5),
                span: Span::from_to(0, 0, 0, 1),
            })),
            span: Span::from_to(0, 0, 0, 6),
        };

        let variable_info = VariableInfo {
            variable_type: VariableType::Int,
            variable_subtype: None,
            local_index: 0,
            is_local: true,
        };

        let pattern = optimizer.analyze_assignment_increment(&assignment, &variable_info).unwrap();
        
        match pattern.optimization_type {
            IncrementOptimizationType::LocalIncrement { local_index, increment } => {
                assert_eq!(local_index, 0);
                assert_eq!(increment, 5);
            }
            _ => panic!("Expected local increment optimization"),
        }
        
        assert_eq!(pattern.stack_effect, (0, 1));
    }

    #[test]
    fn test_narrowing_conversion() {
        let optimizer = IncrementOptimizer::new();
        
        let variable_info = VariableInfo {
            variable_type: VariableType::Int,
            variable_subtype: Some(VariableSubtype::Byte),
            local_index: 1,
            is_local: true,
        };

        let base_optimization = optimizer.create_base_increment_optimization(&variable_info, 1);
        
        match base_optimization {
            IncrementOptimizationType::GeneralIncrement { 
                requires_narrowing, 
                narrowing_opcode, 
                .. 
            } => {
                assert!(requires_narrowing);
                assert_eq!(narrowing_opcode, Some(145)); // i2b
            }
            _ => panic!("Expected general increment with narrowing"),
        }
    }
}
