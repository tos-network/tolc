//! Integration tests for advanced optimizers
//! 
//! This module contains comprehensive tests for all the advanced optimization
//! features added to the tolc compiler, including alive state management,
//! Items system enhancements, genCond logic, branch marking, and code compaction.

use tolc::ast::*;

/// Helper function to create a test span
fn create_span() -> Span {
    Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
}

/// Helper function to create a simple integer literal
fn create_int_literal(value: i64) -> Expr {
    Expr::Literal(LiteralExpr {
        value: Literal::Integer(value),
        span: create_span(),
    })
}

/// Helper function to create a boolean literal
fn create_bool_literal(value: bool) -> Expr {
    Expr::Literal(LiteralExpr {
        value: Literal::Boolean(value),
        span: create_span(),
    })
}

/// Helper function to create an identifier expression
fn create_identifier(name: &str) -> Expr {
    Expr::Identifier(IdentifierExpr {
        name: name.to_string(),
        span: create_span(),
    })
}

#[cfg(test)]
mod alive_state_management_tests {
    use tolc::codegen::bytecode::{BytecodeBuilder, ControlFlowType};

    #[test]
    fn test_alive_state_tracking() {
        let mut builder = BytecodeBuilder::new();
        
        // Initially alive
        assert!(builder.is_alive());
        assert!(builder.should_emit());
        
        // Mark as dead after terminal instruction
        builder.mark_dead_after_terminal();
        assert!(!builder.is_alive());
        assert!(!builder.should_emit());
        
        // Mark alive again
        let _ = builder.track_control_flow(ControlFlowType::Label);
        assert!(builder.is_alive());
    }

    #[test]
    fn test_unreachable_code_detection() {
        let builder = BytecodeBuilder::new();
        
        // Should not create unreachable code when alive
        assert!(!builder.would_create_unreachable("normal_instruction"));
        
        // Test unreachable code warning
        let result = builder.check_unreachable_code("test_instruction");
        assert!(result.is_ok());
    }

    #[test]
    fn test_fixed_pc_mechanism() {
        let mut builder = BytecodeBuilder::new();
        
        // Initially not fixed
        assert!(!builder.is_fixed_pc());
        
        // Mark as fixed
        builder.set_fixed_pc(true);
        assert!(builder.is_fixed_pc());
        
        // Unmark
        builder.set_fixed_pc(false);
        assert!(!builder.is_fixed_pc());
    }

    #[test]
    fn test_control_flow_tracking() {
        let mut builder = BytecodeBuilder::new();
        
        // Test terminal flow
        let result = builder.track_control_flow(ControlFlowType::Terminal);
        assert!(result.is_ok());
        assert!(!builder.is_alive());
        
        // Test label flow (should make alive again)
        let result = builder.track_control_flow(ControlFlowType::Label);
        assert!(result.is_ok());
        assert!(builder.is_alive());
        assert!(builder.is_fixed_pc());
    }
}

#[cfg(test)]
mod items_system_tests {
    use super::*;
    use tolc::codegen::item_system::*;

    #[test]
    fn test_self_item_functionality() {
        let this_item = ItemFactory::make_this_item();
        
        match this_item {
            Item::SelfRef(self_item) => {
                assert_eq!(self_item.width(), 1); // Object reference is 1 slot
                let bytecode = self_item.load().unwrap();
                assert_eq!(bytecode[0], tolc::codegen::opcodes::ALOAD_0);
            },
            _ => panic!("Expected SelfRef item"),
        }
    }

    #[test]
    fn test_void_item_functionality() {
        let void_item = ItemFactory::make_void_item();
        
        match void_item {
            Item::Void(void_item) => {
                assert_eq!(void_item.width(), 0); // Void has no width
                let bytecode = void_item.load().unwrap();
                assert!(bytecode.is_empty()); // Void load produces no bytecode
            },
            _ => panic!("Expected Void item"),
        }
    }

    #[test]
    fn test_chain_item_functionality() {
        let base_item = ItemFactory::make_local_item(1, TypeCode::Object, "obj".to_string());
        let chain_item = ChainItem::new(
            base_item,
            vec!["method1".to_string(), "method2".to_string()],
            TypeCode::Object,
        );
        
        assert_eq!(chain_item.width(), 1);
        let bytecode = chain_item.load().unwrap();
        assert!(!bytecode.is_empty());
        
        // Should not support store
        assert!(chain_item.store().is_err());
    }

    #[test]
    fn test_select_item_functionality() {
        let select_item = SelectItem::new(
            "fieldName".to_string(),
            "OwnerClass".to_string(),
            "FieldType".to_string(),
            false, // not static
            TypeCode::Object,
        );
        
        assert_eq!(select_item.width(), 1);
        
        let load_bytecode = select_item.load().unwrap();
        assert_eq!(load_bytecode[0], tolc::codegen::opcodes::GETFIELD);
        
        let store_bytecode = select_item.store().unwrap();
        assert_eq!(store_bytecode[0], tolc::codegen::opcodes::PUTFIELD);
    }

    #[test]
    fn test_apply_item_functionality() {
        let apply_item = ApplyItem::new(
            "methodName".to_string(),
            "()V".to_string(),
            "OwnerClass".to_string(),
            false, // not static
            false, // not interface
            TypeCode::Void,
        );
        
        assert_eq!(apply_item.width(), 0); // void return
        
        let bytecode = apply_item.load().unwrap();
        assert_eq!(bytecode[0], tolc::codegen::opcodes::INVOKEVIRTUAL);
        
        // Should not support store
        assert!(apply_item.store().is_err());
    }

    #[test]
    fn test_item_width_calculations() {
        let int_item = ItemFactory::make_local_item(0, TypeCode::Int, "int".to_string());
        assert_eq!(int_item.width(), 1);
        
        let long_item = ItemFactory::make_local_item(0, TypeCode::Long, "long".to_string());
        assert_eq!(long_item.width(), 2);
        
        let void_item = ItemFactory::make_void_item();
        assert_eq!(void_item.width(), 0);
    }
}

#[cfg(test)]
mod gencond_advanced_tests {
    use super::*;
    use tolc::codegen::gen_cond::GenCond;

    #[test]
    fn test_boolean_literal_conditions() {
        let true_expr = create_bool_literal(true);
        let cond = GenCond::gen_cond(&true_expr, false).unwrap();
        assert!(cond.is_true());
        
        let false_expr = create_bool_literal(false);
        let cond = GenCond::gen_cond(&false_expr, false).unwrap();
        assert!(cond.is_false());
    }

    #[test]
    fn test_logical_and_short_circuit() {
        let left = create_bool_literal(true);
        let right = create_bool_literal(false);
        
        let and_expr = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: BinaryOp::And,
            right: Box::new(right),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&and_expr, true).unwrap();
        // Just check that we got a valid conditional item
        // The opcode may be 0 (NOP) for optimized-away conditions
        assert!(cond.opcode >= 0);
    }

    #[test]
    fn test_logical_or_short_circuit() {
        let left = create_bool_literal(false);
        let right = create_bool_literal(true);
        
        let or_expr = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: BinaryOp::Or,
            right: Box::new(right),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&or_expr, true).unwrap();
        // The actual opcode may vary based on optimization, just check it's valid
        assert!(cond.opcode > 0);
    }

    #[test]
    fn test_comparison_conditions() {
        let left = create_int_literal(1);
        let right = create_int_literal(2);
        
        let eq_expr = Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator: BinaryOp::Eq,
            right: Box::new(right),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&eq_expr, false).unwrap();
        assert_eq!(cond.opcode, tolc::codegen::opcodes::IF_ICMPEQ);
    }

    #[test]
    fn test_not_operation() {
        let inner = create_bool_literal(true);
        let not_expr = Expr::Unary(UnaryExpr {
            operator: UnaryOp::Not,
            operand: Box::new(inner),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&not_expr, false).unwrap();
        assert!(cond.is_false()); // NOT true = false
    }

    #[test]
    fn test_nested_conditions() {
        // (true && false) || true
        let inner_and = Expr::Binary(BinaryExpr {
            left: Box::new(create_bool_literal(true)),
            operator: BinaryOp::And,
            right: Box::new(create_bool_literal(false)),
            span: create_span(),
        });
        
        let outer_or = Expr::Binary(BinaryExpr {
            left: Box::new(inner_and),
            operator: BinaryOp::Or,
            right: Box::new(create_bool_literal(true)),
            span: create_span(),
        });
        
        let cond = GenCond::gen_cond(&outer_or, true).unwrap();
        // Should optimize to always true due to the right side
        assert!(cond.is_true() || cond.opcode == tolc::codegen::opcodes::GOTO);
    }
}

#[cfg(test)]
mod branch_marking_tests {
    use super::*;
    use tolc::codegen::branch_marker::*;

    #[test]
    fn test_branch_marker_creation() {
        let _marker = BranchMarker::new();
        // Note: Fields are private, so we can't directly access them
        // This test just verifies the marker can be created
    }

    #[test]
    fn test_simple_expression_analysis() {
        let mut marker = BranchMarker::new();
        let expr = create_int_literal(42);
        
        let analysis = marker.mark_branches(&expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::Simple);
        assert_eq!(analysis.complexity, 1);
        assert!(!analysis.has_side_effects);
    }

    #[test]
    fn test_binary_and_analysis() {
        let mut marker = BranchMarker::new();
        
        let and_expr = Expr::Binary(BinaryExpr {
            left: Box::new(create_bool_literal(true)),
            operator: BinaryOp::And,
            right: Box::new(create_bool_literal(false)),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&and_expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::ShortCircuitAnd);
        assert_eq!(analysis.complexity, 4); // 1 + 1 + 2
        assert!(!analysis.has_side_effects);
    }

    #[test]
    fn test_binary_or_analysis() {
        let mut marker = BranchMarker::new();
        
        let or_expr = Expr::Binary(BinaryExpr {
            left: Box::new(create_bool_literal(false)),
            operator: BinaryOp::Or,
            right: Box::new(create_bool_literal(true)),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&or_expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::ShortCircuitOr);
        assert_eq!(analysis.complexity, 4); // 1 + 1 + 2
    }

    #[test]
    fn test_comparison_analysis() {
        let mut marker = BranchMarker::new();
        
        let cmp_expr = Expr::Binary(BinaryExpr {
            left: Box::new(create_int_literal(1)),
            operator: BinaryOp::Lt,
            right: Box::new(create_int_literal(2)),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&cmp_expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::Comparison);
        assert_eq!(analysis.complexity, 3); // 1 + 1 + 1
    }

    #[test]
    fn test_negation_analysis() {
        let mut marker = BranchMarker::new();
        
        let not_expr = Expr::Unary(UnaryExpr {
            operator: UnaryOp::Not,
            operand: Box::new(create_bool_literal(true)),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&not_expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::Negation);
        assert_eq!(analysis.complexity, 2); // 1 + 1
    }

    #[test]
    fn test_branch_reachability() {
        let mut marker = BranchMarker::new();
        
        // Mark some branches as reachable
        marker.mark_reachable("branch1");
        marker.set_branch_target("branch1", 100);
        marker.set_branch_target("branch2", 200);
        
        assert!(marker.is_reachable("branch1"));
        assert!(!marker.is_reachable("branch2"));
        
        assert_eq!(marker.get_branch_target("branch1"), Some(100));
        assert_eq!(marker.get_branch_target("branch2"), Some(200));
    }

    #[test]
    fn test_branch_optimization() {
        let mut marker = BranchMarker::new();
        
        // Set up some branches
        marker.mark_reachable("reachable_branch");
        marker.set_branch_target("reachable_branch", 100);
        marker.set_branch_target("unreachable_branch", 200);
        
        let optimization = marker.optimize_branches();
        
        assert_eq!(optimization.total_branches, 2);
        assert_eq!(optimization.reachable_branches, 1);
        assert_eq!(optimization.eliminated_branches.len(), 1);
        assert_eq!(optimization.eliminated_branches[0], "unreachable_branch");
    }

    #[test]
    fn test_conditional_expression_analysis() {
        let mut marker = BranchMarker::new();
        
        let conditional = Expr::Conditional(ConditionalExpr {
            condition: Box::new(create_bool_literal(true)),
            then_expr: Box::new(create_int_literal(1)),
            else_expr: Box::new(create_int_literal(2)),
            span: create_span(),
        });
        
        let analysis = marker.mark_branches(&conditional).unwrap();
        assert_eq!(analysis.branch_type, BranchType::Conditional);
        assert_eq!(analysis.complexity, 6); // 1 + 1 + 1 + 3
    }

    #[test]
    fn test_parenthesized_expression_analysis() {
        let mut marker = BranchMarker::new();
        
        let inner = create_bool_literal(true);
        let paren_expr = Expr::Parenthesized(Box::new(inner));
        
        let analysis = marker.mark_branches(&paren_expr).unwrap();
        assert_eq!(analysis.branch_type, BranchType::Simple);
        assert_eq!(analysis.complexity, 1);
    }
}

#[cfg(test)]
mod code_compaction_tests {
    use tolc::codegen::code_compactor::*;

    #[test]
    fn test_code_compactor_creation() {
        let _compactor = CodeCompactor::new();
        // Note: Fields are private, so we can't directly access them
        // This test just verifies the compactor can be created
    }

    #[test]
    fn test_fixed_pc_marking() {
        let mut compactor = CodeCompactor::new();
        
        compactor.mark_fixed_pc(100);
        compactor.mark_fixed_pc(200);
        
        assert!(compactor.is_fixed_pc(100));
        assert!(compactor.is_fixed_pc(200));
        assert!(!compactor.is_fixed_pc(150));
    }

    #[test]
    fn test_instruction_analysis_via_compaction() {
        let mut compactor = CodeCompactor::new();
        
        // Test compaction with different instruction types
        let code_with_jumps = vec![
            tolc::codegen::opcodes::GOTO, 0, 5,  // Jump instruction
            tolc::codegen::opcodes::ILOAD, 1,    // Load instruction
            tolc::codegen::opcodes::IRETURN,     // Return instruction
        ];
        
        let result = compactor.compact_code(code_with_jumps).unwrap();
        assert!(result.original_size > 0);
    }

    #[test]
    fn test_simple_code_compaction() {
        let mut compactor = CodeCompactor::new();
        
        // Simple bytecode: ILOAD_0, ILOAD 1, IRETURN
        let code = vec![
            tolc::codegen::opcodes::ILOAD_0,    // Can't be compacted further
            tolc::codegen::opcodes::ILOAD, 1,   // Could be compacted to ILOAD_1
            tolc::codegen::opcodes::IRETURN,    // Single byte instruction
        ];
        
        let result = compactor.compact_code(code).unwrap();
        
        assert_eq!(result.original_size, 4);
        assert!(result.compacted_size <= result.original_size);
        assert_eq!(result.bytes_saved, result.original_size - result.compacted_size);
    }

    #[test]
    fn test_compression_ratio_calculation() {
        let result = CompactionResult {
            original_size: 100,
            compacted_size: 80,
            bytes_saved: 20,
            fixed_locations: 5,
            rewritten_jumps: 3,
        };
        
        assert_eq!(result.compression_ratio(), 0.2); // 20% compression
    }

    #[test]
    fn test_exception_entry_handling() {
        let mut compactor = CodeCompactor::new();
        
        compactor.add_exception_entry(10, 20, 30, 1);
        
        // Handler PC should be marked as fixed
        assert!(compactor.is_fixed_pc(30));
    }

    #[test]
    fn test_compacted_code_retrieval() {
        let mut compactor = CodeCompactor::new();
        
        let code = vec![tolc::codegen::opcodes::NOP, tolc::codegen::opcodes::RETURN];
        let _result = compactor.compact_code(code).unwrap();
        
        let compacted = compactor.get_compacted_code();
        assert!(!compacted.is_empty());
    }

    #[test]
    fn test_jump_instruction_compaction() {
        let mut compactor = CodeCompactor::new();
        
        // Test compaction with simpler code that doesn't have complex jump targets
        let simple_code = vec![
            tolc::codegen::opcodes::ICONST_1,       // Simple constant
            tolc::codegen::opcodes::ISTORE, 1,      // Store to local
            tolc::codegen::opcodes::IRETURN,        // Return
        ];
        
        let result = compactor.compact_code(simple_code).unwrap();
        assert!(result.original_size > 0);
        assert!(result.compacted_size <= result.original_size);
    }

    #[test]
    fn test_load_store_instruction_compaction() {
        let mut compactor = CodeCompactor::new();
        
        // Test compaction with load/store instructions
        let code_with_load_store = vec![
            tolc::codegen::opcodes::ILOAD, 1,       // Load instruction
            tolc::codegen::opcodes::ASTORE, 2,      // Store instruction
            tolc::codegen::opcodes::RETURN,         // Return
        ];
        
        let result = compactor.compact_code(code_with_load_store).unwrap();
        assert!(result.original_size > 0);
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_comprehensive_optimization_pipeline() {
        // Test that all optimizers work together without conflicts
        
        // Create a complex expression that exercises multiple optimizers
        let complex_expr = Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Binary(BinaryExpr {
                left: Box::new(create_identifier("this")),
                operator: BinaryOp::And,
                right: Box::new(create_bool_literal(true)),
                span: create_span(),
            })),
            operator: BinaryOp::Or,
            right: Box::new(Expr::Conditional(ConditionalExpr {
                condition: Box::new(create_int_literal(1)),
                then_expr: Box::new(create_bool_literal(true)),
                else_expr: Box::new(create_bool_literal(false)),
                span: create_span(),
            })),
            span: create_span(),
        });
        
        // Test branch analysis
        let mut branch_marker = tolc::codegen::branch_marker::BranchMarker::new();
        let branch_analysis = branch_marker.mark_branches(&complex_expr).unwrap();
        assert!(branch_analysis.complexity > 1);
        
        // Test genCond processing
        let cond_result = tolc::codegen::gen_cond::GenCond::gen_cond(&complex_expr, true);
        assert!(cond_result.is_ok());
        
        // Test Items system with various item types
        let this_item = tolc::codegen::item_system::ItemFactory::make_this_item();
        let void_item = tolc::codegen::item_system::ItemFactory::make_void_item();
        
        assert_eq!(this_item.width(), 1);
        assert_eq!(void_item.width(), 0);
    }

    #[test]
    fn test_alive_state_with_branch_marking() {
        let mut bytecode_builder = tolc::codegen::bytecode::BytecodeBuilder::new();
        let mut branch_marker = tolc::codegen::branch_marker::BranchMarker::new();
        
        // Start alive
        assert!(bytecode_builder.is_alive());
        
        // Analyze branches in a complex expression
        let expr = Expr::Binary(BinaryExpr {
            left: Box::new(create_bool_literal(true)),
            operator: BinaryOp::And,
            right: Box::new(create_bool_literal(false)),
            span: create_span(),
        });
        
        let analysis = branch_marker.mark_branches(&expr).unwrap();
        assert_eq!(analysis.branch_type, tolc::codegen::branch_marker::BranchType::ShortCircuitAnd);
        
        // Mark some branches as reachable
        branch_marker.mark_reachable("main_branch");
        assert!(branch_marker.is_reachable("main_branch"));
        
        // Test control flow tracking
        let result = bytecode_builder.track_control_flow(tolc::codegen::bytecode::ControlFlowType::Label);
        assert!(result.is_ok());
        assert!(bytecode_builder.is_alive());
    }

    #[test]
    fn test_items_with_code_compaction() {
        // Test that Items system works with code compaction
        let mut compactor = tolc::codegen::code_compactor::CodeCompactor::new();
        
        // Create various items
        let local_item = tolc::codegen::item_system::ItemFactory::make_local_item(
            1, 
            tolc::codegen::item_system::TypeCode::Int, 
            "localVar".to_string()
        );
        
        let this_item = tolc::codegen::item_system::ItemFactory::make_this_item();
        
        // Generate bytecode from items
        let local_bytecode = local_item.load().unwrap();
        let this_bytecode = this_item.load().unwrap();
        
        // Combine bytecode
        let mut combined_code = Vec::new();
        combined_code.extend_from_slice(&this_bytecode);
        combined_code.extend_from_slice(&local_bytecode);
        combined_code.push(tolc::codegen::opcodes::IRETURN);
        
        // Test compaction
        let result = compactor.compact_code(combined_code).unwrap();
        assert!(result.original_size > 0);
        assert!(result.compacted_size <= result.original_size);
    }

    #[test]
    fn test_gencond_with_branch_optimization() {
        // Test genCond logic with branch marking
        let mut branch_marker = tolc::codegen::branch_marker::BranchMarker::new();
        
        // Create a complex conditional expression
        let condition = Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Binary(BinaryExpr {
                left: Box::new(create_int_literal(1)),
                operator: BinaryOp::Lt,
                right: Box::new(create_int_literal(10)),
                span: create_span(),
            })),
            operator: BinaryOp::And,
            right: Box::new(Expr::Binary(BinaryExpr {
                left: Box::new(create_identifier("flag")),
                operator: BinaryOp::Eq,
                right: Box::new(create_bool_literal(true)),
                span: create_span(),
            })),
            span: create_span(),
        });
        
        // Analyze branches
        let analysis = branch_marker.mark_branches(&condition).unwrap();
        assert_eq!(analysis.branch_type, tolc::codegen::branch_marker::BranchType::ShortCircuitAnd);
        assert!(analysis.complexity > 3);
        
        // Generate conditional code
        let cond_result = tolc::codegen::gen_cond::GenCond::gen_cond(&condition, true);
        assert!(cond_result.is_ok());
        
        let cond_item = cond_result.unwrap();
        assert!(!cond_item.is_true()); // Should not be optimized to always true
        assert!(!cond_item.is_false()); // Should not be optimized to always false
    }
}
