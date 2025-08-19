use tolc::codegen::fatcode_manager::{FatcodeManager, JumpType};
use tolc::codegen::pending_jumps::PendingJumpsManager;
use tolc::codegen::fixed_pc_manager::{FixedPcManager, FixedPcReason};
use tolc::codegen::opcode_enum::Opcode;

#[test]
fn test_fatcode_mechanism() {
    let mut fatcode_manager = FatcodeManager::new();
    
    // Test normal jump (should not trigger fatcode)
    let jump_id = fatcode_manager.register_jump(100, JumpType::Goto);
    fatcode_manager.resolve_jump(jump_id, 200).unwrap();
    
    assert!(!fatcode_manager.fatcode);
    assert_eq!(fatcode_manager.max_jump_distance, 100);
    
    // Test long jump (should trigger fatcode)
    let long_jump_id = fatcode_manager.register_jump(1000, JumpType::Goto);
    fatcode_manager.resolve_jump(long_jump_id, 40000).unwrap();
    
    assert!(fatcode_manager.max_jump_distance > 32767);
    
    // Test fatcode detection
    assert!(fatcode_manager.check_fatcode_needed(40000));
    
    // Enable fatcode
    fatcode_manager.enable_fatcode();
    assert!(fatcode_manager.fatcode);
    
    // Test opcode selection
    assert_eq!(fatcode_manager.get_jump_opcode(jump_id, Opcode::Goto).unwrap(), Opcode::GotoW);
    
    println!("✅ Fatcode mechanism test passed");
}

#[test]
fn test_pending_jumps_optimization() {
    let mut pending_jumps = PendingJumpsManager::new();
    
    // Create a jump chain
    let chain_id = pending_jumps.create_chain_with_jump(100, Opcode::Goto, 3);
    
    // Initially unresolved
    assert!(!pending_jumps.is_chain_resolved(chain_id));
    assert!(pending_jumps.has_pending_jumps());
    assert_eq!(pending_jumps.pending_jump_count(), 1);
    
    // Add more jumps to the chain
    pending_jumps.add_jump_to_chain(chain_id, 200, Opcode::Goto, 3).unwrap();
    pending_jumps.add_jump_to_chain(chain_id, 300, Opcode::Goto, 3).unwrap();
    
    assert_eq!(pending_jumps.pending_jump_count(), 3);
    
    // Resolve the chain
    let patches = pending_jumps.resolve_chain(chain_id, 500).unwrap();
    
    // Should be resolved now
    assert!(pending_jumps.is_chain_resolved(chain_id));
    assert!(!pending_jumps.has_pending_jumps());
    assert_eq!(pending_jumps.get_chain_target(chain_id), Some(500));
    
    // Should have patches for all jumps
    assert_eq!(patches.len(), 3);
    
    // Test jump-to-jump optimization
    let chain1 = pending_jumps.create_chain_with_jump(600, Opcode::Goto, 3);
    let chain2 = pending_jumps.create_chain_with_jump(700, Opcode::Goto, 3);
    
    // Merge chains
    pending_jumps.merge_chains(chain1, chain2).unwrap();
    
    // Chain1 should be removed, chain2 should have both jumps
    assert!(!pending_jumps.pending_chains.contains_key(&chain1));
    
    println!("✅ Pending jumps optimization test passed");
}

#[test]
fn test_enhanced_alive_state_management() {
    use tolc::codegen::bytecode::{BytecodeBuilder, ControlFlowType};
    
    let mut bytecode_builder = BytecodeBuilder::new();
    let pending_jumps = PendingJumpsManager::new();
    
    // Initially alive
    assert!(bytecode_builder.is_alive());
    assert!(bytecode_builder.is_alive_with_pending_jumps(Some(&pending_jumps)));
    
    // Mark as dead
    bytecode_builder.mark_dead();
    assert!(!bytecode_builder.is_alive());
    
    // But should still be alive if there are pending jumps
    let mut pending_jumps_with_jumps = PendingJumpsManager::new();
    let _chain_id = pending_jumps_with_jumps.create_chain_with_jump(100, Opcode::Goto, 3);
    
    assert!(bytecode_builder.is_alive_with_pending_jumps(Some(&pending_jumps_with_jumps)));
    
    // Test control flow tracking
    bytecode_builder.track_control_flow(ControlFlowType::Terminal).unwrap();
    assert!(!bytecode_builder.is_alive());
    
    // Labels should make code alive again
    bytecode_builder.track_control_flow(ControlFlowType::Label).unwrap();
    assert!(bytecode_builder.is_alive());
    assert!(bytecode_builder.is_fixed_pc());
    
    println!("✅ Enhanced alive state management test passed");
}

#[test]
fn test_fixed_pc_mechanism() {
    let mut fixed_pc_manager = FixedPcManager::new();
    
    // Initially no fixed PCs
    assert!(!fixed_pc_manager.is_fixed(100));
    assert!(fixed_pc_manager.can_compact_at(100));
    
    // Mark a jump target as fixed
    fixed_pc_manager.mark_jump_target(100, 50, "goto");
    
    // Should be fixed now
    assert!(fixed_pc_manager.is_fixed(100));
    assert!(!fixed_pc_manager.can_compact_at(100));
    
    // Check reasons
    let reasons = fixed_pc_manager.get_fixed_reasons(100).unwrap();
    assert_eq!(reasons.len(), 1);
    assert!(matches!(reasons[0], FixedPcReason::JumpTarget { .. }));
    
    // Mark multiple types at same PC
    fixed_pc_manager.mark_line_number(100, 42);
    fixed_pc_manager.mark_stack_map_frame(100);
    
    let reasons = fixed_pc_manager.get_fixed_reasons(100).unwrap();
    assert_eq!(reasons.len(), 3);
    
    // Test current fixed flag
    fixed_pc_manager.set_current_fixed(true);
    assert!(fixed_pc_manager.is_current_fixed());
    
    fixed_pc_manager.apply_current_fixed(200);
    assert!(fixed_pc_manager.is_fixed(200));
    assert!(!fixed_pc_manager.is_current_fixed()); // Should be reset
    
    // Test range operations
    let fixed_pcs = fixed_pc_manager.get_fixed_pcs_in_range(50, 250);
    assert!(fixed_pcs.contains(&100));
    assert!(fixed_pcs.contains(&200));
    
    // Test statistics
    let stats = fixed_pc_manager.get_stats();
    assert_eq!(stats.total_fixed_pcs, 2);
    assert_eq!(stats.jump_targets, 1);
    
    println!("✅ Fixed PC mechanism test passed");
}

#[test]
fn test_integration_all_four_features() {
    use tolc::codegen::method_writer::MethodWriter;
    
    let mut method_writer = MethodWriter::new();
    
    // Test fatcode integration
    let jump_type = tolc::codegen::fatcode_manager::JumpType::Goto;
    let jump_id = method_writer.register_jump(jump_type).unwrap();
    method_writer.resolve_jump(jump_id, 1000).unwrap();
    
    // Test pending jumps integration
    let chain_id = method_writer.create_pending_jump_chain();
    method_writer.add_jump_to_pending_chain(chain_id, Opcode::Goto, 3).unwrap();
    let _patches = method_writer.resolve_pending_jump_chain(chain_id, 2000).unwrap();
    
    // Test fixed PC integration
    method_writer.mark_current_pc_fixed(FixedPcReason::Label {
        label_name: "test_label".to_string()
    });
    method_writer.mark_jump_target_fixed(1000, 500, "conditional");
    
    // Test alive state integration
    assert!(method_writer.is_code_alive());
    assert!(method_writer.should_emit_instruction());
    
    // Test fatcode detection
    let _fatcode_enabled = method_writer.check_and_enable_fatcode().unwrap();
    
    println!("✅ Integration test for all four features passed");
}

#[test]
fn test_opcode_enum_functionality() {
    // Test opcode classification
    assert!(Opcode::Ifeq.is_conditional_branch());
    assert!(Opcode::Goto.is_unconditional_branch());
    assert!(Opcode::Return.is_return());
    assert!(Opcode::Athrow.is_terminal());
    assert!(Opcode::GotoW.is_wide());
    
    // Test instruction sizes
    assert_eq!(Opcode::Nop.instruction_size(), 1);
    assert_eq!(Opcode::Goto.instruction_size(), 3);
    assert_eq!(Opcode::GotoW.instruction_size(), 5);
    assert_eq!(Opcode::Ifeq.instruction_size(), 3);
    
    // Test byte conversion
    assert_eq!(Opcode::Nop.to_byte(), 0x00);
    assert_eq!(Opcode::Goto.to_byte(), 0xa7);
    assert_eq!(Opcode::Return.to_byte(), 0xb1);
    
    // Test from byte conversion
    assert_eq!(Opcode::from(0x00), Opcode::Nop);
    assert_eq!(Opcode::from(0xa7), Opcode::Goto);
    assert_eq!(Opcode::from(0xb1), Opcode::Return);
    
    println!("✅ Opcode enum functionality test passed");
}

#[test]
fn test_javac_style_optimizations() {
    let mut fatcode_manager = FatcodeManager::new();
    let mut pending_jumps = PendingJumpsManager::new();
    let mut fixed_pc_manager = FixedPcManager::new();
    
    // Simulate a complex method with many jumps
    let mut jump_ids = Vec::new();
    
    // Register multiple jumps
    for i in 0..10 {
        let pc = i * 1000;
        let jump_id = fatcode_manager.register_jump(pc, JumpType::Conditional(Opcode::Ifeq));
        jump_ids.push(jump_id);
        
        // Mark some as fixed
        if i % 2 == 0 {
            fixed_pc_manager.mark_jump_target(pc + 500, pc, "conditional");
        }
    }
    
    // Create pending jump chains
    let mut chain_ids = Vec::new();
    for i in 0..5 {
        let chain_id = pending_jumps.create_chain_with_jump(i * 2000, Opcode::Goto, 3);
        chain_ids.push(chain_id);
    }
    
    // Resolve some jumps (simulate forward resolution)
    for (i, &jump_id) in jump_ids.iter().enumerate() {
        let target = (i + 1) * 1000 + 500;
        fatcode_manager.resolve_jump(jump_id, target as u32).unwrap();
    }
    
    // Resolve some chains
    for (i, &chain_id) in chain_ids.iter().enumerate() {
        if i < 3 {
            let target = (i + 10) * 1000;
            pending_jumps.resolve_chain(chain_id, target as u32).unwrap();
        }
    }
    
    // Test optimization
    let optimizations = pending_jumps.optimize_jump_chains();
    println!("Applied {} jump-to-jump optimizations", optimizations);
    
    // Check if fatcode is needed
    if fatcode_manager.check_fatcode_needed(50000) {
        fatcode_manager.enable_fatcode();
        println!("Enabled fatcode mode for large method");
    }
    
    // Generate report
    let report = fixed_pc_manager.generate_report();
    println!("Fixed PC Report:\n{}", report);
    
    // Validate consistency
    fixed_pc_manager.validate().unwrap();
    
    println!("✅ JavaC-style optimizations test passed");
}
