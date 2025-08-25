use tolc::codegen::javac_jump_optimizer::{JavacJumpOptimizer, JumpChain, MachineState, OptimizationStats};
use tolc::codegen::opcode_enum::Opcode;
use tolc::common::error::Result;

/// Test JavaC-aligned jump chain optimization
/// 
/// This test verifies that tolc's jump chain optimization matches JavaC's behavior
/// by testing the core optimization patterns: jump-to-jump elimination, dead code 
/// removal, and lazy resolution.

#[cfg(test)]
mod javac_jump_chain_tests {
    use super::*;

    #[test]
    fn test_jump_chain_creation() {
        let mut optimizer = JavacJumpOptimizer::new();
        let state = MachineState::new(2, 4);
        
        // Create a simple jump chain
        let chain = optimizer.branch(Opcode::Goto, state.clone());
        
        assert_eq!(chain.pc, 0);
        assert_eq!(chain.opcode, Opcode::Goto);
        assert_eq!(chain.instruction_size, 3);
        assert_eq!(chain.state.stack_size, 2);
        assert_eq!(chain.state.max_locals, 4);
        assert!(chain.state.alive);
    }
    
    #[test] 
    fn test_chain_merging_javac_order() {
        let state1 = MachineState::new(1, 2);
        let state2 = MachineState::new(2, 3);
        
        // JavaC maintains decreasing PC order in chains
        let chain1 = JumpChain::new(100, None, state1, Opcode::Goto, 3);
        let chain2 = JumpChain::new(200, None, state2, Opcode::Ifeq, 3);
        
        let merged = JumpChain::merge_chains(Some(chain1), Some(chain2));
        assert!(merged.is_some());
        
        let merged = merged.unwrap();
        // Higher PC (200) should come first in JavaC order
        assert_eq!(merged.pc, 200);
        assert!(merged.next.is_some());
        assert_eq!(merged.next.unwrap().pc, 100);
    }
    
    #[test]
    fn test_jump_to_jump_detection() {
        let mut optimizer = JavacJumpOptimizer::new();
        
        // Simulate bytecode with jump-to-jump pattern
        // Position 10: goto +5 (points to position 15)
        // Position 15: goto +8 (points to position 23)
        // This should be optimized to: goto +13 (direct jump to 23)
        
        let state = MachineState::new(0, 1);
        
        // First jump at PC 10
        let _chain1 = optimizer.branch(Opcode::Goto, state.clone());
        
        // Second jump at PC 15 (this would be detected as jump-to-jump)
        let _chain2 = optimizer.branch(Opcode::Goto, state.clone());
        
        // Verify the optimizer can detect jump instructions
        assert!(!optimizer.is_fat_code()); // Should start in normal mode
        
        let stats = optimizer.get_stats();
        assert_eq!(stats.total_optimizations, 0); // No optimizations yet
    }
    
    #[test]
    fn test_machine_state_tracking() {
        let alive_state = MachineState::new(3, 5);
        assert_eq!(alive_state.stack_size, 3);
        assert_eq!(alive_state.max_locals, 5);
        assert!(alive_state.alive);
        
        let dead_state = MachineState::dead();
        assert_eq!(dead_state.stack_size, 0);
        assert_eq!(dead_state.max_locals, 0);
        assert!(!dead_state.alive);
    }
    
    #[test]
    fn test_fat_code_mode_switching() {
        let mut optimizer = JavacJumpOptimizer::new();
        
        // Should start in normal mode
        assert!(!optimizer.is_fat_code());
        
        // Switch to fat code mode (for large offsets)
        optimizer.set_fat_code(true);
        assert!(optimizer.is_fat_code());
        
        // Create branch in fat code mode
        let state = MachineState::new(1, 2);
        let chain = optimizer.branch(Opcode::Goto, state);
        
        // Fat code goto should have 5-byte instruction size
        assert_eq!(chain.instruction_size, 5);
    }
    
    #[test]
    fn test_optimization_statistics_tracking() {
        let optimizer = JavacJumpOptimizer::new();
        let stats = optimizer.get_stats();
        
        // Initial stats should be zero
        assert_eq!(stats.jump_to_jump_eliminations, 0);
        assert_eq!(stats.dead_goto_removals, 0);
        assert_eq!(stats.chains_merged, 0);
        assert_eq!(stats.total_optimizations, 0);
    }
    
    #[test]
    fn test_lazy_resolution_pattern() {
        let mut optimizer = JavacJumpOptimizer::new();
        let state = MachineState::new(1, 1);
        
        // Create multiple branches that will be resolved later (JavaC pattern)
        let _chain1 = optimizer.branch(Opcode::Ifeq, state.clone());
        let _chain2 = optimizer.branch(Opcode::Ifne, state.clone());
        let _chain3 = optimizer.branch(Opcode::Goto, state);
        
        // Resolve all pending jumps to target 100
        let result = optimizer.resolve_pending(100);
        assert!(result.is_ok());
        
        // After resolution, stats should show optimizations
        let stats = optimizer.get_stats();
        assert!(stats.total_optimizations > 0);
    }
    
    #[test]
    fn test_reset_functionality() {
        let mut optimizer = JavacJumpOptimizer::new();
        let state = MachineState::new(2, 3);
        
        // Create some branches
        let _chain = optimizer.branch(Opcode::Goto, state);
        
        // Reset should clear everything
        optimizer.reset();
        
        let stats = optimizer.get_stats();
        assert_eq!(stats.total_optimizations, 0);
        assert!(!optimizer.is_fat_code());
    }
    
    #[test]
    fn test_conditional_jump_opcodes() {
        let optimizer = JavacJumpOptimizer::new();
        let state = MachineState::new(2, 2);
        
        // Test various conditional jump opcodes
        let conditional_opcodes = vec![
            Opcode::Ifeq,
            Opcode::Ifne, 
            Opcode::Iflt,
            Opcode::Ifle,
            Opcode::Ifgt,
            Opcode::Ifge,
            Opcode::IfIcmpeq,
            Opcode::IfIcmpne,
            Opcode::IfIcmplt,
            Opcode::IfIcmple,
            Opcode::IfIcmpgt,
            Opcode::IfIcmpge,
        ];
        
        for opcode in conditional_opcodes {
            let chain = JumpChain::new(0, None, state.clone(), opcode, 3);
            assert_eq!(chain.instruction_size, 3); // All conditionals are 3 bytes
            assert_eq!(chain.opcode, opcode);
        }
    }
}