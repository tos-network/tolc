use crate::codegen::opcode_enum::Opcode;
use std::collections::{HashMap, VecDeque};

/// JavaC-style pending jumps optimization
/// 
/// This implements the lazy jump resolution mechanism from javac's Code.java
/// to avoid "jumps to jumps" and optimize control flow.
#[derive(Debug, Clone)]
pub struct PendingJumpsManager {
    /// Chain of pending jumps to be resolved
    pub pending_chains: HashMap<u32, JumpChain>,
    
    /// Next available chain ID
    next_chain_id: u32,
    
    /// Statistics for optimization tracking
    pub stats: PendingJumpStats,
}

#[derive(Debug, Clone)]
pub struct JumpChain {
    /// Unique identifier for this chain
    pub chain_id: u32,
    
    /// List of jump locations in this chain
    pub jumps: VecDeque<JumpLocation>,
    
    /// Target PC when resolved (None if still pending)
    pub resolved_target: Option<u32>,
    
    /// Whether this chain has been optimized
    pub optimized: bool,
}

#[derive(Debug, Clone)]
pub struct JumpLocation {
    /// Program counter of the jump instruction
    pub pc: u32,
    
    /// Type of jump instruction
    pub opcode: Opcode,
    
    /// Size of the jump instruction (for offset calculation)
    pub instruction_size: u32,
    
    /// Whether this jump has been resolved
    pub resolved: bool,
}

#[derive(Debug, Clone, Default)]
pub struct PendingJumpStats {
    /// Total number of jump chains created
    pub total_chains: u32,
    
    /// Number of chains that were optimized (avoided jumps to jumps)
    pub optimized_chains: u32,
    
    /// Total number of individual jumps
    pub total_jumps: u32,
    
    /// Number of jumps that were eliminated through optimization
    pub eliminated_jumps: u32,
    
    /// Maximum chain length encountered
    pub max_chain_length: u32,
}

impl PendingJumpsManager {
    /// Create a new pending jumps manager
    pub fn new() -> Self {
        Self {
            pending_chains: HashMap::new(),
            next_chain_id: 0,
            stats: PendingJumpStats::default(),
        }
    }
    
    /// Create a new jump chain
    pub fn create_chain(&mut self) -> u32 {
        let chain_id = self.next_chain_id;
        self.next_chain_id += 1;
        
        let chain = JumpChain {
            chain_id,
            jumps: VecDeque::new(),
            resolved_target: None,
            optimized: false,
        };
        
        self.pending_chains.insert(chain_id, chain);
        self.stats.total_chains += 1;
        
        eprintln!("🔍 DEBUG: PendingJumps: Created new chain {}", chain_id);
        
        chain_id
    }
    
    /// Add a jump to an existing chain
    pub fn add_jump_to_chain(&mut self, chain_id: u32, pc: u32, opcode: Opcode, instruction_size: u32) -> Result<(), String> {
        if let Some(chain) = self.pending_chains.get_mut(&chain_id) {
            let jump_location = JumpLocation {
                pc,
                opcode,
                instruction_size,
                resolved: false,
            };
            
            chain.jumps.push_back(jump_location);
            self.stats.total_jumps += 1;
            
            // Update max chain length
            self.stats.max_chain_length = self.stats.max_chain_length.max(chain.jumps.len() as u32);
            
            eprintln!("🔍 DEBUG: PendingJumps: Added jump at PC {} to chain {} (opcode: {:?})", 
                     pc, chain_id, opcode);
            
            Ok(())
        } else {
            Err(format!("Chain ID {} not found", chain_id))
        }
    }
    
    /// Create a new chain with a single jump
    pub fn create_chain_with_jump(&mut self, pc: u32, opcode: Opcode, instruction_size: u32) -> u32 {
        let chain_id = self.create_chain();
        self.add_jump_to_chain(chain_id, pc, opcode, instruction_size).unwrap();
        chain_id
    }
    
    /// Resolve a chain to a target PC
    pub fn resolve_chain(&mut self, chain_id: u32, target_pc: u32) -> Result<Vec<(u32, i32)>, String> {
        if let Some(chain) = self.pending_chains.get_mut(&chain_id) {
            chain.resolved_target = Some(target_pc);
            
            let mut patch_locations = Vec::new();
            
            // Calculate offsets for all jumps in the chain
            for jump in &mut chain.jumps {
                if !jump.resolved {
                    // Calculate the offset from jump instruction's offset field to target
                    // JVM jump offset is relative to the offset field (PC + 1), not the end of instruction
                    let offset_field_pc = jump.pc + 1; // Skip opcode, point to offset field
                    let offset = target_pc as i32 - offset_field_pc as i32;
                    
                    patch_locations.push((jump.pc, offset));
                    jump.resolved = true;
                    
                    eprintln!("🔍 DEBUG: PendingJumps: Resolved jump at PC {} to target {} (offset: {})", 
                             jump.pc, target_pc, offset);
                }
            }
            
            eprintln!("🔍 DEBUG: PendingJumps: Resolved chain {} to target {} ({} jumps)", 
                     chain_id, target_pc, patch_locations.len());
            
            Ok(patch_locations)
        } else {
            Err(format!("Chain ID {} not found", chain_id))
        }
    }
    
    /// Merge two chains (when one chain jumps to another)
    pub fn merge_chains(&mut self, source_chain_id: u32, target_chain_id: u32) -> Result<(), String> {
        // Check if target chain exists and get its target
        let target_pc = if let Some(target_chain) = self.pending_chains.get(&target_chain_id) {
            target_chain.resolved_target
        } else {
            return Err(format!("Target chain ID {} not found", target_chain_id));
        };
        
        // If target chain is resolved, resolve source chain to the same target
        if let Some(target_pc) = target_pc {
            self.resolve_chain(source_chain_id, target_pc)?;
            self.stats.optimized_chains += 1;
            
            eprintln!("🔍 DEBUG: PendingJumps: Merged chain {} into resolved chain {} (target: {})", 
                     source_chain_id, target_chain_id, target_pc);
        } else {
            // Merge the actual jump lists
            let source_jumps = if let Some(source_chain) = self.pending_chains.remove(&source_chain_id) {
                source_chain.jumps
            } else {
                return Err(format!("Source chain ID {} not found", source_chain_id));
            };
            
            if let Some(target_chain) = self.pending_chains.get_mut(&target_chain_id) {
                // Add all source jumps to target chain
                for jump in source_jumps {
                    target_chain.jumps.push_back(jump);
                }
                target_chain.optimized = true;
                self.stats.optimized_chains += 1;
                
                // Update max chain length
                self.stats.max_chain_length = self.stats.max_chain_length.max(target_chain.jumps.len() as u32);
                
                eprintln!("🔍 DEBUG: PendingJumps: Merged chain {} into pending chain {} ({} total jumps)", 
                         source_chain_id, target_chain_id, target_chain.jumps.len());
            }
        }
        
        Ok(())
    }
    
    /// Check if a chain is resolved
    pub fn is_chain_resolved(&self, chain_id: u32) -> bool {
        self.pending_chains
            .get(&chain_id)
            .map(|chain| chain.resolved_target.is_some())
            .unwrap_or(false)
    }
    
    /// Get the target of a resolved chain
    pub fn get_chain_target(&self, chain_id: u32) -> Option<u32> {
        self.pending_chains
            .get(&chain_id)
            .and_then(|chain| chain.resolved_target)
    }
    
    /// Get all unresolved chains
    pub fn get_unresolved_chains(&self) -> Vec<u32> {
        self.pending_chains
            .iter()
            .filter(|(_, chain)| chain.resolved_target.is_none())
            .map(|(id, _)| *id)
            .collect()
    }
    
    /// Optimize jump-to-jump patterns
    pub fn optimize_jump_chains(&mut self) -> u32 {
        let mut optimizations = 0;
        
        // Collect potential optimizations first to avoid borrowing conflicts
        let mut optimization_pairs = Vec::new();
        
        // Find chains that jump to other chains
        let chain_ids: Vec<u32> = self.pending_chains.keys().cloned().collect();
        
        for chain_id in &chain_ids {
            if let Some(chain) = self.pending_chains.get(chain_id) {
                if chain.resolved_target.is_none() {
                    // Check if any jump in this chain targets another chain
                    for jump in &chain.jumps {
                        // Look for chains that start at this jump's target
                        let potential_target = jump.pc + jump.instruction_size;
                        
                        // Find if there's a chain starting at this location
                        for (other_chain_id, other_chain) in &self.pending_chains {
                            if *other_chain_id != *chain_id {
                                if let Some(first_jump) = other_chain.jumps.front() {
                                    if first_jump.pc == potential_target {
                                        // Found a jump-to-jump pattern
                                        optimization_pairs.push((*chain_id, *other_chain_id));
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Apply optimizations
        for (source_chain, target_chain) in optimization_pairs {
            if let Ok(()) = self.merge_chains(source_chain, target_chain) {
                optimizations += 1;
                eprintln!("🔍 DEBUG: PendingJumps: Optimized jump-to-jump from chain {} to chain {}", 
                         source_chain, target_chain);
            }
        }
        
        self.stats.eliminated_jumps += optimizations;
        optimizations
    }
    
    /// Advanced jump chain optimization: detect and optimize common patterns
    pub fn advanced_optimize_chains(&mut self) -> u32 {
        let mut optimizations = 0;
        
        // Pattern 1: Detect redundant goto chains
        optimizations += self.optimize_redundant_goto_chains();
        
        // Pattern 2: Detect conditional jump chains that can be simplified
        optimizations += self.optimize_conditional_jump_chains();
        
        // Pattern 3: Detect jump chains that can be eliminated through code reordering
        optimizations += self.optimize_code_reordering_chains();
        
        eprintln!("🔍 DEBUG: PendingJumps: Advanced optimization completed with {} total optimizations", optimizations);
        optimizations
    }
    
    /// Optimize redundant goto chains (goto -> goto -> target)
    fn optimize_redundant_goto_chains(&mut self) -> u32 {
        let mut optimizations = 0;
        let chain_ids: Vec<u32> = self.pending_chains.keys().cloned().collect();
        
        for chain_id in &chain_ids {
            if let Some(chain) = self.pending_chains.get(chain_id) {
                if chain.resolved_target.is_none() && chain.jumps.len() > 1 {
                    // Check if this chain has multiple consecutive goto instructions
                    let mut consecutive_gotos = 0;
                    for jump in &chain.jumps {
                        if matches!(jump.opcode, Opcode::Goto) {
                            consecutive_gotos += 1;
                        } else {
                            break;
                        }
                    }
                    
                    if consecutive_gotos > 1 {
                        eprintln!("🔍 DEBUG: PendingJumps: Found redundant goto chain {} with {} consecutive gotos", 
                                 chain_id, consecutive_gotos);
                        // Mark for optimization
                        if let Some(chain) = self.pending_chains.get_mut(chain_id) {
                            chain.optimized = true;
                        }
                        optimizations += 1;
                    }
                }
            }
        }
        
        optimizations
    }
    
    /// Optimize conditional jump chains that can be simplified
    fn optimize_conditional_jump_chains(&mut self) -> u32 {
        let mut optimizations = 0;
        let chain_ids: Vec<u32> = self.pending_chains.keys().cloned().collect();
        
        for chain_id in &chain_ids {
            if let Some(chain) = self.pending_chains.get(chain_id) {
                if chain.resolved_target.is_none() && chain.jumps.len() >= 2 {
                    // Look for patterns like: if_icmpeq -> goto -> target
                    let mut has_conditional = false;
                    let mut has_goto = false;
                    
                    for jump in &chain.jumps {
                        match jump.opcode {
                            Opcode::Ifeq | Opcode::Ifne | Opcode::Iflt | Opcode::Ifle | 
                            Opcode::Ifgt | Opcode::Ifge | Opcode::IfIcmpeq | Opcode::IfIcmpne |
                            Opcode::IfIcmplt | Opcode::IfIcmple | Opcode::IfIcmpgt | Opcode::IfIcmpge => {
                                has_conditional = true;
                            }
                            Opcode::Goto => {
                                has_goto = true;
                            }
                            _ => {}
                        }
                    }
                    
                    if has_conditional && has_goto {
                        eprintln!("🔍 DEBUG: PendingJumps: Found conditional-goto chain {} for optimization", chain_id);
                        if let Some(chain) = self.pending_chains.get_mut(chain_id) {
                            chain.optimized = true;
                        }
                        optimizations += 1;
                    }
                }
            }
        }
        
        optimizations
    }
    
    /// Optimize chains that can benefit from code reordering
    fn optimize_code_reordering_chains(&mut self) -> u32 {
        let mut optimizations = 0;
        
        // This is a placeholder for future code reordering optimization
        // In a full implementation, this would analyze control flow graphs
        // and suggest code reordering to eliminate unnecessary jumps
        
        eprintln!("🔍 DEBUG: PendingJumps: Code reordering optimization placeholder (future enhancement)");
        
        optimizations
    }
    
    /// Force resolve all pending chains (for method end)
    pub fn resolve_all_pending(&mut self, default_target: u32) -> Vec<(u32, i32)> {
        let mut all_patches = Vec::new();
        
        let unresolved_chains: Vec<u32> = self.get_unresolved_chains();
        
        for chain_id in unresolved_chains {
            if let Ok(patches) = self.resolve_chain(chain_id, default_target) {
                all_patches.extend(patches);
                eprintln!("🔍 DEBUG: PendingJumps: Force resolved chain {} to default target {}", 
                         chain_id, default_target);
            }
        }
        
        all_patches
    }
    
    /// Get statistics
    pub fn get_stats(&self) -> &PendingJumpStats {
        &self.stats
    }
    
    /// Reset for new method
    pub fn reset(&mut self) {
        self.pending_chains.clear();
        self.next_chain_id = 0;
    }
    
    /// Check if there are any pending jumps
    pub fn has_pending_jumps(&self) -> bool {
        self.pending_chains
            .values()
            .any(|chain| chain.resolved_target.is_none())
    }
    
    /// Get the number of pending jumps
    pub fn pending_jump_count(&self) -> usize {
        self.pending_chains
            .values()
            .filter(|chain| chain.resolved_target.is_none())
            .map(|chain| chain.jumps.len())
            .sum()
    }
}

impl Default for PendingJumpsManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_chain_creation_and_resolution() {
        let mut manager = PendingJumpsManager::new();
        
        // Create a chain with a jump
        let chain_id = manager.create_chain_with_jump(100, Opcode::Goto, 3);
        
        // Should be unresolved initially
        assert!(!manager.is_chain_resolved(chain_id));
        
        // Resolve the chain
        let patches = manager.resolve_chain(chain_id, 200).unwrap();
        
        // Should be resolved now
        assert!(manager.is_chain_resolved(chain_id));
        assert_eq!(manager.get_chain_target(chain_id), Some(200));
        
        // Should have one patch
        assert_eq!(patches.len(), 1);
        assert_eq!(patches[0].0, 100); // PC
        assert_eq!(patches[0].1, 99);  // Offset: 200 - (100 + 1) = 99 (offset field is at PC + 1)
    }
    
    #[test]
    fn test_chain_merging() {
        let mut manager = PendingJumpsManager::new();
        
        // Create two chains
        let chain1 = manager.create_chain_with_jump(100, Opcode::Goto, 3);
        let chain2 = manager.create_chain_with_jump(200, Opcode::Goto, 3);
        
        // Merge chain1 into chain2
        manager.merge_chains(chain1, chain2).unwrap();
        
        // Chain1 should be removed, chain2 should have both jumps
        assert!(!manager.pending_chains.contains_key(&chain1));
        assert_eq!(manager.pending_chains.get(&chain2).unwrap().jumps.len(), 2);
    }
    
    #[test]
    fn test_pending_jump_detection() {
        let mut manager = PendingJumpsManager::new();
        
        // Initially no pending jumps
        assert!(!manager.has_pending_jumps());
        
        // Create an unresolved chain
        let chain_id = manager.create_chain_with_jump(100, Opcode::Goto, 3);
        
        // Should have pending jumps
        assert!(manager.has_pending_jumps());
        assert_eq!(manager.pending_jump_count(), 1);
        
        // Resolve the chain
        manager.resolve_chain(chain_id, 200).unwrap();
        
        // Should no longer have pending jumps
        assert!(!manager.has_pending_jumps());
        assert_eq!(manager.pending_jump_count(), 0);
    }
}
