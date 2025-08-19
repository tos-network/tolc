use std::collections::{HashMap, HashSet};

/// JavaC-style fixedPc mechanism for preventing code compaction at jump targets
/// 
/// This implements the fixedPc logic from javac's Code.java to ensure that
/// jump targets and other critical code locations are not moved during optimization.
#[derive(Debug, Clone)]
pub struct FixedPcManager {
    /// Set of program counters that must not be moved during compaction
    pub fixed_pcs: HashSet<u32>,
    
    /// Mapping from original PC to reasons why it's fixed
    pub fixed_reasons: HashMap<u32, Vec<FixedPcReason>>,
    
    /// Temporary fixedPc state (current instruction being emitted)
    pub current_fixed: bool,
    
    /// Statistics for optimization tracking
    pub stats: FixedPcStats,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FixedPcReason {
    /// Jump target (goto, conditional jumps, etc.)
    JumpTarget { source_pc: u32, jump_type: String },
    
    /// Exception handler entry point
    ExceptionHandler { handler_type: String },
    
    /// Label definition
    Label { label_name: String },
    
    /// Switch table target
    SwitchTarget { switch_pc: u32, case_value: i32 },
    
    /// Line number mapping
    LineNumber { line: u32 },
    
    /// Stack map frame location
    StackMapFrame,
    
    /// Method entry point
    MethodEntry,
    
    /// Try block boundaries
    TryBlock { block_type: String },
    
    /// Synchronized block entry/exit
    SynchronizedBlock,
    
    /// User-defined fixed point
    UserDefined { reason: String },
}

#[derive(Debug, Clone, Default)]
pub struct FixedPcStats {
    /// Total number of fixed PCs
    pub total_fixed_pcs: u32,
    
    /// Number of jump targets marked as fixed
    pub jump_targets: u32,
    
    /// Number of exception handlers marked as fixed
    pub exception_handlers: u32,
    
    /// Number of labels marked as fixed
    pub labels: u32,
    
    /// Number of compaction operations prevented
    pub prevented_compactions: u32,
    
    /// Total bytes saved by allowing compaction at non-fixed locations
    pub bytes_saved: u32,
}

impl FixedPcManager {
    /// Create a new fixed PC manager
    pub fn new() -> Self {
        Self {
            fixed_pcs: HashSet::new(),
            fixed_reasons: HashMap::new(),
            current_fixed: false,
            stats: FixedPcStats::default(),
        }
    }
    
    /// Mark a PC as fixed (cannot be moved during compaction)
    pub fn mark_fixed(&mut self, pc: u32, reason: FixedPcReason) {
        if self.fixed_pcs.insert(pc) {
            self.stats.total_fixed_pcs += 1;
            
            // Update category statistics
            match &reason {
                FixedPcReason::JumpTarget { .. } => self.stats.jump_targets += 1,
                FixedPcReason::ExceptionHandler { .. } => self.stats.exception_handlers += 1,
                FixedPcReason::Label { .. } => self.stats.labels += 1,
                _ => {}
            }
        }
        
        // Add reason to the list
        self.fixed_reasons
            .entry(pc)
            .or_insert_with(Vec::new)
            .push(reason.clone());
        
        eprintln!("🔍 DEBUG: FixedPc: Marked PC {} as fixed (reason: {:?})", pc, reason);
    }
    
    /// Mark multiple PCs as fixed with the same reason
    pub fn mark_fixed_range(&mut self, start_pc: u32, end_pc: u32, reason: FixedPcReason) {
        for pc in start_pc..=end_pc {
            self.mark_fixed(pc, reason.clone());
        }
    }
    
    /// Check if a PC is fixed
    pub fn is_fixed(&self, pc: u32) -> bool {
        self.fixed_pcs.contains(&pc)
    }
    
    /// Get the reasons why a PC is fixed
    pub fn get_fixed_reasons(&self, pc: u32) -> Option<&Vec<FixedPcReason>> {
        self.fixed_reasons.get(&pc)
    }
    
    /// Set the current fixedPc state (javac-style)
    pub fn set_current_fixed(&mut self, fixed: bool) {
        self.current_fixed = fixed;
    }
    
    /// Check if the current location should be fixed
    pub fn is_current_fixed(&self) -> bool {
        self.current_fixed
    }
    
    /// Mark current PC as fixed if the current_fixed flag is set
    pub fn apply_current_fixed(&mut self, pc: u32) {
        if self.current_fixed {
            self.mark_fixed(pc, FixedPcReason::UserDefined {
                reason: "Current fixedPc flag set".to_string()
            });
            self.current_fixed = false; // Reset after applying
        }
    }
    
    /// Mark a jump target as fixed
    pub fn mark_jump_target(&mut self, target_pc: u32, source_pc: u32, jump_type: &str) {
        self.mark_fixed(target_pc, FixedPcReason::JumpTarget {
            source_pc,
            jump_type: jump_type.to_string(),
        });
    }
    
    /// Mark an exception handler as fixed
    pub fn mark_exception_handler(&mut self, handler_pc: u32, handler_type: &str) {
        self.mark_fixed(handler_pc, FixedPcReason::ExceptionHandler {
            handler_type: handler_type.to_string(),
        });
    }
    
    /// Mark a label as fixed
    pub fn mark_label(&mut self, label_pc: u32, label_name: &str) {
        self.mark_fixed(label_pc, FixedPcReason::Label {
            label_name: label_name.to_string(),
        });
    }
    
    /// Mark a switch target as fixed
    pub fn mark_switch_target(&mut self, target_pc: u32, switch_pc: u32, case_value: i32) {
        self.mark_fixed(target_pc, FixedPcReason::SwitchTarget {
            switch_pc,
            case_value,
        });
    }
    
    /// Mark a line number location as fixed
    pub fn mark_line_number(&mut self, pc: u32, line: u32) {
        self.mark_fixed(pc, FixedPcReason::LineNumber { line });
    }
    
    /// Mark a stack map frame location as fixed
    pub fn mark_stack_map_frame(&mut self, pc: u32) {
        self.mark_fixed(pc, FixedPcReason::StackMapFrame);
    }
    
    /// Check if compaction is allowed at a specific PC
    pub fn can_compact_at(&self, pc: u32) -> bool {
        !self.is_fixed(pc)
    }
    
    /// Get all fixed PCs in a range
    pub fn get_fixed_pcs_in_range(&self, start_pc: u32, end_pc: u32) -> Vec<u32> {
        self.fixed_pcs
            .iter()
            .filter(|&&pc| pc >= start_pc && pc <= end_pc)
            .cloned()
            .collect()
    }
    
    /// Calculate compaction savings (bytes that could be saved)
    pub fn calculate_potential_savings(&self, _total_code_size: u32, compactable_instructions: u32) -> u32 {
        // Estimate: each compactable instruction could save 1-2 bytes on average
        let _potential_savings = compactable_instructions * 1; // Conservative estimate
        
        // But we can't compact at fixed PCs
        let fixed_instructions = self.fixed_pcs.len() as u32;
        let actual_compactable = compactable_instructions.saturating_sub(fixed_instructions);
        
        actual_compactable * 1 // 1 byte average savings per instruction
    }
    
    /// Record that compaction was prevented at a PC
    pub fn record_prevented_compaction(&mut self, pc: u32) {
        if self.is_fixed(pc) {
            self.stats.prevented_compactions += 1;
            eprintln!("🔍 DEBUG: FixedPc: Prevented compaction at PC {} (fixed)", pc);
        }
    }
    
    /// Record bytes saved through compaction
    pub fn record_bytes_saved(&mut self, bytes: u32) {
        self.stats.bytes_saved += bytes;
    }
    
    /// Get all fixed PCs sorted
    pub fn get_all_fixed_pcs(&self) -> Vec<u32> {
        let mut pcs: Vec<u32> = self.fixed_pcs.iter().cloned().collect();
        pcs.sort();
        pcs
    }
    
    /// Remove fixed PC (for cleanup)
    pub fn unmark_fixed(&mut self, pc: u32) {
        if self.fixed_pcs.remove(&pc) {
            self.fixed_reasons.remove(&pc);
            self.stats.total_fixed_pcs = self.stats.total_fixed_pcs.saturating_sub(1);
            eprintln!("🔍 DEBUG: FixedPc: Unmarked PC {} as fixed", pc);
        }
    }
    
    /// Clear all fixed PCs (for new method)
    pub fn reset(&mut self) {
        self.fixed_pcs.clear();
        self.fixed_reasons.clear();
        self.current_fixed = false;
    }
    
    /// Get statistics
    pub fn get_stats(&self) -> &FixedPcStats {
        &self.stats
    }
    
    /// Generate a report of all fixed PCs
    pub fn generate_report(&self) -> String {
        let mut report = String::new();
        report.push_str("=== Fixed PC Report ===\n");
        report.push_str(&format!("Total Fixed PCs: {}\n", self.stats.total_fixed_pcs));
        report.push_str(&format!("Jump Targets: {}\n", self.stats.jump_targets));
        report.push_str(&format!("Exception Handlers: {}\n", self.stats.exception_handlers));
        report.push_str(&format!("Labels: {}\n", self.stats.labels));
        report.push_str(&format!("Prevented Compactions: {}\n", self.stats.prevented_compactions));
        report.push_str(&format!("Bytes Saved: {}\n", self.stats.bytes_saved));
        
        report.push_str("\n=== Fixed PC Details ===\n");
        let mut pcs: Vec<u32> = self.fixed_pcs.iter().cloned().collect();
        pcs.sort();
        
        for pc in pcs {
            if let Some(reasons) = self.fixed_reasons.get(&pc) {
                report.push_str(&format!("PC {}: ", pc));
                for (i, reason) in reasons.iter().enumerate() {
                    if i > 0 {
                        report.push_str(", ");
                    }
                    report.push_str(&format!("{:?}", reason));
                }
                report.push('\n');
            }
        }
        
        report
    }
    
    /// Validate fixedPc consistency (for debugging)
    pub fn validate(&self) -> Result<(), String> {
        // Check that all fixed PCs have reasons
        for &pc in &self.fixed_pcs {
            if !self.fixed_reasons.contains_key(&pc) {
                return Err(format!("Fixed PC {} has no recorded reasons", pc));
            }
        }
        
        // Check that all reasons correspond to fixed PCs
        for (&pc, _) in &self.fixed_reasons {
            if !self.fixed_pcs.contains(&pc) {
                return Err(format!("PC {} has reasons but is not marked as fixed", pc));
            }
        }
        
        Ok(())
    }
}

impl Default for FixedPcManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_fixed_pc_marking() {
        let mut manager = FixedPcManager::new();
        
        // Initially no fixed PCs
        assert!(!manager.is_fixed(100));
        
        // Mark a PC as fixed
        manager.mark_jump_target(100, 50, "goto");
        
        // Should be fixed now
        assert!(manager.is_fixed(100));
        assert_eq!(manager.stats.total_fixed_pcs, 1);
        assert_eq!(manager.stats.jump_targets, 1);
    }
    
    #[test]
    fn test_compaction_check() {
        let mut manager = FixedPcManager::new();
        
        // Initially can compact anywhere
        assert!(manager.can_compact_at(100));
        
        // Mark as fixed
        manager.mark_label(100, "loop_start");
        
        // Should not be able to compact
        assert!(!manager.can_compact_at(100));
    }
    
    #[test]
    fn test_fixed_pc_reasons() {
        let mut manager = FixedPcManager::new();
        
        // Mark with multiple reasons
        manager.mark_jump_target(100, 50, "goto");
        manager.mark_line_number(100, 42);
        
        // Should have both reasons
        let reasons = manager.get_fixed_reasons(100).unwrap();
        assert_eq!(reasons.len(), 2);
    }
    
    #[test]
    fn test_current_fixed_flag() {
        let mut manager = FixedPcManager::new();
        
        // Set current fixed flag
        manager.set_current_fixed(true);
        assert!(manager.is_current_fixed());
        
        // Apply to a PC
        manager.apply_current_fixed(100);
        
        // Should be fixed and flag should be reset
        assert!(manager.is_fixed(100));
        assert!(!manager.is_current_fixed());
    }
}
