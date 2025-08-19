use crate::codegen::opcode_enum::Opcode;
use std::collections::HashMap;

/// JavaC-style fatcode mechanism for handling jumps >= 32K
/// 
/// This module implements the automatic detection and handling of long jumps
/// that exceed the 16-bit limit (32767 bytes), similar to javac's fatcode system.
#[derive(Debug, Clone)]
pub struct FatcodeManager {
    /// Whether we're currently in fatcode mode (using wide jumps)
    pub fatcode: bool,
    
    /// Maximum jump distance encountered so far
    pub max_jump_distance: u32,
    
    /// Jump locations that need to be resolved
    pub jump_locations: HashMap<u32, JumpInfo>,
    
    /// Code size limit before switching to fatcode mode
    pub fatcode_threshold: u32,
    
    /// Statistics for optimization decisions
    pub stats: FatcodeStats,
}

#[derive(Debug, Clone)]
pub struct JumpInfo {
    /// Program counter where the jump instruction is located
    pub jump_pc: u32,
    
    /// Target program counter (may be unresolved)
    pub target_pc: Option<u32>,
    
    /// Type of jump instruction
    pub jump_type: JumpType,
    
    /// Whether this jump requires wide encoding
    pub needs_wide: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpType {
    /// Conditional jumps (if*, goto)
    Conditional(Opcode),
    
    /// Unconditional goto
    Goto,
    
    /// JSR (subroutine call)
    Jsr,
    
    /// Switch table jumps
    Switch,
    
    /// Exception handler jumps
    Exception,
}

#[derive(Debug, Clone, Default)]
pub struct FatcodeStats {
    /// Total number of jumps
    pub total_jumps: u32,
    
    /// Number of jumps requiring wide encoding
    pub wide_jumps: u32,
    
    /// Number of times fatcode mode was triggered
    pub fatcode_switches: u32,
    
    /// Maximum method size encountered
    pub max_method_size: u32,
}

impl FatcodeManager {
    /// Create a new fatcode manager
    pub fn new() -> Self {
        Self {
            fatcode: false,
            max_jump_distance: 0,
            jump_locations: HashMap::new(),
            fatcode_threshold: 32767, // 16-bit signed limit
            stats: FatcodeStats::default(),
        }
    }
    
    /// Create a fatcode manager with custom threshold
    pub fn with_threshold(threshold: u32) -> Self {
        let mut manager = Self::new();
        manager.fatcode_threshold = threshold;
        manager
    }
    
    /// Check if we need to switch to fatcode mode
    pub fn check_fatcode_needed(&mut self, current_pc: u32) -> bool {
        if self.fatcode {
            return false; // Already in fatcode mode
        }
        
        // Check if any pending jumps would exceed the threshold
        for (jump_pc, jump_info) in &self.jump_locations {
            if let Some(target_pc) = jump_info.target_pc {
                let distance = if target_pc > *jump_pc {
                    target_pc - *jump_pc
                } else {
                    *jump_pc - target_pc
                };
                
                if distance > self.fatcode_threshold {
                    eprintln!("🔍 DEBUG: FatcodeManager: Jump from {} to {} (distance: {}) exceeds threshold {}", 
                             jump_pc, target_pc, distance, self.fatcode_threshold);
                    return true;
                }
            }
        }
        
        // Check if current method size is approaching limits
        if current_pc > self.fatcode_threshold / 2 {
            eprintln!("🔍 DEBUG: FatcodeManager: Method size {} approaching fatcode threshold", current_pc);
            // Preemptively switch to fatcode for large methods
            return current_pc > self.fatcode_threshold * 3 / 4;
        }
        
        false
    }
    
    /// Switch to fatcode mode
    pub fn enable_fatcode(&mut self) {
        if !self.fatcode {
            eprintln!("🔍 DEBUG: FatcodeManager: Switching to fatcode mode");
            self.fatcode = true;
            self.stats.fatcode_switches += 1;
            
            // Mark all existing jumps as needing wide encoding
            for jump_info in self.jump_locations.values_mut() {
                jump_info.needs_wide = true;
            }
        }
    }
    
    /// Register a new jump location
    pub fn register_jump(&mut self, jump_pc: u32, jump_type: JumpType) -> u32 {
        let jump_id = self.jump_locations.len() as u32;
        
        let jump_info = JumpInfo {
            jump_pc,
            target_pc: None,
            jump_type: jump_type.clone(),
            needs_wide: self.fatcode,
        };
        
        self.jump_locations.insert(jump_id, jump_info);
        self.stats.total_jumps += 1;
        
        eprintln!("🔍 DEBUG: FatcodeManager: Registered jump {} at PC {} (type: {:?})", 
                 jump_id, jump_pc, jump_type);
        
        jump_id
    }
    
    /// Resolve a jump target
    pub fn resolve_jump(&mut self, jump_id: u32, target_pc: u32) -> Result<(), String> {
        if let Some(jump_info) = self.jump_locations.get_mut(&jump_id) {
            jump_info.target_pc = Some(target_pc);
            
            // Calculate jump distance
            let distance = if target_pc > jump_info.jump_pc {
                target_pc - jump_info.jump_pc
            } else {
                jump_info.jump_pc - target_pc
            };
            
            self.max_jump_distance = self.max_jump_distance.max(distance);
            
            // Check if this jump needs wide encoding
            if distance > self.fatcode_threshold {
                jump_info.needs_wide = true;
                self.stats.wide_jumps += 1;
                
                eprintln!("🔍 DEBUG: FatcodeManager: Jump {} requires wide encoding (distance: {})", 
                         jump_id, distance);
            }
            
            eprintln!("🔍 DEBUG: FatcodeManager: Resolved jump {} to PC {} (distance: {})", 
                     jump_id, target_pc, distance);
            
            Ok(())
        } else {
            Err(format!("Jump ID {} not found", jump_id))
        }
    }
    
    /// Get the opcode for a jump (normal or wide version)
    pub fn get_jump_opcode(&self, jump_id: u32, base_opcode: Opcode) -> Result<Opcode, String> {
        if let Some(jump_info) = self.jump_locations.get(&jump_id) {
            if jump_info.needs_wide || self.fatcode {
                // Return wide version of the opcode
                match base_opcode {
                    Opcode::Goto => Ok(Opcode::GotoW),
                    Opcode::Jsr => Ok(Opcode::JsrW),
                    // Conditional jumps don't have wide versions, need to be converted
                    // to inverted condition + goto_w
                    _ => Ok(base_opcode), // Will be handled by caller
                }
            } else {
                Ok(base_opcode)
            }
        } else {
            Err(format!("Jump ID {} not found", jump_id))
        }
    }
    
    /// Check if a jump needs wide encoding
    pub fn needs_wide_jump(&self, jump_id: u32) -> bool {
        self.jump_locations
            .get(&jump_id)
            .map(|info| info.needs_wide || self.fatcode)
            .unwrap_or(false)
    }
    
    /// Get the size of a jump instruction (2 or 4 bytes for offset)
    pub fn get_jump_size(&self, jump_id: u32, base_size: u32) -> u32 {
        if self.needs_wide_jump(jump_id) {
            base_size + 2 // Wide jumps use 4-byte offsets instead of 2-byte
        } else {
            base_size
        }
    }
    
    /// Calculate the total code size adjustment needed for fatcode
    pub fn calculate_size_adjustment(&self) -> u32 {
        let mut adjustment = 0;
        
        for jump_info in self.jump_locations.values() {
            if jump_info.needs_wide {
                // Each wide jump adds 2 extra bytes
                adjustment += 2;
            }
        }
        
        adjustment
    }
    
    /// Reset for a new method compilation
    pub fn reset(&mut self) {
        self.fatcode = false;
        self.max_jump_distance = 0;
        self.jump_locations.clear();
    }
    
    /// Get compilation statistics
    pub fn get_stats(&self) -> &FatcodeStats {
        &self.stats
    }
    
    /// Check if method needs recompilation due to fatcode
    pub fn needs_recompilation(&self) -> bool {
        !self.fatcode && self.max_jump_distance > self.fatcode_threshold
    }
    
    /// Convert conditional jump to inverted condition + goto_w pattern
    pub fn convert_conditional_to_wide(&self, condition_opcode: Opcode) -> (Opcode, Opcode) {
        let inverted_condition = match condition_opcode {
            Opcode::Ifeq => Opcode::Ifne,
            Opcode::Ifne => Opcode::Ifeq,
            Opcode::Iflt => Opcode::Ifge,
            Opcode::Ifge => Opcode::Iflt,
            Opcode::Ifgt => Opcode::Ifle,
            Opcode::Ifle => Opcode::Ifgt,
            Opcode::IfIcmpeq => Opcode::IfIcmpne,
            Opcode::IfIcmpne => Opcode::IfIcmpeq,
            Opcode::IfIcmplt => Opcode::IfIcmpge,
            Opcode::IfIcmpge => Opcode::IfIcmplt,
            Opcode::IfIcmpgt => Opcode::IfIcmple,
            Opcode::IfIcmple => Opcode::IfIcmpgt,
            Opcode::IfAcmpeq => Opcode::IfAcmpne,
            Opcode::IfAcmpne => Opcode::IfAcmpeq,
            Opcode::Ifnull => Opcode::Ifnonnull,
            Opcode::Ifnonnull => Opcode::Ifnull,
            _ => condition_opcode, // Not a conditional jump
        };
        
        (inverted_condition, Opcode::GotoW)
    }
}

impl Default for FatcodeManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_fatcode_threshold_detection() {
        let mut manager = FatcodeManager::with_threshold(100);
        
        // Register a jump
        let jump_id = manager.register_jump(10, JumpType::Goto);
        
        // Resolve to a distant target
        manager.resolve_jump(jump_id, 200).unwrap();
        
        // Should detect need for fatcode
        assert!(manager.check_fatcode_needed(200));
    }
    
    #[test]
    fn test_jump_opcode_selection() {
        let mut manager = FatcodeManager::new();
        
        // Register a jump
        let jump_id = manager.register_jump(10, JumpType::Goto);
        
        // Initially should use normal opcode
        assert_eq!(manager.get_jump_opcode(jump_id, Opcode::Goto).unwrap(), Opcode::Goto);
        
        // Enable fatcode
        manager.enable_fatcode();
        
        // Should now use wide opcode
        assert_eq!(manager.get_jump_opcode(jump_id, Opcode::Goto).unwrap(), Opcode::GotoW);
    }
    
    #[test]
    fn test_conditional_jump_conversion() {
        let manager = FatcodeManager::new();
        
        let (inverted, wide_goto) = manager.convert_conditional_to_wide(Opcode::Ifeq);
        assert_eq!(inverted, Opcode::Ifne);
        assert_eq!(wide_goto, Opcode::GotoW);
    }
}
