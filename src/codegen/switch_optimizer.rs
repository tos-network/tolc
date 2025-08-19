/// Switch statement optimization (javac-style)
/// Intelligently chooses between tableswitch and lookupswitch based on case density

use crate::codegen::opcodes;

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub value: i32,
    pub target_pc: usize,
}

#[derive(Debug, Clone)]
pub struct SwitchOptimizer {
    pub cases: Vec<SwitchCase>,
    pub default_pc: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum SwitchInstruction {
    TableSwitch {
        low: i32,
        high: i32,
        default_offset: i32,
        offsets: Vec<i32>,
    },
    LookupSwitch {
        default_offset: i32,
        pairs: Vec<(i32, i32)>, // (value, offset) pairs
    },
}

impl SwitchOptimizer {
    pub fn new() -> Self {
        SwitchOptimizer {
            cases: Vec::new(),
            default_pc: None,
        }
    }
    
    pub fn add_case(&mut self, value: i32, target_pc: usize) {
        self.cases.push(SwitchCase { value, target_pc });
    }
    
    pub fn set_default(&mut self, target_pc: usize) {
        self.default_pc = Some(target_pc);
    }
    
    /// Optimize switch statement (javac algorithm)
    pub fn optimize(&self, current_pc: usize) -> SwitchInstruction {
        if self.cases.is_empty() {
            // No cases, just use lookupswitch with empty pairs
            return SwitchInstruction::LookupSwitch {
                default_offset: self.calculate_offset(current_pc, self.default_pc.unwrap_or(current_pc)),
                pairs: Vec::new(),
            };
        }
        
        // Sort cases by value
        let mut sorted_cases = self.cases.clone();
        sorted_cases.sort_by_key(|case| case.value);
        
        let lo = sorted_cases[0].value;
        let hi = sorted_cases[sorted_cases.len() - 1].value;
        let nlabels = sorted_cases.len() as i64;
        
        // javac's cost analysis algorithm
        let table_space_cost = 4 + (hi as i64 - lo as i64 + 1); // words
        let table_time_cost = 3; // comparisons
        let lookup_space_cost = 3 + 2 * nlabels;
        let lookup_time_cost = nlabels; // comparisons
        
        // Choose based on cost analysis (javac formula)
        let use_table = table_space_cost + 3 * table_time_cost <= 
                       lookup_space_cost + 3 * lookup_time_cost;
        
        if use_table {
            self.generate_tableswitch(current_pc, lo, hi, &sorted_cases)
        } else {
            self.generate_lookupswitch(current_pc, &sorted_cases)
        }
    }
    
    fn generate_tableswitch(&self, current_pc: usize, lo: i32, hi: i32, sorted_cases: &[SwitchCase]) -> SwitchInstruction {
        let range = (hi - lo + 1) as usize;
        let mut offsets = vec![self.calculate_offset(current_pc, self.default_pc.unwrap_or(current_pc)); range];
        
        // Fill in the actual case targets
        for case in sorted_cases {
            let index = (case.value - lo) as usize;
            if index < offsets.len() {
                offsets[index] = self.calculate_offset(current_pc, case.target_pc);
            }
        }
        
        SwitchInstruction::TableSwitch {
            low: lo,
            high: hi,
            default_offset: self.calculate_offset(current_pc, self.default_pc.unwrap_or(current_pc)),
            offsets,
        }
    }
    
    fn generate_lookupswitch(&self, current_pc: usize, sorted_cases: &[SwitchCase]) -> SwitchInstruction {
        let pairs: Vec<(i32, i32)> = sorted_cases
            .iter()
            .map(|case| (case.value, self.calculate_offset(current_pc, case.target_pc)))
            .collect();
        
        SwitchInstruction::LookupSwitch {
            default_offset: self.calculate_offset(current_pc, self.default_pc.unwrap_or(current_pc)),
            pairs,
        }
    }
    
    fn calculate_offset(&self, from_pc: usize, to_pc: usize) -> i32 {
        to_pc as i32 - from_pc as i32
    }
    
    /// Generate bytecode for the optimized switch instruction
    pub fn emit_bytecode(&self, current_pc: usize) -> Vec<u8> {
        let instruction = self.optimize(current_pc);
        
        match instruction {
            SwitchInstruction::TableSwitch { low, high, default_offset, offsets } => {
                self.emit_tableswitch(low, high, default_offset, &offsets)
            }
            SwitchInstruction::LookupSwitch { default_offset, pairs } => {
                self.emit_lookupswitch(default_offset, &pairs)
            }
        }
    }
    
    fn emit_tableswitch(&self, low: i32, high: i32, default_offset: i32, offsets: &[i32]) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Opcode
        bytecode.push(opcodes::TABLESWITCH);
        
        // Padding to align to 4-byte boundary
        let padding = (4 - (bytecode.len() % 4)) % 4;
        for _ in 0..padding {
            bytecode.push(0);
        }
        
        // Default offset (4 bytes, big-endian)
        bytecode.extend_from_slice(&default_offset.to_be_bytes());
        
        // Low value (4 bytes, big-endian)
        bytecode.extend_from_slice(&low.to_be_bytes());
        
        // High value (4 bytes, big-endian)
        bytecode.extend_from_slice(&high.to_be_bytes());
        
        // Jump offsets (4 bytes each, big-endian)
        for offset in offsets {
            bytecode.extend_from_slice(&offset.to_be_bytes());
        }
        
        bytecode
    }
    
    fn emit_lookupswitch(&self, default_offset: i32, pairs: &[(i32, i32)]) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Opcode
        bytecode.push(opcodes::LOOKUPSWITCH);
        
        // Padding to align to 4-byte boundary
        let padding = (4 - (bytecode.len() % 4)) % 4;
        for _ in 0..padding {
            bytecode.push(0);
        }
        
        // Default offset (4 bytes, big-endian)
        bytecode.extend_from_slice(&default_offset.to_be_bytes());
        
        // Number of pairs (4 bytes, big-endian)
        bytecode.extend_from_slice(&(pairs.len() as i32).to_be_bytes());
        
        // Match-offset pairs (8 bytes each: 4 for match, 4 for offset, big-endian)
        for (value, offset) in pairs {
            bytecode.extend_from_slice(&value.to_be_bytes());
            bytecode.extend_from_slice(&offset.to_be_bytes());
        }
        
        bytecode
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_dense_switch_uses_tableswitch() {
        let mut optimizer = SwitchOptimizer::new();
        optimizer.add_case(1, 100);
        optimizer.add_case(2, 200);
        optimizer.add_case(3, 300);
        optimizer.add_case(4, 400);
        optimizer.set_default(500);
        
        let instruction = optimizer.optimize(0);
        match instruction {
            SwitchInstruction::TableSwitch { low, high, .. } => {
                assert_eq!(low, 1);
                assert_eq!(high, 4);
            }
            _ => panic!("Expected TableSwitch for dense cases"),
        }
    }
    
    #[test]
    fn test_sparse_switch_uses_lookupswitch() {
        let mut optimizer = SwitchOptimizer::new();
        optimizer.add_case(1, 100);
        optimizer.add_case(100, 200);
        optimizer.add_case(1000, 300);
        optimizer.set_default(500);
        
        let instruction = optimizer.optimize(0);
        match instruction {
            SwitchInstruction::LookupSwitch { pairs, .. } => {
                assert_eq!(pairs.len(), 3);
                assert_eq!(pairs[0].0, 1);
                assert_eq!(pairs[1].0, 100);
                assert_eq!(pairs[2].0, 1000);
            }
            _ => panic!("Expected LookupSwitch for sparse cases"),
        }
    }
    
    #[test]
    fn test_empty_switch() {
        let optimizer = SwitchOptimizer::new();
        let instruction = optimizer.optimize(0);
        match instruction {
            SwitchInstruction::LookupSwitch { pairs, .. } => {
                assert_eq!(pairs.len(), 0);
            }
            _ => panic!("Expected LookupSwitch for empty switch"),
        }
    }
}
