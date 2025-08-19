/// Chain-based jump resolution system (enhanced)
/// This is more efficient than simple label resolution for complex control flow

#[derive(Debug, Clone)]
pub struct Chain {
    /// Program counter where the jump instruction is located
    pub pc: usize,
    /// Next chain in the linked list
    pub next: Option<Box<Chain>>,
    /// Stack state at the time of the jump
    pub stack_state: StackState,
}

#[derive(Debug, Clone)]
pub struct StackState {
    pub depth: u16,
    pub max_depth: u16,
}

impl Chain {
    /// Create a new chain node
    pub fn new(pc: usize, next: Option<Box<Chain>>, stack_state: StackState) -> Self {
        Chain {
            pc,
            next,
            stack_state,
        }
    }
    
    /// Merge two chains into one (javac-style recursive merge sort)
    pub fn merge(chain1: Option<Box<Chain>>, chain2: Option<Box<Chain>>) -> Option<Box<Chain>> {
        match (chain1, chain2) {
            (None, chain2) => chain2,
            (chain1, None) => chain1,
            (Some(mut c1), Some(c2)) => {
                if c1.pc <= c2.pc {
                    c1.next = Self::merge(c1.next, Some(c2));
                    Some(c1)
                } else {
                    let mut c2 = c2;
                    c2.next = Self::merge(Some(c1), c2.next);
                    Some(c2)
                }
            }
        }
    }
    
    /// Merge two optional chains (convenience method)
    pub fn merge_option(chain1: Option<Chain>, chain2: Option<Chain>) -> Option<Chain> {
        match (chain1, chain2) {
            (None, chain2) => chain2,
            (chain1, None) => chain1,
            (Some(c1), Some(c2)) => {
                Self::merge(Some(Box::new(c1)), Some(Box::new(c2))).map(|boxed| *boxed)
            }
        }
    }
    
    /// Get the length of the chain
    pub fn length(&self) -> usize {
        let mut count = 1;
        let mut current = &self.next;
        while let Some(ref chain) = current {
            count += 1;
            current = &chain.next;
        }
        count
    }
    
    /// Iterate over all program counters in the chain
    pub fn iter_pcs(&self) -> ChainPcIterator {
        ChainPcIterator {
            current: Some(self),
        }
    }
}

pub struct ChainPcIterator<'a> {
    current: Option<&'a Chain>,
}

impl<'a> Iterator for ChainPcIterator<'a> {
    type Item = usize;
    
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(chain) = self.current {
            let pc = chain.pc;
            self.current = chain.next.as_ref().map(|boxed| boxed.as_ref());
            Some(pc)
        } else {
            None
        }
    }
}

/// Chain-based jump manager (javac-style)
pub struct ChainManager {
    /// Pending jumps that need to be resolved
    pending_jumps: Option<Box<Chain>>,
    /// Code buffer for patching jumps
    code: Vec<u8>,
}

impl ChainManager {
    pub fn new() -> Self {
        ChainManager {
            pending_jumps: None,
            code: Vec::new(),
        }
    }
    
    /// Emit a jump instruction and return its chain (javac-style)
    pub fn emit_jump(&mut self, opcode: u8, stack_state: StackState) -> Option<Box<Chain>> {
        let pc = self.code.len();
        
        // Emit the opcode
        self.code.push(opcode);
        
        // Emit placeholder for jump offset (will be patched later)
        self.code.push(0);
        self.code.push(0);
        
        // Create and return chain
        Some(Box::new(Chain::new(pc, None, stack_state)))
    }
    
    /// Branch with given opcode and return its chain (javac-style)
    pub fn branch(&mut self, opcode: u8, stack_state: StackState) -> Option<Box<Chain>> {
        let mut result = None;
        
        // Special handling for goto (javac pattern)
        if opcode == crate::codegen::opcodes::GOTO {
            result = self.pending_jumps.take();
        }
        
        // Emit the jump and create chain
        if let Some(jump_chain) = self.emit_jump(opcode, stack_state.clone()) {
            result = Chain::merge(Some(jump_chain), result);
        }
        
        result
    }
    
    /// Resolve chain to point to given target (javac-style)
    pub fn resolve(&mut self, chain: Option<Box<Chain>>, target: usize) {
        let mut current_chain = chain;
        
        while let Some(chain) = current_chain {
            let pc = chain.pc;
            let offset = target as i32 - pc as i32;
            
            // Patch the jump offset (big-endian 16-bit)
            if pc + 1 < self.code.len() && pc + 2 < self.code.len() {
                let offset_bytes = (offset as i16).to_be_bytes();
                self.code[pc + 1] = offset_bytes[0];
                self.code[pc + 2] = offset_bytes[1];
            }
            
            current_chain = chain.next;
        }
    }
    
    /// Resolve chain to current position (javac-style)
    pub fn resolve_to_current(&mut self, chain: Option<Box<Chain>>) {
        let current_pc = self.code.len();
        self.resolve(chain, current_pc);
    }
    
    /// Add chain to pending jumps (javac-style)
    pub fn add_pending(&mut self, chain: Option<Box<Chain>>) {
        self.pending_jumps = Chain::merge(chain, self.pending_jumps.take());
    }
    
    /// Resolve all pending jumps (javac-style)
    pub fn resolve_pending(&mut self) {
        let pending = self.pending_jumps.take();
        let current_pc = self.code.len();
        self.resolve(pending, current_pc);
    }
    
    /// Get current program counter
    pub fn current_pc(&self) -> usize {
        self.code.len()
    }
    
    /// Get the code buffer
    pub fn code(&self) -> &[u8] {
        &self.code
    }
    
    /// Get mutable code buffer
    pub fn code_mut(&mut self) -> &mut Vec<u8> {
        &mut self.code
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_chain_creation() {
        let stack_state = StackState { depth: 1, max_depth: 2 };
        let chain = Chain::new(10, None, stack_state);
        assert_eq!(chain.pc, 10);
        assert_eq!(chain.length(), 1);
    }
    
    #[test]
    fn test_chain_merge() {
        let stack_state = StackState { depth: 1, max_depth: 2 };
        let chain1 = Some(Box::new(Chain::new(5, None, stack_state.clone())));
        let chain2 = Some(Box::new(Chain::new(10, None, stack_state)));
        
        let merged = Chain::merge(chain1, chain2);
        assert!(merged.is_some());
        
        let pcs: Vec<usize> = merged.unwrap().iter_pcs().collect();
        assert_eq!(pcs, vec![5, 10]);
    }
    
    #[test]
    fn test_chain_manager() {
        let mut manager = ChainManager::new();
        let stack_state = StackState { depth: 1, max_depth: 2 };
        
        // Emit a jump
        let chain = manager.emit_jump(crate::codegen::opcodes::GOTO, stack_state);
        assert!(chain.is_some());
        
        // Resolve to target
        manager.resolve(chain, 100);
        
        // Check that code was patched
        let code = manager.code();
        assert_eq!(code.len(), 3); // opcode + 2 bytes offset
        assert_eq!(code[0], crate::codegen::opcodes::GOTO);
    }
}
