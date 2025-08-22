/*!
 * Chain-based Jump Management System
 * 
 * Inspired by Oracle javac's Chain implementation for forward jump resolution.
 * This system enables elegant handling of complex control flow patterns.
 */


/// Represents a linked list of unresolved forward jumps.
/// Jump locations are stored in decreasing order for efficient resolution.
#[derive(Debug, Clone)]
pub struct Chain {
    /// Program counter position of the jump instruction
    pub pc: u16,
    
    /// Next chain element (linked list structure)
    pub next: Option<Box<Chain>>,
    
    /// Stack size and register state at the jump point
    /// This ensures all jumps in a chain have compatible machine states
    pub stack_size: u16,
    pub locals_count: u16,
}

impl Chain {
    /// Create a new chain with a single jump location
    pub fn new(pc: u16, stack_size: u16, locals_count: u16) -> Self {
        Chain {
            pc,
            next: None,
            stack_size,
            locals_count,
        }
    }
    
    /// Create a new chain and prepend it to an existing chain
    pub fn prepend(pc: u16, stack_size: u16, locals_count: u16, next: Option<Box<Chain>>) -> Self {
        Chain {
            pc,
            next,
            stack_size,
            locals_count,
        }
    }
    
    /// Convert to boxed chain for easier manipulation
    pub fn boxed(self) -> Option<Box<Chain>> {
        Some(Box::new(self))
    }
    
    /// Get the length of the chain
    pub fn len(&self) -> usize {
        let mut count = 1;
        let mut current = &self.next;
        while let Some(ref chain) = current {
            count += 1;
            current = &chain.next;
        }
        count
    }
    
    /// Check if chain is empty (this should not happen as Chain always has at least one element)
    pub fn is_empty(&self) -> bool {
        false // A Chain always has at least one jump
    }
    
    /// Iterate over all jump positions in the chain
    pub fn iter(&self) -> ChainIterator {
        ChainIterator {
            current: Some(self),
        }
    }
    
    /// Append another chain to this chain (JavaC mergeChains pattern)
    pub fn append(&mut self, other: Box<Chain>) {
        // Find the end of this chain
        let mut current = self;
        while let Some(ref mut next) = current.next {
            current = next;
        }
        // Append the other chain
        current.next = Some(other);
    }
}

/// Iterator for Chain elements
pub struct ChainIterator<'a> {
    current: Option<&'a Chain>,
}

impl<'a> Iterator for ChainIterator<'a> {
    type Item = u16;
    
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(chain) = self.current {
            let pc = chain.pc;
            self.current = chain.next.as_ref().map(|b| b.as_ref());
            Some(pc)
        } else {
            None
        }
    }
}

/// Chain manipulation utilities (similar to Code.java static methods)
pub struct ChainOps;

impl ChainOps {
    /// Merge two chains into one, maintaining sorted order (decreasing PC values)
    /// This is equivalent to Code.mergeChains in javac
    pub fn merge(chain1: Option<Box<Chain>>, chain2: Option<Box<Chain>>) -> Option<Box<Chain>> {
        match (chain1, chain2) {
            (None, None) => None,
            (Some(c), None) | (None, Some(c)) => Some(c),
            (Some(c1), Some(c2)) => {
                // Merge chains in decreasing PC order
                if c1.pc >= c2.pc {
                    Some(Box::new(Chain::prepend(c1.pc, c1.stack_size, c1.locals_count, 
                                                 ChainOps::merge(c1.next, Some(c2)))))
                } else {
                    Some(Box::new(Chain::prepend(c2.pc, c2.stack_size, c2.locals_count,
                                                 ChainOps::merge(Some(c1), c2.next))))
                }
            }
        }
    }
    
    /// Create a chain from a single jump location
    pub fn single(pc: u16, stack_size: u16, locals_count: u16) -> Option<Box<Chain>> {
        Some(Box::new(Chain::new(pc, stack_size, locals_count)))
    }
    
    /// Check if two chain states are compatible (same stack size and locals)
    pub fn compatible(chain1: &Chain, chain2: &Chain) -> bool {
        chain1.stack_size == chain2.stack_size && 
        chain1.locals_count == chain2.locals_count
    }
}

/// Jump resolution context
/// Tracks the current state needed for resolving forward jumps
#[derive(Debug, Clone)]
pub struct JumpContext {
    /// Current program counter
    pub current_pc: u16,
    
    /// Current stack size
    pub stack_size: u16,
    
    /// Current locals count
    pub locals_count: u16,
    
    /// Whether we're in fat bytecode mode (wide jumps)
    pub fat_code: bool,
}

impl JumpContext {
    pub fn new(current_pc: u16, stack_size: u16, locals_count: u16, fat_code: bool) -> Self {
        JumpContext {
            current_pc,
            stack_size,
            locals_count,
            fat_code,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_chain_creation() {
        let chain = Chain::new(100, 2, 3);
        assert_eq!(chain.pc, 100);
        assert_eq!(chain.stack_size, 2);
        assert_eq!(chain.locals_count, 3);
        assert!(chain.next.is_none());
    }
    
    #[test]
    fn test_chain_merge() {
        let chain1 = ChainOps::single(200, 1, 2);
        let chain2 = ChainOps::single(100, 1, 2);
        
        let merged = ChainOps::merge(chain1, chain2);
        assert!(merged.is_some());
        
        let merged = merged.unwrap();
        assert_eq!(merged.pc, 200); // Higher PC should come first
        assert!(merged.next.is_some());
        assert_eq!(merged.next.as_ref().unwrap().pc, 100);
    }
    
    #[test]
    fn test_chain_iterator() {
        let chain3 = Chain::new(50, 1, 2);
        let chain2 = Chain::prepend(100, 1, 2, Some(Box::new(chain3)));
        let chain1 = Chain::prepend(150, 1, 2, Some(Box::new(chain2)));
        
        let positions: Vec<u16> = chain1.iter().collect();
        assert_eq!(positions, vec![150, 100, 50]);
    }
    
    #[test]
    fn test_chain_compatibility() {
        let chain1 = Chain::new(100, 2, 3);
        let chain2 = Chain::new(200, 2, 3);
        let chain3 = Chain::new(300, 1, 3); // Different stack size
        
        assert!(ChainOps::compatible(&chain1, &chain2));
        assert!(!ChainOps::compatible(&chain1, &chain3));
    }
}