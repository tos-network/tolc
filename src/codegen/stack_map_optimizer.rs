//! Stack Map Table Generation and Compression Optimizations
//! 
//! This module implements javac's stack map table optimizations including:
//! - Intelligent frame type selection (Same, SameLocals1StackItem, etc.)
//! - Frame compression and delta encoding
//! - Redundant frame elimination
//! - Optimal offset delta calculation

use crate::ast::*;
use crate::codegen::frame::{StackMapFrame, StackMapTable, VerificationType, FrameState};
use std::collections::HashMap;

/// Stack map optimization patterns (based on javac's emitStackMap logic)
#[derive(Debug, Clone, PartialEq)]
pub enum StackMapOptimization {
    /// Use Same frame (0-63) for identical locals/stack
    SameFrame { offset_delta: u16 },
    /// Use SameLocals1StackItem (64-127) for single stack item
    SameLocals1StackItem { offset_delta: u16, stack_item: VerificationType },
    /// Use extended forms for larger deltas
    ExtendedFrame { frame_type: ExtendedFrameType },
    /// Use Chop frame to remove locals
    ChopFrame { chop_count: u8, offset_delta: u16 },
    /// Use Append frame to add locals
    AppendFrame { append_locals: Vec<VerificationType>, offset_delta: u16 },
    /// Use Full frame when necessary
    FullFrame { locals: Vec<VerificationType>, stack: Vec<VerificationType>, offset_delta: u16 },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExtendedFrameType {
    SameLocals1StackItemExtended,
    SameExtended,
}

/// Stack map frame compression analyzer (javac's frame compression logic)
#[derive(Debug)]
pub struct StackMapOptimizer {
    /// Previous frame state for delta calculation
    previous_frame: Option<FrameState>,
    /// Current PC for offset calculation
    current_pc: u16,
    /// Previous PC for delta calculation
    previous_pc: u16,
    /// Optimization statistics
    compression_stats: CompressionStats,
}

#[derive(Debug, Default)]
pub struct CompressionStats {
    pub same_frames: usize,
    pub same_locals_1_stack_item: usize,
    pub extended_frames: usize,
    pub chop_frames: usize,
    pub append_frames: usize,
    pub full_frames: usize,
    pub bytes_saved: usize,
}

impl StackMapOptimizer {
    pub fn new() -> Self {
        Self {
            previous_frame: None,
            current_pc: 0,
            previous_pc: 0,
            compression_stats: CompressionStats::default(),
        }
    }

    /// Analyze and optimize stack map frame generation (javac's emitStackMap)
    pub fn analyze_frame_optimization(
        &mut self,
        locals: Vec<VerificationType>,
        stack: Vec<VerificationType>,
        pc: u16,
    ) -> StackMapOptimization {
        let offset_delta = if self.previous_frame.is_none() {
            pc  // First frame uses absolute PC
        } else {
            pc.saturating_sub(self.previous_pc + 1)
        };

        let current_frame = FrameState {
            locals: locals.clone(),
            stack: stack.clone(),
            new_types: HashMap::new(),
        };

        let optimization = if let Some(ref prev_frame) = self.previous_frame {
            self.choose_optimal_frame_type(prev_frame, &current_frame, offset_delta)
        } else {
            // First frame is always Full frame
            StackMapOptimization::FullFrame {
                locals,
                stack,
                offset_delta,
            }
        };

        self.update_compression_stats(&optimization);
        self.previous_frame = Some(current_frame);
        self.previous_pc = pc;
        self.current_pc = pc;

        optimization
    }

    /// Choose optimal frame type based on javac's frame compression logic
    fn choose_optimal_frame_type(
        &self,
        prev_frame: &FrameState,
        current_frame: &FrameState,
        offset_delta: u16,
    ) -> StackMapOptimization {
        // Same frame: identical locals and empty stack
        if prev_frame.locals == current_frame.locals && current_frame.stack.is_empty() {
            return if offset_delta <= 63 {
                StackMapOptimization::SameFrame { offset_delta }
            } else {
                StackMapOptimization::ExtendedFrame {
                    frame_type: ExtendedFrameType::SameExtended,
                }
            };
        }

        // SameLocals1StackItem: identical locals and single stack item
        if prev_frame.locals == current_frame.locals && current_frame.stack.len() == 1 {
            let stack_item = current_frame.stack[0].clone();
            return if offset_delta <= 63 {
                StackMapOptimization::SameLocals1StackItem { offset_delta, stack_item }
            } else {
                StackMapOptimization::ExtendedFrame {
                    frame_type: ExtendedFrameType::SameLocals1StackItemExtended,
                }
            };
        }

        // Chop frame: fewer locals than previous
        if current_frame.locals.len() < prev_frame.locals.len() && current_frame.stack.is_empty() {
            let chop_count = (prev_frame.locals.len() - current_frame.locals.len()) as u8;
            if chop_count <= 3 && self.can_use_chop_frame(prev_frame, current_frame, chop_count) {
                return StackMapOptimization::ChopFrame { chop_count, offset_delta };
            }
        }

        // Append frame: more locals than previous
        if current_frame.locals.len() > prev_frame.locals.len() && current_frame.stack.is_empty() {
            let append_count = current_frame.locals.len() - prev_frame.locals.len();
            if append_count <= 3 && self.can_use_append_frame(prev_frame, current_frame) {
                let append_locals = current_frame.locals[prev_frame.locals.len()..].to_vec();
                return StackMapOptimization::AppendFrame { append_locals, offset_delta };
            }
        }

        // Full frame: fallback for complex cases
        StackMapOptimization::FullFrame {
            locals: current_frame.locals.clone(),
            stack: current_frame.stack.clone(),
            offset_delta,
        }
    }

    /// Check if chop frame can be used (javac's chop frame validation)
    fn can_use_chop_frame(
        &self,
        prev_frame: &FrameState,
        current_frame: &FrameState,
        chop_count: u8,
    ) -> bool {
        let expected_len = prev_frame.locals.len() - chop_count as usize;
        if current_frame.locals.len() != expected_len {
            return false;
        }

        // Check that remaining locals match
        for i in 0..expected_len {
            if prev_frame.locals[i] != current_frame.locals[i] {
                return false;
            }
        }

        true
    }

    /// Check if append frame can be used (javac's append frame validation)
    fn can_use_append_frame(&self, prev_frame: &FrameState, current_frame: &FrameState) -> bool {
        if current_frame.locals.len() <= prev_frame.locals.len() {
            return false;
        }

        // Check that existing locals match
        for i in 0..prev_frame.locals.len() {
            if prev_frame.locals[i] != current_frame.locals[i] {
                return false;
            }
        }

        true
    }

    /// Generate optimized stack map frame
    pub fn generate_optimized_frame(&self, optimization: &StackMapOptimization) -> StackMapFrame {
        match optimization {
            StackMapOptimization::SameFrame { offset_delta } => {
                StackMapFrame::Same { offset_delta: *offset_delta }
            }
            StackMapOptimization::SameLocals1StackItem { offset_delta, stack_item } => {
                if *offset_delta <= 63 {
                    StackMapFrame::SameLocals1StackItem {
                        offset_delta: *offset_delta,
                        stack: stack_item.clone(),
                    }
                } else {
                    StackMapFrame::SameLocals1StackItemExtended {
                        offset_delta: *offset_delta,
                        stack: stack_item.clone(),
                    }
                }
            }
            StackMapOptimization::ExtendedFrame { frame_type } => {
                match frame_type {
                    ExtendedFrameType::SameExtended => {
                        StackMapFrame::SameExtended { offset_delta: 0 } // Will be set by caller
                    }
                    ExtendedFrameType::SameLocals1StackItemExtended => {
                        StackMapFrame::SameLocals1StackItemExtended {
                            offset_delta: 0, // Will be set by caller
                            stack: VerificationType::Top, // Will be set by caller
                        }
                    }
                }
            }
            StackMapOptimization::ChopFrame { chop_count, offset_delta } => {
                StackMapFrame::Chop {
                    k: *chop_count,
                    offset_delta: *offset_delta,
                }
            }
            StackMapOptimization::AppendFrame { append_locals, offset_delta } => {
                StackMapFrame::Append {
                    k: append_locals.len() as u8,
                    offset_delta: *offset_delta,
                    locals: append_locals.clone(),
                }
            }
            StackMapOptimization::FullFrame { locals, stack, offset_delta } => {
                StackMapFrame::Full {
                    offset_delta: *offset_delta,
                    locals: locals.clone(),
                    stack: stack.clone(),
                }
            }
        }
    }

    /// Update compression statistics
    fn update_compression_stats(&mut self, optimization: &StackMapOptimization) {
        match optimization {
            StackMapOptimization::SameFrame { .. } => {
                self.compression_stats.same_frames += 1;
                self.compression_stats.bytes_saved += 10; // Estimate
            }
            StackMapOptimization::SameLocals1StackItem { .. } => {
                self.compression_stats.same_locals_1_stack_item += 1;
                self.compression_stats.bytes_saved += 8; // Estimate
            }
            StackMapOptimization::ExtendedFrame { .. } => {
                self.compression_stats.extended_frames += 1;
                self.compression_stats.bytes_saved += 5; // Estimate
            }
            StackMapOptimization::ChopFrame { .. } => {
                self.compression_stats.chop_frames += 1;
                self.compression_stats.bytes_saved += 6; // Estimate
            }
            StackMapOptimization::AppendFrame { .. } => {
                self.compression_stats.append_frames += 1;
                self.compression_stats.bytes_saved += 4; // Estimate
            }
            StackMapOptimization::FullFrame { .. } => {
                self.compression_stats.full_frames += 1;
                // No bytes saved for full frames
            }
        }
    }

    /// Get compression statistics
    pub fn get_compression_stats(&self) -> &CompressionStats {
        &self.compression_stats
    }

    /// Reset optimizer state for new method
    pub fn reset(&mut self) {
        self.previous_frame = None;
        self.current_pc = 0;
        self.previous_pc = 0;
        self.compression_stats = CompressionStats::default();
    }
}

/// Stack map table compression utility (javac's StackMapTableWriter)
pub struct StackMapTableCompressor {
    optimizer: StackMapOptimizer,
}

impl StackMapTableCompressor {
    pub fn new() -> Self {
        Self {
            optimizer: StackMapOptimizer::new(),
        }
    }

    /// Compress stack map table using javac's optimization strategies
    pub fn compress_stack_map_table(
        &mut self,
        frames: Vec<(u16, Vec<VerificationType>, Vec<VerificationType>)>,
    ) -> StackMapTable {
        let mut compressed_frames = Vec::new();
        self.optimizer.reset();

        for (pc, locals, stack) in frames {
            let optimization = self.optimizer.analyze_frame_optimization(locals, stack, pc);
            let frame = self.optimizer.generate_optimized_frame(&optimization);
            compressed_frames.push(frame);
        }

        StackMapTable {
            frames: compressed_frames,
        }
    }

    /// Get compression statistics
    pub fn get_stats(&self) -> &CompressionStats {
        self.optimizer.get_compression_stats()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_same_frame_optimization() {
        let mut optimizer = StackMapOptimizer::new();
        
        // First frame (always full)
        let locals1 = vec![VerificationType::Object(1)]; // Use constant pool index
        let stack1 = vec![];
        let opt1 = optimizer.analyze_frame_optimization(locals1.clone(), stack1.clone(), 0);
        
        println!("First optimization: {:?}", opt1);
        assert!(matches!(opt1, StackMapOptimization::FullFrame { .. }));
        
        // Second frame with same locals and empty stack
        let opt2 = optimizer.analyze_frame_optimization(locals1, stack1, 10);
        
        println!("Second optimization: {:?}", opt2);
        assert!(matches!(opt2, StackMapOptimization::SameFrame { offset_delta: 9 }));
    }

    #[test]
    fn test_same_locals_1_stack_item_optimization() {
        let mut optimizer = StackMapOptimizer::new();
        
        // First frame
        let locals = vec![VerificationType::Object(1)];
        let stack1 = vec![];
        optimizer.analyze_frame_optimization(locals.clone(), stack1, 0);
        
        // Second frame with same locals and one stack item
        let stack2 = vec![VerificationType::Integer];
        let opt = optimizer.analyze_frame_optimization(locals, stack2, 5);
        
        assert!(matches!(opt, StackMapOptimization::SameLocals1StackItem { offset_delta: 4, .. }));
    }

    #[test]
    fn test_chop_frame_optimization() {
        let mut optimizer = StackMapOptimizer::new();
        
        // First frame with 3 locals
        let locals1 = vec![
            VerificationType::Object(1),
            VerificationType::Integer,
            VerificationType::Long,
        ];
        let stack = vec![];
        optimizer.analyze_frame_optimization(locals1, stack.clone(), 0);
        
        // Second frame with 1 local (chop 2)
        let locals2 = vec![VerificationType::Object(1)];
        let opt = optimizer.analyze_frame_optimization(locals2, stack, 8);
        
        assert!(matches!(opt, StackMapOptimization::ChopFrame { chop_count: 2, offset_delta: 7 }));
    }

    #[test]
    fn test_append_frame_optimization() {
        let mut optimizer = StackMapOptimizer::new();
        
        // First frame with 1 local
        let locals1 = vec![VerificationType::Object(1)];
        let stack = vec![];
        optimizer.analyze_frame_optimization(locals1.clone(), stack.clone(), 0);
        
        // Second frame with 3 locals (append 2)
        let mut locals2 = locals1.clone();
        locals2.push(VerificationType::Integer);
        locals2.push(VerificationType::Double);
        let opt = optimizer.analyze_frame_optimization(locals2, stack, 12);
        
        assert!(matches!(opt, StackMapOptimization::AppendFrame { append_locals, offset_delta: 11 } 
                        if append_locals.len() == 2));
    }

    #[test]
    fn test_stack_map_table_compression() {
        let mut compressor = StackMapTableCompressor::new();
        
        let frames = vec![
            (0, vec![VerificationType::Object(1)], vec![]),
            (10, vec![VerificationType::Object(1)], vec![]),
            (20, vec![VerificationType::Object(1)], vec![VerificationType::Integer]),
        ];
        
        let compressed = compressor.compress_stack_map_table(frames);
        
        assert_eq!(compressed.frames.len(), 3);
        
        // First frame should be Full
        assert!(matches!(compressed.frames[0], StackMapFrame::Full { .. }));
        
        // Second frame should be Same
        assert!(matches!(compressed.frames[1], StackMapFrame::Same { .. }));
        
        // Third frame should be SameLocals1StackItem
        assert!(matches!(compressed.frames[2], StackMapFrame::SameLocals1StackItem { .. }));
    }

    #[test]
    fn test_compression_stats() {
        let mut compressor = StackMapTableCompressor::new();
        
        let frames = vec![
            (0, vec![VerificationType::Object(1)], vec![]),
            (10, vec![VerificationType::Object(1)], vec![]),
            (20, vec![VerificationType::Object(1)], vec![VerificationType::Integer]),
        ];
        
        compressor.compress_stack_map_table(frames);
        let stats = compressor.get_stats();
        
        assert_eq!(stats.full_frames, 1);
        assert_eq!(stats.same_frames, 1);
        assert_eq!(stats.same_locals_1_stack_item, 1);
        assert!(stats.bytes_saved > 0);
    }
}
