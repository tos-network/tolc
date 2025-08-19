/// Exception handling optimization (javac-style)
/// Implements javac's sophisticated exception table management and JSR optimization

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ExceptionRange {
    pub start_pc: usize,
    pub end_pc: usize,
    pub handler_pc: usize,
    pub catch_type: Option<String>, // None for catch-all (finally)
}

#[derive(Debug, Clone)]
pub struct ExceptionGap {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct TryBlock {
    pub start_pc: usize,
    pub end_pc: usize,
    pub catch_blocks: Vec<CatchBlock>,
    pub finally_block: Option<FinallyBlock>,
    pub gaps: Vec<ExceptionGap>,
}

#[derive(Debug, Clone)]
pub struct CatchBlock {
    pub exception_type: String,
    pub handler_pc: usize,
}

#[derive(Debug, Clone)]
pub struct FinallyBlock {
    pub handler_pc: usize,
    pub has_jsr_optimization: bool,
}

pub struct ExceptionOptimizer {
    /// Exception ranges being built
    ranges: Vec<ExceptionRange>,
    /// Current gaps in exception coverage
    gaps: Vec<ExceptionGap>,
    /// JSR targets for finally blocks
    jsr_targets: HashMap<usize, Vec<usize>>,
}

impl ExceptionOptimizer {
    pub fn new() -> Self {
        ExceptionOptimizer {
            ranges: Vec::new(),
            gaps: Vec::new(),
            jsr_targets: HashMap::new(),
        }
    }
    
    /// Register exception range (javac registerCatch pattern)
    pub fn register_catch(&mut self, start_pc: usize, end_pc: usize, handler_pc: usize, catch_type: Option<String>) {
        if start_pc != end_pc {
            self.ranges.push(ExceptionRange {
                start_pc,
                end_pc,
                handler_pc,
                catch_type,
            });
        }
    }
    
    /// Add gap in exception coverage (javac gaps pattern)
    pub fn add_gap(&mut self, start: usize, end: usize) {
        self.gaps.push(ExceptionGap { start, end });
    }
    
    /// Generate optimized try-catch-finally block (javac visitTry pattern)
    pub fn generate_try_block(&mut self, try_block: &TryBlock) -> TryBlockOptimization {
        let mut optimization = TryBlockOptimization {
            exception_ranges: Vec::new(),
            jsr_optimizations: Vec::new(),
            register_segments: Vec::new(),
            has_catch_all: false,
        };
        
        // Generate catch blocks
        for catch_block in &try_block.catch_blocks {
            self.register_catch(
                try_block.start_pc,
                try_block.end_pc,
                catch_block.handler_pc,
                Some(catch_block.exception_type.clone()),
            );
            
            optimization.exception_ranges.push(ExceptionRange {
                start_pc: try_block.start_pc,
                end_pc: try_block.end_pc,
                handler_pc: catch_block.handler_pc,
                catch_type: Some(catch_block.exception_type.clone()),
            });
        }
        
        // Generate finally block with JSR optimization
        if let Some(finally_block) = &try_block.finally_block {
            optimization.has_catch_all = true;
            
            if finally_block.has_jsr_optimization {
                // javac JSR optimization pattern
                optimization.jsr_optimizations.push(JSROptimization {
                    finally_pc: finally_block.handler_pc,
                    jsr_sites: self.collect_jsr_sites(finally_block.handler_pc),
                    use_ret_instruction: true,
                });
            }
            
            // Register catch-all for finally
            self.register_catch_all_with_gaps(
                try_block.start_pc,
                try_block.end_pc,
                finally_block.handler_pc,
                &try_block.gaps,
            );
            
            // Create new register segment (javac newRegSegment pattern)
            optimization.register_segments.push(RegisterSegment {
                start_pc: finally_block.handler_pc,
                purpose: "finally_block".to_string(),
            });
        }
        
        optimization
    }
    
    /// Register catch-all with gaps (javac pattern)
    fn register_catch_all_with_gaps(&mut self, start_pc: usize, end_pc: usize, handler_pc: usize, gaps: &[ExceptionGap]) {
        let mut current_start = start_pc;
        
        for gap in gaps {
            // Register range before gap
            if current_start < gap.start {
                self.register_catch(current_start, gap.start, handler_pc, None);
            }
            current_start = gap.end;
        }
        
        // Register final range after last gap
        if current_start < end_pc {
            self.register_catch(current_start, end_pc, handler_pc, None);
        }
    }
    
    /// Collect JSR sites for finally block
    fn collect_jsr_sites(&self, finally_pc: usize) -> Vec<usize> {
        self.jsr_targets.get(&finally_pc).cloned().unwrap_or_default()
    }
    
    /// Add JSR site (javac pattern)
    pub fn add_jsr_site(&mut self, jsr_pc: usize, target_pc: usize) {
        self.jsr_targets.entry(target_pc).or_insert_with(Vec::new).push(jsr_pc);
    }
    
    /// Optimize exception table (javac compressCatchTable pattern)
    pub fn optimize_exception_table(&mut self) -> Vec<ExceptionRange> {
        // Sort ranges by start_pc, then by end_pc, then by handler_pc
        self.ranges.sort_by(|a, b| {
            a.start_pc.cmp(&b.start_pc)
                .then(a.end_pc.cmp(&b.end_pc))
                .then(a.handler_pc.cmp(&b.handler_pc))
        });
        
        // Remove duplicates and merge overlapping ranges
        let mut optimized = Vec::new();
        let mut current: Option<ExceptionRange> = None;
        
        for range in &self.ranges {
            match &current {
                None => current = Some(range.clone()),
                Some(curr) => {
                    if Self::can_merge_ranges(curr, range) {
                        // Merge ranges
                        current = Some(ExceptionRange {
                            start_pc: curr.start_pc.min(range.start_pc),
                            end_pc: curr.end_pc.max(range.end_pc),
                            handler_pc: curr.handler_pc,
                            catch_type: curr.catch_type.clone(),
                        });
                    } else {
                        optimized.push(curr.clone());
                        current = Some(range.clone());
                    }
                }
            }
        }
        
        if let Some(curr) = current {
            optimized.push(curr);
        }
        
        optimized
    }
    
    /// Check if two exception ranges can be merged
    fn can_merge_ranges(a: &ExceptionRange, b: &ExceptionRange) -> bool {
        a.handler_pc == b.handler_pc && 
        a.catch_type == b.catch_type &&
        (a.end_pc >= b.start_pc || b.end_pc >= a.start_pc) // Overlapping or adjacent
    }
    
    /// Generate exception table bytecode
    pub fn generate_exception_table_bytecode(&self) -> Vec<u8> {
        let optimized_ranges = self.ranges.clone();
        let mut bytecode = Vec::new();
        
        // Exception table count (2 bytes)
        bytecode.extend_from_slice(&(optimized_ranges.len() as u16).to_be_bytes());
        
        // Exception table entries (8 bytes each)
        for range in &optimized_ranges {
            bytecode.extend_from_slice(&(range.start_pc as u16).to_be_bytes());
            bytecode.extend_from_slice(&(range.end_pc as u16).to_be_bytes());
            bytecode.extend_from_slice(&(range.handler_pc as u16).to_be_bytes());
            
            // Catch type index (0 for catch-all)
            let catch_type_index: u16 = match &range.catch_type {
                Some(_) => 1, // Placeholder - would be actual constant pool index
                None => 0,
            };
            bytecode.extend_from_slice(&catch_type_index.to_be_bytes());
        }
        
        bytecode
    }
}

#[derive(Debug, Clone)]
pub struct TryBlockOptimization {
    pub exception_ranges: Vec<ExceptionRange>,
    pub jsr_optimizations: Vec<JSROptimization>,
    pub register_segments: Vec<RegisterSegment>,
    pub has_catch_all: bool,
}

#[derive(Debug, Clone)]
pub struct JSROptimization {
    pub finally_pc: usize,
    pub jsr_sites: Vec<usize>,
    pub use_ret_instruction: bool,
}

#[derive(Debug, Clone)]
pub struct RegisterSegment {
    pub start_pc: usize,
    pub purpose: String,
}

/// Advanced exception optimization patterns
pub struct AdvancedExceptionOptimizer;

impl AdvancedExceptionOptimizer {
    /// Optimize nested try-catch blocks
    pub fn optimize_nested_try_blocks(blocks: &[TryBlock]) -> NestedTryOptimization {
        let mut optimization = NestedTryOptimization {
            merged_ranges: Vec::new(),
            shared_finally_blocks: Vec::new(),
            optimization_benefit: 0,
        };
        
        // Analyze nesting patterns
        for (i, outer_block) in blocks.iter().enumerate() {
            for (j, inner_block) in blocks.iter().enumerate() {
                if i != j && Self::is_nested(outer_block, inner_block) {
                    // Found nested try block - can optimize
                    optimization.optimization_benefit += 1;
                    
                    // Check for shared finally blocks
                    if let (Some(outer_finally), Some(inner_finally)) = 
                        (&outer_block.finally_block, &inner_block.finally_block) {
                        if Self::can_share_finally(outer_finally, inner_finally) {
                            optimization.shared_finally_blocks.push(SharedFinallyBlock {
                                outer_pc: outer_finally.handler_pc,
                                inner_pc: inner_finally.handler_pc,
                                shared_pc: outer_finally.handler_pc, // Use outer as shared
                            });
                        }
                    }
                }
            }
        }
        
        optimization
    }
    
    /// Check if one try block is nested inside another
    fn is_nested(outer: &TryBlock, inner: &TryBlock) -> bool {
        outer.start_pc <= inner.start_pc && inner.end_pc <= outer.end_pc
    }
    
    /// Check if finally blocks can be shared
    fn can_share_finally(outer: &FinallyBlock, inner: &FinallyBlock) -> bool {
        // Simple heuristic - can be made more sophisticated
        outer.has_jsr_optimization == inner.has_jsr_optimization
    }
    
    /// Analyze exception handling performance
    pub fn analyze_exception_performance(ranges: &[ExceptionRange]) -> ExceptionPerformanceAnalysis {
        let mut analysis = ExceptionPerformanceAnalysis {
            total_ranges: ranges.len(),
            catch_all_count: 0,
            specific_catch_count: 0,
            overlapping_ranges: 0,
            optimization_opportunities: Vec::new(),
        };
        
        for range in ranges {
            match &range.catch_type {
                None => analysis.catch_all_count += 1,
                Some(_) => analysis.specific_catch_count += 1,
            }
        }
        
        // Check for overlapping ranges
        for (i, range1) in ranges.iter().enumerate() {
            for (j, range2) in ranges.iter().enumerate() {
                if i != j && Self::ranges_overlap(range1, range2) {
                    analysis.overlapping_ranges += 1;
                }
            }
        }
        analysis.overlapping_ranges /= 2; // Each overlap counted twice
        
        // Suggest optimizations
        if analysis.catch_all_count > 1 {
            analysis.optimization_opportunities.push(
                "Multiple catch-all blocks can be merged".to_string()
            );
        }
        
        if analysis.overlapping_ranges > 0 {
            analysis.optimization_opportunities.push(
                "Overlapping exception ranges can be optimized".to_string()
            );
        }
        
        analysis
    }
    
    /// Check if two exception ranges overlap
    fn ranges_overlap(a: &ExceptionRange, b: &ExceptionRange) -> bool {
        !(a.end_pc <= b.start_pc || b.end_pc <= a.start_pc)
    }
}

#[derive(Debug, Clone)]
pub struct NestedTryOptimization {
    pub merged_ranges: Vec<ExceptionRange>,
    pub shared_finally_blocks: Vec<SharedFinallyBlock>,
    pub optimization_benefit: usize,
}

#[derive(Debug, Clone)]
pub struct SharedFinallyBlock {
    pub outer_pc: usize,
    pub inner_pc: usize,
    pub shared_pc: usize,
}

#[derive(Debug, Clone)]
pub struct ExceptionPerformanceAnalysis {
    pub total_ranges: usize,
    pub catch_all_count: usize,
    pub specific_catch_count: usize,
    pub overlapping_ranges: usize,
    pub optimization_opportunities: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_exception_range_registration() {
        let mut optimizer = ExceptionOptimizer::new();
        
        optimizer.register_catch(10, 20, 30, Some("java/lang/Exception".to_string()));
        optimizer.register_catch(25, 35, 40, None); // catch-all
        
        assert_eq!(optimizer.ranges.len(), 2);
        assert_eq!(optimizer.ranges[0].start_pc, 10);
        assert_eq!(optimizer.ranges[0].end_pc, 20);
        assert_eq!(optimizer.ranges[0].handler_pc, 30);
        assert!(optimizer.ranges[0].catch_type.is_some());
        
        assert_eq!(optimizer.ranges[1].catch_type, None); // catch-all
    }
    
    #[test]
    fn test_exception_table_optimization() {
        let mut optimizer = ExceptionOptimizer::new();
        
        // Add overlapping ranges that can be merged
        optimizer.register_catch(10, 20, 30, Some("java/lang/Exception".to_string()));
        optimizer.register_catch(15, 25, 30, Some("java/lang/Exception".to_string()));
        
        let optimized = optimizer.optimize_exception_table();
        
        // Should be merged into one range
        assert_eq!(optimized.len(), 1);
        assert_eq!(optimized[0].start_pc, 10);
        assert_eq!(optimized[0].end_pc, 25);
        assert_eq!(optimized[0].handler_pc, 30);
    }
    
    #[test]
    fn test_gap_handling() {
        let mut optimizer = ExceptionOptimizer::new();
        
        let gaps = vec![
            ExceptionGap { start: 15, end: 18 },
            ExceptionGap { start: 22, end: 25 },
        ];
        
        optimizer.register_catch_all_with_gaps(10, 30, 40, &gaps);
        
        // Should create 3 ranges: 10-15, 18-22, 25-30
        assert_eq!(optimizer.ranges.len(), 3);
        assert_eq!(optimizer.ranges[0].start_pc, 10);
        assert_eq!(optimizer.ranges[0].end_pc, 15);
        assert_eq!(optimizer.ranges[1].start_pc, 18);
        assert_eq!(optimizer.ranges[1].end_pc, 22);
        assert_eq!(optimizer.ranges[2].start_pc, 25);
        assert_eq!(optimizer.ranges[2].end_pc, 30);
    }
    
    #[test]
    fn test_jsr_optimization() {
        let mut optimizer = ExceptionOptimizer::new();
        
        optimizer.add_jsr_site(100, 200); // JSR at 100 targeting 200
        optimizer.add_jsr_site(150, 200); // JSR at 150 targeting 200
        
        let jsr_sites = optimizer.collect_jsr_sites(200);
        assert_eq!(jsr_sites.len(), 2);
        assert!(jsr_sites.contains(&100));
        assert!(jsr_sites.contains(&150));
    }
    
    #[test]
    fn test_performance_analysis() {
        let ranges = vec![
            ExceptionRange {
                start_pc: 10,
                end_pc: 20,
                handler_pc: 30,
                catch_type: Some("java/lang/Exception".to_string()),
            },
            ExceptionRange {
                start_pc: 25,
                end_pc: 35,
                handler_pc: 40,
                catch_type: None, // catch-all
            },
        ];
        
        let analysis = AdvancedExceptionOptimizer::analyze_exception_performance(&ranges);
        
        assert_eq!(analysis.total_ranges, 2);
        assert_eq!(analysis.specific_catch_count, 1);
        assert_eq!(analysis.catch_all_count, 1);
        assert_eq!(analysis.overlapping_ranges, 0);
    }
}
