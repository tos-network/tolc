use crate::codegen::opcode_enum::Opcode;
use crate::ast::*;
use std::collections::{HashMap, HashSet};

/// JavaC-style JSR/RET optimization for subroutine calls and finally blocks
/// 
/// This implements the JSR (Jump to Subroutine) and RET (Return from Subroutine) 
/// optimization logic from javac's Code.java, including jsrlimit control and
/// finalizer inlining decisions.
#[derive(Debug, Clone)]
pub struct JsrRetOptimizer {
    /// Maximum number of JSR instructions allowed before switching to inlining
    pub jsr_limit: u32,
    
    /// Current JSR count in the method
    pub current_jsr_count: u32,
    
    /// Subroutine definitions and their usage
    pub subroutines: HashMap<String, SubroutineInfo>,
    
    /// Finally block information for inlining decisions
    pub finally_blocks: HashMap<String, FinallyBlockInfo>,
    
    /// Statistics for optimization decisions
    pub stats: JsrRetStats,
    
    /// Whether to prefer inlining over JSR/RET
    pub prefer_inlining: bool,
}

#[derive(Debug, Clone)]
pub struct SubroutineInfo {
    /// Unique identifier for this subroutine
    pub id: String,
    
    /// Program counter where the subroutine starts
    pub start_pc: u32,
    
    /// Program counter where the subroutine ends
    pub end_pc: Option<u32>,
    
    /// Number of times this subroutine is called
    pub call_count: u32,
    
    /// Size of the subroutine in bytes
    pub size_bytes: u32,
    
    /// Whether this subroutine modifies local variables
    pub modifies_locals: bool,
    
    /// Local variables used by this subroutine
    pub used_locals: HashSet<u16>,
    
    /// Whether this subroutine can throw exceptions
    pub can_throw: bool,
    
    /// Complexity score for inlining decisions
    pub complexity_score: u32,
}

#[derive(Debug, Clone)]
pub struct FinallyBlockInfo {
    /// Unique identifier for this finally block
    pub id: String,
    
    /// Associated try block information
    pub try_block_id: String,
    
    /// Size of the finally block
    pub size_bytes: u32,
    
    /// Number of exit points that need this finally block
    pub exit_point_count: u32,
    
    /// Whether this finally block should be inlined
    pub should_inline: bool,
    
    /// Inlining decision reason
    pub inline_reason: InlineReason,
    
    /// Exception types that trigger this finally block
    pub exception_types: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InlineReason {
    /// Small size, worth inlining
    SmallSize,
    
    /// Too many JSR instructions, switch to inlining
    JsrLimitExceeded,
    
    /// Complex control flow, inlining is clearer
    ComplexControlFlow,
    
    /// Performance critical path
    PerformanceCritical,
    
    /// Exception handling requirements
    ExceptionHandling,
    
    /// JSR/RET preferred for code size
    PreferJsrRet,
    
    /// User configuration preference
    UserPreference,
}

#[derive(Debug, Clone, Default)]
pub struct JsrRetStats {
    /// Total number of subroutines created
    pub total_subroutines: u32,
    
    /// Number of subroutines using JSR/RET
    pub jsr_ret_subroutines: u32,
    
    /// Number of subroutines inlined
    pub inlined_subroutines: u32,
    
    /// Total JSR instructions generated
    pub total_jsr_instructions: u32,
    
    /// Total RET instructions generated
    pub total_ret_instructions: u32,
    
    /// Bytes saved through subroutines
    pub bytes_saved: u32,
    
    /// Bytes added due to inlining
    pub bytes_added_inlining: u32,
}

impl JsrRetOptimizer {
    /// Create a new JSR/RET optimizer with default settings
    pub fn new() -> Self {
        Self {
            jsr_limit: 50, // JavaC default jsrlimit
            current_jsr_count: 0,
            subroutines: HashMap::new(),
            finally_blocks: HashMap::new(),
            stats: JsrRetStats::default(),
            prefer_inlining: false,
        }
    }
    
    /// Create a JSR/RET optimizer with custom JSR limit
    pub fn with_jsr_limit(jsr_limit: u32) -> Self {
        let mut optimizer = Self::new();
        optimizer.jsr_limit = jsr_limit;
        optimizer
    }
    
    /// Analyze a try-finally statement for optimization opportunities
    pub fn analyze_try_finally(&mut self, try_stmt: &TryStmt) -> Result<FinallyOptimization, String> {
        let finally_id = format!("finally_{}", self.finally_blocks.len());
        
        // Analyze the finally block if present
        if let Some(finally_block) = &try_stmt.finally_block {
            let finally_info = self.analyze_finally_block(&finally_id, finally_block)?;
            
            // Make inlining decision
            let optimization = self.decide_finally_optimization(&finally_info)?;
            
            self.finally_blocks.insert(finally_id.clone(), finally_info);
            
            eprintln!("🔍 DEBUG: JsrRetOptimizer: Analyzed finally block {} -> {:?}", 
                     finally_id, optimization.strategy);
            
            Ok(optimization)
        } else {
            Ok(FinallyOptimization {
                strategy: OptimizationStrategy::NoFinally,
                finally_id,
                estimated_size_change: 0,
                performance_impact: PerformanceImpact::Neutral,
            })
        }
    }
    
    /// Analyze a finally block to determine its characteristics
    fn analyze_finally_block(&mut self, finally_id: &str, finally_block: &Block) -> Result<FinallyBlockInfo, String> {
        let mut info = FinallyBlockInfo {
            id: finally_id.to_string(),
            try_block_id: format!("try_{}", finally_id),
            size_bytes: 0,
            exit_point_count: 0,
            should_inline: false,
            inline_reason: InlineReason::PreferJsrRet,
            exception_types: Vec::new(),
        };
        
        // Estimate size of finally block
        info.size_bytes = self.estimate_block_size(finally_block);
        
        // Count potential exit points (return statements, throws, etc.)
        info.exit_point_count = self.count_exit_points(finally_block);
        
        // Analyze exception handling requirements
        info.exception_types = self.analyze_exception_types(finally_block);
        
        eprintln!("🔍 DEBUG: JsrRetOptimizer: Finally block {} - size: {}, exits: {}, exceptions: {}", 
                 finally_id, info.size_bytes, info.exit_point_count, info.exception_types.len());
        
        Ok(info)
    }
    
    /// Decide whether to inline or use JSR/RET for a finally block
    fn decide_finally_optimization(&mut self, finally_info: &FinallyBlockInfo) -> Result<FinallyOptimization, String> {
        let mut strategy = OptimizationStrategy::UseJsrRet;
        let mut reason = InlineReason::PreferJsrRet;
        
        // Check JSR limit
        if self.current_jsr_count >= self.jsr_limit {
            strategy = OptimizationStrategy::Inline;
            reason = InlineReason::JsrLimitExceeded;
        }
        // Small finally blocks are good candidates for inlining
        else if finally_info.size_bytes <= 20 {
            strategy = OptimizationStrategy::Inline;
            reason = InlineReason::SmallSize;
        }
        // Many exit points favor inlining
        else if finally_info.exit_point_count > 3 {
            strategy = OptimizationStrategy::Inline;
            reason = InlineReason::ComplexControlFlow;
        }
        // Complex exception handling favors inlining
        else if finally_info.exception_types.len() > 2 {
            strategy = OptimizationStrategy::Inline;
            reason = InlineReason::ExceptionHandling;
        }
        // User preference
        else if self.prefer_inlining {
            strategy = OptimizationStrategy::Inline;
            reason = InlineReason::UserPreference;
        }
        
        // Calculate estimated size change
        let estimated_size_change = match strategy {
            OptimizationStrategy::Inline => {
                // Inlining: size * exit_points - JSR/RET overhead
                (finally_info.size_bytes * finally_info.exit_point_count) as i32 - 6
            },
            OptimizationStrategy::UseJsrRet => {
                // JSR/RET: subroutine + JSR calls
                finally_info.size_bytes as i32 + (finally_info.exit_point_count * 3) as i32
            },
            _ => 0,
        };
        
        // Determine performance impact
        let performance_impact = match strategy {
            OptimizationStrategy::Inline => {
                if finally_info.size_bytes <= 10 {
                    PerformanceImpact::Positive
                } else {
                    PerformanceImpact::Negative
                }
            },
            OptimizationStrategy::UseJsrRet => PerformanceImpact::Neutral,
            _ => PerformanceImpact::Neutral,
        };
        
        eprintln!("🔍 DEBUG: JsrRetOptimizer: Finally optimization decision - strategy: {:?}, reason: {:?}, size_change: {}", 
                 strategy, reason, estimated_size_change);
        
        Ok(FinallyOptimization {
            strategy,
            finally_id: finally_info.id.clone(),
            estimated_size_change,
            performance_impact,
        })
    }
    
    /// Create a subroutine for a finally block
    pub fn create_subroutine(&mut self, subroutine_id: &str, start_pc: u32) -> Result<(), String> {
        let subroutine_info = SubroutineInfo {
            id: subroutine_id.to_string(),
            start_pc,
            end_pc: None,
            call_count: 0,
            size_bytes: 0,
            modifies_locals: false,
            used_locals: HashSet::new(),
            can_throw: false,
            complexity_score: 0,
        };
        
        self.subroutines.insert(subroutine_id.to_string(), subroutine_info);
        self.stats.total_subroutines += 1;
        
        eprintln!("🔍 DEBUG: JsrRetOptimizer: Created subroutine {} at PC {}", subroutine_id, start_pc);
        
        Ok(())
    }
    
    /// Generate JSR instruction to call a subroutine
    pub fn generate_jsr_call(&mut self, subroutine_id: &str, target_pc: u32) -> Result<Vec<u8>, String> {
        // Check JSR limit
        if self.current_jsr_count >= self.jsr_limit {
            return Err(format!("JSR limit ({}) exceeded", self.jsr_limit));
        }
        
        // Update subroutine call count
        if let Some(subroutine) = self.subroutines.get_mut(subroutine_id) {
            subroutine.call_count += 1;
        }
        
        self.current_jsr_count += 1;
        self.stats.total_jsr_instructions += 1;
        
        // Generate JSR instruction (opcode + 2-byte offset)
        let mut bytecode = Vec::new();
        bytecode.push(Opcode::Jsr.to_byte());
        
        // Calculate offset (placeholder for now)
        let offset = target_pc as i16;
        bytecode.extend_from_slice(&offset.to_be_bytes());
        
        eprintln!("🔍 DEBUG: JsrRetOptimizer: Generated JSR to {} (target PC: {})", subroutine_id, target_pc);
        
        Ok(bytecode)
    }
    
    /// Generate RET instruction to return from a subroutine
    pub fn generate_ret_instruction(&mut self, local_var_index: u16) -> Result<Vec<u8>, String> {
        self.stats.total_ret_instructions += 1;
        
        let mut bytecode = Vec::new();
        
        if local_var_index <= 255 {
            // Normal RET instruction
            bytecode.push(Opcode::Ret.to_byte());
            bytecode.push(local_var_index as u8);
        } else {
            // Wide RET instruction
            bytecode.push(Opcode::Wide.to_byte());
            bytecode.push(Opcode::Ret.to_byte());
            bytecode.extend_from_slice(&(local_var_index).to_be_bytes());
        }
        
        eprintln!("🔍 DEBUG: JsrRetOptimizer: Generated RET instruction (local var: {})", local_var_index);
        
        Ok(bytecode)
    }
    
    /// Estimate the size of a code block in bytes
    fn estimate_block_size(&self, block: &Block) -> u32 {
        let mut size = 0;
        
        for stmt in &block.statements {
            size += self.estimate_statement_size(stmt);
        }
        
        size
    }
    
    /// Estimate the size of a statement in bytes
    fn estimate_statement_size(&self, stmt: &Stmt) -> u32 {
        match stmt {
            Stmt::Expression(expr_stmt) => self.estimate_expression_size(&expr_stmt.expr),
            Stmt::If(_) => 10, // Rough estimate for if statement
            Stmt::While(_) => 15, // Rough estimate for while loop
            Stmt::For(_) => 20, // Rough estimate for for loop
            Stmt::Try(_) => 25, // Rough estimate for try statement
            Stmt::Return(_) => 3, // Return instruction
            Stmt::Throw(_) => 5, // Throw instruction
            Stmt::Block(block) => self.estimate_block_size(block),
            _ => 5, // Default estimate
        }
    }
    
    /// Estimate the size of an expression in bytes
    fn estimate_expression_size(&self, expr: &Expr) -> u32 {
        match expr {
            Expr::Literal(_) => 2, // Load constant
            Expr::Identifier(_) => 2, // Load variable
            Expr::Binary(_) => 5, // Binary operation
            Expr::Unary(_) => 3, // Unary operation
            Expr::MethodCall(_) => 8, // Method invocation
            Expr::FieldAccess(_) => 4, // Field access
            Expr::Assignment(_) => 4, // Assignment
            _ => 3, // Default estimate
        }
    }
    
    /// Count the number of exit points in a block
    fn count_exit_points(&self, block: &Block) -> u32 {
        let mut count = 0;
        
        for stmt in &block.statements {
            count += self.count_statement_exit_points(stmt);
        }
        
        count
    }
    
    /// Count exit points in a statement
    fn count_statement_exit_points(&self, stmt: &Stmt) -> u32 {
        match stmt {
            Stmt::Return(_) => 1,
            Stmt::Throw(_) => 1,
            Stmt::If(if_stmt) => {
                let mut count = 0;
                count += self.count_statement_exit_points(&if_stmt.then_branch);
                if let Some(else_stmt) = &if_stmt.else_branch {
                    count += self.count_statement_exit_points(else_stmt);
                }
                count
            },
            Stmt::Block(block) => self.count_exit_points(block),
            _ => 0,
        }
    }
    
    /// Analyze exception types that might be thrown
    fn analyze_exception_types(&self, _block: &Block) -> Vec<String> {
        // Simplified analysis - in a real implementation, this would
        // analyze method calls and throw statements
        vec!["java.lang.Exception".to_string()]
    }
    
    /// Finalize a subroutine (set end PC and calculate final size)
    pub fn finalize_subroutine(&mut self, subroutine_id: &str, end_pc: u32) -> Result<(), String> {
        if let Some(subroutine) = self.subroutines.get_mut(subroutine_id) {
            subroutine.end_pc = Some(end_pc);
            subroutine.size_bytes = end_pc - subroutine.start_pc;
            
            // Update statistics
            if subroutine.call_count > 0 {
                self.stats.jsr_ret_subroutines += 1;
                
                // Calculate bytes saved (original size * calls - subroutine size - JSR overhead)
                let original_size = subroutine.size_bytes * subroutine.call_count;
                let optimized_size = subroutine.size_bytes + (subroutine.call_count * 3); // 3 bytes per JSR
                if original_size > optimized_size {
                    self.stats.bytes_saved += original_size - optimized_size;
                }
            }
            
            eprintln!("🔍 DEBUG: JsrRetOptimizer: Finalized subroutine {} - size: {} bytes, calls: {}", 
                     subroutine_id, subroutine.size_bytes, subroutine.call_count);
            
            Ok(())
        } else {
            Err(format!("Subroutine {} not found", subroutine_id))
        }
    }
    
    /// Get optimization statistics
    pub fn get_stats(&self) -> &JsrRetStats {
        &self.stats
    }
    
    /// Reset for new method compilation
    pub fn reset(&mut self) {
        self.current_jsr_count = 0;
        self.subroutines.clear();
        self.finally_blocks.clear();
    }
    
    /// Check if JSR limit has been reached
    pub fn is_jsr_limit_reached(&self) -> bool {
        self.current_jsr_count >= self.jsr_limit
    }
    
    /// Set preference for inlining vs JSR/RET
    pub fn set_prefer_inlining(&mut self, prefer: bool) {
        self.prefer_inlining = prefer;
    }
    
    /// Generate a comprehensive optimization report
    pub fn generate_report(&self) -> String {
        let mut report = String::new();
        report.push_str("=== JSR/RET Optimization Report ===\n");
        report.push_str(&format!("JSR Limit: {}\n", self.jsr_limit));
        report.push_str(&format!("Current JSR Count: {}\n", self.current_jsr_count));
        report.push_str(&format!("Total Subroutines: {}\n", self.stats.total_subroutines));
        report.push_str(&format!("JSR/RET Subroutines: {}\n", self.stats.jsr_ret_subroutines));
        report.push_str(&format!("Inlined Subroutines: {}\n", self.stats.inlined_subroutines));
        report.push_str(&format!("Total JSR Instructions: {}\n", self.stats.total_jsr_instructions));
        report.push_str(&format!("Total RET Instructions: {}\n", self.stats.total_ret_instructions));
        report.push_str(&format!("Bytes Saved: {}\n", self.stats.bytes_saved));
        report.push_str(&format!("Bytes Added (Inlining): {}\n", self.stats.bytes_added_inlining));
        
        report.push_str("\n=== Subroutine Details ===\n");
        for (id, subroutine) in &self.subroutines {
            report.push_str(&format!("Subroutine {}: PC {}-{:?}, {} calls, {} bytes\n", 
                                   id, subroutine.start_pc, subroutine.end_pc, 
                                   subroutine.call_count, subroutine.size_bytes));
        }
        
        report.push_str("\n=== Finally Block Details ===\n");
        for (id, finally_block) in &self.finally_blocks {
            report.push_str(&format!("Finally {}: {} bytes, {} exits, inline: {} ({:?})\n", 
                                   id, finally_block.size_bytes, finally_block.exit_point_count,
                                   finally_block.should_inline, finally_block.inline_reason));
        }
        
        report
    }
}

#[derive(Debug, Clone)]
pub struct FinallyOptimization {
    /// Optimization strategy to use
    pub strategy: OptimizationStrategy,
    
    /// ID of the finally block
    pub finally_id: String,
    
    /// Estimated change in code size (positive = larger, negative = smaller)
    pub estimated_size_change: i32,
    
    /// Expected performance impact
    pub performance_impact: PerformanceImpact,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OptimizationStrategy {
    /// Inline the finally block at each exit point
    Inline,
    
    /// Use JSR/RET subroutine pattern
    UseJsrRet,
    
    /// No finally block present
    NoFinally,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PerformanceImpact {
    /// Expected to improve performance
    Positive,
    
    /// No significant performance impact
    Neutral,
    
    /// May hurt performance
    Negative,
}

impl Default for JsrRetOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    
    #[test]
    fn test_jsr_ret_basic_functionality() {
        let mut optimizer = JsrRetOptimizer::new();
        
        // Test subroutine creation
        optimizer.create_subroutine("test_sub", 100).unwrap();
        assert_eq!(optimizer.subroutines.len(), 1);
        
        // Test JSR generation
        let jsr_bytecode = optimizer.generate_jsr_call("test_sub", 100).unwrap();
        assert_eq!(jsr_bytecode[0], Opcode::Jsr.to_byte());
        assert_eq!(optimizer.current_jsr_count, 1);
        
        // Test RET generation
        let ret_bytecode = optimizer.generate_ret_instruction(5).unwrap();
        assert_eq!(ret_bytecode[0], Opcode::Ret.to_byte());
        assert_eq!(ret_bytecode[1], 5);
        
        // Test subroutine finalization
        optimizer.finalize_subroutine("test_sub", 150).unwrap();
        let subroutine = optimizer.subroutines.get("test_sub").unwrap();
        assert_eq!(subroutine.size_bytes, 50);
    }
    
    #[test]
    fn test_jsr_limit_enforcement() {
        let mut optimizer = JsrRetOptimizer::with_jsr_limit(2);
        
        // Should succeed
        optimizer.generate_jsr_call("sub1", 100).unwrap();
        optimizer.generate_jsr_call("sub2", 200).unwrap();
        
        // Should fail due to limit
        assert!(optimizer.generate_jsr_call("sub3", 300).is_err());
        assert!(optimizer.is_jsr_limit_reached());
    }
    
    #[test]
    fn test_finally_optimization_decision() {
        let mut optimizer = JsrRetOptimizer::new();
        
        // Create a small finally block (should be inlined)
        let small_block = Block { 
            statements: vec![],
            span: crate::ast::Span::from_to(0, 0, 0, 0),
        };
        let finally_info = FinallyBlockInfo {
            id: "small_finally".to_string(),
            try_block_id: "try_1".to_string(),
            size_bytes: 10, // Small size
            exit_point_count: 2,
            should_inline: false,
            inline_reason: InlineReason::PreferJsrRet,
            exception_types: vec![],
        };
        
        let optimization = optimizer.decide_finally_optimization(&finally_info).unwrap();
        assert_eq!(optimization.strategy, OptimizationStrategy::Inline);
        assert_eq!(optimization.performance_impact, PerformanceImpact::Positive);
    }
}
