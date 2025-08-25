//! Unified optimization manager for bytecode generation
//!
//! This module provides a centralized optimization management system that coordinates
//! all 20+ individual optimizers in the codebase. It follows javac's optimization
//! pipeline pattern and provides unified control over optimization passes.

use crate::ast::*;
use crate::common::error::Result;
use crate::codegen::{
    instruction_optimizer::{InstructionOptimizer, PeepholeOptimizer},
    javac_jump_optimizer::JavacJumpOptimizer,
    constant_optimizer::{ConstantOptimizer, ConstantInstruction},
    branch_optimizer::{BranchOptimizer, BranchOptimizationContext},
};
use std::collections::{HashMap, HashSet};

/// Optimization types available in the system
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum OptimizationType {
    // Control flow optimizations
    ConstantFolding,
    DeadCodeElimination,
    LoopOptimization,
    JumpOptimization,
    BranchOptimization,
    
    // Data flow optimizations
    AssignmentOptimization,
    IncrementOptimization,
    FieldAccessOptimization,
    
    // Method call optimizations
    MethodInvocationOptimization,
    StaticCallOptimization,
    VirtualCallOptimization,
    
    // String optimizations
    StringOptimization,
    StringBufferOptimization,
    StringConcatenationOptimization,
    
    // Type optimizations
    TypeCoercionOptimization,
    CastOptimization,
    PrimitiveOptimization,
    
    // Object optimizations
    ObjectOptimization,
    ArrayOptimization,
    InstanceOfOptimization,
    
    // Advanced optimizations
    InliningOptimization,
    TailCallOptimization,
    RegisterAllocation,
    
    // Exception optimizations
    ExceptionOptimization,
    FinalizerOptimization,
    
    // Instruction-level optimizations
    InstructionOptimization,
    PeepholeOptimization,
    StackMapOptimization,
}

/// Optimization level configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptimizationLevel {
    None,       // No optimizations (-O0)
    Basic,      // Basic optimizations (-O1)
    Standard,   // Standard optimizations (-O2)
    Aggressive, // Aggressive optimizations (-O3)
    Maximum,    // Maximum optimizations (-Ofast)
}

/// Optimization pass configuration
#[derive(Debug, Clone)]
pub struct OptimizationPass {
    /// Type of optimization
    pub optimization_type: OptimizationType,
    
    /// Priority (lower values run first)
    pub priority: u8,
    
    /// Whether this pass is enabled
    pub enabled: bool,
    
    /// Minimum optimization level required
    pub min_level: OptimizationLevel,
    
    /// Maximum number of iterations
    pub max_iterations: u8,
    
    /// Whether this pass can be run in parallel
    pub parallel_safe: bool,
}

/// Statistics for optimization passes
#[derive(Debug, Default)]
pub struct OptimizationStats {
    /// Number of optimizations applied
    pub optimizations_applied: u32,
    
    /// Bytes saved by optimizations
    pub bytes_saved: u32,
    
    /// Time spent on optimizations (microseconds)
    pub time_spent_us: u64,
    
    /// Per-optimization statistics
    pub per_optimization: HashMap<OptimizationType, OptimizationPassStats>,
}

/// Statistics for individual optimization pass
#[derive(Debug, Default)]
pub struct OptimizationPassStats {
    /// Number of times this optimization was applied
    pub applications: u32,
    
    /// Bytes saved by this optimization
    pub bytes_saved: u32,
    
    /// Time spent on this optimization (microseconds)
    pub time_spent_us: u64,
    
    /// Number of iterations run
    pub iterations: u8,
}

/// Context passed to optimization passes
#[derive(Debug)]
pub struct OptimizationContext<'a> {
    /// Current method being optimized
    pub method: &'a MethodDecl,
    
    /// Class context
    pub class: &'a ClassDecl,
    
    /// All types in compilation unit
    pub all_types: &'a [TypeDecl],
    
    /// Current optimization level
    pub optimization_level: OptimizationLevel,
    
    /// Whether debug information should be preserved
    pub preserve_debug_info: bool,
    
    /// Maximum bytecode size allowed
    pub max_bytecode_size: Option<u32>,
}

/// Main optimization manager
pub struct OptimizationManager {
    /// Current optimization level
    pub optimization_level: OptimizationLevel,
    
    /// Enabled optimization passes
    pub enabled_optimizations: HashSet<OptimizationType>,
    
    /// Optimization pass configurations
    pub optimization_passes: Vec<OptimizationPass>,
    
    /// Global optimization statistics
    pub stats: OptimizationStats,
    
    /// Whether to preserve debug information
    pub preserve_debug_info: bool,
    
    /// Maximum iterations for optimization cycles
    pub max_global_iterations: u8,
    
    /// Whether to run optimizations in parallel where possible
    pub parallel_execution: bool,
    
    /// Target method size threshold for aggressive optimization
    pub method_size_threshold: u32,
    
    /// JavaC-aligned jump chain optimizer for complex jump handling
    pub javac_jump_optimizer: JavacJumpOptimizer,
    
    /// Branch optimization manager (JavaC pattern)
    pub branch_optimizer: BranchOptimizer,
}

impl OptimizationManager {
    /// Create new optimization manager with default configuration
    pub fn new() -> Self {
        let mut manager = Self {
            optimization_level: OptimizationLevel::Standard,
            enabled_optimizations: HashSet::new(),
            optimization_passes: Vec::new(),
            stats: OptimizationStats::default(),
            preserve_debug_info: true,
            max_global_iterations: 3,
            parallel_execution: false,
            method_size_threshold: 1000,
            javac_jump_optimizer: JavacJumpOptimizer::new(),
            branch_optimizer: BranchOptimizer::new(),
        };
        
        manager.initialize_default_passes();
        manager.configure_for_level(OptimizationLevel::Standard);
        manager
    }
    
    /// Create optimization manager with specific level
    pub fn with_level(level: OptimizationLevel) -> Self {
        let mut manager = Self::new();
        manager.set_optimization_level(level);
        manager
    }
    
    /// Set optimization level and reconfigure passes
    pub fn set_optimization_level(&mut self, level: OptimizationLevel) {
        self.optimization_level = level;
        self.configure_for_level(level);
    }
    
    /// Configure passes for specific optimization level
    fn configure_for_level(&mut self, level: OptimizationLevel) {
        self.enabled_optimizations.clear();
        
        match level {
            OptimizationLevel::None => {
                // No optimizations enabled
            }
            OptimizationLevel::Basic => {
                self.enabled_optimizations.extend([
                    OptimizationType::ConstantFolding,
                    OptimizationType::DeadCodeElimination,
                    OptimizationType::JumpOptimization,
                    OptimizationType::StringOptimization,
                ]);
            }
            OptimizationLevel::Standard => {
                self.enabled_optimizations.extend([
                    OptimizationType::ConstantFolding,
                    OptimizationType::DeadCodeElimination,
                    OptimizationType::LoopOptimization,
                    OptimizationType::JumpOptimization,
                    OptimizationType::BranchOptimization,
                    OptimizationType::AssignmentOptimization,
                    OptimizationType::IncrementOptimization,
                    OptimizationType::MethodInvocationOptimization,
                    OptimizationType::StringOptimization,
                    OptimizationType::StringBufferOptimization,
                    OptimizationType::TypeCoercionOptimization,
                    OptimizationType::InstructionOptimization,
                ]);
            }
            OptimizationLevel::Aggressive => {
                self.enabled_optimizations.extend([
                    OptimizationType::ConstantFolding,
                    OptimizationType::DeadCodeElimination,
                    OptimizationType::LoopOptimization,
                    OptimizationType::JumpOptimization,
                    OptimizationType::BranchOptimization,
                    OptimizationType::AssignmentOptimization,
                    OptimizationType::IncrementOptimization,
                    OptimizationType::FieldAccessOptimization,
                    OptimizationType::MethodInvocationOptimization,
                    OptimizationType::StaticCallOptimization,
                    OptimizationType::VirtualCallOptimization,
                    OptimizationType::StringOptimization,
                    OptimizationType::StringBufferOptimization,
                    OptimizationType::StringConcatenationOptimization,
                    OptimizationType::TypeCoercionOptimization,
                    OptimizationType::CastOptimization,
                    OptimizationType::ObjectOptimization,
                    OptimizationType::ArrayOptimization,
                    OptimizationType::InliningOptimization,
                    OptimizationType::InstructionOptimization,
                    OptimizationType::PeepholeOptimization,
                ]);
            }
            OptimizationLevel::Maximum => {
                // Enable all optimizations
                self.enabled_optimizations.extend([
                    OptimizationType::ConstantFolding,
                    OptimizationType::DeadCodeElimination,
                    OptimizationType::LoopOptimization,
                    OptimizationType::JumpOptimization,
                    OptimizationType::BranchOptimization,
                    OptimizationType::AssignmentOptimization,
                    OptimizationType::IncrementOptimization,
                    OptimizationType::FieldAccessOptimization,
                    OptimizationType::MethodInvocationOptimization,
                    OptimizationType::StaticCallOptimization,
                    OptimizationType::VirtualCallOptimization,
                    OptimizationType::StringOptimization,
                    OptimizationType::StringBufferOptimization,
                    OptimizationType::StringConcatenationOptimization,
                    OptimizationType::TypeCoercionOptimization,
                    OptimizationType::CastOptimization,
                    OptimizationType::PrimitiveOptimization,
                    OptimizationType::ObjectOptimization,
                    OptimizationType::ArrayOptimization,
                    OptimizationType::InstanceOfOptimization,
                    OptimizationType::InliningOptimization,
                    OptimizationType::TailCallOptimization,
                    OptimizationType::RegisterAllocation,
                    OptimizationType::ExceptionOptimization,
                    OptimizationType::FinalizerOptimization,
                    OptimizationType::InstructionOptimization,
                    OptimizationType::PeepholeOptimization,
                    OptimizationType::StackMapOptimization,
                ]);
            }
        }
        
        // Update pass enabled status based on level
        for pass in &mut self.optimization_passes {
            pass.enabled = self.enabled_optimizations.contains(&pass.optimization_type)
                && level >= pass.min_level;
        }
    }
    
    /// Initialize default optimization passes
    fn initialize_default_passes(&mut self) {
        self.optimization_passes = vec![
            // Phase 1: Basic optimizations (priority 1-10)
            OptimizationPass {
                optimization_type: OptimizationType::ConstantFolding,
                priority: 1,
                enabled: true,
                min_level: OptimizationLevel::Basic,
                max_iterations: 3,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::DeadCodeElimination,
                priority: 2,
                enabled: true,
                min_level: OptimizationLevel::Basic,
                max_iterations: 2,
                parallel_safe: false,
            },
            OptimizationPass {
                optimization_type: OptimizationType::JumpOptimization,
                priority: 3,
                enabled: true,
                min_level: OptimizationLevel::Basic,
                max_iterations: 2,
                parallel_safe: false,
            },
            
            // Phase 2: Control flow optimizations (priority 11-20)
            OptimizationPass {
                optimization_type: OptimizationType::BranchOptimization,
                priority: 11,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 2,
                parallel_safe: false,
            },
            OptimizationPass {
                optimization_type: OptimizationType::LoopOptimization,
                priority: 12,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 1,
                parallel_safe: false,
            },
            
            // Phase 3: Data flow optimizations (priority 21-30)
            OptimizationPass {
                optimization_type: OptimizationType::AssignmentOptimization,
                priority: 21,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 2,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::IncrementOptimization,
                priority: 22,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 1,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::FieldAccessOptimization,
                priority: 23,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: true,
            },
            
            // Phase 4: Method call optimizations (priority 31-40)
            OptimizationPass {
                optimization_type: OptimizationType::MethodInvocationOptimization,
                priority: 31,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 1,
                parallel_safe: false,
            },
            OptimizationPass {
                optimization_type: OptimizationType::StaticCallOptimization,
                priority: 32,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::VirtualCallOptimization,
                priority: 33,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: false,
            },
            
            // Phase 5: String optimizations (priority 41-50)
            OptimizationPass {
                optimization_type: OptimizationType::StringOptimization,
                priority: 41,
                enabled: false,
                min_level: OptimizationLevel::Basic,
                max_iterations: 1,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::StringBufferOptimization,
                priority: 42,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 1,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::StringConcatenationOptimization,
                priority: 43,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: true,
            },
            
            // Phase 6: Type optimizations (priority 51-60)
            OptimizationPass {
                optimization_type: OptimizationType::TypeCoercionOptimization,
                priority: 51,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 1,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::CastOptimization,
                priority: 52,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: true,
            },
            
            // Phase 7: Object optimizations (priority 61-70)
            OptimizationPass {
                optimization_type: OptimizationType::ObjectOptimization,
                priority: 61,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::ArrayOptimization,
                priority: 62,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: true,
            },
            
            // Phase 8: Advanced optimizations (priority 71-80)
            OptimizationPass {
                optimization_type: OptimizationType::InliningOptimization,
                priority: 71,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 1,
                parallel_safe: false,
            },
            
            // Phase 9: Instruction-level optimizations (priority 91-100)
            OptimizationPass {
                optimization_type: OptimizationType::InstructionOptimization,
                priority: 91,
                enabled: false,
                min_level: OptimizationLevel::Standard,
                max_iterations: 2,
                parallel_safe: true,
            },
            OptimizationPass {
                optimization_type: OptimizationType::PeepholeOptimization,
                priority: 92,
                enabled: false,
                min_level: OptimizationLevel::Aggressive,
                max_iterations: 3,
                parallel_safe: true,
            },
        ];
        
        // Sort passes by priority
        self.optimization_passes.sort_by_key(|pass| pass.priority);
    }
    
    /// Run optimization pipeline on bytecode
    pub fn optimize_bytecode(&mut self, bytecode: Vec<u8>, context: &OptimizationContext) -> Result<Vec<u8>> {
        let start_time = std::time::Instant::now();
        let mut current_bytecode = bytecode;
        let original_size = current_bytecode.len();
        
        // Run optimization iterations
        for _iteration in 0..self.max_global_iterations {
            let _iteration_start = current_bytecode.len();
            let mut optimizations_in_iteration = 0;
            let mut optimization_results = Vec::new();
            
            // Collect all passes to run
            let passes_to_run: Vec<_> = self.optimization_passes.iter()
                .filter(|pass| pass.enabled)
                .cloned()
                .collect();
            
            // Run all enabled passes in priority order
            for pass in passes_to_run {
                // Run the optimization pass
                let pass_result = self.run_optimization_pass(
                    &pass,
                    current_bytecode.clone(),
                    context,
                )?;
                
                if let Some(optimized_bytecode) = pass_result {
                    if optimized_bytecode.len() < current_bytecode.len() {
                        let bytes_saved = current_bytecode.len() - optimized_bytecode.len();
                        current_bytecode = optimized_bytecode;
                        optimizations_in_iteration += 1;
                        
                        // Store optimization result for statistics
                        optimization_results.push((pass.optimization_type, bytes_saved as u32));
                    }
                }
            }
            
            // Update statistics after the optimization loop
            for (optimization_type, bytes_saved) in optimization_results {
                self.update_pass_stats(&optimization_type, bytes_saved);
            }
            
            // If no optimizations were applied in this iteration, we're done
            if optimizations_in_iteration == 0 {
                break;
            }
        }
        
        // Update global statistics
        let total_time = start_time.elapsed();
        self.stats.time_spent_us += total_time.as_micros() as u64;
        self.stats.bytes_saved += (original_size - current_bytecode.len()) as u32;
        self.stats.optimizations_applied += 1;
        
        Ok(current_bytecode)
    }
    
    /// Run a specific optimization pass
    fn run_optimization_pass(
        &self,
        pass: &OptimizationPass,
        bytecode: Vec<u8>,
        context: &OptimizationContext,
    ) -> Result<Option<Vec<u8>>> {
        let start_time = std::time::Instant::now();
        
        // Dispatch to specific optimization implementation
        let result = match pass.optimization_type {
            OptimizationType::ConstantFolding => {
                self.run_constant_folding(bytecode, context)
            }
            OptimizationType::DeadCodeElimination => {
                self.run_dead_code_elimination(bytecode, context)
            }
            OptimizationType::JumpOptimization => {
                self.run_jump_optimization(bytecode, context)
            }
            OptimizationType::BranchOptimization => {
                self.run_branch_optimization(bytecode, context)
            }
            OptimizationType::StringOptimization => {
                self.run_string_optimization(bytecode, context)
            }
            OptimizationType::InstructionOptimization => {
                self.run_instruction_optimization(bytecode, context)
            }
            _ => {
                // For now, return None for unimplemented optimizations
                Ok(None)
            }
        };
        
        let elapsed = start_time.elapsed();
        
        // Log optimization timing for performance analysis
        if elapsed.as_millis() > 10 {
            eprintln!("⚠️  Optimization {:?} took {}ms", pass.optimization_type, elapsed.as_millis());
        }
        
        result
    }
    
    /// Update statistics for an optimization pass
    fn update_pass_stats(&mut self, optimization_type: &OptimizationType, bytes_saved: u32) {
        let stats = self.stats.per_optimization
            .entry(optimization_type.clone())
            .or_insert_with(OptimizationPassStats::default);
        
        stats.applications += 1;
        stats.bytes_saved += bytes_saved;
    }
    
    /// Enable a specific optimization
    pub fn enable_optimization(&mut self, optimization: OptimizationType) {
        self.enabled_optimizations.insert(optimization);
        
        // Update corresponding pass
        for pass in &mut self.optimization_passes {
            if pass.optimization_type == optimization {
                pass.enabled = true;
                break;
            }
        }
    }
    
    /// Disable a specific optimization
    pub fn disable_optimization(&mut self, optimization: OptimizationType) {
        self.enabled_optimizations.remove(&optimization);
        
        // Update corresponding pass
        for pass in &mut self.optimization_passes {
            if pass.optimization_type == optimization {
                pass.enabled = false;
                break;
            }
        }
    }
    
    /// Get optimization statistics
    pub fn get_stats(&self) -> &OptimizationStats {
        &self.stats
    }
    
    /// Reset optimization statistics
    pub fn reset_stats(&mut self) {
        self.stats = OptimizationStats::default();
    }
    
    // Placeholder optimization implementations
    // These will delegate to existing optimizer modules
    
    fn run_constant_folding(&self, _bytecode: Vec<u8>, _context: &OptimizationContext) -> Result<Option<Vec<u8>>> {
        // JavaC-style constant folding is handled during expression generation (gen_expr.rs)
        // This method is for post-generation constant optimization
        Ok(None) // Handled in generation phase
    }
    
    fn run_dead_code_elimination(&self, _bytecode: Vec<u8>, _context: &OptimizationContext) -> Result<Option<Vec<u8>>> {
        // JavaC pattern: eliminate unreachable code during generation rather than post-processing
        // This is more of a generation-time optimization in javac
        Ok(None) // Handled during generation phase
    }
    
    fn run_jump_optimization(&self, bytecode: Vec<u8>, _context: &OptimizationContext) -> Result<Option<Vec<u8>>> {
        // JavaC pattern: jump chain optimization during bytecode emission
        // Uses PendingJumpsManager for complex jump optimization
        // Note: In javac style, most jump optimization happens during generation
        // This is for post-generation jump peephole optimization
        
        if self.optimization_level >= OptimizationLevel::Standard {
            // Apply simple jump peephole optimizations
            let optimized = PeepholeOptimizer::optimize_sequence(&bytecode);
            if optimized.len() != bytecode.len() {
                return Ok(Some(optimized));
            }
        }
        
        Ok(None)
    }
    
    fn run_branch_optimization(&self, bytecode: Vec<u8>, context: &OptimizationContext) -> Result<Option<Vec<u8>>> {
        // JavaC-aligned branch optimization using BranchOptimizer
        if self.optimization_level >= OptimizationLevel::Standard {
            let branch_context = BranchOptimizationContext {
                method_name: context.method.name.clone(),
                branch_frequency: HashMap::new(), // Could be enhanced with profiling data
                target_size_limit: context.max_bytecode_size,
                preserve_debug_info: context.preserve_debug_info,
            };
            
            // Create a temporary branch optimizer for this method
            // In a full implementation, we'd use the shared one with synchronization
            let mut branch_optimizer = BranchOptimizer::new();
            let optimized = branch_optimizer.optimize_branches(bytecode, &branch_context)?;
            
            // Update global statistics (simplified for single-threaded case)
            // In multi-threaded case, we'd need proper synchronization
            
            return Ok(Some(optimized));
        }
        
        Ok(None)
    }
    
    fn run_string_optimization(&self, _bytecode: Vec<u8>, _context: &OptimizationContext) -> Result<Option<Vec<u8>>> {
        // JavaC pattern: string concatenation optimization during expression generation
        // StringBuilder optimization is handled in gen_expr.rs during generation
        // This is reserved for post-generation string optimizations
        Ok(None) // Handled in generation phase
    }
    
    fn run_instruction_optimization(&self, bytecode: Vec<u8>, _context: &OptimizationContext) -> Result<Option<Vec<u8>>> {
        // JavaC pattern: instruction selection and peephole optimizations
        // Integrates with InstructionOptimizer for post-generation optimizations
        
        if self.optimization_level >= OptimizationLevel::Basic {
            // Apply peephole optimizations
            let optimized = PeepholeOptimizer::optimize_sequence(&bytecode);
            if optimized.len() != bytecode.len() {
                return Ok(Some(optimized));
            }
        }
        
        Ok(None)
    }
    
    // JavaC-style optimization helpers for generation phase
    // These methods are called during bytecode generation rather than as post-processing
    
    /// Optimize instruction selection during generation (JavaC pattern)
    pub fn optimize_load_instruction(&self, var_type: &str, index: u16) -> Vec<u8> {
        if self.enabled_optimizations.contains(&OptimizationType::InstructionOptimization) {
            InstructionOptimizer::optimize_load_instruction(var_type, index)
        } else {
            // Fallback to basic instruction
            vec![0x15, index as u8] // Basic iload
        }
    }
    
    /// Optimize store instruction during generation (JavaC pattern)  
    pub fn optimize_store_instruction(&self, var_type: &str, index: u16) -> Vec<u8> {
        if self.enabled_optimizations.contains(&OptimizationType::InstructionOptimization) {
            InstructionOptimizer::optimize_store_instruction(var_type, index)
        } else {
            // Fallback to basic instruction
            vec![0x36, index as u8] // Basic istore
        }
    }
    
    /// Optimize constant loading during generation (JavaC pattern)
    pub fn optimize_constant_loading(&self, value: i32) -> ConstantInstruction {
        if self.enabled_optimizations.contains(&OptimizationType::ConstantFolding) {
            ConstantOptimizer::optimize_int(value)
        } else {
            // Fallback to LDC
            ConstantInstruction::Ldc(0)
        }
    }
    
    /// Optimize binary operation during generation (JavaC pattern)
    pub fn optimize_binary_operation(&self, op: &BinaryOp, operand_type: &str) -> Vec<u8> {
        if self.enabled_optimizations.contains(&OptimizationType::InstructionOptimization) {
            InstructionOptimizer::optimize_binary_operation(op, operand_type)
        } else {
            // Fallback to basic instruction
            vec![0x60] // Basic iadd
        }
    }
    
    /// Optimize increment instruction during generation (JavaC pattern)
    pub fn optimize_increment_instruction(&self, index: u16, increment: i16) -> Vec<u8> {
        if self.enabled_optimizations.contains(&OptimizationType::IncrementOptimization) {
            InstructionOptimizer::optimize_increment_instruction(index, increment)
        } else {
            // Fallback to load-add-store sequence
            let mut bytecode = vec![0x15, index as u8]; // iload
            bytecode.push(0x10); // bipush
            bytecode.push(increment as u8);
            bytecode.push(0x60); // iadd
            bytecode.push(0x36); // istore
            bytecode.push(index as u8);
            bytecode
        }
    }
    
    /// Optimize array operations during generation (JavaC pattern)
    pub fn optimize_array_load(&self, element_type: &str) -> Vec<u8> {
        if self.enabled_optimizations.contains(&OptimizationType::ArrayOptimization) {
            InstructionOptimizer::optimize_array_load(element_type)
        } else {
            // Fallback to basic instruction
            vec![0x2E] // Basic iaload
        }
    }
    
    /// Optimize type conversion during generation (JavaC pattern)
    pub fn optimize_type_conversion(&self, from_type: &str, to_type: &str) -> Vec<u8> {
        if self.enabled_optimizations.contains(&OptimizationType::TypeCoercionOptimization) {
            InstructionOptimizer::optimize_type_conversion(from_type, to_type)
        } else {
            // No conversion optimization
            vec![]
        }
    }
    
    /// Optimize return instruction during generation (JavaC pattern)
    pub fn optimize_return_instruction(&self, return_type: &str) -> Vec<u8> {
        if self.enabled_optimizations.contains(&OptimizationType::InstructionOptimization) {
            InstructionOptimizer::optimize_return_instruction(return_type)
        } else {
            // Fallback to basic return
            vec![0xB1] // Basic return
        }
    }
    
    /// Check if an optimization is enabled and at the required level
    pub fn is_optimization_enabled(&self, optimization: OptimizationType, min_level: OptimizationLevel) -> bool {
        self.optimization_level >= min_level && 
        self.enabled_optimizations.contains(&optimization)
    }
    
    /// Get the JavaC-aligned jump optimizer for complex jump handling
    pub fn get_javac_jump_optimizer(&mut self) -> &mut JavacJumpOptimizer {
        &mut self.javac_jump_optimizer
    }
    
    /// Reset the jump optimizer for a new method (JavaC pattern)
    pub fn reset_jump_optimizer(&mut self) {
        self.javac_jump_optimizer.reset();
    }
    
    /// Apply jump optimizations and resolve pending jumps (JavaC pattern)
    pub fn optimize_and_resolve_jumps(&mut self, target_pc: u32) -> Result<()> {
        // JavaC-style jump resolution - all pending jumps resolved to target
        self.javac_jump_optimizer.resolve_pending(target_pc)
    }
}

impl Default for OptimizationManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Optimization configuration builder
pub struct OptimizationConfig {
    level: OptimizationLevel,
    preserve_debug_info: bool,
    parallel_execution: bool,
    method_size_threshold: u32,
    custom_optimizations: HashSet<OptimizationType>,
}

impl OptimizationConfig {
    /// Create new optimization configuration
    pub fn new() -> Self {
        Self {
            level: OptimizationLevel::Standard,
            preserve_debug_info: true,
            parallel_execution: false,
            method_size_threshold: 1000,
            custom_optimizations: HashSet::new(),
        }
    }
    
    /// Set optimization level
    pub fn with_level(mut self, level: OptimizationLevel) -> Self {
        self.level = level;
        self
    }
    
    /// Set debug info preservation
    pub fn preserve_debug_info(mut self, preserve: bool) -> Self {
        self.preserve_debug_info = preserve;
        self
    }
    
    /// Enable parallel execution
    pub fn parallel_execution(mut self, enabled: bool) -> Self {
        self.parallel_execution = enabled;
        self
    }
    
    /// Set method size threshold for aggressive optimization
    pub fn method_size_threshold(mut self, threshold: u32) -> Self {
        self.method_size_threshold = threshold;
        self
    }
    
    /// Add custom optimization
    pub fn add_optimization(mut self, optimization: OptimizationType) -> Self {
        self.custom_optimizations.insert(optimization);
        self
    }
    
    /// Build optimization manager from configuration
    pub fn build(self) -> OptimizationManager {
        let mut manager = OptimizationManager::with_level(self.level);
        manager.preserve_debug_info = self.preserve_debug_info;
        manager.parallel_execution = self.parallel_execution;
        manager.method_size_threshold = self.method_size_threshold;
        
        // Enable custom optimizations
        for optimization in self.custom_optimizations {
            manager.enable_optimization(optimization);
        }
        
        manager
    }
}

impl Default for OptimizationConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_optimization_manager_creation() {
        let manager = OptimizationManager::new();
        assert_eq!(manager.optimization_level, OptimizationLevel::Standard);
        assert!(!manager.optimization_passes.is_empty());
    }
    
    #[test]
    fn test_optimization_level_configuration() {
        let mut manager = OptimizationManager::new();
        
        manager.set_optimization_level(OptimizationLevel::Aggressive);
        assert_eq!(manager.optimization_level, OptimizationLevel::Aggressive);
        assert!(manager.enabled_optimizations.len() > 10);
        
        manager.set_optimization_level(OptimizationLevel::None);
        assert_eq!(manager.optimization_level, OptimizationLevel::None);
        assert!(manager.enabled_optimizations.is_empty());
    }
    
    #[test]
    fn test_optimization_enable_disable() {
        let mut manager = OptimizationManager::new();
        
        manager.enable_optimization(OptimizationType::ConstantFolding);
        assert!(manager.enabled_optimizations.contains(&OptimizationType::ConstantFolding));
        
        manager.disable_optimization(OptimizationType::ConstantFolding);
        assert!(!manager.enabled_optimizations.contains(&OptimizationType::ConstantFolding));
    }
    
    #[test]
    fn test_optimization_config_builder() {
        let config = OptimizationConfig::new()
            .with_level(OptimizationLevel::Maximum)
            .preserve_debug_info(false)
            .parallel_execution(true)
            .add_optimization(OptimizationType::TailCallOptimization);
        
        let manager = config.build();
        assert_eq!(manager.optimization_level, OptimizationLevel::Maximum);
        assert!(!manager.preserve_debug_info);
        assert!(manager.parallel_execution);
        assert!(manager.enabled_optimizations.contains(&OptimizationType::TailCallOptimization));
    }
}