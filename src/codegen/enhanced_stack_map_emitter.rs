//! Enhanced Stack Map Frame Emission
//! 
//! This module implements advanced emitStackMapFrame functionality, providing
//! intelligent stack map frame generation with compression and optimization.
//! Based on industry-standard JVM bytecode generation techniques.

use crate::codegen::frame::{StackMapFrame, VerificationType};
use crate::codegen::stack_map_optimizer::{StackMapOptimizer, StackMapOptimization};

/// Enhanced Stack Map Frame Emitter
/// 
/// This implements advanced stack map frame emission logic,
/// providing intelligent frame generation with compression and optimization.
#[derive(Debug)]
pub struct EnhancedStackMapEmitter {
    /// Previous frame for delta calculation
    last_frame: Option<StackMapFrameInfo>,
    /// Frame before last for rollback
    frame_before_last: Option<StackMapFrameInfo>,
    /// Stack map optimizer for compression
    optimizer: StackMapOptimizer,
    /// Current method information
    method_info: MethodInfo,
    /// Generated frames buffer
    frames_buffer: Vec<StackMapFrame>,
    /// Frame generation statistics
    stats: EmissionStats,
}

/// Stack map frame information
#[derive(Debug, Clone)]
pub struct StackMapFrameInfo {
    /// Program counter
    pub pc: u16,
    /// Local variable types
    pub locals: Vec<VerificationType>,
    /// Stack types
    pub stack: Vec<VerificationType>,
}

/// Method information for frame generation
#[derive(Debug, Clone)]
pub struct MethodInfo {
    /// Is method static
    pub is_static: bool,
    /// Is constructor
    pub is_constructor: bool,
    /// Owner class type
    pub owner_type: String,
    /// Method parameter types
    pub parameter_types: Vec<String>,
    /// Maximum locals
    pub max_locals: u16,
}

/// Frame emission statistics
#[derive(Debug, Default)]
pub struct EmissionStats {
    pub frames_emitted: usize,
    pub frames_compressed: usize,
    pub frames_dropped: usize,
    pub initial_frames: usize,
    pub delta_frames: usize,
}

impl EnhancedStackMapEmitter {
    /// Create new enhanced stack map emitter
    pub fn new(method_info: MethodInfo) -> Self {
        Self {
            last_frame: None,
            frame_before_last: None,
            optimizer: StackMapOptimizer::new(),
            method_info,
            frames_buffer: Vec::new(),
            stats: EmissionStats::default(),
        }
    }

    /// Emit stack map frame (enhanced emitStackMapFrame implementation)
    pub fn emit_stack_map_frame(
        &mut self,
        pc: u16,
        locals: Vec<VerificationType>,
        stack: Vec<VerificationType>,
    ) -> Result<(), StackMapError> {
        // Handle first frame case
        if self.last_frame.is_none() {
            let initial_frame = self.get_initial_frame()?;
            self.last_frame = Some(initial_frame);
            self.stats.initial_frames += 1;
        }

        // Check for duplicate frame at same PC (drop existing frame)
        if let Some(ref last) = self.last_frame {
            if last.pc == pc {
                // Drop existing stackmap at this offset
                if !self.frames_buffer.is_empty() {
                    self.frames_buffer.pop();
                    self.stats.frames_dropped += 1;
                }
                self.last_frame = self.frame_before_last.clone();
                self.frame_before_last = None;
            }
        }

        // Create new frame
        let frame = StackMapFrameInfo {
            pc,
            locals: self.process_locals(locals)?,
            stack: self.process_stack(stack)?,
        };

        // Generate optimized frame using compression
        let optimization = self.optimizer.analyze_frame_optimization(
            frame.locals.clone(),
            frame.stack.clone(),
            pc,
        );

        let compressed_frame = self.optimizer.generate_optimized_frame(&optimization);
        self.frames_buffer.push(compressed_frame);

        // Update frame tracking
        self.frame_before_last = self.last_frame.clone();
        self.last_frame = Some(frame);

        self.stats.frames_emitted += 1;
        if matches!(optimization, StackMapOptimization::FullFrame { .. }) {
            // Full frame
        } else {
            self.stats.frames_compressed += 1;
        }

        Ok(())
    }

    /// Get initial frame (enhanced getInitialFrame implementation)
    fn get_initial_frame(&self) -> Result<StackMapFrameInfo, StackMapError> {
        let mut locals = Vec::new();
        // Add 'this' parameter for non-static methods
        if !self.method_info.is_static {
            let this_type = if self.method_info.is_constructor && self.method_info.owner_type != "java/lang/Object" {
                // Uninitialized this for constructors (except Object)
                VerificationType::UninitializedThis
            } else {
                // Regular object reference
                VerificationType::Object(1) // Placeholder constant pool index
            };
            locals.push(this_type);
        }

        // Add method parameters
        for param_type in &self.method_info.parameter_types {
            let verification_type = self.type_to_verification_type(param_type)?;
            locals.push(verification_type);
        }

        Ok(StackMapFrameInfo {
            pc: 0, // Initial frame uses PC 0
            locals,
            stack: Vec::new(), // Initial frame has empty stack
        })
    }

    /// Process locals array (enhanced local processing logic)
    fn process_locals(&self, locals: Vec<VerificationType>) -> Result<Vec<VerificationType>, StackMapError> {
        let mut processed = Vec::new();
        
        for (i, local_type) in locals.iter().enumerate() {
            if i < self.method_info.max_locals as usize {
                // Apply type erasure
                let erased_type = self.apply_type_erasure(local_type.clone());
                processed.push(erased_type);
            }
        }

        Ok(processed)
    }

    /// Process stack array (enhanced stack processing logic)
    fn process_stack(&self, stack: Vec<VerificationType>) -> Result<Vec<VerificationType>, StackMapError> {
        let mut processed = Vec::new();
        
        for stack_item in stack {
            // Apply type erasure to stack items
            let erased_type = self.apply_type_erasure(stack_item);
            processed.push(erased_type);
        }

        Ok(processed)
    }

    /// Apply type erasure (enhanced types.erasure equivalent)
    fn apply_type_erasure(&self, verification_type: VerificationType) -> VerificationType {
        match verification_type {
            // Primitive types are not erased
            VerificationType::Integer | 
            VerificationType::Float | 
            VerificationType::Long | 
            VerificationType::Double |
            VerificationType::Null |
            VerificationType::Top => verification_type,
            
            // Object types might need erasure for generics
            VerificationType::Object(index) => {
                // In a full implementation, this would erase generic type information
                VerificationType::Object(index)
            }
            
            // Uninitialized types are preserved
            VerificationType::Uninitialized(_offset) => verification_type,
            VerificationType::UninitializedThis => verification_type,
        }
    }

    /// Convert type string to verification type
    fn type_to_verification_type(&self, type_str: &str) -> Result<VerificationType, StackMapError> {
        match type_str {
            "int" | "boolean" | "byte" | "char" | "short" => Ok(VerificationType::Integer),
            "float" => Ok(VerificationType::Float),
            "long" => Ok(VerificationType::Long),
            "double" => Ok(VerificationType::Double),
            _ => {
                // Object type - in a full implementation, this would resolve to constant pool index
                Ok(VerificationType::Object(1)) // Placeholder
            }
        }
    }

    /// Check if type is wide (takes two local variable slots)
    fn is_wide_type(&self, verification_type: &VerificationType) -> bool {
        matches!(verification_type, VerificationType::Long | VerificationType::Double)
    }

    /// Get generated frames
    pub fn get_frames(&self) -> &[StackMapFrame] {
        &self.frames_buffer
    }

    /// Get emission statistics
    pub fn get_stats(&self) -> &EmissionStats {
        &self.stats
    }

    /// Reset emitter for new method
    pub fn reset(&mut self, method_info: MethodInfo) {
        self.last_frame = None;
        self.frame_before_last = None;
        self.optimizer.reset();
        self.method_info = method_info;
        self.frames_buffer.clear();
        self.stats = EmissionStats::default();
    }

    /// Check if frame emission is needed (enhanced needStackMap logic)
    pub fn needs_frame_emission(&self, _pc: u16, is_jump_target: bool, is_exception_handler: bool) -> bool {
        // Enhanced emitter emits frames at:
        // 1. Jump targets (branch destinations)
        // 2. Exception handler entry points
        // 3. After instructions that significantly change stack state
        
        is_jump_target || is_exception_handler
    }

    /// Emit frame at jump target (enhanced common pattern)
    pub fn emit_frame_at_jump_target(
        &mut self,
        pc: u16,
        locals: Vec<VerificationType>,
        stack: Vec<VerificationType>,
    ) -> Result<(), StackMapError> {
        if self.needs_frame_emission(pc, true, false) {
            self.emit_stack_map_frame(pc, locals, stack)?;
        }
        Ok(())
    }

    /// Emit frame at exception handler (enhanced exception handling)
    pub fn emit_frame_at_exception_handler(
        &mut self,
        pc: u16,
        locals: Vec<VerificationType>,
        exception_type: VerificationType,
    ) -> Result<(), StackMapError> {
        if self.needs_frame_emission(pc, false, true) {
            let stack = vec![exception_type]; // Exception on stack
            self.emit_stack_map_frame(pc, locals, stack)?;
        }
        Ok(())
    }
}

/// Stack map frame emission errors
#[derive(Debug, thiserror::Error)]
pub enum StackMapError {
    #[error("Invalid frame state: {message}")]
    InvalidFrameState { message: String },
    
    #[error("Type resolution error: {type_name}")]
    TypeResolutionError { type_name: String },
    
    #[error("Frame compression error: {message}")]
    CompressionError { message: String },
    
    #[error("Method info error: {message}")]
    MethodInfoError { message: String },
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_method_info() -> MethodInfo {
        MethodInfo {
            is_static: false,
            is_constructor: false,
            owner_type: "test/TestClass".to_string(),
            parameter_types: vec!["int".to_string(), "java/lang/String".to_string()],
            max_locals: 10,
        }
    }

    #[test]
    fn test_enhanced_stack_map_emitter_creation() {
        let method_info = create_test_method_info();
        let emitter = EnhancedStackMapEmitter::new(method_info);
        
        assert_eq!(emitter.frames_buffer.len(), 0);
        assert!(emitter.last_frame.is_none());
    }

    #[test]
    fn test_initial_frame_generation() {
        let method_info = create_test_method_info();
        let emitter = EnhancedStackMapEmitter::new(method_info);
        
        let initial_frame = emitter.get_initial_frame().unwrap();
        
        // Should have 'this' + 2 parameters = 3 locals
        assert_eq!(initial_frame.locals.len(), 3);
        assert_eq!(initial_frame.stack.len(), 0);
        assert_eq!(initial_frame.pc, 0);
    }

    #[test]
    fn test_static_method_initial_frame() {
        let mut method_info = create_test_method_info();
        method_info.is_static = true;
        
        let emitter = EnhancedStackMapEmitter::new(method_info);
        let initial_frame = emitter.get_initial_frame().unwrap();
        
        // Should have only 2 parameters (no 'this')
        assert_eq!(initial_frame.locals.len(), 2);
    }

    #[test]
    fn test_constructor_initial_frame() {
        let mut method_info = create_test_method_info();
        method_info.is_constructor = true;
        
        let emitter = EnhancedStackMapEmitter::new(method_info);
        let initial_frame = emitter.get_initial_frame().unwrap();
        
        // Should have UninitializedThis for constructors
        assert!(matches!(initial_frame.locals[0], VerificationType::UninitializedThis));
    }

    #[test]
    fn test_frame_emission() {
        let method_info = create_test_method_info();
        let mut emitter = EnhancedStackMapEmitter::new(method_info);
        
        let locals = vec![VerificationType::Object(1), VerificationType::Integer];
        let stack = vec![];
        
        emitter.emit_stack_map_frame(10, locals, stack).unwrap();
        
        assert_eq!(emitter.get_frames().len(), 1);
        assert_eq!(emitter.get_stats().frames_emitted, 1);
    }

    #[test]
    fn test_duplicate_frame_handling() {
        let method_info = create_test_method_info();
        let mut emitter = EnhancedStackMapEmitter::new(method_info);
        
        let locals = vec![VerificationType::Object(1)];
        let stack = vec![];
        
        // Emit frame at PC 10
        emitter.emit_stack_map_frame(10, locals.clone(), stack.clone()).unwrap();
        assert_eq!(emitter.get_frames().len(), 1);
        
        // Emit another frame at same PC (should drop previous)
        emitter.emit_stack_map_frame(10, locals, stack).unwrap();
        assert_eq!(emitter.get_frames().len(), 1);
        assert_eq!(emitter.get_stats().frames_dropped, 1);
    }

    #[test]
    fn test_jump_target_emission() {
        let method_info = create_test_method_info();
        let mut emitter = EnhancedStackMapEmitter::new(method_info);
        
        let locals = vec![VerificationType::Object(1)];
        let stack = vec![];
        
        emitter.emit_frame_at_jump_target(20, locals, stack).unwrap();
        
        assert_eq!(emitter.get_frames().len(), 1);
    }

    #[test]
    fn test_exception_handler_emission() {
        let method_info = create_test_method_info();
        let mut emitter = EnhancedStackMapEmitter::new(method_info);
        
        let locals = vec![VerificationType::Object(1)];
        let exception_type = VerificationType::Object(2); // Exception class
        
        emitter.emit_frame_at_exception_handler(30, locals, exception_type).unwrap();
        
        assert_eq!(emitter.get_frames().len(), 1);
    }

    #[test]
    fn test_type_erasure() {
        let method_info = create_test_method_info();
        let emitter = EnhancedStackMapEmitter::new(method_info);
        
        // Test primitive types (not erased)
        let int_type = VerificationType::Integer;
        let erased = emitter.apply_type_erasure(int_type.clone());
        assert_eq!(erased, int_type);
        
        // Test object types (potentially erased)
        let obj_type = VerificationType::Object(1);
        let erased = emitter.apply_type_erasure(obj_type.clone());
        assert_eq!(erased, obj_type);
    }

    #[test]
    fn test_wide_type_detection() {
        let method_info = create_test_method_info();
        let emitter = EnhancedStackMapEmitter::new(method_info);
        
        assert!(emitter.is_wide_type(&VerificationType::Long));
        assert!(emitter.is_wide_type(&VerificationType::Double));
        assert!(!emitter.is_wide_type(&VerificationType::Integer));
        assert!(!emitter.is_wide_type(&VerificationType::Object(1)));
    }

    #[test]
    fn test_emitter_reset() {
        let method_info = create_test_method_info();
        let mut emitter = EnhancedStackMapEmitter::new(method_info.clone());
        
        // Emit some frames
        let locals = vec![VerificationType::Object(1)];
        let stack = vec![];
        emitter.emit_stack_map_frame(10, locals, stack).unwrap();
        
        assert_eq!(emitter.get_frames().len(), 1);
        
        // Reset
        emitter.reset(method_info);
        
        assert_eq!(emitter.get_frames().len(), 0);
        assert!(emitter.last_frame.is_none());
    }
}
