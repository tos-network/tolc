//! Method-level context management for bytecode generation
//! 
//! This module corresponds to javac's method-level state management within Gen.java.
//! It handles local variable allocation, stack depth tracking, and method-specific
//! generation context that persists throughout method compilation.

use crate::ast::*;
use crate::error::Result;
use std::collections::HashMap;

/// Method-level generation context
/// Corresponds to method-level state in javac's Gen class
#[derive(Debug, Clone)]
pub struct MethodContext {
    /// Current method being compiled
    pub method: Option<MethodDecl>,
    
    /// Local variable mappings (name -> LocalVar)
    pub locals: HashMap<String, LocalVar>,
    
    /// Next available local variable slot
    pub next_local: u16,
    
    /// Current stack depth
    pub stack_depth: u16,
    
    /// Maximum stack depth seen so far
    pub max_stack: u16,
    
    /// Stack type tracking for verification
    pub stack_types: Vec<String>,
    
    /// Method parameters (for proper local slot allocation)
    pub parameters: Vec<Parameter>,
    
    /// Return type of current method
    pub return_type: Option<TypeRef>,
    
    /// Whether current method is static
    pub is_static: bool,
    
    /// Exception table entries for this method
    pub exception_handlers: Vec<ExceptionHandler>,
    
    /// Local variable table for debugging info
    pub local_var_table: Vec<LocalVarEntry>,
    
    /// Line number table for debugging info
    pub line_number_table: Vec<LineNumberEntry>,
    
    /// Try-catch block stack for nested exception handling
    pub try_catch_stack: Vec<TryCatchFrame>,
    
    /// Labels created in this method
    pub labels: HashMap<String, u16>,
    
    /// Next label number for unique label generation
    pub next_label: u16,
    
    /// Current source line for line number tracking
    pub current_line: u16,
    
    /// Method signature for invocation
    pub method_signature: Option<String>,
}

/// Local variable information
#[derive(Debug, Clone)]
pub struct LocalVar {
    /// Local variable slot index
    pub slot: u16,
    
    /// Variable type descriptor
    pub var_type: String,
    
    /// Variable name
    pub name: String,
    
    /// Start PC where variable becomes active
    pub start_pc: u16,
    
    /// Length of variable scope
    pub length: u16,
    
    /// Whether variable is a parameter
    pub is_parameter: bool,
    
    /// Whether variable is final
    pub is_final: bool,
}

/// Exception handler entry
#[derive(Debug, Clone)]
pub struct ExceptionHandler {
    /// Start PC of try block
    pub start_pc: u16,
    
    /// End PC of try block
    pub end_pc: u16,
    
    /// Handler PC (catch block start)
    pub handler_pc: u16,
    
    /// Exception type (constant pool index) or 0 for finally
    pub catch_type: u16,
}

/// Local variable table entry for debugging
#[derive(Debug, Clone)]
pub struct LocalVarEntry {
    /// Start PC where variable is visible
    pub start_pc: u16,
    
    /// Length of variable visibility
    pub length: u16,
    
    /// Name constant pool index
    pub name_index: u16,
    
    /// Descriptor constant pool index
    pub descriptor_index: u16,
    
    /// Local variable slot
    pub index: u16,
}

/// Line number table entry for debugging
#[derive(Debug, Clone)]
pub struct LineNumberEntry {
    /// Start PC for this line
    pub start_pc: u16,
    
    /// Source line number
    pub line_number: u16,
}

/// Try-catch frame for nested exception handling
#[derive(Debug, Clone)]
pub struct TryCatchFrame {
    /// Start PC of try block
    pub try_start: u16,
    
    /// End PC of try block
    pub try_end: u16,
    
    /// Catch handlers for this try block
    pub catch_handlers: Vec<CatchHandler>,
    
    /// Finally handler PC (optional)
    pub finally_handler: Option<u16>,
    
    /// Stack depth when entering try block
    pub entry_stack_depth: u16,
}

/// Individual catch handler within a try-catch frame
#[derive(Debug, Clone)]
pub struct CatchHandler {
    /// Exception type to catch
    pub exception_type: String,
    
    /// Handler PC (catch block start)
    pub handler_pc: u16,
    
    /// Variable name for caught exception
    pub exception_var: Option<String>,
}

impl MethodContext {
    /// Create new method context
    pub fn new() -> Self {
        Self {
            method: None,
            locals: HashMap::new(),
            next_local: 0,
            stack_depth: 0,
            max_stack: 0,
            stack_types: Vec::new(),
            parameters: Vec::new(),
            return_type: None,
            is_static: false,
            exception_handlers: Vec::new(),
            local_var_table: Vec::new(),
            line_number_table: Vec::new(),
            try_catch_stack: Vec::new(),
            labels: HashMap::new(),
            next_label: 0,
            current_line: 1,
            method_signature: None,
        }
    }
    
    /// Initialize context for method compilation
    pub fn init_method(&mut self, method: MethodDecl) -> Result<()> {
        self.method = Some(method.clone());
        self.locals.clear();
        self.stack_types.clear();
        self.exception_handlers.clear();
        self.local_var_table.clear();
        self.line_number_table.clear();
        self.try_catch_stack.clear();
        self.labels.clear();
        
        // Check if method is static
        self.is_static = method.modifiers.iter().any(|m| matches!(m, Modifier::Static));
        
        // Set return type
        self.return_type = method.return_type.clone();
        
        // Set parameters
        self.parameters = method.parameters.clone();
        
        // Allocate 'this' reference if non-static
        self.next_local = if self.is_static { 0 } else { 1 };
        
        // Allocate parameter slots
        for (_i, param) in method.parameters.iter().enumerate() {
            let slot = self.next_local;
            let type_descriptor = self.type_ref_to_descriptor(&param.type_ref);
            
            let local_var = LocalVar {
                slot,
                var_type: type_descriptor.clone(),
                name: param.name.clone(),
                start_pc: 0,
                length: 0, // Will be set when method compilation completes
                is_parameter: true,
                is_final: param.modifiers.iter().any(|m| matches!(m, Modifier::Final)),
            };
            
            self.locals.insert(param.name.clone(), local_var);
            
            // Advance slot (double for long/double)
            self.next_local += if type_descriptor == "J" || type_descriptor == "D" { 2 } else { 1 };
        }
        
        // Reset stack tracking
        self.stack_depth = 0;
        self.max_stack = 0;
        self.next_label = 0;
        self.current_line = 1;
        
        // Generate method signature
        self.method_signature = Some(self.generate_method_signature(&method));
        
        Ok(())
    }
    
    /// Allocate local variable slot
    pub fn allocate_local(&mut self, name: String, var_type: TypeRef, is_final: bool) -> Result<u16> {
        let type_descriptor = self.type_ref_to_descriptor(&var_type);
        let slot = self.next_local;
        
        let local_var = LocalVar {
            slot,
            var_type: type_descriptor.clone(),
            name: name.clone(),
            start_pc: 0, // Will be set when variable is first assigned
            length: 0,   // Will be set when variable goes out of scope
            is_parameter: false,
            is_final,
        };
        
        self.locals.insert(name, local_var);
        
        // Advance slot (double for long/double)
        self.next_local += if type_descriptor == "J" || type_descriptor == "D" { 2 } else { 1 };
        
        Ok(slot)
    }
    
    /// Get local variable by name
    pub fn get_local(&self, name: &str) -> Option<&LocalVar> {
        self.locals.get(name)
    }
    
    /// Update stack depth tracking
    pub fn update_stack_depth(&mut self, delta: i16) {
        if delta > 0 {
            self.stack_depth += delta as u16;
            if self.stack_depth > self.max_stack {
                self.max_stack = self.stack_depth;
            }
        } else {
            self.stack_depth = self.stack_depth.saturating_sub((-delta) as u16);
        }
    }
    
    /// Push type onto stack for verification
    pub fn push_stack_type(&mut self, type_desc: String) {
        self.stack_types.push(type_desc);
        self.update_stack_depth(1);
    }
    
    /// Pop type from stack for verification
    pub fn pop_stack_type(&mut self) -> Option<String> {
        self.update_stack_depth(-1);
        self.stack_types.pop()
    }
    
    /// Peek at top stack type without popping
    pub fn peek_stack_type(&self) -> Option<&String> {
        self.stack_types.last()
    }
    
    /// Create unique label
    pub fn create_label(&mut self) -> u16 {
        let label = self.next_label;
        self.next_label += 1;
        label
    }
    
    /// Register label at current PC
    pub fn register_label(&mut self, label: String, pc: u16) {
        self.labels.insert(label, pc);
    }
    
    /// Get PC for label
    pub fn get_label_pc(&self, label: &str) -> Option<u16> {
        self.labels.get(label).copied()
    }
    
    /// Add exception handler
    pub fn add_exception_handler(&mut self, handler: ExceptionHandler) {
        self.exception_handlers.push(handler);
    }
    
    /// Enter try-catch block
    pub fn enter_try_catch(&mut self, frame: TryCatchFrame) {
        self.try_catch_stack.push(frame);
    }
    
    /// Exit try-catch block
    pub fn exit_try_catch(&mut self) -> Option<TryCatchFrame> {
        self.try_catch_stack.pop()
    }
    
    /// Get current try-catch frame
    pub fn current_try_catch(&self) -> Option<&TryCatchFrame> {
        self.try_catch_stack.last()
    }
    
    /// Add line number entry
    pub fn add_line_number(&mut self, pc: u16, line: u16) {
        self.line_number_table.push(LineNumberEntry {
            start_pc: pc,
            line_number: line,
        });
        self.current_line = line;
    }
    
    /// Add local variable table entry
    pub fn add_local_var_entry(&mut self, entry: LocalVarEntry) {
        self.local_var_table.push(entry);
    }
    
    /// Finalize local variable scope
    pub fn finalize_local_scope(&mut self, var_name: &str, end_pc: u16) {
        if let Some(local_var) = self.locals.get_mut(var_name) {
            if local_var.length == 0 {
                local_var.length = end_pc - local_var.start_pc;
            }
        }
    }
    
    /// Finalize all local variable scopes
    pub fn finalize_all_local_scopes(&mut self, method_end_pc: u16) {
        for local_var in self.locals.values_mut() {
            if local_var.length == 0 {
                local_var.length = method_end_pc - local_var.start_pc;
            }
        }
    }
    
    /// Generate method signature string
    fn generate_method_signature(&self, method: &MethodDecl) -> String {
        let mut sig = String::from("(");
        
        for param in &method.parameters {
            sig.push_str(&self.type_ref_to_descriptor(&param.type_ref));
        }
        
        sig.push(')');
        
        if let Some(ref return_type) = method.return_type {
            sig.push_str(&self.type_ref_to_descriptor(return_type));
        } else {
            sig.push('V'); // void
        }
        
        sig
    }
    
    /// Convert TypeRef to JVM type descriptor
    fn type_ref_to_descriptor(&self, type_ref: &TypeRef) -> String {
        let mut desc = String::new();
        
        // Add array dimensions
        for _ in 0..type_ref.array_dims {
            desc.push('[');
        }
        
        // Add base type
        match type_ref.name.as_str() {
            "boolean" => desc.push('Z'),
            "byte" => desc.push('B'),
            "char" => desc.push('C'),
            "short" => desc.push('S'),
            "int" => desc.push('I'),
            "long" => desc.push('J'),
            "float" => desc.push('F'),
            "double" => desc.push('D'),
            "void" => desc.push('V'),
            class_name => {
                desc.push('L');
                desc.push_str(&class_name.replace('.', "/"));
                desc.push(';');
            }
        }
        
        desc
    }
    
    /// Check if type is wide (long or double)
    pub fn is_wide_type(&self, type_desc: &str) -> bool {
        type_desc == "J" || type_desc == "D"
    }
    
    /// Get slot count for type
    pub fn get_type_slot_count(&self, type_desc: &str) -> u16 {
        if self.is_wide_type(type_desc) { 2 } else { 1 }
    }
    
    /// Validate stack state for operation
    pub fn validate_stack_for_operation(&self, required_types: &[&str]) -> Result<()> {
        if self.stack_types.len() < required_types.len() {
            return Err(crate::error::Error::CodeGen {
                message: format!("Stack underflow: need {} types, have {}", 
                                required_types.len(), self.stack_types.len()),
            });
        }
        
        // Check types in reverse order (top of stack first)
        for (i, &required_type) in required_types.iter().rev().enumerate() {
            let stack_index = self.stack_types.len() - 1 - i;
            let actual_type = &self.stack_types[stack_index];
            
            if !self.types_compatible(actual_type, required_type) {
                return Err(crate::error::Error::CodeGen {
                    message: format!("Type mismatch: expected {}, found {}", 
                                    required_type, actual_type),
                });
            }
        }
        
        Ok(())
    }
    
    /// Check if types are compatible for assignment/operation
    fn types_compatible(&self, actual: &str, expected: &str) -> bool {
        if actual == expected {
            return true;
        }
        
        // Handle primitive widening conversions
        match (actual, expected) {
            ("B", "S") | ("B", "I") | ("B", "J") | ("B", "F") | ("B", "D") => true,
            ("S", "I") | ("S", "J") | ("S", "F") | ("S", "D") => true,
            ("C", "I") | ("C", "J") | ("C", "F") | ("C", "D") => true,
            ("I", "J") | ("I", "F") | ("I", "D") => true,
            ("J", "F") | ("J", "D") => true,
            ("F", "D") => true,
            _ => false,
        }
    }
    
    /// Get method access flags
    pub fn get_method_access_flags(&self) -> u16 {
        if let Some(ref method) = self.method {
            let mut flags = 0u16;
            
            for modifier in &method.modifiers {
                match modifier {
                    Modifier::Public => flags |= 0x0001,
                    Modifier::Private => flags |= 0x0002,
                    Modifier::Protected => flags |= 0x0004,
                    Modifier::Static => flags |= 0x0008,
                    Modifier::Final => flags |= 0x0010,
                    Modifier::Synchronized => flags |= 0x0020,
                    Modifier::Native => flags |= 0x0100,
                    Modifier::Abstract => flags |= 0x0400,
                    Modifier::Strictfp => flags |= 0x0800,
                    _ => {}
                }
            }
            
            flags
        } else {
            0
        }
    }
    
    /// Check if method returns void
    pub fn is_void_method(&self) -> bool {
        self.return_type.is_none()
    }
    
    /// Check if current method is constructor
    pub fn is_constructor(&self) -> bool {
        if let Some(ref method) = self.method {
            method.name == "<init>"
        } else {
            false
        }
    }
    
    /// Check if current method is static initializer
    pub fn is_static_initializer(&self) -> bool {
        if let Some(ref method) = self.method {
            method.name == "<clinit>"
        } else {
            false
        }
    }
    
    /// Get parameter count (including 'this' for non-static methods)
    pub fn get_parameter_count(&self) -> usize {
        let base_count = self.parameters.len();
        if self.is_static { base_count } else { base_count + 1 }
    }
    
    /// Reset for new method compilation
    pub fn reset(&mut self) {
        self.method = None;
        self.locals.clear();
        self.next_local = 0;
        self.stack_depth = 0;
        self.max_stack = 0;
        self.stack_types.clear();
        self.parameters.clear();
        self.return_type = None;
        self.is_static = false;
        self.exception_handlers.clear();
        self.local_var_table.clear();
        self.line_number_table.clear();
        self.try_catch_stack.clear();
        self.labels.clear();
        self.next_label = 0;
        self.current_line = 1;
        self.method_signature = None;
    }
}

impl Default for MethodContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_method_context_creation() {
        let ctx = MethodContext::new();
        assert_eq!(ctx.next_local, 0);
        assert_eq!(ctx.stack_depth, 0);
        assert_eq!(ctx.max_stack, 0);
        assert!(ctx.locals.is_empty());
    }
    
    #[test]
    fn test_local_variable_allocation() {
        let mut ctx = MethodContext::new();
        let type_ref = TypeRef {
            name: "int".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: Span::default(),
        };
        
        let slot = ctx.allocate_local("x".to_string(), type_ref, false).unwrap();
        assert_eq!(slot, 0);
        assert_eq!(ctx.next_local, 1);
        assert!(ctx.locals.contains_key("x"));
    }
    
    #[test]
    fn test_stack_depth_tracking() {
        let mut ctx = MethodContext::new();
        
        ctx.update_stack_depth(2);
        assert_eq!(ctx.stack_depth, 2);
        assert_eq!(ctx.max_stack, 2);
        
        ctx.update_stack_depth(1);
        assert_eq!(ctx.stack_depth, 3);
        assert_eq!(ctx.max_stack, 3);
        
        ctx.update_stack_depth(-1);
        assert_eq!(ctx.stack_depth, 2);
        assert_eq!(ctx.max_stack, 3);
    }
    
    #[test]
    fn test_type_descriptor_conversion() {
        let ctx = MethodContext::new();
        
        let int_type = TypeRef {
            name: "int".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: Span::default(),
        };
        assert_eq!(ctx.type_ref_to_descriptor(&int_type), "I");
        
        let string_type = TypeRef {
            name: "java.lang.String".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 0,
            span: Span::default(),
        };
        assert_eq!(ctx.type_ref_to_descriptor(&string_type), "Ljava/lang/String;");
        
        let int_array_type = TypeRef {
            name: "int".to_string(),
            type_args: vec![],
            annotations: vec![],
            array_dims: 2,
            span: Span::default(),
        };
        assert_eq!(ctx.type_ref_to_descriptor(&int_array_type), "[[I");
    }
}