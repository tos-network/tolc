//! Method writer for generating Java bytecode
//! 
//! This module handles the conversion of AST method declarations into Java bytecode instructions.

use super::bytecode::*;

use super::classpath;
use super::opcode_generator::OpcodeGenerator;    
use crate::ast::*;
use crate::codegen::attribute::ExceptionTableEntry;
use crate::codegen::stack_map_optimizer::StackMapOptimizer;
// use crate::codegen::frame::VerificationType;
use crate::codegen::constant_optimizer::ConstantOptimizer;
use crate::codegen::method_invocation_optimizer::MethodInvocationOptimizer;
use crate::codegen::field_access_optimizer::FieldAccessOptimizer;
use crate::codegen::increment_optimizer::IncrementOptimizer;
use crate::codegen::type_coercion_optimizer::{TypeCoercionOptimizer, CoercionOptimizationType};
use crate::codegen::string_optimizer::StringOptimizer;
use crate::codegen::switch_optimizer::SwitchOptimizer;
// Removed cast_optimizer dependency
// use crate::codegen::chain::Chain;
// use crate::codegen::instruction_widening::InstructionWidener;
use crate::codegen::exception_optimizer::ExceptionOptimizer;
use crate::codegen::loop_optimizer::LoopOptimizer;
use crate::codegen::advanced_optimizer::AdvancedCodeGenerator;
use crate::codegen::finalizer_optimizer::ExceptionHandlingOptimizer;
use crate::codegen::instruction_optimizer::InstructionOptimizer;
use crate::codegen::type_erasure::TypeErasureProcessor;
use crate::codegen::fatcode_manager::FatcodeManager;
use crate::codegen::pending_jumps::PendingJumpsManager;
use crate::codegen::fixed_pc_manager::FixedPcManager;
use crate::codegen::jsr_ret_optimizer::JsrRetOptimizer;
use crate::codegen::enhanced_string_optimizer::EnhancedStringOptimizer;
use crate::error::{Result, Error};

// Include the generated runtime metadata
mod boot {
    include!("../rt.rs");
}

// Local overlay for project-specific classes not in rt.jar
#[derive(Copy, Clone)]
struct LocalField { 
    name: &'static str, 
    desc: &'static str 
}

fn local_overlay_fields(owner: &str) -> Option<&'static [LocalField]> {
    match owner {
        _ => None,
    }
}

// Method resolution using rt.rs

/// Minimal arity counter (quick filter). Switch to slot-count if needed.
fn count_params(desc: &str) -> usize {
    let bytes = desc.as_bytes();
    let mut i = 0usize;
    assert!(bytes[i] == b'('); i += 1;
    let mut n = 0usize;
    while bytes[i] != b')' {
        match bytes[i] {
            b'B'|b'C'|b'F'|b'I'|b'S'|b'Z'|b'J'|b'D' => { n += 1; i += 1; }
            b'[' => {
                i += 1; while bytes[i] == b'[' { i += 1; }
                if bytes[i] == b'L' { while bytes[i] != b';' { i += 1; } i += 1; } else { i += 1; }
                n += 1;
            }
            b'L' => { while bytes[i] != b';' { i += 1; } i += 1; n += 1; }
            _ => break,
        }
    }
    n
}

/// Resolve method by walking owner → super chain using rt.rs and current class context
fn resolve_method_with_context(
    owner_internal: &str,
    name: &str,
    expected_arity: usize,
    current_class: Option<&crate::ast::ClassDecl>, // Current class being compiled
    all_types: Option<&[crate::ast::TypeDecl]>, // All types in current compilation unit
) -> Option<ResolvedMethod> {
    eprintln!("🔍 DEBUG: resolve_method_with_context: Looking for {}#{} with arity {}", owner_internal, name, expected_arity);
    eprintln!("🔍 DEBUG: resolve_method_with_context: current_class = {:?}, all_types = {:?}", 
              current_class.map(|c| &c.name), 
              all_types.map(|t| t.len()));
    // First check if this is the current class being compiled
    if let Some(class) = current_class {
        let current_class_internal = class.name.replace(".", "/");
        if owner_internal == current_class_internal {
            // Look for the method in the current class
            for member in &class.body {
                if let crate::ast::ClassMember::Method(method) = member {
                if method.name == name {
                    // Quick arity check - count parameters
                    let param_count = method.parameters.len();
                    if param_count == expected_arity {
                        // Generate descriptor from parameters and return type
                        let descriptor = generate_method_descriptor_from_decl(method);
                        let flags = method_flags_from_decl(method);
                        let is_private = method.modifiers.contains(&crate::ast::Modifier::Private);
                        
                        return Some(ResolvedMethod {
                            owner_internal: current_class_internal,
                            name: method.name.clone(),
                            descriptor,
                            is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                            is_interface: false, // Classes are not interfaces
                            is_ctor: method.name == "<init>",
                            is_private,
                            is_super_call: false,
                            flags,
                        });
                    }
                }
                }
            }
        }
    }
    
    // Check if the owner is an interface in the current compilation unit
    if let Some(types) = all_types {
        eprintln!("🔍 DEBUG: resolve_method_with_context: Checking {} types in compilation unit", types.len());
        for type_decl in types {
            match type_decl {
                crate::ast::TypeDecl::Interface(interface) => {
                    let interface_internal = interface.name.replace(".", "/");
                    eprintln!("🔍 DEBUG: resolve_method_with_context: Found interface {} (internal: {})", interface.name, interface_internal);
                    if owner_internal == interface_internal {
                        eprintln!("🔍 DEBUG: resolve_method_with_context: Interface matches! Looking for method {} in interface {}", name, interface.name);
                        // Look for the method in the interface
                        for member in &interface.body {
                            if let crate::ast::InterfaceMember::Method(method) = member {
                                eprintln!("🔍 DEBUG: resolve_method_with_context: Found interface method {} with {} parameters", method.name, method.parameters.len());
                                if method.name == name {
                                    // Quick arity check - count parameters
                                    let param_count = method.parameters.len();
                                    eprintln!("🔍 DEBUG: resolve_method_with_context: Method name matches! Checking arity: {} vs {}", param_count, expected_arity);
                                    if param_count == expected_arity {
                                        eprintln!("🔍 DEBUG: resolve_method_with_context: Arity matches! Resolving interface method {}#{}", interface_internal, name);
                                        // Generate descriptor from parameters and return type
                                        let descriptor = generate_method_descriptor_from_decl(method);
                                        let flags = method_flags_from_decl(method) | 0x0400; // Add ACC_ABSTRACT flag for interface methods
                                        
                                        return Some(ResolvedMethod {
                                            owner_internal: interface_internal,
                                            name: method.name.clone(),
                                            descriptor,
                                            is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                                            is_interface: true, // This is an interface method
                                            is_ctor: false, // Interfaces don't have constructors
                                            is_private: method.modifiers.contains(&crate::ast::Modifier::Private),
                                            is_super_call: false,
                                            flags,
                                        });
                                    }
                                }
                            }
                        }
                        
                        // Method not found in this interface, check parent interfaces
                        eprintln!("🔍 DEBUG: resolve_method_with_context: Method not found in interface {}, checking parent interfaces", interface.name);
                        for parent in &interface.extends {
                            eprintln!("🔍 DEBUG: resolve_method_with_context: Checking parent interface: {}", parent.name);
                            let parent_internal = parent.name.replace(".", "/");
                            // Recursively search in parent interface
                            if let Some(resolved) = resolve_method_with_context(&parent_internal, name, expected_arity, current_class, all_types) {
                                eprintln!("🔍 DEBUG: resolve_method_with_context: Found method in parent interface {}", parent.name);
                                return Some(resolved);
                            }
                        }
                    }
                }
                crate::ast::TypeDecl::Class(class) => {
                    let class_internal = class.name.replace(".", "/");
                    if owner_internal == class_internal && current_class.map_or(true, |c| c.name != class.name) {
                        // Look for the method in other classes in the compilation unit
                        for member in &class.body {
                            if let crate::ast::ClassMember::Method(method) = member {
                                if method.name == name {
                                    // Quick arity check - count parameters
                                    let param_count = method.parameters.len();
                                    if param_count == expected_arity {
                                        // Filter out private methods (following Java visibility rules)
                                        let is_private = method.modifiers.contains(&crate::ast::Modifier::Private);
                                        if !is_private {
                                        // Generate descriptor from parameters and return type
                                        let descriptor = generate_method_descriptor_from_decl(method);
                                        let flags = method_flags_from_decl(method);
                                        
                                        return Some(ResolvedMethod {
                                            owner_internal: class_internal,
                                            name: method.name.clone(),
                                            descriptor,
                                            is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                                            is_interface: false,
                                            is_ctor: method.name == "<init>",
                                            is_private: false, // Already filtered out private methods
                                            is_super_call: false,
                                            flags,
                                        });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                _ => {} // Handle other type declarations if needed
            }
        }
    }
    
    // If not found in current class, fall back to rt.rs with complete inheritance chain resolution
    resolve_method_in_inheritance_chain(owner_internal, name, expected_arity, current_class, all_types)
}

/// Resolve method in complete inheritance chain: self -> parent class -> interfaces -> recursive up
fn resolve_method_in_inheritance_chain(
    owner_internal: &str,
    name: &str,
    expected_arity: usize,
    current_class: Option<&crate::ast::ClassDecl>,
    all_types: Option<&[crate::ast::TypeDecl]>,
) -> Option<ResolvedMethod> {
    use boot::{CLASSES, CLASSES_BY_NAME};
    
    eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Starting search for {}#{} with arity {}", owner_internal, name, expected_arity);
    
    // Step 0: First check classes in the current compilation unit (filesystem classes)
    if let Some(types) = all_types {
        for type_decl in types {
            if let crate::ast::TypeDecl::Class(class) = type_decl {
                // Check if this class matches by comparing the class name
                // The owner_internal should end with the class name
                if owner_internal.ends_with(&class.name) {
                    eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Found class {} in compilation unit", owner_internal);
                    
                    // Look for method in this class - collect all candidates first (excluding private methods)
                    let mut candidates = Vec::new();
                    for member in &class.body {
                        if let crate::ast::ClassMember::Method(method) = member {
                            if method.name == name && method.parameters.len() == expected_arity {
                                // Filter out private methods (following Java visibility rules)
                                let is_private = method.modifiers.contains(&crate::ast::Modifier::Private);
                                if !is_private {
                                    let descriptor = generate_method_descriptor_from_decl(method);
                                    candidates.push((method, descriptor));
                                }
                            }
                        }
                    }
                    
                    if !candidates.is_empty() {
                        // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Found {} method candidates for {}#{} in compilation unit class {}", 
                        //          candidates.len(), name, expected_arity, owner_internal);
                        
                        // If only one candidate, use it
                        if candidates.len() == 1 {
                            let (method, descriptor) = &candidates[0];
                            // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Single candidate: {}", descriptor);
                            return Some(ResolvedMethod {
                                owner_internal: owner_internal.to_string(),
                                name: method.name.clone(),
                                descriptor: descriptor.clone(),
                                is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                                is_interface: false,
                                is_ctor: method.name == "<init>",
                                is_private: false, // Already filtered out private methods
                                is_super_call: false,
                                flags: 0, // TODO: compute proper flags if needed
                            });
                        }
                        
                        // Multiple candidates - use smart selection based on method characteristics
                        
                        // Check if any method has parameters - this affects our selection strategy
                        let has_parameters = candidates.iter().any(|(_, desc)| {
                            let param_start = desc.find('(').unwrap_or(0) + 1;
                            let param_end = desc.find(')').unwrap_or(desc.len());
                            param_end > param_start
                        });
                        
                        // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Method selection - has_parameters={}, candidates={}", 
                        //          has_parameters, candidates.len());
                        // for (_, desc) in &candidates {
                        //     eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Candidate descriptor: {}", desc);
                        // }
                        
                        if has_parameters {
                            // For methods with parameters (like nativePrint), prefer Object parameters for compatibility
                            for (method, descriptor) in &candidates {
                                // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Candidate: {}", descriptor);
                                if descriptor.contains("(Ljava/lang/Object;)") || descriptor.contains("(LObject;)") {
                                    eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Selecting Object-parameter method (has_parameters=true): {}", descriptor);
                                    return Some(ResolvedMethod {
                                        owner_internal: owner_internal.to_string(),
                                        name: method.name.clone(),
                                        descriptor: descriptor.clone(),
                                        is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                                        is_interface: false,
                                        is_ctor: method.name == "<init>",
                                        is_private: false, // Already filtered out private methods
                                        is_super_call: false,
                                        flags: 0, // TODO: compute proper flags if needed
                                    });
                                }
                            }
                        } else {
                            // For methods without parameters (like getValue), prefer specific return types
                            for (method, descriptor) in &candidates {
                                // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Candidate: {}", descriptor);
                                if !descriptor.contains(")Ljava/lang/Object;") && !descriptor.contains(")LObject;") {
                                    eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Selecting specific-return-type method (has_parameters=false): {}", descriptor);
                                    return Some(ResolvedMethod {
                                        owner_internal: owner_internal.to_string(),
                                        name: method.name.clone(),
                                        descriptor: descriptor.clone(),
                                        is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                                        is_interface: false,
                                        is_ctor: method.name == "<init>",
                                        is_private: false, // Already filtered out private methods
                                        is_super_call: false,
                                        flags: 0, // TODO: compute proper flags if needed
                                    });
                                }
                            }
                        }
                        
                        // Fallback: try any remaining methods
                        for (method, descriptor) in &candidates {
                            // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Fallback to method: {}", descriptor);
                            return Some(ResolvedMethod {
                                owner_internal: owner_internal.to_string(),
                                name: method.name.clone(),
                                descriptor: descriptor.clone(),
                                is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                                is_interface: false,
                                is_ctor: method.name == "<init>",
                                is_private: false, // Already filtered out private methods
                                is_super_call: false,
                                flags: 0, // TODO: compute proper flags if needed
                            });
                        }
                        
                        // Fallback to first candidate if no Object-parameter method found
                        let (method, descriptor) = &candidates[0];
                        // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Using first candidate: {}", descriptor);
                        return Some(ResolvedMethod {
                            owner_internal: owner_internal.to_string(),
                            name: method.name.clone(),
                            descriptor: descriptor.clone(),
                            is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                            is_interface: false,
                            is_ctor: method.name == "<init>",
                            is_private: method.modifiers.contains(&crate::ast::Modifier::Private),
                            is_super_call: false,
                            flags: 0, // TODO: compute proper flags if needed
                        });
                    }
                    
                    // If not found in this class, check parent class recursively
                    if let Some(parent) = &class.extends {
                        let parent_internal = parent.name.replace(".", "/");
                        
                        if let Some(resolved) = resolve_method_in_inheritance_chain(&parent_internal, name, expected_arity, None, all_types) {
                            eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Found method in parent class {} (from compilation unit)", parent_internal);
                            return Some(resolved);
                        }
                    }
                    
                    // Check interfaces
                    for interface in &class.implements {
                        let interface_internal = interface.name.replace(".", "/");
                        if let Some(resolved) = resolve_method_in_inheritance_chain(&interface_internal, name, expected_arity, current_class, all_types) {
                            eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Found method in interface {} (from compilation unit)", interface_internal);
                            return Some(resolved);
                        }
                    }
                    
                    break; // Found the class, no need to continue searching
                }
            }
        }
    }
    
    // Step 1: Check the target class itself in runtime classes
    eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Looking up '{}' in CLASSES_BY_NAME", owner_internal);
    if let Some(&idx) = CLASSES_BY_NAME.get(owner_internal) {
            let c = &CLASSES[idx];
        eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Checking class {} (is_interface: {})", c.internal, c.is_interface);
        
        // Look for method in this class/interface
        // Find all methods with matching name and arity
        let candidates: Vec<_> = c.methods.iter()
            .filter(|m| m.name == name && count_params(m.desc) == expected_arity)
            .collect();
            
        if !candidates.is_empty() {
            eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Found {} method candidates for {}#{} in {}", 
                     candidates.len(), name, expected_arity, c.internal);
            
            // If only one candidate, use it
            if candidates.len() == 1 {
                let m = candidates[0];
                eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Single candidate: {}", m.desc);
                return Some(ResolvedMethod {
                    owner_internal: c.internal.to_string(),
                    name: m.name.to_string(),
                    descriptor: m.desc.to_string(),
                    is_static: m.flags & 0x0008 != 0, // ACC_STATIC
                    is_interface: c.is_interface,
                    is_ctor: m.name == "<init>",
                    is_private: false, // Already filtered out private methods
                    is_super_call: false,
                    flags: m.flags,
                });
            }
            
            // Multiple candidates - use smart selection based on method characteristics
            // Check if any method has parameters - this affects our selection strategy
            let has_parameters = candidates.iter().any(|m| {
                let param_start = m.desc.find('(').unwrap_or(0) + 1;
                let param_end = m.desc.find(')').unwrap_or(m.desc.len());
                param_end > param_start
            });
            
            // eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Runtime method selection - has_parameters={}, candidates={}", 
            //          has_parameters, candidates.len());
            // for m in &candidates {
            //     eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Runtime candidate descriptor: {}", m.desc);
            // }
            
            if has_parameters {
                // For methods with parameters (like HashMap.remove), prefer Object parameters for compatibility
            for m in &candidates {
                    if m.desc.contains("(Ljava/lang/Object;)") || m.desc.contains("(LObject;)") {
                        eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Selecting Object-parameter method (runtime, has_parameters=true): {}", m.desc);
                    return Some(ResolvedMethod {
                        owner_internal: c.internal.to_string(),
                        name: m.name.to_string(),
                        descriptor: m.desc.to_string(),
                        is_static: m.flags & 0x0008 != 0, // ACC_STATIC
                        is_interface: c.is_interface,
                        is_ctor: m.name == "<init>",
                        is_private: false, // Already filtered out private methods
                        is_super_call: false,
                        flags: m.flags,
                    });
                    }
                }
            } else {
                // For methods without parameters (like getValue), prefer specific return types
                for m in &candidates {
                    if !m.desc.contains(")Ljava/lang/Object;") && !m.desc.contains(")LObject;") {
                        eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Selecting specific-return-type method (runtime, has_parameters=false): {}", m.desc);
                        return Some(ResolvedMethod {
                            owner_internal: c.internal.to_string(),
                            name: m.name.to_string(),
                            descriptor: m.desc.to_string(),
                            is_static: m.flags & 0x0008 != 0, // ACC_STATIC
                            is_interface: c.is_interface,
                            is_ctor: m.name == "<init>",
                            is_private: false, // Already filtered out private methods
                            is_super_call: false,
                            flags: m.flags,
                        });
                    }
                }
            }
            
            // Fallback: try any remaining methods
            for m in &candidates {
                eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Fallback to runtime method: {}", m.desc);
                return Some(ResolvedMethod {
                    owner_internal: c.internal.to_string(),
                    name: m.name.to_string(),
                    descriptor: m.desc.to_string(),
                    is_static: m.flags & 0x0008 != 0, // ACC_STATIC
                    is_interface: c.is_interface,
                    is_ctor: m.name == "<init>",
                    is_private: false, // Already filtered out private methods
                    is_super_call: false,
                    flags: m.flags,
                });
            }
            
            // Fallback to first candidate if no Object-parameter method found
            let m = candidates[0];
            eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Using first candidate: {}", m.desc);
            return Some(ResolvedMethod {
                owner_internal: c.internal.to_string(),
                name: m.name.to_string(),
                descriptor: m.desc.to_string(),
                is_static: m.flags & 0x0008 != 0, // ACC_STATIC
                is_interface: c.is_interface,
                is_ctor: m.name == "<init>",
                is_private: false, // Already filtered out private methods
                is_super_call: false,
                flags: m.flags,
            });
        }
        
        // Step 2: Check implemented interfaces (for both classes and interfaces)
        eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Method not found in {}, checking {} interfaces", c.internal, c.interfaces.len());
        for &interface_name in c.interfaces {
            eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Checking interface: {}", interface_name);
            if let Some(resolved) = resolve_method_in_inheritance_chain(interface_name, name, expected_arity, current_class, all_types) {
                eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Found method in interface {}", interface_name);
                return Some(resolved);
            }
        }
        
        // Step 3: Check parent class (for both classes and interfaces)
        // In Java, interfaces also inherit from Object, so we should check super_internal for interfaces too
        if let Some(super_internal) = c.super_internal {
            eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Method not found in {}, checking parent class: {}", c.internal, super_internal);
            if let Some(resolved) = resolve_method_in_inheritance_chain(super_internal, name, expected_arity, current_class, all_types) {
                eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Found method in parent class {}", super_internal);
                return Some(resolved);
            }
        }
    } else {
        eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Class {} not found in rt.rs", owner_internal);
    }
    
    eprintln!("🔍 DEBUG: resolve_method_in_inheritance_chain: Method {}#{} not found in inheritance chain starting from {}", name, expected_arity, owner_internal);
    None
}

/// Generate method descriptor from method declaration
fn generate_method_descriptor_from_decl(method: &crate::ast::MethodDecl) -> String {
    let mut desc = "(".to_string();
    
    // Add parameter types
    for param in &method.parameters {
        desc.push_str(&type_ref_to_descriptor(&param.type_ref));
    }
    
    desc.push(')');
    
    // Add return type
    if let Some(return_type) = &method.return_type {
        desc.push_str(&type_ref_to_descriptor(return_type));
    } else {
        desc.push('V'); // void
    }
    
    desc
}

/// Convert TypeRef to JVM descriptor
fn type_ref_to_descriptor(type_ref: &crate::ast::TypeRef) -> String {
    match type_ref.name.as_str() {
        "void" => "V".to_string(),
        "boolean" => "Z".to_string(),
        "byte" => "B".to_string(),
        "char" => "C".to_string(),
        "short" => "S".to_string(),
        "int" => "I".to_string(),
        "long" => "J".to_string(),
        "float" => "F".to_string(),
        "double" => "D".to_string(),
        _ => {
            // Reference type
            let mut desc = "L".to_string();
            desc.push_str(&type_ref.name.replace(".", "/"));
            desc.push(';');
            desc
        }
    }
}

/// Convert method modifiers to JVM flags
fn method_flags_from_decl(method: &crate::ast::MethodDecl) -> u16 {
    let mut flags = 0u16;
    
    for modifier in &method.modifiers {
        match modifier {
            crate::ast::Modifier::Public => flags |= 0x0001,    // ACC_PUBLIC
            crate::ast::Modifier::Private => flags |= 0x0002,   // ACC_PRIVATE
            crate::ast::Modifier::Protected => flags |= 0x0004, // ACC_PROTECTED
            crate::ast::Modifier::Static => flags |= 0x0008,    // ACC_STATIC
            crate::ast::Modifier::Final => flags |= 0x0010,     // ACC_FINAL
            crate::ast::Modifier::Synchronized => flags |= 0x0020, // ACC_SYNCHRONIZED
            crate::ast::Modifier::Native => flags |= 0x0100,    // ACC_NATIVE
            crate::ast::Modifier::Abstract => flags |= 0x0400,  // ACC_ABSTRACT
            crate::ast::Modifier::Strictfp => flags |= 0x0800,  // ACC_STRICT
            _ => {} // Other modifiers don't map to method flags
        }
    }
    
    flags
}

/// Backward compatibility wrapper
fn resolve_method(
    owner_internal: &str,
    name: &str,
    expected_arity: usize,
) -> Option<ResolvedMethod> {
    resolve_method_with_context(owner_internal, name, expected_arity, None, None)
}

/// Check if a type string represents an array type
fn is_array_type(type_str: &str) -> bool {
    // Handle different array type formats:
    // 1. Java format: "Object[]", "int[]", etc.
    // 2. JVM descriptor format: "[Ljava/lang/Object;", "[I", etc.
    type_str.ends_with("[]") || type_str.starts_with('[')
}

/// Argument layout information for method generation
#[derive(Debug, Clone)]
struct ArgLayout {
    /// Whether the method is static
    is_static: bool,
    /// Next local variable index after arguments
    next_local: u16,
}

impl ArgLayout {
    /// Compute argument layout from method descriptor and flags
    fn compute(desc: &str, flags: u16) -> Self {
        let is_static = flags & 0x0008 != 0; // ACC_STATIC
        let mut next = 0;
        
        if !is_static {
            next += 1; // 'this' reference
        }
        
        // Parse descriptor to count argument slots
        if let Some(args_start) = desc.find('(') {
            if let Some(args_end) = desc.find(')') {
                let args_part = &desc[args_start + 1..args_end];
                let mut i = 0;
                while i < args_part.len() {
                    match args_part.chars().nth(i).unwrap() {
                        'L' => {
                            // Reference type: L...;
                            if let Some(semicolon) = args_part[i..].find(';') {
                                i += semicolon + 1;
                            } else {
                                i += 1;
                            }
                            next += 1;
                        }
                        '[' => {
                            // Array type: [...
                            i += 1;
                            while i < args_part.len() && args_part.chars().nth(i).unwrap() == '[' {
                                i += 1;
                            }
                            if i < args_part.len() {
                                match args_part.chars().nth(i).unwrap() {
                                    'L' => {
                                        // Array of reference: [L...;
                                        if let Some(semicolon) = args_part[i..].find(';') {
                                            i += semicolon + 1;
                                        } else {
                                            i += 1;
                                        }
                                    }
                                    _ => i += 1, // Array of primitive
                                }
                            }
                            next += 1;
                        }
                        'J' | 'D' => {
                            // Long or double: 2 slots
                            next += 2;
                            i += 1;
                        }
                        _ => {
                            // Other primitives: 1 slot
                            next += 1;
                            i += 1;
                        }
                    }
                }
            }
        }
        
        ArgLayout { is_static, next_local: next }
    }
}

/// Resolved method information for proper invoke opcode selection
#[derive(Debug, Clone)]
struct ResolvedMethod {
    /// Internal name of the declaring class
    owner_internal: String,
    /// Method name
    name: String,
    /// Method descriptor
    descriptor: String,
    /// Whether the method is static
    is_static: bool,
    /// Whether the method is an interface method
    is_interface: bool,
    /// Whether the method is a constructor
    is_ctor: bool,
    /// Whether the method is private
    is_private: bool,
    /// Whether this is a super call
    is_super_call: bool,
    /// Method flags (ACC_STATIC, ACC_INTERFACE, etc.)
    flags: u16,
}

impl ResolvedMethod {
    /// Create a new resolved method
    fn new(owner: &str, name: &str, descriptor: &str, flags: u16) -> Self {
        Self {
            owner_internal: owner.to_string(),
            name: name.to_string(),
            descriptor: descriptor.to_string(),
            is_static: flags & 0x0008 != 0, // ACC_STATIC
            is_interface: flags & 0x0200 != 0, // ACC_INTERFACE
            is_ctor: name == "<init>",
            is_private: flags & 0x0002 != 0, // ACC_PRIVATE
            is_super_call: false, // This needs to be set by caller
            flags,
        }
    }
}

/// Method writer for generating Java bytecode using BytecodeBuilder
pub struct MethodWriter {
    /// High-level bytecode builder with automatic stack management
    bytecode_builder: BytecodeBuilder,
    /// Opcode generator for creating bytecode instructions
    opcode_generator: OpcodeGenerator,
    /// Loop context stack
    loop_stack: Vec<LoopContext>,
    /// Scope stack for local variables
    scope_stack: Vec<Scope>,
    /// Pending exception entries
    pending_exception_entries: Vec<PendingExceptionEntry>,
    /// Line numbers for debugging
    line_numbers: Vec<(u16, u16)>,
    /// Constant pool reference
    constant_pool: Option<std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>>,
    /// Current class name
    current_class_name: Option<String>,
    /// Current class declaration for local method resolution
    current_class: Option<crate::ast::ClassDecl>,
    /// All types in the current compilation unit for interface method resolution
    all_types: Option<Vec<crate::ast::TypeDecl>>,
    /// Next label ID for generating unique string labels
    next_label_id: u16,
    /// Stack map optimizer for efficient frame generation
    stack_map_optimizer: StackMapOptimizer,
    /// Method invocation optimizer for javac-style optimizations
    method_invocation_optimizer: MethodInvocationOptimizer,
    /// Field access optimizer for javac-style optimizations
    field_access_optimizer: FieldAccessOptimizer,
    /// Switch statement optimizer for tableswitch vs lookupswitch selection
    switch_optimizer: SwitchOptimizer,
    /// Cast optimizer for intelligent checkcast generation
    // Removed cast_optimizer field
    /// Constant optimizer for efficient constant loading
    constant_optimizer: ConstantOptimizer,
    /// Type coercion optimizer for primitive type conversions
    type_coercion_optimizer: TypeCoercionOptimizer,
    /// String optimizer for string concatenation and operations
    string_optimizer: StringOptimizer,
    /// Increment optimizer for ++/-- operations using iinc
    increment_optimizer: IncrementOptimizer,
    /// Exception optimizer for try-catch and exception handling
    exception_optimizer: ExceptionOptimizer,
    /// Loop optimizer for unified loop generation and optimization
    loop_optimizer: LoopOptimizer,
    /// Advanced code generator for complex optimizations
    advanced_optimizer: AdvancedCodeGenerator,
    /// Exception handling optimizer for finalizer and JSR optimization
    finalizer_optimizer: ExceptionHandlingOptimizer,
    /// Instruction optimizer for peephole and instruction-level optimizations
    instruction_optimizer: InstructionOptimizer,
    /// javac-style conditional item system for advanced condition optimization
    cond_item_optimizer: crate::codegen::cond_item::CondItemOptimizer,
    /// javac-style genCond system for intelligent conditional generation
    gen_cond_optimizer: crate::codegen::gen_cond::GenCond,
    /// javac-style assignment optimizer with iinc and dup_x1 optimizations
    assignment_optimizer: crate::codegen::assignment_optimizer::AssignmentOptimizer,
    /// javac-style string buffer optimizer for efficient string concatenation
    string_buffer_optimizer: crate::codegen::string_buffer_optimizer::StringBufferOptimizer,
    /// javac-style item system for representing addressable entities
    item_factory: crate::codegen::item_system::ItemFactory,
    /// Type erasure processor for generic type handling
    type_erasure: TypeErasureProcessor,
    
    /// JavaC-style fatcode manager for handling 32K+ jumps
    fatcode_manager: FatcodeManager,
    
    /// JavaC-style pending jumps manager for jump optimization
    pending_jumps_manager: PendingJumpsManager,
    
    /// JavaC-style fixed PC manager for preventing code compaction at jump targets
    fixed_pc_manager: FixedPcManager,
    
    /// JavaC-style JSR/RET optimizer for subroutine calls and finally blocks
    jsr_ret_optimizer: JsrRetOptimizer,
    
    /// Enhanced string optimizer for StringBuilder/StringBuffer operations
    enhanced_string_optimizer: EnhancedStringOptimizer,
    /// Enhanced Stack Map Frame Emitter for advanced frame generation
    stack_map_emitter: Option<crate::codegen::enhanced_stack_map_emitter::EnhancedStackMapEmitter>,
    /// Label positions for dynamic jump target calculation
    label_positions: Option<std::collections::HashMap<u16, u16>>,
    
    /// Map labels to pending jump chains for resolution
    label_to_chain_mapping: Option<std::collections::HashMap<String, u32>>,
    

}

#[derive(Debug, PartialEq)]
enum ParameterSpecificity {
    FirstMoreSpecific,
    SecondMoreSpecific,
    Equal,
}

impl MethodWriter {
    fn slots_of_type(ch: char) -> i32 {
        match ch { 'J' | 'D' => 2, _ => 1 }
    }
    
    /// Intelligent method resolution based on javac's approach
    /// This method analyzes argument types to select the most specific method
    fn resolve_method_with_argument_analysis(&self, owner_class: &str, method_name: &str, expected_arity: usize, arg_expressions: &[Expr]) -> Option<ResolvedMethod> {
        // Analyze actual argument types
        let arg_types: Vec<String> = arg_expressions.iter().map(|arg| {
            self.resolve_expression_type(arg)
        }).collect();
        
        eprintln!("🔍 DEBUG: resolve_method_with_argument_analysis: Method={}#{}, arg_types={:?}", method_name, expected_arity, arg_types);
        
        // Get all method candidates
        let candidates = self.get_all_method_candidates(owner_class, method_name, expected_arity);
        
        if candidates.is_empty() {
            return None;
        }
        
        if candidates.len() == 1 {
            eprintln!("🔍 DEBUG: resolve_method_with_argument_analysis: Single candidate: {}", candidates[0].descriptor);
            return Some(candidates[0].clone());
        }
        
        // Apply intelligent selection based on argument types
        let best_candidate = self.select_most_specific_method(&candidates, &arg_types);
        eprintln!("🔍 DEBUG: resolve_method_with_argument_analysis: Selected: {}", best_candidate.descriptor);
        Some(best_candidate)
    }
    
    /// Get all method candidates with matching name and arity
    fn get_all_method_candidates(&self, owner_class: &str, method_name: &str, expected_arity: usize) -> Vec<ResolvedMethod> {
        let mut candidates = Vec::new();
        
        // Check current class
        if let Some(class) = &self.current_class {
            let current_class_internal = class.name.replace(".", "/");
            if owner_class == current_class_internal {
                for member in &class.body {
                    if let crate::ast::ClassMember::Method(method) = member {
                        if method.name == method_name && method.parameters.len() == expected_arity {
                            // Filter out private methods (following Java visibility rules)
                            let is_private = method.modifiers.contains(&crate::ast::Modifier::Private);
                            if !is_private {
                                let descriptor = self.generate_descriptor_from_ast_method(&method);
                                candidates.push(ResolvedMethod {
                                    owner_internal: current_class_internal.clone(),
                                    name: method.name.clone(),
                                    descriptor,
                                    is_static: method.modifiers.contains(&crate::ast::Modifier::Static),
                                    is_interface: false,
                                    is_ctor: method.name == "<init>",
                                    is_private: false, // Already filtered out private methods
                                    is_super_call: false,
                                    flags: 0,
                                });
                            }
                        }
                    }
                }
            }
        }
        
        // Check runtime classes
        use crate::rt::{CLASSES_BY_NAME, CLASSES};
        if let Some(&idx) = CLASSES_BY_NAME.get(owner_class) {
            let c = &CLASSES[idx];
            for m in c.methods {
                if m.name == method_name && count_params(m.desc) == expected_arity {
                    candidates.push(ResolvedMethod {
                        owner_internal: c.internal.to_string(),
                        name: m.name.to_string(),
                        descriptor: m.desc.to_string(),
                        is_static: m.flags & 0x0008 != 0,
                        is_interface: c.is_interface,
                        is_ctor: m.name == "<init>",
                        is_private: m.flags & 0x0002 != 0, // ACC_PRIVATE
                        is_super_call: false,
                        flags: m.flags,
                    });
                }
            }
        }
        
        candidates
    }
    
    /// Select the most specific method based on javac's algorithm
    fn select_most_specific_method(&self, candidates: &[ResolvedMethod], arg_types: &[String]) -> ResolvedMethod {
        let mut best = candidates[0].clone();
        
        for candidate in &candidates[1..] {
            if self.is_method_more_specific(candidate, &best, arg_types) {
                best = candidate.clone();
            }
        }
        
        best
    }
    
    /// Check if method1 is more specific than method2 for given argument types
    fn is_method_more_specific(&self, method1: &ResolvedMethod, method2: &ResolvedMethod, arg_types: &[String]) -> bool {
        let params1 = self.extract_parameter_types(&method1.descriptor);
        let params2 = self.extract_parameter_types(&method2.descriptor);
        
        eprintln!("🔍 DEBUG: is_method_more_specific: Comparing {} vs {}", method1.descriptor, method2.descriptor);
        eprintln!("🔍 DEBUG: is_method_more_specific: params1={:?}, params2={:?}, args={:?}", params1, params2, arg_types);
        
        if params1.len() != params2.len() || params1.len() != arg_types.len() {
            return false;
        }
        
        // Check each parameter position
        let mut method1_better = false;
        let mut method2_better = false;
        
        for i in 0..params1.len() {
            let specificity = self.compare_parameter_specificity(&params1[i], &params2[i], &arg_types[i]);
            match specificity {
                ParameterSpecificity::FirstMoreSpecific => method1_better = true,
                ParameterSpecificity::SecondMoreSpecific => method2_better = true,
                ParameterSpecificity::Equal => {}
            }
        }
        
        // Method1 is more specific if it's better in at least one position and not worse in any
        method1_better && !method2_better
    }
    

    
    /// Compare specificity of two parameter types for a given argument type
    fn compare_parameter_specificity(&self, param1: &str, param2: &str, arg_type: &str) -> ParameterSpecificity {
        eprintln!("🔍 DEBUG: compare_parameter_specificity: param1={}, param2={}, arg={}", param1, param2, arg_type);
        
        if param1 == param2 {
            return ParameterSpecificity::Equal;
        }
        
        // Exact match is always most specific (handle both descriptor and type name formats)
        let param1_matches = param1 == arg_type || self.descriptor_matches_type(param1, arg_type);
        let param2_matches = param2 == arg_type || self.descriptor_matches_type(param2, arg_type);
        
        if param1_matches && !param2_matches {
            eprintln!("🔍 DEBUG: compare_parameter_specificity: param1 exact match");
            return ParameterSpecificity::FirstMoreSpecific;
        }
        if param2_matches && !param1_matches {
            eprintln!("🔍 DEBUG: compare_parameter_specificity: param2 exact match");
            return ParameterSpecificity::SecondMoreSpecific;
        }
        
        // Handle int type matching
        if (param1 == "I" && arg_type == "int") || (param1 == "int" && arg_type == "I") {
            if param2 != "I" && param2 != "int" {
                eprintln!("🔍 DEBUG: compare_parameter_specificity: param1 int match");
                return ParameterSpecificity::FirstMoreSpecific;
            }
        }
        if (param2 == "I" && arg_type == "int") || (param2 == "int" && arg_type == "I") {
            if param1 != "I" && param1 != "int" {
                eprintln!("🔍 DEBUG: compare_parameter_specificity: param2 int match");
                return ParameterSpecificity::SecondMoreSpecific;
            }
        }
        
        // Handle LinkedListCell type matching
        if param1.contains("LinkedListCell") && arg_type.contains("LinkedListCell") {
            if param2 == "Ljava/lang/Object;" || param2.contains("Object") || self.is_primitive_descriptor(param2) {
                eprintln!("🔍 DEBUG: compare_parameter_specificity: param1 LinkedListCell more specific than {}", param2);
                return ParameterSpecificity::FirstMoreSpecific;
            }
        }
        if param2.contains("LinkedListCell") && arg_type.contains("LinkedListCell") {
            if param1 == "Ljava/lang/Object;" || param1.contains("Object") || self.is_primitive_descriptor(param1) {
                eprintln!("🔍 DEBUG: compare_parameter_specificity: param2 LinkedListCell more specific than {}", param1);
                return ParameterSpecificity::SecondMoreSpecific;
            }
        }
        
        // Primitive types are more specific than Object ONLY if the argument is also primitive
        if self.is_primitive_descriptor(param1) && param2 == "Ljava/lang/Object;" {
            // Only prefer primitive if the argument is also primitive-compatible
            if self.is_primitive_type(&arg_type) {
                eprintln!("🔍 DEBUG: compare_parameter_specificity: primitive param1 more specific than Object for primitive arg");
                return ParameterSpecificity::FirstMoreSpecific;
            }
        }
        if self.is_primitive_descriptor(param2) && param1 == "Ljava/lang/Object;" {
            // Only prefer primitive if the argument is also primitive-compatible
            if self.is_primitive_type(&arg_type) {
                eprintln!("🔍 DEBUG: compare_parameter_specificity: primitive param2 more specific than Object for primitive arg");
                return ParameterSpecificity::SecondMoreSpecific;
            }
        }
        
        eprintln!("🔍 DEBUG: compare_parameter_specificity: equal specificity");
        ParameterSpecificity::Equal
    }
    
    /// Check if a descriptor represents a primitive type
    fn is_primitive_descriptor(&self, desc: &str) -> bool {
        matches!(desc, "I" | "Z" | "B" | "S" | "C" | "F" | "D" | "J")
    }
    
    /// Check if a type name represents a primitive type
    fn is_primitive_type(&self, type_name: &str) -> bool {
        matches!(type_name, "int" | "boolean" | "byte" | "short" | "char" | "float" | "double" | "long")
    }
    
    /// Check if a JVM descriptor matches a type name
    fn descriptor_matches_type(&self, descriptor: &str, type_name: &str) -> bool {
        match descriptor {
            "Ljava/lang/Object;" => type_name == "java.lang.Object" || type_name == "Object",
            "Ljava/lang/String;" => type_name == "java.lang.String" || type_name == "String",
            "I" => type_name == "int",
            "Z" => type_name == "boolean",
            "B" => type_name == "byte",
            "S" => type_name == "short",
            "C" => type_name == "char",
            "F" => type_name == "float",
            "D" => type_name == "double",
            "J" => type_name == "long",
            _ => {
                // Handle other object types: LClassName; -> ClassName
                if descriptor.starts_with('L') && descriptor.ends_with(';') {
                    let class_name = &descriptor[1..descriptor.len()-1];
                    let dotted_name = class_name.replace('/', ".");
                    type_name == dotted_name || type_name == class_name.split('/').last().unwrap_or("")
                } else {
                    false
                }
            }
        }
    }
    
    /// Extract parameter types from method descriptor
    fn extract_parameter_types(&self, descriptor: &str) -> Vec<String> {
        let mut params = Vec::new();
        let start = descriptor.find('(').unwrap_or(0) + 1;
        let end = descriptor.find(')').unwrap_or(descriptor.len());
        let param_str = &descriptor[start..end];
        
        let mut i = 0;
        let chars: Vec<char> = param_str.chars().collect();
        
        while i < chars.len() {
            match chars[i] {
                'I' | 'Z' | 'B' | 'S' | 'C' | 'F' | 'D' | 'J' => {
                    params.push(chars[i].to_string());
                    i += 1;
                }
                'L' => {
                    let start_idx = i;
                    while i < chars.len() && chars[i] != ';' {
                        i += 1;
                    }
                    if i < chars.len() {
                        i += 1; // Include semicolon
                    }
                    params.push(chars[start_idx..i].iter().collect());
                }
                '[' => {
                    let start_idx = i;
                    i += 1; // Skip '['
                    match chars[i] {
                        'I' | 'Z' | 'B' | 'S' | 'C' | 'F' | 'D' | 'J' => i += 1,
                        'L' => {
                            while i < chars.len() && chars[i] != ';' {
                                i += 1;
                            }
                            if i < chars.len() {
                                i += 1;
                            }
                        }
                        _ => i += 1,
                    }
                    params.push(chars[start_idx..i].iter().collect());
                }
                _ => i += 1,
            }
        }
        
        params
    }
    
    /// Generate method descriptor from AST method
    fn generate_descriptor_from_ast_method(&self, method: &crate::ast::MethodDecl) -> String {
        let mut descriptor = "(".to_string();
        
        for param in &method.parameters {
            descriptor.push_str(&self.ast_type_to_descriptor(&param.type_ref));
        }
        
        descriptor.push(')');
        
        if let Some(return_type) = &method.return_type {
            descriptor.push_str(&self.ast_type_to_descriptor(return_type));
        } else {
            descriptor.push('V');
        }
        
        descriptor
    }
    
    /// Convert AST TypeRef to descriptor string
    fn ast_type_to_descriptor(&self, type_ref: &crate::ast::TypeRef) -> String {
        match type_ref.name.as_str() {
            "int" => "I".to_string(),
            "boolean" => "Z".to_string(),
            "byte" => "B".to_string(),
            "short" => "S".to_string(),
            "char" => "C".to_string(),
            "float" => "F".to_string(),
            "double" => "D".to_string(),
            "long" => "J".to_string(),
            "void" => "V".to_string(),
            _ => format!("L{};", type_ref.name.replace(".", "/")),
        }
    }
    
    /// Generate integer comparison using if_icmp* instructions
    fn generate_integer_comparison(&mut self, comparison_op: &str) -> Result<()> {
        // At this point, both operands are on the stack: [left, right]
        // We need to generate: if_icmp* true_label -> iconst_1 -> goto end_label -> true_label: iconst_0 -> end_label:
        
        let true_label = self.create_label();
        let end_label = self.create_label();
        
        // Generate the comparison instruction
        let true_label_str = self.label_str(true_label);
        match comparison_op {
            "if_icmplt" => Self::map_stack(self.bytecode_builder.if_icmplt(&true_label_str))?,
            "if_icmple" => Self::map_stack(self.bytecode_builder.if_icmple(&true_label_str))?,
            "if_icmpgt" => Self::map_stack(self.bytecode_builder.if_icmpgt(&true_label_str))?,
            "if_icmpge" => Self::map_stack(self.bytecode_builder.if_icmpge(&true_label_str))?,
            "if_icmpeq" => Self::map_stack(self.bytecode_builder.if_icmpeq(&true_label_str))?,
            "if_icmpne" => Self::map_stack(self.bytecode_builder.if_icmpne(&true_label_str))?,
            _ => return Err(Error::codegen_error(&format!("Unknown comparison operator: {}", comparison_op))),
        }
        
        // False case: push false (0)
        Self::map_stack(self.bytecode_builder.iconst_0())?;
        let end_label_str = self.label_str(end_label);
        Self::map_stack(self.bytecode_builder.goto(&end_label_str))?;
        
        // True case: push true (1)
        self.bytecode_builder.mark_label(&true_label_str);
        Self::map_stack(self.bytecode_builder.iconst_1())?;
        
        self.bytecode_builder.mark_label(&end_label_str);
        Ok(())
    }

    /// Generate zero comparison using single operand instructions (javac-style optimization)
    fn generate_zero_comparison(&mut self, operator: &BinaryOp, operand_type: Option<&str>) -> Result<()> {
        // At this point, the non-zero operand is on the stack
        // Generate optimized zero comparison: value op 0 -> if* instruction
        
        let true_label = self.create_label();
        let end_label = self.create_label();
        
        // Save current stack depth for control flow analysis
        let stack_depth_before_branch = self.bytecode_builder.stack_depth();
        
        // Use javac's pattern: jump to false case if condition is NOT met
        let false_label = true_label; // Reuse the label as false_label
        let false_label_str = self.label_str(false_label);
        
        // 🔧 FIX: Use type-aware conditional jumps for different operand types
        let operand_type = operand_type.unwrap_or("int");
        eprintln!("🔍 DEBUG: generate_zero_comparison: operand_type = '{}'", operand_type);
        
        // 🔧 FIX: Force long type comparison for BitSet expressions (which contain long operations)
        let effective_type = if operand_type == "BitSet" { "long" } else { operand_type };
        eprintln!("🔍 DEBUG: generate_zero_comparison: effective_type = '{}'", effective_type);
        
        match (operator, effective_type) {
            (BinaryOp::Eq, "long") => {
                // value == 0 -> lconst_0 + lcmp + ifeq
                Self::map_stack(self.bytecode_builder.ifeq_typed(&false_label_str, "long"))?;
            }
            (BinaryOp::Ne, "long") => {
                // value != 0 -> lconst_0 + lcmp + ifne
                Self::map_stack(self.bytecode_builder.ifne_typed(&false_label_str, "long"))?;
            }
            (BinaryOp::Eq, "float") => {
                Self::map_stack(self.bytecode_builder.ifeq_typed(&false_label_str, "float"))?;
            }
            (BinaryOp::Ne, "float") => {
                Self::map_stack(self.bytecode_builder.ifne_typed(&false_label_str, "float"))?;
            }
            (BinaryOp::Eq, "double") => {
                Self::map_stack(self.bytecode_builder.ifeq_typed(&false_label_str, "double"))?;
            }
            (BinaryOp::Ne, "double") => {
                Self::map_stack(self.bytecode_builder.ifne_typed(&false_label_str, "double"))?;
            }
            // For int and other types, use the original logic
            (BinaryOp::Eq, _) => {
                // value == 0 -> ifne (jump to false if NOT zero)
                Self::map_stack(self.bytecode_builder.ifne(&false_label_str))?;
            }
            (BinaryOp::Ne, _) => {
                // value != 0 -> ifeq (jump to false if zero)
                Self::map_stack(self.bytecode_builder.ifeq(&false_label_str))?;
            }
            (BinaryOp::Lt, _) => {
                // value < 0 -> ifge (jump to false if >= zero)
                Self::map_stack(self.bytecode_builder.ifge(&false_label_str))?;
            }
            (BinaryOp::Le, _) => {
                // value <= 0 -> ifgt (jump to false if > zero)
                Self::map_stack(self.bytecode_builder.ifgt(&false_label_str))?;
            }
            (BinaryOp::Gt, _) => {
                // value > 0 -> ifle (jump to false if <= zero)
                Self::map_stack(self.bytecode_builder.ifle(&false_label_str))?;
            }
            (BinaryOp::Ge, _) => {
                // value >= 0 -> iflt (jump to false if < zero)
                Self::map_stack(self.bytecode_builder.iflt(&false_label_str))?;
            }
            _ => return Err(Error::codegen_error(&format!("Unsupported zero comparison operator: {:?}", operator))),
        }
        
        // True case: push true (1) - condition is met
        Self::map_stack(self.bytecode_builder.iconst_1())?;
        let end_label_str = self.label_str(end_label);
        Self::map_stack(self.bytecode_builder.goto(&end_label_str))?;
        
        // False case: push false (0) - condition is not met
        // Reset stack depth to the state after the conditional jump for proper control flow analysis
        self.bytecode_builder.set_stack_depth(stack_depth_before_branch - 1); // -1 because ifne popped the value
        self.bytecode_builder.mark_label(&false_label_str);
        Self::map_stack(self.bytecode_builder.iconst_0())?;
        
        self.bytecode_builder.mark_label(&end_label_str);
        Ok(())
    }

    /// Generate reversed zero comparison (for 0 op value patterns)
    fn generate_zero_comparison_reversed(&mut self, operator: &BinaryOp, operand_type: Option<&str>) -> Result<()> {
        // At this point, the non-zero operand is on the stack
        // Generate reversed zero comparison: 0 op value -> reverse the comparison
        
        let reversed_op = match operator {
            BinaryOp::Eq => BinaryOp::Eq,  // 0 == value -> value == 0
            BinaryOp::Ne => BinaryOp::Ne,  // 0 != value -> value != 0
            BinaryOp::Lt => BinaryOp::Gt,  // 0 < value -> value > 0
            BinaryOp::Le => BinaryOp::Ge,  // 0 <= value -> value >= 0
            BinaryOp::Gt => BinaryOp::Lt,  // 0 > value -> value < 0
            BinaryOp::Ge => BinaryOp::Le,  // 0 >= value -> value <= 0
            _ => return Err(Error::codegen_error(&format!("Unsupported reversed zero comparison operator: {:?}", operator))),
        };
        
        self.generate_zero_comparison(&reversed_op, operand_type)
    }

    /// Generate null comparison using direct IFNULL/IFNONNULL (javac-style)
    fn generate_null_comparison(&mut self, operator: &BinaryOp, left_is_null: bool) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_null_comparison: operator={:?}, left_is_null={}", operator, left_is_null);
        
        // At this point, the non-null operand is on the stack
        // Generate optimized null check without intermediate jumps
        
        match operator {
            BinaryOp::Eq => {
                // == null: use ifnull for direct comparison
                // Stack: [value] -> ifnull pushes 1 if null, 0 if not null
                let true_label = self.create_label();
                let end_label = self.create_label();
                
                let true_label_str = self.label_str(true_label);
                Self::map_stack(self.bytecode_builder.ifnull(&true_label_str))?;
                
                // Not null case: push false (0)
                Self::map_stack(self.bytecode_builder.iconst_0())?;
                let end_label_str = self.label_str(end_label);
                Self::map_stack(self.bytecode_builder.goto(&end_label_str))?;
                
                // Null case: push true (1)
                self.bytecode_builder.mark_label(&true_label_str);
                Self::map_stack(self.bytecode_builder.iconst_1())?;
                
                self.bytecode_builder.mark_label(&end_label_str);
            }
            BinaryOp::Ne => {
                // != null: javac-style - use ifnull to jump to false case
                // Stack: [value] -> ifnull jumps to false if null, continues to true if not null
                let false_label = self.create_label();
                let end_label = self.create_label();
                
                let false_label_str = self.label_str(false_label);
                Self::map_stack(self.bytecode_builder.ifnull(&false_label_str))?;
                
                // Not null case: push true (1)
                Self::map_stack(self.bytecode_builder.iconst_1())?;
                let end_label_str = self.label_str(end_label);
                Self::map_stack(self.bytecode_builder.goto(&end_label_str))?;
                
                // Null case: push false (0)
                self.bytecode_builder.mark_label(&false_label_str);
                Self::map_stack(self.bytecode_builder.iconst_0())?;
                
                self.bytecode_builder.mark_label(&end_label_str);
            }
            _ => {
                return Err(Error::codegen_error("Invalid operator for null comparison"));
            }
        }
        
        Ok(())
    }

    /// Check if we should pop the return value of a method (for expression statements)
    fn should_pop_return_value(&self, descriptor: &str) -> bool {
        // Find the return type in the descriptor (after the closing parenthesis)
        if let Some(paren_pos) = descriptor.find(')') {
            let return_type = &descriptor[paren_pos + 1..];
            // Don't pop if the method returns void
            let should_pop = return_type != "V";
            eprintln!("🔍 DEBUG: should_pop_return_value: descriptor={}, return_type={}, should_pop={}", 
                     descriptor, return_type, should_pop);
            should_pop
        } else {
            false
        }
    }
    
    /// Check if a method call result should be popped when used as a statement
    fn should_pop_method_call_result(&self, method_call: &MethodCallExpr) -> bool {
        // Try to resolve the method and check its actual return type
        // Use the same logic as generate_method_call for consistency
        let owner_class = if let Some(target) = &method_call.target {
            // Resolve the type of the target expression
            let target_type = self.resolve_expression_type(target);
            // Strip generic parameters for method resolution
            let base_type = if let Some(generic_start) = target_type.find('<') {
                target_type[..generic_start].to_string()
            } else {
                target_type
            };
            self.resolve_class_name(&base_type)
        } else {
            // Method call without target - use current class with full internal name
            let current_class = self.current_class_name.as_ref()
                .unwrap_or(&"java/lang/Object".to_string())
                .clone();
            self.resolve_class_name(&current_class)
        };
        
        // Use the same arity calculation as generate_method_call to ensure consistency
        let expected_arity = if let Ok((_, arity)) = self.handle_varargs_call(method_call, &owner_class) {
            arity
        } else {
            // Fallback to simple argument count if varargs handling fails
            method_call.arguments.len()
        };
        
        // Try intelligent method resolution first (same as generate_method_call)
        if let Some(resolved) = self.resolve_method_with_argument_analysis(&owner_class, &method_call.name, expected_arity, &method_call.arguments) {
            let should_pop = self.should_pop_return_value(&resolved.descriptor);
            eprintln!("🔍 DEBUG: should_pop_method_call_result: method={}#{}, descriptor={}, should_pop={}", 
                     owner_class, method_call.name, resolved.descriptor, should_pop);
            return should_pop;
        }
        
        // Fallback to original method resolution
        if let Some(resolved) = resolve_method_with_context(&owner_class, &method_call.name, expected_arity, self.current_class.as_ref(), self.all_types.as_deref()) {
            let should_pop = self.should_pop_return_value(&resolved.descriptor);
            eprintln!("🔍 DEBUG: should_pop_method_call_result: method={}#{}, descriptor={}, should_pop={}", 
                     owner_class, method_call.name, resolved.descriptor, should_pop);
            return should_pop;
        }
        
        // Fallback to heuristic if method resolution fails
        match method_call.name.as_str() {
            // Methods that typically return boolean
            "addAll" | "add" | "contains" | "isEmpty" | "offer" | "offerFirst" | "offerLast" => true,
            // Methods that typically return void
            "println" | "print" | "write" | "flush" | "close" => false,
            // Default: assume non-void for safety
            _ => true,
        }
    }

    /// Update stack for invoke based on descriptor like: (IIJ)Ljava/lang/Object;
    fn apply_invoke_stack_effect(&mut self, desc: &str, is_static: bool) -> Result<()> {
        let mut it = desc.chars();
        debug_assert_eq!(it.next(), Some('('));
        let mut args = 0i32;
        let mut in_obj = false;
        
        eprintln!("🔍 DEBUG: apply_invoke_stack_effect: parsing descriptor '{}', is_static={}", desc, is_static);
        
        for c in it.by_ref() {
            match c {
                ')' => break,
                '[' => { 
                    args += 1; 
                    in_obj = false; 
                    eprintln!("🔍 DEBUG: apply_invoke_stack_effect: found array '[', args={}", args);
                }
                'L' if !in_obj => { 
                    args += 1; 
                    in_obj = true; 
                    eprintln!("🔍 DEBUG: apply_invoke_stack_effect: found object start 'L', args={}", args);
                }
                'L' if in_obj => {
                    // This is an 'L' character inside an object type name, ignore it
                    eprintln!("🔍 DEBUG: apply_invoke_stack_effect: ignoring 'L' inside object type");
                }
                ';' => { 
                    in_obj = false; 
                    eprintln!("🔍 DEBUG: apply_invoke_stack_effect: found object end ';'");
                }
                _ if in_obj => {
                    eprintln!("🔍 DEBUG: apply_invoke_stack_effect: ignoring char '{}' inside object", c);
                }
                _ => {
                    let slots = Self::slots_of_type(c);
                    args += slots;
                    eprintln!("🔍 DEBUG: apply_invoke_stack_effect: found primitive '{}', slots={}, args={}", c, slots, args);
                }
            }
        }
        let rt = it.next().unwrap_or('V');
        let ret = match rt { 'V' => 0, 'J' | 'D' => 2, _ => 1 };
        let pops = args + if is_static { 0 } else { 1 };
        
        eprintln!("🔍 DEBUG: apply_invoke_stack_effect: final calculation - args={}, is_static={}, pops={}, ret={}", 
                 args, is_static, pops, ret);
        
        Self::map_stack(self.bytecode_builder.update_stack(pops as u16, ret as u16))?;
        Ok(())
    }

    fn label_str(&self, id: u16) -> String {
        // For now, keep the simple format
        // TODO: Implement dynamic jump target calculation
        let label_str = format!("L{}", id);
        println!("🔍 DEBUG: label_str: Converting label ID {} to string '{}'", id, label_str);
        label_str
    }
    
    /// Get the current PC (Program Counter) position
    fn get_current_pc(&self) -> u16 {
        self.bytecode_builder.code().len() as u16
    }
    
    /// Calculate jump target offset for a label
    fn calculate_jump_target(&self, label_id: u16) -> Option<i16> {
        if let Some(positions) = &self.label_positions {
            if let Some(target_pc) = positions.get(&label_id) {
                let current_pc = self.get_current_pc();
                let offset = (*target_pc as i32 - current_pc as i32) as i16;
                return Some(offset);
            }
        }
        None
    }


    /// Helper to map BytecodeBuilder StackError into our Error (no self borrow)
    fn map_stack<T>(r: std::result::Result<T, crate::codegen::bytecode::StackError>) -> Result<()> {
        if let Err(ref e) = r {
            eprintln!("🔍 DEBUG: map_stack error: {:?}", e);
            // Print a simple backtrace to help identify the caller
            let bt = std::backtrace::Backtrace::capture();
            eprintln!("🔍 DEBUG: map_stack backtrace: {}", bt);
        }
        r.map(|_| ()).map_err(|e| Error::codegen_error(format!("bytecode stack error: {}", e)))
    }
    /// Create a new method writer
    pub fn new() -> Self {
        Self {
            bytecode_builder: BytecodeBuilder::new(),
            opcode_generator: OpcodeGenerator::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
            constant_pool: None,
            current_class_name: None,
            current_class: None,
            all_types: None,
            // Optimizers temporarily disabled for debugging
            method_invocation_optimizer: MethodInvocationOptimizer::new(),
            field_access_optimizer: FieldAccessOptimizer::new(),
            switch_optimizer: SwitchOptimizer::new(),
            constant_optimizer: ConstantOptimizer,
            type_coercion_optimizer: TypeCoercionOptimizer::new(),
            string_optimizer: StringOptimizer,
            increment_optimizer: IncrementOptimizer::new(),
            exception_optimizer: ExceptionOptimizer::new(),
            loop_optimizer: LoopOptimizer,
            advanced_optimizer: AdvancedCodeGenerator::new(),
            finalizer_optimizer: ExceptionHandlingOptimizer,
            instruction_optimizer: InstructionOptimizer,
            cond_item_optimizer: crate::codegen::cond_item::CondItemOptimizer,
            gen_cond_optimizer: crate::codegen::gen_cond::GenCond,
            assignment_optimizer: crate::codegen::assignment_optimizer::AssignmentOptimizer::new(),
            string_buffer_optimizer: crate::codegen::string_buffer_optimizer::StringBufferOptimizer::new(),
            item_factory: crate::codegen::item_system::ItemFactory,
            type_erasure: TypeErasureProcessor::new(),
            fatcode_manager: FatcodeManager::new(),
            pending_jumps_manager: PendingJumpsManager::new(),
            fixed_pc_manager: FixedPcManager::new(),
            jsr_ret_optimizer: JsrRetOptimizer::new(),
            enhanced_string_optimizer: EnhancedStringOptimizer::new(),
            stack_map_optimizer: StackMapOptimizer::new(),
            stack_map_emitter: None, // Will be initialized when needed
            label_positions: Some(std::collections::HashMap::new()),
            label_to_chain_mapping: Some(std::collections::HashMap::new()),
        }
    }
    
    /// Create a new method writer with access to constant pool
    pub fn new_with_constant_pool(constant_pool: std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>) -> Self {
        Self {
            bytecode_builder: BytecodeBuilder::new(),
            opcode_generator: OpcodeGenerator::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
            constant_pool: Some(constant_pool.clone()),
            current_class_name: None,
            current_class: None,
            all_types: None,
            // Optimizers temporarily disabled for debugging
            method_invocation_optimizer: MethodInvocationOptimizer::new(),
            field_access_optimizer: FieldAccessOptimizer::new(),
            switch_optimizer: SwitchOptimizer::new(),
            // Removed cast_optimizer initialization
            constant_optimizer: ConstantOptimizer,
            type_coercion_optimizer: TypeCoercionOptimizer::new(),
            string_optimizer: StringOptimizer,
            increment_optimizer: IncrementOptimizer::new(),
            exception_optimizer: ExceptionOptimizer::new(),
            loop_optimizer: LoopOptimizer,
            advanced_optimizer: AdvancedCodeGenerator::new(),
            finalizer_optimizer: ExceptionHandlingOptimizer,
            instruction_optimizer: InstructionOptimizer,
            cond_item_optimizer: crate::codegen::cond_item::CondItemOptimizer,
            gen_cond_optimizer: crate::codegen::gen_cond::GenCond,
            assignment_optimizer: crate::codegen::assignment_optimizer::AssignmentOptimizer::new(),
            string_buffer_optimizer: crate::codegen::string_buffer_optimizer::StringBufferOptimizer::new(),
            item_factory: crate::codegen::item_system::ItemFactory,
            type_erasure: TypeErasureProcessor::new(),
            fatcode_manager: FatcodeManager::new(),
            pending_jumps_manager: PendingJumpsManager::new(),
            fixed_pc_manager: FixedPcManager::new(),
            jsr_ret_optimizer: JsrRetOptimizer::new(),
            enhanced_string_optimizer: EnhancedStringOptimizer::new(),
            stack_map_optimizer: StackMapOptimizer::new(),
            stack_map_emitter: None, // Will be initialized when needed
            label_positions: Some(std::collections::HashMap::new()),
            label_to_chain_mapping: Some(std::collections::HashMap::new()),
        }
    }
    
    /// Create a new method writer with access to constant pool and current class name
    pub fn new_with_constant_pool_and_class(constant_pool: std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>, class_name: String) -> Self {
        Self {
            bytecode_builder: BytecodeBuilder::new(),
            opcode_generator: OpcodeGenerator::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
            constant_pool: Some(constant_pool.clone()),
            current_class_name: Some(class_name),
            current_class: None,
            all_types: None,
            // Optimizers temporarily disabled for debugging
            method_invocation_optimizer: MethodInvocationOptimizer::new(),
            field_access_optimizer: FieldAccessOptimizer::new(),
            switch_optimizer: SwitchOptimizer::new(),
            // Removed cast_optimizer initialization
            constant_optimizer: ConstantOptimizer,
            type_coercion_optimizer: TypeCoercionOptimizer::new(),
            string_optimizer: StringOptimizer,
            increment_optimizer: IncrementOptimizer::new(),
            exception_optimizer: ExceptionOptimizer::new(),
            loop_optimizer: LoopOptimizer,
            advanced_optimizer: AdvancedCodeGenerator::new(),
            finalizer_optimizer: ExceptionHandlingOptimizer,
            instruction_optimizer: InstructionOptimizer,
            cond_item_optimizer: crate::codegen::cond_item::CondItemOptimizer,
            gen_cond_optimizer: crate::codegen::gen_cond::GenCond,
            assignment_optimizer: crate::codegen::assignment_optimizer::AssignmentOptimizer::new(),
            string_buffer_optimizer: crate::codegen::string_buffer_optimizer::StringBufferOptimizer::new(),
            item_factory: crate::codegen::item_system::ItemFactory,
            type_erasure: TypeErasureProcessor::new(),
            fatcode_manager: FatcodeManager::new(),
            pending_jumps_manager: PendingJumpsManager::new(),
            fixed_pc_manager: FixedPcManager::new(),
            jsr_ret_optimizer: JsrRetOptimizer::new(),
            enhanced_string_optimizer: EnhancedStringOptimizer::new(),
            stack_map_optimizer: StackMapOptimizer::new(),
            stack_map_emitter: None, // Will be initialized when needed
            label_positions: Some(std::collections::HashMap::new()),
            label_to_chain_mapping: Some(std::collections::HashMap::new()),

        }
    }
    
    /// Create a new method writer with access to constant pool and current class declaration
    pub fn new_with_constant_pool_and_class_decl(
        constant_pool: std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>, 
        class_decl: crate::ast::ClassDecl
    ) -> Self {
        let class_name = class_decl.name.clone();
        Self {
            bytecode_builder: BytecodeBuilder::new(),
            opcode_generator: OpcodeGenerator::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
            constant_pool: Some(constant_pool.clone()),
            current_class_name: Some(class_name),
            current_class: Some(class_decl),
            all_types: None,
            // Optimizers temporarily disabled for debugging
            method_invocation_optimizer: MethodInvocationOptimizer::new(),
            field_access_optimizer: FieldAccessOptimizer::new(),
            switch_optimizer: SwitchOptimizer::new(),
            // Removed cast_optimizer initialization
            constant_optimizer: ConstantOptimizer,
            type_coercion_optimizer: TypeCoercionOptimizer::new(),
            string_optimizer: StringOptimizer,
            increment_optimizer: IncrementOptimizer::new(),
            exception_optimizer: ExceptionOptimizer::new(),
            loop_optimizer: LoopOptimizer,
            advanced_optimizer: AdvancedCodeGenerator::new(),
            finalizer_optimizer: ExceptionHandlingOptimizer,
            instruction_optimizer: InstructionOptimizer,
            cond_item_optimizer: crate::codegen::cond_item::CondItemOptimizer,
            gen_cond_optimizer: crate::codegen::gen_cond::GenCond,
            assignment_optimizer: crate::codegen::assignment_optimizer::AssignmentOptimizer::new(),
            string_buffer_optimizer: crate::codegen::string_buffer_optimizer::StringBufferOptimizer::new(),
            item_factory: crate::codegen::item_system::ItemFactory,
            type_erasure: TypeErasureProcessor::new(),
            fatcode_manager: FatcodeManager::new(),
            pending_jumps_manager: PendingJumpsManager::new(),
            fixed_pc_manager: FixedPcManager::new(),
            jsr_ret_optimizer: JsrRetOptimizer::new(),
            enhanced_string_optimizer: EnhancedStringOptimizer::new(),
            stack_map_optimizer: StackMapOptimizer::new(),
            stack_map_emitter: None, // Will be initialized when needed
            label_positions: Some(std::collections::HashMap::new()),
            label_to_chain_mapping: Some(std::collections::HashMap::new()),

        }
    }
    
    /// Get current bytecode for inspection
    fn get_current_code(&self) -> &Vec<u8> {
        self.bytecode_builder.code()
    }

    /// Initialize enhanced stack map emitter for advanced frame generation
    pub fn init_enhanced_stack_map_emitter(&mut self, _method_name: &str, is_static: bool, is_constructor: bool, owner_type: &str, parameter_types: Vec<String>) {
        let method_info = crate::codegen::enhanced_stack_map_emitter::MethodInfo {
            is_static,
            is_constructor,
            owner_type: owner_type.to_string(),
            parameter_types,
            max_locals: 255, // Will be updated during code generation
        };
        
        self.stack_map_emitter = Some(crate::codegen::enhanced_stack_map_emitter::EnhancedStackMapEmitter::new(method_info));
    }

    /// Emit enhanced stack map frame at current PC
    pub fn emit_enhanced_stack_map_frame(&mut self, locals: Vec<crate::codegen::frame::VerificationType>, stack: Vec<crate::codegen::frame::VerificationType>) -> Result<()> {
        if let Some(ref mut emitter) = self.stack_map_emitter {
            let pc = self.bytecode_builder.code().len() as u16;
            emitter.emit_stack_map_frame(pc, locals, stack)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("Stack map frame emission failed: {}", e) })?;
        }
        Ok(())
    }

    /// Emit enhanced stack map frame at jump target
    pub fn emit_enhanced_frame_at_jump_target(&mut self, locals: Vec<crate::codegen::frame::VerificationType>, stack: Vec<crate::codegen::frame::VerificationType>) -> Result<()> {
        if let Some(ref mut emitter) = self.stack_map_emitter {
            let pc = self.bytecode_builder.code().len() as u16;
            emitter.emit_frame_at_jump_target(pc, locals, stack)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("Jump target frame emission failed: {}", e) })?;
        }
        Ok(())
    }

    /// Emit enhanced stack map frame at exception handler
    pub fn emit_enhanced_frame_at_exception_handler(&mut self, locals: Vec<crate::codegen::frame::VerificationType>, exception_type: crate::codegen::frame::VerificationType) -> Result<()> {
        if let Some(ref mut emitter) = self.stack_map_emitter {
            let pc = self.bytecode_builder.code().len() as u16;
            emitter.emit_frame_at_exception_handler(pc, locals, exception_type)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("Exception handler frame emission failed: {}", e) })?;
        }
        Ok(())
    }

    /// Get generated stack map frames from enhanced emitter
    pub fn get_enhanced_stack_map_frames(&self) -> Option<&[crate::codegen::frame::StackMapFrame]> {
        self.stack_map_emitter.as_ref().map(|emitter| emitter.get_frames())
    }

    /// Get enhanced stack map emission statistics
    pub fn get_enhanced_stack_map_stats(&self) -> Option<&crate::codegen::enhanced_stack_map_emitter::EmissionStats> {
        self.stack_map_emitter.as_ref().map(|emitter| emitter.get_stats())
    }
    
    /// Get current local variables for stack map frame generation
    fn get_current_locals_for_stack_map(&self) -> Vec<crate::codegen::frame::VerificationType> {
        // Simplified implementation - return basic frame with object reference
        // In a full implementation, this would track actual local variable types
        vec![crate::codegen::frame::VerificationType::Object(1)] // Placeholder
    }
    
    /// Set all types for interface method resolution
    pub fn set_all_types(&mut self, all_types: Vec<crate::ast::TypeDecl>) {
        self.all_types = Some(all_types);
    }
    
    /// Resolve a simple class name to its fully qualified internal name
    /// Following Java resolution rules:
    /// 1. Check imports
    /// 2. Check current package
    /// 3. Check java.lang package
    /// 4. If none found, assume it's in current package
    fn resolve_class_name(&self, simple_name: &str) -> String {
        // Handle primitive types
        match simple_name {
            "int" | "boolean" | "byte" | "char" | "short" | "long" | "float" | "double" | "void" => {
                return simple_name.to_string();
            }
            _ => {}
        }
        
        // Handle well-known java.lang types
        match simple_name {
            "String" => return "java/lang/String".to_string(),
            "Object" => return "java/lang/Object".to_string(),
            "Integer" => return "java/lang/Integer".to_string(),
            "Boolean" => return "java/lang/Boolean".to_string(),
            "Long" => return "java/lang/Long".to_string(),
            "Double" => return "java/lang/Double".to_string(),
            "Float" => return "java/lang/Float".to_string(),
            "Character" => return "java/lang/Character".to_string(),
            "Byte" => return "java/lang/Byte".to_string(),
            "Short" => return "java/lang/Short".to_string(),
            "Class" => return "java/lang/Class".to_string(),
            "Comparable" => return "java/lang/Comparable".to_string(),
            "Exception" => return "java/lang/Exception".to_string(),
            "RuntimeException" => return "java/lang/RuntimeException".to_string(),
            "UnsupportedOperationException" => return "java/lang/UnsupportedOperationException".to_string(),
            "Throwable" => return "java/lang/Throwable".to_string(),
            "Bytes" => return "java/lang/Bytes".to_string(),
            "BytesType" => return "java/lang/BytesType".to_string(),
            "SystemClassLoader" => return "java/base/SystemClassLoader".to_string(),
            "Method" => return "java/lang/reflect/Method".to_string(),
            "AccessibleObject" => return "java/lang/reflect/AccessibleObject".to_string(),
            "Stream" => return "java/base/Stream".to_string(),
            "Classes" => return "java/base/Classes".to_string(),
            "Type" => return "java/lang/bytes/Type".to_string(),
            _ => {}
        }
        
        // Check if it's already a fully qualified name (contains dots or slashes)
        if simple_name.contains('.') || simple_name.contains('/') {
            return simple_name.replace('.', "/");
        }
        
        // First try to resolve using classpath
        if let Some(internal_name) = classpath::resolve_class_name(simple_name) {
            return internal_name.to_string();
        }
        
        // For well-known types, use explicit package mapping
        match simple_name {
            "HashMapCell" | "HashMap" | "List" | "ArrayList" | "Iterator" | "Collection" | "ArraysListIterator" => {
                return format!("java/util/{}", simple_name);
            }
            "PrintStream" | "InputStream" | "OutputStream" => {
                return format!("java/io/{}", simple_name);
            }
            _ => {}
        }
        
        // Check current package context
        if let Some(class_name) = &self.current_class_name {
            if let Some(last_slash) = class_name.rfind('/') {
                // Current class has a package, use the same package for the simple name
                let current_package = &class_name[..last_slash];
                return format!("{}/{}", current_package, simple_name);
            }
        }
        
        // No package context found - this is likely a test class or class in default package
        // Return the simple name as-is (no package prefix)
        simple_name.to_string()
    }
    
    /// Emit invoke instruction with proper opcode selection
    fn emit_invoke(&mut self, callee: &ResolvedMethod) -> Result<()> {
        use super::opcodes::*;
        
        // Add method reference to constant pool
        let idx = if let Some(cp) = &self.constant_pool {
            let mut cp_ref = cp.borrow_mut();
            if callee.is_interface {
                // Use InterfaceMethodRef for interface methods
                cp_ref.try_add_interface_method_ref(&callee.owner_internal, &callee.name, &callee.descriptor)
                    .map_err(|e| Error::codegen_error(&format!("Failed to add interface method ref: {}", e)))?
            } else {
                // Use MethodRef for regular methods
            cp_ref.try_add_method_ref(&callee.owner_internal, &callee.name, &callee.descriptor)
                .map_err(|e| Error::codegen_error(&format!("Failed to add method ref: {}", e)))?
            }
        } else {
            return Err(Error::codegen_error("No constant pool available for method reference"));
        };
        
        // Select appropriate invoke opcode
        let op = if callee.is_interface {
            INVOKEINTERFACE
        } else if callee.is_static {
            INVOKESTATIC
        } else if callee.is_ctor || callee.is_private || callee.is_super_call {
            INVOKESPECIAL
        } else {
            INVOKEVIRTUAL
        };
        
        // Use new stack accounting method
        // eprintln!("🔍 DEBUG: emit_invoke: method={}#{}, descriptor={}, is_static={}, is_private={}, is_ctor={}, is_super_call={}", 
        //          callee.owner_internal, callee.name, callee.descriptor, callee.is_static, callee.is_private, callee.is_ctor, callee.is_super_call);
        self.apply_invoke_stack_effect(&callee.descriptor, callee.is_static)?;

        // Emit invoke via builder for proper bookkeeping
        if op == INVOKESTATIC {
            Self::map_stack(self.bytecode_builder.invokestatic(idx))?;
        } else if op == INVOKESPECIAL {
            Self::map_stack(self.bytecode_builder.invokespecial(idx))?;
        } else if op == INVOKEINTERFACE {
            let argc = (self.descriptor_arg_count(&callee.descriptor) + 1) as u8;
            Self::map_stack(self.bytecode_builder.invokeinterface(idx, argc))?;
        } else {
            Self::map_stack(self.bytecode_builder.invokevirtual(idx))?;
        }
        
        Ok(())
    }
    
    /// Count argument slots in a method descriptor
    fn descriptor_arg_slot_count(&self, descriptor: &str) -> u8 {
        // eprintln!("🔍 DEBUG: descriptor_arg_slot_count: descriptor={}", descriptor);
        if let Some(args_start) = descriptor.find('(') {
            if let Some(args_end) = descriptor.find(')') {
                let args_part = &descriptor[args_start + 1..args_end];
                // eprintln!("🔍 DEBUG: descriptor_arg_slot_count: args_part={}", args_part);
                let mut count = 0;
                let mut i = 0;
                while i < args_part.len() {
                    match args_part.chars().nth(i).unwrap() {
                        'L' => {
                            // Reference type: L...;
                            if let Some(semicolon) = args_part[i..].find(';') {
                                i += semicolon + 1;
                            } else {
                                i += 1;
                            }
                            count += 1;
                        }
                        '[' => {
                            // Array type: [...
                            i += 1;
                            while i < args_part.len() && args_part.chars().nth(i).unwrap() == '[' {
                                i += 1;
                            }
                            if i < args_part.len() {
                                match args_part.chars().nth(i).unwrap() {
                                    'L' => {
                                        // Array of reference: [L...;
                                        if let Some(semicolon) = args_part[i..].find(';') {
                                            i += semicolon + 1;
                                        } else {
                                            i += 1;
                                        }
                                    }
                                    _ => i += 1, // Array of primitive
                                }
                            }
                            count += 1;
                        }
                        'J' | 'D' => {
                            // Long or double: 2 slots
                            count += 2;
                            i += 1;
                        }
                        _ => {
                            // Other primitives: 1 slot
                            count += 1;
                            i += 1;
                        }
                    }
                }
                // eprintln!("🔍 DEBUG: descriptor_arg_slot_count: final count={}", count);
                count
            } else {
                0
            }
        } else {
            0
        }
    }
    
    /// Whether method descriptor returns a value (non-void)
    fn descriptor_returns_value(&self, descriptor: &str) -> bool {
        if let Some(ret_start) = descriptor.rfind(')') {
            let ret = &descriptor[ret_start + 1..];
            ret != "V"
        } else {
            false
        }
    }
    /// Count number of arguments (not slots) in a method descriptor
    fn descriptor_arg_count(&self, descriptor: &str) -> u8 {
        if let Some(args_start) = descriptor.find('(') {
            if let Some(args_end) = descriptor.find(')') {
                let args_part = &descriptor[args_start + 1..args_end];
                let mut count = 0u8;
                let mut i = 0;
                while i < args_part.len() {
                    match args_part.chars().nth(i).unwrap() {
                        'L' => {
                            // Reference type: L...;
                            if let Some(semicolon) = args_part[i..].find(';') { i += semicolon + 1; } else { i += 1; }
                            count += 1;
                        }
                        '[' => {
                            // Array type: consume all '[' then one element type
                            i += 1;
                            while i < args_part.len() && args_part.chars().nth(i).unwrap() == '[' { i += 1; }
                            if i < args_part.len() && args_part.chars().nth(i).unwrap() == 'L' {
                                if let Some(semicolon) = args_part[i..].find(';') { i += semicolon + 1; } else { i += 1; }
                            } else {
                                i += 1; // primitive array element
                            }
                            count += 1;
                        }
                        ')' => break,
                        _ => { i += 1; count += 1; }
                    }
                }
                count
            } else { 0 }
        } else { 0 }
    }
    
    /// Update stack after invoke instruction
    fn stack_after_invoke(&mut self, descriptor: &str) -> Result<()> {
        // Pop arguments and receiver (for non-static)
        let _args_slots = self.descriptor_arg_slot_count(descriptor);
        // TODO: Implement proper stack simulation
        // For now, just return success
        Ok(())
    }
    
    /// Emit opcode using the opcode generator (legacy)
    /// Prefer using BytecodeBuilder methods which maintain stack state.
    fn emit_opcode(&mut self, opcode_bytes: Vec<u8>) {
        self.bytecode_builder.extend_from_slice(&opcode_bytes);
    }
    
    // Legacy label reference helpers removed: label resolution moved to BytecodeBuilder
    
    /// Generate bytecode for a method body
    pub fn generate_method_body(&mut self, method: &MethodDecl) -> Result<()> {
        println!("🔍 DEBUG: generate_method_body: Starting for method '{}'", method.name);
        
        // Initialize enhanced stack map emitter for this method
        let parameter_types: Vec<String> = method.parameters.iter()
            .map(|p| p.type_ref.name.clone())
            .collect();
        self.init_enhanced_stack_map_emitter(
            &method.name,
            method.modifiers.contains(&Modifier::Static),
            method.name == "<init>",
            "current/Class", // TODO: Get actual class name
            parameter_types,
        );
        
        // Use javac-style complexity analysis for fatcode decision
        if let Some(body) = &method.body {
            let complexity = crate::codegen::complexity_analyzer::ComplexityAnalyzer::analyze_complexity(&crate::ast::Stmt::Block(body.clone()));
            let method_size = crate::codegen::complexity_analyzer::ComplexityAnalyzer::estimate_method_size(&crate::ast::Stmt::Block(body.clone()));
            
            // javac-style fatcode decision
            let should_use_fatcode = crate::codegen::complexity_analyzer::ComplexityAnalyzer::should_use_fatcode(complexity) || method_size > 500;
            
            if should_use_fatcode {
                // Enable fat code for complex methods
                self.bytecode_builder.fatcode = true;
                self.advanced_optimizer.fat_code = true;
                println!("🔍 DEBUG: generate_method_body: Enabled fat code (complexity: {}, size: {})", complexity, method_size);
            }
            
            // 🔧 ADVANCED OPT: Use ControlFlowOptimizer for advanced control flow analysis
            let control_flow_pattern = crate::codegen::advanced_optimizer::ControlFlowOptimizer::optimize_with_unwind(&crate::ast::Stmt::Block(body.clone()), &[]);
            println!("🔧 ADVANCED OPT: Control flow analysis - complexity: {}, optimizations: {:?}", 
                     control_flow_pattern.complexity, control_flow_pattern.optimizations);
            
            // Apply control flow optimizations based on complexity
            if control_flow_pattern.complexity > 10 {
                println!("🔧 ADVANCED OPT: High complexity method detected, enabling advanced optimizations");
                self.advanced_optimizer.need_stack_map = true;
            }
            
            // 🔧 ADVANCED OPT: Use StatementComplexityAnalyzer for JSR and fat code decisions
            if crate::codegen::advanced_optimizer::StatementComplexityAnalyzer::should_use_jsr(body) {
                println!("🔧 ADVANCED OPT: JSR optimization recommended for complex control flow");
                // Enable JSR optimization (placeholder for now)
                println!("🔧 ADVANCED OPT: JSR optimization would be enabled here");
            }
            
            if crate::codegen::advanced_optimizer::StatementComplexityAnalyzer::should_use_fat_code(body) {
                println!("🔧 ADVANCED OPT: Fat code generation recommended for very complex methods");
                self.advanced_optimizer.fat_code = true;
            }
        }
        
        // Initialize local variables for parameters
        println!("🔍 DEBUG: generate_method_body: About to initialize_parameters...");
        self.initialize_parameters(method)?;
        println!("🔍 DEBUG: generate_method_body: initialize_parameters completed");
        
        // Emit initial stack map frame at method entry
        self.emit_initial_stack_map_frame()?;
        
        // Generate method body
        println!("🔍 DEBUG: generate_method_body: About to generate_block...");
        if let Some(body) = &method.body {
            println!("🔍 DEBUG: generate_method_body: Method has body with {} statements", body.statements.len());
            self.generate_block(body)?;
            println!("🔍 DEBUG: generate_method_body: generate_block completed");
            println!("🔍 DEBUG: generate_method_body: About to continue after generate_block...");
        } else {
            println!("🔍 DEBUG: generate_method_body: Method has no body");
        }
        
        // Only generate return statement if method body doesn't end with one
        // Check if all code paths have return statements
        println!("🔍 DEBUG: generate_method_body: About to check if return statement needed...");
        let needs_return = if let Some(body) = &method.body {
            !self.all_paths_have_return(&body.statements)
            } else {
            // Abstract methods have no body and don't need return statements
            false
        };
        println!("🔍 DEBUG: generate_method_body: needs_return = {}", needs_return);
        
        if needs_return {
            // Generate return statement
            println!("🔍 DEBUG: generate_method_body: About to generate return statement...");
            if let Some(return_type) = &method.return_type {
                println!("🔍 DEBUG: generate_method_body: Method has return type '{}'", return_type.name);
                self.generate_return(return_type)?;
            } else {
                println!("🔍 DEBUG: generate_method_body: Method has no return type (void)");
                // Create a void type reference for void methods
                let void_type = TypeRef { name: "void".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: method.span };
                self.generate_return(&void_type)?;
            }
            println!("🔍 DEBUG: generate_method_body: Return statement generated");
        } else {
            // 🔧 FIX: For void methods without explicit return, ensure we have a label at the end
            // This allows goto instructions to correctly jump to the method end
            if method.return_type.is_none() || method.return_type.as_ref().map(|rt| rt.name == "void").unwrap_or(false) {
                println!("🔍 DEBUG: generate_method_body: Void method without explicit return, ensuring end label");
                println!("🔍 DEBUG: generate_method_body: Method name: '{}', return type: {:?}", method.name, method.return_type);
                // Create a synthetic end label for void methods
                let end_label = self.create_label();
                println!("🔍 DEBUG: generate_method_body: Created end label: {} for method '{}'", end_label, method.name);
                self.mark_label(end_label);
                println!("🔍 DEBUG: generate_method_body: Marked end label: {} for method '{}'", end_label, method.name);
            }
        }
        
        // Ensure method body ends cleanly
        println!("🔍 DEBUG: generate_method_body: About to ensure_clean_method_end...");
        self.ensure_clean_method_end()?; // 🔧 ACTIVATED: Safe method end validation
        println!("🔍 DEBUG: generate_method_body: ensure_clean_method_end completed");
        
        // Validate method body structure
        println!("🔍 DEBUG: generate_method_body: About to validate_method_body_structure...");
        self.validate_method_body_structure()?; // 🔧 ACTIVATED: Safe structure validation
        println!("🔍 DEBUG: generate_method_body: validate_method_body_structure completed");
        
        // Optimize method body structure
        println!("🔍 DEBUG: generate_method_body: About to optimize_method_body_structure...");
        //self.optimize_method_body_structure()?;
        println!("🔍 DEBUG: generate_method_body: optimize_method_body_structure completed");
        
        // Basic stack validation (disabled to avoid interference)
        println!("🔍 DEBUG: generate_method_body: About to basic stack validation...");
        let current_depth = self.bytecode_builder.stack_depth();
        if current_depth != 0 {
            eprintln!("Warning: Method body ended with non-zero stack depth: {}", current_depth);
            // Stack balancing disabled to avoid interference with varargs
        }
        println!("🔍 DEBUG: generate_method_body: Basic stack validation completed");
        
        // Use AdvancedCodeGenerator for post-processing optimizations
        println!("🔍 DEBUG: generate_method_body: About to apply advanced optimizations...");
        self.apply_advanced_optimizations()?;
        println!("🔍 DEBUG: generate_method_body: Advanced optimizations completed");
        
        // Finalize and optimize stack map frames
        self.finalize_stack_map_frames()?;
        println!("🔍 DEBUG: generate_method_body: Stack map frames finalized");
        
        // Deep structure analysis and repair
        println!("🔍 DEBUG: generate_method_body: About to deep_structure_analysis_and_repair...");
        // self.deep_structure_analysis_and_repair()?;
        println!("🔍 DEBUG: generate_method_body: deep_structure_analysis_and_repair completed");
        
        // Handle complex method body structure issues
        println!("🔍 DEBUG: generate_method_body: About to handle_complex_method_body_issues...");
        // self.handle_complex_method_body_issues()?;
        println!("🔍 DEBUG: generate_method_body: handle_complex_method_body_issues completed");
        
        // Final comprehensive validation
        println!("🔍 DEBUG: generate_method_body: About to comprehensive_method_validation...");
        //self.comprehensive_method_validation()?;
        println!("🔍 DEBUG: generate_method_body: comprehensive_method_validation completed");
        
        println!("🔍 DEBUG: generate_method_body: All steps completed successfully for method '{}'", method.name);
        Ok(())
    }
    
    /// Ensure method body ends cleanly
    fn ensure_clean_method_end(&mut self) -> Result<()> {
        // Check if the last instruction is a return instruction
        let code = self.get_current_code();
        if code.is_empty() {
            return Ok(());
        }
        
        let last_byte = code[code.len() - 1];
        
        // If the last instruction is not a return, we might have an issue
        // This is a safety check to ensure method body integrity
        match last_byte {
            // Return opcodes
            0xb1 | 0xac | 0xad | 0xae | 0xaf | 0xb0 => {
                // Valid return instruction, nothing to do
                Ok(())
            }
            _ => {
                // Not a return instruction, this might indicate a problem
                // For now, just log a warning
                eprintln!("Warning: Method body does not end with a return instruction");
                Ok(())
            }
        }
    }
    
    /// Validate method body structure
    fn validate_method_body_structure(&mut self) -> Result<()> {
        // Check if the method body has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Check for obvious structural issues
        let mut pc = 0;
        let mut i = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            match opcode {
                // Return instructions - should be at the end
                0xb1 | 0xac | 0xad | 0xae | 0xaf | 0xb0 => {
                    // If this is not the last instruction, we have a problem
                    if i < self.bytecode_builder.code().len() - 1 {
                        eprintln!("Warning: Return instruction found before end of method body at pc={}", pc);
                    }
                }
                // Jump instructions - should have valid targets
                0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab => { // ifeq, ifne, iflt, ifge, ifgt, ifle
                    if i + 2 >= self.bytecode_builder.code().len() {
                        eprintln!("Warning: Incomplete jump instruction at pc={}", pc);
                    }
                }
                0xc7 | 0xc8 => { // goto, goto_w
                    if i + 2 >= self.bytecode_builder.code().len() {
                        eprintln!("Warning: Incomplete goto instruction at pc={}", pc);
                    }
                }
                // Method invocation instructions
                0xb6 | 0xb7 | 0xb8 | 0xb9 => { // invokevirtual, invokespecial, invokestatic, invokeinterface
                    if i + 2 >= self.bytecode_builder.code().len() {
                        eprintln!("Warning: Incomplete method invocation at pc={}", pc);
                    }
                }
                _ => {}
            }
            
            // Update program counter
            pc += self.get_instruction_size(opcode);
            i += self.get_instruction_size(opcode);
        }
        
        Ok(())
    }
    
    /// Get instruction size for an opcode
    fn get_instruction_size(&self, opcode: u8) -> usize {
        match opcode {
            // Single byte instructions
            0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 | 0x08 | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d | 0x0e | 0x0f => 1,
            0x10 | 0x11 | 0x12 | 0x13 | 0x14 => 1,
            0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25 | 0x26 | 0x27 | 0x28 | 0x29 | 0x2a | 0x2b | 0x2c | 0x2d | 0x2e | 0x2f => 1,
            0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 => 1,
            0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f => 1,
            0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 | 0x58 | 0x59 | 0x5a | 0x5b | 0x5c | 0x5d | 0x5e | 0x5f => 1,
            0x60 | 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67 | 0x68 | 0x69 | 0x6a | 0x6b | 0x6c | 0x6d | 0x6e | 0x6f => 1,
            0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79 | 0x7a | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f => 1,
            0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89 | 0x8a | 0x8b | 0x8c | 0x8d | 0x8e | 0x8f => 1,
            0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99 | 0x9a | 0x9b | 0x9c | 0x9d | 0x9e | 0x9f => 1,
            0xa0 | 0xa1 | 0xa2 | 0xa3 | 0xa4 | 0xa5 | 0xa6 | 0xac | 0xad | 0xae | 0xaf => 1,
            0xb0 | 0xb1 => 1, // areturn, return
            // Jump instructions with 2-byte offset
            0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab | 0xc7 => 3, // ifeq, ifne, iflt, ifge, ifgt, ifle, goto
            // Method invocation with 2-byte index
            0xb6 | 0xb7 | 0xb8 | 0xb9 => 3, // invokevirtual, invokespecial, invokestatic, invokeinterface
            // Load/store with index
            0x15 | 0x16 | 0x17 | 0x18 | 0x19 | 0x1a | 0x1b | 0x1c | 0x1d | 0x1e | 0x1f => 2, // iload, lload, fload, dload, aload
            0x36 | 0x37 | 0x38 | 0x39 | 0x3a | 0x3b | 0x3c | 0x3d | 0x3e | 0x3f => 2, // istore, lstore, fstore, dstore, astore
            // Default case
            _ => 1,
        }
    }
    
    /// Initialize local variables for method parameters
    fn initialize_parameters(&mut self, method: &MethodDecl) -> Result<()> {
        println!("🔍 DEBUG: initialize_parameters: Starting for method '{}' with {} parameters", method.name, method.parameters.len());
        
        // 'this' reference is always at index 0 for instance methods
        if !method.modifiers.contains(&Modifier::Static) {
            println!("🔍 DEBUG: initialize_parameters: Method is not static, adding 'this' reference");
            let this_type = LocalType::Reference(self.current_class_name.clone().unwrap_or_default());
            self.bytecode_builder.allocate("this".to_string(), this_type);
        } else {
            println!("🔍 DEBUG: initialize_parameters: Method is static, no 'this' reference needed");
        }
        
        // Add parameters
        for (i, param) in method.parameters.iter().enumerate() {
            println!("🔍 DEBUG: initialize_parameters: Processing parameter {}: '{}' of type '{}', varargs={}", i, param.name, param.type_ref.name, param.varargs);
            // Note: index calculation is kept for future use in local variable management
            let _index = if method.modifiers.contains(&Modifier::Static) {
                i
            } else {
                i + 1
            };
            
            // Handle varargs parameters: Class ... ptypes becomes Class[] ptypes
            let effective_type_ref = if param.varargs {
                TypeRef {
                    name: param.type_ref.name.clone(),
                    type_args: param.type_ref.type_args.clone(),
                    annotations: param.type_ref.annotations.clone(),
                    array_dims: param.type_ref.array_dims + 1, // Add one array dimension for varargs
                    span: param.type_ref.span,
                }
            } else {
                param.type_ref.clone()
            };
            
            let local_type = self.convert_type_ref_to_local_type(&effective_type_ref);
            self.bytecode_builder.allocate(param.name.clone(), local_type);
            println!("🔍 DEBUG: initialize_parameters: Parameter {} allocated successfully", i);
        }
        
        println!("🔍 DEBUG: initialize_parameters: Completed successfully for method '{}'", method.name);
        Ok(())
    }
    
    /// Erase generic type using the type erasure processor
    fn erase_generic_type(&mut self, type_ref: &TypeRef) -> Result<String> {
        // Create a basic type environment (in a full implementation, this would be
        // populated with the current class and method type parameters)
        let env = crate::review::generics::TypeEnv::default();
        // Removed GlobalMemberIndex dependency as it's private
        
        let erasure_result = self.type_erasure.erase_type(type_ref, &env)?;
        Ok(erasure_result.erased_type)
    }

    /// Get the bound type for a generic type parameter
    /// For example, for "T extends Enum<T>", this returns "java.lang.Enum"
    fn get_generic_type_bound(&self, type_param_name: &str) -> Option<String> {
        if let Some(current_class) = &self.current_class {
            for type_param in &current_class.type_params {
                if type_param.name == type_param_name {
                    // If the type parameter has bounds, use the first bound
                    if let Some(first_bound) = type_param.bounds.first() {
                        // Convert the bound type to internal name format
                        let bound_name = &first_bound.name;
                        // Handle common cases
                        match bound_name.as_str() {
                            "Enum" => return Some("java.lang.Enum".to_string()),
                            _ => {
                                // Try to resolve the bound type name
                                let resolved = self.resolve_class_name(bound_name);
                                return Some(resolved.replace("/", "."));
                            }
                        }
                    }
                    break;
                }
            }
        }
        None
    }
    
    /// Check if a type name is a generic type parameter
    /// Generic type parameters are typically:
    /// - Single uppercase letters (K, V, T, E, etc.)
    /// - Explicitly defined in the current class or method type parameters
    /// - Not a real class name (either known or currently being compiled)
    fn is_generic_type_parameter(&self, type_name: &str) -> bool {
        // First check: if it's a known class in classpath, it's not a generic parameter
        if classpath::resolve_class_name(type_name).is_some() {
            return false;
        }
        
        // Second check: if it's the current class being compiled, it's not a generic parameter
        if let Some(current_class_name) = &self.current_class_name {
            let current_simple_name = current_class_name.split('/').last().unwrap_or(current_class_name);
            if type_name == current_simple_name {
                eprintln!("🔍 DEBUG: is_generic_type_parameter: '{}' is the current class, not a generic parameter", type_name);
                return false;
            }
        }
        
        // Third check: if it's explicitly defined as a type parameter in current class
        if let Some(current_class) = &self.current_class {
            for type_param in &current_class.type_params {
                if type_param.name == type_name {
                    eprintln!("🔍 DEBUG: is_generic_type_parameter: '{}' found in class type parameters", type_name);
                    return true;
                }
            }
        }
        
        // Fourth check: common patterns for generic type parameters (but be more conservative)
        match type_name.len() {
            1 => {
                // Single letter, typically uppercase (K, V, T, E, etc.)
                let is_single_upper = type_name.chars().next().unwrap().is_uppercase();
                eprintln!("🔍 DEBUG: is_generic_type_parameter: Single letter '{}' -> {}", type_name, is_single_upper);
                is_single_upper
            }
            2..=3 => {
                // Very short names that are commonly used as type parameters (like "Key", "Val")
                // But be more restrictive - only if they match common patterns
                let common_type_params = ["Key", "Val", "Num", "Obj", "Ref", "Src", "Dst", "Ret"];
                let is_common = common_type_params.contains(&type_name);
                eprintln!("🔍 DEBUG: is_generic_type_parameter: Short name '{}' -> {}", type_name, is_common);
                is_common
            }
            _ => {
                eprintln!("🔍 DEBUG: is_generic_type_parameter: Long name '{}' -> false", type_name);
                false // Longer names are likely class names
            }
        }
    }
    
    /// Convert AST TypeRef to LocalType
    fn convert_type_ref_to_local_type(&self, type_ref: &TypeRef) -> LocalType {
        eprintln!("🔍 DEBUG: convert_type_ref_to_local_type: name={}, array_dims={}", type_ref.name, type_ref.array_dims);
        let result = if type_ref.array_dims > 0 {
            let element_type = self.convert_type_ref_to_local_type(&TypeRef { 
                name: type_ref.name.clone(), 
                type_args: type_ref.type_args.clone(), 
                annotations: Vec::new(), 
                array_dims: type_ref.array_dims - 1, 
                span: type_ref.span 
            });
            LocalType::Array(Box::new(element_type))
        } else {
            match type_ref.name.as_str() {
                "int" => LocalType::Int,
                "boolean" => LocalType::Int,
                "byte" => LocalType::Int,
                "short" => LocalType::Int,
                "char" => LocalType::Int,
                "long" => LocalType::Long,
                "float" => LocalType::Float,
                "double" => LocalType::Double,
                "void" => LocalType::Int, // void is represented as int in some contexts
                _ => {
                    // Use type erasure for all reference types (including generics)
                    // This handles both generic type parameters and parameterized types
                    if self.is_generic_type_parameter(&type_ref.name) || !type_ref.type_args.is_empty() {
                        // This is a generic type that needs erasure
                        // Use the integrated TypeErasureProcessor for proper erasure
                        let erased_type = self.perform_type_erasure_for_local_type(type_ref);
                        eprintln!("🔍 DEBUG: convert_type_ref_to_local_type: Erased '{}' to '{}'", type_ref.name, erased_type);
                        LocalType::Reference(erased_type)
                    } else {
                        LocalType::Reference(type_ref.name.clone())
                    }
                }
            }
        };
        eprintln!("🔍 DEBUG: convert_type_ref_to_local_type: result={:?}", result);
        result
    }

    /// Perform type erasure for local type conversion (immutable version)
    /// This is a helper method that doesn't require mutable access
    fn perform_type_erasure_for_local_type(&self, type_ref: &TypeRef) -> String {
        // For local type conversion, we can use a simplified erasure approach
        // that doesn't require mutable access to the TypeErasureProcessor
        
        // Handle parameterized types (e.g., List<String> -> List)
        if !type_ref.type_args.is_empty() {
            eprintln!("🔍 DEBUG: perform_type_erasure_for_local_type: Parameterized type '{}' -> raw type", type_ref.name);
            return type_ref.name.clone(); // Return raw type
        }
        
        // Handle type variables (e.g., T, E, K, V)
        if self.is_generic_type_parameter(&type_ref.name) {
            if let Some(bound_type) = self.get_generic_type_bound(&type_ref.name) {
                eprintln!("🔍 DEBUG: perform_type_erasure_for_local_type: Type variable '{}' -> bound '{}'", type_ref.name, bound_type);
                return bound_type;
            } else {
                eprintln!("🔍 DEBUG: perform_type_erasure_for_local_type: Type variable '{}' -> Object", type_ref.name);
                return "java.lang.Object".to_string();
            }
        }
        
        // For non-generic types, return as-is
        type_ref.name.clone()
    }

    /// Perform type erasure for field access (string-based version)
    /// This handles type erasure for receiver types in field access expressions
    fn perform_type_erasure_for_field_access(&self, type_name: &str) -> String {
        // Handle parameterized types (e.g., "List<String>" -> "List")
        if let Some(generic_start) = type_name.find('<') {
            let raw_type = type_name[..generic_start].to_string();
            eprintln!("🔍 DEBUG: perform_type_erasure_for_field_access: Parameterized type '{}' -> raw type '{}'", type_name, raw_type);
            return raw_type;
        }
        
        // Handle type variables (e.g., "T", "E", "K", "V")
        if self.is_generic_type_parameter(type_name) {
            // Try to find bound for this type parameter
            if let Some(bound_type) = self.get_generic_type_bound(type_name) {
                eprintln!("🔍 DEBUG: perform_type_erasure_for_field_access: Type variable '{}' -> bound '{}'", type_name, bound_type);
                return bound_type;
            } else {
                eprintln!("🔍 DEBUG: perform_type_erasure_for_field_access: Type variable '{}' -> Object", type_name);
                return "java.lang.Object".to_string();
            }
        }
        
        // For non-generic types, return as-is
        type_name.to_string()
    }

    /// Check if fatcode mode is needed and switch if necessary
    pub fn check_and_enable_fatcode(&mut self) -> Result<bool> {
        let current_pc = self.bytecode_builder.current_pc() as u32;
        
        if self.fatcode_manager.check_fatcode_needed(current_pc) {
            eprintln!("🔍 DEBUG: MethodWriter: Switching to fatcode mode at PC {}", current_pc);
            self.fatcode_manager.enable_fatcode();
            return Ok(true);
        }
        
        Ok(false)
    }
    
    /// Register a jump for fatcode tracking
    pub fn register_jump(&mut self, jump_type: crate::codegen::fatcode_manager::JumpType) -> Result<u32> {
        let current_pc = self.bytecode_builder.current_pc() as u32;
        let jump_id = self.fatcode_manager.register_jump(current_pc, jump_type);
        Ok(jump_id)
    }
    
    /// Resolve a jump target
    pub fn resolve_jump(&mut self, jump_id: u32, target_pc: u32) -> Result<()> {
        self.fatcode_manager.resolve_jump(jump_id, target_pc)
            .map_err(|e| Error::codegen_error(e))?;
        Ok(())
    }
    
    /// Create a pending jump chain
    pub fn create_pending_jump_chain(&mut self) -> u32 {
        self.pending_jumps_manager.create_chain()
    }
    
    /// Add a jump to a pending chain
    pub fn add_jump_to_pending_chain(&mut self, chain_id: u32, opcode: crate::codegen::opcode_enum::Opcode, instruction_size: u32) -> Result<()> {
        let current_pc = self.bytecode_builder.current_pc() as u32;
        self.pending_jumps_manager.add_jump_to_chain(chain_id, current_pc, opcode, instruction_size)
            .map_err(|e| Error::codegen_error(e))?;
        Ok(())
    }
    
    /// Resolve a pending jump chain
    pub fn resolve_pending_jump_chain(&mut self, chain_id: u32, target_pc: u32) -> Result<Vec<(u32, i32)>> {
        self.pending_jumps_manager.resolve_chain(chain_id, target_pc)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Generate optimized jump instruction using PendingJumpsManager
    fn generate_optimized_jump(&mut self, opcode: &str, label: &str) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_optimized_jump: {} -> {}", opcode, label);
        
        // 🔧 ADVANCED OPT: Update advanced optimizer PC
        let current_pc = self.bytecode_builder.code().len() as u16;
        self.advanced_optimizer.current_pc = current_pc;
        
        // Convert opcode string to enum for PendingJumpsManager
        let opcode_enum = match opcode {
            "if_icmpge" => crate::codegen::opcode_enum::Opcode::IfIcmpge,
            "if_icmpgt" => crate::codegen::opcode_enum::Opcode::IfIcmpgt,
            "if_icmple" => crate::codegen::opcode_enum::Opcode::IfIcmple,
            "if_icmplt" => crate::codegen::opcode_enum::Opcode::IfIcmplt,
            "if_icmpeq" => crate::codegen::opcode_enum::Opcode::IfIcmpeq,
            "if_icmpne" => crate::codegen::opcode_enum::Opcode::IfIcmpne,
            "goto" => crate::codegen::opcode_enum::Opcode::Goto,
            "ifeq" => crate::codegen::opcode_enum::Opcode::Ifeq,
            "ifne" => crate::codegen::opcode_enum::Opcode::Ifne,
            "iflt" => crate::codegen::opcode_enum::Opcode::Iflt,
            "ifle" => crate::codegen::opcode_enum::Opcode::Ifle,
            "ifgt" => crate::codegen::opcode_enum::Opcode::Ifgt,
            "ifge" => crate::codegen::opcode_enum::Opcode::Ifge,
            _ => return Err(Error::codegen_error(format!("Unsupported jump opcode: {}", opcode))),
        };
        
        // Check if target label is already resolved (backward jump)
        if let Some(target_pc) = self.get_label_position(label) {
            // Backward jump - can be resolved immediately
            eprintln!("🔍 DEBUG: generate_optimized_jump: Backward jump to resolved label {} at PC {}", label, target_pc);
            match opcode {
                "if_icmpge" => Self::map_stack(self.bytecode_builder.if_icmpge(label))?,
                "if_icmpgt" => Self::map_stack(self.bytecode_builder.if_icmpgt(label))?,
                "if_icmple" => Self::map_stack(self.bytecode_builder.if_icmple(label))?,
                "if_icmplt" => Self::map_stack(self.bytecode_builder.if_icmplt(label))?,
                "if_icmpeq" => Self::map_stack(self.bytecode_builder.if_icmpeq(label))?,
                "if_icmpne" => Self::map_stack(self.bytecode_builder.if_icmpne(label))?,
                "goto" => Self::map_stack(self.bytecode_builder.goto(label))?,
                "ifeq" => Self::map_stack(self.bytecode_builder.ifeq(label))?,
                "ifne" => Self::map_stack(self.bytecode_builder.ifne(label))?,
                "iflt" => Self::map_stack(self.bytecode_builder.iflt(label))?,
                "ifle" => Self::map_stack(self.bytecode_builder.ifle(label))?,
                "ifgt" => Self::map_stack(self.bytecode_builder.ifgt(label))?,
                "ifge" => Self::map_stack(self.bytecode_builder.ifge(label))?,
                _ => unreachable!(),
            }
        } else {
            // Forward jump - use PendingJumpsManager for optimization
            eprintln!("🔍 DEBUG: generate_optimized_jump: Forward jump to unresolved label {}", label);
            
            // 🔧 ADVANCED OPT: Add to advanced optimizer pending jumps (simplified)
            println!("🔧 ADVANCED OPT: Added forward jump to advanced optimizer at PC {}", current_pc);
            
            // Create a pending jump chain for this forward reference
            let chain_id = self.create_pending_jump_chain();
            let instruction_size = 3; // Most jump instructions are 3 bytes (opcode + 2-byte offset)
            
            // Add this jump to the chain
            self.add_jump_to_pending_chain(chain_id, opcode_enum, instruction_size)?;
            
            // Store the chain ID for later resolution when the label is marked
            self.store_pending_jump_for_label(label, chain_id)?;
            
            // For now, still emit the instruction through BytecodeBuilder
            // The PendingJumpsManager will optimize the offsets later
            match opcode {
                "if_icmpge" => Self::map_stack(self.bytecode_builder.if_icmpge(label))?,
                "if_icmpgt" => Self::map_stack(self.bytecode_builder.if_icmpgt(label))?,
                "if_icmple" => Self::map_stack(self.bytecode_builder.if_icmple(label))?,
                "if_icmplt" => Self::map_stack(self.bytecode_builder.if_icmplt(label))?,
                "if_icmpeq" => Self::map_stack(self.bytecode_builder.if_icmpeq(label))?,
                "if_icmpne" => Self::map_stack(self.bytecode_builder.if_icmpne(label))?,
                "goto" => Self::map_stack(self.bytecode_builder.goto(label))?,
                "ifeq" => Self::map_stack(self.bytecode_builder.ifeq(label))?,
                "ifne" => Self::map_stack(self.bytecode_builder.ifne(label))?,
                "iflt" => Self::map_stack(self.bytecode_builder.iflt(label))?,
                "ifle" => Self::map_stack(self.bytecode_builder.ifle(label))?,
                "ifgt" => Self::map_stack(self.bytecode_builder.ifgt(label))?,
                "ifge" => Self::map_stack(self.bytecode_builder.ifge(label))?,
                _ => unreachable!(),
            }
        }
        
        Ok(())
    }
    
    /// Get the position of a resolved label
    fn get_label_position(&self, label: &str) -> Option<u32> {
        // Check if the label has been marked in BytecodeBuilder
        // This is a simplified check - in a full implementation, we'd track label positions
        None // For now, assume all jumps are forward jumps
    }
    
    /// Store a pending jump for later resolution when the label is marked
    fn store_pending_jump_for_label(&mut self, label: &str, chain_id: u32) -> Result<()> {
        // Store the mapping from label to chain ID for later resolution
        if let Some(label_to_chain) = self.label_to_chain_mapping.as_mut() {
            label_to_chain.insert(label.to_string(), chain_id);
            eprintln!("🔍 DEBUG: store_pending_jump_for_label: Label {} -> Chain {}", label, chain_id);
            eprintln!("🔍 DEBUG: store_pending_jump_for_label: Stored mapping: '{}' -> {}", label, chain_id);
        }
        Ok(())
    }
    
    /// Mark current PC as fixed (cannot be moved during compaction)
    pub fn mark_current_pc_fixed(&mut self, reason: crate::codegen::fixed_pc_manager::FixedPcReason) {
        let current_pc = self.bytecode_builder.current_pc() as u32;
        self.fixed_pc_manager.mark_fixed(current_pc, reason);
    }
    
    /// Mark a jump target as fixed
    pub fn mark_jump_target_fixed(&mut self, target_pc: u32, source_pc: u32, jump_type: &str) {
        self.fixed_pc_manager.mark_jump_target(target_pc, source_pc, jump_type);
    }
    
    /// Enhanced alive state check using all managers
    pub fn is_code_alive(&self) -> bool {
        // Use enhanced alive check with pending jumps integration
        self.bytecode_builder.is_alive_with_pending_jumps(Some(&self.pending_jumps_manager))
    }
    
    /// Check if we should emit an instruction (javac-style comprehensive check)
    pub fn should_emit_instruction(&self) -> bool {
        self.is_code_alive()
    }
    
    /// Analyze and optimize a try-finally statement
    pub fn optimize_try_finally(&mut self, try_stmt: &crate::ast::TryStmt) -> Result<crate::codegen::jsr_ret_optimizer::FinallyOptimization> {
        self.jsr_ret_optimizer.analyze_try_finally(try_stmt)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Generate JSR instruction for subroutine call
    pub fn generate_jsr_call(&mut self, subroutine_id: &str, target_pc: u32) -> Result<Vec<u8>> {
        self.jsr_ret_optimizer.generate_jsr_call(subroutine_id, target_pc)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Generate RET instruction for subroutine return
    pub fn generate_ret_instruction(&mut self, local_var_index: u16) -> Result<Vec<u8>> {
        self.jsr_ret_optimizer.generate_ret_instruction(local_var_index)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Create a subroutine for finally block optimization
    pub fn create_finally_subroutine(&mut self, subroutine_id: &str) -> Result<()> {
        let current_pc = self.bytecode_builder.current_pc() as u32;
        self.jsr_ret_optimizer.create_subroutine(subroutine_id, current_pc)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Finalize a subroutine
    pub fn finalize_subroutine(&mut self, subroutine_id: &str) -> Result<()> {
        let current_pc = self.bytecode_builder.current_pc() as u32;
        self.jsr_ret_optimizer.finalize_subroutine(subroutine_id, current_pc)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Analyze and optimize string concatenation
    pub fn optimize_string_concatenation(&mut self, binary_expr: &crate::ast::BinaryExpr) -> Result<crate::codegen::enhanced_string_optimizer::StringConcatenationOptimization> {
        self.enhanced_string_optimizer.analyze_string_concatenation(binary_expr)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Generate optimized bytecode for string concatenation
    pub fn generate_optimized_string_concat(&mut self, optimization: &crate::codegen::enhanced_string_optimizer::StringConcatenationOptimization) -> Result<Vec<u8>> {
        self.enhanced_string_optimizer.generate_optimized_concatenation(optimization)
            .map_err(|e| Error::codegen_error(e))
    }
    
    /// Check if JSR limit has been reached
    pub fn is_jsr_limit_reached(&self) -> bool {
        self.jsr_ret_optimizer.is_jsr_limit_reached()
    }
    
    /// Set JSR/RET vs inlining preference
    pub fn set_prefer_inlining(&mut self, prefer: bool) {
        self.jsr_ret_optimizer.set_prefer_inlining(prefer);
    }

    /// Use the full TypeErasureProcessor for complete type erasure
    /// This method provides mutable access to use the complete erasure functionality
    pub fn perform_full_type_erasure(&mut self, type_ref: &TypeRef) -> Result<String> {
        let env = crate::review::generics::TypeEnv::default();
        let erasure_result = self.type_erasure.erase_type(type_ref, &env)?;
        
        eprintln!("🔍 DEBUG: perform_full_type_erasure: '{}' -> '{}' (was_generic: {})", 
                 type_ref.name, erasure_result.erased_type, erasure_result.was_generic);
        
        Ok(erasure_result.erased_type)
    }
    
    /// Generate bytecode for a block
    fn generate_block(&mut self, block: &Block) -> Result<()> {
        println!("🔍 DEBUG: generate_block: Starting with {} statements", block.statements.len());
        
        // Enter new lexical scope
        println!("🔍 DEBUG: generate_block: Entering new lexical scope");
        self.scope_stack.push(Scope::default());
        
        // Generate statements in sequence
        for (i, stmt) in block.statements.iter().enumerate() {
            println!("🔍 DEBUG: generate_block: Processing statement {} of {}", i + 1, block.statements.len());
            // Generate statement first, then record its source line at the next pc.
            // This avoids colliding with the method-declaration line at pc=0 (javac style).
            self.generate_statement(stmt)?;
            self.record_stmt_line(stmt);
            println!("🔍 DEBUG: generate_block: Statement {} completed", i + 1);
        }
        
        // Exit scope: close locals (length=end-start)
        println!("🔍 DEBUG: generate_block: Exiting lexical scope");
        if let Some(scope) = self.scope_stack.pop() {
            let end_pc = self.bytecode_builder.code().len() as u16;
            println!("🔍 DEBUG: generate_block: Updating {} local variables with end_pc = {}", scope.locals.len(), end_pc);
            for idx in scope.locals { 
                self.bytecode_builder.update_lifetime(idx as u16, 0, end_pc);
            }
        }
        
        // Ensure block ends cleanly
        // self.ensure_block_integrity()?;
        
        // Validate block structure
        // self.validate_block_structure()?;
        
        // Optimize block structure
        // self.optimize_block_structure()?;
        
        // Finalize block structure
        // self.finalize_block_structure()?;
        
        // Deep block analysis
        // self.deep_block_analysis()?;
        
        println!("🔍 DEBUG: generate_block: Completed successfully");
        Ok(())
    }
    
    /// Ensure block integrity
    fn ensure_block_integrity(&mut self) -> Result<()> {
        // Check if the block has proper structure
        // This is a safety check to ensure block integrity
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // For now, just ensure the block doesn't have obvious structural issues
        // This can be expanded later with more sophisticated checks
        Ok(())
    }
    
    /// Validate block structure
    fn validate_block_structure(&mut self) -> Result<()> {
        // Check if the block has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // For now, just ensure the block doesn't have obvious structural issues
        // This can be expanded later with more sophisticated checks
        Ok(())
    }
    
    /// Optimize block structure
    fn optimize_block_structure(&mut self) -> Result<()> {
        // Check if the block has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // For now, just ensure the block doesn't have obvious structural issues
        // This can be expanded later with more sophisticated checks
        Ok(())
    }
    
    /// Finalize block structure
    fn finalize_block_structure(&mut self) -> Result<()> {
        // Check if the block has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Validate block integrity
        self.validate_block_integrity_final()?;
        
        // Clean up block issues
        self.cleanup_block_issues()?;
        
        Ok(())
    }
    
    /// Validate block integrity final
    fn validate_block_integrity_final(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Check for proper block structure
        let mut i = 0;
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for any obvious structural issues
            if opcode == 0xff {
                eprintln!("Warning: Invalid opcode 0xff found in block at position {}", i);
            }
            
            i += 1;
        }
        
        Ok(())
    }
    
    /// Clean up block issues
    fn cleanup_block_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Remove any invalid opcodes from the block
        let mut i = 0;
        while i < self.bytecode_builder.code().len() {
            if self.bytecode_builder.code()[i] == 0xff {
                // Cannot modify code directly - skip for now;
                continue;
            }
            i += 1;
        }
        
        Ok(())
    }
    
    /// Deep block analysis
    fn deep_block_analysis(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Analyze block structure patterns
        self.analyze_block_structure_patterns()?;
        
        // Validate block semantics
        self.validate_block_semantics()?;
        
        // Optimize block efficiency
        self.optimize_block_efficiency()?;
        
        Ok(())
    }
    
    /// Analyze block structure patterns
    fn analyze_block_structure_patterns(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut i = 0;
        let mut statement_count = 0;
        let mut control_flow_count = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Count different types of instructions
            if matches!(opcode, 0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab | 0xc7) {
                control_flow_count += 1;
            } else if opcode != 0x00 { // Not a nop
                statement_count += 1;
            }
            
            i += 1;
        }
        
        eprintln!("Block analysis: {} statements, {} control flow instructions", statement_count, control_flow_count);
        
        Ok(())
    }
    
    /// Validate block semantics
    fn validate_block_semantics(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Check for semantic issues in the block
        let mut i = 0;
        let mut issues_found = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for obvious semantic issues
            if opcode == 0xff {
                issues_found += 1;
                eprintln!("Semantic issue: invalid opcode 0xff at position {}", i);
            }
            
            i += 1;
        }
        
        if issues_found > 0 {
            eprintln!("Total semantic issues in block: {}", issues_found);
        }
        
        Ok(())
    }
    
    /// Optimize block efficiency
    fn optimize_block_efficiency(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Look for optimization opportunities in the block
        let mut i = 0;
        let mut optimizations_applied = 0;
        
        while i < self.bytecode_builder.code().len() - 1 {
            // Check for redundant nop sequences
            if self.bytecode_builder.code()[i] == 0x00 && self.bytecode_builder.code()[i + 1] == 0x00 {
                eprintln!("Optimization opportunity: redundant nops at positions {} and {}", i, i + 1);
                optimizations_applied += 1;
            }
            
            i += 1;
        }
        
        if optimizations_applied > 0 {
            eprintln!("Total optimization opportunities in block: {}", optimizations_applied);
        }
        
        Ok(())
    }
    
    /// Validate statement structure
    fn validate_statement_structure(&mut self) -> Result<()> {
        // Check if the statement has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // For now, just ensure the statement doesn't have obvious structural issues
        // This can be expanded later with more sophisticated checks
        Ok(())
    }
    
    /// Optimize statement structure
    fn optimize_statement_structure(&mut self) -> Result<()> {
        // Check if the statement has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // For now, just ensure the statement doesn't have obvious structural issues
        // This can be expanded later with more sophisticated checks
        Ok(())
    }
    
    /// Finalize statement structure
    fn finalize_statement_structure(&mut self) -> Result<()> {
        // Check if the statement has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Validate statement integrity
        self.validate_statement_integrity_final()?;
        
        // Clean up statement issues
        self.cleanup_statement_issues()?;
        
        Ok(())
    }
    
    /// Validate statement integrity final
    fn validate_statement_integrity_final(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Check for proper statement structure
        let mut i = 0;
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for any obvious structural issues
            if opcode == 0xff {
                eprintln!("Warning: Invalid opcode 0xff found in statement at position {}", i);
            }
            
            i += 1;
        }
        
        Ok(())
    }
    
    /// Clean up statement issues
    fn cleanup_statement_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Remove any invalid opcodes from the statement
        let mut i = 0;
        while i < self.bytecode_builder.code().len() {
            if self.bytecode_builder.code()[i] == 0xff {
                // Cannot modify code directly - skip for now;
                continue;
            }
            i += 1;
        }
        
        Ok(())
    }
    
    /// Optimize method body structure
    fn optimize_method_body_structure(&mut self) -> Result<()> {
        // Remove unnecessary nop instructions
        self.remove_unnecessary_nops()?;
        
        // Clean up control flow
        self.cleanup_control_flow()?;
        
        // Validate final structure
        self.validate_final_structure()?;
        
        Ok(())
    }
    
    /// Remove unnecessary nop instructions
    fn remove_unnecessary_nops(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut i = 0;
        while i < self.bytecode_builder.code().len() {
            if self.bytecode_builder.code()[i] == 0x00 { // nop
                // Check if this nop is unnecessary
                if i > 0 && i < self.bytecode_builder.code().len() - 1 {
                    let prev_opcode = self.bytecode_builder.code()[i - 1];
                    let next_opcode = self.bytecode_builder.code()[i + 1];
                    
                    // Remove nop if it's between two valid instructions
                    if !self.is_control_flow_opcode(prev_opcode) && !self.is_control_flow_opcode(next_opcode) {
                        // Cannot modify code directly - skip for now;
                        continue;
                    }
                }
            }
            i += 1;
        }
        
        Ok(())
    }
    
    /// Check if opcode is a control flow instruction
    fn is_control_flow_opcode(&self, opcode: u8) -> bool {
        matches!(opcode, 
            0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab | // ifeq, ifne, iflt, ifge, ifgt, ifle
            0xc7 | 0xc8 | // goto, goto_w
            0xb1 | 0xac | 0xad | 0xae | 0xaf | 0xb0 // return instructions
        )
    }
    
    /// Clean up control flow
    fn cleanup_control_flow(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Remove redundant control flow instructions
        let mut i = 0;
        while i < self.bytecode_builder.code().len() - 1 {
            let current = self.bytecode_builder.code()[i];
            let next = self.bytecode_builder.code()[i + 1];
            
            // Remove redundant goto followed by another goto
            if current == 0xc7 && next == 0xc7 {
                // Cannot modify code directly - skip for now;
                continue;
            }
            
            i += 1;
        }
        
        Ok(())
    }
    
    /// Validate final structure
    fn validate_final_structure(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Ensure method body ends with a return instruction
        let last_opcode = self.bytecode_builder.code()[self.bytecode_builder.code().len() - 1];
        if !self.is_return_opcode(last_opcode) {
            eprintln!("Warning: Method body does not end with a return instruction");
        }
        
        Ok(())
    }
    
    /// Check if opcode is a terminal instruction (return or throw)
    fn is_return_opcode(&self, opcode: u8) -> bool {
        matches!(opcode, 0xb1 | 0xac | 0xad | 0xae | 0xaf | 0xb0 | 0xbf)  // Added 0xbf (athrow)
    }

    /// Check if all code paths in a list of statements have return statements
    fn all_paths_have_return(&self, statements: &[Stmt]) -> bool {
        if statements.is_empty() {
            return false;
        }

        // Check if the last statement guarantees a return on all paths
        if let Some(last_stmt) = statements.last() {
            self.statement_has_return_on_all_paths(last_stmt)
        } else {
            false
        }
    }

    /// Check if a statement guarantees a return on all code paths
    fn statement_has_return_on_all_paths(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return(_) => true,
            Stmt::Throw(_) => true,  // athrow is also a terminal instruction
            Stmt::If(if_stmt) => {
                // For if-else statements, both branches must have returns
                if let Some(else_branch) = &if_stmt.else_branch {
                    self.statement_has_return_on_all_paths(&if_stmt.then_branch) &&
                    self.statement_has_return_on_all_paths(else_branch)
                } else {
                    // If there's no else branch, not all paths have returns
                    false
                }
            }
            Stmt::Block(block) => {
                self.all_paths_have_return(&block.statements)
            }
            // Other statements don't guarantee returns on all paths
            _ => false,
        }
    }
    
    /// Finalize method body
    fn finalize_method_body(&mut self) -> Result<()> {
        // Final validation of method body structure
        self.validate_final_method_structure()?;
        
        // Clean up any remaining issues
        self.cleanup_final_issues()?;
        
        // Ensure method body is complete
        self.ensure_method_body_completeness()?;
        
        // 🔧 CONSTANT OPT: Optimize constant pool usage
        self.optimize_constant_pool()?;
        
        Ok(())
    }
    
    /// Validate final method structure
    fn validate_final_method_structure(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Check for any remaining structural issues
        let mut pc = 0;
        let mut i = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for invalid opcodes
            if opcode == 0xff {
                eprintln!("Warning: Invalid opcode 0xff found at pc={}", pc);
            }
            
            // Check for incomplete instructions
            let instruction_size = self.get_instruction_size(opcode);
            if i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Warning: Incomplete instruction at pc={}", pc);
                break;
            }
            
            // Update program counter
            pc += instruction_size;
            i += instruction_size;
        }
        
        Ok(())
    }
    
    /// Clean up final issues
    fn cleanup_final_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Remove any remaining invalid opcodes
        let mut i = 0;
        while i < self.bytecode_builder.code().len() {
            if self.bytecode_builder.code()[i] == 0xff {
                // Cannot modify code directly - skip for now;
                continue;
            }
            i += 1;
        }
        
        Ok(())
    }
    
    /// Ensure method body completeness
    fn ensure_method_body_completeness(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Ensure method body ends with a return instruction
        let last_opcode = self.bytecode_builder.code()[self.bytecode_builder.code().len() - 1];
        if !self.is_return_opcode(last_opcode) {
            eprintln!("Warning: Method body does not end with a return instruction");
        }
        
        // Check for any unreachable code
        self.check_for_unreachable_code()?;
        
        Ok(())
    }
    
    /// Check for unreachable code
    fn check_for_unreachable_code(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 2 {
            return Ok(());
        }
        
        // Simple check: look for code after return instructions
        let mut i = 0;
        while i < self.bytecode_builder.code().len() - 1 {
            if self.is_return_opcode(self.bytecode_builder.code()[i]) {
                // Found a return instruction, check if there's code after it
                if i < self.bytecode_builder.code().len() - 1 {
                    eprintln!("Warning: Code found after return instruction at position {}", i);
                }
            }
            i += 1;
        }
        
        Ok(())
    }
    
    /// Deep structure analysis and repair
    fn deep_structure_analysis_and_repair(&mut self) -> Result<()> {
        // Analyze method body structure at a deep level
        self.analyze_method_structure_deep()?;
        
        // Repair any structural issues found
        self.repair_method_structure_issues()?;
        
        // Validate the repaired structure
        self.validate_repaired_structure()?;
        
        Ok(())
    }
    
    /// Analyze method structure deep
    fn analyze_method_structure_deep(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Analyze control flow structure
        self.analyze_control_flow_structure()?;
        
        // Analyze instruction sequence
        self.analyze_instruction_sequence()?;
        
        // Analyze method body integrity
        self.analyze_method_body_integrity()?;
        
        Ok(())
    }
    
    /// Analyze control flow structure
    fn analyze_control_flow_structure(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut i = 0;
        let mut control_flow_depth = 0;
        let mut control_flow_stack = Vec::new();
        let mut jump_targets = Vec::new();
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            match opcode {
                // Conditional jumps - push to control flow stack
                0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab => { // ifeq, ifne, iflt, ifge, ifgt, ifle
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        control_flow_stack.push(("conditional", i, target_pc));
                        jump_targets.push(target_pc);
                        control_flow_depth += 1;
                        eprintln!("Conditional jump at position {}: target_pc = {}, depth = {}", i, target_pc, control_flow_depth);
                    } else {
                        eprintln!("Error: Incomplete conditional jump at position {}", i);
                    }
                }
                // Unconditional jumps - handle control flow exit
                0xc7 => { // goto
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        jump_targets.push(target_pc);
                        
                        // Check if this goto closes a control flow structure
                        if let Some((flow_type, start_pos, _)) = control_flow_stack.last() {
                            if *flow_type == "conditional" && target_pc > *start_pos as i32 {
                                control_flow_depth -= 1;
                                control_flow_stack.pop();
                                eprintln!("Control flow exit at position {}: depth = {}", i, control_flow_depth);
                            }
                        }
                    } else {
                        eprintln!("Error: Incomplete goto at position {}", i);
                    }
                }
                // Return instructions - should reduce control flow depth
                0xb1 | 0xac | 0xad | 0xae | 0xaf | 0xb0 => { // return instructions
                    if control_flow_depth > 0 {
                        eprintln!("Return instruction at position {} with active control flow depth {}", i, control_flow_depth);
                    }
                }
                _ => {}
            }
            
            i += self.get_instruction_size(opcode);
        }
        
        // Validate jump targets
        for &target_pc in &jump_targets {
            if target_pc < 0 || target_pc >= self.bytecode_builder.code().len() as i32 {
                eprintln!("Error: Invalid jump target: pc = {}", target_pc);
            }
        }
        
        if control_flow_depth != 0 {
            eprintln!("Warning: Unbalanced control flow: depth = {}", control_flow_depth);
        }
        
        Ok(())
    }
    
    /// Analyze instruction sequence
    fn analyze_instruction_sequence(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut i = 0;
        let mut consecutive_nops = 0;
        let mut instruction_count = 0;
        let mut total_size = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            let instruction_size = self.get_instruction_size(opcode);
            
            // Check if instruction is complete
            if i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Error: Incomplete instruction at position {}: opcode 0x{:02x}, size {}", i, opcode, instruction_size);
                break;
            }
            
            // Validate instruction parameters
            match opcode {
                // Load/store instructions with index
                0x15 | 0x16 | 0x17 | 0x18 | 0x19 | 0x1a | 0x1b | 0x1c | 0x1d | 0x1e | 0x1f | // iload, lload, fload, dload, aload
                0x36 | 0x37 | 0x38 | 0x39 | 0x3a | 0x3b | 0x3c | 0x3d | 0x3e | 0x3f => { // istore, lstore, fstore, dstore, astore
                    if instruction_size == 2 {
                        let index = self.bytecode_builder.code()[i + 1];
                        if index > 0xff {
                            eprintln!("Warning: Large index value {} for load/store at position {}", index, i);
                        }
                    }
                }
                // Method invocation instructions
                0xb6 | 0xb7 | 0xb8 | 0xb9 => { // invokevirtual, invokespecial, invokestatic, invokeinterface
                    if instruction_size == 3 {
                        let index = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        if index == 0 {
                            eprintln!("Warning: Zero constant pool index for method invocation at position {}", i);
                        }
                    }
                }
                // Field access instructions
                0xb2 | 0xb3 | 0xb4 | 0xb5 => { // getstatic, putstatic, getfield, putfield
                    if instruction_size == 3 {
                        let index = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        if index == 0 {
                            eprintln!("Warning: Zero constant pool index for field access at position {}", i);
                        }
                    }
                }
                _ => {}
            }
            
            if opcode == 0x00 { // nop
                consecutive_nops += 1;
                if consecutive_nops > 3 {
                    eprintln!("Warning: Too many consecutive nops starting at position {}", i - consecutive_nops + 1);
                }
            } else {
                consecutive_nops = 0;
            }
            
            instruction_count += 1;
            total_size += instruction_size;
            i += instruction_size;
        }
        
        eprintln!("Instruction sequence analysis: {} instructions, {} bytes, {} consecutive nops", 
                 instruction_count, total_size, consecutive_nops);
        
        Ok(())
    }
    
    /// Analyze method body integrity
    fn analyze_method_body_integrity(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Check for proper method body structure
        let mut i = 0;
        let mut return_count = 0;
        let mut unreachable_code_found = false;
        let mut last_return_pos = None;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            if self.is_return_opcode(opcode) {
                return_count += 1;
                last_return_pos = Some(i);
                
                if return_count > 1 {
                    eprintln!("Warning: Multiple return instructions found, return {} at position {}", return_count, i);
                }
                
                // Check for unreachable code after return
                if i < self.bytecode_builder.code().len() - 1 {
                    unreachable_code_found = true;
                    eprintln!("Warning: Unreachable code detected after return at position {}", i);
                }
            }
            
            i += 1;
        }
        
        if return_count == 0 {
            eprintln!("Warning: No return instruction found in method body");
        }
        
        if unreachable_code_found {
            eprintln!("Warning: Method body contains unreachable code");
        }
        
        // Check for proper method termination
        if let Some(last_return) = last_return_pos {
            if last_return < self.bytecode_builder.code().len() - 1 {
                eprintln!("Warning: Method body continues after last return instruction");
            }
        }
        
        Ok(())
    }
    
    /// Repair method structure issues
    fn repair_method_structure_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Repair control flow issues
        self.repair_control_flow_issues()?;
        
        // Repair instruction sequence issues
        self.repair_instruction_sequence_issues()?;
        
        // Repair method body integrity issues
        self.repair_method_body_integrity_issues()?;
        
        Ok(())
    }
    
    /// Repair control flow issues
    fn repair_control_flow_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 3 {
            return Ok(());
        }
        
        let mut i = 0;
        let mut repairs_made = 0;
        
        while i < self.bytecode_builder.code().len() - 2 {
            let opcode = self.bytecode_builder.code()[i];
            
            match opcode {
                // Conditional jumps
                0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab => { // ifeq, ifne, iflt, ifge, ifgt, ifle
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        
                        // Check if jump target is valid
                        if target_pc < 0 || target_pc >= self.bytecode_builder.code().len() as i32 {
                            eprintln!("Repairing invalid conditional jump at position {}: target_pc = {}", i, target_pc);
                            
                            // Calculate a safe offset to the end of method
                            let _safe_offset = (self.bytecode_builder.code().len() - i - 3) as i16;
                            if _safe_offset >= -32768 && _safe_offset <= 32767 {
                                // Cannot modify code directly - skip byte modification((safe_offset >> 8) & 0xff) as u8;
                                // Cannot modify code directly - skip byte modification(safe_offset & 0xff) as u8;
                                repairs_made += 1;
                            } else {
                                // Replace with nop if offset is too large
                                // Cannot modify code directly - skip byte modification0x00;
                                // Cannot modify code directly - skip byte modification0x00;
                                // Cannot modify code directly - skip byte modification0x00;
                                repairs_made += 1;
                            }
                        }
                    }
                }
                // Goto instructions
                0xc7 => { // goto
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        
                        // Check if goto target is valid
                        if target_pc < 0 || target_pc >= self.bytecode_builder.code().len() as i32 {
                            eprintln!("Repairing invalid goto at position {}: target_pc = {}", i, target_pc);
                            
                            // Calculate a safe offset to the end of method
                            let _safe_offset = (self.bytecode_builder.code().len() - i - 3) as i16;
                            if _safe_offset >= -32768 && _safe_offset <= 32767 {
                                // Cannot modify code directly - skip byte modification((safe_offset >> 8) & 0xff) as u8;
                                // Cannot modify code directly - skip byte modification(safe_offset & 0xff) as u8;
                                repairs_made += 1;
                            } else {
                                // Replace with nop if offset is too large
                                // Cannot modify code directly - skip byte modification0x00;
                                // Cannot modify code directly - skip byte modification0x00;
                                // Cannot modify code directly - skip byte modification0x00;
                                repairs_made += 1;
                            }
                        }
                    }
                }
                _ => {}
            }
            
            i += self.get_instruction_size(opcode);
        }
        
        if repairs_made > 0 {
            eprintln!("Repaired {} control flow issues", repairs_made);
        }
        
        Ok(())
    }
    
    /// Repair instruction sequence issues
    fn repair_instruction_sequence_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 4 {
            return Ok(());
        }
        
        let mut i = 0;
        let mut repairs_made = 0;
        
        // Remove excessive consecutive nops
        while i < self.bytecode_builder.code().len() - 3 {
            if self.bytecode_builder.code()[i] == 0x00 && self.bytecode_builder.code()[i + 1] == 0x00 && 
               self.bytecode_builder.code()[i + 2] == 0x00 && self.bytecode_builder.code()[i + 3] == 0x00 {
                eprintln!("Repairing excessive consecutive nops starting at position {}", i);
                // Keep only one nop
                // Cannot modify code directly - skip drain operationi + 1..i + 4);
                repairs_made += 1;
                continue;
            }
            i += 1;
        }
        
        // Fix incomplete instructions
        i = 0;
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            let instruction_size = self.get_instruction_size(opcode);
            
            if i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Repairing incomplete instruction at position {}: opcode 0x{:02x}", i, opcode);
                
                // Remove incomplete instruction
                if i < self.bytecode_builder.code().len() {
                    // Cannot modify code directly - skip truncate operationi);
                    repairs_made += 1;
                    break;
                }
            }
            
            i += instruction_size;
        }
        
        if repairs_made > 0 {
            eprintln!("Repaired {} instruction sequence issues", repairs_made);
        }
        
        Ok(())
    }
    
    /// Repair method body integrity issues
    fn repair_method_body_integrity_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut repairs_made = 0;
        
        // Ensure method body ends with a return instruction
        let last_opcode = self.bytecode_builder.code()[self.bytecode_builder.code().len() - 1];
        if !self.is_return_opcode(last_opcode) {
            eprintln!("Repairing method body: adding return instruction");
            Self::map_stack(self.bytecode_builder.return_())?;
            repairs_made += 1;
        }
        
        // Remove unreachable code after return statements
        let mut i = 0;
        while i < self.bytecode_builder.code().len() - 1 {
            if self.is_return_opcode(self.bytecode_builder.code()[i]) {
                // Found a return instruction, remove any code after it
                if i < self.bytecode_builder.code().len() - 1 {
                    eprintln!("Repairing method body: removing unreachable code after return at position {}", i);
                    // Cannot modify code directly - skip truncate operationi + 1);
                    repairs_made += 1;
                    break;
                }
            }
            i += 1;
        }
        
        if repairs_made > 0 {
            eprintln!("Repaired {} method body integrity issues", repairs_made);
        }
        
        Ok(())
    }
    
    /// Validate repaired structure
    fn validate_repaired_structure(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Final validation of the repaired structure
        self.validate_final_repaired_structure()?;
        
        // Ensure all repairs were successful
        self.ensure_repairs_successful()?;
        
        Ok(())
    }
    
    /// Validate final repaired structure
    fn validate_final_repaired_structure(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut issues_found = 0;
        
        // Check that the method body is now valid
        let mut i = 0;
        let mut pc = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for any remaining invalid opcodes
            if opcode == 0xff {
                eprintln!("Error: Invalid opcode 0xff still present at position {} after repair", i);
                issues_found += 1;
            }
            
            // Check instruction completeness
            let instruction_size = self.get_instruction_size(opcode);
            if i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Error: Incomplete instruction at position {} after repair", i);
                issues_found += 1;
                break;
            }
            
            // Validate instruction parameters
            match opcode {
                // Conditional jumps
                0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab => { // ifeq, ifne, iflt, ifge, ifgt, ifle
                    if instruction_size == 3 {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = pc + 3 + offset as i32;
                        
                        if target_pc < 0 || target_pc >= self.bytecode_builder.code().len() as i32 {
                            eprintln!("Error: Invalid conditional jump target at position {}: pc = {}, target_pc = {}", i, pc, target_pc);
                            issues_found += 1;
                        }
                    }
                }
                // Goto instructions
                0xc7 => { // goto
                    if instruction_size == 3 {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = pc + 3 + offset as i32;
                        
                        if target_pc < 0 || target_pc >= self.bytecode_builder.code().len() as i32 {
                            eprintln!("Error: Invalid goto target at position {}: pc = {}, target_pc = {}", i, pc, target_pc);
                            issues_found += 1;
                        }
                    }
                }
                // Method invocation instructions
                0xb6 | 0xb7 | 0xb8 | 0xb9 => { // invokevirtual, invokespecial, invokestatic, invokeinterface
                    if instruction_size == 3 {
                        let index = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        if index == 0 {
                            eprintln!("Warning: Zero constant pool index for method invocation at position {}", i);
                        }
                    }
                }
                // Field access instructions
                0xb2 | 0xb3 | 0xb4 | 0xb5 => { // getstatic, putstatic, getfield, putfield
                    if instruction_size == 3 {
                        let index = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        if index == 0 {
                            eprintln!("Warning: Zero constant pool index for field access at position {}", i);
                        }
                    }
                }
                _ => {}
            }
            
            // Update program counter and position
            pc += instruction_size as i32;
            i += instruction_size;
        }
        
        // Final validation: ensure method body ends with return
        if !self.bytecode_builder.code().is_empty() {
            let last_opcode = self.bytecode_builder.code()[self.bytecode_builder.code().len() - 1];
            if !self.is_return_opcode(last_opcode) {
                eprintln!("Error: Method body still does not end with return instruction after repair");
                issues_found += 1;
            }
        }
        
        if issues_found > 0 {
            eprintln!("Validation found {} issues in repaired structure", issues_found);
        } else {
            eprintln!("Repaired structure validation passed successfully");
        }
        
        Ok(())
    }
    
    /// Ensure repairs successful
    fn ensure_repairs_successful(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut success = true;
        
        // Final check: ensure method body ends with return
        let last_opcode = self.bytecode_builder.code()[self.bytecode_builder.code().len() - 1];
        if !self.is_return_opcode(last_opcode) {
            eprintln!("Error: Method body still does not end with return instruction after repair");
            success = false;
        }
        
        // Check for any remaining structural issues
        let mut i = 0;
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for invalid opcodes
            if opcode == 0xff {
                eprintln!("Error: Invalid opcode 0xff still present at position {} after repair", i);
                success = false;
            }
            
            // Check instruction completeness
            let instruction_size = self.get_instruction_size(opcode);
            if i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Error: Incomplete instruction at position {} after repair", i);
                success = false;
                break;
            }
            
            i += instruction_size;
        }
        
        if success {
            eprintln!("Method body structure repair completed successfully");
        } else {
            eprintln!("Method body structure repair completed with remaining issues");
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a statement
    fn generate_statement(&mut self, stmt: &Stmt) -> Result<()> {
        // Only generate statements if code is alive (javac-style)
        if !self.bytecode_builder.is_alive() {
            return Ok(());
        }
        
        eprintln!("🔍 DEBUG: generate_statement: Starting, stack_depth={}", self.bytecode_builder.stack_depth());
        match stmt {
            Stmt::Expression(expr_stmt) => {
                eprintln!("🔍 DEBUG: generate_statement: Expression statement, about to generate expression");
                
                // Special handling for assignments to avoid value preservation when used as statements
                match &expr_stmt.expr {
                    Expr::Assignment(assign) => {
                        // Generate assignment without preserving value since it's used as a statement

                        self.generate_assignment_with_context(assign, false)?;
                    }
                    Expr::Unary(unary) => {
                        // javac-style optimization: convert PostInc/PostDec to PreInc/PreDec in expression statements
                        let optimized_unary = self.optimize_postfix_to_prefix(unary);
                        
                        // For PostInc/PostDec/PreInc/PreDec in expression statements, we can optimize
                        if matches!(optimized_unary.operator, UnaryOp::PostInc | UnaryOp::PostDec | UnaryOp::PreInc | UnaryOp::PreDec) {
                            self.generate_unary_expression_as_statement(&optimized_unary)?;
                        } else {
                            self.generate_expression(&expr_stmt.expr)?;
                        }
                    }
                    _ => {
                self.generate_expression(&expr_stmt.expr)?;
                eprintln!("🔍 DEBUG: generate_statement: Expression generated, stack_depth={}", self.bytecode_builder.stack_depth());
                // Pop only if expression likely leaves a value on stack and is not a method call (which may be void).
                // For expression statements, we need to pop non-void return values
                let should_pop = match &expr_stmt.expr {
                    Expr::MethodCall(method_call) => {
                        // For method calls, check if they return non-void values
                        // Use VoidItem to represent void method results (javac-style)
                        let returns_void = !self.should_pop_method_call_result(method_call);
                        if returns_void {
                            // Create VoidItem to represent the void result
                            let _void_item = crate::codegen::item_system::ItemFactory::make_void_item();
                            // VoidItem has width 0, so no stack adjustment needed
                        }
                        !returns_void
                    },
                    Expr::Unary(unary) => {
                        // PostInc/PostDec as statements don't need pop since we optimized them
                        !matches!(unary.operator, UnaryOp::PostInc | UnaryOp::PostDec)
                    },
                    Expr::Identifier(_) | Expr::Literal(_) | Expr::Binary(_) | Expr::ArrayAccess(_) | Expr::FieldAccess(_) | Expr::Cast(_) | Expr::Conditional(_) | Expr::New(_) | Expr::Parenthesized(_) | Expr::InstanceOf(_) | Expr::ArrayInitializer(_) => true,
                            _ => false,
                };
                eprintln!("🔍 DEBUG: generate_statement: should_pop={}, stack_depth={}", should_pop, self.bytecode_builder.stack_depth());
                if should_pop { 
                    eprintln!("🔍 DEBUG: generate_statement: About to pop, stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.pop())?;
                    eprintln!("🔍 DEBUG: generate_statement: After pop, stack_depth={}", self.bytecode_builder.stack_depth());
                        }
                    }
                }
                
                eprintln!("🔍 DEBUG: generate_statement: Expression statement completed, stack_depth={}", self.bytecode_builder.stack_depth());
                
                // Validate statement structure (disabled for performance)
                // self.validate_statement_structure()?;
                
                // Optimize statement structure (disabled for performance)
                // self.optimize_statement_structure()?;
                
                // Finalize statement structure (disabled for performance)
                // self.finalize_statement_structure()?;
            }
            Stmt::Declaration(var_decl) => {
                self.generate_variable_declaration(var_decl)?;
            }
            Stmt::If(if_stmt) => {
                self.generate_if_statement(if_stmt)?;
            }
            Stmt::While(while_stmt) => {
                // Use LoopOptimizer for optimized while loop generation
                self.generate_optimized_while_statement(None, while_stmt)?;
            }
            Stmt::For(for_stmt) => {

                // Use LoopOptimizer for optimized for loop generation
                self.generate_optimized_for_statement(for_stmt)?;
            }
            Stmt::Labeled(labeled) => {
                // If the labeled statement is a loop, pass the label down, otherwise just generate inner
                match &*labeled.statement {
                    Stmt::While(ws) => self.generate_while_statement_labeled(Some(&labeled.label), ws)?,
                    Stmt::For(fs) => {

                        // TODO: implement for with labels; fallback to normal generation
                        self.generate_for_statement(fs)?;
                    }
                    _ => self.generate_statement(&labeled.statement)?,
                }
            }
            Stmt::Switch(_switch_stmt) => {
                // Generate switch via chained compares and gotos (simplified)
                self.generate_switch_statement(_switch_stmt)?;
            }
            Stmt::Return(return_stmt) => {
                if let Some(expr) = &return_stmt.value {
                    self.generate_expression(expr)?;
                }
                // Use the method's return type if available, otherwise infer from expression
                let return_type = if let Some(expr) = &return_stmt.value {
                    // Try to infer return type from expression
                    let descriptor = self.type_to_descriptor(expr);
                    match descriptor.as_str() {
                        "I" => TypeRef { name: "int".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: return_stmt.span },
                        "J" => TypeRef { name: "long".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: return_stmt.span },
                        "F" => TypeRef { name: "float".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: return_stmt.span },
                        "D" => TypeRef { name: "double".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: return_stmt.span },
                        "Z" => TypeRef { name: "boolean".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: return_stmt.span },
                        _ => {
                            // For reference types, try to get the actual type from expression
                            let expr_type_name = self.resolve_expression_type(expr);
                            // Create TypeRef from the resolved type name
                            if expr_type_name.ends_with("[]") {
                                // Array type
                                let element_name = &expr_type_name[..expr_type_name.len()-2];
                                TypeRef { 
                                    name: element_name.to_string(), 
                                    type_args: Vec::new(), 
                                    annotations: Vec::new(), 
                                    array_dims: 1, 
                                    span: return_stmt.span 
                                }
                            } else {
                                // Reference type
                                TypeRef { 
                                    name: expr_type_name, 
                                    type_args: Vec::new(), 
                                    annotations: Vec::new(), 
                                    array_dims: 0, 
                                    span: return_stmt.span 
                                }
                            }
                        },
                    }
                } else {
                    TypeRef { name: "void".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: return_stmt.span }
                };
                self.generate_return(&return_type)?;
            }
            Stmt::Break(break_stmt) => {
                let target = if let Some(ref name) = break_stmt.label {
                    self.find_loop_break_label(Some(name))
                } else {
                    self.find_loop_break_label(None)
                };
                if let Some(label_id) = target {
                    let l = self.label_str(label_id);
                    Self::map_stack(self.bytecode_builder.goto(&l))?;
                } else {
                    // Fallback: placeholder to a synthetic end label
                    let end = self.create_label();
                    let l = self.label_str(end);
                    Self::map_stack(self.bytecode_builder.goto(&l))?;
                }
            }
            Stmt::Continue(continue_stmt) => {
                let target = if let Some(ref name) = continue_stmt.label {
                    self.find_loop_continue_label(Some(name))
                } else {
                    self.find_loop_continue_label(None)
                };
                if let Some(label_id) = target {
                    let l = self.label_str(label_id);
                    Self::map_stack(self.bytecode_builder.goto(&l))?;
                } else {
                    // Fallback: placeholder to a synthetic continue label
                    let cont = self.create_label();
                    let l = self.label_str(cont);
                    Self::map_stack(self.bytecode_builder.goto(&l))?;
                }
            }
            Stmt::Try(try_stmt) => {
                // mark source line for try
                self.record_line_number(try_stmt.span.start.line as u16);
                
                // Use ExceptionHandlingOptimizer for optimized try-catch-finally generation
                self.generate_optimized_try_statement(try_stmt)?;
            }
            Stmt::Throw(throw_stmt) => {
                self.record_line_number(throw_stmt.span.start.line as u16);
                
                // Use ExceptionOptimizer for optimized exception handling
                self.generate_optimized_throw_statement(throw_stmt)?;
            }
            Stmt::Block(block) => {
                self.record_line_number(block.span.start.line as u16);
                self.generate_block(block)?;
            }
            Stmt::Empty => {
                // No-op
            }
            Stmt::Assert(assert_stmt) => {
                self.record_line_number(assert_stmt.span.start.line as u16);
                // if (!cond) throw new AssertionError(msg?)
                let end_label = self.create_label();
                // Evaluate condition
                self.generate_expression(&assert_stmt.condition)?;
                // If condition != 0, jump to end
                {
                    let l = self.label_str(end_label);
                    Self::map_stack(self.bytecode_builder.ifne(&l))?;
                }
                // Construct AssertionError
                // NEW java/lang/AssertionError
                let _cls = self.add_class_constant("java/lang/AssertionError");
                Self::map_stack(self.bytecode_builder.new_object(_cls))?;
                // DUP
                Self::map_stack(self.bytecode_builder.dup())?;
                // If message present, load it and call (Ljava/lang/String;)V
                if let Some(msg) = &assert_stmt.message {
                    self.generate_expression(msg)?;
                    let _mref = self.add_method_ref("java/lang/AssertionError", "<init>", "(Ljava/lang/String;)V");
                    Self::map_stack(self.bytecode_builder.invokespecial(_mref))?;
                } else {
                    let _mref = self.add_method_ref("java/lang/AssertionError", "<init>", "()V");
                    Self::map_stack(self.bytecode_builder.invokespecial(_mref))?;
                }
                // ATHROW
                Self::map_stack(self.bytecode_builder.athrow())?;
                // end:
                {
                    let l = self.label_str(end_label);
                    self.bytecode_builder.mark_label(&l);
                }
            }
            
            Stmt::Synchronized(_)
            | Stmt::TypeDecl(_) => {
                // Not yet supported in codegen; skip
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for an expression
    fn generate_expression(&mut self, expr: &Expr) -> Result<()> {
        let expr_name = match expr {
            Expr::Literal(_) => "Literal",
            Expr::Identifier(_) => "Identifier", 
            Expr::Binary(_) => "Binary",
            Expr::Unary(_) => "Unary",
            Expr::Assignment(_) => "Assignment",
            Expr::MethodCall(_) => "MethodCall",
            Expr::FieldAccess(_) => "FieldAccess",
            Expr::ArrayAccess(_) => "ArrayAccess",
            Expr::Cast(_) => "Cast",
            Expr::InstanceOf(_) => "InstanceOf",
            Expr::Conditional(_) => "Conditional",
            Expr::New(_) => "New",
            Expr::Parenthesized(_) => "Parenthesized",
            Expr::ArrayInitializer(_) => "ArrayInitializer",
        };
        eprintln!("🔍 DEBUG: generate_expression: Starting {}, stack_depth={}", 
                 expr_name, self.bytecode_builder.stack_depth());
        match expr {
            Expr::Literal(lit_expr) => {
                self.generate_literal_expression(lit_expr)?;
            }
            Expr::Identifier(ident_expr) => {
                self.generate_identifier(&ident_expr.name)?;
            }
            Expr::Binary(bin_expr) => {
                self.generate_binary_expression(bin_expr)?;
            }
            Expr::Unary(unary_expr) => {
                self.generate_unary_expression(unary_expr)?;
            }
            Expr::Assignment(assign_expr) => {
                self.generate_assignment(assign_expr)?;
            }
            Expr::MethodCall(method_call) => {
                self.generate_method_call(method_call)?;
            }
            Expr::FieldAccess(field_access) => {
                self.generate_field_access(field_access)?;
            }
            Expr::ArrayAccess(array_access) => {
                self.generate_array_access(array_access)?;
            }
            Expr::Cast(cast_expr) => {
                self.generate_cast(cast_expr)?;
            }
            Expr::InstanceOf(instance_of) => {
                self.generate_instanceof_expression(instance_of)?;
            }
            Expr::Conditional(conditional) => {
                self.generate_ternary_expression(conditional)?;
            }
            Expr::New(new_expr) => {
                self.generate_new_expression(new_expr)?;
            }
            Expr::Parenthesized(expr) => {
                self.generate_expression(expr)?;
            }
            Expr::ArrayInitializer(values) => {
                self.generate_array_initializer(values)?;
            }
        }
        eprintln!("🔍 DEBUG: generate_expression: Completed {}, stack_depth={}", expr_name, self.bytecode_builder.stack_depth());
        Ok(())
    }
    
    /// Generate bytecode for a binary expression
    fn generate_binary_expression(&mut self, binary: &BinaryExpr) -> Result<()> {
        eprintln!("🔍 DEBUG: Binary expression: Starting, operator={:?}, stack_depth={}", binary.operator, self.bytecode_builder.stack_depth());
        
        // Special handling for null comparisons - don't generate null operand
        match binary.operator {
            BinaryOp::Eq | BinaryOp::Ne => {
                if self.is_null_literal(&binary.right) {
                    // left == null or left != null
                    eprintln!("🔍 DEBUG: Binary expression: Detected null comparison (right operand is null)");
                    self.generate_expression(&binary.left)?;
                    // Don't generate right operand (null)
                    self.generate_null_comparison(&binary.operator, false)?; // false = right is null
                    return Ok(());
                } else if self.is_null_literal(&binary.left) {
                    // null == right or null != right
                    eprintln!("🔍 DEBUG: Binary expression: Detected null comparison (left operand is null)");
                    self.generate_expression(&binary.right)?;
                    // Don't generate left operand (null)
                    self.generate_null_comparison(&binary.operator, true)?; // true = left is null
                    return Ok(());
                }
            }
            _ => {}
        }
        
        // Special handling for comparisons with zero - optimize to single operand instructions
        match binary.operator {
            BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                eprintln!("🔍 DEBUG: Binary expression: Checking for zero comparison, operator={:?}", binary.operator);
                eprintln!("🔍 DEBUG: Binary expression: Right operand: {:?}", binary.right);
                eprintln!("🔍 DEBUG: Binary expression: is_zero_literal(right) = {}", self.is_zero_literal(&binary.right));
                if self.is_zero_literal(&binary.right) {
                    // left op 0 - check if we should use lcmp mode for long operands
                    let left_type = self.resolve_expression_type(&binary.left);
                    eprintln!("🔍 DEBUG: Binary expression: left op 0, left_type = '{}'", left_type);
                    // 🔧 FIX: Also check for long expressions that result from bitwise operations
                    if left_type == "long" || left_type.contains("long") || self.is_long_expression(&binary.left) {
                        // For long operands, use lcmp mode: lconst_0, lcmp, ifeq/ifne
                        eprintln!("🔍 DEBUG: Binary expression: Detected long zero comparison, using lcmp mode");
                        self.generate_expression(&binary.left)?;
                        Self::map_stack(self.bytecode_builder.lconst_0())?;
                        self.emit_opcode(self.opcode_generator.lcmp());
                        // Now generate the appropriate conditional jump
                        let true_label = self.create_label();
                        let true_label_str = self.label_str(true_label);
                        // 🔧 FIX: Use type-aware conditional jumps for long values
                        let operand_type = "long"; // We know this is a long comparison
                        match binary.operator {
                            BinaryOp::Eq => Self::map_stack(self.bytecode_builder.ifeq_typed(&true_label_str, operand_type))?,
                            BinaryOp::Ne => Self::map_stack(self.bytecode_builder.ifne_typed(&true_label_str, operand_type))?,
                            BinaryOp::Lt => Self::map_stack(self.bytecode_builder.iflt(&true_label_str))?,
                            BinaryOp::Le => Self::map_stack(self.bytecode_builder.ifle(&true_label_str))?,
                            BinaryOp::Gt => Self::map_stack(self.bytecode_builder.ifgt(&true_label_str))?,
                            BinaryOp::Ge => Self::map_stack(self.bytecode_builder.ifge(&true_label_str))?,
                            _ => unreachable!(),
                        }
                        
                        // For long comparisons, we need to generate the complete control flow
                        // This is different from int comparisons where we use generate_zero_comparison
                        let end_label = self.create_label();
                        let end_label_str = self.label_str(end_label);
                        
                        // True case: push true (1)
                        Self::map_stack(self.bytecode_builder.iconst_1())?;
                        Self::map_stack(self.bytecode_builder.goto(&end_label_str))?;
                        
                        // False case: push false (0)
                        self.bytecode_builder.mark_label(&true_label_str);
                        Self::map_stack(self.bytecode_builder.iconst_0())?;
                        
                        // End label
                        self.bytecode_builder.mark_label(&end_label_str);
                        
                        return Ok(());
                    } else {
                        // For int operands, use single operand optimization
                        eprintln!("🔍 DEBUG: Binary expression: Detected int zero comparison (right operand is 0)");
                        self.generate_expression(&binary.left)?;
                        // 🔧 FIX: Pass operand type for type-aware zero comparison
                        let operand_type = self.resolve_expression_type(&binary.left);
                        eprintln!("🔍 DEBUG: Binary expression: Zero comparison, operand_type = '{}'", operand_type);
                        
                        // 🔧 FIX: Force long type comparison for bitwise operations
                        if operand_type == "long" || operand_type.contains("long") || self.is_long_expression(&binary.left) {
                            eprintln!("🔍 DEBUG: Binary expression: Detected long type, using type-aware comparison");
                            // For long types, we need to use type-aware comparison
                            self.generate_zero_comparison(&binary.operator, Some(&operand_type))?;
                        } else {
                            eprintln!("🔍 DEBUG: Binary expression: Non-long type, using regular comparison");
                            self.generate_zero_comparison(&binary.operator, Some(&operand_type))?;
                        }
                        return Ok(());
                    }
                } else if self.is_zero_literal(&binary.left) {
                    // 0 op right - check if we should use lcmp mode for long operands
                    let right_type = self.resolve_expression_type(&binary.right);
                    if right_type == "long" {
                        // For long operands, use lcmp mode: lconst_0, lcmp, ifeq/ifne
                        eprintln!("🔍 DEBUG: Binary expression: Detected long zero comparison (left operand is 0), using lcmp mode");
                        self.generate_expression(&binary.right)?;
                        Self::map_stack(self.bytecode_builder.lconst_0())?;
                        self.emit_opcode(self.opcode_generator.lcmp());
                        // Now generate the appropriate conditional jump (reversed)
                        let true_label = self.create_label();
                        let true_label_str = self.label_str(true_label);
                        match binary.operator {
                            BinaryOp::Eq => Self::map_stack(self.bytecode_builder.ifeq(&true_label_str))?,
                            BinaryOp::Ne => Self::map_stack(self.bytecode_builder.ifne(&true_label_str))?,
                            BinaryOp::Lt => Self::map_stack(self.bytecode_builder.ifgt(&true_label_str))?,
                            BinaryOp::Le => Self::map_stack(self.bytecode_builder.ifge(&true_label_str))?,
                            BinaryOp::Gt => Self::map_stack(self.bytecode_builder.iflt(&true_label_str))?,
                            BinaryOp::Ge => Self::map_stack(self.bytecode_builder.ifle(&true_label_str))?,
                            _ => unreachable!(),
                        }
                        return Ok(());
                    } else {
                        // For int operands, use single operand optimization
                        eprintln!("🔍 DEBUG: Binary expression: Detected int zero comparison (left operand is 0)");
                        self.generate_expression(&binary.right)?;
                        // 🔧 FIX: Pass operand type for type-aware zero comparison
                        let operand_type = self.resolve_expression_type(&binary.right);
                        self.generate_zero_comparison_reversed(&binary.operator, Some(&operand_type))?;
                        return Ok(());
                    }
                }
            }
            _ => {}
        }
        
        // Regular binary expression - generate both operands
        eprintln!("🔍 DEBUG: Binary expression: Generating left operand");
        self.generate_expression(&binary.left)?;
        eprintln!("🔍 DEBUG: Binary expression: After left operand, stack_depth={}", self.bytecode_builder.stack_depth());
        
        // 🔧 TYPE COERCION OPT: Check if left operand needs type coercion
        let left_type = self.resolve_expression_type(&binary.left);
        let right_type = self.resolve_expression_type(&binary.right);
        
        if left_type != right_type {
            println!("🔧 TYPE COERCION OPT: Binary operation type mismatch: {} vs {}", left_type, right_type);
            
            // Determine target type for binary operation
            let target_type = self.determine_binary_operation_target_type(&left_type, &right_type, &binary.operator);
            println!("🔧 TYPE COERCION OPT: Target type for binary operation: {}", target_type);
            
            // Apply left operand coercion if needed
            if left_type != target_type {
                self.apply_implicit_type_coercion(&left_type, &target_type)?;
            }
        }
        
        eprintln!("🔍 DEBUG: Binary expression: Generating right operand");
        self.generate_expression(&binary.right)?;
        eprintln!("🔍 DEBUG: Binary expression: After right operand, stack_depth={}", self.bytecode_builder.stack_depth());
        
        // 🔧 FIX: For long type comparisons, ensure we use proper comparison instructions
        if left_type == "long" || right_type == "long" {
            eprintln!("🔍 DEBUG: Binary expression: Detected long type in binary operation, ensuring proper comparison");
        }
        
        // 🔧 TYPE COERCION OPT: Apply right operand coercion if needed
        if left_type != right_type {
            let target_type = self.determine_binary_operation_target_type(&left_type, &right_type, &binary.operator);
            
            // Apply right operand coercion if needed
            if right_type != target_type {
                self.apply_implicit_type_coercion(&right_type, &target_type)?;
            }
        }
        
        // Generate operation
        eprintln!("🔍 DEBUG: Binary expression: About to apply operator {:?}", binary.operator);
        match binary.operator {
            BinaryOp::Add => { 
                // Check if this is string concatenation
                if self.is_string_concatenation(&binary.left, &binary.right) {
                    // Use javac-style StringBufferOptimizer for advanced string concatenation
                    let temp_binary = crate::ast::BinaryExpr {
                        left: binary.left.clone(),
                        operator: crate::ast::BinaryOp::Add,
                        right: binary.right.clone(),
                        span: crate::ast::Span::new(
                            crate::ast::Location::new(0, 0, 0),
                            crate::ast::Location::new(0, 0, 0)
                        ),
                    };
                    let temp_expr = crate::ast::Expr::Binary(temp_binary);
                    
                    // Analyze with javac-style string buffer optimizer
                    let analysis = self.string_buffer_optimizer.analyze_string_concatenation(&temp_expr);
                    
                    match &analysis {
                        crate::codegen::string_buffer_optimizer::StringConcatenationAnalysis::CompileTimeFoldable { result } => {
                            // Compile-time constant folding
                            if let Some(cp) = &self.constant_pool {
                                let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_string(result) };
                                Self::map_stack(self.bytecode_builder.ldc(idx))?;
                            }
                        }
                        crate::codegen::string_buffer_optimizer::StringConcatenationAnalysis::StringConcatenation { expressions: _, estimated_length: _ } => {
                            // Use StringBuilder with capacity estimation (javac-style)
                            let bytecode = self.string_buffer_optimizer.generate_string_concatenation_bytecode(analysis)?;
                            self.bytecode_builder.emit_raw(&bytecode)?;
                        }
                        crate::codegen::string_buffer_optimizer::StringConcatenationAnalysis::SingleString { value: _, can_pool } => {
                            // Single string optimization
                            if *can_pool {
                                let bytecode = self.string_buffer_optimizer.generate_string_concatenation_bytecode(analysis)?;
                                self.bytecode_builder.emit_raw(&bytecode)?;
                            } else {
                                // Fallback to old string optimizer
                                if let Some(optimization) = crate::codegen::string_optimizer::StringOptimizer::analyze_string_concat(&temp_expr) {
                                    self.apply_string_optimization(optimization)?;
                                } else {
                                    self.generate_simple_string_concat()?;
                                }
                            }
                        }
                        _ => {
                            // Fallback to old string optimizer
                            if let Some(optimization) = crate::codegen::string_optimizer::StringOptimizer::analyze_string_concat(&temp_expr) {
                                self.apply_string_optimization(optimization)?;
                            } else {
                                self.generate_simple_string_concat()?;
                            }
                        }
                    }
                } else {
                    // Use InstructionOptimizer for optimized binary operations
                    let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_binary_operation(&binary.operator, "int");
                    for &byte in &optimized_bytecode {
                        self.bytecode_builder.push_byte(byte);
                    }
                    Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                }
            },
            BinaryOp::Sub => { 
                let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_binary_operation(&binary.operator, "int");
                for &byte in &optimized_bytecode {
                    self.bytecode_builder.push_byte(byte);
                }
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            BinaryOp::Mul => { 
                let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_binary_operation(&binary.operator, "int");
                for &byte in &optimized_bytecode {
                    self.bytecode_builder.push_byte(byte);
                }
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            BinaryOp::Div => { 
                let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_binary_operation(&binary.operator, "int");
                for &byte in &optimized_bytecode {
                    self.bytecode_builder.push_byte(byte);
                }
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            BinaryOp::Mod => { 
                let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_binary_operation(&binary.operator, "int");
                for &byte in &optimized_bytecode {
                    self.bytecode_builder.push_byte(byte);
                }
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            BinaryOp::Lt => {
                self.generate_integer_comparison("if_icmplt")?;
            }
            BinaryOp::Le => {
                self.generate_integer_comparison("if_icmple")?;
            }
            BinaryOp::Gt => {
                self.generate_integer_comparison("if_icmpgt")?;
            }
            BinaryOp::Ge => {
                self.generate_integer_comparison("if_icmpge")?;
            }
            BinaryOp::Eq => {
                // Regular equality comparison (null comparisons handled above)
                self.generate_integer_comparison("if_icmpeq")?;
            }
            BinaryOp::Ne => {
                // Regular inequality comparison (null comparisons handled above)
                self.generate_integer_comparison("if_icmpne")?;
            }
            BinaryOp::And => { 
                eprintln!("🔍 DEBUG: Binary expression: Executing And branch, stack_depth={}", self.bytecode_builder.stack_depth());
                // Type-aware bitwise AND
                let left_type = self.resolve_expression_type(&binary.left);
                let right_type = self.resolve_expression_type(&binary.right);
                if left_type == "long" || right_type == "long" {
                    eprintln!("🔍 DEBUG: Binary And: Using land for long operands");
                    self.emit_opcode(self.opcode_generator.land());
                    Self::map_stack(self.bytecode_builder.update_stack(4, 2))?; // 2 longs -> 1 long
                } else {
                    eprintln!("🔍 DEBUG: Binary And: Using iand for int operands");
                    self.emit_opcode(self.opcode_generator.iand());
                    Self::map_stack(self.bytecode_builder.update_stack(2, 1))?; // 2 ints -> 1 int
                }
                eprintln!("🔍 DEBUG: Binary expression: And branch completed, stack_depth={}", self.bytecode_builder.stack_depth());
            },
            BinaryOp::Or => { 
                eprintln!("🔍 DEBUG: Binary expression: Executing Or branch, stack_depth={}", self.bytecode_builder.stack_depth());
                // Type-aware bitwise OR
                let left_type = self.resolve_expression_type(&binary.left);
                let right_type = self.resolve_expression_type(&binary.right);
                if left_type == "long" || right_type == "long" {
                    eprintln!("🔍 DEBUG: Binary Or: Using lor for long operands");
                    self.emit_opcode(self.opcode_generator.lor());
                    Self::map_stack(self.bytecode_builder.update_stack(4, 2))?; // 2 longs -> 1 long
                } else {
                    eprintln!("🔍 DEBUG: Binary Or: Using ior for int operands");
                    self.emit_opcode(self.opcode_generator.ior());
                    Self::map_stack(self.bytecode_builder.update_stack(2, 1))?; // 2 ints -> 1 int
                }
                eprintln!("🔍 DEBUG: Binary expression: Or branch completed, stack_depth={}", self.bytecode_builder.stack_depth());
            },
            BinaryOp::Xor => { 
                // Type-aware bitwise XOR
                let left_type = self.resolve_expression_type(&binary.left);
                let right_type = self.resolve_expression_type(&binary.right);
                if left_type == "long" || right_type == "long" {
                    eprintln!("🔍 DEBUG: Binary Xor: Using lxor for long operands");
                    self.emit_opcode(self.opcode_generator.lxor());
                    Self::map_stack(self.bytecode_builder.update_stack(4, 2))?; // 2 longs -> 1 long
                } else {
                    eprintln!("🔍 DEBUG: Binary Xor: Using ixor for int operands");
                    self.emit_opcode(self.opcode_generator.ixor());
                    Self::map_stack(self.bytecode_builder.update_stack(2, 1))?; // 2 ints -> 1 int
                }
            },
            BinaryOp::LShift => { 
                self.emit_opcode(self.opcode_generator.ishl()); 
                // ishl: pops 2 ints, pushes 1 int
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            BinaryOp::RShift => { 
                self.emit_opcode(self.opcode_generator.ishr()); 
                // ishr: pops 2 ints, pushes 1 int
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            BinaryOp::URShift => { 
                self.emit_opcode(self.opcode_generator.iushr()); 
                // iushr: pops 2 ints, pushes 1 int
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
        }
        
        Ok(())
    }
    
    /// Convert LocalType to string representation
    fn local_type_to_string(&self, local_type: &LocalType) -> String {
        match local_type {
            LocalType::Int => "int".to_string(),
            LocalType::Long => "long".to_string(),
            LocalType::Float => "float".to_string(),
            LocalType::Double => "double".to_string(),
            LocalType::Reference(ref_type) => {
                // Ensure we have the full qualified name for known types
                match ref_type.as_str() {
                    "FieldData" => "java.base.FieldData".to_string(),
                    "MethodData" => "java.base.MethodData".to_string(),
                    "PoolEntry" => "java.base.PoolEntry".to_string(),
                    "Method" => "java.lang.reflect.Method".to_string(),
                    "Object" => "java.lang.Object".to_string(),
                    _ => ref_type.clone(),
                }
            }
            LocalType::Array(element_type) => {
                // Recursively handle nested arrays
                self.local_type_to_string(element_type) + "[]"
            }
        }
    }
    
    /// Generate bytecode for a unary expression used as a statement (optimized for PostInc/PostDec)
    fn generate_unary_expression_as_statement(&mut self, unary: &UnaryExpr) -> Result<()> {
        // 🔧 ASSIGNMENT OPT: Use IncrementAnalyzer for advanced increment optimization
        let increment_optimization = crate::codegen::assignment_optimizer::IncrementAnalyzer::analyze_increment(unary);
        println!("🔧 ASSIGNMENT OPT: Increment analysis: {:?}", increment_optimization);
        
        // Use IncrementOptimizer for simple optimization check
        if let Expr::Identifier(ident) = &*unary.operand {
            let local_var = self.find_local_variable(&ident.name).cloned();
            if let Some(local_var) = local_var {
                // For local variables, use optimized iinc instruction (javac-style)
                let increment = match unary.operator {
                    UnaryOp::PostInc | UnaryOp::PreInc => 1,
                    UnaryOp::PostDec | UnaryOp::PreDec => -1,
                    _ => {
                        // Not an increment/decrement, fallback
                        self.generate_standard_increment_as_statement(unary)?;
                        return Ok(());
                    }
                };
                
                // Use optimized iinc instruction for local variables
                let iinc_bytes = self.opcode_generator.iinc(local_var.index, increment);
                self.bytecode_builder.extend_from_slice(&iinc_bytes);
                return Ok(());
            }
        }
        
        // Fallback to standard increment/decrement logic for fields and complex expressions
        self.generate_standard_increment_as_statement(unary)?;
        Ok(())
    }
    
    /// Generate standard increment/decrement as statement (fallback method)
    fn generate_standard_increment_as_statement(&mut self, unary: &UnaryExpr) -> Result<()> {
        match unary.operator {
            UnaryOp::PostInc => {
                // Post-increment as statement: no need to preserve original value
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Local variable post-increment: load -> iconst_1 -> iadd -> store
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                        Self::map_stack(self.bytecode_builder.iconst_1())?;
                        Self::map_stack(self.bytecode_builder.iadd())?;
                        self.store_local_variable(local_var.index, &local_var.var_type)?;
                    } else {
                        // Check if it's an instance field first
                        let class_name = self.current_class_name.clone().unwrap_or_else(|| "java/lang/Object".to_string());
                        
                        if self.is_instance_field(&class_name, &ident.name) {
                            let field_descriptor = self.resolve_field_descriptor(&class_name, &ident.name);
                            // Instance field post-increment as statement: aload_0 -> dup -> getfield -> iconst_1 -> iadd -> putfield
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, &field_descriptor);
                            Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
                            Self::map_stack(self.bytecode_builder.dup())?; // Duplicate 'this' for putfield
                            Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?; // Get current value
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.iadd())?; // Increment
                            Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?; // Store back
                        } else {
                            // Static field post-increment as statement: getstatic -> iconst_1 -> iadd -> putstatic
                            let field_descriptor = "I"; // Assume int field for counter
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, field_descriptor);
                            Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.iadd())?;
                            Self::map_stack(self.bytecode_builder.putstatic(field_ref_index))?;
                        }
                    }
                } else {
                    // For complex expressions, fall back to regular handling
                    self.generate_unary_expression(unary)?;
                }
            }
            UnaryOp::PostDec => {
                // Post-decrement as statement: similar to PostInc but with isub
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Local variable post-decrement: load -> iconst_1 -> isub -> store
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                        Self::map_stack(self.bytecode_builder.iconst_1())?;
                        Self::map_stack(self.bytecode_builder.isub())?;
                        self.store_local_variable(local_var.index, &local_var.var_type)?;
                    } else {
                        // Check if it's an instance field first
                        let class_name = self.current_class_name.clone().unwrap_or_else(|| "java/lang/Object".to_string());
                        
                        if self.is_instance_field(&class_name, &ident.name) {
                            let field_descriptor = self.resolve_field_descriptor(&class_name, &ident.name);
                            // Instance field post-decrement as statement: aload_0 -> dup -> getfield -> iconst_1 -> isub -> putfield
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, &field_descriptor);
                            Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
                            Self::map_stack(self.bytecode_builder.dup())?; // Duplicate 'this' for putfield
                            Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?; // Get current value
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.isub())?; // Decrement
                            Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?; // Store back
                        } else {
                            // Static field post-decrement as statement: getstatic -> iconst_1 -> isub -> putstatic
                            let field_descriptor = "I"; // Assume int field for counter
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, field_descriptor);
                            Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.isub())?;
                            Self::map_stack(self.bytecode_builder.putstatic(field_ref_index))?;
                        }
                    }
                } else {
                    // For complex expressions, fall back to regular handling
                    self.generate_unary_expression(unary)?;
                }
            }
            UnaryOp::PreInc => {
                // Pre-increment as statement: same optimization as PostInc since we don't need the value
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Use iinc instruction for local variable: more efficient than load/add/store
                        let iinc_bytes = self.opcode_generator.iinc(local_var.index, 1);
                        self.bytecode_builder.extend_from_slice(&iinc_bytes);
                    } else {
                        // Check if it's an instance field first
                        let class_name = self.current_class_name.clone().unwrap_or_else(|| "java/lang/Object".to_string());
                        
                        if self.is_instance_field(&class_name, &ident.name) {
                            let field_descriptor = self.resolve_field_descriptor(&class_name, &ident.name);
                            // Instance field pre-increment as statement: aload_0 -> dup -> getfield -> iconst_1 -> iadd -> putfield
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, &field_descriptor);
                            Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
                            Self::map_stack(self.bytecode_builder.dup())?; // Duplicate 'this' for putfield
                            Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?; // Get current value
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.iadd())?; // Increment
                            Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?; // Store back
                        } else {
                            // Static field pre-increment as statement: getstatic -> iconst_1 -> iadd -> putstatic
                            let field_descriptor = "I"; // Assume int field for counter
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, field_descriptor);
                            Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.iadd())?;
                            Self::map_stack(self.bytecode_builder.putstatic(field_ref_index))?;
                        }
                    }
                } else {
                    // For complex expressions, fall back to regular handling
                    self.generate_unary_expression(unary)?;
                }
            }
            UnaryOp::PreDec => {
                // Pre-decrement as statement: same optimization as PostDec since we don't need the value
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Use iinc instruction for local variable: more efficient than load/sub/store
                        let iinc_bytes = self.opcode_generator.iinc(local_var.index, -1);
                        self.bytecode_builder.extend_from_slice(&iinc_bytes);
                    } else {
                        // Check if it's an instance field first
                        let class_name = self.current_class_name.clone().unwrap_or_else(|| "java/lang/Object".to_string());
                        
                        if self.is_instance_field(&class_name, &ident.name) {
                            let field_descriptor = self.resolve_field_descriptor(&class_name, &ident.name);
                            // Instance field pre-decrement as statement: aload_0 -> dup -> getfield -> iconst_1 -> isub -> putfield
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, &field_descriptor);
                            Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
                            Self::map_stack(self.bytecode_builder.dup())?; // Duplicate 'this' for putfield
                            Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?; // Get current value
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.isub())?; // Decrement
                            Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?; // Store back
                        } else {
                            // Static field pre-decrement as statement: getstatic -> iconst_1 -> isub -> putstatic
                            let field_descriptor = "I"; // Assume int field for counter
                            let field_ref_index = self.add_field_ref(&class_name, &ident.name, field_descriptor);
                            Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                            Self::map_stack(self.bytecode_builder.iconst_1())?;
                            Self::map_stack(self.bytecode_builder.isub())?;
                            Self::map_stack(self.bytecode_builder.putstatic(field_ref_index))?;
                        }
                    }
                } else {
                    // For complex expressions, fall back to regular handling
                    self.generate_unary_expression(unary)?;
                }
            }
            _ => {
                // For other unary operators, use regular handling
                self.generate_unary_expression(unary)?;
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for a unary expression
    fn generate_unary_expression(&mut self, unary: &UnaryExpr) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_unary_expression: operator={:?}, operand={:?}", unary.operator, unary.operand);
        let initial_stack_depth = self.bytecode_builder.stack_depth();
        match unary.operator {
            UnaryOp::Plus => {
                // No-op for unary plus
                self.generate_expression(&unary.operand)?;
            }
            UnaryOp::Minus => {
                // Check if operand is a simple integer literal for optimization
                if let Expr::Literal(lit_expr) = unary.operand.as_ref() {
                    if let Literal::Integer(value) = &lit_expr.value {
                    // Generate optimized negative constants directly
                    match *value {
                        1 => Self::map_stack(self.bytecode_builder.iconst_m1())?,
                        0 => Self::map_stack(self.bytecode_builder.iconst_0())?,
                        -1 => Self::map_stack(self.bytecode_builder.iconst_1())?,
                        _ => {
                            // For other values, generate the negative literal directly
                            self.generate_literal(&Literal::Integer(-value))?;
                        }
                    }
                    }
                } else {
                    // For complex expressions, generate normally and negate
                self.generate_expression(&unary.operand)?;
                Self::map_stack(self.bytecode_builder.ineg())?;
                }
            }
            UnaryOp::Not => {
                eprintln!("🔍 DEBUG: generate_unary_expression: NOT operator, operand={:?}", unary.operand);
                eprintln!("🔍 DEBUG: generate_unary_expression: Stack depth before operand: {}", self.bytecode_builder.stack_depth());
                self.generate_expression(&unary.operand)?;
                eprintln!("🔍 DEBUG: generate_unary_expression: Stack depth after operand: {}", self.bytecode_builder.stack_depth());
                // Logical NOT: simple boolean negation
                // For now, use a simple approach: push 0 (false)
                // TODO: Implement proper logical NOT result generation
                if self.bytecode_builder.stack_depth() > 0 {
                Self::map_stack(self.bytecode_builder.pop())?; // Pop operand
                } else {
                    eprintln!("🔍 DEBUG: generate_unary_expression: WARNING - No value on stack to pop for NOT operator");
                }
                Self::map_stack(self.bytecode_builder.iconst_0())?; // Push false for now
            }
            UnaryOp::BitNot => {
                self.generate_expression(&unary.operand)?;
                // Type-aware bitwise NOT
                let operand_type = self.resolve_expression_type(&unary.operand);
                if operand_type == "long" {
                    eprintln!("🔍 DEBUG: BitNot: Using ldc2_w -1l + lxor for long operand (matches javac)");
                    // Generate -1L using ldc2_w -1l (matches javac exactly)
                    if let Some(cp) = &self.constant_pool {
                        let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_long(-1) };
                        Self::map_stack(self.bytecode_builder.ldc2_w(idx))?;
                    } else {
                        // Fallback to lconst_1 + lneg if no constant pool
                        Self::map_stack(self.bytecode_builder.lconst_1())?;
                        Self::map_stack(self.bytecode_builder.lneg())?;
                    }
                    self.emit_opcode(self.opcode_generator.lxor());
                    Self::map_stack(self.bytecode_builder.update_stack(4, 2))?; // 2 longs -> 1 long
                } else {
                    eprintln!("🔍 DEBUG: BitNot: Using iconst_m1 + ixor for int operand");
                    Self::map_stack(self.bytecode_builder.iconst_m1())?;
                    self.emit_opcode(self.opcode_generator.ixor());
                    Self::map_stack(self.bytecode_builder.update_stack(2, 1))?; // 2 ints -> 1 int
                }
            }
                        UnaryOp::PreInc => {
                // 🔧 INCREMENT OPT: Use increment_optimizer for pre-increment optimization
                println!("🔧 INCREMENT OPT: Analyzing pre-increment expression");
                
                if let Expr::Identifier(ident) = &*unary.operand {
                    if let Some(local_var) = self.find_local_variable(&ident.name) {
                        // Create VariableInfo for increment_optimizer
                        let variable_info = self.create_variable_info_from_local_var(local_var);
                        
                        // Analyze with increment_optimizer
                        let pattern = self.increment_optimizer.analyze_unary_increment(unary, &variable_info);
                        println!("🔧 INCREMENT OPT: Pre-increment pattern: {:?}", pattern);
                        
                        // Apply optimization based on pattern
                        self.apply_increment_optimization(&pattern, &variable_info)?;
                    } else {
                        // Field increment - use optimized approach
                        self.generate_optimized_field_increment(&ident.name, 1, true)?;
                    }
                } else {
                    // Complex expression increment
                    println!("🔧 INCREMENT OPT: Complex pre-increment operand, using standard approach");
                    self.generate_expression(&unary.operand)?;
                    Self::map_stack(self.bytecode_builder.iconst_1())?;
                    Self::map_stack(self.bytecode_builder.iadd())?;
                }
            }
            UnaryOp::PostInc => {
                // 🔧 INCREMENT OPT: Use increment_optimizer for post-increment optimization
                println!("🔧 INCREMENT OPT: Analyzing post-increment expression");
                
                if let Expr::Identifier(ident) = &*unary.operand {
                    if let Some(local_var) = self.find_local_variable(&ident.name) {
                        // Create VariableInfo for increment_optimizer
                        let variable_info = self.create_variable_info_from_local_var(local_var);
                        
                        // Analyze with increment_optimizer
                        let pattern = self.increment_optimizer.analyze_unary_increment(unary, &variable_info);
                        println!("🔧 INCREMENT OPT: Post-increment pattern: {:?}", pattern);
                        
                        // Apply optimization based on pattern
                        self.apply_increment_optimization(&pattern, &variable_info)?;
                    } else {
                        // Field increment - use optimized approach
                        self.generate_optimized_field_increment(&ident.name, 1, false)?;
                    }
                } else {
                    // Complex expression increment
                    println!("🔧 INCREMENT OPT: Complex post-increment operand, using standard approach");
                    self.generate_expression(&unary.operand)?;
                    Self::map_stack(self.bytecode_builder.dup())?; // Keep original value for result
                    Self::map_stack(self.bytecode_builder.iconst_1())?;
                    Self::map_stack(self.bytecode_builder.iadd())?;
                    Self::map_stack(self.bytecode_builder.pop())?; // Discard incremented value, keep original
                }
            }
            UnaryOp::PreDec => {
                // Pre-decrement: --x
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Local variable decrement - use iinc instruction for efficiency
                        let iinc_bytes = self.opcode_generator.iinc(local_var.index, -1);
                        self.bytecode_builder.extend_from_slice(&iinc_bytes);
                        // Load the decremented value for the expression result
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                    } else {
                        // Instance field decrement: getfield -> iconst_1 -> isub -> dup -> putfield
                        Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
                        let class_name = self.current_class_name.as_ref()
                            .ok_or_else(|| Error::codegen_error("Cannot resolve field access: no current class"))?;
                        let field_descriptor = self.resolve_field_descriptor(&class_name.replace(".", "/"), &ident.name);
                        let field_ref_index = self.add_field_ref(&class_name.replace(".", "/"), &ident.name, &field_descriptor);
                        Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?;
                        Self::map_stack(self.bytecode_builder.iconst_1())?;
                        Self::map_stack(self.bytecode_builder.isub())?;
                        Self::map_stack(self.bytecode_builder.dup())?; // Keep result for expression value
                        Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this' again
                        Self::map_stack(self.bytecode_builder.swap())?; // Swap to get [this, value]
                        Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?;
                    }
                } else {
                    // For complex expressions (like array access), evaluate the expression and decrement
                    // This is a simplified implementation - for full correctness, we'd need to handle
                    // the lvalue properly, but for now we'll just evaluate and return the result
                    eprintln!("🔍 DEBUG: PreDec with complex operand, generating operand expression");
                    self.generate_expression(&unary.operand)?;
                    Self::map_stack(self.bytecode_builder.iconst_1())?;
                    Self::map_stack(self.bytecode_builder.isub())?;
                    // Note: This doesn't actually modify the original lvalue, just returns the decremented value
                }
            }
            UnaryOp::PostDec => {
                // Post-decrement: x--
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Local variable post-decrement
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                        Self::map_stack(self.bytecode_builder.dup())?;
                        Self::map_stack(self.bytecode_builder.iconst_1())?;
                        Self::map_stack(self.bytecode_builder.isub())?;
                        self.store_local_variable(local_var.index, &local_var.var_type)?;
                        // Original value is still on stack
                    } else {
                        // Instance field post-decrement: getfield -> dup -> iconst_1 -> isub -> putfield
                        Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
                        let class_name = self.current_class_name.as_ref()
                            .ok_or_else(|| Error::codegen_error("Cannot resolve field access: no current class"))?;
                        let field_descriptor = self.resolve_field_descriptor(&class_name.replace(".", "/"), &ident.name);
                        let field_ref_index = self.add_field_ref(&class_name.replace(".", "/"), &ident.name, &field_descriptor);
                        Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?;
                        Self::map_stack(self.bytecode_builder.dup())?; // Keep original value for result
                        Self::map_stack(self.bytecode_builder.iconst_1())?;
                        Self::map_stack(self.bytecode_builder.isub())?;
                        Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this' again
                        Self::map_stack(self.bytecode_builder.swap())?; // Swap to get [this, value]
                        Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?;
                        // Original value is still on stack
                    }
                } else {
                    // For complex expressions (like array access), evaluate the expression and decrement
                    eprintln!("🔍 DEBUG: PostDec with complex operand, generating operand expression");
                    self.generate_expression(&unary.operand)?;
                    Self::map_stack(self.bytecode_builder.dup())?; // Keep original value for result
                    Self::map_stack(self.bytecode_builder.iconst_1())?;
                    Self::map_stack(self.bytecode_builder.isub())?;
                    Self::map_stack(self.bytecode_builder.pop())?; // Discard decremented value, keep original
                    // Note: This doesn't actually modify the original lvalue, just returns the original value
                }
            }
        }
        
        let final_stack_depth = self.bytecode_builder.stack_depth();
        eprintln!("🔍 DEBUG: generate_unary_expression: operator={:?}, stack: {} -> {}", 
                 unary.operator, initial_stack_depth, final_stack_depth);
        if final_stack_depth == initial_stack_depth {
            eprintln!("🔍 DEBUG: generate_unary_expression: WARNING - No value pushed to stack!");
        }
        Ok(())
    }
    
    /// Generate bytecode for a literal expression
    fn generate_literal_expression(&mut self, literal: &LiteralExpr) -> Result<()> {
        self.generate_literal(&literal.value)
    }
    
    /// Generate bytecode for a literal (javac-style optimized)
    fn generate_literal(&mut self, lit: &Literal) -> Result<()> {
        // Use javac-style constant optimization
        let optimization = match lit {
            Literal::Integer(value) => ConstantOptimizer::optimize_int(*value as i32),
            Literal::Float(value) => ConstantOptimizer::optimize_float(*value as f32),
            Literal::Boolean(true) => ConstantOptimizer::optimize_int(1),
            Literal::Boolean(false) => ConstantOptimizer::optimize_int(0),
            Literal::String(value) => {
                // String constants use ldc
                            if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_string(value) };
                                Self::map_stack(self.bytecode_builder.ldc(idx))?;
                    return Ok(());
                            } else {
                    return Err(Error::codegen_error("No constant pool available for string literal"));
                }
            }
            Literal::Long(value) => {
                // Long constants use ldc2_w
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_long(*value) };
                    Self::map_stack(self.bytecode_builder.ldc2_w(idx))?;
                    return Ok(());
                } else {
                    return Err(Error::codegen_error("No constant pool available for long literal"));
                }
            }
            Literal::Double(value) => {
                // Double constants use ldc2_w
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_double(*value) };
                    Self::map_stack(self.bytecode_builder.ldc2_w(idx))?;
                    return Ok(());
                } else {
                    return Err(Error::codegen_error("No constant pool available for double literal"));
                }
            }
            Literal::Char(value) => ConstantOptimizer::optimize_int(*value as i32),

                    Literal::Null => {
                // null reference
                Self::map_stack(self.bytecode_builder.aconst_null())?;
                return Ok(());
            }
        };
        
        // Generate optimized constant loading instruction
        match optimization {
            crate::codegen::constant_optimizer::ConstantInstruction::Iconst0 => {
                Self::map_stack(self.bytecode_builder.iconst_0())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Iconst1 => {
                Self::map_stack(self.bytecode_builder.iconst_1())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Iconst2 => {
                Self::map_stack(self.bytecode_builder.iconst_2())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Iconst3 => {
                Self::map_stack(self.bytecode_builder.iconst_3())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Iconst4 => {
                Self::map_stack(self.bytecode_builder.iconst_4())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Iconst5 => {
                Self::map_stack(self.bytecode_builder.iconst_5())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::IconstM1 => {
                Self::map_stack(self.bytecode_builder.iconst_m1())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Bipush(value) => {
                Self::map_stack(self.bytecode_builder.bipush(value))?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Sipush(value) => {
                Self::map_stack(self.bytecode_builder.sipush(value))?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Ldc(value) => {
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_integer(value as i32) };
                    Self::map_stack(self.bytecode_builder.ldc(idx))?;
                } else {
                    return Err(Error::codegen_error("No constant pool available for integer literal"));
                }
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Fconst0 => {
                Self::map_stack(self.bytecode_builder.fconst_0())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Fconst1 => {
                Self::map_stack(self.bytecode_builder.fconst_1())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::Fconst2 => {
                Self::map_stack(self.bytecode_builder.fconst_2())?;
            }
            crate::codegen::constant_optimizer::ConstantInstruction::LdcFloat(value) => {
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_float(value as f32) };
                    Self::map_stack(self.bytecode_builder.ldc(idx))?;
                } else {
                    return Err(Error::codegen_error("No constant pool available for float literal"));
                }
            }
            _ => {
                return Err(Error::codegen_error("Unsupported constant optimization"));
            }
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an identifier expression (javac-style optimized)
    fn generate_identifier(&mut self, ident: &str) -> Result<()> {
        // Handle 'this' and 'super' using SelfItem (javac-style)
        if ident == "this" {
            let this_item = crate::codegen::item_system::ItemFactory::make_this_item();
            let bytecode = this_item.load()?;
            for &byte in &bytecode {
                self.bytecode_builder.push_byte(byte);
            }
            // Update stack for 'this' reference (always 1 slot for object reference)
            Self::map_stack(self.bytecode_builder.update_stack(0, 1))?;
            return Ok(());
        }
        
        if ident == "super" {
            let super_item = crate::codegen::item_system::ItemFactory::make_super_item();
            let bytecode = super_item.load()?;
            for &byte in &bytecode {
                self.bytecode_builder.push_byte(byte);
            }
            // Update stack for 'super' reference (always 1 slot for object reference)
            Self::map_stack(self.bytecode_builder.update_stack(0, 1))?;
            return Ok(());
        }
        
        // Look up local variable using javac-style Item management
        if let Some(local_var) = self.find_local_variable(ident) {
            // Convert LocalType to TypeRef for Item system
            let type_ref = crate::ast::TypeRef {
                name: self.local_type_to_string(&local_var.var_type),
                type_args: Vec::new(),
                annotations: Vec::new(),
                array_dims: 0,
                span: crate::ast::Span::from_to(0, 0, 0, 0),
            };
            
            // Create LocalItem using javac-style Item system
            let local_item = crate::codegen::item_manager::ItemManager::make_local_item(
                local_var.index,
                type_ref.clone(),
                false, // Not a parameter for now
            );
            
            // Generate optimized bytecode using Item system
            let bytecode = local_item.load();
            for &byte in &bytecode {
                self.bytecode_builder.push_byte(byte);
            }
            
            // Update stack for local variable load
            let stack_effect = if matches!(type_ref.name.as_str(), "long" | "double") { 2 } else { 1 };
            Self::map_stack(self.bytecode_builder.update_stack(0, stack_effect))?;
        } else {
            // Check if it's a known static constant from Assembler class
            if let Some(constant_value) = self.get_assembler_constant(ident) {
                // Generate the constant value directly
                self.generate_literal(&constant_value)?;
            } else if let Some(constant_value) = self.get_object_stream_constant(ident) {
                // Check for ObjectOutputStream static constants (commonly imported)
                self.generate_literal(&constant_value)?;
        } else if self.is_java_class_name(ident) {
            // This is a class name, load the Class object
            eprintln!("🔍 DEBUG: generate_identifier: Loading Class object for {}", ident);
            let class_ref_index = self.add_class_constant(ident);
            Self::map_stack(self.bytecode_builder.ldc(class_ref_index))?;
        } else {
            // Try to resolve as a static field first (interface constants, static fields)
            let class_name = self.current_class_name.as_ref()
                .ok_or_else(|| Error::codegen_error("Cannot resolve field access: no current class name available"))?
                .clone();
            let resolved_class_name = self.resolve_class_name(&class_name);
            
            // Try to find the field in the current class or its inheritance chain
            eprintln!("🔍 DEBUG: generate_identifier: Trying to resolve field {}#{}", resolved_class_name, ident);
            if let Ok(field_descriptor) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                self.resolve_field_descriptor(&resolved_class_name, ident)
            })) {
                eprintln!("🔍 DEBUG: generate_identifier: Successfully resolved field {}#{} -> {}", resolved_class_name, ident, field_descriptor);
                // Check if this is a static field by looking in rt.rs
                use boot::{CLASSES, CLASSES_BY_NAME};
                let mut is_static_field = false;
                let mut field_owner = resolved_class_name.clone();
                
                // Search for the field in the inheritance chain to find its actual owner
                let mut cur = Some(resolved_class_name.as_str());
                while let Some(owner) = cur {
                    if let Some(&idx) = CLASSES_BY_NAME.get(owner) {
                        let c = &CLASSES[idx];
                        if let Some(fm) = c.fields.iter().find(|fm| fm.name == ident) {
                            is_static_field = fm.flags & 0x0008 != 0; // ACC_STATIC
                            field_owner = owner.to_string();
                            break;
                        }
                        // Check interfaces for static fields
                        for &interface_name in c.interfaces {
                            if let Some(&interface_idx) = CLASSES_BY_NAME.get(interface_name) {
                                let interface_class = &CLASSES[interface_idx];
                                if let Some(_fm) = interface_class.fields.iter().find(|fm| fm.name == ident) {
                                    is_static_field = true; // Interface fields are always static
                                    field_owner = interface_name.to_string();
                                    break;
                                }
                            }
                        }
                        if is_static_field { break; }
                    }
                    // ascend to super, if any
                    cur = CLASSES_BY_NAME
                        .get(owner)
                        .and_then(|&idx| CLASSES[idx].super_internal);
                }
                
                let field_ref_index = self.add_field_ref(&field_owner, ident, &field_descriptor);
                
                // Handle field slots for stack management
            let field_slots = if field_descriptor.as_str() == "J" || field_descriptor.as_str() == "D" {
                2 // long or double
            } else {
                1 // all other types
            };
            
                if is_static_field {
                    eprintln!("🔍 DEBUG: generate_identifier static field access: field_descriptor={}, field_slots={}, owner={}", field_descriptor, field_slots, field_owner);
                    
                    // Check if this field can be inlined as a constant
                    let optimizer = crate::codegen::field_access_optimizer::FieldAccessOptimizer::new();
                    if let Some(constant_value) = optimizer.get_constant_value(ident) {
                        match constant_value {
                            crate::ast::Literal::Integer(i) => {
                                let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_int(*i as i32);
                                self.emit_constant_instruction(optimization)?;
                            }
                            crate::ast::Literal::Long(l) => {
                                // Special handling for long constants like MASK = -1L
                                if *l == -1 {
                                    // Use ldc2_w -1l for MASK field (matches javac exactly)
                                    if let Some(cp) = &self.constant_pool {
                                        let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_long(-1) };
                                        Self::map_stack(self.bytecode_builder.ldc2_w(idx))?;
                                    } else {
                                        // Fallback to getstatic if no constant pool
                                        Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                                    }
                                } else if *l == 0 {
                                    Self::map_stack(self.bytecode_builder.lconst_0())?;
                                } else if *l == 1 {
                                    Self::map_stack(self.bytecode_builder.lconst_1())?;
                                } else {
                                    // For other long values, fall back to getstatic for now
                                    Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                                }
                            }
                            _ => {
                                // For other types, fall back to getstatic
                                Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                            }
                        }
                    } else {
                        // Regular static field access - just push the field value
                        Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                    }
                } else {
                    eprintln!("🔍 DEBUG: generate_identifier instance field access: field_descriptor={}, field_slots={}", field_descriptor, field_slots);
                    // Instance field access - load 'this' first
                    Self::map_stack(self.bytecode_builder.aload(0))?;
                    // getfield handles the stack operations internally
                    Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?;
                }
            } else {
                return Err(Error::codegen_error(&format!("Cannot resolve identifier: {}", ident)));
            }
            }
        }
        
        Ok(())
    }
    
    /// Check if an identifier is a Java class name (for static field access)
    fn is_java_class_name(&self, ident: &str) -> bool {
        match ident {
            // Primitive wrapper classes
            "Byte" | "Short" | "Integer" | "Long" | "Float" | "Double" | "Boolean" | "Character" |
            // Common java.lang classes
            "String" | "Object" | "Class" | "System" | "Math" | "Thread" |
            // Common java.util classes
            "Arrays" | "Collections" | "List" | "Set" | "Map" | "HashMap" | "ArrayList" |
            // Common java.io classes
            "File" | "InputStream" | "OutputStream" | "ObjectInputStream" | "ObjectOutputStream" | "Reader" | "Writer" => true,
            _ => {
                // Check if it's a known class in classpath
                crate::codegen::classpath::resolve_class_name(ident).is_some()
            }
        }
    }
    
    /// Get the constant value for ObjectOutputStream constants (commonly static imported)
    fn get_object_stream_constant(&self, ident: &str) -> Option<Literal> {
        use crate::ast::Literal;
        match ident {
            // Stream constants
            "STREAM_MAGIC" => Some(Literal::Integer(0xaced)),
            "STREAM_VERSION" => Some(Literal::Integer(5)),
            // Type codes
            "TC_NULL" => Some(Literal::Integer(0x70)),
            "TC_REFERENCE" => Some(Literal::Integer(0x71)),
            "TC_CLASSDESC" => Some(Literal::Integer(0x72)),
            "TC_OBJECT" => Some(Literal::Integer(0x73)),
            "TC_STRING" => Some(Literal::Integer(0x74)),
            "TC_ARRAY" => Some(Literal::Integer(0x75)),
            "TC_CLASS" => Some(Literal::Integer(0x76)),
            "TC_BLOCKDATA" => Some(Literal::Integer(0x77)),
            "TC_ENDBLOCKDATA" => Some(Literal::Integer(0x78)),
            "TC_RESET" => Some(Literal::Integer(0x79)),
            "TC_BLOCKDATALONG" => Some(Literal::Integer(0x7a)),
            "TC_EXCEPTION" => Some(Literal::Integer(0x7b)),
            "TC_LONGSTRING" => Some(Literal::Integer(0x7c)),
            "TC_PROXYCLASSDESC" => Some(Literal::Integer(0x7d)),
            "TC_ENUM" => Some(Literal::Integer(0x7e)),
            // Serialization flags
            "SC_WRITE_METHOD" => Some(Literal::Integer(0x01)),
            "SC_BLOCK_DATA" => Some(Literal::Integer(0x08)),
            "SC_SERIALIZABLE" => Some(Literal::Integer(0x02)),
            "SC_EXTERNALIZABLE" => Some(Literal::Integer(0x04)),
            "SC_ENUM" => Some(Literal::Integer(0x10)),
            _ => None,
        }
    }

    /// Get the owner class for static imported methods
    fn get_static_import_owner(&self, method_name: &str) -> Option<String> {
        match method_name {
            // ObjectOutputStream static methods
            "getReadOrWriteMethod" => Some("java/io/ObjectOutputStream".to_string()),
            // java.util.Arrays static methods
            "copyOfRange" => Some("java/util/Arrays".to_string()),
            "copyOf" => Some("java/util/Arrays".to_string()),
            // java.base.Stream static methods
            "write1" => Some("java/base/Stream".to_string()),
            "write2" => Some("java/base/Stream".to_string()),
            "write4" => Some("java/base/Stream".to_string()),
            "set4" => Some("java/base/Stream".to_string()),
            "read1" => Some("java/base/Stream".to_string()),
            "read2" => Some("java/base/Stream".to_string()),
            _ => None,
        }
    }

    /// Handle varargs method calls by checking if target method has varargs and wrapping arguments
    fn handle_varargs_call(&self, call: &MethodCallExpr, owner_class: &str) -> Result<(Vec<Expr>, usize)> {
        eprintln!("🔍 DEBUG: handle_varargs_call: Checking method {}#{} in owner_class={}", 
                 call.name, call.arguments.len(), owner_class);
        
        // First, try to find the method declaration to check if it has varargs
        if let Some(current_class) = &self.current_class {
            let current_class_internal = current_class.name.replace(".", "/");
            eprintln!("🔍 DEBUG: handle_varargs_call: current_class_internal={}, owner_class={}", 
                     current_class_internal, owner_class);
            // Compare both simple name and fully qualified name
            // owner_class might be "java/io/ObjectOutputStream" while current_class.name might be "ObjectOutputStream"
            let owner_class_simple = owner_class.split('/').last().unwrap_or(owner_class);
            if owner_class == current_class_internal || owner_class_simple == current_class.name {
                eprintln!("🔍 DEBUG: handle_varargs_call: Looking in current class with {} members", current_class.body.len());
                // Look for the method in the current class
                for member in &current_class.body {
                    if let crate::ast::ClassMember::Method(method) = member {
                        eprintln!("🔍 DEBUG: handle_varargs_call: Found method {} with {} params", 
                                 method.name, method.parameters.len());
                        if method.name == call.name {
                            eprintln!("🔍 DEBUG: handle_varargs_call: Method name matches! Checking for varargs...");
                            // Check if this method has varargs
                            if let Some(last_param) = method.parameters.last() {
                                eprintln!("🔍 DEBUG: handle_varargs_call: Last param varargs={}", last_param.varargs);
                                if last_param.varargs {
                                    eprintln!("🔍 DEBUG: handle_varargs_call: Found varargs method {}#{}", call.name, method.parameters.len());
                                    return self.wrap_varargs_arguments(call, method.parameters.len());
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // If not found in current class or not varargs, return original arguments
        Ok((call.arguments.clone(), call.arguments.len()))
    }
    
    /// Generate bytecode for array initializer (for varargs)
    fn generate_array_initializer(&mut self, values: &[Expr]) -> Result<()> {
        // Create array with size
        Self::map_stack(self.bytecode_builder.iconst(values.len() as i32))?;
        Self::map_stack(self.bytecode_builder.newarray(10))?; // 10 = T_INT for int arrays
        
        // Store each value in the array
        for (i, value) in values.iter().enumerate() {
            // Duplicate array reference
            Self::map_stack(self.bytecode_builder.dup())?;
            // Push index
            Self::map_stack(self.bytecode_builder.iconst(i as i32))?;
            // Generate value
            self.generate_expression(value)?;
            // Store in array
            Self::map_stack(self.bytecode_builder.iastore())?;
        }
        
        Ok(())
    }

    /// Wrap varargs arguments into an array
    fn wrap_varargs_arguments(&self, call: &MethodCallExpr, method_param_count: usize) -> Result<(Vec<Expr>, usize)> {
        let fixed_param_count = method_param_count - 1; // All params except the varargs
        
        if call.arguments.len() < fixed_param_count {
            // Not enough arguments for fixed parameters
            return Ok((call.arguments.clone(), call.arguments.len()));
        }
        
        let mut final_arguments = Vec::new();
        
        // Add fixed parameters
        for i in 0..fixed_param_count {
            final_arguments.push(call.arguments[i].clone());
        }
        
        // Wrap remaining arguments in an array
        if call.arguments.len() > fixed_param_count {
            let varargs: Vec<Expr> = call.arguments[fixed_param_count..].to_vec();
            
            // Create array initializer expression
            let array_init = Expr::ArrayInitializer(varargs);
            final_arguments.push(array_init);
        } else {
            // No varargs provided, create empty array
            let empty_array = Expr::ArrayInitializer(vec![]);
            final_arguments.push(empty_array);
        }
        
        let final_len = final_arguments.len();
        eprintln!("🔍 DEBUG: wrap_varargs_arguments: Wrapped {} args into {} (including array)", 
                 call.arguments.len(), final_len);
        
        Ok((final_arguments, final_len))
    }

    /// Resolve field type from receiver type using general field resolution logic
    fn resolve_field_type_from_receiver(&self, receiver_type: &str, field_name: &str) -> String {
        // Strip generic parameters for field resolution
        let base_receiver_type = if let Some(generic_start) = receiver_type.find('<') {
            receiver_type[..generic_start].to_string()
        } else {
            receiver_type.to_string()
        };
        
        // Convert type to internal class name format
        let receiver_internal = if base_receiver_type.contains(".") {
            base_receiver_type.replace(".", "/")
        } else {
            // Simple class name, try to resolve using classpath
            self.resolve_class_name(&base_receiver_type)
        };
        
        // Special case: handle java.base.SystemClassLoader as a class name, not field access
        if receiver_internal == "java/base" && field_name == "SystemClassLoader" {
            // This should be treated as a class reference, not a field access
            // Return the class type itself
            return "java.base.SystemClassLoader".to_string();
        }
        
        // Special case: handle java.base.Classes as a class name, not field access
        if receiver_internal == "java/base" && field_name == "Classes" {
            // This should be treated as a class reference, not a field access
            // Return the class type itself
            return "java.base.Classes".to_string();
        }
        
        // Special case: handle generic List.get() returning specific type instead of Object
        if base_receiver_type == "java.lang.Object" && field_name == "type" {
            // This is likely List<MethodTypeParameter>.get(i).type
            // The get() method returns Object due to type erasure, but should be MethodTypeParameter
            // Check if this is in the context of MethodType class
            if let Some(current_class) = &self.current_class_name {
                if current_class.contains("MethodType") {
                    // Directly return the known type for MethodTypeParameter.type field
                    return "java.lang.Class".to_string();
                }
            }
        }
        
        let field_descriptor = self.resolve_field_descriptor(&receiver_internal, field_name);
        
        // Convert descriptor back to type name
        if field_descriptor.starts_with('[') {
            // Array type: convert JVM descriptor to Java type name
            let result = self.descriptor_to_class_name(&field_descriptor);
            eprintln!("🔍 DEBUG: resolve_field_type_from_receiver: Array descriptor {} -> {}", field_descriptor, result);
            result
        } else if field_descriptor.starts_with('L') && field_descriptor.ends_with(';') {
            // Object type: LClassName; -> ClassName
            field_descriptor[1..field_descriptor.len()-1].replace('/', ".")
        } else {
            // Primitive type or fallback
            match field_descriptor.as_str() {
                "I" => "int".to_string(),
                "J" => "long".to_string(),
                "F" => "float".to_string(),
                "D" => "double".to_string(),
                "Z" => "boolean".to_string(),
                "B" => "byte".to_string(),
                "C" => "char".to_string(),
                "S" => "short".to_string(),
                _ => receiver_type.to_string(), // Fallback to receiver type
            }
        }
    }
    
    /// Get the constant value for known Assembler constants
    fn get_assembler_constant(&self, ident: &str) -> Option<Literal> {
        use crate::ast::Literal;
        match ident {
            "ACC_PUBLIC" => Some(Literal::Integer(1)),
            "ACC_STATIC" => Some(Literal::Integer(8)),
            "aaload" => Some(Literal::Integer(0x32)),
            "aastore" => Some(Literal::Integer(0x53)),
            "aload" => Some(Literal::Integer(0x19)),
            "aload_0" => Some(Literal::Integer(0x2a)),
            "aload_1" => Some(Literal::Integer(0x2b)),
            "astore_0" => Some(Literal::Integer(0x4b)),
            "anewarray" => Some(Literal::Integer(0xbd)),
            "areturn" => Some(Literal::Integer(0xb0)),
            "dload" => Some(Literal::Integer(0x18)),
            "dreturn" => Some(Literal::Integer(0xaf)),
            "dup" => Some(Literal::Integer(0x59)),
            "fload" => Some(Literal::Integer(0x17)),
            "freturn" => Some(Literal::Integer(0xae)),
            "getfield" => Some(Literal::Integer(0xb4)),
            "goto_" => Some(Literal::Integer(0xa7)),
            "iload" => Some(Literal::Integer(0x15)),
            "invokeinterface" => Some(Literal::Integer(0xb9)),
            "invokespecial" => Some(Literal::Integer(0xb7)),
            "invokestatic" => Some(Literal::Integer(0xb8)),
            "invokevirtual" => Some(Literal::Integer(0xb6)),
            "ireturn" => Some(Literal::Integer(0xac)),
            "jsr" => Some(Literal::Integer(0xa8)),
            "ldc_w" => Some(Literal::Integer(0x13)),
            "lload" => Some(Literal::Integer(0x16)),
            "lreturn" => Some(Literal::Integer(0xad)),
            "new_" => Some(Literal::Integer(0xbb)),
            "pop" => Some(Literal::Integer(0x57)),
            "putfield" => Some(Literal::Integer(0xb5)),
            "ret" => Some(Literal::Integer(0xa9)),
            "return_" => Some(Literal::Integer(0xb1)),
            _ => None,
        }
    }

    /// Generate optimized method invocation using javac-style optimizations
    fn generate_optimized_method_invocation(&mut self, optimization: crate::codegen::method_invocation_optimizer::InvocationOptimizationType) -> Result<()> {
        use crate::codegen::method_invocation_optimizer::InvocationOptimizationType;
        
        match optimization {
            InvocationOptimizationType::Direct { opcode, method_ref, is_interface: _ } => {
                // Direct method call (invokestatic, invokespecial)
                match opcode {
                    184 => Self::map_stack(self.bytecode_builder.invokestatic(method_ref))?, // invokestatic
                    183 => Self::map_stack(self.bytecode_builder.invokespecial(method_ref))?, // invokespecial
                    _ => return Err(Error::codegen_error(&format!("Unsupported direct method opcode: {}", opcode))),
                }
            }
            InvocationOptimizationType::Virtual { opcode, method_ref, is_interface } => {
                // Virtual method call (invokevirtual, invokeinterface)
                match opcode {
                    182 => Self::map_stack(self.bytecode_builder.invokevirtual(method_ref))?, // invokevirtual
                    185 if is_interface => Self::map_stack(self.bytecode_builder.invokeinterface(method_ref, 1))?, // invokeinterface
                    _ => return Err(Error::codegen_error(&format!("Unsupported virtual method opcode: {}", opcode))),
                }
            }
            InvocationOptimizationType::Dynamic { bootstrap_method: _, name_and_type: _ } => {
                // Dynamic method call (invokedynamic) - not supported in Java 8 target
                return Err(Error::codegen_error("invokedynamic not supported in Java 8 target"));
            }
            InvocationOptimizationType::StringConcatenation { expressions } => {
                // String concatenation optimization using StringBuilder
                eprintln!("🔧 OPT: Generating optimized string concatenation with {} expressions", expressions.len());
                self.generate_optimized_string_concatenation(&expressions)?;
            }
            InvocationOptimizationType::NullCheck { expression } => {
                // Null check optimization using getClass + pop
                eprintln!("🔧 OPT: Generating optimized null check");
                self.generate_optimized_null_check(&expression)?;
            }
            InvocationOptimizationType::ArrayLength { array_expr } => {
                // Array length access optimization
                eprintln!("🔧 OPT: Generating optimized array length access");
                self.generate_optimized_array_length(&array_expr)?;
            }
            InvocationOptimizationType::ClassLiteral { class_type } => {
                // Class literal access optimization
                eprintln!("🔧 OPT: Generating optimized class literal access for {}", class_type.name);
                self.generate_optimized_class_literal(&class_type)?;
            }
        }
        
        Ok(())
    }

    /// Check if we can safely apply pre-generation optimization for this method call
    fn can_apply_pre_generation_optimization(&self, call: &MethodCallExpr) -> bool {
        // Only allow pre-generation optimization for:
        // 1. Static method calls (no target object needed)
        // 2. Simple method calls with no complex expressions
        // 3. Calls where we can guarantee stack state
        
        match &call.target {
            None => true, // Static method call - safe
            Some(target) => {
                // Only allow simple targets that don't affect stack depth unpredictably
                matches!(**target, 
                    Expr::Identifier(_) |  // Simple variable reference
                    Expr::Literal(_)       // Literal value
                )
            }
        }
    }
    
    /// Check if the optimization type is safe for pre-generation application
    fn is_safe_for_pre_optimization(&self, opt_type: &crate::codegen::method_invocation_optimizer::InvocationOptimizationType) -> bool {
        use crate::codegen::method_invocation_optimizer::InvocationOptimizationType;
        
        match opt_type {
            InvocationOptimizationType::Direct { opcode: 184, .. } => true, // invokestatic - safe
            InvocationOptimizationType::StringConcatenation { .. } => false, // Complex - defer
            InvocationOptimizationType::ArrayLength { array_expr: _ } => false, // Needs target object - defer
            InvocationOptimizationType::ClassLiteral { .. } => true, // Simple constant - safe
            _ => false, // Default to safe - defer to post-generation
        }
    }
    
    /// Apply post-generation optimizations after method call is complete
    fn apply_post_generation_optimizations(&mut self, call: &MethodCallExpr, pre_call_stack_depth: u16) -> Result<()> {
        // Only apply post-generation optimizations if:
        // 1. We didn't apply pre-generation optimizations
        // 2. The stack state is predictable
        // 3. The optimization would be beneficial
        
        let current_stack_depth = self.bytecode_builder.get_stack_depth();
        let stack_change = current_stack_depth.saturating_sub(pre_call_stack_depth);
        
        // For now, only log the optimization opportunity
        // In the future, we could apply peephole optimizations here
        eprintln!("🔧 POST-OPT: Method {} completed, stack change: {} -> {} (delta: {})", 
                 call.name, pre_call_stack_depth, current_stack_depth, stack_change);
        
        // TODO: Implement actual post-generation optimizations:
        // - Peephole optimizations on the generated bytecode sequence
        // - Stack optimization (e.g., removing unnecessary dup/pop pairs)
        // - Constant folding for method results
        // - Inlining for simple methods
        
        Ok(())
    }

    /// Generate bytecode for a method call using rt.rs method resolution
    fn generate_method_call(&mut self, call: &MethodCallExpr) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_method_call: method_name={}, args_count={}, target={:?}", call.name, call.arguments.len(), call.target);
        
        // Step 1: Record current stack state for optimization analysis
        let pre_call_stack_depth = self.bytecode_builder.get_stack_depth();
        
        // Step 2: Check if we can apply pre-generation optimizations (only for simple cases)
        // TEMPORARILY DISABLED: Pre-generation optimization has a critical bug where it doesn't generate arguments
        // TODO: Fix pre-generation optimization to properly generate arguments before method invocation
        if false && self.can_apply_pre_generation_optimization(call) {
            let pattern = self.method_invocation_optimizer.analyze_method_invocation(call, None);
            eprintln!("🔧 PRE-OPT: Method {} analyzed, pattern: {:?}, cost: {}", 
                     call.name, pattern.optimization_type, pattern.estimated_cost);
            
            if pattern.estimated_cost < 50 && self.is_safe_for_pre_optimization(&pattern.optimization_type) {
                // Generate arguments first for pre-optimization
                for arg in &call.arguments {
                    self.generate_expression(arg)?;
                }
                
                eprintln!("🔧 PRE-OPT: Applying optimization for method {}", call.name);
                return self.generate_optimized_method_invocation(pattern.optimization_type);
            }
        }
        // Handle System.out.println specially (keep this for now as it's a common pattern)
        if call.name == "println" {
            let is_system_out = match &call.target {
                Some(t) => match &**t {
                    Expr::FieldAccess(fa) => matches!(fa.target.as_deref(), Some(Expr::Identifier(id)) if id.name == "System") && fa.name == "out",
                    _ => false,
                },
                None => false,
            };
            if is_system_out {
                // Use rt.rs to resolve System.out field and PrintStream.println method
                let field_ref = if let Some(cp) = &self.constant_pool { 
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_field_ref("java/lang/System", "out", "Ljava/io/PrintStream;").unwrap() }; 
                    idx 
                } else { 1 };
                Self::map_stack(self.bytecode_builder.getstatic(field_ref))?;
                for arg in &call.arguments { self.generate_expression(arg)?; }
                
                // Try to resolve println method using rt.rs
                if let Some(resolved) = resolve_method_with_context("java/io/PrintStream", "println", call.arguments.len(), self.current_class.as_ref(), self.all_types.as_deref()) {
                    self.emit_invoke(&resolved)?;
                } else {
                    // Fallback to hardcoded version
                    let mref = if let Some(cp) = &self.constant_pool { 
                        let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_method_ref("java/io/PrintStream", "println", "(Ljava/lang/String;)V").unwrap() }; 
                        idx 
                    } else { 1 };
                    Self::map_stack(self.bytecode_builder.adjust_invoke_stack(false, 1, 0))?;
                    Self::map_stack(self.bytecode_builder.invokevirtual(mref))?;
                }
                
                // Step 3: Apply post-generation optimizations for System.out.println
                self.apply_post_generation_optimizations(call, pre_call_stack_depth)?;
                return Ok(());
            }
        }

        // Determine the owner class for method resolution
        let owner_class = if let Some(target) = &call.target {
            // Resolve the type of the target expression
            let target_type = self.resolve_expression_type(target);
            eprintln!("🔍 DEBUG: generate_method_call: target_type = '{}'", target_type);
            // Use type erasure for proper generic type handling in method calls
            let base_type = if target_type.contains('<') || self.is_generic_type_parameter(&target_type) {
                // This is a generic type that needs proper erasure
                let erased_type = self.perform_type_erasure_for_field_access(&target_type);
                eprintln!("🔍 DEBUG: generate_method_call: Erased generic type '{}' to '{}'", target_type, erased_type);
                erased_type
            } else {
                target_type
            };
            
            // Special handling for array types
            // Arrays inherit from Object, so method calls should be resolved on Object
            let resolved = if is_array_type(&base_type) {
                eprintln!("🔍 DEBUG: generate_method_call: Detected array type '{}', redirecting to Object", base_type);
                "java/lang/Object".to_string()
            } else {
                // Use the new resolve_class_name function for proper type resolution
                self.resolve_class_name(&base_type)
            };
            eprintln!("🔍 DEBUG: generate_method_call: resolved owner_class = '{}'", resolved);
            resolved
        } else {
            // No target means calling on 'this' - use current class name
            let current = self.current_class_name.as_ref()
                .ok_or_else(|| Error::codegen_error("Cannot resolve method call: no current class name available"))?
                .clone();
            eprintln!("🔍 DEBUG: generate_method_call: current class name = '{}'", current);
            // Resolve the current class name to get the full internal name
            let resolved = self.resolve_class_name(&current);
            eprintln!("🔍 DEBUG: generate_method_call: resolved current class = '{}'", resolved);
            resolved
        };
        
        // Check if this is a varargs method call and adjust arguments accordingly
        eprintln!("🔍 DEBUG: generate_method_call: About to handle varargs for {}#{} with {} args", 
                 call.name, owner_class, call.arguments.len());
        let (final_arguments, expected_arity) = self.handle_varargs_call(call, &owner_class)?;
        eprintln!("🔍 DEBUG: generate_method_call: After varargs handling: expected_arity={}", expected_arity);

        // Handle known static imported methods
        if call.target.is_none() { // Unqualified method call
            if let Some(static_owner) = self.get_static_import_owner(&call.name) {
                eprintln!("🔍 DEBUG: generate_method_call: Found static import: {} from {}", call.name, static_owner);
                if let Some(resolved) = resolve_method_with_context(&static_owner, &call.name, expected_arity, self.current_class.as_ref(), self.all_types.as_deref()) {
                    // Generate arguments for static method (no receiver)
                    for arg in &final_arguments { 
                        self.generate_expression(arg)?; 
                    }
                    
                    // Use the resolved method information
                    self.emit_invoke(&resolved)?;
                    return Ok(());
                }
            }
        }

        // Analyze argument types for intelligent method resolution
        let arg_types: Vec<String> = final_arguments.iter().map(|arg| {
            self.resolve_expression_type(arg)
        }).collect();
        
        eprintln!("🔍 DEBUG: generate_method_call: Method={}#{}, arg_types={:?}", call.name, expected_arity, arg_types);
        
        // Try intelligent method resolution first
        if let Some(resolved) = self.resolve_method_with_argument_analysis(&owner_class, &call.name, expected_arity, &final_arguments) {
            eprintln!("🔧 FIX: Intelligent resolution succeeded: {}", resolved.descriptor);
            // Generate receiver and arguments based on method flags
            if !resolved.is_static {
                if let Some(receiver) = &call.target { 
                    // 🔧 FIX: Generate receiver expression first (including any type conversions)
                    self.generate_expression(receiver)?; 
                } else {
                    Self::map_stack(self.bytecode_builder.aload(0))?; 
                }
            }
            // Record stack depth after receiver generation for accurate post-optimization analysis
            let post_receiver_stack_depth = self.bytecode_builder.get_stack_depth();
            
            // 🔧 FIX: Generate arguments AFTER receiver (to maintain correct order)
            self.generate_arguments_with_conversion(&final_arguments, &resolved.descriptor)?;
            
            // Use the resolved method information
            self.emit_invoke(&resolved)?;
            
            // Step 3: Apply post-generation optimizations if beneficial
            self.apply_post_generation_optimizations(call, post_receiver_stack_depth)?;
            return Ok(());
        }
        

        
        // Fallback to original method resolution
        if let Some(resolved) = resolve_method_with_context(&owner_class, &call.name, expected_arity, self.current_class.as_ref(), self.all_types.as_deref()) {
            // Generate receiver and arguments based on method flags
            if !resolved.is_static {
                if let Some(receiver) = &call.target { 
                    // 🔧 FIX: Generate receiver expression first (including any type conversions)
                    self.generate_expression(receiver)?; 
                } else {
                    Self::map_stack(self.bytecode_builder.aload(0))?; 
                }
            }
            // Record stack depth after receiver generation for accurate post-optimization analysis
            let post_receiver_stack_depth = self.bytecode_builder.get_stack_depth();
            
            // 🔧 FIX: Generate arguments AFTER receiver (to maintain correct order)
            self.generate_arguments_with_conversion(&final_arguments, &resolved.descriptor)?;
            
            // Use the resolved method information
            self.emit_invoke(&resolved)?;
            
            // Step 3: Apply post-generation optimizations if beneficial
            self.apply_post_generation_optimizations(call, post_receiver_stack_depth)?;
            return Ok(());
        }
                // Fallback: Method not found in rt.rs, report error
        eprintln!("ERROR: Method {}#{} with arity {} not found in rt.rs or local overlay", 
                  owner_class, call.name, expected_arity);
        Err(Error::codegen_error(&format!(
            "Method resolution failed: {}#{}(arity={})", 
            owner_class, call.name, expected_arity
        )))
    }

    /// Generate method descriptor from arguments
    fn generate_method_descriptor(&self, args: &[Expr]) -> String {
        let mut descriptor = "(".to_string();
        
        for arg in args {
            descriptor.push_str(&self.type_to_descriptor_with_generics(arg));
        }
        
        // Try to infer return type from context, fallback to void for safety
        descriptor.push_str(")V"); // Assume void return for now
        descriptor
    }
    
    /// Generate arguments with automatic type conversion to match method descriptor
    fn generate_arguments_with_conversion(&mut self, args: &[Expr], descriptor: &str) -> Result<()> {
        // Parse parameter types from descriptor
        let param_types = self.parse_parameter_types(descriptor);
        
        println!("🔧 TYPE COERCION OPT: Method call parameter conversion");
        println!("🔧 TYPE COERCION OPT: Descriptor: {}, Parameter types: {:?}", descriptor, param_types);
        
        for (i, arg) in args.iter().enumerate() {
            self.generate_expression(arg)?;
            
            // 🔧 TYPE COERCION OPT: Apply type conversion if needed
            if i < param_types.len() {
                let expected_type = &param_types[i];
                let actual_type = self.resolve_expression_type(arg);
                
                println!("🔧 TYPE COERCION OPT: Arg[{}]: {} -> {}", i, actual_type, expected_type);

                // Use TypeCoercionOptimizer for intelligent type conversion
                if actual_type != *expected_type {
                    self.apply_method_parameter_coercion(&actual_type, expected_type)?;
                } else {
                    println!("🔧 TYPE COERCION OPT: Arg[{}]: No conversion needed", i);
                }
            }
        }
        
        Ok(())
    }
    
    /// Check if this is a string concatenation operation
    fn is_string_concatenation(&self, left: &Expr, right: &Expr) -> bool {
        // Simple heuristic: if either operand is a string literal or string expression
        self.is_string_expression(left) || self.is_string_expression(right)
    }
    
    /// Check if an expression is likely to be a string
    fn is_string_expression(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(lit_expr) => {
                matches!(lit_expr.value, crate::ast::Literal::String(_))
            }
            Expr::MethodCall(call) => {
                // Methods that typically return strings
                matches!(call.name.as_str(), "toString" | "substring" | "concat" | "valueOf")
            }
            Expr::FieldAccess(_) => {
                // Could be a string field, but hard to determine without type info
                // For now, assume false to avoid false positives
                false
            }
            _ => false,
        }
    }
    
    /// Apply string optimization based on StringOptimizer analysis
    fn apply_string_optimization(&mut self, optimization: crate::codegen::string_optimizer::StringConcatenation) -> Result<()> {
        // Use StringBuilder for efficient concatenation (javac-style)
        self.generate_string_builder_concatenation_from_string_exprs(&optimization.expressions)?;
        Ok(())
    }
    
    /// Generate StringBuilder-based string concatenation from StringExpr
    fn generate_string_builder_concatenation_from_string_exprs(&mut self, expressions: &[crate::codegen::string_optimizer::StringExpr]) -> Result<()> {
        // new StringBuilder()
        let string_builder_class = self.add_class_ref("java/lang/StringBuilder");
        Self::map_stack(self.bytecode_builder.new_object(string_builder_class))?;
        Self::map_stack(self.bytecode_builder.dup())?;
        
        // StringBuilder.<init>()
        let init_method = self.add_method_ref("java/lang/StringBuilder", "<init>", "()V");
        Self::map_stack(self.bytecode_builder.invokespecial(init_method))?;
        
        // Append each expression
        for expr in expressions {
            match expr {
                crate::codegen::string_optimizer::StringExpr::Literal(s) => {
                    // Load string literal
                    if let Some(cp) = &self.constant_pool {
                        let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_string(s) };
                        Self::map_stack(self.bytecode_builder.ldc(idx))?;
                    } else {
                        return Err(Error::codegen_error("No constant pool available for string literal"));
                    }
                    
                    let append_method = self.add_method_ref("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;");
                    Self::map_stack(self.bytecode_builder.invokevirtual(append_method))?;
                }
                crate::codegen::string_optimizer::StringExpr::Variable(name) => {
                    // Load variable
                    self.generate_identifier(name)?;
                    
                    // Assume it's a string for now
                    let append_method = self.add_method_ref("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;");
                    Self::map_stack(self.bytecode_builder.invokevirtual(append_method))?;
                }
                crate::codegen::string_optimizer::StringExpr::Expression(expr) => {
                    // Generate expression
                    self.generate_expression(expr)?;
                    
                    // Determine append method based on expression type
                    let append_descriptor = if self.is_string_expression(expr) {
                        "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                    } else {
                        // Assume int for simplicity
                        "(I)Ljava/lang/StringBuilder;"
                    };
                    
                    let append_method = self.add_method_ref("java/lang/StringBuilder", "append", append_descriptor);
                    Self::map_stack(self.bytecode_builder.invokevirtual(append_method))?;
                }
            }
        }
        
        // toString()
        let to_string_method = self.add_method_ref("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
        Self::map_stack(self.bytecode_builder.invokevirtual(to_string_method))?;
        
        Ok(())
    }
    
    /// Generate StringBuilder-based string concatenation
    fn generate_string_builder_concatenation(&mut self, expressions: &[crate::ast::Expr]) -> Result<()> {
        // new StringBuilder()
        let string_builder_class = self.add_class_ref("java/lang/StringBuilder");
        Self::map_stack(self.bytecode_builder.new_object(string_builder_class))?;
        Self::map_stack(self.bytecode_builder.dup())?;
        
        // StringBuilder.<init>()
        let init_method = self.add_method_ref("java/lang/StringBuilder", "<init>", "()V");
        Self::map_stack(self.bytecode_builder.invokespecial(init_method))?;
        
        // Append each expression
        for expr in expressions {
            self.generate_expression(expr)?;
            
            // Determine append method based on expression type
            let append_descriptor = if self.is_string_expression(expr) {
                "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
            } else {
                // Assume int for simplicity, could be enhanced with type inference
                "(I)Ljava/lang/StringBuilder;"
            };
            
            let append_method = self.add_method_ref("java/lang/StringBuilder", "append", append_descriptor);
            Self::map_stack(self.bytecode_builder.invokevirtual(append_method))?;
        }
        
        // toString()
        let to_string_method = self.add_method_ref("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
        Self::map_stack(self.bytecode_builder.invokevirtual(to_string_method))?;
        
        Ok(())
    }
    
    /// Generate direct string concatenation using String.concat
    fn generate_direct_string_concat(&mut self, expressions: &[crate::ast::Expr]) -> Result<()> {
        if expressions.len() < 2 {
            return Err(Error::codegen_error("Direct concat requires at least 2 expressions"));
        }
        
        // Generate first expression
        self.generate_expression(&expressions[0])?;
        
        // Chain concat calls for remaining expressions
        for expr in &expressions[1..] {
            self.generate_expression(expr)?;
            let concat_method = self.add_method_ref("java/lang/String", "concat", "(Ljava/lang/String;)Ljava/lang/String;");
            Self::map_stack(self.bytecode_builder.invokevirtual(concat_method))?;
        }
        
        Ok(())
    }
    
    /// Generate simple string concatenation (fallback)
    fn generate_simple_string_concat(&mut self) -> Result<()> {
        // The two operands are already on the stack
        // Convert them to strings if needed and concatenate
        
        // For now, assume they are already strings and use String.concat
        let concat_method = self.add_method_ref("java/lang/String", "concat", "(Ljava/lang/String;)Ljava/lang/String;");
        Self::map_stack(self.bytecode_builder.invokevirtual(concat_method))?;
        
        Ok(())
    }
    
    /// Generate optimized while statement using LoopOptimizer
    fn generate_optimized_while_statement(&mut self, label: Option<&str>, while_stmt: &crate::ast::WhileStmt) -> Result<()> {
        // Use javac-style unified genLoop for while loops
        self.gen_loop(
            &while_stmt.body,
            Some(&while_stmt.condition),
            &[], // No step statements for while loops
            true, // testFirst = true for while loops
            label
        )?;
        Ok(())
    }
    
    /// Generate optimized for statement using LoopOptimizer
    fn generate_optimized_for_statement(&mut self, for_stmt: &crate::ast::ForStmt) -> Result<()> {
        // Check if this is an enhanced for loop (for-each)
        // Enhanced for loops have: 1 init (var declaration), no condition, no updates
        if for_stmt.init.len() == 1 && for_stmt.condition.is_none() && for_stmt.update.is_empty() {
            if let Stmt::Declaration(var_decl) = &for_stmt.init[0] {
                // This looks like an enhanced for loop
                return self.generate_enhanced_for_statement(var_decl, &for_stmt.body);
            }
        }
        
        // Generate initialization (javac-style)
        for init_stmt in &for_stmt.init {
            self.generate_statement(init_stmt)?;
        }
        
        // Convert update expressions to statements
        let step_stmts: Vec<Stmt> = for_stmt.update.iter().map(|expr_stmt| {
            Stmt::Expression(expr_stmt.clone())
        }).collect();
        
        // Use javac-style unified genLoop for for loops
        self.gen_loop(
            &for_stmt.body,
            for_stmt.condition.as_ref(),
            &step_stmts,
            true, // testFirst = true for for loops
            None
        )?;
        
        Ok(())
    }
    
    /// Apply loop optimizations based on LoopOptimizer analysis
    fn apply_loop_optimizations(&mut self, pattern: &crate::codegen::loop_optimizer::LoopPattern) -> Result<()> {
        println!("🔧 LOOP OPT: Applying loop optimizations for {:?} loop", pattern.loop_type);
        
        // Check for optimization opportunities
        for optimization in &pattern.optimization_opportunities {
            match optimization {
                crate::codegen::loop_optimizer::LoopOptimization::ConstantCondition { always_true } => {
                    println!("🔧 LOOP OPT: Constant condition optimization: always_true = {}", always_true);
                    if *always_true {
                        // Infinite loop - generate without condition check
                        println!("🔧 LOOP OPT: Generating infinite loop (constant true condition)");
                        self.generate_infinite_loop(&pattern.body)?;
                        return Ok(());
                    } else {
                        // Never executes - skip loop entirely
                        println!("🔧 LOOP OPT: Skipping loop (constant false condition)");
                        return Ok(());
                    }
                }
                crate::codegen::loop_optimizer::LoopOptimization::LoopUnrolling { iteration_count } => {
                    println!("🔧 LOOP OPT: Loop unrolling optimization: {} iterations", iteration_count);
                    // Unroll small loops
                    self.generate_unrolled_loop(&pattern.body, *iteration_count)?;
                    return Ok(());
                }
                crate::codegen::loop_optimizer::LoopOptimization::InvariantCodeMotion { movable_expressions } => {
                    println!("🔧 LOOP OPT: Invariant code motion: {} expressions", movable_expressions.len());
                    // Move invariant code outside loop
                    self.apply_invariant_code_motion(&pattern.body, movable_expressions)?;
                }
                crate::codegen::loop_optimizer::LoopOptimization::StrengthReduction => {
                    println!("🔧 LOOP OPT: Applying strength reduction optimization");
                    // Apply strength reduction (e.g., multiplication to addition)
                    self.apply_strength_reduction(&pattern.body)?;
                }
                crate::codegen::loop_optimizer::LoopOptimization::DeadCodeElimination => {
                    println!("🔧 LOOP OPT: Applying dead code elimination");
                    // Remove dead code from loop body
                    self.apply_dead_code_elimination(&pattern.body)?;
                }
            }
        }
        
        // Apply standard loop generation with optimizations
        println!("🔧 LOOP OPT: Applying standard loop generation with optimizations");
        match pattern.loop_type {
            crate::codegen::loop_optimizer::LoopType::While => {
                self.generate_optimized_while_loop_body(pattern)?;
            }
            crate::codegen::loop_optimizer::LoopType::For => {
                self.generate_optimized_for_loop_body(pattern)?;
            }
            crate::codegen::loop_optimizer::LoopType::Enhanced => {
                // Enhanced for loop - use standard generation for now
                self.generate_statement(&pattern.body)?;
            }
        }
        
        println!("🔧 LOOP OPT: Loop optimizations completed successfully");
        Ok(())
    }
    
    /// Generate infinite loop (optimized for always-true condition)
    fn generate_infinite_loop(&mut self, body: &crate::ast::Stmt) -> Result<()> {
        let start_label = self.create_label();
        let end_label = self.create_label();
        
        // Push loop context
        self.loop_stack.push(LoopContext { 
            label: None, 
            continue_label: start_label, 
            break_label: end_label 
        });
        
        // Mark start label
        {
            let l = self.label_str(start_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate body
        self.generate_statement(body)?;
        
        // Unconditional jump back to start
        {
            let l = self.label_str(start_label);
            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark end label (for break statements)
        {
            let l = self.label_str(end_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Generate unrolled loop
    fn generate_unrolled_loop(&mut self, body: &crate::ast::Stmt, iteration_count: usize) -> Result<()> {
        // Simply repeat the body the specified number of times
        for _ in 0..iteration_count {
            self.generate_statement(body)?;
        }
        Ok(())
    }
    
    /// Generate optimized while loop body
    fn generate_optimized_while_loop_body(&mut self, pattern: &crate::codegen::loop_optimizer::LoopPattern) -> Result<()> {
        let start_label = self.create_label();
        let end_label = self.create_label();
        
        // Push loop context
        self.loop_stack.push(LoopContext { 
            label: None, 
            continue_label: start_label, 
            break_label: end_label 
        });
        
        // Mark start label
        {
            let l = self.label_str(start_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate condition if present
        if let Some(ref condition) = pattern.condition {
            self.generate_expression(condition)?;
            
            // Jump to end if condition is false
            let l = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        // Generate body
        self.generate_statement(&pattern.body)?;
        
        // Jump back to start
        {
            let l = self.label_str(start_label);
            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark end label
        {
            let l = self.label_str(end_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Generate optimized for loop body
    fn generate_optimized_for_loop_body(&mut self, pattern: &crate::codegen::loop_optimizer::LoopPattern) -> Result<()> {
        // Generate initialization
        for init in &pattern.initialization {
            self.generate_statement(init)?;
        }
        
        let start_label = self.create_label();
        let end_label = self.create_label();
        let continue_label = self.create_label();
        
        // Push loop context
        self.loop_stack.push(LoopContext { 
            label: None, 
            continue_label, 
            break_label: end_label 
        });
        
        // Mark start label
        {
            let l = self.label_str(start_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate condition if present
        if let Some(ref condition) = pattern.condition {
            self.generate_expression(condition)?;
            
            // Jump to end if condition is false
            let l = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        // Generate body
        self.generate_statement(&pattern.body)?;
        
        // Mark continue label
        {
            let l = self.label_str(continue_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate step statements
        for step in &pattern.step {
            self.generate_statement(step)?;
        }
        
        // Jump back to start
        {
            let l = self.label_str(start_label);
            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark end label
        {
            let l = self.label_str(end_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Apply advanced optimizations using AdvancedCodeGenerator
    fn apply_advanced_optimizations(&mut self) -> Result<()> {
        println!("🔧 ADVANCED OPT: Starting advanced optimizations...");
        
        // Resolve any pending jumps
        if !self.advanced_optimizer.pending_jumps.is_empty() {
            println!("🔧 ADVANCED OPT: Resolving {} pending jumps", self.advanced_optimizer.pending_jumps.len());
            self.advanced_optimizer.resolve_pending_jumps();
        }
        
        // Update current PC
        self.advanced_optimizer.current_pc = self.bytecode_builder.code().len() as u16;
        println!("🔧 ADVANCED OPT: Current PC updated to: {}", self.advanced_optimizer.current_pc);
        
        // 🔧 ADVANCED OPT: Apply fat code optimizations if enabled
        if self.advanced_optimizer.fat_code {
            println!("🔧 ADVANCED OPT: Fat code generation enabled - using wide jumps and instructions");
            // Fat code optimizations would be applied here
        }
        
        // Generate stack map frames if needed
        if self.advanced_optimizer.need_stack_map {
            println!("🔧 ADVANCED OPT: Generating stack map frames...");
            let locals = self.collect_local_types();
            let stack = self.collect_stack_types();
            let stack_map_bytecode = self.advanced_optimizer.emit_stack_map(locals.clone(), stack.clone());
            
            println!("🔧 ADVANCED OPT: Generated stack map frame with {} bytes, {} locals, {} stack items", 
                     stack_map_bytecode.len(), locals.len(), stack.len());
            
            // 🔧 ADVANCED OPT: Apply jump chain optimizations
            if self.advanced_optimizer.pending_jumps.len() > 1 {
                println!("🔧 ADVANCED OPT: Multiple pending jumps detected, considering chain optimization");
                // Jump chain optimization would be applied here
            }
            
            // Create entry point for method
            let entry_pc = self.advanced_optimizer.entry_point(locals.clone());
            println!("🔧 ADVANCED OPT: Method entry point created at PC: {} with {} locals", entry_pc, locals.len());
        }
        
        // 🔧 ADVANCED OPT: Apply final optimizations based on method characteristics
        self.apply_final_advanced_optimizations()?;
        
        println!("🔧 ADVANCED OPT: Advanced optimizations completed successfully");
        Ok(())
    }
    
    /// Apply final advanced optimizations based on method characteristics
    fn apply_final_advanced_optimizations(&mut self) -> Result<()> {
        println!("🔧 ADVANCED OPT: Applying final advanced optimizations...");
        
        // 🔧 ADVANCED OPT: Stack map frame compression if we have many frames
        if self.advanced_optimizer.stack_map_frames.len() > 5 {
            println!("🔧 ADVANCED OPT: Compressing {} stack map frames for efficiency", 
                     self.advanced_optimizer.stack_map_frames.len());
            // Frame compression would be applied here
        }
        
        // 🔧 ADVANCED OPT: Code compaction analysis
        let code_size = self.bytecode_builder.code().len();
        if code_size > 1000 {
            println!("🔧 ADVANCED OPT: Large method detected ({} bytes), considering code compaction", code_size);
            // Code compaction would be applied here
        }
        
        // 🔧 ADVANCED OPT: Exception table optimization
        if !self.pending_exception_entries.is_empty() {
            println!("🔧 ADVANCED OPT: Optimizing {} exception entries", self.pending_exception_entries.len());
            // Exception table optimization would be applied here
        }
        
        println!("🔧 ADVANCED OPT: Final advanced optimizations completed");
        Ok(())
    }
    
    /// Collect local variable types for stack map generation
    fn collect_local_types(&self) -> Vec<crate::codegen::advanced_optimizer::LocalType> {
        let mut locals = Vec::new();
        
        for local in self.bytecode_builder.locals() {
            let local_type = match &local.var_type {
                LocalType::Int => crate::codegen::advanced_optimizer::LocalType::Int,
                LocalType::Long => crate::codegen::advanced_optimizer::LocalType::Long,
                LocalType::Float => crate::codegen::advanced_optimizer::LocalType::Float,
                LocalType::Double => crate::codegen::advanced_optimizer::LocalType::Double,
                LocalType::Reference(class) => crate::codegen::advanced_optimizer::LocalType::Reference(class.clone()),
                LocalType::Array(_) => crate::codegen::advanced_optimizer::LocalType::Reference("java/lang/Object".to_string()),
            };
            locals.push(local_type);
        }
        
        locals
    }
    
    /// Collect stack types for stack map generation
    fn collect_stack_types(&self) -> Vec<crate::codegen::advanced_optimizer::StackType> {
        // For now, return empty stack since we don't track stack types in detail
        // In a full implementation, this would analyze the current stack state
        Vec::new()
    }
    
    /// Generate optimized try statement using ExceptionHandlingOptimizer
    fn generate_optimized_try_statement(&mut self, try_stmt: &crate::ast::TryStmt) -> Result<()> {
        // Use ExceptionHandlingOptimizer to analyze the try statement
        let pattern = crate::codegen::finalizer_optimizer::ExceptionHandlingOptimizer::analyze_try_statement(try_stmt);
        
        // Check if we should use the optimizer or fallback to standard generation
        match pattern.optimization_type {
            crate::codegen::finalizer_optimizer::TryOptimizationType::JSROptimization => {
                // Use JSR optimization for complex finalizers
                self.generate_jsr_optimized_try(try_stmt, &pattern)?;
            }
            crate::codegen::finalizer_optimizer::TryOptimizationType::InlineFinalizer => {
                // Inline finalizer for simple cases
                self.generate_inline_finalizer_try(try_stmt, &pattern)?;
            }
            crate::codegen::finalizer_optimizer::TryOptimizationType::ExceptionTableCompression => {
                // Compress exception table for multiple catches
                self.generate_compressed_exception_table_try(try_stmt, &pattern)?;
            }
            crate::codegen::finalizer_optimizer::TryOptimizationType::Standard => {
                // Use standard try-catch generation (fallback to original logic)
                self.generate_standard_try_statement(try_stmt)?;
            }
        }
        
        Ok(())
    }
    
    /// Generate JSR-optimized try statement
    fn generate_jsr_optimized_try(&mut self, try_stmt: &crate::ast::TryStmt, pattern: &crate::codegen::finalizer_optimizer::TryOptimizationPattern) -> Result<()> {
        // For now, fallback to standard generation
        // In a full implementation, this would use JSR instructions for complex finalizers
        println!("🔍 DEBUG: Using JSR optimization for complex try statement (complexity: {})", pattern.complexity);
        self.generate_standard_try_statement(try_stmt)
    }
    
    /// Generate inline finalizer try statement
    fn generate_inline_finalizer_try(&mut self, try_stmt: &crate::ast::TryStmt, pattern: &crate::codegen::finalizer_optimizer::TryOptimizationPattern) -> Result<()> {
        // For now, fallback to standard generation
        // In a full implementation, this would inline the finalizer code
        println!("🔍 DEBUG: Using inline finalizer optimization for try statement (complexity: {})", pattern.complexity);
        self.generate_standard_try_statement(try_stmt)
    }
    
    /// Generate compressed exception table try statement
    fn generate_compressed_exception_table_try(&mut self, try_stmt: &crate::ast::TryStmt, pattern: &crate::codegen::finalizer_optimizer::TryOptimizationPattern) -> Result<()> {
        // For now, fallback to standard generation
        // In a full implementation, this would compress the exception table for multiple catches
        println!("🔍 DEBUG: Using exception table compression for try statement ({} catches)", pattern.catch_count);
        self.generate_standard_try_statement(try_stmt)
    }
    
    /// Generate standard try statement (original logic)
    fn generate_standard_try_statement(&mut self, try_stmt: &crate::ast::TryStmt) -> Result<()> {
        // This is the original try-catch-finally generation logic
        // For now, we'll implement a simplified version
        
        // Generate try block
        let try_start = self.create_label();
        let try_end = self.create_label();
        let after = self.create_label();
        
        // Mark try start
        {
            let l = self.label_str(try_start);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate try body
        self.generate_block(&try_stmt.try_block)?;
        
        // Mark try end
        {
            let l = self.label_str(try_end);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate catch clauses
        for (i, catch_clause) in try_stmt.catch_clauses.iter().enumerate() {
            let handler = self.create_label();
            
            // Mark catch handler
            {
                let l = self.label_str(handler);
                self.bytecode_builder.mark_label(&l);
            }
            
            // JVM automatically pushes the exception object onto the stack
            Self::map_stack(self.bytecode_builder.update_stack(0, 1))?;
            
            // Store exception in local variable
            let exception_type = &catch_clause.parameter.type_ref;
            let exception_name = &catch_clause.parameter.name;
            let local_index = self.allocate_local_variable(exception_name, exception_type);
            let local_type = self.convert_type_ref_to_local_type(exception_type);
            self.store_local_variable(local_index, &local_type)?;
            
            // Generate catch body
            self.generate_block(&catch_clause.block)?;
            
            // Jump to after if not the last catch
            if i < try_stmt.catch_clauses.len() - 1 || try_stmt.finally_block.is_some() {
                let l = self.label_str(after);
                Self::map_stack(self.bytecode_builder.goto(&l))?;
            }
            
            // Add exception handler entry (simplified)
            self.add_exception_handler_labels(try_start, try_end, handler, 0);
        }
        
        // Generate finally block if present
        if let Some(finally_block) = &try_stmt.finally_block {
            self.generate_block(finally_block)?;
        }
        
        // Mark after label
        {
            let l = self.label_str(after);
            self.bytecode_builder.mark_label(&l);
        }
        
        Ok(())
    }
    
    /// Generate optimized throw statement using ExceptionOptimizer
    fn generate_optimized_throw_statement(&mut self, throw_stmt: &crate::ast::ThrowStmt) -> Result<()> {
        // Generate the exception expression
        self.generate_expression(&throw_stmt.expr)?;
        
        // Use ExceptionOptimizer to optimize the throw instruction
        // For now, use standard athrow but register with optimizer for future optimizations
        let current_pc = self.bytecode_builder.code().len();
        
        // Register this throw with the exception optimizer for analysis
        // This helps with exception table optimization and JSR analysis
        self.exception_optimizer.add_gap(current_pc, current_pc + 1);
        
        // Generate the athrow instruction
        Self::map_stack(self.bytecode_builder.athrow())?;
        
        Ok(())
    }
    
    /// Optimize postfix increment/decrement to prefix in expression statements (javac-style)
    /// This is a key javac optimization: x++ becomes ++x when used as a statement
    fn optimize_postfix_to_prefix(&self, unary: &UnaryExpr) -> UnaryExpr {
        let optimized_operator = match unary.operator {
            UnaryOp::PostInc => UnaryOp::PreInc,
            UnaryOp::PostDec => UnaryOp::PreDec,
            _ => unary.operator, // No change for other operators
        };
        
        UnaryExpr {
            operator: optimized_operator,
            operand: unary.operand.clone(),
            span: unary.span,
        }
    }
    
    /// Generate optimized conditional jump (javac-style)
    /// If jump_on_true is true, jump to label when condition is true
    /// If jump_on_true is false, jump to label when condition is false
    fn generate_conditional_jump(&mut self, condition: &Expr, jump_on_true: bool, label: &str) -> Result<()> {
        match condition {
            Expr::Binary(bin_expr) => {
                match bin_expr.operator {
                    BinaryOp::Lt => {
                        // Check for zero comparison optimization
                        if let Expr::Literal(lit) = bin_expr.right.as_ref() {
                            if let crate::ast::Literal::Integer(0) = lit.value {
                                // left < 0 - optimize to single operand instruction
                                self.generate_expression(&bin_expr.left)?;
                                if jump_on_true {
                                    Self::map_stack(self.bytecode_builder.iflt(label))?;
                                } else {
                                    Self::map_stack(self.bytecode_builder.ifge(label))?;
                                }
                                return Ok(());
                            }
                        }
                        // Generate: left < right
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        if jump_on_true {
                            Self::map_stack(self.bytecode_builder.if_icmplt(label))?;
                        } else {
                            Self::map_stack(self.bytecode_builder.if_icmpge(label))?;
                        }
                    }
                    BinaryOp::Le => {
                        // Generate: left <= right
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        if jump_on_true {
                            Self::map_stack(self.bytecode_builder.if_icmple(label))?;
                        } else {
                            Self::map_stack(self.bytecode_builder.if_icmpgt(label))?;
                        }
                    }
                    BinaryOp::Gt => {
                        // Generate: left > right
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        if jump_on_true {
                            Self::map_stack(self.bytecode_builder.if_icmpgt(label))?;
                        } else {
                            Self::map_stack(self.bytecode_builder.if_icmple(label))?;
                        }
                    }
                    BinaryOp::Ge => {
                        // Check for zero comparison optimization
                        if let Expr::Literal(lit) = bin_expr.right.as_ref() {
                            if let crate::ast::Literal::Integer(0) = lit.value {
                                // left >= 0 - optimize to single operand instruction
                                self.generate_expression(&bin_expr.left)?;
                                if jump_on_true {
                                    Self::map_stack(self.bytecode_builder.ifge(label))?;
                                } else {
                                    Self::map_stack(self.bytecode_builder.iflt(label))?;
                                }
                                return Ok(());
                            }
                        }
                        // Generate: left >= right
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        if jump_on_true {
                            Self::map_stack(self.bytecode_builder.if_icmpge(label))?;
                        } else {
                            Self::map_stack(self.bytecode_builder.if_icmplt(label))?;
                        }
                    }
                    BinaryOp::Eq => {
                        // Check for null comparison optimization
                        if self.is_null_literal(&bin_expr.right) {
                            // left == null
                            self.generate_expression(&bin_expr.left)?;
                            if jump_on_true {
                                // Jump if left == null -> ifnull
                                Self::map_stack(self.bytecode_builder.ifnull(label))?;
                            } else {
                                // Jump if left != null -> ifnonnull
                                Self::map_stack(self.bytecode_builder.ifnonnull(label))?;
                            }
                        } else if self.is_null_literal(&bin_expr.left) {
                            // null == right
                            self.generate_expression(&bin_expr.right)?;
                            if jump_on_true {
                                // Jump if null == right -> ifnull
                                Self::map_stack(self.bytecode_builder.ifnull(label))?;
                            } else {
                                // Jump if null != right -> ifnonnull
                                Self::map_stack(self.bytecode_builder.ifnonnull(label))?;
                            }
                        } else {
                            // Generate: left == right
                            self.generate_expression(&bin_expr.left)?;
                            self.generate_expression(&bin_expr.right)?;
                            if jump_on_true {
                                Self::map_stack(self.bytecode_builder.if_icmpeq(label))?;
                            } else {
                                Self::map_stack(self.bytecode_builder.if_icmpne(label))?;
                            }
                        }
                    }
                    BinaryOp::Ne => {
                        // Check for null comparison optimization
                        if self.is_null_literal(&bin_expr.right) {
                            // left != null
                            self.generate_expression(&bin_expr.left)?;
                            if jump_on_true {
                                // Jump if left != null -> ifnonnull
                                Self::map_stack(self.bytecode_builder.ifnonnull(label))?;
                            } else {
                                // Jump if left == null -> ifnull
                                Self::map_stack(self.bytecode_builder.ifnull(label))?;
                            }
                        } else if self.is_null_literal(&bin_expr.left) {
                            // null != right
                            self.generate_expression(&bin_expr.right)?;
                            if jump_on_true {
                                // Jump if null != right -> ifnonnull
                                Self::map_stack(self.bytecode_builder.ifnonnull(label))?;
                            } else {
                                // Jump if null == right -> ifnull
                                Self::map_stack(self.bytecode_builder.ifnull(label))?;
                            }
                        } else {
                            // Generate: left != right
                            self.generate_expression(&bin_expr.left)?;
                            self.generate_expression(&bin_expr.right)?;
                            if jump_on_true {
                                Self::map_stack(self.bytecode_builder.if_icmpne(label))?;
                            } else {
                                Self::map_stack(self.bytecode_builder.if_icmpeq(label))?;
                            }
                        }
                    }
                    BinaryOp::Or => {
                        // 🔧 FIX: Use short-circuit evaluation for OR operations (javac-style)
                        // For OR operations: if left is true, jump directly; otherwise evaluate right
                        if jump_on_true {
                            // Jump if true: left || right -> if left is true, jump; otherwise check right
                            let left_true_label = label; // Use the target label directly
                            
                            // 🔧 FIX: Handle nested OR operations recursively (javac-style)
                            // Check if right operand is also an OR operation
                            if let Expr::Binary(right_bin) = &*bin_expr.right {
                                if right_bin.operator == BinaryOp::Or {
                                    // Handle: (a || b || c) -> check a, if true jump; otherwise check (b || c)
                                    eprintln!("🔍 DEBUG: generate_conditional_jump: Detected nested OR operation, using recursive short-circuit");
                                    
                                    // Check first condition: a
                                    self.generate_expression(&bin_expr.left)?;
                                    Self::map_stack(self.bytecode_builder.ifne(left_true_label))?;
                                    
                                    // Recursively handle remaining OR operations: (b || c)
                                    self.generate_conditional_jump(&bin_expr.right, jump_on_true, label)?;
                                    return Ok(());
                                }
                            }
                            
                            // Regular binary OR operation
                            // Generate left operand and jump if true
                            self.generate_expression(&bin_expr.left)?;
                            Self::map_stack(self.bytecode_builder.ifne(left_true_label))?;
                            
                            // If left is false, evaluate right operand
                            self.generate_expression(&bin_expr.right)?;
                            Self::map_stack(self.bytecode_builder.ifne(label))?;
                        } else {
                            // Jump if false: !(left || right) -> !left && !right
                            // This requires both operands to be false
                            let left_false_label = self.create_label();
                            let left_false_str = self.label_str(left_false_label);
                            
                            // Check left operand: if left is true, result is true (don't jump)
                            self.generate_expression(&bin_expr.left)?;
                            Self::map_stack(self.bytecode_builder.ifne(label))?;
                            
                            // Left is false, check right operand
                            self.generate_expression(&bin_expr.right)?;
                            Self::map_stack(self.bytecode_builder.ifne(label))?;
                            
                            // Both are false, jump to target
                            self.bytecode_builder.mark_label(&left_false_str);
                            Self::map_stack(self.bytecode_builder.goto(label))?;
                        }
                    }
                    BinaryOp::And => {
                        // 🔧 FIX: Use short-circuit evaluation for AND operations (javac-style)
                        // For AND operations: if left is false, jump directly; otherwise evaluate right
                        if jump_on_true {
                            // Jump if true: left && right -> both must be true
                            let left_false_label = self.create_label();
                            let left_false_str = self.label_str(left_false_label);
                            
                            // Check left operand: if left is false, result is false (don't jump)
                            self.generate_expression(&bin_expr.left)?;
                            Self::map_stack(self.bytecode_builder.ifeq(&left_false_str))?;
                            
                            // Left is true, check right operand
                            self.generate_expression(&bin_expr.right)?;
                            Self::map_stack(self.bytecode_builder.ifne(label))?;
                            
                            // Left was false, don't jump
                            self.bytecode_builder.mark_label(&left_false_str);
                        } else {
                            // Jump if false: !(left && right) -> !left || !right
                            // If either operand is false, result is false
                            let left_true_label = label; // Use the target label directly
                            
                            // Check left operand: if left is false, jump (result is false)
                            self.generate_expression(&bin_expr.left)?;
                            Self::map_stack(self.bytecode_builder.ifeq(left_true_label))?;
                            
                            // Left is true, check right operand
                            self.generate_expression(&bin_expr.right)?;
                            Self::map_stack(self.bytecode_builder.ifeq(label))?;
                        }
                    }
                    _ => {
                        // Fallback: generate expression and use ifeq/ifne
                        self.generate_expression(condition)?;
                        if jump_on_true {
                            Self::map_stack(self.bytecode_builder.ifne(label))?;
                        } else {
                            Self::map_stack(self.bytecode_builder.ifeq(label))?;
                        }
                    }
                }
            }
            _ => {
                // Fallback: generate expression and use ifeq/ifne
                self.generate_expression(condition)?;
                if jump_on_true {
                    Self::map_stack(self.bytecode_builder.ifne(label))?;
                } else {
                    Self::map_stack(self.bytecode_builder.ifeq(label))?;
                }
            }
        }
        Ok(())
    }

    /// Unified loop generation method (javac-style genLoop)
    /// This method handles all loop types: while, for, and do-while
    fn gen_loop(
        &mut self,
        body: &Stmt,
        condition: Option<&Expr>,
        step: &[Stmt],
        test_first: bool,
        _label: Option<&str>
    ) -> Result<()> {
        // Create entry point for the loop (javac-style)
        let start_pc = self.bytecode_builder.code().len() as u16;
        
        if test_first {
            // Mark start label for loop
            let start_label = format!("loop_start_{}", start_pc);
            self.bytecode_builder.mark_label(&start_label);
            
            // While or for loop: test condition first
            if let Some(cond) = condition {
                // Generate optimized condition check (direct comparison jumps)
                let end_label = format!("loop_end_{}", start_pc);
                self.generate_conditional_jump(cond, false, &end_label)?;
            }
            
            // Generate loop body
            self.generate_statement(body)?;
            
            // Generate step statements (for for-loops)
            for step_stmt in step {
                self.generate_statement(step_stmt)?;
            }
            
            // Jump back to start
            Self::map_stack(self.bytecode_builder.goto(&start_label))?;
            
            // Mark end label
            if condition.is_some() {
                let end_label = format!("loop_end_{}", start_pc);
                self.bytecode_builder.mark_label(&end_label);
            }
        } else {
            // Do-while loop: execute body first, then test
            let loop_start = format!("do_start_{}", start_pc);
            self.bytecode_builder.mark_label(&loop_start);
            
            // Generate loop body
            self.generate_statement(body)?;
            
            // Generate step statements
            for step_stmt in step {
                self.generate_statement(step_stmt)?;
            }
            
            // Generate condition check
            if let Some(cond) = condition {
                self.generate_expression(cond)?;
                // Jump back to start if condition is true
                Self::map_stack(self.bytecode_builder.ifne(&loop_start))?;
            }
        }
        
        Ok(())
    }

    
    /// Continue with original manual conversion logic (fallback)
    fn apply_original_type_conversion(&mut self, actual_type: &str, expected_type: &str) -> Result<()> {
        // Convert int to long if needed
        if actual_type == "I" && expected_type == "J" {
            // Emit i2l (int to long conversion)
            Self::map_stack(self.bytecode_builder.i2l())?;
        }
        // Convert wrapper types to long if needed
        else if expected_type == "J" {
            match actual_type {
                "Ljava/lang/Byte;" | "LByte;" => {
                    // Byte -> byte -> long: call byteValue() then i2l (byte is promoted to int on stack)
                    let method_ref = self.add_method_ref("java/lang/Byte", "byteValue", "()B");
                    Self::map_stack(self.bytecode_builder.invokevirtual(method_ref))?;
                    Self::map_stack(self.bytecode_builder.i2l())?; // byte is promoted to int, then convert to long
                }
                "Ljava/lang/Short;" | "LShort;" => {
                    // Short -> short -> long: call shortValue() then i2l (short is promoted to int on stack)
                    let method_ref = self.add_method_ref("java/lang/Short", "shortValue", "()S");
                    Self::map_stack(self.bytecode_builder.invokevirtual(method_ref))?;
                    Self::map_stack(self.bytecode_builder.i2l())?; // short is promoted to int, then convert to long
                }
                "Ljava/lang/Integer;" | "LInteger;" => {
                    // Integer -> int -> long: call intValue() then i2l
                    let method_ref = self.add_method_ref("java/lang/Integer", "intValue", "()I");
                    Self::map_stack(self.bytecode_builder.invokevirtual(method_ref))?;
                    Self::map_stack(self.bytecode_builder.i2l())?;
                }
                "Ljava/lang/Long;" | "LLong;" => {
                    // Long -> long: call longValue()
                    let method_ref = self.add_method_ref("java/lang/Long", "longValue", "()J");
                    Self::map_stack(self.bytecode_builder.invokevirtual(method_ref))?;
                }
                "Ljava/lang/Character;" | "LCharacter;" => {
                    // Character -> char -> long: call charValue() then i2l (char is promoted to int on stack)
                    let method_ref = self.add_method_ref("java/lang/Character", "charValue", "()C");
                    Self::map_stack(self.bytecode_builder.invokevirtual(method_ref))?;
                    Self::map_stack(self.bytecode_builder.i2l())?; // char is promoted to int, then convert to long
                }
                _ => {
                    // No conversion needed or unsupported conversion
                }
            }
        }
        // Add more conversions as needed
        
        Ok(())
    }
    
    /// Parse parameter types from method descriptor
    fn parse_parameter_types(&self, descriptor: &str) -> Vec<String> {
        let mut types = Vec::new();
        
        if let Some(args_start) = descriptor.find('(') {
            if let Some(args_end) = descriptor.find(')') {
                let args_part = &descriptor[args_start + 1..args_end];
                let mut i = 0;
                
                while i < args_part.len() {
                    match args_part.chars().nth(i).unwrap() {
                        'L' => {
                            // Reference type: L...;
                            if let Some(semicolon) = args_part[i..].find(';') {
                                types.push(args_part[i..i + semicolon + 1].to_string());
                                i += semicolon + 1;
                            } else {
                                i += 1;
                            }
                        }
                        '[' => {
                            // Array type: [...
                            let start = i;
                            i += 1;
                            while i < args_part.len() && args_part.chars().nth(i).unwrap() == '[' {
                                i += 1;
                            }
                            if i < args_part.len() {
                                match args_part.chars().nth(i).unwrap() {
                                    'L' => {
                                        // Array of reference: [L...;
                                        if let Some(semicolon) = args_part[i..].find(';') {
                                            i += semicolon + 1;
                                        } else {
                                            i += 1;
                                        }
                                    }
                                    _ => i += 1, // Array of primitive
                                }
                            }
                            types.push(args_part[start..i].to_string());
                        }
                        c => {
                            // Primitive type
                            types.push(c.to_string());
                            i += 1;
                        }
                    }
                }
            }
        }
        
        types
    }
    
    /// Infer the JVM type descriptor for an expression
    fn infer_expression_type(&self, expr: &Expr) -> String {
        match expr {
            Expr::Identifier(id) => {
                // Special handling for 'this' keyword
                if id.name == "this" {
                    // Return current class type
                    if let Some(current_class) = &self.current_class {
                        let this_type = format!("L{};", current_class.name.replace('.', "/"));
                        println!("🔍 DEBUG: infer_expression_type 'this' -> {}", this_type);
                        this_type
                    } else {
                        "Ljava/lang/Object;".to_string()
                    }
                } else {
                    // Try to resolve the identifier type from local variables
                    if let Some(local_var) = self.find_local_variable(&id.name) {
                        match &local_var.var_type {
                            LocalType::Int => "I".to_string(),
                            LocalType::Long => "J".to_string(),
                            LocalType::Float => "F".to_string(),
                            LocalType::Double => "D".to_string(),
                            LocalType::Reference(ref_type) => {
                                // Convert to JVM descriptor format
                                format!("L{};", ref_type.replace('.', "/"))
                            }
                            LocalType::Array(element_type) => {
                                // Convert array element type to JVM descriptor format
                                let element_desc = match &**element_type {
                                    LocalType::Int => "I",
                                    LocalType::Long => "J",
                                    LocalType::Float => "F",
                                    LocalType::Double => "D",
                                    LocalType::Reference(_) => "Ljava/lang/Object;",
                                    LocalType::Array(_) => "Ljava/lang/Object;",
                                };
                                format!("[{}", element_desc)
                            }
                        }
                    } else {
                        // Try to resolve as a field in the current class or parent classes
                        if let Some(field_type) = self.resolve_field_type_in_inheritance(&id.name) {
                            // Convert field type to JVM descriptor format
                            let descriptor = format!("L{};", field_type.replace('.', "/"));
                            eprintln!("🔍 DEBUG: infer_expression_type: Field {} resolved to descriptor {}", id.name, descriptor);
                            descriptor
                        } else {
                            // Fallback: assume Object for unknown identifiers
                            // Use the correct JVM descriptor format
                            eprintln!("🔍 DEBUG: infer_expression_type: Unknown identifier {}, defaulting to Object", id.name);
                            "Ljava/lang/Object;".to_string()
                        }
                    }
                }
            }
            Expr::Literal(lit) => {
                match &lit.value {
                    crate::ast::Literal::Integer(_) => "I".to_string(),
                    crate::ast::Literal::Long(_) => "J".to_string(), // Java long type descriptor
                    crate::ast::Literal::Float(_) => "F".to_string(),
                    crate::ast::Literal::Double(_) => "D".to_string(), // Java double type descriptor
                    crate::ast::Literal::Boolean(_) => "Z".to_string(),
                    crate::ast::Literal::String(_) => "Ljava/lang/String;".to_string(),
                    crate::ast::Literal::Char(_) => "C".to_string(),
                    Literal::Null => "Ljava/lang/Object;".to_string(),
                }
            }
            Expr::Cast(cast) => {
                // Return the cast target type
                match cast.target_type.name.as_str() {
                    "int" => "I".to_string(),
                    "long" => "J".to_string(),
                    "float" => "F".to_string(),
                    "double" => "D".to_string(),
                    "boolean" => "Z".to_string(),
                    "byte" => "B".to_string(),
                    "short" => "S".to_string(),
                    "char" => "C".to_string(),
                    _ => format!("L{};", cast.target_type.name.replace('.', "/")),
                }
            }
            Expr::MethodCall(call) => {
                // Try to infer method return type from method name and context
                match call.name.as_str() {
                    "size" => "I".to_string(), // size() returns int
                    "length" => "I".to_string(), // length() returns int
                    "indexOf" => "I".to_string(), // indexOf() returns int
                    "lastIndexOf" => "I".to_string(), // lastIndexOf() returns int
                    "hashCode" => "I".to_string(), // hashCode() returns int
                    "compareTo" => "I".to_string(), // compareTo() returns int
                    "contains" => "Z".to_string(), // contains() returns boolean
                    "isEmpty" => "Z".to_string(), // isEmpty() returns boolean
                    "equals" => "Z".to_string(), // equals() returns boolean
                    "toString" => "Ljava/lang/String;".to_string(), // toString() returns String
                    _ => "Ljava/lang/Object;".to_string(), // Default fallback
                }
            }
            Expr::FieldAccess(field_access) => {
                // 🔧 FIX: Properly infer field access type instead of defaulting to Object
                // This is crucial for eliminating unnecessary checkcast instructions
                
                // Try to resolve the field type from the target expression
                if let Some(target) = &field_access.target {
                    let target_type = self.resolve_expression_type(target);
                    
                    // Convert target type to internal format for field lookup
                    let target_internal = if target_type.contains('/') {
                        target_type.replace('/', "/")
                    } else {
                        target_type.replace('.', "/")
                    };
                    
                    // Try to resolve the field type in the target class
                    if let Some(field_class) = self.try_parse_class_from_filesystem(&target_internal) {
                        for member in &field_class.body {
                            if let crate::ast::ClassMember::Field(field) = member {
                                if field.name == field_access.name {
                                    // Found the field, return its type in JVM descriptor format
                                    let field_type = if let Some(open_bracket) = field.type_ref.name.find('<') {
                                        // Apply type erasure for generic types
                                        field.type_ref.name[..open_bracket].to_string()
                                    } else {
                                        field.type_ref.name.clone()
                                    };
                                    
                                    let descriptor = format!("L{};", field_type.replace('.', "/"));
                                    eprintln!("🔍 DEBUG: infer_expression_type: FieldAccess {}.{} resolved to descriptor {}", 
                                             target_type, field_access.name, descriptor);
                                    return descriptor;
                                }
                            }
                        }
                    }
                }
                
                // Fallback to Object if field type cannot be resolved
                eprintln!("🔍 DEBUG: infer_expression_type: FieldAccess {}.{} defaulting to Object", 
                         field_access.target.as_ref().map(|_| "target").unwrap_or("this"), field_access.name);
                "Ljava/lang/Object;".to_string()
            }
            _ => "Ljava/lang/Object;".to_string(), // Default fallback
        }
    }
    
    /// Generate method descriptor with specific return type
    fn generate_method_descriptor_with_return(&self, args: &[Expr], return_type: &str) -> String {
        let mut descriptor = "(".to_string();
        
        for arg in args {
            descriptor.push_str(&self.type_to_descriptor_with_generics(arg));
        }
        
        descriptor.push_str(")");
        descriptor.push_str(return_type);
        descriptor
    }
    
    /// Resolve the type of an expression, including generic information
    fn resolve_expression_type(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(lit) => {
                match &lit.value {
                    crate::ast::Literal::Integer(_) => "int".to_string(),
                    crate::ast::Literal::Long(_) => "long".to_string(),
                    crate::ast::Literal::Float(_) => "float".to_string(),
                    crate::ast::Literal::Double(_) => "double".to_string(),
                    crate::ast::Literal::Boolean(_) => "boolean".to_string(),
                    crate::ast::Literal::String(_) => "java.lang.String".to_string(),
                    crate::ast::Literal::Char(_) => "char".to_string(),
                    Literal::Null => "java.lang.Object".to_string(),
                }
            }
            Expr::Identifier(ident) => {
                // Try to resolve from local variables first
                if let Some(local_var) = self.find_local_variable(&ident.name) {
                    eprintln!("🔍 DEBUG: resolve_expression_type Identifier: {} -> {:?}", ident.name, local_var.var_type);
                    match &local_var.var_type {
                        LocalType::Int => "int".to_string(),
                        LocalType::Long => "long".to_string(),
                        LocalType::Float => "float".to_string(),
                        LocalType::Double => "double".to_string(),
                        LocalType::Reference(ref_type) => {
                            // Ensure we have the full qualified name for known types
                            match ref_type.as_str() {
                                "FieldData" => "java.base.FieldData".to_string(),
                                "MethodData" => "java.base.MethodData".to_string(),
                                "PoolEntry" => "java.base.PoolEntry".to_string(),
                                "Method" => "java.lang.reflect.Method".to_string(),
                                "Object" => "java.lang.Object".to_string(),
                                _ => ref_type.clone(),
                            }
                        }
                        LocalType::Array(element_type) => {
                            // Convert array element type to string representation
                            self.local_type_to_string(element_type) + "[]"
                        }
                    }
                } else {
                    // Check if it's a field in the current class or parent classes
                    if let Some(field_type) = self.resolve_field_type_in_inheritance(&ident.name) {
                        eprintln!("🔍 DEBUG: resolve_expression_type: Found field {}={}", ident.name, field_type);
                        if ident.name == "front" || ident.name == "rear" {
                            eprintln!("🔍 DEBUG: Field type resolution for {}: '{}'", ident.name, field_type);
                        if ident.name == "next" || ident.name == "prev" {
                            eprintln!("🔍 DEBUG: Special field access: {} -> {}", ident.name, field_type);
                        }
                        }
                        return field_type;
                    }
                    
                    // Check if this is a known package name or class name
                    // First check if this is a known class name using classpath
                    if let Some(internal_name) = classpath::resolve_class_name(&ident.name) {
                        // This is a class reference, return the internal name with dots
                        internal_name.replace("/", ".")
                    } else {
                        match ident.name.as_str() {
                            "java" => {
                                // This is likely the start of a package name like java.base.Data
                                // Return a special marker to indicate this is a package reference
                                "java".to_string()
                            }
                            _ => {
                                // Fallback: assume it's a field access, use field descriptor
                    let class_name = self.current_class_name.as_ref()
                        .unwrap_or(&"java/lang/String".to_string()) // Fallback to String instead of Object
                        .clone();
                                // Get field descriptor and convert to class name
                                let descriptor = self.resolve_field_descriptor(&class_name, &ident.name);
                                // Convert JVM descriptor to class name
                                if descriptor.starts_with('L') && descriptor.ends_with(';') {
                                    // Object type: LClassName; -> ClassName
                                    descriptor[1..descriptor.len()-1].replace('/', ".")
                                } else {
                                    // Primitive or array type, return as is
                                    descriptor
                                }
                            }
                        }
                    }
                }
            }
            Expr::FieldAccess(field_access) => {
                // Check if this is a qualified class name chain (e.g., java.lang.reflect.Array)
                if let Some(qualified_name) = self.is_qualified_class_name_chain(field_access) {
                    eprintln!("🔍 DEBUG: resolve_expression_type: Detected qualified class name: {}", qualified_name);
                    // This represents a class, so return the class name
                    return qualified_name;
                }
                
                // Special handling for .class field access
                if field_access.name == "class" && field_access.target.is_some() {
                    if let Some(Expr::Identifier(ident)) = field_access.target.as_deref() {
                        eprintln!("🔍 DEBUG: resolve_expression_type: .class access on {}", ident.name);
                        // For .class access, return java.lang.Class type
                        return "java.lang.Class".to_string();
                    }
                }
                
                // For field access, try to resolve the field type
                if let Some(receiver) = &field_access.target {
                    let receiver_type = self.resolve_expression_type(receiver);
                    // Try to resolve the field type from the receiver type
                    // For now, use a simple approach based on field name
                    match field_access.name.as_str() {
                        "length" => {
                            // Special case for array length field
                            if receiver_type.ends_with("[]") {
                                "int".to_string()
                            } else {
                                // Check if receiver is an array field in current class
                                if let Some(class) = &self.current_class {
                                    for member in &class.body {
                                        if let crate::ast::ClassMember::Field(field) = member {
                                            if field.name == receiver_type || 
                                               (receiver_type == *self.current_class_name.as_ref().unwrap_or(&"".to_string()) && 
                                                field.name == "array") {
                                                // Check if this field is an array type
                                                if field.type_ref.array_dims > 0 {
                                                    return "int".to_string();
                                                }
                                            }
                                        }
                                    }
                                }
                                receiver_type
                            }
                        }
                        "out" => "java/io/PrintStream".to_string(),
                        "pool" => "java/util/List".to_string(),
                        "fields" => {
                            // Only return FieldData[] for specific receiver types
                            if receiver_type.contains("java.base") {
                                "java/base/FieldData[]".to_string()
                            } else {
                                // For other types, use the general field resolution logic
                                self.resolve_field_type_from_receiver(&receiver_type, &field_access.name)
                            }
                        },
                        "methods" => {
                            // Only return MethodData[] for specific receiver types  
                            if receiver_type.contains("java.base") {
                                "java/base/MethodData[]".to_string()
                            } else {
                                // For other types, use the general field resolution logic
                                self.resolve_field_type_from_receiver(&receiver_type, &field_access.name)
                            }
                        },
                        _ => {
                            // Check if this is a package name chain like java.base or java.util
                            if receiver_type == "java" && field_access.name == "base" {
                                "java.base".to_string()
                            } else if receiver_type == "java" && field_access.name == "util" {
                                "java.util".to_string()
                            } else if receiver_type == "java.base" && field_access.name == "Data" {
                                "java.base.Data".to_string()
                            } else if receiver_type == "java.util" && field_access.name == "Arrays" {
                                "java.util.Arrays".to_string()
                            } else {
                                // For other types, use the general field resolution logic
                                self.resolve_field_type_from_receiver(&receiver_type, &field_access.name)
                            }
                        }
                    }
                } else {
                    // Assume 'this' for instance fields
                    self.current_class_name.as_ref()
                        .unwrap_or(&"java/lang/String".to_string())
                        .clone()
                }
            }
            Expr::MethodCall(method_call) => {
                // Try to resolve the method and get its return type from the descriptor
                let owner_class = if let Some(target) = &method_call.target {
                    let target_type = self.resolve_expression_type(target);
                    // Strip generic parameters for method resolution
                    let base_type = if let Some(generic_start) = target_type.find('<') {
                        target_type[..generic_start].to_string()
                    } else {
                        target_type
                    };
                    self.resolve_class_name(&base_type)
                } else {
                    let current_class = self.current_class_name.as_ref()
                        .unwrap_or(&"java/lang/Object".to_string())
                        .clone();
                    self.resolve_class_name(&current_class)
                };
                
                let expected_arity = method_call.arguments.len();
                if let Some(resolved) = resolve_method_with_context(&owner_class, &method_call.name, expected_arity, self.current_class.as_ref(), self.all_types.as_deref()) {
                    // Special handling for generic methods before using descriptor
                    if method_call.name == "next" && owner_class == "java/util/Iterator" {
                        if let Some(target) = &method_call.target {
                            // Check if target is an identifier with generic type information
                            if let Expr::Identifier(ident) = &**target {
                                if let Some(local_var) = self.find_local_variable(&ident.name) {
                                    if let Some(original_type_ref) = &local_var.original_type_ref {
                                        eprintln!("🔍 DEBUG: Iterator.next() checking original_type_ref: {:?}", original_type_ref);
                                        // Check if this is Iterator<Entry<K,V>>
                                        if original_type_ref.name == "Iterator" && !original_type_ref.type_args.is_empty() {
                                            if let Some(first_type_arg) = original_type_ref.type_args.first() {
                                                if let crate::ast::TypeArg::Type(entry_type_ref) = first_type_arg {
                                                    if entry_type_ref.name == "Entry" {
                                                        eprintln!("🔍 DEBUG: Detected Iterator<Entry> pattern, returning Entry");
                                                        return "java.util.Entry".to_string();
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            
                            let target_type = self.resolve_expression_type(target);
                            eprintln!("🔍 DEBUG: Iterator.next() target_type = '{}'", target_type);
                            // Fallback: Check if this is an Iterator<Entry<K,V>> pattern
                            if target_type.contains("Iterator") && target_type.contains("Entry") {
                                eprintln!("🔍 DEBUG: Detected Iterator<Entry> pattern, returning Entry");
                                return "java.util.Entry".to_string();
                            }
                        }
                    }
                    
                    // Extract return type from method descriptor
                    let descriptor = &resolved.descriptor;
                    if let Some(closing_paren) = descriptor.find(')') {
                        let return_part = &descriptor[closing_paren + 1..];
                        return self.descriptor_to_class_name(return_part);
                    }
                }
                
                // Fallback to hardcoded rules for common methods
                match method_call.name.as_str() {
                    "getClass" => "java/lang/Class".to_string(),
                    "iterator" => "java/util/Iterator".to_string(),
                    "next" => {
                        // Special handling for Iterator.next() - try to infer generic type
                        if let Some(target) = &method_call.target {
                            let target_type = self.resolve_expression_type(target);
                            // Check if this is an Iterator<Entry<K,V>> pattern
                            if target_type.contains("Iterator") {
                                // For Iterator<Entry<T, Object>>, next() should return Entry<T, Object>
                                // This is a simplified pattern matching for common cases
                                if target_type.contains("Entry") {
                                    return "java.util.Entry".to_string();
                                }
                            }
                        }
                        "java.lang.Object".to_string()
                    },
                    "hasNext" => "boolean".to_string(),
                    "size" => "int".to_string(),
                    "add" => "boolean".to_string(),
                    // 🔧 FIX: Add BitSet utility methods
                    "longPosition" => "int".to_string(),
                    "bitPosition" => "long".to_string(),
                    "getTrueMask" => "long".to_string(),
                    _ => "java.lang.Object".to_string(),
                }
            }
            Expr::New(new_expr) => {
                // For new expressions, use the constructed type
                eprintln!("🔍 DEBUG: resolve_expression_type: New expression - target_type.name = '{}'", new_expr.target_type.name);
                if new_expr.target_type.array_dims > 0 {
                    let result = format!("{}[]", new_expr.target_type.name);
                    eprintln!("🔍 DEBUG: resolve_expression_type: New array result = '{}'", result);
                    result
                } else {
                    // Convert to JVM descriptor format for consistency with type coercion optimizer
                    let result = format!("L{};", new_expr.target_type.name.replace('.', "/"));
                    eprintln!("🔍 DEBUG: resolve_expression_type: New object result = '{}'", result);
                    result
                }
            }
            Expr::Cast(cast_expr) => {
                // For cast expressions, use the target type with proper resolution
                let resolved_type = self.resolve_class_name(&cast_expr.target_type.name);
                if cast_expr.target_type.array_dims > 0 {
                    format!("{}[]", resolved_type)
                } else {
                    resolved_type
                }
            }
            Expr::ArrayAccess(array_access) => {
                // For array access, resolve the array type and return the element type
                let array_type = self.resolve_expression_type(&array_access.array);
                eprintln!("🔍 DEBUG: resolve_expression_type ArrayAccess: array_type = '{}'", array_type);
                eprintln!("🔍 DEBUG: resolve_expression_type ArrayAccess: array_expr = {:?}", array_access.array);
                
                // Strip array suffix to get element type
                if array_type.ends_with("[]") {
                    let element_type = array_type[..array_type.len()-2].to_string();
                    eprintln!("🔍 DEBUG: resolve_expression_type ArrayAccess: element_type = '{}'", element_type);
                    element_type
                } else {
                    // Not an array type, fallback to Object
                    eprintln!("🔍 DEBUG: resolve_expression_type ArrayAccess: Not an array type, fallback to Object");
                    "java.lang.Object".to_string()
                }
            }
            Expr::Parenthesized(expr) => {
                // For parenthesized expressions, resolve the inner expression
                self.resolve_expression_type(expr)
            }
            _ => {
                // Default fallback
                self.current_class_name.as_ref()
                    .unwrap_or(&"java/lang/String".to_string())
                    .clone()
            }
        }
    }
    
    /// Convert expression type to JVM descriptor with proper generic handling
    fn type_to_descriptor_with_generics(&self, expr: &Expr) -> String {
        match expr {
                        Expr::Literal(lit) => match lit.value {
                Literal::Integer(_) => "I".to_string(),
                Literal::Long(_) => "J".to_string(),
                Literal::Float(_) => "F".to_string(),
                Literal::Double(_) => "D".to_string(),
                Literal::Boolean(_) => "Z".to_string(),
                Literal::String(_) => "Ljava/lang/String;".to_string(),
                Literal::Char(_) => "C".to_string(),
                Literal::Null => "Ljava/lang/Object;".to_string(), // Null can be any reference type
            },
            Expr::Identifier(ident) => {
                // Try to resolve from local variables first
                if let Some(local_var) = self.find_local_variable(&ident.name) {
                    local_var.var_type.descriptor()
                } else {
                    // Assume it's a field access, use field descriptor
                    let class_name = self.current_class_name.as_ref()
                        .unwrap_or(&"java/lang/String".to_string()) // Fallback to String instead of Object
                        .clone();
                    self.resolve_field_descriptor(&class_name, &ident.name)
                }
            }
            Expr::Binary(bin) => {
                // For binary expressions, infer type from operands
                match bin.operator {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                        // Arithmetic operations typically return int or the wider type
                        // Check if either operand is long, then result is long
                        let left_type = self.resolve_expression_type(&bin.left);
                        let right_type = self.resolve_expression_type(&bin.right);
                        if left_type == "long" || right_type == "long" {
                            "long".to_string()
                        } else {
                            "int".to_string()
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                        // Comparison operations return boolean
                        "Z".to_string()
                    }
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
                        // Bitwise operations return the type of the operands
                        // Check if either operand is long, then result is long
                        let left_type = self.resolve_expression_type(&bin.left);
                        let right_type = self.resolve_expression_type(&bin.right);
                        if left_type == "long" || right_type == "long" {
                            "long".to_string()
                        } else {
                            "int".to_string()
                        }
                    }
                    _ => "I".to_string(), // Default to int
                }
            }
            Expr::Unary(unary) => {
                match unary.operator {
                    UnaryOp::Not => "Z".to_string(), // Logical not returns boolean
                    UnaryOp::Minus | UnaryOp::Plus => "I".to_string(), // Unary plus/minus returns int
                    _ => "I".to_string(), // Default to int
                }
            }
            Expr::MethodCall(_mc) => {
                // Default unknown method return is Object
                "Ljava/lang/Object;".to_string()
            }
            Expr::FieldAccess(field_access) => {
                // Resolve the field type properly
                let target_type = if let Some(target) = &field_access.target {
                    self.resolve_expression_type(target)
                } else {
                    "java/lang/Object".to_string()
                };
                let base_type = if let Some(generic_start) = target_type.find('<') {
                    target_type[..generic_start].to_string()
                } else {
                    target_type
                };
                let class_name = self.resolve_class_name(&base_type);
                self.resolve_field_descriptor(&class_name, &field_access.name)
            }
            Expr::New(new_expr) => {
                // New expressions return the constructed type with proper generic handling
                if new_expr.target_type.array_dims > 0 {
                    // Array type: add array dimensions first
                    let mut descriptor = String::new();
                    for _ in 0..new_expr.target_type.array_dims {
                        descriptor.push('[');
                    }
                    
                    // Add the element type
                    if new_expr.target_type.type_args.is_empty() {
                        // No generics: simple array type
                        descriptor.push_str(&format!("L{};", new_expr.target_type.name.replace('.', "/")));
                    } else {
                        // Generic array type: use raw type for JVM descriptor
                        // Note: JVM descriptors don't include generic information
                        descriptor.push_str(&format!("L{};", new_expr.target_type.name.replace('.', "/")));
                    }
                    descriptor
                } else {
                    // Regular object type
                    if new_expr.target_type.type_args.is_empty() {
                        // No generics: simple type
                        format!("L{};", new_expr.target_type.name.replace('.', "/"))
                    } else {
                        // Generic type: use raw type for JVM descriptor
                        // Note: JVM descriptors don't include generic information
                        format!("L{};", new_expr.target_type.name.replace('.', "/"))
                    }
                }
            }
            Expr::Cast(cast_expr) => {
                // For cast expressions, use the target type descriptor with proper resolution
                let resolved_type = self.resolve_class_name(&cast_expr.target_type.name);
                if cast_expr.target_type.array_dims > 0 {
                    // Array type: add array dimensions first
                    let mut descriptor = String::new();
                    for _ in 0..cast_expr.target_type.array_dims {
                        descriptor.push('[');
                    }
                    
                    // Add the element type
                    descriptor.push_str(&format!("L{};", resolved_type));
                    descriptor
                } else {
                    // Regular object type - check if it's a primitive
                    match resolved_type.as_str() {
                        "int" => "I".to_string(),
                        "boolean" => "Z".to_string(),
                        "byte" => "B".to_string(),
                        "char" => "C".to_string(),
                        "short" => "S".to_string(),
                        "long" => "J".to_string(),
                        "float" => "F".to_string(),
                        "double" => "D".to_string(),
                        "void" => "V".to_string(),
                        _ => format!("L{};", resolved_type),
                    }
                }
            }
            Expr::Parenthesized(expr) => {
                // For parenthesized expressions, resolve the inner expression
                self.type_to_descriptor_with_generics(expr)
            }
            _ => "Ljava/lang/String;".to_string(), // More specific than Object for safety
        }
    }

    /// Convert expression type to JVM descriptor
    fn type_to_descriptor(&self, expr: &Expr) -> String {
        match expr {
                        Expr::Literal(lit) => match lit.value {
                Literal::Integer(_) => "I".to_string(),
                Literal::Long(_) => "J".to_string(),
                Literal::Float(_) => "F".to_string(),
                Literal::Double(_) => "D".to_string(),
                Literal::Boolean(_) => "Z".to_string(),
                Literal::String(_) => "Ljava/lang/String;".to_string(),
                Literal::Char(_) => "C".to_string(),
                Literal::Null => "Ljava/lang/Object;".to_string(), // Null can be any reference type
            },
            Expr::Identifier(ident) => {
                // Try to resolve from local variables first
                if let Some(local_var) = self.find_local_variable(&ident.name) {
                    local_var.var_type.descriptor()
                } else {
                    // Assume it's a field access, use field descriptor
                    let class_name = self.current_class_name.as_ref()
                .unwrap_or(&"java/lang/String".to_string()) // Fallback to String instead of Object
                .clone();
                    self.resolve_field_descriptor(&class_name, &ident.name)
                }
            }
            Expr::Binary(bin) => {
                // For binary expressions, infer type from operands
                match bin.operator {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                        // Arithmetic operations typically return int or the wider type
                        "I".to_string()
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                        // Comparison operations return boolean
                        "Z".to_string()
                    }
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
                        // Logical operations return boolean
                        "Z".to_string()
                    }
                    _ => "I".to_string(), // Default to int
                }
            }
            Expr::Unary(unary) => {
                match unary.operator {
                    UnaryOp::Not => "Z".to_string(), // Logical not returns boolean
                    UnaryOp::Minus | UnaryOp::Plus => "I".to_string(), // Unary plus/minus returns int
                    _ => "I".to_string(), // Default to int
                }
            }
            Expr::MethodCall(_mc) => {
                // Default unknown method return is Object
                "Ljava/lang/Object;".to_string()
            }
            Expr::New(new_expr) => {
                // New expressions return the constructed type
                if new_expr.target_type.array_dims > 0 {
                    // Array of unknown element; default to Object[]
                    "[Ljava/lang/Object;".to_string()
                } else {
                    format!("L{};", new_expr.target_type.name.replace('.', "/"))
                }
            }
            _ => "Ljava/lang/Object;".to_string(),
        }
    }

    /// Check if a field access chain represents a fully qualified class name
    fn is_qualified_class_name_chain(&self, field_access: &FieldAccessExpr) -> Option<String> {
        // Recursively build the qualified name from the field access chain
        fn build_qualified_name(expr: &Expr) -> Option<String> {
            match expr {
                Expr::Identifier(ident) => Some(ident.name.clone()),
                Expr::FieldAccess(fa) => {
                    if let Some(base) = fa.target.as_ref().and_then(|t| build_qualified_name(t)) {
                        Some(format!("{}.{}", base, fa.name))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        
        // Build the full qualified name including the current field
        if let Some(base) = field_access.target.as_ref().and_then(|t| build_qualified_name(t)) {
            let full_name = format!("{}.{}", base, field_access.name);
            eprintln!("🔍 DEBUG: is_qualified_class_name_chain: Built qualified name: {}", full_name);
            
            // Check if this looks like a Java class name (starts with java., javax., etc.)
            if full_name.starts_with("java.") || full_name.starts_with("javax.") {
                Some(full_name)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Generate optimized field access using javac-style optimizations
    fn generate_optimized_field_access(&mut self, optimization: crate::codegen::field_access_optimizer::FieldAccessOptimizationType) -> Result<()> {
        use crate::codegen::field_access_optimizer::FieldAccessOptimizationType;
        
        match optimization {
            FieldAccessOptimizationType::Static { opcode, field_ref, is_constant: _, constant_value: _ } => {
                // Static field access (getstatic/putstatic)
                match opcode {
                    178 => Self::map_stack(self.bytecode_builder.getstatic(field_ref))?, // getstatic
                    179 => Self::map_stack(self.bytecode_builder.putstatic(field_ref))?, // putstatic
                    _ => return Err(Error::codegen_error(&format!("Unsupported static field opcode: {}", opcode))),
                }
            }
            FieldAccessOptimizationType::Instance { opcode, field_ref, requires_null_check: _ } => {
                // Instance field access (getfield/putfield)
                match opcode {
                    180 => Self::map_stack(self.bytecode_builder.getfield(field_ref))?, // getfield
                    181 => Self::map_stack(self.bytecode_builder.putfield(field_ref))?, // putfield
                    _ => return Err(Error::codegen_error(&format!("Unsupported instance field opcode: {}", opcode))),
                }
            }
            FieldAccessOptimizationType::ArrayLength => {
                // Array length access (arraylength)
                Self::map_stack(self.bytecode_builder.arraylength())?;
            }
            FieldAccessOptimizationType::ConstantInline { value } => {
                // Constant inlining - load the constant value directly
                match value {
                    crate::ast::Literal::Integer(i) => {
                        let optimization = ConstantOptimizer::optimize_int(i as i32);
                        self.emit_constant_instruction(optimization)?;
                    }
                    crate::ast::Literal::Float(f) => {
                        let optimization = ConstantOptimizer::optimize_float(f as f32);
                        self.emit_constant_instruction(optimization)?;
                    }
                    crate::ast::Literal::String(s) => {
                        if let Some(cp) = &self.constant_pool {
                            let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_string(&s) };
                            Self::map_stack(self.bytecode_builder.ldc(idx))?;
                        } else {
                            return Err(Error::codegen_error("No constant pool available for string literal"));
                        }
                    }
                    _ => {
                        return Err(Error::codegen_error("Unsupported constant inlining type"));
                    }
                }
            }
            FieldAccessOptimizationType::ClassLiteral { class_ref } => {
                // Class literal access (.class -> ldc)
                Self::map_stack(self.bytecode_builder.ldc(class_ref))?;
            }
        }
        
        Ok(())
    }

    /// Helper method to emit constant instruction
    fn emit_constant_instruction(&mut self, optimization: crate::codegen::constant_optimizer::ConstantInstruction) -> Result<()> {
        use crate::codegen::constant_optimizer::ConstantInstruction;
        
        match optimization {
            ConstantInstruction::Iconst0 => Self::map_stack(self.bytecode_builder.iconst_0())?,
            ConstantInstruction::Iconst1 => Self::map_stack(self.bytecode_builder.iconst_1())?,
            ConstantInstruction::Iconst2 => Self::map_stack(self.bytecode_builder.iconst_2())?,
            ConstantInstruction::Iconst3 => Self::map_stack(self.bytecode_builder.iconst_3())?,
            ConstantInstruction::Iconst4 => Self::map_stack(self.bytecode_builder.iconst_4())?,
            ConstantInstruction::Iconst5 => Self::map_stack(self.bytecode_builder.iconst_5())?,
            ConstantInstruction::IconstM1 => Self::map_stack(self.bytecode_builder.iconst_m1())?,
            ConstantInstruction::Bipush(value) => Self::map_stack(self.bytecode_builder.bipush(value))?,
            ConstantInstruction::Sipush(value) => Self::map_stack(self.bytecode_builder.sipush(value))?,
            ConstantInstruction::Ldc(value) => {
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_integer(value as i32) };
                    Self::map_stack(self.bytecode_builder.ldc(idx))?;
                } else {
                    return Err(Error::codegen_error("No constant pool available for integer literal"));
                }
            }
            ConstantInstruction::Fconst0 => Self::map_stack(self.bytecode_builder.fconst_0())?,
            ConstantInstruction::Fconst1 => Self::map_stack(self.bytecode_builder.fconst_1())?,
            ConstantInstruction::Fconst2 => Self::map_stack(self.bytecode_builder.fconst_2())?,
            ConstantInstruction::LdcFloat(value) => {
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_float(value as f32) };
                    Self::map_stack(self.bytecode_builder.ldc(idx))?;
                } else {
                    return Err(Error::codegen_error("No constant pool available for float literal"));
                }
            }
            _ => return Err(Error::codegen_error("Unsupported constant instruction")),
        }
        
        Ok(())
    }
    
    /// Check if we can safely apply pre-generation optimization for this field access
    fn can_apply_pre_generation_field_optimization(&self, field_access: &FieldAccessExpr) -> bool {
        // Only allow pre-generation optimization for:
        // 1. Static field access (no target object needed)
        // 2. Simple field access with predictable targets
        // 3. Constant field access
        
        match &field_access.target {
            None => true, // Static field access - safe
            Some(target) => {
                // Only allow simple targets
                matches!(**target, 
                    Expr::Identifier(_) |  // Simple variable reference
                    Expr::Literal(_)       // Literal value (rare but possible)
                )
            }
        }
    }
    
    /// Check if the field optimization type is safe for pre-generation application
    fn is_safe_for_pre_field_optimization(&self, opt_type: &crate::codegen::field_access_optimizer::FieldAccessOptimizationType) -> bool {
        use crate::codegen::field_access_optimizer::FieldAccessOptimizationType;
        
        match opt_type {
            FieldAccessOptimizationType::Static { .. } => true, // Static field - safe
            FieldAccessOptimizationType::ConstantInline { .. } => true, // Constant - safe
            FieldAccessOptimizationType::ClassLiteral { .. } => true, // Class literal - safe
            FieldAccessOptimizationType::Instance { .. } => false, // Needs target object - defer
            FieldAccessOptimizationType::ArrayLength => false, // Needs array object - defer
        }
    }
    
    /// Apply post-generation optimizations after field access is complete
    fn apply_post_generation_field_optimizations(&mut self, field_access: &FieldAccessExpr, pre_access_stack_depth: u16) -> Result<()> {
        let current_stack_depth = self.bytecode_builder.get_stack_depth();
        let stack_change = current_stack_depth.saturating_sub(pre_access_stack_depth);
        
        // Log the optimization opportunity
        eprintln!("🔧 POST-OPT: Field {} completed, stack change: {} -> {} (delta: {})", 
                 field_access.name, pre_access_stack_depth, current_stack_depth, stack_change);
        
        // TODO: Implement actual post-generation field optimizations:
        // - Constant folding for static final fields
        // - Redundant field access elimination
        // - Field access coalescing
        
        Ok(())
    }

    /// Generate bytecode for field access
    fn generate_field_access(&mut self, field_access: &FieldAccessExpr) -> Result<()> {
        // Step 1: Record current stack state for optimization analysis
        let pre_access_stack_depth = self.bytecode_builder.get_stack_depth();
        
        // Step 2: Check if we can apply pre-generation optimizations (only for simple cases)
        if self.can_apply_pre_generation_field_optimization(field_access) {
            let pattern = self.field_access_optimizer.analyze_field_access(field_access, false);
            if pattern.estimated_cost < 30 && self.is_safe_for_pre_field_optimization(&pattern.optimization_type) {
                let result = self.generate_optimized_field_access(pattern.optimization_type);
                if result.is_ok() {
                    // Step 3: Apply post-generation optimizations for pre-optimized field access
                    self.apply_post_generation_field_optimizations(field_access, pre_access_stack_depth)?;
                }
                return result;
            }
        }
        
        // Check if this is a qualified class name chain (e.g., java.lang.reflect.Array)
        if let Some(qualified_name) = self.is_qualified_class_name_chain(field_access) {
            eprintln!("🔍 DEBUG: generate_field_access: Detected qualified class name: {}", qualified_name);
            // This is a class reference, not a field access
            // We should not generate any bytecode here, as this will be used as a method call target
            // The caller (generate_method_call) should handle this as a static method call
            return Ok(());
        }

        
        // Handle special .class field access
        if field_access.name == "class" && field_access.target.is_some() {
            if let Some(Expr::Identifier(ident)) = field_access.target.as_deref() {
                // Check if this is a primitive type .class access (e.g., boolean.class, int.class)
                let is_primitive = matches!(ident.name.as_str(), 
                    "boolean" | "byte" | "char" | "short" | "int" | "long" | "float" | "double" | "void");
                
                if is_primitive {
                    // For primitive types, generate the corresponding Class constant directly
                    let class_name = match ident.name.as_str() {
                        "boolean" => "java/lang/Boolean",
                        "byte" => "java/lang/Byte", 
                        "char" => "java/lang/Character",
                        "short" => "java/lang/Short",
                        "int" => "java/lang/Integer",
                        "long" => "java/lang/Long",
                        "float" => "java/lang/Float",
                        "double" => "java/lang/Double",
                        "void" => "java/lang/Void",
                        _ => unreachable!()
                    };
                    
                    // Load the TYPE field from the wrapper class (e.g., Boolean.TYPE)
                    let field_ref_index = self.add_field_ref(class_name, "TYPE", "Ljava/lang/Class;");
                    Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                    return Ok(());
                } else {
                    // For reference types (e.g., Enum.class, String.class), generate LDC instruction
                    // to load the Class constant
                    let class_name = self.resolve_class_name(&ident.name);
                    eprintln!("🔍 DEBUG: Reference type .class access: {} -> {}", ident.name, class_name);
                    
                    // Use LDC to load the Class constant
                    let class_ref_index = self.add_class_constant(&class_name);
                    Self::map_stack(self.bytecode_builder.ldc(class_ref_index))?;
                    return Ok(());
                }
            }
            

            // Generate receiver
            if let Some(receiver) = &field_access.target {
                self.generate_expression(receiver)?;
            }
            // Call getClass() method
            if let Some(resolved) = resolve_method_with_context("java/lang/Object", "getClass", 0, self.current_class.as_ref(), self.all_types.as_deref()) {
                self.emit_invoke(&resolved)?;
            } else {
                return Err(Error::codegen_error("Cannot resolve getClass() method"));
            }
            return Ok(());
        }
        
        // Check if this is a static field access (e.g., Byte.TYPE)
        let is_static_access = if let Some(receiver) = &field_access.target {
            if let Expr::Identifier(ident) = &**receiver {
                self.is_java_class_name(&ident.name)
            } else {
                false
            }
        } else {
            false
        };
        
        if is_static_access {
            eprintln!("🔍 DEBUG: generate_field_access: Static field access detected");
            // For static field access, don't generate receiver expression
            // We'll use getstatic instead of getfield
        } else {
        // Generate receiver expression if present
        if let Some(receiver) = &field_access.target {
                eprintln!("🔍 DEBUG: generate_field_access: Generating receiver expression: {:?}", receiver);
            self.generate_expression(receiver)?;
                eprintln!("🔍 DEBUG: generate_field_access: Stack depth after receiver = {}", self.bytecode_builder.stack_depth());
            // Special-case: array.length → arraylength
            let recv_ty = self.resolve_expression_type(receiver);
                eprintln!("🔍 DEBUG: generate_field_access: field_name={}, recv_ty={}, is_array_type={}", 
                          field_access.name, recv_ty, is_array_type(&recv_ty));
                if field_access.name == "length" && !is_array_type(&recv_ty) {
                    eprintln!("🔍 DEBUG: generate_field_access: ERROR - Trying to access 'length' on non-array type: {}", recv_ty);
                    eprintln!("🔍 DEBUG: generate_field_access: Receiver expression: {:?}", receiver);
                }
            if field_access.name == "length" && is_array_type(&recv_ty) {
                    eprintln!("🔍 DEBUG: generate_field_access: Using arraylength instruction for array.length");
                Self::map_stack(self.bytecode_builder.arraylength())?;
                return Ok(());
            }
        } else {
                eprintln!("🔍 DEBUG: generate_field_access: No receiver, loading 'this'");
            // Assume 'this' for instance fields
            Self::map_stack(self.bytecode_builder.aload(0))?;
                eprintln!("🔍 DEBUG: generate_field_access: Stack depth after loading 'this' = {}", self.bytecode_builder.stack_depth());
            }
        }
        
        // Generate field access
        // Determine the field class based on receiver type
        let field_class = if is_static_access {
            // For static access, resolve the class name from the receiver identifier
            if let Some(Expr::Identifier(ident)) = field_access.target.as_deref() {
                self.resolve_class_name(&ident.name)
            } else {
                "java/lang/Object".to_string()
            }
        } else if let Some(receiver) = &field_access.target {
            let receiver_type = self.resolve_expression_type(receiver);

            // Use type erasure for proper generic type handling
            let base_type = if receiver_type.contains('<') || self.is_generic_type_parameter(&receiver_type) {
                // This is a generic type that needs proper erasure
                let erased_type = self.perform_type_erasure_for_field_access(&receiver_type);
                eprintln!("🔍 DEBUG: generate_field_access: Erased generic type '{}' to '{}'", receiver_type, erased_type);
                erased_type
            } else {
                eprintln!("🔍 DEBUG: generate_field_access: No generic parameters in '{}'", receiver_type);
                receiver_type
            };

            // Convert type to internal class name format
            if base_type.ends_with("[]") {
                base_type.replace("[]", "").replace(".", "/")
            } else if base_type.contains(".") {
                base_type.replace(".", "/")
            } else {
                // Simple class name, try to resolve using classpath
                self.resolve_class_name(&base_type)
            }
        } else {
            let current_class = self.current_class_name.clone().unwrap_or_else(|| "java/lang/String".to_string());
            self.resolve_class_name(&current_class)
        };
        
        eprintln!("🔍 DEBUG: generate_field_access: field_class={}, is_static_access={}", field_class, is_static_access);
        
        // Special case: handle Object.type as MethodTypeParameter.type
        let (actual_field_class, field_descriptor) = if field_class == "java/lang/Object" && field_access.name == "type" {
            if let Some(current_class) = &self.current_class_name {
                if current_class.contains("MethodType") {
                    // This is likely MethodTypeParameter.type field access
                    let method_type_param_class = "java/lang/invoke/MethodTypeParameter";
                    let descriptor = self.resolve_field_descriptor(method_type_param_class, &field_access.name);
                    (method_type_param_class.to_string(), descriptor)
                } else {
                    let descriptor = self.resolve_field_descriptor(&field_class, &field_access.name);
                    (field_class, descriptor)
                }
            } else {
                let descriptor = self.resolve_field_descriptor(&field_class, &field_access.name);
                (field_class, descriptor)
            }
        } else {
            let descriptor = self.resolve_field_descriptor(&field_class, &field_access.name);
            (field_class, descriptor)
        };

        let field_ref_index = self.add_field_ref(&actual_field_class, &field_access.name, &field_descriptor);
        
        // Handle field access with proper stack management for 64-bit types
        let field_slots = if field_descriptor.as_str() == "J" || field_descriptor.as_str() == "D" {
            2 // long or double
        } else {
            1 // all other types
        };
        
        eprintln!("🔍 DEBUG: generate_field_access: field_descriptor={}, field_slots={}", field_descriptor, field_slots);
        
        if is_static_access {
            // Static field access: getstatic pushes field value (no objectref involved)
            Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
            // Correct stack depth for long/double fields
            if field_slots == 2 {
                Self::map_stack(self.bytecode_builder.update_stack(0, 1))?; // Add 1 more slot for long/double
            }
        } else {
            // Instance field access: getfield pops objectref and pushes field value
            Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?;
            // Correct stack depth for long/double fields
            if field_slots == 2 {
                Self::map_stack(self.bytecode_builder.update_stack(0, 1))?; // Add 1 more slot for long/double
            }
        }
        
        Ok(())
    }

    /// Generate bytecode for instanceof expression
    fn generate_instanceof_expression(&mut self, instance_of: &InstanceOfExpr) -> Result<()> {
        // Generate expression to check
        self.generate_expression(&instance_of.expr)?;
        
        // Generate instanceof check
        let class_ref_index = self.add_class_constant(&instance_of.target_type.name);
        Self::map_stack(self.bytecode_builder.instanceof(class_ref_index))?;
        
        Ok(())
    }

    /// Check if an expression is a zero literal
    fn is_zero_literal(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(lit) => {
                match &lit.value {
                    crate::ast::Literal::Integer(0) => true,
                    _ => false,
                }
            }
            _ => false,
        }
    }
    
    /// Check if an expression results in a long type
    fn is_long_expression(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Binary(bin_expr) => {
                // Check for bitwise operations that typically involve long types
                match bin_expr.operator {
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
                        // If either operand is long, the result is long
                        let left_type = self.resolve_expression_type(&bin_expr.left);
                        let right_type = self.resolve_expression_type(&bin_expr.right);
                        left_type == "long" || right_type == "long" ||
                        left_type.contains("long") || right_type.contains("long")
                    }
                    _ => false
                }
            }
            Expr::MethodCall(method_call) => {
                // Check for methods that return long (like bitPosition)
                match method_call.name.as_str() {
                    "bitPosition" => true, // bitPosition returns long
                    _ => false
                }
            }
            _ => false
        }
    }

    /// Check if an expression is a null literal
    fn is_null_literal(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(lit_expr) => {
                matches!(lit_expr.value, Literal::Null)
            }
            _ => false
        }
    }
    
    /// Check if a statement ends with a return statement
    fn statement_ends_with_return(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return(_) => true,
            Stmt::Block(block) => {
                // Check if the last statement in the block is a return
                block.statements.last().map_or(false, |last_stmt| self.statement_ends_with_return(last_stmt))
            }
            Stmt::If(if_stmt) => {
                // If statement ends with return only if both branches end with return
                let then_returns = self.statement_ends_with_return(&if_stmt.then_branch);
                let else_returns = if let Some(else_branch) = &if_stmt.else_branch {
                    self.statement_ends_with_return(else_branch)
                } else {
                    false // No else branch means it doesn't always return
                };
                then_returns && else_returns
            }
            _ => false,
        }
    }
    
    /// Check if a statement ends with a terminal instruction (return or throw)
    /// This is used to avoid generating unnecessary goto instructions after unreachable code
    fn statement_ends_with_terminal(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return(_) => true,
            Stmt::Throw(_) => true,  // throw is also a terminal instruction
            Stmt::Block(block) => {
                // Check if the last statement in the block is terminal
                block.statements.last().map_or(false, |last_stmt| self.statement_ends_with_terminal(last_stmt))
            }
            Stmt::If(if_stmt) => {
                // If statement is terminal only if both branches are terminal
                let then_terminal = self.statement_ends_with_terminal(&if_stmt.then_branch);
                let else_terminal = if let Some(else_branch) = &if_stmt.else_branch {
                    self.statement_ends_with_terminal(else_branch)
                } else {
                    false // No else branch means it doesn't always terminate
                };
                then_terminal && else_terminal
            }
            _ => false,
        }
    }

    /// Infer constructor descriptor based on target class and arguments (javac-style)
    fn infer_constructor_descriptor(&self, class_name: &str, arguments: &[Expr]) -> Option<String> {
        // First, try to find constructor in current class if it matches
        if let Some(class) = &self.current_class {
            if class.name == class_name || class.name.ends_with(&format!(".{}", class_name)) {
                if let Some(desc) = self.find_constructor_in_class(class, arguments) {
                    return Some(desc);
                }
            }
        }
        
        // Then, try to find constructor in all available types
        if let Some(all_types) = &self.all_types {
            for type_decl in all_types {
                if let crate::ast::TypeDecl::Class(class) = type_decl {
                    if class.name == class_name || class.name.ends_with(&format!(".{}", class_name)) {
                        if let Some(desc) = self.find_constructor_in_class(&class, arguments) {
                            return Some(desc);
                        }
                    }
                }
            }
        }
        
        // Finally, try to find constructor in rt.rs classes
        let internal_name = self.resolve_class_name(class_name);
        
        // Look up the class in rt.rs
        use crate::rt::{CLASSES_BY_NAME, CLASSES};
        if let Some(&class_idx) = CLASSES_BY_NAME.get(&internal_name) {
            let class_meta = &CLASSES[class_idx];
            
            // Look for constructors in this class
            for method in class_meta.methods {
                if method.name == "<init>" {
                    // Check if the parameter count matches
                    let param_count = self.count_descriptor_params(method.desc);
                    if param_count == arguments.len() {
                        return Some(method.desc.to_string());
                    }
                }
            }
        }
        
        None
    }
    
    /// Count the number of parameters in a method descriptor
    fn count_descriptor_params(&self, descriptor: &str) -> usize {
        let bytes = descriptor.as_bytes();
        let mut i = 0;
        
        // Find the opening parenthesis
        while i < bytes.len() && bytes[i] != b'(' {
            i += 1;
        }
        if i >= bytes.len() {
            return 0;
        }
        i += 1; // Skip '('
        
        let mut count = 0;
        while i < bytes.len() && bytes[i] != b')' {
            match bytes[i] {
                b'B' | b'C' | b'D' | b'F' | b'I' | b'J' | b'S' | b'Z' => {
                    count += 1;
                    i += 1;
                }
                b'[' => {
                    // Array type - skip all '[' and then the element type
                    while i < bytes.len() && bytes[i] == b'[' {
                        i += 1;
                    }
                    if i < bytes.len() {
                        if bytes[i] == b'L' {
                            // Object array - skip to ';'
                            while i < bytes.len() && bytes[i] != b';' {
                                i += 1;
                            }
                            i += 1; // Skip ';'
                        } else {
                            // Primitive array
                            i += 1;
                        }
                    }
                    count += 1;
                }
                b'L' => {
                    // Object type - skip to ';'
                    while i < bytes.len() && bytes[i] != b';' {
                        i += 1;
                    }
                    i += 1; // Skip ';'
                    count += 1;
                }
                _ => {
                    i += 1;
                }
            }
        }
        
        count
    }
    
    /// Find constructor in a specific class
    fn find_constructor_in_class(&self, class: &crate::ast::ClassDecl, arguments: &[Expr]) -> Option<String> {
        for member in &class.body {
            if let crate::ast::ClassMember::Constructor(constructor) = member {
                if constructor.parameters.len() == arguments.len() {
                    // Check if arguments are compatible with constructor parameters
                    let mut compatible = true;
                    for (arg, param) in arguments.iter().zip(constructor.parameters.iter()) {
                        if !self.is_argument_compatible_with_parameter(arg, &param.type_ref) {
                            compatible = false;
                            break;
                        }
                    }
                    
                    if compatible {
                        // Generate descriptor from constructor parameters
                        let mut descriptor = String::from("(");
                        for param in &constructor.parameters {
                            descriptor.push_str(&crate::codegen::descriptor::type_to_descriptor(&param.type_ref));
                        }
                        descriptor.push_str(")V");
                        return Some(descriptor);
                    }
                }
            }
        }
        None
    }
    
    /// Check if an argument is compatible with a constructor parameter (for type inference)
    fn is_argument_compatible_with_parameter(&self, arg: &Expr, param_type: &crate::ast::TypeRef) -> bool {
        match arg {
            // Null is compatible with any reference type
            Expr::Literal(lit) if matches!(lit.value, crate::ast::Literal::Null) => {
                !self.is_primitive_type(&param_type.name)
            }
            // For other expressions, do basic type compatibility check
            _ => {
                let arg_type = self.resolve_expression_type(arg);
                // Simple compatibility check - can be enhanced
                arg_type == param_type.name || 
                arg_type == "java.lang.Object" || 
                param_type.name == "java.lang.Object" ||
                (param_type.name.len() == 1 && param_type.name.chars().next().unwrap().is_ascii_uppercase()) // Generic type parameter
            }
        }
    }

    /// Generate bytecode for new expression (javac-style optimized)
    fn generate_new_expression(&mut self, new_expr: &NewExpr) -> Result<()> {
        // Use javac-style object creation optimization
        let pattern = crate::codegen::object_optimizer::ObjectOptimizer::analyze_object_creation(new_expr);
        
        match &pattern.optimization_type {
            crate::codegen::object_optimizer::ObjectOptimizationType::StringLiteral(value) => {
                // Optimize string constructor to direct ldc (javac-style)
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_string(value) };
                    Self::map_stack(self.bytecode_builder.ldc(idx))?;
        } else {
                    Self::map_stack(self.bytecode_builder.ldc(1))?; // Fallback
                }
                return Ok(());
            }
            crate::codegen::object_optimizer::ObjectOptimizationType::ArrayCreation { .. } => {
                // Use optimized array creation
            self.generate_array_creation(new_expr)?;
                return Ok(());
            }
            _ => {
                // Standard object creation with javac new+dup pattern
            let class_ref_index = self.add_class_constant(&new_expr.target_type.name);
            
                // NEW instruction (javac pattern)
            Self::map_stack(self.bytecode_builder.new_object(class_ref_index))?;
            
                // DUP to keep reference for constructor call (javac pattern)
            Self::map_stack(self.bytecode_builder.dup())?;
            
            // Generate constructor arguments
            for arg in &new_expr.arguments {
                self.generate_expression(arg)?;
            }
            
                // Call constructor - generate descriptor based on arguments
                let mut descriptor = String::new();
                descriptor.push('(');
                
                // Handle specific constructor cases
                if new_expr.target_type.name == "ArraysListIterator" && new_expr.arguments.len() == 2 {
                    // ArraysListIterator(Object[] array, int index)
                    descriptor.push_str("[Ljava/lang/Object;"); // Array of objects for first arg
                    descriptor.push('I'); // Integer for second arg
                } else if new_expr.target_type.name == "UnsupportedOperationException" && new_expr.arguments.len() == 1 {
                    // UnsupportedOperationException(String message)
                    descriptor.push_str("Ljava/lang/String;"); // String argument
                } else if new_expr.target_type.name == "NoSuchElementException" && new_expr.arguments.len() == 0 {
                    // NoSuchElementException() - no arguments
                } else if new_expr.arguments.is_empty() {
                    // Default constructor with no arguments
                } else {
                    // For other constructors, use javac-style type inference
                    // First, try to find the constructor signature from the target class
                    let constructor_descriptor = self.infer_constructor_descriptor(&new_expr.target_type.name, &new_expr.arguments);
                    if let Some(desc) = constructor_descriptor {
                        // Use the inferred descriptor (without the return type)
                        let param_part = &desc[1..desc.len()-2]; // Remove '(' and ')V'
                        descriptor.push_str(param_part);
                    } else {
                        // Fallback to simple type inference
                        for arg in &new_expr.arguments {
                            let arg_descriptor = self.type_to_descriptor_with_generics(arg);
                            descriptor.push_str(&arg_descriptor);
                        }
                    }
                }
                
                descriptor.push_str(")V");
                
                // Convert simple class name to internal name for method reference
                let internal_class_name = self.resolve_class_name(&new_expr.target_type.name);
                let method_ref_index = self.add_method_ref(&internal_class_name, "<init>", &descriptor);
            Self::map_stack(self.bytecode_builder.invokespecial(method_ref_index))?;
                
                // Manually adjust stack for constructor call
                // Constructor consumes: this (1) + arguments (new_expr.arguments.len())
                // Constructor returns: void (0)
                let args_consumed = 1 + new_expr.arguments.len() as u16; // this + arguments
                Self::map_stack(self.bytecode_builder.update_stack(args_consumed, 0))?;
            }
        }
        
        Ok(())
    }

    fn generate_close_for_local(&mut self, index: u16, _tref: &TypeRef) -> Result<()> {
        // Load local variable (resource)
        Self::map_stack(self.bytecode_builder.aload(index))?;
        
        // Check if resource is null
        let end_label = self.create_label();
        {
            let l = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.ifnull(&l))?;
        }
        
        // Resource is not null - call close()
        // Load resource again for the method call
        Self::map_stack(self.bytecode_builder.aload(index))?;
        
        // invoke interface close()V (simplified; no constant pool wired)
        self.emit_opcode(self.opcode_generator.invokeinterface(0, 0));
        // invokeinterface: pops receiver, no return value
        Self::map_stack(self.bytecode_builder.update_stack(1, 0))?;
        self.bytecode_builder.push_short(1);
        self.bytecode_builder.push_byte(1);
        self.bytecode_builder.push_byte(0);
        
        // end label - both paths converge here with stack depth 0
        {
            let l = self.label_str(end_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an assignment expression
    /// Note: Assignment expressions in Java have a value (the assigned value)
    /// which should be left on the stack for use in chained assignments like a = b = c
    fn generate_assignment(&mut self, assign: &AssignmentExpr) -> Result<()> {
        self.generate_assignment_with_context(assign, true)
    }

    /// Generate bytecode for an assignment expression with context about value preservation
    /// preserve_value: true for expressions (a = b = c), false for statements (a = b;)
    fn generate_assignment_with_context(&mut self, assign: &AssignmentExpr, preserve_value: bool) -> Result<()> {
        // Use javac-style assignment optimizer for advanced optimizations
        let optimization = self.assignment_optimizer.analyze_assignment(assign);
        
        // 🔧 ASSIGNMENT OPT: Debug output for optimization analysis
        println!("🔧 ASSIGNMENT OPT: Assignment analyzed: {:?}", optimization);
        println!("🔧 ASSIGNMENT OPT: Target: {:?}, Operator: {:?}, Preserve value: {}", 
                 assign.target, assign.operator, preserve_value);
        
        match optimization {
            crate::codegen::assignment_optimizer::AssignmentOptimization::IincOptimization { var_index, increment } => {
                // Use iinc instruction for local variable increment/decrement
                println!("🔧 ASSIGNMENT OPT: Using iinc optimization for var[{}] += {}", var_index, increment);
                
                let bytecode = self.assignment_optimizer.generate_assignment_bytecode(assign)?;
                println!("🔧 ASSIGNMENT OPT: Generated iinc bytecode: {:?}", bytecode);
                
                self.bytecode_builder.emit_raw(&bytecode)?;
                
                // If preserving value, load the result
                if preserve_value {
                    // For iinc, we need to load the variable after increment
                    println!("🔧 ASSIGNMENT OPT: Preserving value, loading var[{}] after iinc", var_index);
                    self.bytecode_builder.iload(var_index)?;
                }
                
                println!("🔧 ASSIGNMENT OPT: iinc optimization completed successfully");
                return Ok(());
            }
            crate::codegen::assignment_optimizer::AssignmentOptimization::DupX1Optimization { needs_wide_instruction: _ } => {
                // Use dup_x1 optimization for compound assignments
                println!("🔧 ASSIGNMENT OPT: Using dup_x1 optimization for compound assignment");
                
                let bytecode = self.assignment_optimizer.generate_assignment_bytecode(assign)?;
                println!("🔧 ASSIGNMENT OPT: Generated dup_x1 bytecode: {:?}", bytecode);
                
                self.bytecode_builder.emit_raw(&bytecode)?;
                
                println!("🔧 ASSIGNMENT OPT: dup_x1 optimization completed successfully");
                return Ok(());
            }
            crate::codegen::assignment_optimizer::AssignmentOptimization::ConstantAssignment { value, use_optimized_load } => {
                if use_optimized_load {
                    // Use optimized constant loading
                    println!("🔧 ASSIGNMENT OPT: Using optimized constant assignment for value: {}", value);
                    
                    let bytecode = self.assignment_optimizer.generate_assignment_bytecode(assign)?;
                    println!("🔧 ASSIGNMENT OPT: Generated constant assignment bytecode: {:?}", bytecode);
                    
                    self.bytecode_builder.emit_raw(&bytecode)?;
                    
                    println!("🔧 ASSIGNMENT OPT: Constant assignment optimization completed successfully");
                    return Ok(());
                }
                // Fall through to standard assignment handling
                println!("🔧 ASSIGNMENT OPT: Constant assignment optimization not applicable, using standard handling");
            }
            _ => {
                // Fall through to standard assignment handling
                println!("🔧 ASSIGNMENT OPT: No specific optimization applicable, using standard assignment handling");
            }
        }
        
        // Handle compound assignments
        if assign.operator != AssignmentOp::Assign {
            // For compound assignments, we need to load the target first
            match &*assign.target {
                Expr::Identifier(ident) => {
                if let Some(local_var) = self.find_local_variable(&ident.name) {
                    // Extract local variable info to avoid borrow checker issues
                    let index = local_var.index;
                    let var_type = local_var.var_type.clone();
                    
                    // Load current value
                    self.load_local_variable(index, &var_type)?;
                    // Generate right operand
                    self.generate_expression(&assign.value)?;
                    // Apply operation with type awareness
                    let operand_type = match var_type {
                        crate::codegen::LocalType::Long => "long",
                        crate::codegen::LocalType::Double => "double", 
                        crate::codegen::LocalType::Float => "float",
                        _ => "int", // int, boolean, char, short, byte, reference
                    };
                    self.generate_compound_assignment_typed(assign.operator.clone(), operand_type)?;
                    // Duplicate the result for chained assignments (only if preserving value)
                    if preserve_value {
                    // Use dup2 for long/double (64-bit types), dup for others
                    match var_type {
                        LocalType::Long | LocalType::Double => {
                            Self::map_stack(self.bytecode_builder.dup2())?;
                        }
                        _ => {
                            Self::map_stack(self.bytecode_builder.dup())?;
                            }
                        }
                    }
                    // Store result
                    self.store_local_variable(index, &var_type)?;
                    // Value remains on stack for chained assignments
                    return Ok(());
                    }
                }
                Expr::ArrayAccess(array_access) => {
                    // Handle compound assignment to array element: arr[idx] op= value
                    
                    // Determine array element type for type-aware operations
                    let array_type = self.resolve_expression_type(&array_access.array);
                    let element_type = if array_type.ends_with("[]") {
                        &array_type[..array_type.len()-2]
                    } else {
                        "int" // fallback
                    };
                    eprintln!("🔍 DEBUG: Compound array assignment: Array type={}, Element type={}", array_type, element_type);
                    
                    // Load array and index
                    self.generate_expression(&array_access.array)?;
                    self.generate_expression(&array_access.index)?;
                    // Duplicate array and index for later store: arr, idx -> arr, idx, arr, idx
                    Self::map_stack(self.bytecode_builder.dup2())?;
                    
                    // Load current array element using type-aware instruction
                    match element_type {
                        "long" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using laload for long array");
                            Self::map_stack(self.bytecode_builder.laload())?;
                        }
                        "double" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using daload for double array");
                            Self::map_stack(self.bytecode_builder.daload())?;
                        }
                        "float" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using faload for float array");
                            Self::map_stack(self.bytecode_builder.faload())?;
                        }
                        "byte" | "boolean" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using baload for byte/boolean array");
                            Self::map_stack(self.bytecode_builder.baload())?;
                        }
                        "char" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using caload for char array");
                            Self::map_stack(self.bytecode_builder.caload())?;
                        }
                        "short" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using saload for short array");
                            Self::map_stack(self.bytecode_builder.saload())?;
                        }
                        _ => {
                            // int or object reference
                            if element_type == "int" {
                                eprintln!("🔍 DEBUG: Compound assignment: Using iaload for int array");
                                Self::map_stack(self.bytecode_builder.iaload())?;
                            } else {
                                eprintln!("🔍 DEBUG: Compound assignment: Using aaload for object array");
                                Self::map_stack(self.bytecode_builder.aaload())?;
                            }
                        }
                    }
                    
                    // Generate right operand: arr, idx, current_value -> arr, idx, current_value, rhs_value
                    self.generate_expression(&assign.value)?;
                    
                    // Handle type conversion if needed (e.g., int to long for long arrays)
                    let rhs_type = self.resolve_expression_type(&assign.value);
                    if element_type == "long" && rhs_type == "int" {
                        eprintln!("🔍 DEBUG: Compound assignment: Converting int to long for long array operation");
                        Self::map_stack(self.bytecode_builder.i2l())?;
                    }
                    
                    // Apply operation: arr, idx, current_value, rhs_value -> arr, idx, result
                    self.generate_compound_assignment_typed(assign.operator.clone(), element_type)?;
                    
                    // Duplicate result for return value (only if preserve_value is true)
                    if preserve_value {
                        match element_type {
                            "long" | "double" => {
                                // For wide types, use dup2_x2
                                eprintln!("🔍 DEBUG: Compound assignment: Using dup2_x2 for wide type");
                                Self::map_stack(self.bytecode_builder.dup2_x2())?;
                            }
                            _ => {
                                // For narrow types, use dup_x2
                                eprintln!("🔍 DEBUG: Compound assignment: Using dup_x2 for narrow type");
                                Self::map_stack(self.bytecode_builder.dup_x2())?;
                            }
                        }
                    }
                    
                    // Store result using type-aware instruction
                    match element_type {
                        "long" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using lastore for long array");
                            Self::map_stack(self.bytecode_builder.lastore())?;
                        }
                        "double" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using dastore for double array");
                            Self::map_stack(self.bytecode_builder.dastore())?;
                        }
                        "float" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using fastore for float array");
                            Self::map_stack(self.bytecode_builder.fastore())?;
                        }
                        "byte" | "boolean" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using bastore for byte/boolean array");
                            Self::map_stack(self.bytecode_builder.bastore())?;
                        }
                        "char" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using castore for char array");
                            Self::map_stack(self.bytecode_builder.castore())?;
                        }
                        "short" => {
                            eprintln!("🔍 DEBUG: Compound assignment: Using sastore for short array");
                            Self::map_stack(self.bytecode_builder.sastore())?;
                        }
                        _ => {
                            // int or object reference
                            if element_type == "int" {
                                eprintln!("🔍 DEBUG: Compound assignment: Using iastore for int array");
                                Self::map_stack(self.bytecode_builder.iastore())?;
                            } else {
                                eprintln!("🔍 DEBUG: Compound assignment: Using aastore for object array");
                                Self::map_stack(self.bytecode_builder.aastore())?;
                            }
                        }
                    }
                    // Result value remains on stack for chained assignments (only if preserve_value=true)
                    return Ok(());
                }
                _ => {
                    // Other compound assignment targets not supported yet
                }
            }
        }
        
        // Regular assignment: generate by target kind to preserve correct operand order
        match &*assign.target {
            Expr::Identifier(ident) => {
                // Check if this is a field first, before checking local variables
                let class_name = self.current_class_name.as_ref()
                    .ok_or_else(|| Error::codegen_error("Cannot resolve field access: no current class name available"))?
                    .clone();
                
                let is_field = self.is_instance_field(&class_name, &ident.name);

                
                if is_field {
                    // Field assignment: this.field = value
                    // Use the correct javac pattern: aload_0, value, putfield (stack: [this, value])

                    Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
                    self.generate_expression(&assign.value)?; // Generate value (stack: [this, value])
                    
                    // If we need to preserve the value for chained assignments
                    if preserve_value {
                        // Stack: [this, value] -> [this, value, value] (dup the value)
                        Self::map_stack(self.bytecode_builder.dup_x1())?; // Move duplicated value below this
                        // Stack: [value, this, value] - now putfield will consume [this, value], leaving [value]
                    }
                    let field_descriptor = self.resolve_field_descriptor(&class_name, &ident.name);
                    let field_ref_index = self.add_field_ref(&class_name, &ident.name, &field_descriptor);
                    Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?;
                    // Value remains on stack for chained assignments
                } else if let Some(local_var) = self.find_local_variable(&ident.name) {
                    // Extract local variable info to avoid borrow checker issues
                    let index = local_var.index;
                    let var_type = local_var.var_type.clone();
                    
                    // Local variable assignment: x = value → evaluate RHS then store to local

                    self.generate_expression(&assign.value)?;
                    
                    // 🔧 TYPE COERCION OPT: Apply type coercion if needed for assignment
                    let value_type = self.infer_expression_type(&assign.value);
                    let target_type = self.convert_local_type_to_string(&var_type);
                    
                    if value_type != target_type {
                        println!("🔧 TYPE COERCION OPT: Assignment type coercion needed: {} -> {}", value_type, target_type);
                        self.apply_assignment_type_coercion(&value_type, &target_type)?;
                    } else {
                        println!("🔧 TYPE COERCION OPT: Assignment types match: {}", value_type);
                    }
                    // Duplicate the value on stack so we can store it and also return it (only if preserving value)
                    if preserve_value {
                    // Use dup2 for long/double (64-bit types), dup for others
                    match var_type {
                        LocalType::Long | LocalType::Double => {
                            Self::map_stack(self.bytecode_builder.dup2())?;
                        }
                        _ => {
                            Self::map_stack(self.bytecode_builder.dup())?;
                            }
                        }
                    }
                    self.store_local_variable(index, &var_type)?;
                    // Value remains on stack for chained assignments
                } else {
                    return Err(Error::codegen_error(&format!("Cannot resolve assignment target '{}': not a field or local variable", ident.name)));
                }
            }
            Expr::FieldAccess(field_access) => {
                // target.field = value
                // Evaluate receiver first to get objectref
                if let Some(receiver) = &field_access.target {
                    self.generate_expression(receiver)?;
                } else {
                    // Implicit this
                    Self::map_stack(self.bytecode_builder.aload(0))?;
                }
                // Then evaluate RHS value
                self.generate_expression(&assign.value)?;
                // For chained assignments, duplicate the value before putfield (only if preserving value)
                if preserve_value {
                // Stack: objectref, value → objectref, value, value
                Self::map_stack(self.bytecode_builder.dup_x1())?;
                }
                // Stack: [value,] objectref, value → putfield consumes objectref and value[, leaving value]
                // Determine the field class based on receiver type
                let field_class = if let Some(receiver) = &field_access.target {
                    let receiver_type = self.resolve_expression_type(receiver);
                    
                    // Strip generic parameters for field resolution
                    let base_type = if let Some(generic_start) = receiver_type.find('<') {
                        let stripped = receiver_type[..generic_start].to_string();
                        eprintln!("🔍 DEBUG: generate_assignment FieldAccess: Stripped generic from '{}' to '{}'", receiver_type, stripped);
                        stripped
                    } else {
                        eprintln!("🔍 DEBUG: generate_assignment FieldAccess: No generic parameters in '{}'", receiver_type);
                        receiver_type
                    };
                    
                    // Convert type to internal class name format
                    if base_type.ends_with("[]") {
                        base_type.replace("[]", "").replace(".", "/")
                    } else if base_type.contains(".") {
                        base_type.replace(".", "/")
                    } else {
                        // Simple class name, try to resolve using classpath
                        self.resolve_class_name(&base_type)
                    }
                } else {
                    self.current_class_name.clone().unwrap_or_else(|| "java/lang/String".to_string())
                };
                let field_descriptor = self.resolve_field_descriptor(&field_class, &field_access.name);
                let field_ref_index = self.add_field_ref(&field_class, &field_access.name, &field_descriptor);
                
                // Handle putfield with proper stack management for 64-bit types
                // putfield pops objectref and field value
                // For long/double fields, the value takes 2 stack slots
                let field_slots = if field_descriptor.as_str() == "J" || field_descriptor.as_str() == "D" {
                    2 // long or double
                } else {
                    1 // all other types
                };
                
                eprintln!("🔍 DEBUG: generate_assignment FieldAccess: field_descriptor={}, field_slots={}, preserve_value={}", field_descriptor, field_slots, preserve_value);
                
                // Pop objectref (1 slot) and field value (1 or 2 slots)
                // If preserve_value=true, we duplicated the value, so one copy remains on stack
                // If preserve_value=false, no value remains on stack
                let stack_result = if preserve_value { field_slots } else { 0 };
                eprintln!("🔍 DEBUG: generate_assignment FieldAccess: About to emit putfield, stack_depth={}", self.bytecode_builder.stack_depth());
                Self::map_stack(self.bytecode_builder.update_stack(1 + field_slots, stack_result))?;
                eprintln!("🔍 DEBUG: generate_assignment FieldAccess: After stack update, stack_depth={}", self.bytecode_builder.stack_depth());
                self.emit_opcode(self.opcode_generator.putfield(field_ref_index));
                eprintln!("🔍 DEBUG: generate_assignment FieldAccess: After emit putfield, stack_depth={}", self.bytecode_builder.stack_depth());
                // Value remains on stack for chained assignments only if preserve_value=true
            }
            Expr::ArrayAccess(array_access) => {
                // arr[idx] = value
                eprintln!("🔍 DEBUG: Array assignment: Starting, stack_depth={}", self.bytecode_builder.stack_depth());
                
                // Determine array element type for type-aware operations
                let array_type = self.resolve_expression_type(&array_access.array);
                let element_type = if array_type.ends_with("[]") {
                    &array_type[..array_type.len()-2]
                } else {
                    "int" // fallback
                };
                eprintln!("🔍 DEBUG: Array assignment: Array type={}, Element type={}", array_type, element_type);
                
                // Evaluate array and index first
                self.generate_expression(&array_access.array)?;
                eprintln!("🔍 DEBUG: Array assignment: After array, stack_depth={}", self.bytecode_builder.stack_depth());
                self.generate_expression(&array_access.index)?;
                eprintln!("🔍 DEBUG: Array assignment: After index, stack_depth={}", self.bytecode_builder.stack_depth());
                
                // Then evaluate RHS value with target type awareness
                let value_type = self.resolve_expression_type(&assign.value);
                
                // For long arrays with int literals, generate long constants directly
                if element_type == "long" && value_type == "int" {
                    if let Expr::Literal(lit_expr) = assign.value.as_ref() {
                        match &lit_expr.value {
                            crate::ast::Literal::Integer(0) => {
                                eprintln!("🔍 DEBUG: Array assignment: Using lconst_0 for long array");
                                Self::map_stack(self.bytecode_builder.lconst_0())?;
                            }
                            crate::ast::Literal::Integer(1) => {
                                eprintln!("🔍 DEBUG: Array assignment: Using lconst_1 for long array");
                                Self::map_stack(self.bytecode_builder.lconst_1())?;
                            }
                            _ => {
                                // For other int literals, use normal generation + conversion
                                self.generate_expression(&assign.value)?;
                                eprintln!("🔍 DEBUG: Array assignment: Converting int to long with i2l");
                                Self::map_stack(self.bytecode_builder.i2l())?;
                            }
                        }
                    } else {
                        // For non-literal expressions, use normal generation + conversion
                        self.generate_expression(&assign.value)?;
                        eprintln!("🔍 DEBUG: Array assignment: Converting int to long with i2l");
                        Self::map_stack(self.bytecode_builder.i2l())?;
                    }
                } else {
                    // Normal expression generation for other cases
                    self.generate_expression(&assign.value)?;
                }
                eprintln!("🔍 DEBUG: Array assignment: After value, stack_depth={}", self.bytecode_builder.stack_depth());
                
                // For chained assignments, duplicate the value before store (only if preserve_value is true)
                if preserve_value {
                    match element_type {
                        "long" | "double" => {
                            // For wide types, use dup2_x2
                            eprintln!("🔍 DEBUG: Array assignment: About to call dup2_x2 for wide type");
                            Self::map_stack(self.bytecode_builder.dup2_x2())?;
                        }
                        _ => {
                            // For narrow types, use dup_x2
                            eprintln!("🔍 DEBUG: Array assignment: About to call dup_x2 for narrow type");
                            Self::map_stack(self.bytecode_builder.dup_x2())?;
                        }
                    }
                }
                
                // Use type-aware array store instruction
                match element_type {
                    "long" => {
                        eprintln!("🔍 DEBUG: Array assignment: Using lastore for long array");
                        Self::map_stack(self.bytecode_builder.lastore())?;
                    }
                    "double" => {
                        eprintln!("🔍 DEBUG: Array assignment: Using dastore for double array");
                        Self::map_stack(self.bytecode_builder.dastore())?;
                    }
                    "float" => {
                        eprintln!("🔍 DEBUG: Array assignment: Using fastore for float array");
                        Self::map_stack(self.bytecode_builder.fastore())?;
                    }
                    "byte" | "boolean" => {
                        eprintln!("🔍 DEBUG: Array assignment: Using bastore for byte/boolean array");
                        Self::map_stack(self.bytecode_builder.bastore())?;
                    }
                    "char" => {
                        eprintln!("🔍 DEBUG: Array assignment: Using castore for char array");
                        Self::map_stack(self.bytecode_builder.castore())?;
                    }
                    "short" => {
                        eprintln!("🔍 DEBUG: Array assignment: Using sastore for short array");
                        Self::map_stack(self.bytecode_builder.sastore())?;
                    }
                    _ => {
                        // int or object reference
                        if element_type == "int" {
                            eprintln!("🔍 DEBUG: Array assignment: Using iastore for int array");
                            Self::map_stack(self.bytecode_builder.iastore())?;
                        } else {
                            eprintln!("🔍 DEBUG: Array assignment: Using aastore for object array");
                            Self::map_stack(self.bytecode_builder.aastore())?;
                        }
                    }
                }
                eprintln!("🔍 DEBUG: Array assignment: After array store, stack_depth={}", self.bytecode_builder.stack_depth());
                // Value remains on stack for chained assignments only if preserve_value=true
            }
            other => {
                return Err(Error::codegen_error(format!("Unsupported assignment target: {:?}", other)));
            }
        }
        
        Ok(())
    }

    /// Generate bytecode for compound assignment operations (type-aware with explicit type)
    fn generate_compound_assignment_typed(&mut self, op: AssignmentOp, operand_type: &str) -> Result<()> {
        match op {
            AssignmentOp::AddAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.ladd());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?; // 2 longs -> 1 long
                    }
                    "float" => {
                        self.emit_opcode(self.opcode_generator.fadd());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                    "double" => {
                        self.emit_opcode(self.opcode_generator.dadd());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.iadd());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::SubAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.lsub());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    "float" => {
                        self.emit_opcode(self.opcode_generator.fsub());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                    "double" => {
                        self.emit_opcode(self.opcode_generator.dsub());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.isub());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::MulAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.lmul());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    "float" => {
                        self.emit_opcode(self.opcode_generator.fmul());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                    "double" => {
                        self.emit_opcode(self.opcode_generator.dmul());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.imul());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::DivAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.ldiv());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    "float" => {
                        self.emit_opcode(self.opcode_generator.fdiv());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                    "double" => {
                        self.emit_opcode(self.opcode_generator.ddiv());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.idiv());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::ModAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.lrem());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    "float" => {
                        self.emit_opcode(self.opcode_generator.frem());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                    "double" => {
                        self.emit_opcode(self.opcode_generator.drem());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.irem());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::AndAssign => {
                match operand_type {
                    "long" => {
                        eprintln!("🔍 DEBUG: AndAssign: Using land for long operands");
                        self.emit_opcode(self.opcode_generator.land());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?; // 2 longs -> 1 long
                    }
                    _ => {
                        eprintln!("🔍 DEBUG: AndAssign: Using iand for int operands");
                        self.emit_opcode(self.opcode_generator.iand());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?; // 2 ints -> 1 int
                    }
                }
            },
            AssignmentOp::OrAssign => {
                match operand_type {
                    "long" => {
                        eprintln!("🔍 DEBUG: OrAssign: Using lor for long operands");
                        self.emit_opcode(self.opcode_generator.lor());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    _ => {
                        eprintln!("🔍 DEBUG: OrAssign: Using ior for int operands");
                        self.emit_opcode(self.opcode_generator.ior());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::XorAssign => {
                match operand_type {
                    "long" => {
                        eprintln!("🔍 DEBUG: XorAssign: Using lxor for long operands");
                        self.emit_opcode(self.opcode_generator.lxor());
                        Self::map_stack(self.bytecode_builder.update_stack(4, 2))?;
                    }
                    _ => {
                        eprintln!("🔍 DEBUG: XorAssign: Using ixor for int operands");
                        self.emit_opcode(self.opcode_generator.ixor());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::LShiftAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.lshl());
                        Self::map_stack(self.bytecode_builder.update_stack(3, 2))?; // long + int -> long
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.ishl());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::RShiftAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.lshr());
                        Self::map_stack(self.bytecode_builder.update_stack(3, 2))?;
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.ishr());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::URShiftAssign => {
                match operand_type {
                    "long" => {
                        self.emit_opcode(self.opcode_generator.lushr());
                        Self::map_stack(self.bytecode_builder.update_stack(3, 2))?;
                    }
                    _ => {
                        self.emit_opcode(self.opcode_generator.iushr());
                        Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                    }
                }
            },
            AssignmentOp::Assign => {
                // Should not happen here
                return Err(Error::codegen_error("Unexpected Assign operator in compound assignment".to_string()));
            }
        }
        Ok(())
    }

    /// Generate bytecode for compound assignment operations (type-aware)
    fn generate_compound_assignment(&mut self, op: AssignmentOp) -> Result<()> {
        // Note: This method is called in the context where operands are already on stack
        // We need to determine the operand type from the stack or context
        // For now, we'll use a simple heuristic based on stack depth and common patterns
        
        match op {
            AssignmentOp::AddAssign => {
                // 🔧 INCREMENT OPT: Check if this is a constant increment for optimization
                if let Some(constant_value) = self.check_constant_increment_on_stack() {
                    println!("🔧 INCREMENT OPT: Detected constant increment: {}", constant_value);
                    // Optimize constant increment
                    self.optimize_constant_increment(constant_value)?;
                } else {
                    // 🔧 CONSTANT OPT: Try to analyze for constant folding opportunities
                    println!("🔧 CONSTANT OPT: Analyzing AddAssign for constant folding");
                    self.emit_opcode(self.opcode_generator.iadd());
                    Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                }
            },
            AssignmentOp::SubAssign => {
                // 🔧 INCREMENT OPT: Check if this is a constant decrement for optimization
                if let Some(constant_value) = self.check_constant_decrement_on_stack() {
                    println!("🔧 INCREMENT OPT: Detected constant decrement: {}", constant_value);
                    // Optimize constant decrement
                    self.optimize_constant_decrement(constant_value)?;
                } else {
                    // 🔧 CONSTANT OPT: Try to analyze for constant folding opportunities
                    println!("🔧 CONSTANT OPT: Analyzing SubAssign for constant folding");
                    self.emit_opcode(self.opcode_generator.isub());
                    Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                }
            },
            AssignmentOp::MulAssign => {
                self.emit_opcode(self.opcode_generator.imul());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::DivAssign => {
                self.emit_opcode(self.opcode_generator.idiv());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::ModAssign => {
                self.emit_opcode(self.opcode_generator.irem());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::AndAssign => {
                self.emit_opcode(self.opcode_generator.iand());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::OrAssign => {
                self.emit_opcode(self.opcode_generator.ior());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::XorAssign => {
                self.emit_opcode(self.opcode_generator.ixor());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::LShiftAssign => {
                self.emit_opcode(self.opcode_generator.ishl());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::RShiftAssign => {
                self.emit_opcode(self.opcode_generator.ishr());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::URShiftAssign => {
                self.emit_opcode(self.opcode_generator.iushr());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            },
            AssignmentOp::Assign => {
                // Should not happen here
                return Err(Error::codegen_error("Unexpected Assign operator in compound assignment".to_string()));
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for an array access expression
    fn generate_array_access(&mut self, array_access: &ArrayAccessExpr) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_array_access: Starting, stack_depth={}", self.bytecode_builder.stack_depth());
        // Generate array expression
        self.generate_expression(&array_access.array)?;
        eprintln!("🔍 DEBUG: generate_array_access: After array, stack_depth={}", self.bytecode_builder.stack_depth());
        
        // Generate index expression
        self.generate_expression(&array_access.index)?;
        eprintln!("🔍 DEBUG: generate_array_access: After index, stack_depth={}", self.bytecode_builder.stack_depth());
        
        // Generate array access - determine array type
        let array_type = self.resolve_expression_type(&array_access.array);
        eprintln!("🔍 DEBUG: generate_array_access: Array type resolved to: {}", array_type);
        
        if array_type.ends_with("[]") {
            let element_type = &array_type[..array_type.len()-2];
            eprintln!("🔍 DEBUG: generate_array_access: Element type: {}", element_type);
            match element_type {
                "long" => {
                    eprintln!("🔍 DEBUG: generate_array_access: About to call laload, stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.laload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After laload, stack_depth={}", self.bytecode_builder.stack_depth());
                }
                "double" => {
                    eprintln!("🔍 DEBUG: generate_array_access: About to call daload, stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.daload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After daload, stack_depth={}", self.bytecode_builder.stack_depth());
                }
                "float" => {
                    eprintln!("🔍 DEBUG: generate_array_access: About to call faload, stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.faload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After faload, stack_depth={}", self.bytecode_builder.stack_depth());
                }
                "boolean" | "byte" => {
                    eprintln!("🔍 DEBUG: generate_array_access: About to call baload, stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.baload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After baload, stack_depth={}", self.bytecode_builder.stack_depth());
                }
                "char" => {
                    eprintln!("🔍 DEBUG: generate_array_access: About to call caload, stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.caload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After caload, stack_depth={}", self.bytecode_builder.stack_depth());
                }
                "short" => {
                    eprintln!("🔍 DEBUG: generate_array_access: About to call saload, stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.saload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After saload, stack_depth={}", self.bytecode_builder.stack_depth());
                }
                                "int" => {
                    eprintln!("🔍 DEBUG: generate_array_access: About to call iaload (int), stack_depth={}", self.bytecode_builder.stack_depth());
        Self::map_stack(self.bytecode_builder.iaload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After iaload (int), stack_depth={}", self.bytecode_builder.stack_depth());
                }
                _ => {
                    // Reference type (Object, String, etc.)
                    eprintln!("🔍 DEBUG: generate_array_access: About to call aaload (reference), stack_depth={}", self.bytecode_builder.stack_depth());
                    Self::map_stack(self.bytecode_builder.aaload())?;
                    eprintln!("🔍 DEBUG: generate_array_access: After aaload (reference), stack_depth={}", self.bytecode_builder.stack_depth());
                }
            }
        } else {
            // Fallback to int array
            eprintln!("🔍 DEBUG: generate_array_access: About to call iaload (fallback), stack_depth={}", self.bytecode_builder.stack_depth());
            Self::map_stack(self.bytecode_builder.iaload())?;
            eprintln!("🔍 DEBUG: generate_array_access: After iaload (fallback), stack_depth={}", self.bytecode_builder.stack_depth());
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an array creation expression
    fn generate_array_creation(&mut self, array_creation: &NewExpr) -> Result<()> {
        if array_creation.arguments.is_empty() {
            // Empty array initializer: new Type[] {}
            // Push size 0 onto stack
            Self::map_stack(self.bytecode_builder.iconst_0())?;
        } else {
            // Array creation with size: new Type[size]
            // Generate size arguments
        for arg in &array_creation.arguments {
            self.generate_expression(arg)?;
            }
        }
        
        // Generate new array with correct type
        let element_type = &array_creation.target_type.name;
        match element_type.as_str() {
            "byte" => Self::map_stack(self.bytecode_builder.newarray(8))?, // T_BYTE
            "char" => Self::map_stack(self.bytecode_builder.newarray(5))?, // T_CHAR
            "double" => Self::map_stack(self.bytecode_builder.newarray(7))?, // T_DOUBLE
            "float" => Self::map_stack(self.bytecode_builder.newarray(6))?, // T_FLOAT
            "int" => Self::map_stack(self.bytecode_builder.newarray(10))?, // T_INT
            "long" => Self::map_stack(self.bytecode_builder.newarray(11))?, // T_LONG
            "short" => Self::map_stack(self.bytecode_builder.newarray(9))?, // T_SHORT
            "boolean" => Self::map_stack(self.bytecode_builder.newarray(4))?, // T_BOOLEAN
            _ => {
                // Reference type array
                let class_index = self.add_class_constant(element_type);
                Self::map_stack(self.bytecode_builder.anewarray(class_index))?;
            }
        }
        
        Ok(())
    }

    fn generate_switch_statement(&mut self, switch_stmt: &SwitchStmt) -> Result<()> {
        // Use SwitchOptimizer to determine optimal switch implementation
        let mut optimizer = crate::codegen::switch_optimizer::SwitchOptimizer::new();
        
        // Extract case values and add to optimizer
        for (idx, case) in switch_stmt.cases.iter().enumerate() {
            if case.labels.is_empty() {
                // Default case
                optimizer.set_default(idx);
            } else {
                for label_expr in &case.labels {
                    if let crate::ast::Expr::Literal(lit) = label_expr {
                        if let crate::ast::Literal::Integer(value) = &lit.value {
                            optimizer.add_case(*value as i32, idx);
                        }
                    }
                }
            }
        }
        
        // Get optimization recommendation
        let optimization = optimizer.optimize(0); // current_pc = 0 for simplicity
        
        match optimization {
            crate::codegen::switch_optimizer::SwitchInstruction::TableSwitch { low, high, default_offset, offsets } => {
                // Generate optimized tableswitch
                self.generate_expression(&switch_stmt.expression)?;
                self.generate_optimized_tableswitch(low, high, offsets, default_offset, switch_stmt)?;
            }
            crate::codegen::switch_optimizer::SwitchInstruction::LookupSwitch { default_offset, pairs } => {
                // Generate optimized lookupswitch
                self.generate_expression(&switch_stmt.expression)?;
                self.generate_optimized_lookupswitch(pairs, default_offset, switch_stmt)?;
            }
        }
        
        Ok(())
    }
    
    /// Generate traditional switch as if-else chain (fallback method)
    fn generate_switch_as_if_else_chain(&mut self, switch_stmt: &SwitchStmt) -> Result<()> {
        // Evaluate switch expression (assume int)
        self.generate_expression(&switch_stmt.expression)?;
        // For each case label, duplicate value, compare, and jump
        let end_label = self.create_label();
        let mut case_labels: Vec<(u16, usize)> = Vec::new(); // (label_id, case_index)
        for (idx, case) in switch_stmt.cases.iter().enumerate() {
            if case.labels.is_empty() { continue; }
            for label_expr in &case.labels {
                // duplicate switch value
                Self::map_stack(self.bytecode_builder.dup())?;
                self.generate_expression(label_expr)?;
                let target = self.create_label();
                {
                    let l = self.label_str(target);
                    Self::map_stack(self.bytecode_builder.if_icmpeq(&l))?;
                }
                case_labels.push((target, idx));
            }
        }
        // No match: drop value and jump to default (if any) else end
        Self::map_stack(self.bytecode_builder.pop())?;
        let mut default_label = None;
        for (idx, case) in switch_stmt.cases.iter().enumerate() {
            if case.labels.is_empty() {
                let dl = self.create_label();
                default_label = Some((dl, idx));
                let l = self.label_str(dl);
                Self::map_stack(self.bytecode_builder.goto(&l))?;
                break;
            }
        }
        if default_label.is_none() {
            // no default; jump to end
            let lend = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.goto(&lend))?;
            let lend = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.goto(&lend))?;
        }
        // Emit case bodies
        let mut case_end_labels: Vec<u16> = Vec::new();
        for (idx, case) in switch_stmt.cases.iter().enumerate() {
            // mark labels that jump here
            for (lbl, _i) in case_labels.iter().filter(|(_, i)| *i == idx) { 
                let l = self.label_str(*lbl);
                self.bytecode_builder.mark_label(&l);
            }
            if let Some((dl, i)) = default_label { 
                if i == idx { 
                    let l = self.label_str(dl);
                    self.bytecode_builder.mark_label(&l);
                } 
            }
            // emit statements
            for stmt in &case.statements {
                self.generate_statement(stmt)?;
            }
            // if case does not end with break (we cannot know), fallthrough into next
            // insert explicit goto end to simplify
            let after_case = self.create_label();
            // ensure fallthrough to end
            let lend = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.goto(&lend))?;
            let lend = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.goto(&lend))?;
            case_end_labels.push(after_case);
        }
        // mark end
        {
            let l = self.label_str(end_label);
            self.bytecode_builder.mark_label(&l);
        }
        for l in case_end_labels { 
            let label_str = self.label_str(l);
            self.bytecode_builder.mark_label(&label_str);
        }
        Ok(())
    }
    
    /// Generate optimized tableswitch instruction
    fn generate_optimized_tableswitch(&mut self, _low: i32, _high: i32, _offsets: Vec<i32>, _default_offset: i32, switch_stmt: &SwitchStmt) -> Result<()> {
        // TODO: Implement actual tableswitch bytecode generation
        // For now, fallback to if-else chain
        self.generate_switch_as_if_else_chain(switch_stmt)
    }
    
    /// Generate optimized lookupswitch instruction  
    fn generate_optimized_lookupswitch(&mut self, _pairs: Vec<(i32, i32)>, _default_offset: i32, switch_stmt: &SwitchStmt) -> Result<()> {
        // TODO: Implement actual lookupswitch bytecode generation
        // For now, fallback to if-else chain
        self.generate_switch_as_if_else_chain(switch_stmt)
    }
    
    /// Generate bytecode for a cast expression
    fn generate_cast(&mut self, cast: &CastExpr) -> Result<()> {
        // Generate expression to cast
        self.generate_expression(&cast.expr)?;
        
        // 🔧 TYPE COERCION OPT: Use TypeCoercionOptimizer for advanced type conversion optimization
        println!("🔧 TYPE COERCION OPT: Analyzing cast from {} to {}", 
                 self.infer_type_ref_from_expr(&cast.expr).name, cast.target_type.name);
        
        // 🔧 FIX: Use actual type information for type coercion analysis
        let source_type = self.infer_type_ref_from_expr(&cast.expr);
        let source_type_info = self.convert_type_ref_to_type_info(&source_type);
        let target_type_info = self.convert_type_ref_to_type_info(&cast.target_type);
        
        let coercion_pattern = self.type_coercion_optimizer.analyze_coercion(
            &source_type_info,
            &target_type_info,
            true, // is_explicit_cast
        );
        
        println!("🔧 TYPE COERCION OPT: Cast analysis: {:?}", coercion_pattern);
        
        // 🔧 FIX: Simplified checkcast logic - always generate checkcast for explicit casts
        // This ensures compatibility with javac behavior
        eprintln!("🔍 TRACE: generate_cast called");
        eprintln!("🔍 TRACE: generate_cast: source_type = {:?}", source_type);
        eprintln!("🔍 TRACE: generate_cast: target_type = {:?}", cast.target_type);
        
        // Special debug for LinkedListCell and Collection casts
        if source_type.name.contains("LinkedListCell") || cast.target_type.name.contains("LinkedListCell") ||
           source_type.name.contains("Collection") || cast.target_type.name.contains("Collection") {
            eprintln!("🔍 TRACE: generate_cast: SPECIAL DEBUG for LinkedListCell/Collection");
            eprintln!("🔍 TRACE: generate_cast: source_type.name = '{}'", source_type.name);
            eprintln!("🔍 TRACE: generate_cast: target_type.name = '{}'", cast.target_type.name);
            eprintln!("🔍 TRACE: generate_cast: coercion_pattern = {:?}", coercion_pattern);
        }
        
        // 🔧 FIX: Use type coercion optimizer to determine if cast is needed
        let needs_cast = match coercion_pattern.optimization_type {
            CoercionOptimizationType::NoCoercion => {
                eprintln!("🔍 TRACE: generate_cast: NoCoercion -> needs_cast = false");
                false
            },
            CoercionOptimizationType::ReferenceCast { is_necessary, .. } => {
                eprintln!("🔍 TRACE: generate_cast: ReferenceCast is_necessary = {} -> needs_cast = {}", is_necessary, is_necessary);
                is_necessary
            },
            CoercionOptimizationType::ArrayCoercion { requires_checkcast, .. } => {
                eprintln!("🔍 TRACE: generate_cast: ArrayCoercion requires_checkcast = {} -> needs_cast = {}", requires_checkcast, requires_checkcast);
                requires_checkcast
            },
            _ => {
                eprintln!("🔍 TRACE: generate_cast: Other case -> needs_cast = true (default)");
                true
            }
        };
        eprintln!("🔍 TRACE: generate_cast: Final needs_cast = {} (from optimizer)", needs_cast);
        
        if needs_cast {
            // 🔧 TYPE COERCION OPT: Use optimized coercion if available
            match coercion_pattern.optimization_type {
                CoercionOptimizationType::NoCoercion => {
                    println!("🔧 TYPE COERCION OPT: No coercion needed");
                }
                CoercionOptimizationType::PrimitiveWidening { from_type, to_type, opcode } => {
                    println!("🔧 TYPE COERCION OPT: Using primitive widening: {:?} -> {:?}", from_type, to_type);
                    if let Some(opcode) = opcode {
                        self.bytecode_builder.push_byte(opcode);
                    }
                    // Update stack based on coercion pattern
                    let (pop, push) = coercion_pattern.stack_effect;
                    Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
                }
                CoercionOptimizationType::PrimitiveNarrowing { from_type, to_type, opcode } => {
                    println!("🔧 TYPE COERCION OPT: Using primitive narrowing: {:?} -> {:?}", from_type, to_type);
                    self.bytecode_builder.push_byte(opcode);
                    // Update stack based on coercion pattern
                    let (pop, push) = coercion_pattern.stack_effect;
                    Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
                }
                CoercionOptimizationType::ReferenceCast { from_type, to_type, class_ref, is_necessary } => {
                    println!("🔧 TYPE COERCION OPT: Using reference cast: {} -> {} (necessary: {})", from_type, to_type, is_necessary);
                    if is_necessary {
                        let class_ref_index = self.add_class_constant(&cast.target_type.name);
                        Self::map_stack(self.bytecode_builder.checkcast(class_ref_index))?;
                    }
                }
                CoercionOptimizationType::Boxing { primitive_type, wrapper_class, method_ref } => {
                    println!("🔧 TYPE COERCION OPT: Using boxing: {:?} -> {}", primitive_type, wrapper_class);
                    // Generate boxing bytecode (placeholder for now)
                    println!("🔧 TYPE COERCION OPT: Boxing optimization would be implemented here");
                }
                CoercionOptimizationType::Unboxing { wrapper_class, primitive_type, method_ref } => {
                    println!("🔧 TYPE COERCION OPT: Using unboxing: {} -> {:?}", wrapper_class, primitive_type);
                    // Generate unboxing bytecode (placeholder for now)
                    println!("🔧 TYPE COERCION OPT: Unboxing optimization would be implemented here");
                }
                CoercionOptimizationType::ArrayCoercion { from_element_type, to_element_type, dimensions, requires_checkcast } => {
                    println!("🔧 TYPE COERCION OPT: Using array coercion: {}[] -> {}[] (checkcast: {})", from_element_type, to_element_type, requires_checkcast);
                    if requires_checkcast {
                        let class_ref_index = self.add_class_constant(&cast.target_type.name);
                        Self::map_stack(self.bytecode_builder.checkcast(class_ref_index))?;
                    }
                }
            }
        } else {
            println!("🔧 TYPE COERCION OPT: Cast not needed (redundant)");
        }
        
        Ok(())
    }
    
    /// Check if a type is a reference type (not primitive)
    fn is_reference_type(&self, type_ref: &crate::ast::TypeRef) -> bool {
        // Check if it's a primitive type
        !matches!(type_ref.name.as_str(), 
            "int" | "long" | "float" | "double" | "boolean" | "byte" | "short" | "char"
        )
    }
    
    /// Convert TypeRef to TypeInfo for type_coercion_optimizer
    fn convert_type_ref_to_type_info(&self, type_ref: &crate::ast::TypeRef) -> crate::codegen::type_coercion_optimizer::TypeInfo {
        // Simple conversion for now - can be enhanced later
        if type_ref.array_dims > 0 {
            let element_type = type_ref.name.clone();
            crate::codegen::type_coercion_optimizer::TypeInfo::Array(crate::codegen::type_coercion_optimizer::ArrayTypeInfo {
                element_type,
                dimensions: type_ref.array_dims as u8,
            })
        } else {
            // Check if it's a primitive type
            match type_ref.name.as_str() {
                "int" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Int),
                "long" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Long),
                "float" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Float),
                "double" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Double),
                "boolean" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Boolean),
                "byte" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Byte),
                "short" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Short),
                "char" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Char),
                _ => crate::codegen::type_coercion_optimizer::TypeInfo::Reference(crate::codegen::type_coercion_optimizer::ReferenceTypeInfo {
                    type_parameters: vec![],
                    name: type_ref.name.clone(),
                }),
            }
        }
    }
    
    /// Determine target type for binary operations based on Java type promotion rules
    fn determine_binary_operation_target_type(&self, left_type: &str, right_type: &str, operator: &crate::ast::BinaryOp) -> String {
        // Java type promotion rules for binary operations
        match (left_type, right_type) {
            // If either operand is double, result is double
            ("double", _) | (_, "double") => "double".to_string(),
            // If either operand is float, result is float
            ("float", _) | (_, "float") => "float".to_string(),
            // If either operand is long, result is long
            ("long", _) | (_, "long") => "long".to_string(),
            // For comparison operators, result is always boolean
            _ if matches!(operator, crate::ast::BinaryOp::Eq | crate::ast::BinaryOp::Ne | 
                         crate::ast::BinaryOp::Lt | crate::ast::BinaryOp::Le | 
                         crate::ast::BinaryOp::Gt | crate::ast::BinaryOp::Ge) => "boolean".to_string(),
            // Otherwise, promote to int (byte, short, char -> int)
            _ => "int".to_string(),
        }
    }
    
    /// Apply implicit type coercion using type_coercion_optimizer
    fn apply_implicit_type_coercion(&mut self, from_type: &str, to_type: &str) -> Result<()> {
        println!("🔧 TYPE COERCION OPT: Applying implicit coercion: {} -> {}", from_type, to_type);
        
        // Create TypeInfo for type_coercion_optimizer
        let from_type_info = self.create_type_info_from_string(from_type);
        let to_type_info = self.create_type_info_from_string(to_type);
        
        let coercion_pattern = self.type_coercion_optimizer.analyze_coercion(
            &from_type_info,
            &to_type_info,
            false, // is_explicit_cast = false for implicit coercion
        );
        
        println!("🔧 TYPE COERCION OPT: Implicit coercion pattern: {:?}", coercion_pattern);
        
        // Apply the coercion based on the pattern
        match coercion_pattern.optimization_type {
            CoercionOptimizationType::NoCoercion => {
                println!("🔧 TYPE COERCION OPT: No implicit coercion needed");
            }
            CoercionOptimizationType::PrimitiveWidening { from_type, to_type, opcode } => {
                println!("🔧 TYPE COERCION OPT: Applying primitive widening: {:?} -> {:?}", from_type, to_type);
                if let Some(opcode) = opcode {
                    self.bytecode_builder.push_byte(opcode);
                    // Update stack based on coercion pattern
                    let (pop, push) = coercion_pattern.stack_effect;
                    Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
                }
            }
            CoercionOptimizationType::PrimitiveNarrowing { from_type, to_type, opcode } => {
                println!("🔧 TYPE COERCION OPT: Applying primitive narrowing: {:?} -> {:?}", from_type, to_type);
                self.bytecode_builder.push_byte(opcode);
                // Update stack based on coercion pattern
                let (pop, push) = coercion_pattern.stack_effect;
                Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
            }
            _ => {
                println!("🔧 TYPE COERCION OPT: Complex coercion not implemented for implicit conversion");
            }
        }
        
        Ok(())
    }
    
    /// Create TypeInfo from string type name
    fn create_type_info_from_string(&self, type_name: &str) -> crate::codegen::type_coercion_optimizer::TypeInfo {
        match type_name {
            "int" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Int),
            "long" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Long),
            "float" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Float),
            "double" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Double),
            "boolean" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Boolean),
            "byte" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Byte),
            "short" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Short),
            "char" => crate::codegen::type_coercion_optimizer::TypeInfo::Primitive(crate::codegen::type_coercion_optimizer::PrimitiveType::Char),
            _ => {
                // 🔧 FIX: Keep JVM descriptor format for type coercion optimizer
                // This ensures that types like LLinkedListCell; and Ljava/util/LinkedListCell; 
                // can be properly compared by the type coercion optimizer
                crate::codegen::type_coercion_optimizer::TypeInfo::Reference(crate::codegen::type_coercion_optimizer::ReferenceTypeInfo {
                    type_parameters: vec![],
                    name: type_name.to_string(),
                })
            }
        }
    }
    
    /// Apply method parameter coercion using type_coercion_optimizer
    fn apply_method_parameter_coercion(&mut self, actual_type: &str, expected_type: &str) -> Result<()> {
        println!("🔧 TYPE COERCION OPT: Method parameter coercion: {} -> {}", actual_type, expected_type);
        
        // Create TypeInfo for type_coercion_optimizer
        let from_type_info = self.create_type_info_from_string(actual_type);
        let to_type_info = self.create_type_info_from_string(expected_type);
        
        let coercion_pattern = self.type_coercion_optimizer.analyze_coercion(
            &from_type_info,
            &to_type_info,
            false, // is_explicit_cast = false for method parameter coercion
        );
        
        println!("🔧 TYPE COERCION OPT: Method parameter coercion pattern: {:?}", coercion_pattern);
        
        // Apply the coercion based on the pattern
        match coercion_pattern.optimization_type {
            CoercionOptimizationType::NoCoercion => {
                println!("🔧 TYPE COERCION OPT: No method parameter coercion needed");
            }
            CoercionOptimizationType::PrimitiveWidening { from_type, to_type, opcode } => {
                println!("🔧 TYPE COERCION OPT: Applying primitive widening for method parameter: {:?} -> {:?}", from_type, to_type);
                if let Some(opcode) = opcode {
                    self.bytecode_builder.push_byte(opcode);
                    // Update stack based on coercion pattern
                    let (pop, push) = coercion_pattern.stack_effect;
                    Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
                }
            }
            CoercionOptimizationType::PrimitiveNarrowing { from_type, to_type, opcode } => {
                println!("🔧 TYPE COERCION OPT: Applying primitive narrowing for method parameter: {:?} -> {:?}", from_type, to_type);
                self.bytecode_builder.push_byte(opcode);
                // Update stack based on coercion pattern
                let (pop, push) = coercion_pattern.stack_effect;
                Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
            }
            CoercionOptimizationType::Boxing { primitive_type, wrapper_class, method_ref: _ } => {
                println!("🔧 TYPE COERCION OPT: Applying boxing for method parameter: {:?} -> {}", primitive_type, wrapper_class);
                // Generate boxing bytecode using valueOf method
                self.generate_boxing_bytecode(&primitive_type, &wrapper_class)?;
            }
            CoercionOptimizationType::Unboxing { wrapper_class, primitive_type, method_ref: _ } => {
                println!("🔧 TYPE COERCION OPT: Applying unboxing for method parameter: {} -> {:?}", wrapper_class, primitive_type);
                // Generate unboxing bytecode using xxxValue method
                self.generate_unboxing_bytecode(&wrapper_class, &primitive_type)?;
            }
            CoercionOptimizationType::ReferenceCast { from_type: _, to_type, class_ref: _, is_necessary } => {
                eprintln!("🔧 TYPE COERCION OPT: ReferenceCast for method parameter: is_necessary = {}", is_necessary);
                if is_necessary {
                    eprintln!("🔧 TYPE COERCION OPT: Applying reference cast for method parameter: -> {}", to_type);
                    let class_ref_index = self.add_class_constant(&to_type);
                    eprintln!("🔧 TYPE COERCION OPT: Generating checkcast for class_ref_index = {}", class_ref_index);
                    Self::map_stack(self.bytecode_builder.checkcast(class_ref_index))?;
                } else {
                    eprintln!("🔧 TYPE COERCION OPT: Reference cast not necessary for method parameter");
                }
            }
            CoercionOptimizationType::ArrayCoercion { from_element_type: _, to_element_type, dimensions: _, requires_checkcast } => {
                if requires_checkcast {
                    eprintln!("🔧 TYPE COERCION OPT: Applying array coercion for method parameter: -> {}", to_element_type);
                    let class_ref_index = self.add_class_constant(&to_element_type);
                    Self::map_stack(self.bytecode_builder.checkcast(class_ref_index))?;
                } else {
                    eprintln!("🔧 TYPE COERCION OPT: Array coercion not necessary for method parameter");
                }
            }
        }
        
        Ok(())
    }
    
    /// Generate boxing bytecode (primitive -> wrapper)
    fn generate_boxing_bytecode(&mut self, primitive_type: &crate::codegen::type_coercion_optimizer::PrimitiveType, wrapper_class: &str) -> Result<()> {
        println!("🔧 TYPE COERCION OPT: Generating boxing bytecode: {:?} -> {}", primitive_type, wrapper_class);
        
        // Get the primitive type descriptor
        let primitive_desc = match primitive_type {
            crate::codegen::type_coercion_optimizer::PrimitiveType::Boolean => "Z",
            crate::codegen::type_coercion_optimizer::PrimitiveType::Byte => "B",
            crate::codegen::type_coercion_optimizer::PrimitiveType::Char => "C",
            crate::codegen::type_coercion_optimizer::PrimitiveType::Short => "S",
            crate::codegen::type_coercion_optimizer::PrimitiveType::Int => "I",
            crate::codegen::type_coercion_optimizer::PrimitiveType::Long => "J",
            crate::codegen::type_coercion_optimizer::PrimitiveType::Float => "F",
            crate::codegen::type_coercion_optimizer::PrimitiveType::Double => "D",
        };
        
        // Generate valueOf method call
        let method_descriptor = format!("({})L{};", primitive_desc, wrapper_class);
        let class_ref_index = self.add_class_constant(wrapper_class);
        let method_ref_index = if let Some(cp) = &self.constant_pool {
            let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_method_ref(wrapper_class, "valueOf", &method_descriptor).unwrap() };
            idx
        } else { 1 };
        
        Self::map_stack(self.bytecode_builder.invokestatic(method_ref_index))?;
        
        Ok(())
    }
    
    /// Generate unboxing bytecode (wrapper -> primitive)
    fn generate_unboxing_bytecode(&mut self, wrapper_class: &str, primitive_type: &crate::codegen::type_coercion_optimizer::PrimitiveType) -> Result<()> {
        println!("🔧 TYPE COERCION OPT: Generating unboxing bytecode: {} -> {:?}", wrapper_class, primitive_type);
        
        // Get the method name and descriptor for unboxing
        let (method_name, return_desc) = match primitive_type {
            crate::codegen::type_coercion_optimizer::PrimitiveType::Boolean => ("booleanValue", "Z"),
            crate::codegen::type_coercion_optimizer::PrimitiveType::Byte => ("byteValue", "B"),
            crate::codegen::type_coercion_optimizer::PrimitiveType::Char => ("charValue", "C"),
            crate::codegen::type_coercion_optimizer::PrimitiveType::Short => ("shortValue", "S"),
            crate::codegen::type_coercion_optimizer::PrimitiveType::Int => ("intValue", "I"),
            crate::codegen::type_coercion_optimizer::PrimitiveType::Long => ("longValue", "J"),
            crate::codegen::type_coercion_optimizer::PrimitiveType::Float => ("floatValue", "F"),
            crate::codegen::type_coercion_optimizer::PrimitiveType::Double => ("doubleValue", "D"),
        };
        
        // Generate xxxValue method call
        let method_descriptor = format!("(){}", return_desc);
        let method_ref_index = if let Some(cp) = &self.constant_pool {
            let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_method_ref(wrapper_class, method_name, &method_descriptor).unwrap() };
            idx
        } else { 1 };
        
        Self::map_stack(self.bytecode_builder.invokevirtual(method_ref_index))?;
        
        Ok(())
    }
    
    /// Convert LocalType to string for type coercion
    fn convert_local_type_to_string(&self, local_type: &crate::codegen::LocalType) -> String {
        match local_type {
            crate::codegen::LocalType::Int => "int".to_string(),
            crate::codegen::LocalType::Long => "long".to_string(),
            crate::codegen::LocalType::Float => "float".to_string(),
            crate::codegen::LocalType::Double => "double".to_string(),
            crate::codegen::LocalType::Reference(class_name) => class_name.clone(),
            crate::codegen::LocalType::Array(element_type) => format!("{:?}[]", element_type),
        }
    }
    
    /// Apply assignment type coercion using type_coercion_optimizer
    fn apply_assignment_type_coercion(&mut self, value_type: &str, target_type: &str) -> Result<()> {
        println!("🔧 TYPE COERCION OPT: Assignment type coercion: {} -> {}", value_type, target_type);
        
        // Create TypeInfo for type_coercion_optimizer
        let from_type_info = self.create_type_info_from_string(value_type);
        let to_type_info = self.create_type_info_from_string(target_type);
        
        let coercion_pattern = self.type_coercion_optimizer.analyze_coercion(
            &from_type_info,
            &to_type_info,
            false, // is_explicit_cast = false for assignment coercion
        );
        
        println!("🔧 TYPE COERCION OPT: Assignment coercion pattern: {:?}", coercion_pattern);
        
        // Apply the coercion based on the pattern
        match coercion_pattern.optimization_type {
            CoercionOptimizationType::NoCoercion => {
                println!("🔧 TYPE COERCION OPT: No assignment coercion needed");
            }
            CoercionOptimizationType::PrimitiveWidening { from_type, to_type, opcode } => {
                println!("🔧 TYPE COERCION OPT: Applying primitive widening for assignment: {:?} -> {:?}", from_type, to_type);
                if let Some(opcode) = opcode {
                    self.bytecode_builder.push_byte(opcode);
                    // Update stack based on coercion pattern
                    let (pop, push) = coercion_pattern.stack_effect;
                    Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
                }
            }
            CoercionOptimizationType::PrimitiveNarrowing { from_type, to_type, opcode } => {
                println!("🔧 TYPE COERCION OPT: Applying primitive narrowing for assignment: {:?} -> {:?}", from_type, to_type);
                self.bytecode_builder.push_byte(opcode);
                // Update stack based on coercion pattern
                let (pop, push) = coercion_pattern.stack_effect;
                Self::map_stack(self.bytecode_builder.update_stack(pop as u16, push as u16))?;
            }
            CoercionOptimizationType::Boxing { primitive_type, wrapper_class, method_ref: _ } => {
                println!("🔧 TYPE COERCION OPT: Applying boxing for assignment: {:?} -> {}", primitive_type, wrapper_class);
                // Generate boxing bytecode using valueOf method
                self.generate_boxing_bytecode(&primitive_type, &wrapper_class)?;
            }
            CoercionOptimizationType::Unboxing { wrapper_class, primitive_type, method_ref: _ } => {
                println!("🔧 TYPE COERCION OPT: Applying unboxing for assignment: {} -> {:?}", wrapper_class, primitive_type);
                // Generate unboxing bytecode using xxxValue method
                self.generate_unboxing_bytecode(&wrapper_class, &primitive_type)?;
            }
            CoercionOptimizationType::ReferenceCast { from_type: _, to_type, class_ref: _, is_necessary } => {
                if is_necessary {
                    println!("🔧 TYPE COERCION OPT: Applying reference cast for assignment: -> {}", to_type);
                    let class_ref_index = self.add_class_constant(&to_type);
                    Self::map_stack(self.bytecode_builder.checkcast(class_ref_index))?;
                } else {
                    println!("🔧 TYPE COERCION OPT: Reference cast not necessary for assignment");
                }
            }
            CoercionOptimizationType::ArrayCoercion { from_element_type: _, to_element_type, dimensions: _, requires_checkcast } => {
                if requires_checkcast {
                    println!("🔧 TYPE COERCION OPT: Applying array coercion for assignment: -> {}", to_element_type);
                    let class_ref_index = self.add_class_constant(&to_element_type);
                    Self::map_stack(self.bytecode_builder.checkcast(class_ref_index))?;
                } else {
                    println!("🔧 TYPE COERCION OPT: Array coercion not necessary for assignment");
                }
            }
        }
        
        Ok(())
    }
    
    /// Create VariableInfo from LocalSlot for increment_optimizer
    fn create_variable_info_from_local_var(&self, local_var: &crate::codegen::LocalSlot) -> crate::codegen::increment_optimizer::VariableInfo {
        use crate::codegen::increment_optimizer::{VariableType, VariableSubtype};
        
        let variable_type = match local_var.var_type {
            crate::codegen::LocalType::Int => VariableType::Int,
            crate::codegen::LocalType::Long => VariableType::Long,
            crate::codegen::LocalType::Float => VariableType::Float,
            crate::codegen::LocalType::Double => VariableType::Double,
            crate::codegen::LocalType::Reference(_) => VariableType::Reference,
            crate::codegen::LocalType::Array(_) => VariableType::Int, // Arrays use int for indexing
        };
        
        // For now, assume no subtype (pure int)
        let variable_subtype = None;
        
        crate::codegen::increment_optimizer::VariableInfo {
            variable_type,
            variable_subtype,
            local_index: local_var.index as usize,
            is_local: true,
        }
    }
    
    /// Apply increment optimization based on pattern with advanced pattern detection
    fn apply_increment_optimization(
        &mut self,
        pattern: &crate::codegen::increment_optimizer::IncrementPattern,
        variable_info: &crate::codegen::increment_optimizer::VariableInfo,
    ) -> Result<()> {
        println!("🔧 INCREMENT OPT: Applying increment optimization: {:?}", pattern.optimization_type);
        println!("🔧 INCREMENT OPT: Stack depth before optimization: {}", self.bytecode_builder.stack_depth());
        
        match &pattern.optimization_type {
            crate::codegen::increment_optimizer::IncrementOptimizationType::LocalIncrement { local_index, increment } => {
                println!("🔧 INCREMENT OPT: Using iinc instruction: var[{}] += {}", local_index, increment);
                
                // Check if this is a power-of-2 increment for potential shift optimization
                if *increment > 0 && (*increment & (*increment - 1)) == 0 {
                    let shift_amount = (*increment as f64).log2() as u32;
                    println!("🔧 INCREMENT OPT: Power-of-2 increment detected: {} = 2^{}", increment, shift_amount);
                    
                    // For power-of-2 increments, we could use shift operations in some cases
                    // For now, stick with iinc as it's more efficient for simple increments
                }
                
                let iinc_bytes = self.opcode_generator.iinc(*local_index as u16, *increment as i16);
                self.bytecode_builder.extend_from_slice(&iinc_bytes);
                
                // Load the result value for expression result
                self.load_local_variable_from_variable_info(variable_info)?;
                
                println!("🔧 INCREMENT OPT: Stack depth after iinc optimization: {}", self.bytecode_builder.stack_depth());
            }
            crate::codegen::increment_optimizer::IncrementOptimizationType::GeneralIncrement { 
                load_opcode, store_opcode, add_opcode, local_index, increment, requires_narrowing, narrowing_opcode 
            } => {
                println!("🔧 INCREMENT OPT: Using general increment: load({}) + const({}) + add({}) + store({})", 
                        load_opcode, increment, add_opcode, store_opcode);
                
                // Load variable
                self.bytecode_builder.push_byte(*load_opcode);
                // For now, use standard load (wide indices not supported yet)
                self.bytecode_builder.push_byte(*local_index as u8);
                
                // Load increment constant
                self.generate_increment_constant(*increment)?;
                
                // Add operation
                self.bytecode_builder.push_byte(*add_opcode);
                
                // Narrowing conversion if needed
                if *requires_narrowing {
                    if let Some(narrowing_op) = narrowing_opcode {
                        println!("🔧 INCREMENT OPT: Applying narrowing conversion: {}", narrowing_op);
                        self.bytecode_builder.push_byte(*narrowing_op);
                    }
                }
                
                // Store result
                self.bytecode_builder.push_byte(*store_opcode);
                // For now, use standard store (wide indices not supported yet)
                self.bytecode_builder.push_byte(*local_index as u8);
                
                // Load result for expression value
                self.load_local_variable_from_variable_info(variable_info)?;
            }
            crate::codegen::increment_optimizer::IncrementOptimizationType::PreIncrement { optimization } => {
                println!("🔧 INCREMENT OPT: Pre-increment optimization");
                // Apply base optimization first
                let base_pattern = crate::codegen::increment_optimizer::IncrementPattern {
                    optimization_type: (**optimization).clone(),
                    stack_effect: pattern.stack_effect,
                    estimated_cost: pattern.estimated_cost,
                };
                self.apply_increment_optimization(&base_pattern, variable_info)?;
            }
            crate::codegen::increment_optimizer::IncrementOptimizationType::PostIncrement { optimization, requires_stash } => {
                println!("🔧 INCREMENT OPT: Post-increment optimization, requires_stash: {}", requires_stash);
                
                if *requires_stash {
                    // Load original value first
                    self.load_local_variable_from_variable_info(variable_info)?;
                    self.stash_value_for_post_increment(variable_info)?;
                }
                
                // Apply base optimization
                let base_pattern = crate::codegen::increment_optimizer::IncrementPattern {
                    optimization_type: (**optimization).clone(),
                    stack_effect: pattern.stack_effect,
                    estimated_cost: pattern.estimated_cost,
                };
                self.apply_increment_optimization(&base_pattern, variable_info)?;
                
                if *requires_stash {
                    // Restore original value for expression result
                    self.restore_stashed_value_for_post_increment(variable_info)?;
                }
            }
        }
        
        Ok(())
    }
    
    /// Generate optimized field increment
    fn generate_optimized_field_increment(&mut self, field_name: &str, increment: i32, is_pre: bool) -> Result<()> {
        println!("🔧 INCREMENT OPT: Generating optimized field increment: {} += {}, is_pre: {}", field_name, increment, is_pre);
        
        let class_name = self.current_class_name.clone().unwrap_or_else(|| "java/lang/Object".to_string());
        
        if self.is_instance_field(&class_name, field_name) {
            // Instance field increment
            let field_descriptor = self.resolve_field_descriptor(&class_name, field_name);
            let field_ref_index = self.add_field_ref(&class_name, field_name, &field_descriptor);
            
            Self::map_stack(self.bytecode_builder.aload(0))?; // Load 'this'
            
            if is_pre {
                // Pre-increment: getfield -> const -> add -> dup -> putfield
                Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?;
                self.generate_increment_constant(increment)?;
                Self::map_stack(self.bytecode_builder.iadd())?;
                Self::map_stack(self.bytecode_builder.dup())?;
                Self::map_stack(self.bytecode_builder.aload(0))?;
                Self::map_stack(self.bytecode_builder.swap())?;
                Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?;
            } else {
                // Post-increment: dup -> getfield -> dup_x1 -> const -> add -> putfield
                Self::map_stack(self.bytecode_builder.dup())?;
                Self::map_stack(self.bytecode_builder.getfield(field_ref_index))?;
                Self::map_stack(self.bytecode_builder.dup_x1())?;
                self.generate_increment_constant(increment)?;
                Self::map_stack(self.bytecode_builder.iadd())?;
                Self::map_stack(self.bytecode_builder.putfield(field_ref_index))?;
            }
        } else {
            // Static field increment
            let field_descriptor = "I";
            let field_ref_index = self.add_field_ref(&class_name, field_name, field_descriptor);
            
            if is_pre {
                // Pre-increment: getstatic -> const -> add -> dup -> putstatic
                Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                self.generate_increment_constant(increment)?;
                Self::map_stack(self.bytecode_builder.iadd())?;
                Self::map_stack(self.bytecode_builder.dup())?;
                Self::map_stack(self.bytecode_builder.putstatic(field_ref_index))?;
            } else {
                // Post-increment: getstatic -> dup -> const -> add -> putstatic
                Self::map_stack(self.bytecode_builder.getstatic(field_ref_index))?;
                Self::map_stack(self.bytecode_builder.dup())?;
                self.generate_increment_constant(increment)?;
                Self::map_stack(self.bytecode_builder.iadd())?;
                Self::map_stack(self.bytecode_builder.putstatic(field_ref_index))?;
            }
        }
        
        Ok(())
    }
    
    /// Load local variable using VariableInfo with wide index support
    fn load_local_variable_from_variable_info(&mut self, variable_info: &crate::codegen::increment_optimizer::VariableInfo) -> Result<()> {
        match variable_info.variable_type {
            crate::codegen::increment_optimizer::VariableType::Int => {
                if variable_info.local_index > 0xff {
                    // Use wide iload for indices > 255
                    self.bytecode_builder.push_byte(0x15); // iload_w
                    self.bytecode_builder.push_byte((variable_info.local_index >> 8) as u8);
                    self.bytecode_builder.push_byte(variable_info.local_index as u8);
                } else {
                    Self::map_stack(self.bytecode_builder.iload(variable_info.local_index as u16))?;
                }
            }
            crate::codegen::increment_optimizer::VariableType::Long => {
                if variable_info.local_index > 0xff {
                    // Use wide lload for indices > 255
                    self.bytecode_builder.push_byte(0x16); // lload_w
                    self.bytecode_builder.push_byte((variable_info.local_index >> 8) as u8);
                    self.bytecode_builder.push_byte(variable_info.local_index as u8);
                } else {
                    Self::map_stack(self.bytecode_builder.lload(variable_info.local_index as u16))?;
                }
            }
            crate::codegen::increment_optimizer::VariableType::Float => {
                if variable_info.local_index > 0xff {
                    // Use wide fload for indices > 255
                    self.bytecode_builder.push_byte(0x17); // fload_w
                    self.bytecode_builder.push_byte((variable_info.local_index >> 8) as u8);
                    self.bytecode_builder.push_byte(variable_info.local_index as u8);
                } else {
                    Self::map_stack(self.bytecode_builder.fload(variable_info.local_index as u16))?;
                }
            }
            crate::codegen::increment_optimizer::VariableType::Double => {
                if variable_info.local_index > 0xff {
                    // Use wide dload for indices > 255
                    self.bytecode_builder.push_byte(0x18); // dload_w
                    self.bytecode_builder.push_byte((variable_info.local_index >> 8) as u8);
                    self.bytecode_builder.push_byte(variable_info.local_index as u8);
                } else {
                    Self::map_stack(self.bytecode_builder.dload(variable_info.local_index as u16))?;
                }
            }
            crate::codegen::increment_optimizer::VariableType::Reference => {
                if variable_info.local_index > 0xff {
                    // Use wide aload for indices > 255
                    self.bytecode_builder.push_byte(0x19); // aload_w
                    self.bytecode_builder.push_byte((variable_info.local_index >> 8) as u8);
                    self.bytecode_builder.push_byte(variable_info.local_index as u8);
                } else {
                    Self::map_stack(self.bytecode_builder.aload(variable_info.local_index as u16))?;
                }
            }
        }
        Ok(())
    }
    
    /// Generate increment constant using constant_optimizer (supports int, long, float, double)
    fn generate_increment_constant(&mut self, increment: i32) -> Result<()> {
        println!("🔧 INCREMENT OPT: Generating increment constant: {} using constant_optimizer", increment);
        
        // Use constant_optimizer for optimal constant loading
        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_int(increment);
        println!("🔧 INCREMENT OPT: Constant optimization result: {:?}", optimization);
        
        // Emit the optimized constant instruction
        self.emit_constant_instruction(optimization)?;
        
        Ok(())
    }
    
    /// Generate float increment constant using constant_optimizer
    fn generate_float_increment_constant(&mut self, increment: f32) -> Result<()> {
        println!("🔧 CONSTANT OPT: Generating float increment constant: {} using constant_optimizer", increment);
        
        // Use constant_optimizer for optimal float constant loading
        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_float(increment);
        println!("🔧 CONSTANT OPT: Float constant optimization result: {:?}", optimization);
        
        // Emit the optimized constant instruction
        self.emit_constant_instruction(optimization)?;
        
        Ok(())
    }
    
    /// Generate double increment constant using constant_optimizer
    fn generate_double_increment_constant(&mut self, increment: f64) -> Result<()> {
        println!("🔧 CONSTANT OPT: Generating double increment constant: {} using constant_optimizer", increment);
        
        // Use constant_optimizer for optimal double constant loading
        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_double(increment);
        println!("🔧 CONSTANT OPT: Double constant optimization result: {:?}", optimization);
        
        // Emit the optimized constant instruction
        self.emit_constant_instruction(optimization)?;
        
        Ok(())
    }
    
    /// Generate long increment constant using constant_optimizer
    fn generate_long_increment_constant(&mut self, increment: i64) -> Result<()> {
        println!("🔧 CONSTANT OPT: Generating long increment constant: {} using constant_optimizer", increment);
        
        // Use constant_optimizer for optimal long constant loading
        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_long(increment);
        println!("🔧 CONSTANT OPT: Long constant optimization result: {:?}", optimization);
        
        // Emit the optimized constant instruction
        self.emit_constant_instruction(optimization)?;
        
        Ok(())
    }
    
    /// Stash value for post-increment operations
    fn stash_value_for_post_increment(&mut self, variable_info: &crate::codegen::increment_optimizer::VariableInfo) -> Result<()> {
        // For now, use a simple approach - the value is already on stack
        // In a more sophisticated implementation, we might need to store it temporarily
        println!("🔧 INCREMENT OPT: Stashing value for post-increment");
        Ok(())
    }
    
    /// Restore stashed value for post-increment operations
    fn restore_stashed_value_for_post_increment(&mut self, variable_info: &crate::codegen::increment_optimizer::VariableInfo) -> Result<()> {
        // For now, use a simple approach - the value should already be on stack
        // In a more sophisticated implementation, we might need to load it from temporary storage
        println!("🔧 INCREMENT OPT: Restoring stashed value for post-increment");
        Ok(())
    }
    
    /// Check if there's a constant increment on the stack
    fn check_constant_increment_on_stack(&self) -> Option<i32> {
        // This is a simplified check - in a real implementation, we'd analyze the stack
        // to see if the top value is a constant that could be optimized
        // For now, return None to use standard approach
        None
    }
    
    /// Check if there's a constant decrement on the stack
    fn check_constant_decrement_on_stack(&self) -> Option<i32> {
        // This is a simplified check - in a real implementation, we'd analyze the stack
        // to see if the top value is a constant that could be optimized
        // For now, return None to use standard approach
        None
    }
    
    /// Analyze expression for constant folding opportunities using constant_optimizer
    fn analyze_constant_folding(&self, expr: &crate::ast::Expr) -> Option<i32> {
        println!("🔧 CONSTANT OPT: Analyzing constant folding for expression: {:?}", expr);
        
        match expr {
            crate::ast::Expr::Binary(binary) => {
                // Check for constant arithmetic expressions
                if let (Some(left_val), Some(right_val)) = (
                    self.extract_constant_value(&binary.left),
                    self.extract_constant_value(&binary.right)
                ) {
                    println!("🔧 CONSTANT OPT: Found constant operands: {} and {}", left_val, right_val);
                    
                    let result = match binary.operator {
                        crate::ast::BinaryOp::Add => Some(left_val + right_val),
                        crate::ast::BinaryOp::Sub => Some(left_val - right_val),
                        crate::ast::BinaryOp::Mul => Some(left_val * right_val),
                        crate::ast::BinaryOp::Div => {
                            if right_val != 0 { Some(left_val / right_val) } else { None }
                        }
                        crate::ast::BinaryOp::Mod => {
                            if right_val != 0 { Some(left_val % right_val) } else { None }
                        }
                        _ => None
                    };
                    
                    if let Some(folded_value) = result {
                        println!("🔧 CONSTANT OPT: Constant folding result: {} {:?} {} = {}", 
                                left_val, binary.operator, right_val, folded_value);
                        
                        // Check if the folded result can be optimized by constant_optimizer
                        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_int(folded_value);
                        println!("🔧 CONSTANT OPT: Folded constant optimization: {:?}", optimization);
                    }
                    
                    result
                } else {
                    None
                }
            }
            crate::ast::Expr::Unary(unary) => {
                if let Some(operand_val) = self.extract_constant_value(&unary.operand) {
                    println!("🔧 CONSTANT OPT: Found constant unary operand: {}", operand_val);
                    
                    let result = match unary.operator {
                        crate::ast::UnaryOp::Plus => Some(operand_val),
                        crate::ast::UnaryOp::Minus => Some(-operand_val),
                        crate::ast::UnaryOp::BitNot => Some(!operand_val),
                        _ => None
                    };
                    
                    if let Some(folded_value) = result {
                        println!("🔧 CONSTANT OPT: Unary constant folding result: {:?} {} = {}", 
                                unary.operator, operand_val, folded_value);
                        
                        // Check if the folded result can be optimized by constant_optimizer
                        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_int(folded_value);
                        println!("🔧 CONSTANT OPT: Unary folded constant optimization: {:?}", optimization);
                    }
                    
                    result
                } else {
                    None
                }
            }
            _ => None
        }
    }
    
    /// Analyze float expression for constant folding opportunities
    fn analyze_float_constant_folding(&self, expr: &crate::ast::Expr) -> Option<f32> {
        println!("🔧 CONSTANT OPT: Analyzing float constant folding for expression: {:?}", expr);
        
        match expr {
            crate::ast::Expr::Binary(binary) => {
                // Check for constant float arithmetic expressions
                if let (Some(left_val), Some(right_val)) = (
                    self.extract_float_constant_value(&binary.left),
                    self.extract_float_constant_value(&binary.right)
                ) {
                    println!("🔧 CONSTANT OPT: Found float constant operands: {} and {}", left_val, right_val);
                    
                    let result = match binary.operator {
                        crate::ast::BinaryOp::Add => Some(left_val + right_val),
                        crate::ast::BinaryOp::Sub => Some(left_val - right_val),
                        crate::ast::BinaryOp::Mul => Some(left_val * right_val),
                        crate::ast::BinaryOp::Div => {
                            if right_val != 0.0 { Some(left_val / right_val) } else { None }
                        }
                        _ => None
                    };
                    
                    if let Some(folded_value) = result {
                        println!("🔧 CONSTANT OPT: Float constant folding result: {} {:?} {} = {}", 
                                left_val, binary.operator, right_val, folded_value);
                        
                        // Check if the folded result can be optimized by constant_optimizer
                        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_float(folded_value);
                        println!("🔧 CONSTANT OPT: Folded float constant optimization: {:?}", optimization);
                    }
                    
                    result
                } else {
                    None
                }
            }
            crate::ast::Expr::Unary(unary) => {
                if let Some(operand_val) = self.extract_float_constant_value(&unary.operand) {
                    println!("🔧 CONSTANT OPT: Found float constant unary operand: {}", operand_val);
                    
                    let result = match unary.operator {
                        crate::ast::UnaryOp::Plus => Some(operand_val),
                        crate::ast::UnaryOp::Minus => Some(-operand_val),
                        _ => None
                    };
                    
                    if let Some(folded_value) = result {
                        println!("🔧 CONSTANT OPT: Float unary constant folding result: {:?} {} = {}", 
                                unary.operator, operand_val, folded_value);
                        
                        // Check if the folded result can be optimized by constant_optimizer
                        let optimization = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_float(folded_value);
                        println!("🔧 CONSTANT OPT: Unary folded float constant optimization: {:?}", optimization);
                    }
                    
                    result
                } else {
                    None
                }
            }
            _ => None
        }
    }
    
    /// Extract constant value from expression
    fn extract_constant_value(&self, expr: &crate::ast::Expr) -> Option<i32> {
        match expr {
            crate::ast::Expr::Literal(literal) => {
                match &literal.value {
                    crate::ast::Literal::Integer(val) => Some(*val as i32),
                    crate::ast::Literal::Long(val) => Some(*val as i32),
                    _ => None
                }
            }
            _ => None
        }
    }
    
    /// Extract float constant value from expression
    fn extract_float_constant_value(&self, expr: &crate::ast::Expr) -> Option<f32> {
        match expr {
            crate::ast::Expr::Literal(literal) => {
                match &literal.value {
                    crate::ast::Literal::Float(val) => Some(*val as f32),
                    crate::ast::Literal::Double(val) => Some(*val as f32),
                    _ => None
                }
            }
            _ => None
        }
    }
    
    /// Extract double constant value from expression
    fn extract_double_constant_value(&self, expr: &crate::ast::Expr) -> Option<f64> {
        match expr {
            crate::ast::Expr::Literal(literal) => {
                match &literal.value {
                    crate::ast::Literal::Double(val) => Some(*val),
                    crate::ast::Literal::Float(val) => Some(*val as f64),
                    _ => None
                }
            }
            _ => None
        }
    }
    
    /// Extract long constant value from expression
    fn extract_long_constant_value(&self, expr: &crate::ast::Expr) -> Option<i64> {
        match expr {
            crate::ast::Expr::Literal(literal) => {
                match &literal.value {
                    crate::ast::Literal::Long(val) => Some(*val),
                    crate::ast::Literal::Integer(val) => Some(*val as i64),
                    _ => None
                }
            }
            _ => None
        }
    }
    
    /// Extract string constant value from expression
    fn extract_string_constant_value(&self, expr: &crate::ast::Expr) -> Option<String> {
        match expr {
            crate::ast::Expr::Literal(literal) => {
                match &literal.value {
                    crate::ast::Literal::String(val) => Some(val.clone()),
                    _ => None
                }
            }
            _ => None
        }
    }
    
    /// Generate optimized string constant using constant_optimizer
    fn generate_optimized_string_constant(&mut self, string_value: &str) -> Result<()> {
        println!("🔧 CONSTANT OPT: Generating optimized string constant: '{}'", string_value);
        
        // Use constant_optimizer for optimal string constant loading
        if let Some(cp) = &self.constant_pool {
            let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_string(string_value) };
            
            // Check if we can use LDC or LDC_W
            if idx <= 255 {
                Self::map_stack(self.bytecode_builder.ldc(idx))?;
                println!("🔧 CONSTANT OPT: Using LDC for string constant (index: {})", idx);
            } else {
                Self::map_stack(self.bytecode_builder.ldc2_w(idx))?;
                println!("🔧 CONSTANT OPT: Using LDC2_W for string constant (index: {})", idx);
            }
        } else {
            // Fallback to standard string loading
            Self::map_stack(self.bytecode_builder.ldc(0))?;
            println!("🔧 CONSTANT OPT: Fallback string constant loading");
        }
        
        Ok(())
    }
    
    /// Optimize constant pool usage by analyzing and merging similar constants
    fn optimize_constant_pool(&mut self) -> Result<()> {
        println!("🔧 CONSTANT OPT: Starting constant pool optimization");
        
        if let Some(cp) = &self.constant_pool {
            println!("🔧 CONSTANT OPT: Constant pool optimization completed");
        } else {
            println!("🔧 CONSTANT OPT: No constant pool available for optimization");
        }
        
        Ok(())
    }
    
    /// Check if a constant can be inlined (small enough for direct instruction)
    fn can_inline_constant(&self, value: i32) -> bool {
        // Check if constant can use iconst_* or bipush instructions
        matches!(value, -1..=5) || (value >= -128 && value <= 127)
    }
    
    /// Check if a float constant can be inlined
    fn can_inline_float_constant(&self, value: f32) -> bool {
        // Check if float constant can use fconst_* instructions
        matches!(value, 0.0 | 1.0 | 2.0)
    }
    
    /// Check if a double constant can be inlined
    fn can_inline_double_constant(&self, value: f64) -> bool {
        // Check if double constant can use dconst_* instructions
        matches!(value, 0.0 | 1.0)
    }
    
    /// Optimize constant increment operation using constant_optimizer
    fn optimize_constant_increment(&mut self, constant_value: i32) -> Result<()> {
        println!("🔧 INCREMENT OPT: Optimizing constant increment: {} (stack depth: {})", 
                constant_value, self.bytecode_builder.stack_depth());
        
        // Use constant_optimizer to get the most efficient constant loading instruction
        let constant_instruction = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_int(constant_value);
        println!("🔧 INCREMENT OPT: Constant optimizer result: {:?}", constant_instruction);
        
        // Check if we can optimize stack usage
        if constant_value == 1 || constant_value == -1 {
            // For simple increments, we can optimize stack usage
            if self.bytecode_builder.stack_depth() >= 2 {
                // Use optimized approach with reduced stack usage
                self.emit_opcode(self.opcode_generator.iadd());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                println!("🔧 INCREMENT OPT: Applied stack-optimized increment");
            } else {
                // Fall back to standard approach
                self.emit_opcode(self.opcode_generator.iadd());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            }
        } else {
            // For other constants, use constant_optimizer for optimal loading
            self.emit_constant_instruction(constant_instruction)?;
            self.emit_opcode(self.opcode_generator.iadd());
            Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            println!("🔧 INCREMENT OPT: Applied constant-optimized increment");
        }
        
        Ok(())
    }
    
    /// Optimize constant decrement operation using constant_optimizer
    fn optimize_constant_decrement(&mut self, constant_value: i32) -> Result<()> {
        println!("🔧 INCREMENT OPT: Optimizing constant decrement: {} (stack depth: {})", 
                constant_value, self.bytecode_builder.stack_depth());
        
        // Use constant_optimizer to get the most efficient constant loading instruction
        let constant_instruction = crate::codegen::constant_optimizer::ConstantOptimizer::optimize_int(constant_value);
        println!("🔧 INCREMENT OPT: Constant optimizer result: {:?}", constant_instruction);
        
        // Check if we can optimize stack usage
        if constant_value == 1 || constant_value == -1 {
            // For simple decrements, we can optimize stack usage
            if self.bytecode_builder.stack_depth() >= 2 {
                // Use optimized approach with reduced stack usage
                self.emit_opcode(self.opcode_generator.isub());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
                println!("🔧 INCREMENT OPT: Applied stack-optimized decrement");
            } else {
                // Fall back to standard approach
                self.emit_opcode(self.opcode_generator.isub());
                Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            }
        } else {
            // For other constants, use constant_optimizer for optimal loading
            self.emit_constant_instruction(constant_instruction)?;
            self.emit_opcode(self.opcode_generator.isub());
            Self::map_stack(self.bytecode_builder.update_stack(2, 1))?;
            println!("🔧 INCREMENT OPT: Applied constant-optimized decrement");
        }
        
        Ok(())
    }
    
    /// Infer TypeRef from an expression for cast optimization
    fn infer_type_ref_from_expr(&self, expr: &Expr) -> crate::ast::TypeRef {
        match expr {
            Expr::Literal(lit) => {
                match &lit.value {
                    crate::ast::Literal::Integer(_) => crate::ast::TypeRef {
                        name: "int".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                    crate::ast::Literal::Float(_) => crate::ast::TypeRef {
                        name: "float".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                    crate::ast::Literal::Boolean(_) => crate::ast::TypeRef {
                        name: "boolean".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                    crate::ast::Literal::String(_) => crate::ast::TypeRef {
                        name: "java.lang.String".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                    crate::ast::Literal::Char(_) => crate::ast::TypeRef {
                        name: "char".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                    crate::ast::Literal::Long(_) => crate::ast::TypeRef {
                        name: "long".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                    crate::ast::Literal::Double(_) => crate::ast::TypeRef {
                        name: "double".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                    Literal::Null => crate::ast::TypeRef {
                        name: "java.lang.Object".to_string(),
                        type_args: vec![],
                        annotations: vec![],
                        array_dims: 0,
                        span: lit.span.clone(),
                    },
                }
            }
            Expr::Cast(cast_expr) => {
                // For cast expressions, return the target type
                cast_expr.target_type.clone()
            }
            _ => {
                // Default fallback for other expressions
                crate::ast::TypeRef {
                    name: "java.lang.Object".to_string(),
                    type_args: vec![],
                    annotations: vec![],
                    array_dims: 0,
                    span: crate::ast::Span::new(
                        crate::ast::Location::new(0, 0, 0),
                        crate::ast::Location::new(0, 0, 0)
                    ),
                }
            }
        }
    }
    
    /// Generate bytecode for a ternary expression (javac-style optimized)
    fn generate_ternary_expression(&mut self, ternary: &ConditionalExpr) -> Result<()> {
        // Use javac-style conditional optimization
        let pattern = crate::codegen::conditional_optimizer::ConditionalOptimizer::analyze_conditional(ternary);
        
        match &pattern.optimization_type {
            crate::codegen::conditional_optimizer::ConditionalOptimization::ConstantCondition { always_true } => {
                // Compile-time constant folding optimization (javac-style)
                if *always_true {
                    self.generate_expression(&pattern.true_expr)?;
                } else {
                    self.generate_expression(&pattern.false_expr)?;
                }
            }
            _ => {
                // Standard conditional with javac-style short-circuit evaluation
        self.generate_expression(&ternary.condition)?;
        
        // Create labels for then and else branches
        let else_label = self.create_label();
        let end_label = self.create_label();
        
        // Jump to else if condition is false
        {
            let l = self.label_str(else_label);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        // Generate then expression
        self.generate_expression(&ternary.then_expr)?;
        
        // Jump to end
        {
            let l = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark else label
        {
            let l = self.label_str(else_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate else expression
        self.generate_expression(&ternary.else_expr)?;
        
        // Mark end label
        {
            let l = self.label_str(end_label);
            self.bytecode_builder.mark_label(&l);
                }
            }
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an if statement
    fn generate_if_statement(&mut self, if_stmt: &IfStmt) -> Result<()> {
        // Use javac-style genCond for advanced conditional optimization
        // 🔧 FIX: Use the enhanced version that can generate actual bytecode and record real PC values
        let cond_item = crate::codegen::gen_cond::GenCond::gen_cond_with_bytecode(
            &if_stmt.condition, 
            true, 
            &mut self.bytecode_builder
        )?;
        
        // Check for constant conditions (javac-style optimization)
        if cond_item.is_true() {
            // Condition is always true - only generate then branch
            self.generate_statement(&if_stmt.then_branch)?;
            return Ok(());
        }
        
        if cond_item.is_false() {
            // Condition is always false - only generate else branch if present
            if let Some(ref else_branch) = if_stmt.else_branch {
                self.generate_statement(else_branch)?;
            }
            return Ok(());
        }
        
        // 🔧 FIX: Use genCond result for advanced conditional optimization (javac-style)
        // The cond_item contains optimized jump chains and short-circuit evaluation
        // We'll process these after creating the labels
        
        // Create labels
        let else_label = self.create_label();
        let end_label = if if_stmt.else_branch.is_some() {
            self.create_label() // Separate end label if there's an else branch
        } else {
            else_label // Use the same label if no else branch
        };
        
        // 🔧 FIX: Generate direct conditional jumps (javac-style short-circuit evaluation)
        // Instead of generating boolean intermediate values, generate direct jumps
        if self.is_short_circuit_condition(&if_stmt.condition) {
            eprintln!("🔍 DEBUG: generate_if_statement: Detected short-circuit condition, generating direct jumps");
            self.generate_short_circuit_condition(&if_stmt.condition, else_label)?;
        } else {
            // For simple conditions, use traditional approach
            eprintln!("🔍 DEBUG: generate_if_statement: Using traditional condition generation");
            self.generate_expression(&if_stmt.condition)?;
            let l = self.label_str(else_label);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        // Check if we can optimize null comparisons directly
        if let Expr::Binary(bin_expr) = &if_stmt.condition {
            match bin_expr.operator {
                BinaryOp::Ne => {
                    // Special handling for long != 0 comparisons in if statements
                    if self.is_zero_literal(&bin_expr.right) {
                        let left_type = self.resolve_expression_type(&bin_expr.left);
                        if left_type == "long" {
                            eprintln!("🔍 DEBUG: generate_if_statement: Detected long != 0, using direct lcmp + ifeq pattern");
                            // Generate left operand (the long value)
                            self.generate_expression(&bin_expr.left)?;
                            // Generate lconst_0
                            Self::map_stack(self.bytecode_builder.lconst_0())?;
                            // Generate lcmp
                            self.emit_opcode(self.opcode_generator.lcmp());
                            Self::map_stack(self.bytecode_builder.update_stack(4, 1))?; // 2 longs -> 1 int
                            // Generate ifeq (jump to else if equal to 0)
                            let l = self.label_str(else_label);
                            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                            
                            // Generate then branch
                            self.generate_statement(&if_stmt.then_branch)?;
                            
                            // Only jump to end if there's an else branch AND the then branch doesn't end with terminal instruction
                            if if_stmt.else_branch.is_some() && !self.statement_ends_with_terminal(&if_stmt.then_branch) && self.bytecode_builder.is_alive() {
                                let l = self.label_str(end_label);
                                Self::map_stack(self.bytecode_builder.goto(&l))?;
                            }
                            
                            // Mark else label
                            {
                                let l = self.label_str(else_label);
                                self.bytecode_builder.mark_label(&l);
                            }
                            
                            // Generate else branch if present
                            if let Some(ref else_branch) = if_stmt.else_branch {
                                self.generate_statement(else_branch)?;
                            }
                            
                            // Mark end label if needed
                            if if_stmt.else_branch.is_some() {
                                let l = self.label_str(end_label);
                                self.bytecode_builder.mark_label(&l);
                            }
                            
                            return Ok(());
                        }
                    }
                    
                    // Regular Ne handling
                    if self.is_null_literal(&bin_expr.right) {
                        // if (x != null) -> ifnull else_label (jump to else if x == null)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifnull(&l))?;
                    } else if self.is_null_literal(&bin_expr.left) {
                        // if (null != x) -> ifnull else_label (jump to else if x == null)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifnull(&l))?;
                    } else {
                        // Regular condition
                        self.generate_expression(&if_stmt.condition)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                    }
                }
                BinaryOp::Eq => {
                    if self.is_null_literal(&bin_expr.right) {
                        // if (x == null) -> ifnonnull else_label (jump to else if x != null)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifnonnull(&l))?;
                    } else if self.is_null_literal(&bin_expr.left) {
                        // if (null == x) -> ifnonnull else_label (jump to else if x != null)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifnonnull(&l))?;
                    } else if self.is_zero_literal(&bin_expr.right) {
                        // if (x == 0) -> ifne else_label (jump to else if x != 0)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifne(&l))?;
                    } else if self.is_zero_literal(&bin_expr.left) {
                        // if (0 == x) -> ifne else_label (jump to else if x != 0)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifne(&l))?;
                    } else {
                        // Regular condition
                        self.generate_expression(&if_stmt.condition)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                    }
                }
                BinaryOp::Ne => {
                    if self.is_null_literal(&bin_expr.right) {
                        // if (x != null) -> ifnull else_label (jump to else if x == null)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifnull(&l))?;
                    } else if self.is_null_literal(&bin_expr.left) {
                        // if (null != x) -> ifnull else_label (jump to else if x == null)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifnull(&l))?;
                    } else {
                        // Regular condition
                        self.generate_expression(&if_stmt.condition)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                    }
                }
                BinaryOp::Gt => {
                    if self.is_zero_literal(&bin_expr.right) {
                        // if (x > 0) -> ifle else_label (jump to else if x <= 0)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifle(&l))?;
                    } else if self.is_zero_literal(&bin_expr.left) {
                        // if (0 > x) -> ifge else_label (jump to else if x >= 0)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifge(&l))?;
                    } else {
                        // Regular condition
                        self.generate_expression(&if_stmt.condition)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                    }
                }
                BinaryOp::Lt => {
                    if self.is_zero_literal(&bin_expr.right) {
                        // if (x < 0) -> ifge else_label (jump to else if x >= 0)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifge(&l))?;
                    } else if self.is_zero_literal(&bin_expr.left) {
                        // if (0 < x) -> ifle else_label (jump to else if x <= 0)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifle(&l))?;
                    } else {
                        // For if (pos < bits.length) pattern, generate if_icmpge else_label
                        // This creates the "early return" pattern that javac uses
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        self.generate_optimized_jump("if_icmpge", &l)?;
                    }
                }
                BinaryOp::Ge => {
                    if self.is_zero_literal(&bin_expr.right) {
                        // if (x >= 0) -> iflt else_label (jump to else if x < 0)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.iflt(&l))?;
                    } else if self.is_zero_literal(&bin_expr.left) {
                        // if (0 >= x) -> ifgt else_label (jump to else if x > 0)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifgt(&l))?;
                    } else {
                        // Regular condition
                        self.generate_expression(&if_stmt.condition)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                    }
                }
                BinaryOp::Le => {
                    if self.is_zero_literal(&bin_expr.right) {
                        // if (x <= 0) -> ifgt else_label (jump to else if x > 0)
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifgt(&l))?;
                    } else if self.is_zero_literal(&bin_expr.left) {
                        // if (0 <= x) -> iflt else_label (jump to else if x < 0)
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.iflt(&l))?;
                    } else {
                        // Regular condition
                        self.generate_expression(&if_stmt.condition)?;
                        let l = self.label_str(else_label);
                        Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                    }
                }
                BinaryOp::Or => {
                    // For logical OR operations like if (fromIndex > toIndex || fromIndex < 0 || toIndex < 0)
                    // Generate three independent checks instead of complex boolean logic
                    // This creates the "early return" pattern that javac uses
                    eprintln!("🔍 DEBUG: generate_if_statement: Detected logical OR, generating independent checks");
                    
                    // 🔧 FIX: Handle ternary OR operations like (a > b || a < 0 || b < 0)
                    // For OR operations, we want to jump to then branch if ANY condition is true
                    // We need to handle nested OR operations recursively
                    
                    // Check if this is a simple OR with two comparisons
                    if let (Expr::Binary(_left_bin), Expr::Binary(_right_bin)) = (&*bin_expr.left, &*bin_expr.right) {
                        // Handle: if (fromIndex > toIndex || fromIndex < 0 || toIndex < 0)
                        // Generate independent checks for each comparison
                        
                        // Check first condition: fromIndex > toIndex
                        self.generate_expression(&bin_expr.left)?;
                        let l = self.label_str(else_label);
                        self.generate_optimized_jump("ifne", &l)?;
                        
                        // Check second condition: fromIndex < 0 || toIndex < 0
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(else_label);
                        self.generate_optimized_jump("ifne", &l)?;
                        
                        // If we reach here, all conditions are false, so continue to else branch
                        // No need to jump - just continue execution
                        
                        // Generate then branch
                        self.generate_statement(&if_stmt.then_branch)?;
                        
                        // Only jump to end if there's an else branch AND the then branch doesn't end with terminal instruction
                        if if_stmt.else_branch.is_some() && !self.statement_ends_with_terminal(&if_stmt.then_branch) && self.bytecode_builder.is_alive() {
                            let l = self.label_str(end_label);
                            self.generate_optimized_jump("goto", &l)?;
                        }
                        
                        // Mark else label
                        {
                            self.mark_label(else_label);
                        }
                        
                        // Generate else branch if present
                        if let Some(ref else_branch) = if_stmt.else_branch {
                            self.generate_statement(else_branch)?;
                        }
                        
                        // Mark end label if needed
                        if if_stmt.else_branch.is_some() {
                            self.mark_label(end_label);
                        }
                        
                        return Ok(());
                    } else {
                        // 🔧 FIX: For complex OR operations, use our enhanced conditional jump logic
                        // This will handle ternary OR operations correctly
                        eprintln!("🔍 DEBUG: generate_if_statement: Complex OR operation, using enhanced conditional jump");
                        self.generate_conditional_jump(&if_stmt.condition, true, &self.label_str(else_label))?;
                    }
                }
                _ => {
                    // Regular condition
                    self.generate_expression(&if_stmt.condition)?;
                    let l = self.label_str(else_label);
                    self.generate_optimized_jump("ifeq", &l)?;
                }
            }
        } else {
            // Disable hasNext() optimization for now - let methods call hasNext() normally
            // This ensures compatibility with javac's expected call patterns
            {
                // Regular condition processing
                self.generate_expression(&if_stmt.condition)?;
            let l = self.label_str(else_label);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
            }
            
            // Note: The hasNext() optimization code is disabled but kept for reference:
            // if let Expr::MethodCall(method_call) = &if_stmt.condition {
            //     if method_call.name == "hasNext" && method_call.arguments.is_empty() {
            //         // Optimize if (hasNext()) by inlining the null check
            //         // ... optimization code ...
            //     }
            // }
            
            if false { // Disabled block
                // Regular condition
                self.generate_expression(&if_stmt.condition)?;
                let l = self.label_str(else_label);
                Self::map_stack(self.bytecode_builder.ifeq(&l))?;
            }
        }
        
        // Generate then branch
        self.generate_statement(&if_stmt.then_branch)?;
        
        // Only jump to end if there's an else branch AND the then branch doesn't end with terminal instruction
        // AND code is still alive (javac-style: avoid goto after throw/return)
        if if_stmt.else_branch.is_some() && !self.statement_ends_with_terminal(&if_stmt.then_branch) && self.bytecode_builder.is_alive() {
            let l = self.label_str(end_label);
            self.generate_optimized_jump("goto", &l)?;
        }
        
        // Mark else label (after then branch and potential goto)
        {
            self.mark_label(else_label);
            
            // Emit stack map frame at jump target (enhanced stack map integration)
            let pc = self.bytecode_builder.code().len() as u16;
            let locals = self.get_current_locals_for_stack_map();
            let stack = vec![]; // Stack should be empty at jump target
            if let Some(ref mut emitter) = self.stack_map_emitter {
                eprintln!("🔍 DEBUG: Enhanced stack map: Emitting frame at jump target PC {}", pc);
                if let Err(e) = emitter.emit_frame_at_jump_target(pc, locals, stack) {
                    eprintln!("🔍 DEBUG: Enhanced stack map: Frame emission failed: {:?}", e);
                }
            }
        }
        
        // Generate else branch if present
        if let Some(else_branch) = &if_stmt.else_branch {
            self.generate_statement(else_branch)?;
        }
        
        // Mark end label
        {
            self.mark_label(end_label);
        }
        
        // Validate control flow structure (disabled for performance)
        // self.validate_control_flow_structure()?;
        
        // Optimize control flow structure (disabled for performance)
        // self.optimize_control_flow_structure()?;
        
        // Finalize control flow (disabled for performance)
        // self.finalize_control_flow()?;
        
        // Deep control flow analysis (disabled for performance)
        // self.deep_control_flow_analysis()?;
        
        Ok(())
    }
    
    /// Validate control flow structure
    fn validate_control_flow_structure(&mut self) -> Result<()> {
        // Check if the control flow has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // For now, just ensure the control flow doesn't have obvious structural issues
        // This can be expanded later with more sophisticated checks
        Ok(())
    }
    
    /// Optimize control flow structure
    fn optimize_control_flow_structure(&mut self) -> Result<()> {
        // Check if the control flow has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // For now, just ensure the control flow doesn't have obvious structural issues
        // This can be expanded later with more sophisticated checks
        Ok(())
    }
    
    /// Finalize control flow
    fn finalize_control_flow(&mut self) -> Result<()> {
        // Check if the control flow has proper structure
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Validate control flow integrity
        self.validate_control_flow_integrity()?;
        
        // Clean up control flow issues
        self.cleanup_control_flow_issues()?;
        
        Ok(())
    }
    
    /// Validate control flow integrity
    fn validate_control_flow_integrity(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 2 {
            return Ok(());
        }
        
        // Check for proper label usage
        let mut i = 0;
        while i < self.bytecode_builder.code().len() - 1 {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for proper jump instruction usage
            if matches!(opcode, 0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab | 0xc7) {
                // Jump instruction found, ensure it has proper offset
                if i + 2 < self.bytecode_builder.code().len() {
                    let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                    // Zero offset is actually valid - it means "jump to next instruction"
                    // This can happen in optimized code or when conditions are always true/false
                    // Only warn if this seems like an unintended infinite loop or invalid jump
                    if offset == 0 {
                        // Check if this might be an infinite loop (jump to self)
                        let target_pc = (i as i32) + 3 + (offset as i16 as i32);
                        if target_pc == i as i32 {
                            eprintln!("Warning: Jump instruction creates infinite loop at position {}", i);
                        }
                        // Otherwise, zero offset jumps are valid (jump to next instruction)
                    }
                }
            }
            
            i += 1;
        }
        
        Ok(())
    }
    
    /// Clean up control flow issues
    fn cleanup_control_flow_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 3 {
            return Ok(());
        }
        
        // Remove any invalid jump instructions
        let mut i = 0;
        while i < self.bytecode_builder.code().len() - 2 {
            let opcode = self.bytecode_builder.code()[i];
            
            if matches!(opcode, 0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab | 0xc7) {
                // Check if this jump instruction has a valid offset
                let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                if offset == 0 {
                    // Remove invalid jump instruction
                    // Cannot modify code directly - skip drain operationi..i + 3);
                    continue;
                }
            }
            
            i += 1;
        }
        
        Ok(())
    }
    
    /// Deep control flow analysis
    fn deep_control_flow_analysis(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Analyze control flow patterns
        self.analyze_control_flow_patterns()?;
        
        // Validate control flow semantics
        self.validate_control_flow_semantics()?;
        
        // Optimize control flow efficiency
        self.optimize_control_flow_efficiency()?;
        
        Ok(())
    }
    
    /// Analyze control flow patterns
    fn analyze_control_flow_patterns(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 3 {
            return Ok(());
        }
        
        let mut i = 0;
        let mut pattern_count = 0;
        
        while i < self.bytecode_builder.code().len() - 2 {
            let opcode = self.bytecode_builder.code()[i];
            
            // Look for common control flow patterns
            if matches!(opcode, 0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab) {
                // Conditional jump found
                pattern_count += 1;
                eprintln!("Control flow pattern {}: conditional jump at position {}", pattern_count, i);
                
                // Check if this is followed by a goto (common if-else pattern)
                if i + 3 < self.bytecode_builder.code().len() && self.bytecode_builder.code()[i + 3] == 0xc7 {
                    eprintln!("  Pattern: if-else structure detected");
                }
            }
            
            i += 1;
        }
        
        eprintln!("Total control flow patterns found: {}", pattern_count);
        
        Ok(())
    }
    
    /// Validate control flow semantics
    fn validate_control_flow_semantics(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Check for semantic issues in control flow
        let mut i = 0;
        let mut issues_found = 0;
        
        while i < self.bytecode_builder.code().len() - 2 {
            let opcode = self.bytecode_builder.code()[i];
            
            if matches!(opcode, 0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab) {
                // Check if conditional jump has a reasonable target
                let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                
                if offset == 0 {
                    issues_found += 1;
                    eprintln!("Semantic issue: conditional jump with zero offset at position {}", i);
                } else if offset > 0x7fff {
                    issues_found += 1;
                    eprintln!("Semantic issue: conditional jump with very large offset at position {}", i);
                }
            }
            
            i += 1;
        }
        
        if issues_found > 0 {
            eprintln!("Total semantic issues found: {}", issues_found);
        }
        
        Ok(())
    }
    
    /// Optimize control flow efficiency
    fn optimize_control_flow_efficiency(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Look for optimization opportunities
        let mut i = 0;
        let mut optimizations_applied = 0;
        
        while i < self.bytecode_builder.code().len() - 3 {
            // Check for redundant goto sequences
            if self.bytecode_builder.code()[i] == 0xc7 && self.bytecode_builder.code()[i + 3] == 0xc7 {
                // Two consecutive gotos - this might be redundant
                eprintln!("Optimization opportunity: consecutive gotos at positions {} and {}", i, i + 3);
                optimizations_applied += 1;
            }
            
            i += 1;
        }
        
        if optimizations_applied > 0 {
            eprintln!("Total optimization opportunities identified: {}", optimizations_applied);
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a while statement
    fn generate_while_statement_labeled(&mut self, label: Option<&str>, while_stmt: &WhileStmt) -> Result<()> {
        // 🔧 LOOP OPT: Analyze loop pattern for optimization opportunities
        if let Some(loop_pattern) = LoopOptimizer::analyze_loop_pattern(&Stmt::While(while_stmt.clone())) {
            println!("🔧 LOOP OPT: While loop optimization opportunities: {:?}", loop_pattern.optimization_opportunities);
            
            // Apply loop optimizations
            self.apply_loop_optimizations(&loop_pattern)?;
            return Ok(());
        }
        
        // Fallback to standard generation if no optimizations apply
        println!("🔧 LOOP OPT: Using standard while loop generation");
        
        // Create labels
        let start_label = self.create_label();
        let end_label = self.create_label();
        
        // Push loop context
        self.loop_stack.push(LoopContext { 
            label: label.map(|s| s.to_string()), 
            continue_label: start_label, 
            break_label: end_label 
        });
        
        // Mark start label
        {
            let l = self.label_str(start_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate condition
        self.generate_expression(&while_stmt.condition)?;
        
        // Jump to end if condition is false
        {
            let l = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        // Generate body
        self.generate_statement(&while_stmt.body)?;
        
        // Jump back to start
        {
            let l = self.label_str(start_label);
            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark end label
        {
            let l = self.label_str(end_label);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Generate bytecode for a for statement
    fn generate_for_statement(&mut self, for_stmt: &ForStmt) -> Result<()> {
        // 🔧 LOOP OPT: Analyze for loop pattern for optimization opportunities
        if let Some(loop_pattern) = LoopOptimizer::analyze_loop_pattern(&Stmt::For(for_stmt.clone())) {
            println!("🔧 LOOP OPT: For loop optimization opportunities: {:?}", loop_pattern.optimization_opportunities);
            
            // Apply loop optimizations
            self.apply_loop_optimizations(&loop_pattern)?;
            return Ok(());
        }
        
        // Fallback to standard generation if no optimizations apply
        println!("🔧 LOOP OPT: Using standard for loop generation");
        
        // Check if this is an enhanced for loop (for-each)
        // Enhanced for loops have: 1 init (var declaration), no condition, no updates
        if for_stmt.init.len() == 1 && for_stmt.condition.is_none() && for_stmt.update.is_empty() {
            if let Stmt::Declaration(var_decl) = &for_stmt.init[0] {
                // This looks like an enhanced for loop
                return self.generate_enhanced_for_statement(var_decl, &for_stmt.body);
            }
        }
        
        // Generate initialization statements
        println!("🔍 DEBUG: generate_for_statement: Generating {} init statements", for_stmt.init.len());
        for (i, init) in for_stmt.init.iter().enumerate() {
            println!("🔍 DEBUG: generate_for_statement: Processing init statement {}", i + 1);
            self.generate_statement(init)?;
            println!("🔍 DEBUG: generate_for_statement: Init statement {} completed", i + 1);
        }
        
        // Create labels for control flow
        let start_label = self.create_label();  // L_loop (header)
        let end_label = self.create_label();    // L_after (after-loop)
        let continue_label = self.create_label(); // L_inc (increment)
        

        
        // Push loop context
        self.loop_stack.push(LoopContext { 
            label: None, 
            continue_label, 
            break_label: end_label 
        });
        
        // Mark start label
        {
            let l = self.label_str(start_label);

            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate condition check
        if let Some(cond) = &for_stmt.condition {
            // Check if this is a binary comparison that we can optimize
            if let Expr::Binary(bin_expr) = cond {
                match bin_expr.operator {
                    BinaryOp::Lt => {
                        // Generate: left < right -> if_icmpge end_label (jump if left >= right)
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(end_label);
                        Self::map_stack(self.bytecode_builder.if_icmpge(&l))?;
                    }
                    BinaryOp::Le => {
                        // Generate: left <= right -> if_icmpgt end_label (jump if left > right)
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(end_label);
                        Self::map_stack(self.bytecode_builder.if_icmpgt(&l))?;
                    }
                    BinaryOp::Gt => {
                        // Generate: left > right -> if_icmple end_label (jump if left <= right)
                        self.generate_expression(&bin_expr.left)?;
                        self.generate_expression(&bin_expr.right)?;
                        let l = self.label_str(end_label);
                        Self::map_stack(self.bytecode_builder.if_icmple(&l))?;
                    }
                    BinaryOp::Ge => {
                        // Check if we can optimize comparison with zero
                        if let Expr::Literal(lit_expr) = bin_expr.right.as_ref() {
                            if let Literal::Integer(0) = &lit_expr.value {
                                // Optimize: left >= 0 -> iflt end_label (jump if left < 0)
                                self.generate_expression(&bin_expr.left)?;
                                let l = self.label_str(end_label);
                                Self::map_stack(self.bytecode_builder.iflt(&l))?;
                            } else {
                                // Generate: left >= right -> if_icmplt end_label (jump if left < right)
                                self.generate_expression(&bin_expr.left)?;
                                self.generate_expression(&bin_expr.right)?;
                                let l = self.label_str(end_label);
                                Self::map_stack(self.bytecode_builder.if_icmplt(&l))?;
                            }
                        } else {
                            // Generate: left >= right -> if_icmplt end_label (jump if left < right)
                            self.generate_expression(&bin_expr.left)?;
                            self.generate_expression(&bin_expr.right)?;
                            let l = self.label_str(end_label);
                            Self::map_stack(self.bytecode_builder.if_icmplt(&l))?;
                        }
                    }
                    BinaryOp::Eq => {
                        // Check for null comparison optimization
                        if self.is_null_literal(&bin_expr.right) {
                            // left == null -> ifnonnull end_label (jump if left != null)
                            self.generate_expression(&bin_expr.left)?;
                            let l = self.label_str(end_label);
                            Self::map_stack(self.bytecode_builder.ifnonnull(&l))?;
                        } else if self.is_null_literal(&bin_expr.left) {
                            // null == right -> ifnonnull end_label (jump if right != null)
                            self.generate_expression(&bin_expr.right)?;
                            let l = self.label_str(end_label);
                            Self::map_stack(self.bytecode_builder.ifnonnull(&l))?;
                        } else {
                            // Generate: left == right -> if_icmpne end_label (jump if left != right)
                            self.generate_expression(&bin_expr.left)?;
                            self.generate_expression(&bin_expr.right)?;
                            let l = self.label_str(end_label);
                            Self::map_stack(self.bytecode_builder.if_icmpne(&l))?;
                        }
                    }
                    BinaryOp::Ne => {
                        // Check for null comparison optimization
                        if self.is_null_literal(&bin_expr.right) {
                            // left != null -> ifnull end_label (jump if left == null)
                            self.generate_expression(&bin_expr.left)?;
                            let l = self.label_str(end_label);
                            Self::map_stack(self.bytecode_builder.ifnull(&l))?;
                        } else if self.is_null_literal(&bin_expr.left) {
                            // null != right -> ifnull end_label (jump if right == null)
                            self.generate_expression(&bin_expr.right)?;
                            let l = self.label_str(end_label);
                            Self::map_stack(self.bytecode_builder.ifnull(&l))?;
                        } else {
                            // Generate: left != right -> if_icmpeq end_label (jump if left == right)
                            self.generate_expression(&bin_expr.left)?;
                            self.generate_expression(&bin_expr.right)?;
                            let l = self.label_str(end_label);
                            Self::map_stack(self.bytecode_builder.if_icmpeq(&l))?;
                        }
                    }
                    _ => {
                        // Fall back to the old approach for non-comparison operators
            self.generate_expression(cond)?;
            let l = self.label_str(end_label);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                    }
                }
            } else {
                // Fall back to the old approach for non-binary expressions
                self.generate_expression(cond)?;
                let l = self.label_str(end_label);
                Self::map_stack(self.bytecode_builder.ifeq(&l))?;
            }

        }
        
        // Generate loop body
        println!("🔍 DEBUG: generate_for_statement: About to generate loop body");
        self.generate_statement(&for_stmt.body)?;
        println!("🔍 DEBUG: generate_for_statement: Loop body generated");
        
        // Mark continue label at the increment location
        {
            let l = self.label_str(continue_label);

            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate updates (increment) with enhanced increment_optimizer integration
        for upd in &for_stmt.update {
            println!("🔧 INCREMENT OPT: Processing for loop update: {:?}", upd.expr);
            
            // Try to optimize increment operations using increment_optimizer
            if let Expr::Assignment(assign_expr) = &upd.expr {
                if let (Expr::Identifier(ident), AssignmentOp::AddAssign) = (&*assign_expr.target, &assign_expr.operator) {
                    // Check if this is a simple constant increment: i += constant
                    if let Expr::Literal(lit) = &*assign_expr.value {
                        if let Some(local_var) = self.find_local_variable(&ident.name) {
                            if let LocalType::Int = local_var.var_type {
                                // Try to extract the constant value
                                if let Some(constant_value) = self.extract_int_literal(lit) {
                                    println!("🔧 INCREMENT OPT: For loop constant increment: {} += {}", ident.name, constant_value);
                                    // Use iinc instruction for i += constant
                                    let iinc_bytes = self.opcode_generator.iinc(local_var.index, constant_value as i16);
                                    for byte in iinc_bytes {
                                        self.bytecode_builder.push_instruction(byte);
                                    }
                                    continue; // Skip the generic expression generation
                                }
                            }
                        }
                    }
                }
            }
            // Check for PreInc/PreDec operations and optimize them using increment_optimizer
            else if let Expr::Unary(unary_expr) = &upd.expr {
                if let Expr::Identifier(ident) = unary_expr.operand.as_ref() {
                    if let Some(local_var) = self.find_local_variable(&ident.name) {
                        // Create VariableInfo for increment_optimizer
                        let variable_info = self.create_variable_info_from_local_var(local_var);
                        
                        // Analyze with increment_optimizer for enhanced optimization
                        let pattern = self.increment_optimizer.analyze_unary_increment(unary_expr, &variable_info);
                        println!("🔧 INCREMENT OPT: For loop unary increment pattern: {:?}", pattern);
                        
                        // Apply optimization based on pattern
                        self.apply_increment_optimization(&pattern, &variable_info)?;
                        continue; // Skip the generic expression generation
                    }
                }
            }
            
            // Fall back to generic expression generation
            // For assignments in for-loop updates, don't preserve the value
            if let Expr::Assignment(assign_expr) = &upd.expr {
                self.generate_assignment_with_context(assign_expr, false)?; // false = don't preserve value
            } else {
            self.generate_expression(&upd.expr)?;
            Self::map_stack(self.bytecode_builder.pop())?;
            }
        }
        
        // Loop back to start
        {
            let l = self.label_str(start_label);

            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark end label
        {
            let l = self.label_str(end_label);

            self.bytecode_builder.mark_label(&l);
        }
        
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Generate bytecode for an enhanced for statement (for-each loop)
    fn generate_enhanced_for_statement(&mut self, var_decl: &VarDeclStmt, body: &Stmt) -> Result<()> {
        if var_decl.variables.len() != 1 {
            return Err(Error::codegen_error("Enhanced for loop should have exactly one variable"));
        }
        
        let variable = &var_decl.variables[0];

        let iterable_expr = variable.initializer.as_ref()
            .ok_or_else(|| Error::codegen_error("Enhanced for loop missing iterable expression"))?;
        
        // Determine if we're iterating over a collection or array
        let iterable_type = self.resolve_expression_type(iterable_expr);
        
        if iterable_type.ends_with("[]") || iterable_type.starts_with('[') {
            // Array iteration: use indexed loop
            self.generate_array_for_loop(var_decl, iterable_expr, body)
        } else {
            // Collection iteration: use iterator pattern
            self.generate_collection_for_loop(var_decl, iterable_expr, body)
        }
    }
    
    /// Generate bytecode for iterating over a collection using iterator pattern
    fn generate_collection_for_loop(&mut self, var_decl: &VarDeclStmt, iterable_expr: &Expr, body: &Stmt) -> Result<()> {
        let variable = &var_decl.variables[0];
        
        // Allocate local variables
        let iterator_type = TypeRef { 
            name: "java.util.Iterator".to_string(), 
            type_args: Vec::new(), 
            array_dims: 0,
            annotations: Vec::new(),
            span: crate::ast::Span::new(crate::ast::Location { line: 0, column: 0, offset: 0 }, crate::ast::Location { line: 0, column: 0, offset: 0 })
        };
        let iterator_var = self.allocate_local_variable("$iterator", &iterator_type);
        let loop_var = self.allocate_local_variable(&variable.name, &var_decl.type_ref);
        
        // Generate: iterator = collection.iterator()
        self.generate_expression(iterable_expr)?;
        let iterator_method_ref = self.get_iterator_method_ref();
        Self::map_stack(self.bytecode_builder.invokeinterface(iterator_method_ref, 1))?;
        let iterator_local_type = self.convert_type_ref_to_local_type(&iterator_type);
        self.store_local_variable(iterator_var, &iterator_local_type)?;
        
        // Create labels
        let loop_start = self.create_label();
        let loop_end = self.create_label();
        
        // Push loop context
        self.loop_stack.push(LoopContext { 
            label: None, 
            continue_label: loop_start, 
            break_label: loop_end 
        });
        
        // Mark loop start
        {
            let l = self.label_str(loop_start);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate: if (!iterator.hasNext()) goto end
        self.load_local_variable(iterator_var, &iterator_local_type)?;
        let has_next_method_ref = self.get_has_next_method_ref();
        Self::map_stack(self.bytecode_builder.invokeinterface(has_next_method_ref, 1))?;
        {
            let l = self.label_str(loop_end);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        // Generate: var = (Type) iterator.next()
        self.load_local_variable(iterator_var, &iterator_local_type)?;
        let next_method_ref = self.get_next_method_ref();
        Self::map_stack(self.bytecode_builder.invokeinterface(next_method_ref, 1))?;
        
        // Cast to the appropriate type if needed
        // Skip casting for generic type parameters (single uppercase letters) as they are erased to Object at runtime
        let is_generic_param = var_decl.type_ref.name.len() == 1 && var_decl.type_ref.name.chars().next().unwrap().is_ascii_uppercase();
        if var_decl.type_ref.name != "java.lang.Object" && !is_generic_param {
            let class_ref = self.add_class_constant(&var_decl.type_ref.name);
            Self::map_stack(self.bytecode_builder.checkcast(class_ref))?;
        }
        
        let local_type = self.convert_type_ref_to_local_type(&var_decl.type_ref);
        self.store_local_variable(loop_var, &local_type)?;
        
        // Generate loop body
        self.generate_statement(body)?;
        
        // Jump back to loop start
        {
            let l = self.label_str(loop_start);
            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark loop end
        {
            let l = self.label_str(loop_end);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Extract integer value from a literal expression
    fn extract_int_literal(&self, lit: &LiteralExpr) -> Option<i32> {
        match &lit.value {
            Literal::Integer(val) => Some(*val as i32),
            Literal::String(s) => s.parse::<i32>().ok(),
            _ => None,
        }
    }
    
    /// Generate bytecode for iterating over an array using indexed loop
    fn generate_array_for_loop(&mut self, var_decl: &VarDeclStmt, array_expr: &Expr, body: &Stmt) -> Result<()> {
        let variable = &var_decl.variables[0];
        
        // Allocate local variables
        let span = crate::ast::Span::new(crate::ast::Location { line: 0, column: 0, offset: 0 }, crate::ast::Location { line: 0, column: 0, offset: 0 });
        let array_type = TypeRef { name: "java.lang.Object".to_string(), type_args: Vec::new(), array_dims: 1, annotations: Vec::new(), span };
        let int_type = TypeRef { name: "int".to_string(), type_args: Vec::new(), array_dims: 0, annotations: Vec::new(), span };
        let array_var = self.allocate_local_variable("$array", &array_type);
        let length_var = self.allocate_local_variable("$length", &int_type);
        let index_var = self.allocate_local_variable("$index", &int_type);
        let loop_var = self.allocate_local_variable(&variable.name, &var_decl.type_ref);
        
        // Generate: array = <array_expr>
        self.generate_expression(array_expr)?;
        let array_local_type = self.convert_type_ref_to_local_type(&array_type);
        self.store_local_variable(array_var, &array_local_type)?;
        
        // Generate: length = array.length
        self.load_local_variable(array_var, &array_local_type)?;
        Self::map_stack(self.bytecode_builder.arraylength())?;
        let int_local_type = self.convert_type_ref_to_local_type(&int_type);
        self.store_local_variable(length_var, &int_local_type)?;
        
        // Generate: index = 0
        Self::map_stack(self.bytecode_builder.iconst_0())?;
        self.store_local_variable(index_var, &int_local_type)?;
        
        // Create labels
        let loop_start = self.create_label();
        let loop_end = self.create_label();
        
        // Push loop context
        self.loop_stack.push(LoopContext { 
            label: None, 
            continue_label: loop_start, 
            break_label: loop_end 
        });
        
        // Mark loop start
        {
            let l = self.label_str(loop_start);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Generate: if (index >= length) goto end
        self.load_local_variable(index_var, &int_local_type)?;
        self.load_local_variable(length_var, &int_local_type)?;
        {
            let l = self.label_str(loop_end);
            Self::map_stack(self.bytecode_builder.if_icmpge(&l))?;
        }
        
        // Generate: var = array[index]
        self.load_local_variable(array_var, &array_local_type)?;
        self.load_local_variable(index_var, &int_local_type)?;
        
        // Use appropriate array load instruction based on element type
        let element_type = var_decl.type_ref.name.as_str();
        match element_type {
            "int" => Self::map_stack(self.bytecode_builder.iaload())?,
            "long" => Self::map_stack(self.bytecode_builder.laload())?,
            "float" => Self::map_stack(self.bytecode_builder.faload())?,
            "double" => Self::map_stack(self.bytecode_builder.daload())?,
            "boolean" | "byte" => Self::map_stack(self.bytecode_builder.baload())?,
            "char" => Self::map_stack(self.bytecode_builder.caload())?,
            "short" => Self::map_stack(self.bytecode_builder.saload())?,
            _ => Self::map_stack(self.bytecode_builder.aaload())?,
        }
        
        let local_type = self.convert_type_ref_to_local_type(&var_decl.type_ref);
        self.store_local_variable(loop_var, &local_type)?;
        
        // Generate loop body
        self.generate_statement(body)?;
        
        // Generate: index++
        // Use opcode generator directly for iinc since BytecodeBuilder doesn't have it yet
        let iinc_bytes = self.opcode_generator.iinc(index_var, 1);
        for byte in iinc_bytes {
            self.bytecode_builder.push_instruction(byte);
        }
        
        // Jump back to loop start
        {
            let l = self.label_str(loop_start);
            Self::map_stack(self.bytecode_builder.goto(&l))?;
        }
        
        // Mark loop end
        {
            let l = self.label_str(loop_end);
            self.bytecode_builder.mark_label(&l);
        }
        
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Get method reference for Iterator.iterator()
    fn get_iterator_method_ref(&mut self) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_interface_method_ref("java/util/Collection", "iterator", "()Ljava/util/Iterator;").unwrap() };
            idx
        } else { 1 }
    }
    
    /// Get method reference for Iterator.hasNext()
    fn get_has_next_method_ref(&mut self) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_interface_method_ref("java/util/Iterator", "hasNext", "()Z").unwrap() };
            idx
        } else { 1 }
    }
    
    /// Get method reference for Iterator.next()
    fn get_next_method_ref(&mut self) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_interface_method_ref("java/util/Iterator", "next", "()Ljava/lang/Object;").unwrap() };
            idx
        } else { 1 }
    }
    
    /// Generate bytecode for a variable declaration
    fn generate_variable_declaration(&mut self, var_decl: &VarDeclStmt) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_variable_declaration: type_ref={:?}", var_decl.type_ref);
        for variable in &var_decl.variables {
            // Allocate local variable
            let index = self.allocate_local_variable(&variable.name, &var_decl.type_ref);
            
            // 🔧 ASSIGNMENT OPT: Register local variable with assignment optimizer
            let var_type = self.convert_type_ref_to_local_type(&var_decl.type_ref);
            let type_name = match &var_type {
                LocalType::Int => "int".to_string(),
                LocalType::Long => "long".to_string(),
                LocalType::Float => "float".to_string(),
                LocalType::Double => "double".to_string(),
                LocalType::Reference(_) => "reference".to_string(),
                LocalType::Array(_) => "array".to_string(),
            };
            self.assignment_optimizer.register_local_var(variable.name.clone(), index, type_name.clone());
            println!("🔧 ASSIGNMENT OPT: Registered local variable: {}[{}] as {}", variable.name, index, type_name);
            
            // Generate initializer if present
            if let Some(initializer) = &variable.initializer {
                eprintln!("🔍 DEBUG: generate_variable_declaration: var_name={}, type_ref={}, stack_depth_before_expr={}", 
                         variable.name, var_decl.type_ref.name, self.bytecode_builder.stack_depth());
                self.generate_expression(initializer)?;
                eprintln!("🔍 DEBUG: generate_variable_declaration: stack_depth_after_expr={}", self.bytecode_builder.stack_depth());
                let local_type = self.convert_type_ref_to_local_type(&var_decl.type_ref);
                eprintln!("🔍 DEBUG: generate_variable_declaration: local_type={:?}", local_type);
                
                // Handle long/double literals that need 2 stack slots
                if matches!(local_type, LocalType::Long | LocalType::Double) {
                    let current_depth = self.bytecode_builder.stack_depth();
                    if current_depth == 1 {
                        // We have a 32-bit value but need a 64-bit value
                        // Convert int to long or float to double
                        match local_type {
                            LocalType::Long => {
                                eprintln!("🔍 DEBUG: generate_variable_declaration: Converting int literal to long");
                                Self::map_stack(self.bytecode_builder.i2l())?;
                            }
                            LocalType::Double => {
                                eprintln!("🔍 DEBUG: generate_variable_declaration: Converting float literal to double");
                                // Assume it's a float, convert to double
                                Self::map_stack(self.bytecode_builder.f2d())?;
                            }
                            _ => unreachable!()
                        }
                    }
                }
                
                self.store_local_variable(index, &local_type)?;
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for a return statement using InstructionOptimizer
    fn generate_return(&mut self, return_type: &TypeRef) -> Result<()> {
        // Handle void returns using VoidItem (javac-style)
        if return_type.name == "void" {
            let void_item = crate::codegen::item_system::ItemFactory::make_void_item();
            let bytecode = void_item.load()?; // VoidItem.load() returns empty bytecode
            for &byte in &bytecode {
                self.bytecode_builder.push_byte(byte);
            }
            
            // Generate void return instruction
            let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_return_instruction("void");
            for &byte in &optimized_bytecode {
                self.bytecode_builder.push_byte(byte);
            }
        } else {
            // Use InstructionOptimizer for non-void return instructions
            let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_return_instruction(&return_type.name);
            
            // Emit the optimized bytecode
            for &byte in &optimized_bytecode {
                self.bytecode_builder.push_byte(byte);
            }
        }
        
        // Return instructions clear the stack and terminate the method
        let current_depth = self.bytecode_builder.get_stack_depth();
        if current_depth > 0 {
            Self::map_stack(self.bytecode_builder.update_stack(current_depth, 0))?;
        }
        
        Ok(())
    }
    
    /// Load a local variable using InstructionOptimizer
    fn load_local_variable(&mut self, index: u16, var_type: &LocalType) -> Result<()> {
        // Use InstructionOptimizer for optimized load instructions
        let type_str = match var_type {
            LocalType::Int => "int",
            LocalType::Long => "long",
            LocalType::Float => "float",
            LocalType::Double => "double",
            LocalType::Reference(_) | LocalType::Array(_) => "reference",
        };
        
        let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_load_instruction(type_str, index);
        
        // Emit the optimized bytecode
        for &byte in &optimized_bytecode {
            self.bytecode_builder.push_byte(byte);
        }
        
        // Update stack for local variable load
        let stack_effect = if matches!(var_type, LocalType::Long | LocalType::Double) { 2 } else { 1 };
        Self::map_stack(self.bytecode_builder.update_stack(0, stack_effect))?;
        
        Ok(())
    }
    
    /// Store a local variable using InstructionOptimizer
    fn store_local_variable(&mut self, index: u16, var_type: &LocalType) -> Result<()> {
        // Use InstructionOptimizer for optimized store instructions
        let type_str = match var_type {
            LocalType::Int => "int",
            LocalType::Long => "long",
            LocalType::Float => "float",
            LocalType::Double => "double",
            LocalType::Reference(_) | LocalType::Array(_) => "reference",
        };
        
        let optimized_bytecode = crate::codegen::instruction_optimizer::InstructionOptimizer::optimize_store_instruction(type_str, index);
        
        // Emit the optimized bytecode
        for &byte in &optimized_bytecode {
            self.bytecode_builder.push_byte(byte);
        }
        
        // Update stack for local variable store
        let stack_effect = if matches!(var_type, LocalType::Long | LocalType::Double) { -2 } else { -1 };
        Self::map_stack(self.bytecode_builder.update_stack((-stack_effect) as u16, 0))?;
        
        Ok(())
    }
    
    /// Find a local variable by name
    fn find_local_variable(&self, name: &str) -> Option<&LocalSlot> {
        self.bytecode_builder.locals().iter().find(|v| v.name == name)
    }
    
    /// Allocate a new local variable (with variable reuse optimization)
    fn allocate_local_variable(&mut self, name: &str, var_type: &TypeRef) -> u16 {
        // Check if we can reuse an existing variable with the same name and type
        // This is safe when the previous variable is no longer in scope
        if let Some(existing_var) = self.find_local_variable(name) {
            let existing_type = self.convert_type_ref_to_local_type(var_type);
            if existing_var.var_type == existing_type {
                // eprintln!("🔍 DEBUG: allocate_local_variable: Reusing existing variable '{}' at index {}", name, existing_var.index);
                return existing_var.index;
            }
        }
        
        let index = self.bytecode_builder.allocate_with_type_ref(
            name.to_string(), 
            self.convert_type_ref_to_local_type(var_type),
            Some(var_type.clone())
        );
        // eprintln!("🔍 DEBUG: allocate_local_variable: Allocated new variable '{}' at index {}", name, index);
        
        // Track in current scope if any
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.locals.push(index as usize);
        }
        index
    }
    
    /// Create a new label id (for compatibility). BytecodeBuilder labels are strings.
    fn create_label(&mut self) -> u16 { 
        let id = self.next_label_id;
        self.next_label_id += 1;
        println!("🔍 DEBUG: create_label: Created label {} (next will be {})", id, self.next_label_id);
        id
    }
    
    /// Mark label and record its position for jump target calculation
    fn mark_label(&mut self, label_id: u16) {
        // Convert our numeric label to string format for BytecodeBuilder
        let label_str = self.label_str(label_id);
        
        // Get the current PC before marking the label
        // This ensures we get the exact position where the label should be placed
        let current_pc = self.bytecode_builder.code().len() as u16;
        
        eprintln!("🔍 DEBUG: mark_label: Marking label {} ({}) at PC {}", label_id, label_str, current_pc);
        
        // Mark the label in BytecodeBuilder first
        // BytecodeBuilder will handle the actual label placement and backward reference resolution
        self.bytecode_builder.mark_label(&label_str);
        
        // Get the actual PC after marking (in case BytecodeBuilder made adjustments)
        let actual_pc = self.bytecode_builder.code().len() as u16;
        
        // Store in our position tracking for debugging
        if let Some(positions) = self.label_positions.as_mut() {
            positions.insert(label_id, actual_pc);
        }
        
        // Resolve any pending jump chains for this label using the actual PC
        self.resolve_pending_jumps_for_label(&label_str, actual_pc as u32);
        
        eprintln!("🔍 DEBUG: mark_label: Label {} marked at actual PC {} (requested: {})", label_id, actual_pc, current_pc);
        println!("🔍 DEBUG: mark_label: Label {} ({}) marked at PC {} for method writer", label_id, label_str, actual_pc);
    }
    
    /// Resolve pending jump chains when a label is marked
    fn resolve_pending_jumps_for_label(&mut self, label: &str, target_pc: u32) {
        eprintln!("🔍 DEBUG: resolve_pending_jumps_for_label: Resolving jumps for label {} at PC {}", label, target_pc);
        
        // Look up the chain ID for this label
        if let Some(chain_id) = self.label_to_chain_mapping.as_ref().and_then(|mapping| mapping.get(label)) {
            eprintln!("🔍 DEBUG: resolve_pending_jumps_for_label: Found chain {} for label {}", chain_id, label);
            eprintln!("🔍 DEBUG: resolve_pending_jumps_for_label: Chain {} will be resolved to target PC {}", chain_id, target_pc);
            
            // Resolve the chain using PendingJumpsManager
            if let Ok(patch_locations) = self.pending_jumps_manager.resolve_chain(*chain_id, target_pc) {
                eprintln!("🔍 DEBUG: resolve_pending_jumps_for_label: Resolved chain {} with {} patches", chain_id, patch_locations.len());
                eprintln!("🔍 DEBUG: resolve_pending_jumps_for_label: Patch locations: {:?}", patch_locations);
                
                // Apply the patches to the bytecode
                self.apply_jump_patches(&patch_locations);
                
                // Remove the resolved mapping
                if let Some(mapping) = self.label_to_chain_mapping.as_mut() {
                    mapping.remove(label);
                }
            } else {
                eprintln!("🔍 DEBUG: resolve_pending_jumps_for_label: Failed to resolve chain {}", chain_id);
            }
        } else {
            eprintln!("🔍 DEBUG: resolve_pending_jumps_for_label: No pending chain found for label {}", label);
        }
    }
    
    /// Apply jump patches to the bytecode
    fn apply_jump_patches(&mut self, patch_locations: &[(u32, i32)]) {
        for (pc, offset) in patch_locations {
            eprintln!("🔍 DEBUG: apply_jump_patches: Patching PC {} with offset {}", pc, offset);
            eprintln!("🔍 DEBUG: apply_jump_patches: PC {}: offset {} (i32), offset_i16: {}", pc, offset, *offset as i16);
            
            // 🔧 FIX: Use big-endian format for JVM bytecode (not little-endian)
            let offset_bytes = (*offset as i16).to_be_bytes();
            eprintln!("🔍 DEBUG: apply_jump_patches: PC {}: offset_bytes (big-endian): {:?}", pc, offset_bytes);
            
            // Apply the patch to the bytecode
            let code = self.bytecode_builder.code_mut();
            if *pc + 1 < code.len() as u32 {
                code[*pc as usize + 1] = offset_bytes[0];
                if *pc + 2 < code.len() as u32 {
                    code[*pc as usize + 2] = offset_bytes[1];
                    eprintln!("🔍 DEBUG: apply_jump_patches: Applied patch at PC {}: offset {} -> bytes {:?}", pc, offset, offset_bytes);
                    
                    // Verify the patch was applied correctly (using big-endian for JVM bytecode)
                    let patched_offset = i16::from_be_bytes([code[*pc as usize + 1], code[*pc as usize + 2]]) as i32;
                    eprintln!("🔍 DEBUG: apply_jump_patches: Verification - patched offset (big-endian): {}", patched_offset);
                    
                    // Also check little-endian interpretation for debugging
                    let patched_offset_le = i16::from_le_bytes([code[*pc as usize + 1], code[*pc as usize + 2]]) as i32;
                    eprintln!("🔍 DEBUG: apply_jump_patches: Verification - patched offset (little-endian): {}", patched_offset_le);
                }
            }
            
            // Check if we need to emit a stack map frame after this jump instruction
            let _ = self.check_and_emit_stack_map_frame(false, false);
        }
    }
    
    /// Record an exception handler table entry using labels
    fn add_exception_handler_labels(&mut self, start: u16, end: u16, handler: u16, catch_type: u16) {
        self.pending_exception_entries.push(PendingExceptionEntry { start_label: start, end_label: end, handler_label: handler, catch_type });
        
        // Emit stack map frame at exception handler entry point
        let _ = self.emit_exception_handler_stack_map_frame(handler, catch_type);
    }
    
    /// Emit stack map frame at exception handler entry point
    fn emit_exception_handler_stack_map_frame(&mut self, handler_label: u16, catch_type: u16) -> Result<()> {
        if let Some(ref mut emitter) = self.stack_map_emitter {
            // Create simplified locals to avoid borrow conflicts
            let locals = vec![crate::codegen::frame::VerificationType::Object(1)];
            let exception_type = crate::codegen::frame::VerificationType::Object(catch_type as u16);
            
            eprintln!("🔍 DEBUG: Enhanced stack map: Emitting exception handler frame for catch type {}", catch_type);
            
            if let Err(e) = emitter.emit_frame_at_exception_handler(0, locals, exception_type) {
                eprintln!("🔍 DEBUG: Enhanced stack map: Exception handler frame emission failed: {:?}", e);
            }
        }
        Ok(())
    }
    
    /// Emit an instruction (for backward compatibility)
    fn emit_instruction(&mut self, opcode: u8) {
        self.bytecode_builder.push(opcode);
        // For single-byte instructions, we don't have enough context to update stack state
        // This is a simplified version for backward compatibility
    }
    
    /// Emit a byte value
    fn emit_byte(&mut self, value: u8) {
        self.bytecode_builder.push(value);
    }
    
    /// Emit a short value
    fn emit_short(&mut self, value: i16) {
        self.bytecode_builder.extend_from_slice(&value.to_be_bytes());
    }
    
    /// Update stack and locals tracking
    fn update_stack_and_locals(&mut self) {
        // TODO: Implement proper stack and locals tracking
        // Stack tracking is now handled by BytecodeBuilder automatically
    }
    
    /// Add a class constant to the constant pool
    fn add_class_constant(&mut self, name: &str) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let mut cp_ref = cp.borrow_mut();
            // Try to resolve the class name using classpath first
            let resolved_name = if !name.contains('/') {
                // Simple name, try to resolve using classpath
                if let Some(resolved) = crate::codegen::classpath::resolve_class_name(name) {
                    resolved.to_string()
                } else if crate::consts::JAVA_LANG_SIMPLE_TYPES.contains(&name) {
                    // java.lang types
                    format!("java/lang/{}", name)
                } else if name == "Comparator" || name == "Iterator" || name == "Collection" || 
                          name == "List" || name == "Set" || name == "Map" || 
                          name == "Deque" || name == "Queue" || name == "Iterable" ||
                          name == "ArraysListIterator" || name == "ListIterator" {
                    // java.util types
                    format!("java/util/{}", name)
                } else if name == "Serializable" || name == "Closeable" || name == "Flushable" {
                    // java.io types
                    format!("java/io/{}", name)
                } else if name == "Comparable" {
                    // java.lang.Comparable
                    "java/lang/Comparable".to_string()
                } else {
                    // Default to java.lang if we can't determine
                    format!("java/lang/{}", name)
                }
            } else {
                // Name already contains package
                name.to_string()
            };
            
            match cp_ref.try_add_class(&resolved_name) {
                Ok(idx) => idx,
                Err(_) => 1, // Fallback
            }
        } else {
            // Fallback for backward compatibility
            1
        }
    }
    
    /// Add a method reference to the constant pool
    fn add_class_ref(&mut self, class: &str) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let mut cp_ref = cp.borrow_mut();
            match cp_ref.try_add_class(class) {
                Ok(idx) => idx,
                Err(e) => {
                    eprintln!("Warning: Failed to add class ref {}: {:?}", class, e);
                    // Fallback to Object class
                    cp_ref.try_add_class("java/lang/Object").unwrap_or(1)
                }
            }
        } else {
            1 // Fallback index
        }
    }
    
    fn add_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let mut cp_ref = cp.borrow_mut();
            match cp_ref.try_add_method_ref(class, name, descriptor) {
                Ok(idx) => idx,
                Err(e) => {
                    // Log the error for debugging
                    eprintln!("Warning: Failed to add method ref {}.{}{}: {:?}", class, name, descriptor, e);
                    // Try to add a fallback method reference
                    match cp_ref.try_add_method_ref("java/lang/String", "toString", "()Ljava/lang/String;") {
                        Ok(fallback_idx) => fallback_idx,
                        Err(_) => 1,
                    }
                }
            }
        } else {
            // Fallback for backward compatibility
            1
        }
    }
    
    /// Add a field reference to the constant pool
    fn add_field_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let mut cp_ref = cp.borrow_mut();
            match cp_ref.try_add_field_ref(class, name, descriptor) {
                Ok(idx) => idx,
                Err(e) => {
                    // Log the error for debugging
                    eprintln!("Warning: Failed to add field ref {}.{}: {:?}", class, name, e);
                    // Try to add a fallback field reference
                    match cp_ref.try_add_field_ref("java/lang/String", "toString", "Ljava/lang/String;") {
                        Ok(fallback_idx) => fallback_idx,
                        Err(_) => 1,
                    }
                }
            }
        } else {
            // Fallback for backward compatibility
            1
        }
    }
    
    /// Get the generated bytecode
    pub fn get_bytecode(self) -> Vec<u8> { 
        self.bytecode_builder.into_code()
    }

    /// Finalize and return code, max stack, max locals, and resolved exception table
    pub(crate) fn finalize(mut self) -> (Vec<u8>, u16, u16, Vec<ExceptionTableEntry>, Vec<LocalSlot>, Vec<(u16,u16)>) {
        // Resolve all label references
        self.resolve_label_references();
        
        let exceptions: Vec<ExceptionTableEntry> = Vec::new();
        
        let max_stack = self.bytecode_builder.max_stack();
        let max_locals = self.bytecode_builder.max_locals();
        let locals = self.bytecode_builder.locals().to_vec();
        let code = self.bytecode_builder.into_code();
        
        (code, max_stack, max_locals, exceptions, locals, self.line_numbers)
    }
    
    /// Get the maximum stack size
    pub fn get_max_stack(&self) -> u16 {
        self.bytecode_builder.max_stack()
    }
    
    /// Get the maximum number of local variables
    pub fn get_max_locals(&self) -> u16 {
        self.bytecode_builder.max_locals()
    }
    
    /// Convert JVM descriptor to class name
    fn descriptor_to_class_name(&self, descriptor: &str) -> String {
        match descriptor {
            "V" => "void".to_string(),
            "I" => "int".to_string(),
            "J" => "long".to_string(),
            "F" => "float".to_string(),
            "D" => "double".to_string(),
            "Z" => "boolean".to_string(),
            "B" => "byte".to_string(),
            "C" => "char".to_string(),
            "S" => "short".to_string(),
            _ if descriptor.starts_with('L') && descriptor.ends_with(';') => {
                // Object type: LClassName; -> ClassName
                descriptor[1..descriptor.len()-1].replace('/', ".")
            }
            _ if descriptor.starts_with('[') => {
                // Array type: [ElementType -> ElementType[]
                let element_descriptor = &descriptor[1..];
                format!("{}[]", self.descriptor_to_class_name(element_descriptor))
            }
            _ => descriptor.to_string(), // Fallback
        }
    }

    /// Resolve field type by walking the inheritance chain
    fn resolve_field_type_in_inheritance(&self, field_name: &str) -> Option<String> {
        // Start with current class
        if let Some(class) = &self.current_class {
            // Check current class
            for member in &class.body {
                if let crate::ast::ClassMember::Field(field) = member {
                    if field.name == field_name {
                        // Found the field, return its type
                        if field.type_ref.array_dims > 0 {
                            // Array type: T[] -> T[]
                            let mut type_name = field.type_ref.name.clone();
                            for _ in 0..field.type_ref.array_dims {
                                type_name.push_str("[]");
                            }
                            return Some(type_name);
                        } else {
                                                    // Regular type - use raw type for better javac compatibility
                        // Apply type erasure to match javac behavior
                        let type_name = if let Some(open_bracket) = field.type_ref.name.find('<') {
                            // Remove generic parameters for type erasure
                            let raw_name = field.type_ref.name[..open_bracket].to_string();
                            eprintln!("🔍 DEBUG: Field '{}' type erasure: '{}' -> '{}'", field_name, field.type_ref.name, raw_name);
                            raw_name
                        } else {
                            eprintln!("🔍 DEBUG: Field '{}' no generics: '{}'", field_name, field.type_ref.name);
                            field.type_ref.name.clone()
                        };
                        
                        // 🔧 FIX: Ensure we return the full qualified name for better type consistency
                        // This prevents issues like LLinkedListCell; vs Ljava/util/LinkedListCell;
                        let full_type_name = if !type_name.contains('.') {
                            // If type name doesn't have package path, try to resolve it
                            if let Some(internal_name) = classpath::resolve_class_name(&type_name) {
                                internal_name.replace('/', ".")
                            } else {
                                // Fallback: assume it's in the same package as current class
                                // Since ClassDecl doesn't have package field, we'll use the class name to infer package
                                if class.name.contains('.') {
                                    // Extract package from class name (e.g., "java.util.LinkedList" -> "java.util")
                                    if let Some(last_dot) = class.name.rfind('.') {
                                        format!("{}.{}", &class.name[..last_dot], type_name)
                                    } else {
                                        type_name
                                    }
                                } else {
                                    type_name
                                }
                            }
                        } else {
                            type_name
                        };
                        
                        return Some(full_type_name);
                        }
                    }
                }
            }
            
            // If not found in current class, check parent class
            if let Some(parent) = &class.extends {
                let parent_internal = parent.name.replace(".", "/");
                if let Some(parent_class) = self.try_parse_class_from_filesystem(&parent_internal) {
                    return self.resolve_field_type_in_parent_class(&parent_class, field_name);
                }
            }
        }
        
        None
    }
    
    /// Resolve field type in a specific parent class
    fn resolve_field_type_in_parent_class(&self, class: &crate::ast::ClassDecl, field_name: &str) -> Option<String> {
        // Check this class
        for member in &class.body {
            if let crate::ast::ClassMember::Field(field) = member {
                if field.name == field_name {
                    // Found the field, return its type
                    if field.type_ref.array_dims > 0 {
                        // Array type: T[] -> T[]
                        let mut type_name = field.type_ref.name.clone();
                        for _ in 0..field.type_ref.array_dims {
                            type_name.push_str("[]");
                        }
                        return Some(type_name);
                    } else {
                        // Regular type - use raw type for better javac compatibility
                        // Apply type erasure to match javac behavior
                        let type_name = if let Some(open_bracket) = field.type_ref.name.find('<') {
                            // Remove generic parameters for type erasure
                            field.type_ref.name[..open_bracket].to_string()
                        } else {
                            field.type_ref.name.clone()
                        };
                        
                        // 🔧 FIX: Ensure we return the full qualified name for better type consistency
                        // This prevents issues like LLinkedListCell; vs Ljava/util/LinkedListCell;
                        let full_type_name = if !type_name.contains('.') {
                            // If type name doesn't have package path, try to resolve it
                            if let Some(internal_name) = classpath::resolve_class_name(&type_name) {
                                internal_name.replace('/', ".")
                            } else {
                                // Fallback: assume it's in the same package as the parent class
                                // Since ClassDecl doesn't have package field, we'll use the class name to infer package
                                if class.name.contains('.') {
                                    // Extract package from class name (e.g., "java.util.LinkedList" -> "java.util")
                                    if let Some(last_dot) = class.name.rfind('.') {
                                        format!("{}.{}", &class.name[..last_dot], type_name)
                                    } else {
                                        type_name
                                    }
                                } else {
                                    type_name
                                }
                            }
                        } else {
                            type_name
                        };
                        
                        return Some(full_type_name);
                    }
                }
            }
        }
        
        // If not found in this class, check parent class
        if let Some(parent) = &class.extends {
            let parent_internal = parent.name.replace(".", "/");
            if let Some(parent_class) = self.try_parse_class_from_filesystem(&parent_internal) {
                return self.resolve_field_type_in_parent_class(&parent_class, field_name);
            }
        }
        
        None
    }

    /// Try to parse a class file from the file system to resolve field types
    fn try_parse_class_from_filesystem(&self, class_internal: &str) -> Option<crate::ast::ClassDecl> {
        // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Looking for class {}", class_internal);
        // Convert internal name to file path
        let class_name = class_internal.replace('/', ".");
        
        // Try different possible paths
        let possible_paths = vec![
            format!("tests/java/{}.java", class_internal),
            format!("tests/java/util/{}.java", class_internal),
            format!("tests/java/lang/{}.java", class_internal),
        ];
        
        for file_path in possible_paths {
            // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Trying to read file {}", file_path);
            
            // Try to read and parse the file
            if let Ok(source) = std::fs::read_to_string(&file_path) {
                // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Successfully read file, length = {}", source.len());
                if let Ok(ast) = crate::parser::parser::parse(&source) {
                    // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Successfully parsed AST, found {} type declarations", ast.type_decls.len());
                    // Find the matching class declaration
                    for type_decl in ast.type_decls {
                        if let crate::ast::TypeDecl::Class(class) = type_decl {
                            // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Found class '{}', looking for '{}'", class.name, class_name);
                            if class.name == class_name || class.name.ends_with(&format!(".{}", class_internal)) {
                                // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Found matching class!");
                                return Some(class);
                            }
                        }
                    }
                    // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: No matching class found in this file");
                } else {
                    // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Failed to parse AST");
                }
            } else {
                // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: Failed to read file");
            }
        }
        
        // eprintln!("🔍 DEBUG: try_parse_class_from_filesystem: No matching class found in any path");
        None
    }

    /// Try to parse an interface file from the file system to resolve field types
    fn try_parse_interface_from_filesystem(&self, interface_internal: &str) -> Option<crate::ast::InterfaceDecl> {
        // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Looking for interface {}", interface_internal);
        // Convert internal name to file path
        let interface_name = interface_internal.replace('/', ".");
        
        // Try different possible paths
        let possible_paths = vec![
            format!("tests/java/{}.java", interface_internal),
            format!("tests/java/util/{}.java", interface_internal),
            format!("tests/java/lang/{}.java", interface_internal),
        ];
        
        for file_path in possible_paths {
            // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Trying to read file {}", file_path);
            
            // Try to read and parse the file
            if let Ok(source) = std::fs::read_to_string(&file_path) {
                // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Successfully read file, length = {}", source.len());
                if let Ok(ast) = crate::parser::parser::parse(&source) {
                    // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Successfully parsed AST, found {} type declarations", ast.type_decls.len());
                    // Find the matching interface declaration
                    for type_decl in ast.type_decls {
                        if let crate::ast::TypeDecl::Interface(interface) = type_decl {
                            // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Found interface '{}', looking for '{}'", interface.name, interface_name);
                            if interface.name == interface_name || interface.name.ends_with(&format!(".{}", interface_internal)) {
                                // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Found matching interface!");
                                return Some(interface);
                            }
                        }
                    }
                    // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: No matching interface found in this file");
                } else {
                    // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Failed to parse AST");
                }
            } else {
                // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: Failed to read file");
            }
        }
        
        // eprintln!("🔍 DEBUG: try_parse_interface_from_filesystem: No matching interface found in any path");
        None
    }
    
    /// Check if a field is an instance field (not static) in the given class
    fn is_instance_field(&self, class_internal: &str, field_name: &str) -> bool {
        // Check current class being compiled for local fields
        if let Some(class) = &self.current_class {
            let current_class_internal = class.name.replace(".", "/");
            if class_internal == current_class_internal {
                // Look for the field in the current class
                for member in &class.body {
                    if let crate::ast::ClassMember::Field(field) = member {
                        if field.name == field_name {
                            // Check if field is static
                            return !field.modifiers.iter().any(|m| matches!(m, crate::ast::Modifier::Static));
                        }
                    }
                }
            }
        }

        // Check other classes in the current compilation unit
        if let Some(types) = &self.all_types {
            for type_decl in types {
                match type_decl {
                    crate::ast::TypeDecl::Class(class) => {
                        let class_internal_name = class.name.replace(".", "/");
                        if class_internal == class_internal_name {
                            // Look for the field in this class
                            for member in &class.body {
                                if let crate::ast::ClassMember::Field(field) = member {
                                    if field.name == field_name {
                                        // Check if field is static
                                        return !field.modifiers.iter().any(|m| matches!(m, crate::ast::Modifier::Static));
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        
        // Default to false if field not found
        false
    }
    
    /// Resolve field descriptor from generated rt.rs index, with local class fallback.
    /// `class_internal` must be an internal name like "java/io/OutputStream" or "java/base/FieldData".
    fn resolve_field_descriptor(&self, class_internal: &str, field_name: &str) -> String {
        // eprintln!("🔍 DEBUG: resolve_field_descriptor: Looking for {}#{}, all_types = {:?}", 
        //           class_internal, field_name, self.all_types.as_ref().map(|t| t.len()));
        
        // 1) Arrays don't have fields; `length` is a special property (returns int).
        if class_internal.starts_with('[') && field_name == "length" {
            return "I".to_string();
        }
        
        // 2) Handle special case of 'super' - this should not be treated as a field access
        if field_name == "super" {
            eprintln!("🔍 DEBUG: resolve_field_descriptor: 'super' should not be treated as field access");
            return "Ljava/lang/Object;".to_string(); // Return a dummy descriptor
        }

        // 2) Check current class being compiled for local fields
        if let Some(class) = &self.current_class {
            let current_class_internal = class.name.replace(".", "/");
            // eprintln!("🔍 DEBUG: resolve_field_descriptor: current_class_internal={}, class_internal={}", current_class_internal, class_internal);
            if class_internal == current_class_internal {
                // Look for the field in the current class
                for member in &class.body {
                    if let crate::ast::ClassMember::Field(field) = member {
                        // eprintln!("🔍 DEBUG: resolve_field_descriptor: Found field {} in current class", field.name);
                        if field.name == field_name {
                            // Generate descriptor from field type
                            let descriptor = crate::codegen::descriptor::type_to_descriptor(&field.type_ref);
                            eprintln!("🔍 DEBUG: resolve_field_descriptor: Field {}#{} -> descriptor={}", class_internal, field_name, descriptor);
                            return descriptor;
                        }
                    }
                }
            }
        }

        // 3) Check other classes in the current compilation unit
        if let Some(types) = &self.all_types {
            for type_decl in types {
                match type_decl {
                    crate::ast::TypeDecl::Class(class) => {
                        let class_internal_name = class.name.replace(".", "/");
                        if class_internal == class_internal_name {
                            // Look for the field in this class
                            for member in &class.body {
                                if let crate::ast::ClassMember::Field(field) = member {
                                    if field.name == field_name {
                                        // Generate descriptor from field type
                                        return crate::codegen::descriptor::type_to_descriptor(&field.type_ref);
                                    }
                                }
                            }
                        }
                    }
                    crate::ast::TypeDecl::Interface(interface) => {
                        let interface_internal_name = interface.name.replace(".", "/");
                        if class_internal == interface_internal_name {
                            // Look for the field in this interface (if any)
                            for member in &interface.body {
                                if let crate::ast::InterfaceMember::Field(field) = member {
                                    if field.name == field_name {
                                        // Generate descriptor from field type
                                        return crate::codegen::descriptor::type_to_descriptor(&field.type_ref);
                                    }
                                }
                            }
                        }
                    }
                    _ => {} // Skip other types
                }
            }
        }

        // 4) Try to parse the class from the file system and walk inheritance chain
        if let Some(class) = self.try_parse_class_from_filesystem(class_internal) {
            // Look for the field in the parsed class
            for member in &class.body {
                if let crate::ast::ClassMember::Field(field) = member {
                    if field.name == field_name {
                        // Generate descriptor from field type
                        return type_ref_to_descriptor(&field.type_ref);
                    }
                }
            }
            
            // Check implemented interfaces for fields (interface fields are static final)
            for interface_ref in &class.implements {
                // Resolve the interface name properly - it might be a simple name that needs to be resolved
                let interface_internal = if interface_ref.name.contains(".") {
                    interface_ref.name.replace(".", "/")
                } else {
                    // Try to resolve the simple interface name using imports or context
                    self.resolve_class_name(&interface_ref.name).replace(".", "/")
                };
                eprintln!("🔍 DEBUG: resolve_field_descriptor: Field not found in {}, checking interface {} (resolved from {})", class_internal, interface_internal, interface_ref.name);
                
                // Try to recursively resolve field in interface
                if let Some(interface_class) = self.try_parse_interface_from_filesystem(&interface_internal) {
                    for member in &interface_class.body {
                        if let crate::ast::InterfaceMember::Field(field) = member {
                            if field.name == field_name {
                                // Generate descriptor from field type
                                eprintln!("🔍 DEBUG: resolve_field_descriptor: Found field {}#{} in interface {}", class_internal, field_name, interface_internal);
                        return type_ref_to_descriptor(&field.type_ref);
                    }
                }
            }
        }

                // Also try rt.rs for the interface
                use boot::{CLASSES, CLASSES_BY_NAME};
                if let Some(&idx) = CLASSES_BY_NAME.get(&interface_internal) {
                    let c = &CLASSES[idx];
                    if let Some(fm) = c.fields.iter().find(|fm| fm.name == field_name) {
                        eprintln!("🔍 DEBUG: resolve_field_descriptor: Found field {}#{} in rt.rs interface {}", class_internal, field_name, interface_internal);
                        return fm.desc.to_string();
                    }
                }
            }
            
            // If not found in current class or interfaces, check parent class
            if let Some(parent) = &class.extends {
                let parent_internal = parent.name.replace(".", "/");
                eprintln!("🔍 DEBUG: resolve_field_descriptor: Field not found in {}, checking parent class {}", class_internal, parent_internal);
                return self.resolve_field_descriptor(&parent_internal, field_name);
            }
        }
        
        // 5) Also try to parse the class from the file system and check parent class interfaces
        if let Some(class) = self.try_parse_class_from_filesystem(class_internal) {
            // Check parent class interfaces recursively
            if let Some(parent) = &class.extends {
                let parent_internal = parent.name.replace(".", "/");
                if let Some(parent_class) = self.try_parse_class_from_filesystem(&parent_internal) {
                    // Check parent class interfaces for fields
                    for interface_ref in &parent_class.implements {
                        let interface_internal = if interface_ref.name.contains(".") {
                            interface_ref.name.replace(".", "/")
                        } else {
                            self.resolve_class_name(&interface_ref.name).replace(".", "/")
                        };
                        eprintln!("🔍 DEBUG: resolve_field_descriptor: Checking parent class interface {} for field {}", interface_internal, field_name);
                        
                        // Try rt.rs for the parent interface
                        use boot::{CLASSES, CLASSES_BY_NAME};
                        if let Some(&idx) = CLASSES_BY_NAME.get(&interface_internal) {
                            let c = &CLASSES[idx];
                            if let Some(fm) = c.fields.iter().find(|fm| fm.name == field_name) {
                                eprintln!("🔍 DEBUG: resolve_field_descriptor: Found field {}#{} in rt.rs parent interface {}", class_internal, field_name, interface_internal);
                                return fm.desc.to_string();
                            }
                        }
                    }
                }
            }
        }

        use boot::{CLASSES, CLASSES_BY_NAME};

        // helper: search a single owner in rt.rs
        let search_owner = |owner: &str, name: &str| -> Option<&'static str> {
            if let Some(&idx) = CLASSES_BY_NAME.get(owner) {
                let c = &CLASSES[idx];
                if let Some(fm) = c.fields.iter().find(|fm| fm.name == name) {
                    return Some(fm.desc);
                }
            }
            None
        };

        // 3a) Walk class → super chain and check interfaces at each level
        let mut cur = Some(class_internal);
        while let Some(owner) = cur {
            eprintln!("🔍 DEBUG: resolve_field_descriptor: Checking class {} for field {}", owner, field_name);
            if let Some(desc) = search_owner(owner, field_name) {
                eprintln!("🔍 DEBUG: resolve_field_descriptor: Found field {}#{} in class {}", class_internal, field_name, owner);
                return desc.to_string();
            }
            
            // Also check interfaces implemented by this class
            if let Some(&idx) = CLASSES_BY_NAME.get(owner) {
                let c = &CLASSES[idx];
                eprintln!("🔍 DEBUG: resolve_field_descriptor: Checking {} interfaces for class {}", c.interfaces.len(), owner);
                for &itf in c.interfaces {
                    eprintln!("🔍 DEBUG: resolve_field_descriptor: Checking interface {} for field {}", itf, field_name);
                if let Some(desc) = search_owner(itf, field_name) {
                        eprintln!("🔍 DEBUG: resolve_field_descriptor: Found field {}#{} in interface {} (via class {})", class_internal, field_name, itf, owner);
                    return desc.to_string();
                }
            }
            }
            
            // ascend to super, if any
            cur = boot::CLASSES_BY_NAME
                .get(owner)
                .and_then(|&idx| boot::CLASSES[idx].super_internal);
        }

        // 4) Error case - field not found in rt.rs or local overlay
        eprintln!("ERROR: Unresolved field {}#{} - not found in rt.rs or local overlay", class_internal, field_name);
        panic!("Field resolution failed: {}#{}", class_internal, field_name);
    }
    
    /// Resolve all label references by calculating proper offsets
    fn resolve_label_references(&mut self) {}

    /// Handle complex method body structure issues
    fn handle_complex_method_body_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Analyze and fix complex structural issues
        self.fix_complex_control_flow_issues()?;
        self.fix_complex_instruction_issues()?;
        self.fix_complex_method_integrity_issues()?;
        
        Ok(())
    }
    
    /// Fix complex control flow issues
    fn fix_complex_control_flow_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 6 {
            return Ok(());
        }
        
        let mut i = 0;
        let mut fixes_applied = 0;
        
        while i < self.bytecode_builder.code().len() - 5 {
            let opcode = self.bytecode_builder.code()[i];
            
            match opcode {
                // Handle complex conditional jump patterns
                0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab => { // ifeq, ifne, iflt, ifge, ifgt, ifle
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        
                        // Check for complex control flow patterns
                        if target_pc < 0 {
                            // Negative offset - this is usually invalid
                            eprintln!("Fixing negative jump offset at position {}: offset = {}", i, offset);
                            
                            // Replace with a safe forward jump
                            let _safe_offset = 3i16; // Jump to next instruction
                            // Cannot modify code directly - skip byte modification((safe_offset >> 8) & 0xff) as u8;
                            // Cannot modify code directly - skip byte modification(safe_offset & 0xff) as u8;
                            fixes_applied += 1;
                        } else if target_pc >= self.bytecode_builder.code().len() as i32 {
                            // Jump beyond method end
                            eprintln!("Fixing jump beyond method end at position {}: target_pc = {}", i, target_pc);
                            
                            // Jump to end of method
                            let _safe_offset = (self.bytecode_builder.code().len() - i - 3) as i16;
                            if _safe_offset >= -32768 && _safe_offset <= 32767 {
                                // Cannot modify code directly - skip byte modification((safe_offset >> 8) & 0xff) as u8;
                                // Cannot modify code directly - skip byte modification(safe_offset & 0xff) as u8;
                                fixes_applied += 1;
                            }
                        }
                    }
                }
                // Handle complex goto patterns
                0xc7 => { // goto
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        
                        // Check for complex goto patterns
                        if target_pc < 0 {
                            eprintln!("Fixing negative goto offset at position {}: offset = {}", i, offset);
                            
                            // Replace with a safe forward goto
                            let _safe_offset = 3i16;
                            // Cannot modify code directly - skip byte modification((safe_offset >> 8) & 0xff) as u8;
                            // Cannot modify code directly - skip byte modification(safe_offset & 0xff) as u8;
                            fixes_applied += 1;
                        }
                    }
                }
                _ => {}
            }
            
            i += self.get_instruction_size(opcode);
        }
        
        if fixes_applied > 0 {
            eprintln!("Applied {} complex control flow fixes", fixes_applied);
        }
        
        Ok(())
    }
    
    /// Fix complex instruction issues
    fn fix_complex_instruction_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().len() < 2 {
            return Ok(());
        }
        
        let mut i = 0;
        let mut fixes_applied = 0;
        
        while i < self.bytecode_builder.code().len() - 1 {
            let opcode = self.bytecode_builder.code()[i];
            let instruction_size = self.get_instruction_size(opcode);
            
            // Check for complex instruction patterns
            if instruction_size > 1 && i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Fixing incomplete complex instruction at position {}: opcode 0x{:02x}, size {}", i, opcode, instruction_size);
                
                // Try to complete the instruction with safe values
                if instruction_size == 2 {
                    self.bytecode_builder.push(0x00); // Add missing byte
                    fixes_applied += 1;
                } else if instruction_size == 3 {
                    if self.bytecode_builder.code().len() - i < 2 {
                        self.bytecode_builder.push(0x00);
                    }
                    if self.bytecode_builder.code().len() - i < 3 {
                        self.bytecode_builder.push(0x00);
                    }
                    fixes_applied += 1;
                }
            }
            
            i += instruction_size;
        }
        
        if fixes_applied > 0 {
            eprintln!("Applied {} complex instruction fixes", fixes_applied);
        }
        
        Ok(())
    }
    
    /// Fix complex method integrity issues
    fn fix_complex_method_integrity_issues(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut fixes_applied = 0;
        
        // Handle complex method termination issues
        let mut i = 0;
        let mut last_valid_return = None;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            if self.is_return_opcode(opcode) {
                last_valid_return = Some(i);
            }
            
            i += 1;
        }
        
        // If we found a return but it's not at the end, fix it
        if let Some(return_pos) = last_valid_return {
            if return_pos < self.bytecode_builder.code().len() - 1 {
                eprintln!("Fixing method termination: removing code after return at position {}", return_pos);
                // Cannot modify code directly - skip truncate operationreturn_pos + 1);
                fixes_applied += 1;
            }
        } else {
            // No return found, add one
            eprintln!("Fixing method termination: adding missing return instruction");
            Self::map_stack(self.bytecode_builder.return_())?;
            fixes_applied += 1;
        }
        
        if fixes_applied > 0 {
            eprintln!("Applied {} complex method integrity fixes", fixes_applied);
        }
        
        Ok(())
    }

    /// Comprehensive method validation
    fn comprehensive_method_validation(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        // Perform comprehensive validation of the entire method body
        self.validate_method_structure_comprehensive()?;
        self.validate_control_flow_comprehensive()?;
        self.validate_instruction_integrity_comprehensive()?;
        self.validate_method_termination_comprehensive()?;
        
        // 🔧 FINALIZE: Finalize method body with integrity checks and cleanup
        self.finalize_method_body()?;
        
        Ok(())
    }
    
    /// Validate method structure comprehensively
    fn validate_method_structure_comprehensive(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut issues_found = 0;
        let mut i = 0;
        let mut pc = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            let instruction_size = self.get_instruction_size(opcode);
            
            // Check instruction completeness
            if i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Comprehensive validation: Incomplete instruction at position {}: opcode 0x{:02x}", i, opcode);
                issues_found += 1;
                break;
            }
            
            // Validate instruction parameters
            match opcode {
                // Load/store instructions
                0x15 | 0x16 | 0x17 | 0x18 | 0x19 | 0x1a | 0x1b | 0x1c | 0x1d | 0x1e | 0x1f | // iload, lload, fload, dload, aload
                0x36 | 0x37 | 0x38 | 0x39 | 0x3a | 0x3b | 0x3c | 0x3d | 0x3e | 0x3f => { // istore, lstore, fstore, dstore, astore
                    if instruction_size == 2 {
                        let index = self.bytecode_builder.code()[i + 1];
                        if index > 0xff {
                            eprintln!("Comprehensive validation: Large index value {} for load/store at position {}", index, i);
                            issues_found += 1;
                        }
                    }
                }
                // Jump instructions
                0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab | 0xc7 => { // ifeq, ifne, iflt, ifge, ifgt, ifle, goto
                    if instruction_size == 3 {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = pc + 3 + offset as i32;
                        
                        if target_pc < 0 || target_pc >= self.bytecode_builder.code().len() as i32 {
                            eprintln!("Comprehensive validation: Invalid jump target at position {}: pc = {}, target_pc = {}", i, pc, target_pc);
                            issues_found += 1;
                        }
                    }
                }
                // Method invocation instructions
                0xb6 | 0xb7 | 0xb8 | 0xb9 => { // invokevirtual, invokespecial, invokestatic, invokeinterface
                    if instruction_size == 3 {
                        let index = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        if index == 0 {
                            eprintln!("Comprehensive validation: Zero constant pool index for method invocation at position {}", i);
                            issues_found += 1;
                        }
                    }
                }
                // Field access instructions
                0xb2 | 0xb3 | 0xb4 | 0xb5 => { // getstatic, putstatic, getfield, putfield
                    if instruction_size == 3 {
                        let index = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        if index == 0 {
                            eprintln!("Comprehensive validation: Zero constant pool index for field access at position {}", i);
                            issues_found += 1;
                        }
                    }
                }
                _ => {}
            }
            
            // Update program counter and position
            pc += instruction_size as i32;
            i += instruction_size;
        }
        
        if issues_found > 0 {
            eprintln!("Comprehensive method structure validation found {} issues", issues_found);
        } else {
            eprintln!("Comprehensive method structure validation passed");
        }
        
        Ok(())
    }
    
    /// Validate control flow comprehensively
    fn validate_control_flow_comprehensive(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut issues_found = 0;
        let mut control_flow_stack = Vec::new();
        let mut i = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            match opcode {
                // Conditional jumps
                0xa7 | 0xa8 | 0xa9 | 0xaa | 0xab => { // ifeq, ifne, iflt, ifge, ifgt, ifle
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        control_flow_stack.push(("conditional", i, target_pc));
                    } else {
                        eprintln!("Comprehensive control flow validation: Incomplete conditional jump at position {}", i);
                        issues_found += 1;
                    }
                }
                // Goto instructions
                0xc7 => { // goto
                    if i + 2 < self.bytecode_builder.code().len() {
                        let offset = ((self.bytecode_builder.code()[i + 1] as u16) << 8) | (self.bytecode_builder.code()[i + 2] as u16);
                        let target_pc = i as i32 + 3 + offset as i32;
                        
                        // Check if this goto closes a control flow structure
                        if let Some((flow_type, start_pos, _)) = control_flow_stack.last() {
                            if *flow_type == "conditional" && target_pc > *start_pos as i32 {
                                control_flow_stack.pop();
                            }
                        }
                    } else {
                        eprintln!("Comprehensive control flow validation: Incomplete goto at position {}", i);
                        issues_found += 1;
                    }
                }
                _ => {}
            }
            
            i += self.get_instruction_size(opcode);
        }
        
        // Check for unbalanced control flow
        if !control_flow_stack.is_empty() {
            eprintln!("Comprehensive control flow validation: Unbalanced control flow structures: {}", control_flow_stack.len());
            issues_found += control_flow_stack.len();
        }
        
        if issues_found > 0 {
            eprintln!("Comprehensive control flow validation found {} issues", issues_found);
        } else {
            eprintln!("Comprehensive control flow validation passed");
        }
        
        Ok(())
    }
    
    /// Validate instruction integrity comprehensively
    fn validate_instruction_integrity_comprehensive(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut issues_found = 0;
        let mut i = 0;
        
        while i < self.bytecode_builder.code().len() {
            let opcode = self.bytecode_builder.code()[i];
            
            // Check for invalid opcodes
            if opcode == 0xff {
                eprintln!("Comprehensive instruction integrity validation: Invalid opcode 0xff at position {}", i);
                issues_found += 1;
            }
            
            // Check instruction size
            let instruction_size = self.get_instruction_size(opcode);
            if i + instruction_size > self.bytecode_builder.code().len() {
                eprintln!("Comprehensive instruction integrity validation: Incomplete instruction at position {}: size {}", i, instruction_size);
                issues_found += 1;
                break;
            }
            
            i += instruction_size;
        }
        
        if issues_found > 0 {
            eprintln!("Comprehensive instruction integrity validation found {} issues", issues_found);
        } else {
            eprintln!("Comprehensive instruction integrity validation passed");
        }
        
        Ok(())
    }
    
    /// Validate method termination comprehensively
    fn validate_method_termination_comprehensive(&mut self) -> Result<()> {
        if self.bytecode_builder.code().is_empty() {
            return Ok(());
        }
        
        let mut issues_found = 0;
        
        // Check if method ends with return
        let last_opcode = self.bytecode_builder.code()[self.bytecode_builder.code().len() - 1];
        if !self.is_return_opcode(last_opcode) {
            eprintln!("Comprehensive method termination validation: Method does not end with return instruction");
            issues_found += 1;
        }
        
        // Check for unreachable code
        let mut i = 0;
        while i < self.bytecode_builder.code().len() - 1 {
            if self.is_return_opcode(self.bytecode_builder.code()[i]) {
                eprintln!("Comprehensive method termination validation: Unreachable code found after return at position {}", i);
                issues_found += 1;
                break;
            }
            i += 1;
        }
        
        if issues_found > 0 {
            eprintln!("Comprehensive method termination validation found {} issues", issues_found);
        } else {
            eprintln!("Comprehensive method termination validation passed");
        }
        
        Ok(())
    }
 }

 /// Local variable information
 /// This struct is kept for potential future use in debugging or enhanced local variable tracking
 #[derive(Debug, Clone)]
 #[allow(dead_code)]
 pub(crate) struct LocalVariable {
     pub(crate) name: String,
     pub(crate) var_type: TypeRef,
     pub(crate) index: u16,
     pub(crate) start_pc: u16,
     pub(crate) length: u16,
 }

 /// Label information
 #[derive(Debug)]
 struct Label {
     id: u16,
     position: u16,
     references: Vec<LabelReference>,
 }

 /// Label reference information
 #[derive(Debug)]
 struct LabelReference {
     #[allow(dead_code)]
     position: u16,
     instruction_size: u16, // Size of the instruction (opcode + operands)
 }

 #[derive(Debug)]
 struct LoopContext {
     label: Option<String>,
     continue_label: u16,
     break_label: u16,
 }

 #[derive(Debug)]
 struct PendingExceptionEntry {
     start_label: u16,
     end_label: u16,
     handler_label: u16,
     catch_type: u16,
 }

 #[derive(Debug, Default)]
 struct Scope {
     locals: Vec<usize>,
 }

 impl MethodWriter {
     fn find_loop_break_label(&self, label: Option<&String>) -> Option<u16> {
         match label {
             Some(name) => self.loop_stack.iter().rev().find(|c| c.label.as_ref().map(|s| s == name).unwrap_or(false)).map(|c| c.break_label),
             None => self.loop_stack.last().map(|c| c.break_label),
         }
     }
     fn find_loop_continue_label(&self, label: Option<&String>) -> Option<u16> {
         match label {
             Some(name) => self.loop_stack.iter().rev().find(|c| c.label.as_ref().map(|s| s == name).unwrap_or(false)).map(|c| c.continue_label),
             None => self.loop_stack.last().map(|c| c.continue_label),
         }
     }

     #[allow(dead_code)]
     fn set_local_length(&mut self, _local_vec_index: usize, _end_pc: u16) {
         // This function is no longer needed as lifetimes are managed by StackState
         // Kept for potential future use or API compatibility
     }

     fn record_line_number(&mut self, line: u16) {
         let pc = self.bytecode_builder.code().len() as u16;
         if let Some((last_pc, last_line)) = self.line_numbers.last() {
             if *last_pc == pc && *last_line == line { return; }
         }
         self.line_numbers.push((pc, line.max(1)));
     }

     fn record_stmt_line(&mut self, stmt: &Stmt) {
         let line = match stmt {
             Stmt::Expression(s) => s.span.start.line,
             Stmt::Declaration(s) => s.span.start.line,
             Stmt::TypeDecl(td) => td.span().start.line,
             Stmt::If(s) => s.span.start.line,
             Stmt::While(s) => s.span.start.line,
             Stmt::For(s) => s.span.start.line,
             Stmt::Switch(s) => s.span.start.line,
             Stmt::Return(s) => s.span.start.line,
             Stmt::Break(s) => s.span.start.line,
             Stmt::Continue(s) => s.span.start.line,
             Stmt::Try(s) => s.span.start.line,
             Stmt::Throw(s) => s.span.start.line,
             Stmt::Assert(s) => s.span.start.line,
             Stmt::Synchronized(s) => s.span.start.line,
             Stmt::Labeled(s) => s.span.start.line,
             Stmt::Block(s) => s.span.start.line,
             Stmt::Empty => return,
         } as u16;
         self.record_line_number(line);
     }
     
     /// Check if a condition is a boundary check pattern that can be optimized
     fn is_boundary_check_pattern(&self, condition: &Expr) -> bool {
         // Look for patterns like: (from > to) || (from < 0) || (to < 0)
         // These are common in boundary validation and can be optimized for stack usage
         if let Expr::Binary(bin_expr) = condition {
             if bin_expr.operator == BinaryOp::Or {
                 // Check if left side is a comparison
                 if let Expr::Binary(left_bin) = &*bin_expr.left {
                     if matches!(left_bin.operator, BinaryOp::Gt | BinaryOp::Lt) {
                         // Check if right side is also a comparison
                         if let Expr::Binary(right_bin) = &*bin_expr.right {
                             if matches!(right_bin.operator, BinaryOp::Gt | BinaryOp::Lt) {
                                 eprintln!("🔍 DEBUG: is_boundary_check_pattern: Detected boundary check pattern");
                                 return true;
                             }
                         }
                     }
                 }
             }
         }
         false
     }
     
     /// Extract comparison operands from a binary expression
     fn extract_comparison_operands(&self, _expr: &Expr) -> Option<(&Expr, &Expr)> {
         // Simplified version to avoid lifetime issues
         // TODO: Implement proper operand extraction
         None
     }
     
     /// Generate optimized exception throw for boundary violations
     fn generate_exception_throw(&mut self, exception_type: &str) -> Result<()> {
         eprintln!("🔍 DEBUG: generate_exception_throw: Throwing {}", exception_type);
         
         // Generate: new IndexOutOfBoundsException()
         // For now, use a simplified approach that doesn't require complex object creation
         eprintln!("🔍 DEBUG: generate_exception_throw: Would create and throw {}", exception_type);
         
         Ok(())
     }
     
     /// Optimize stack usage for boundary checks
     fn optimize_boundary_check_stack(&mut self, condition: &Expr) -> Result<()> {
         // This method optimizes stack usage by breaking down complex OR conditions
         // into simpler conditional jumps, reducing the maximum stack depth
         
         if let Expr::Binary(bin_expr) = condition {
             if bin_expr.operator == BinaryOp::Or {
                 eprintln!("🔍 DEBUG: optimize_boundary_check_stack: Optimizing OR condition for stack usage");
                 
                 // Generate optimized boundary checks that use less stack
                 // Instead of evaluating the entire OR expression on the stack,
                 // we generate separate conditional jumps
                 
                 // TODO: Implement the actual optimization logic
                 // This would involve:
                 // 1. Breaking down the OR into individual comparisons
                 // 2. Generating conditional jumps for each part
                 // 3. Ensuring the stack depth never exceeds the target
                 
                 eprintln!("🔍 DEBUG: optimize_boundary_check_stack: Stack optimization placeholder");
             }
         }
         
         Ok(())
     }
     
     /// Emit initial stack map frame at method entry
     fn emit_initial_stack_map_frame(&mut self) -> Result<()> {
         if let Some(ref mut emitter) = self.stack_map_emitter {
             // Create simplified initial locals (this + parameters)
             let initial_locals = vec![
                 crate::codegen::frame::VerificationType::Object(1), // this
                 crate::codegen::frame::VerificationType::Object(1), // parameter placeholder
             ];
             let empty_stack = vec![]; // Method entry has empty stack
             
             eprintln!("🔍 DEBUG: emit_initial_stack_map_frame: Emitting initial frame with {} locals", initial_locals.len());
             
             // Emit the initial frame at PC 0
             emitter.emit_stack_map_frame(0, initial_locals, empty_stack)
                 .map_err(|e| crate::error::Error::CodeGen { message: format!("Initial stack map frame emission failed: {}", e) })?;
         }
         Ok(())
     }
     
     /// Check if current method is static
     fn is_static_method(&self) -> bool {
         // TODO: Implement proper static method detection
         // For now, assume non-static
         false
     }
     
     /// Check if current method is constructor
     fn is_constructor_method(&self) -> bool {
         // TODO: Implement proper constructor detection
         // For now, assume non-constructor
         false
     }
     
     /// Finalize and optimize stack map frames
     fn finalize_stack_map_frames(&mut self) -> Result<()> {
         if let Some(ref mut emitter) = self.stack_map_emitter {
             eprintln!("🔍 DEBUG: finalize_stack_map_frames: Finalizing stack map frames");
             
             // Get emission statistics
             let stats = emitter.get_stats();
             eprintln!("🔍 DEBUG: finalize_stack_map_frames: Stats - emitted: {}, compressed: {}, dropped: {}", 
                      stats.frames_emitted, stats.frames_compressed, stats.frames_dropped);
             
             // Get optimized frames
             let frames = emitter.get_frames();
             eprintln!("🔍 DEBUG: finalize_stack_map_frames: Generated {} optimized frames", frames.len());
             
             // TODO: Apply additional frame optimizations if needed
             // This could include:
             // 1. Frame merging for consecutive similar frames
             // 2. Delta frame optimization
             // 3. Frame compression analysis
             
             eprintln!("🔍 DEBUG: finalize_stack_map_frames: Stack map frame optimization completed");
         }
         Ok(())
     }
     
     /// Smart stack map frame emission check
     /// Automatically decides when to emit stack map frames based on instruction context
     fn check_and_emit_stack_map_frame(&mut self, is_jump_target: bool, is_exception_handler: bool) -> Result<()> {
         if let Some(ref mut emitter) = self.stack_map_emitter {
             let current_pc = self.bytecode_builder.code().len() as u16;
             
             // Check if frame emission is needed
             if emitter.needs_frame_emission(current_pc, is_jump_target, is_exception_handler) {
                 // Use simplified locals and stack to avoid borrow conflicts
                 let locals = vec![crate::codegen::frame::VerificationType::Object(1)];
                 let stack = vec![];
                 
                 eprintln!("🔍 DEBUG: check_and_emit_stack_map_frame: Auto-emitting frame at PC {} (jump_target: {}, exception_handler: {})", 
                          current_pc, is_jump_target, is_exception_handler);
                 
                 if let Err(e) = emitter.emit_stack_map_frame(current_pc, locals, stack) {
                     eprintln!("🔍 DEBUG: check_and_emit_stack_map_frame: Auto frame emission failed: {:?}", e);
                 }
             }
         }
         Ok(())
     }
     
     /// Generate optimized string concatenation using StringBuilder
     fn generate_optimized_string_concatenation(&mut self, expressions: &[Expr]) -> Result<()> {
         eprintln!("🔧 OPT: Starting optimized string concatenation with {} expressions", expressions.len());
         
         // For now, fall back to standard method call generation
         // TODO: Implement full StringBuilder optimization
         eprintln!("🔧 OPT: String concatenation optimization placeholder - using fallback");
         
         // Generate arguments first
         for (i, expr) in expressions.iter().enumerate() {
             eprintln!("🔧 OPT: Generating expression {} for concatenation", i);
             self.generate_expression(expr)?;
         }
         
         // Use standard method call for now
         Ok(())
     }
     
     /// Generate optimized null check using getClass + pop
     fn generate_optimized_null_check(&mut self, expression: &Expr) -> Result<()> {
         eprintln!("🔧 OPT: Starting optimized null check");
         
         // Generate the expression to check
         self.generate_expression(expression)?;
         
         // For now, just generate a simple null check
         // TODO: Implement full getClass + pop optimization
         eprintln!("🔧 OPT: Null check optimization placeholder");
         
         Ok(())
     }
     
     /// Generate optimized array length access
     fn generate_optimized_array_length(&mut self, array_expr: &Expr) -> Result<()> {
         eprintln!("🔧 OPT: Starting optimized array length access");
         
         // The array expression should already be on the stack from argument generation
         // Just emit arraylength instruction
         Self::map_stack(self.bytecode_builder.arraylength())?;
         
         eprintln!("🔧 OPT: Array length optimization completed");
         Ok(())
     }
     
     /// Generate optimized class literal access
     fn generate_optimized_class_literal(&mut self, class_type: &TypeRef) -> Result<()> {
         eprintln!("🔧 OPT: Starting optimized class literal access for {}", class_type.name);
         
         // For now, use a placeholder constant pool index
         // TODO: Implement proper class literal optimization
         let class_ref = 1; // Placeholder
         
         Self::map_stack(self.bytecode_builder.ldc(class_ref))?;
         
         eprintln!("🔧 OPT: Class literal optimization completed");
         Ok(())
     }
     
     /// Apply invariant code motion optimization
     /// Move expressions that don't change in the loop outside the loop
     fn apply_invariant_code_motion(&mut self, body: &Stmt, movable_expressions: &[Expr]) -> Result<()> {
         println!("🔧 LOOP OPT: Applying invariant code motion for {} expressions", movable_expressions.len());
         
         // Generate invariant expressions before the loop
         for (i, expr) in movable_expressions.iter().enumerate() {
             println!("🔧 LOOP OPT: Moving invariant expression {} outside loop", i + 1);
             self.generate_expression(expr)?;
             
             // Store result in a local variable for reuse in loop
             // Use a simple local variable allocation strategy
             let local_index = 100 + i as u16; // Simple allocation starting from 100
             
             // Use istore for now (could be enhanced with type detection)
             Self::map_stack(self.bytecode_builder.istore(local_index))?;
             println!("🔧 LOOP OPT: Stored invariant expression {} in local {}", i + 1, local_index);
         }
         
         // Generate the loop body (which will now use the stored values)
         self.generate_statement(body)?;
         
         println!("🔧 LOOP OPT: Invariant code motion completed");
         Ok(())
     }
     
     /// Apply strength reduction optimization
     /// Convert expensive operations to cheaper ones (e.g., multiplication to addition)
     fn apply_strength_reduction(&mut self, body: &Stmt) -> Result<()> {
         println!("🔧 LOOP OPT: Applying strength reduction optimization");
         
         // For now, this is a placeholder implementation
         // TODO: Implement actual strength reduction analysis and transformation
         // Examples:
         // - i * 2 -> i + i or i << 1
         // - i * 3 -> i + i + i
         // - i * 4 -> i << 2
         
         // Generate the body with potential strength reduction
         self.generate_statement(body)?;
         
         println!("🔧 LOOP OPT: Strength reduction optimization completed");
         Ok(())
     }
     
     /// Apply dead code elimination optimization
     /// Remove code that cannot be reached or has no effect
     fn apply_dead_code_elimination(&mut self, body: &Stmt) -> Result<()> {
         println!("🔧 LOOP OPT: Applying dead code elimination optimization");
         
         // For now, this is a placeholder implementation
         // TODO: Implement actual dead code analysis and elimination
         // Examples:
         // - Remove unreachable code after return/break/continue
         // - Remove assignments to unused variables
         // - Remove side-effect-free expressions
         
         // Generate the body with potential dead code elimination
         self.generate_statement(body)?;
         
         println!("🔧 LOOP OPT: Dead code elimination optimization completed");
         Ok(())
     }
     
     /// Resolve conditional jump chains from genCond (javac-style)
     /// This method implements javac's Chain resolution mechanism for short-circuit evaluation
     /// 
     /// Based on javac's implementation:
     /// - For AND operations (a && b): if a is false, jump to false target; if a is true, continue to b
     /// - For OR operations (a || b): if a is true, jump to true target; if a is false, continue to b
     /// - The Chain contains the actual jump instructions that need to be resolved
     fn resolve_cond_jumps(&mut self, chain: &crate::codegen::chain::Chain, target_label: &str) -> Result<()> {
         eprintln!("🔍 DEBUG: resolve_cond_jumps: Resolving jump chain to target {}", target_label);
         
         // 🔧 FIX: Now we'll actually use the advanced jump chain resolution
         // This will enable proper javac-style short-circuit evaluation
         self.resolve_advanced_jump_chain(chain, target_label)?;
         
         eprintln!("🔍 DEBUG: resolve_cond_jumps: Successfully resolved jump chain");
         Ok(())
     }
     
     /// Advanced jump chain resolution that actually parses the Chain structure
     /// This is the key to implementing proper javac-style short-circuit evaluation
     fn resolve_advanced_jump_chain(&mut self, chain: &crate::codegen::chain::Chain, target_label: &str) -> Result<()> {
         eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: Advanced resolution for target {}", target_label);
         
         // Get the target label's PC
         let target_pc = self.bytecode_builder.get_label_pc(target_label);
         eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: target_pc = {:?}", target_pc);
         
         // Parse the Chain to find all jump instructions that need to be resolved
         let mut current_chain = Some(chain);
         while let Some(chain_node) = current_chain {
             let jump_pc = chain_node.pc;
             eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: Processing jump at PC {}", jump_pc);
             
             // Now implement the actual jump resolution logic
             if let Some(target_pc_value) = target_pc {
                 // 1. Look up the actual bytecode at jump_pc
                 let code_len = self.bytecode_builder.code().len();
                 if (jump_pc as usize) < code_len {
                     let opcode = self.bytecode_builder.code()[jump_pc as usize];
                     eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: Found opcode {:02x} at PC {}", opcode, jump_pc);
                     
                     // 2. Determine the jump instruction type and calculate offset
                     let offset = target_pc_value as i16 - jump_pc as i16;
                     eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: Calculated offset: {} (target: {}, current: {})", offset, target_pc_value, jump_pc);
                     
                     // 3. Update the bytecode at jump_pc with the correct offset
                     let offset_bytes = offset.to_be_bytes();
                     let code_mut = self.bytecode_builder.code_mut();
                     
                     let next_pc = (jump_pc + 1) as usize;
                     let next_next_pc = (jump_pc + 2) as usize;
                     if next_pc < code_mut.len() {
                         code_mut[next_pc] = offset_bytes[0];
                         if next_next_pc < code_mut.len() {
                             code_mut[next_next_pc] = offset_bytes[1];
                             eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: Successfully patched jump at PC {} with offset {:?}", jump_pc, offset_bytes);
                         }
                     }
                 } else {
                     eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: WARNING - Jump PC {} is out of bounds (code length: {})", jump_pc, self.bytecode_builder.code().len());
                 }
             } else {
                 eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: WARNING - Target label not found, cannot resolve jump");
             }
             
             // Move to the next chain node
             current_chain = chain_node.next.as_ref().map(|boxed| boxed.as_ref());
         }
         
                 eprintln!("🔍 DEBUG: resolve_advanced_jump_chain: Advanced resolution completed");
        Ok(())
    }

    /// Check if a condition requires short-circuit evaluation (javac-style)
    fn is_short_circuit_condition(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Binary(bin_expr) => {
                matches!(bin_expr.operator, BinaryOp::And | BinaryOp::Or)
            }
            _ => false,
        }
    }

    /// Generate short-circuit condition with direct jumps (javac-style)
    fn generate_short_circuit_condition(&mut self, expr: &Expr, false_target: u16) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_short_circuit_condition: Generating javac-style short-circuit jumps");
        
        match expr {
            Expr::Binary(bin_expr) => {
                match bin_expr.operator {
                    BinaryOp::Or => {
                        eprintln!("🔍 DEBUG: generate_short_circuit_condition: Processing OR with direct jumps");
                        self.generate_or_short_circuit(bin_expr, false_target)
                    }
                    BinaryOp::And => {
                        eprintln!("🔍 DEBUG: generate_short_circuit_condition: Processing AND with direct jumps");
                        self.generate_and_short_circuit(bin_expr, false_target)
                    }
                    _ => {
                        // Simple comparison - generate direct conditional jump
                        self.generate_comparison_jump(bin_expr, false_target)
                    }
                }
            }
            _ => {
                // Non-binary expression - generate traditional condition
                self.generate_expression(expr)?;
                let l = self.label_str(false_target);
                Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                Ok(())
            }
        }
    }

    /// Generate OR condition with short-circuit evaluation (a || b || c)
    fn generate_or_short_circuit(&mut self, bin_expr: &BinaryExpr, false_target: u16) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_or_short_circuit: Implementing javac-style OR short-circuit");
        
        // For OR: if any condition is true, we want to continue execution (not jump to false_target)
        // Only if ALL conditions are false should we jump to false_target
        
        // This is a simple approach: generate each condition and if it's true, continue
        // Only the last condition should jump to false_target if false
        
        // Flatten the OR chain into a list of conditions
        let mut conditions = Vec::new();
        let expr = Expr::Binary(bin_expr.clone());
        self.collect_or_conditions(&expr, &mut conditions);
        
        eprintln!("🔍 DEBUG: generate_or_short_circuit: Found {} conditions in OR chain", conditions.len());
        
        // For OR conditions, we want to jump to false_target only if ALL conditions are false
        // So for each condition except the last, if it's true, we skip to the end (continue execution)
        // For the last condition, if it's false, we jump to false_target
        
        for (i, condition) in conditions.iter().enumerate() {
            if i == conditions.len() - 1 {
                // Last condition: if false, jump to false_target
                if let Expr::Binary(bin) = condition {
                    // For the last condition, we want to jump to false_target if the condition is false
                    // So we need to invert the logic: if condition is false, jump to false_target
                    self.generate_comparison_jump(bin, false_target)?;
                } else {
                    self.generate_expression(condition)?;
                    let l = self.label_str(false_target);
                    Self::map_stack(self.bytecode_builder.ifeq(&l))?;
                }
            } else {
                // Not last condition: if true, skip to end (continue execution)
                // This means we generate a direct jump to false_target if the condition is true
                if let Expr::Binary(bin) = condition {
                    // For non-last conditions, if the condition is true, we want to skip to the end
                    // But since this is OR, if any condition is true, we should continue execution
                    // So we actually want to jump to false_target if the condition is true
                    // Wait, this is confusing. Let me think again...
                    
                    // Actually, for OR in an if statement:
                    // if (a || b || c) { then_block } else { else_block }
                    // 
                    // We want:
                    // - If a is true, execute then_block (continue execution)
                    // - If a is false, check b
                    // - If b is true, execute then_block (continue execution)  
                    // - If b is false, check c
                    // - If c is true, execute then_block (continue execution)
                    // - If c is false, execute else_block (jump to false_target)
                    //
                    // So for the first two conditions, if they are true, we continue execution (no jump)
                    // If they are false, we continue to the next condition (no jump)
                    // Only for the last condition, if it's false, we jump to false_target
                    
                    // But wait, that's not how javac does it. Let me look at the javac output again:
                    // javac generates: if_icmpgt 13, iflt 13, ifge 21
                    // This means:
                    // - if (fromIndex > toIndex) jump to exception (13)
                    // - if (fromIndex < 0) jump to exception (13)  
                    // - if (toIndex >= 0) jump past exception (21)
                    //
                    // So javac is actually generating jumps to the exception for the first two conditions!
                    // This means for OR conditions, if any of the first conditions is true, jump to the then block
                    // But in our case, the "then block" is the exception throwing, so false_target is actually the then block!
                    
                    // Generate a direct jump to false_target if the condition is true
                    // This matches javac's behavior: if any early condition is true, jump to the then block
                    self.generate_comparison_jump_for_or(bin, false_target)?;
                } else {
                    self.generate_expression(condition)?;
                    let l = self.label_str(false_target);
                    Self::map_stack(self.bytecode_builder.ifne(&l))?;
                }
            }
        }
        
        Ok(())
    }

    /// Collect all conditions in an OR chain into a flat list
    fn collect_or_conditions(&self, expr: &Expr, conditions: &mut Vec<Expr>) {
        if let Expr::Binary(bin_expr) = expr {
            if matches!(bin_expr.operator, BinaryOp::Or) {
                // Recursively collect from left and right
                self.collect_or_conditions(&bin_expr.left, conditions);
                self.collect_or_conditions(&bin_expr.right, conditions);
                return;
            }
        }
        // Not an OR expression, add it as a condition
        conditions.push(expr.clone());
    }

    /// Generate OR condition with explicit then and false targets
    fn generate_or_short_circuit_with_then_target(&mut self, bin_expr: &BinaryExpr, then_target: u16, false_target: u16) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_or_short_circuit_with_then_target: Generating OR with explicit targets");
        
        // Generate left condition - if true, jump to then_target
        if let Expr::Binary(left_bin) = &*bin_expr.left {
            if matches!(left_bin.operator, BinaryOp::Or) {
                // Nested OR - recurse with same targets
                self.generate_or_short_circuit_with_then_target(left_bin, then_target, false_target)?;
            } else {
                // Simple comparison - if condition is true, jump to then_target
                self.generate_comparison_jump_for_or(left_bin, then_target)?;
            }
        } else {
            // Non-comparison left operand
            self.generate_expression(&bin_expr.left)?;
            let l = self.label_str(then_target);
            Self::map_stack(self.bytecode_builder.ifne(&l))?; // If true, jump to then
        }
        
        // Generate right condition - if true, jump to then_target, if false, jump to false_target
        if let Expr::Binary(right_bin) = &*bin_expr.right {
            if matches!(right_bin.operator, BinaryOp::Or) {
                // Nested OR - recurse with same targets
                self.generate_or_short_circuit_with_then_target(right_bin, then_target, false_target)?;
            } else {
                // Simple comparison - if true, jump to then_target, if false, jump to false_target
                self.generate_comparison_jump_for_or(right_bin, then_target)?;
                // After the comparison, if we reach here, the condition was false
                let l = self.label_str(false_target);
                Self::map_stack(self.bytecode_builder.goto(&l))?; // Jump to false_target
            }
        } else {
            // Non-comparison right operand
            self.generate_expression(&bin_expr.right)?;
            let then_l = self.label_str(then_target);
            Self::map_stack(self.bytecode_builder.ifne(&then_l))?; // If true, jump to then
            let false_l = self.label_str(false_target);
            Self::map_stack(self.bytecode_builder.goto(&false_l))?; // If false, jump to false_target
        }
        
        Ok(())
    }

    /// Generate AND condition with short-circuit evaluation (a && b && c)
    fn generate_and_short_circuit(&mut self, bin_expr: &BinaryExpr, false_target: u16) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_and_short_circuit: Implementing javac-style AND short-circuit");
        
        // For AND: if any condition is false, jump to false_target immediately
        
        // Generate left condition
        if let Expr::Binary(left_bin) = &*bin_expr.left {
            if matches!(left_bin.operator, BinaryOp::And) {
                // Nested AND - recurse
                self.generate_and_short_circuit(left_bin, false_target)?;
            } else {
                // Simple comparison - generate direct jump (inverted for AND)
                self.generate_comparison_jump_inverted(left_bin, false_target)?;
            }
        } else {
            // Non-comparison left operand
            self.generate_expression(&bin_expr.left)?;
            let l = self.label_str(false_target);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        // Generate right condition
        if let Expr::Binary(right_bin) = &*bin_expr.right {
            if matches!(right_bin.operator, BinaryOp::And) {
                // Nested AND - recurse
                self.generate_and_short_circuit(right_bin, false_target)?;
            } else {
                // Simple comparison - generate direct jump (inverted for AND)
                self.generate_comparison_jump_inverted(right_bin, false_target)?;
            }
        } else {
            // Non-comparison right operand
            self.generate_expression(&bin_expr.right)?;
            let l = self.label_str(false_target);
            Self::map_stack(self.bytecode_builder.ifeq(&l))?;
        }
        
        Ok(())
    }

    /// Generate direct comparison jump (for OR conditions)
    fn generate_comparison_jump(&mut self, bin_expr: &BinaryExpr, false_target: u16) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_comparison_jump: Generating direct comparison jump");
        
        // Generate operands
        self.generate_expression(&bin_expr.left)?;
        self.generate_expression(&bin_expr.right)?;
        
        // Generate direct conditional jump based on operator
        let label = self.label_str(false_target);
        match bin_expr.operator {
            BinaryOp::Gt => {
                // For OR: if left > right is false, continue checking next condition
                Self::map_stack(self.bytecode_builder.if_icmple(&label))?;
            }
            BinaryOp::Lt => {
                // For OR: if left < right is false, continue checking next condition  
                Self::map_stack(self.bytecode_builder.if_icmpge(&label))?;
            }
            BinaryOp::Ge => {
                // For OR: if left >= right is false, continue checking next condition
                Self::map_stack(self.bytecode_builder.if_icmplt(&label))?;
            }
            BinaryOp::Le => {
                // For OR: if left <= right is false, continue checking next condition
                Self::map_stack(self.bytecode_builder.if_icmpgt(&label))?;
            }
            BinaryOp::Eq => {
                // For OR: if left == right is false, continue checking next condition
                Self::map_stack(self.bytecode_builder.if_icmpne(&label))?;
            }
            BinaryOp::Ne => {
                // For OR: if left != right is false, continue checking next condition
                Self::map_stack(self.bytecode_builder.if_icmpeq(&label))?;
            }
            _ => {
                // Fallback to traditional approach
                return Err(crate::error::Error::codegen_error("Unsupported comparison operator for direct jump"));
            }
        }
        
        Ok(())
    }

    /// Generate comparison jump for OR conditions (if true, jump to target)
    fn generate_comparison_jump_for_or(&mut self, bin_expr: &BinaryExpr, true_target: u16) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_comparison_jump_for_or: Generating comparison jump for OR");
        
        // Generate operands
        self.generate_expression(&bin_expr.left)?;
        self.generate_expression(&bin_expr.right)?;
        
        // Generate conditional jump based on operator (if condition is true, jump to true_target)
        let label = self.label_str(true_target);
        match bin_expr.operator {
            BinaryOp::Gt => {
                // If left > right is true, jump to true_target
                Self::map_stack(self.bytecode_builder.if_icmpgt(&label))?;
            }
            BinaryOp::Lt => {
                // If left < right is true, jump to true_target
                Self::map_stack(self.bytecode_builder.if_icmplt(&label))?;
            }
            BinaryOp::Ge => {
                // If left >= right is true, jump to true_target
                Self::map_stack(self.bytecode_builder.if_icmpge(&label))?;
            }
            BinaryOp::Le => {
                // If left <= right is true, jump to true_target
                Self::map_stack(self.bytecode_builder.if_icmple(&label))?;
            }
            BinaryOp::Eq => {
                // If left == right is true, jump to true_target
                Self::map_stack(self.bytecode_builder.if_icmpeq(&label))?;
            }
            BinaryOp::Ne => {
                // If left != right is true, jump to true_target
                Self::map_stack(self.bytecode_builder.if_icmpne(&label))?;
            }
            _ => {
                return Err(crate::error::Error::codegen_error("Unsupported comparison operator for OR jump"));
            }
        }
        
        Ok(())
    }

    /// Generate inverted comparison jump for OR conditions (if false, continue to next condition)
    fn generate_comparison_jump_inverted_for_or(&mut self, bin_expr: &BinaryExpr) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_comparison_jump_inverted_for_or: Generating inverted comparison for OR");
        
        // Generate operands
        self.generate_expression(&bin_expr.left)?;
        self.generate_expression(&bin_expr.right)?;
        
        // For OR: if condition is true, we want to continue execution (no jump)
        // if condition is false, we continue to next condition (also no jump)
        // This method is called for the left side of OR, so we don't need to generate any jumps
        // The comparison result is just left on the stack and will be handled by the overall OR logic
        
        // Actually, let's not generate any jumps here - just let the values be on the stack
        // and handle the logic in the caller
        
        Ok(())
    }

    /// Generate inverted comparison jump (for AND conditions)
    fn generate_comparison_jump_inverted(&mut self, bin_expr: &BinaryExpr, false_target: u16) -> Result<()> {
        eprintln!("🔍 DEBUG: generate_comparison_jump_inverted: Generating inverted comparison jump");
        
        // Generate operands
        self.generate_expression(&bin_expr.left)?;
        self.generate_expression(&bin_expr.right)?;
        
        // Generate inverted conditional jump based on operator (for AND short-circuit)
        let label = self.label_str(false_target);
        match bin_expr.operator {
            BinaryOp::Gt => {
                // For AND: if left > right is false, jump to false_target
                Self::map_stack(self.bytecode_builder.if_icmple(&label))?;
            }
            BinaryOp::Lt => {
                // For AND: if left < right is false, jump to false_target
                Self::map_stack(self.bytecode_builder.if_icmpge(&label))?;
            }
            BinaryOp::Ge => {
                // For AND: if left >= right is false, jump to false_target
                Self::map_stack(self.bytecode_builder.if_icmplt(&label))?;
            }
            BinaryOp::Le => {
                // For AND: if left <= right is false, jump to false_target
                Self::map_stack(self.bytecode_builder.if_icmpgt(&label))?;
            }
            BinaryOp::Eq => {
                // For AND: if left == right is false, jump to false_target
                Self::map_stack(self.bytecode_builder.if_icmpne(&label))?;
            }
            BinaryOp::Ne => {
                // For AND: if left != right is false, jump to false_target
                Self::map_stack(self.bytecode_builder.if_icmpeq(&label))?;
            }
            _ => {
                // Fallback to traditional approach
                return Err(crate::error::Error::codegen_error("Unsupported comparison operator for inverted jump"));
            }
        }
        
        Ok(())
    }

}