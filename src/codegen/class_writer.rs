//! Class writer for generating Java .class files
//! 
//! This module handles the conversion of AST class declarations into Java bytecode.

use super::opcodes;
use crate::codegen::{
    flag::access_flags,
    attribute::{AttributeInfo, NamedAttribute, LineNumberTableAttribute, LocalVariableTableAttribute, LocalVariableEntry, make_line_number_table_attribute, make_local_variable_table_attribute, make_stack_map_attribute},
    ClassFile,
    field::FieldInfo,
    method::MethodInfo,
    frame::{FrameBuilder, describe_stack_map_frames},
    modifiers_to_flags,
    signature::TypeNameResolver,
};
use super::descriptor::type_to_descriptor;
use crate::config::Config;
use crate::ast::*;
use crate::error::Result;
use super::method_writer::MethodWriter as BodyWriter;
use std::collections::HashMap;

/// Class writer for generating Java bytecode
pub struct ClassWriter {
    class_file: ClassFile,
    config: Config,
    current_class_name: Option<String>,
    current_class_decl: Option<ClassDecl>,
    package_name: Option<String>,
    cp_shared: Option<std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>>,
    pending_default_ctor_method_idx: Option<usize>,
    // Annotation retention index for the current compilation unit: simple or FQ name -> retention
    annotation_retention: HashMap<String, crate::codegen::attribute::RetentionPolicy>,
    // All types in the current compilation unit for interface method resolution
    all_types: Option<Vec<crate::ast::TypeDecl>>,
}

impl ClassWriter {
    /// Create a new class writer
    pub fn new() -> Self {
        Self {
            class_file: ClassFile::new(),
            config: Config::default(),
            current_class_name: None,
            current_class_decl: None,
            package_name: None,
            cp_shared: None,
            pending_default_ctor_method_idx: None,
            annotation_retention: HashMap::new(),
            all_types: None,
        }
    }

    pub fn new_with_config(config: Config) -> Self {
        Self {
            class_file: ClassFile::new(),
            config,
            current_class_name: None,
            current_class_decl: None,
            package_name: None,
            cp_shared: None,
            pending_default_ctor_method_idx: None,
            annotation_retention: HashMap::new(),
            all_types: None,
        }
    }

    /// Optionally set the package name (e.g., "mono" or "com.example")
    pub fn set_package_name<S: Into<String>>(&mut self, package: Option<S>) {
        self.package_name = package.map(|s| s.into());
    }
    
    /// Set all types for interface method resolution
    pub fn set_all_types(&mut self, all_types: Vec<crate::ast::TypeDecl>) {
        self.all_types = Some(all_types);
    }

    /// Provide CU-wide annotation retention mapping
    pub fn set_annotation_retention_index(&mut self, idx: HashMap<String, crate::codegen::attribute::RetentionPolicy>) {
        self.annotation_retention = idx;
    }

    /// Enable or disable debug info (LineNumberTable, LocalVariableTable)
    pub fn set_debug(&mut self, debug: bool) { self.config.debug = debug; }

    /// Enable or disable emission of StackMapTable frames
    pub fn set_emit_frames(&mut self, emit: bool) { self.config.emit_frames = emit; }
    
    /// Generate bytecode for an enum declaration
    pub fn generate_enum(&mut self, enum_decl: &EnumDecl) -> Result<()> {
        // Set enum name and access flags
        let enum_name = &enum_decl.name;
        let this_class_index = self.class_file.constant_pool.add_class(enum_name);
        self.class_file.this_class = this_class_index;
        self.current_class_name = Some(enum_name.to_string());
        
        // Set up shared constant pool for method generation
        self.cp_shared = Some(std::rc::Rc::new(std::cell::RefCell::new(self.class_file.constant_pool.clone())));
        
        // Set access flags - enums are always final
        let mut access_flags = access_flags::ACC_FINAL | access_flags::ACC_SUPER;
        if enum_decl.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        self.class_file.access_flags = access_flags;
        
        // Set superclass to java.lang.Enum
        let super_class_index = self.class_file.constant_pool.try_add_class("java/lang/Enum")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        self.class_file.super_class = super_class_index;
        
        // Add implemented interfaces
        for interface in &enum_decl.implements {
            let interface_index = self.class_file.constant_pool.try_add_class(&interface.name)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            self.class_file.interfaces.push(interface_index);
        }
        
        // Generate enum constants as public static final fields
        for constant in &enum_decl.constants {
            self.generate_enum_constant(constant, &enum_decl.name)?;
        }
        
        // Generate fields and methods from enum body
        for member in &enum_decl.body {
            match member {
                ClassMember::Field(field) => {
                    self.generate_field(field)?;
                }
                ClassMember::Method(method) => {
                    self.generate_method(method)?;
                }
                ClassMember::Constructor(constructor) => {
                    self.generate_constructor(constructor)?;
                }
                ClassMember::TypeDecl(_type_decl) => {
                    // TODO: Handle nested type declarations
                    // For now, skip nested types as they would be separate class files
                }
                ClassMember::Initializer(_initializer) => {
                    // TODO: Handle instance and static initializers
                    // For now, skip initializers
                }
            }
        }
        
        // Generate constructor if not present
        if !enum_decl.body.iter().any(|m| {
            if let ClassMember::Constructor(_) = m { true } else { false }
        }) {
            self.generate_enum_constructor(enum_decl)?;
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an interface declaration
    pub fn generate_interface(&mut self, interface: &InterfaceDecl) -> Result<()> {
        // Initialize a shared constant pool for the whole interface emission
        self.cp_shared = Some(std::rc::Rc::new(std::cell::RefCell::new(self.class_file.constant_pool.clone())));
        
        // Set interface name and access flags
        let internal_name = if let Some(pkg) = &self.package_name {
            if pkg.is_empty() { interface.name.clone() } else { format!("{}/{}", pkg.replace('.', "/"), interface.name) }
        } else { interface.name.clone() };
        let this_class_index = self.class_file.constant_pool.try_add_class(&internal_name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        self.class_file.this_class = this_class_index;
        
        // Set current_class_name for method generation
        self.current_class_name = Some(internal_name.clone());
        
        // Set access flags - interfaces are always abstract
        let mut access_flags = access_flags::ACC_INTERFACE | access_flags::ACC_ABSTRACT;
        if interface.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        self.class_file.access_flags = access_flags;
        
        // Set superclass to java.lang.Object (interfaces implicitly extend Object)
        let super_class_index = self.class_file.constant_pool.try_add_class("java/lang/Object")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        self.class_file.super_class = super_class_index;
        
        // Add extended interfaces
        for extended_interface in &interface.extends {
            let interface_index = self.class_file.constant_pool.try_add_class(&extended_interface.name)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            self.class_file.interfaces.push(interface_index);
        }
        
        // Generate fields and methods from interface body
        for member in &interface.body {
            match member {
                InterfaceMember::Field(field) => {
                    // Interface fields are implicitly public, static, and final
                    self.generate_interface_field(field)?;
                }
                InterfaceMember::Method(method) => {
                    // Interface methods are implicitly public and abstract
                    self.generate_interface_method(method)?;
                }
                InterfaceMember::TypeDecl(_type_decl) => {
                    // TODO: Handle nested type declarations
                }
            }
        }
        
        // Add Signature attribute if interface has generic type parameters
        if !interface.type_params.is_empty() {
            use crate::codegen::signature::{interface_to_signature, TypeNameResolver};
            let type_resolver = TypeNameResolver::with_default_mappings();
            let signature_string = interface_to_signature(interface, self.package_name.as_deref(), self.current_class_name.as_deref(), &type_resolver);
            let signature_index = self.class_file.constant_pool.try_add_utf8(&signature_string)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let signature_attr = crate::codegen::attribute::SignatureAttribute { 
                signature: crate::codegen::typed_index::ConstPoolIndex::from(signature_index) 
            };
            let name_index = self.class_file.constant_pool.try_add_utf8("Signature")
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let named_attr = crate::codegen::attribute::NamedAttribute::new(
                crate::codegen::typed_index::ConstPoolIndex::from(name_index), 
                AttributeInfo::Signature(signature_attr)
            );
            self.class_file.attributes.push(named_attr);
        }

        // Add SourceFile attribute
        let source_file_name = format!("{}.java", interface.name);
        let source_file_index = self.class_file.constant_pool.try_add_utf8(&source_file_name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let source_file_attr = crate::codegen::attribute::SourceFileAttribute { 
            filename: crate::codegen::typed_index::ConstPoolIndex::from(source_file_index) 
        };
        let name_index = self.class_file.constant_pool.try_add_utf8("SourceFile")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let named_attr = crate::codegen::attribute::NamedAttribute::new(
            crate::codegen::typed_index::ConstPoolIndex::from(name_index), 
            AttributeInfo::SourceFile(source_file_attr)
        );
        self.class_file.attributes.push(named_attr);
        
        // Finalize: write back shared pool
        if let Some(cp) = &self.cp_shared { 
            self.class_file.constant_pool = cp.borrow().clone(); 
        }
        self.cp_shared = None;
        
        Ok(())
    }
    
    /// Generate bytecode for a class declaration
    pub fn generate_class(&mut self, class: &ClassDecl) -> Result<()> {
        // Initialize a shared constant pool for the whole class emission
        self.cp_shared = Some(std::rc::Rc::new(std::cell::RefCell::new(self.class_file.constant_pool.clone())));
        // Set class name and access flags
        let internal_name = if let Some(pkg) = &self.package_name {
            if pkg.is_empty() { class.name.clone() } else { format!("{}/{}", pkg.replace('.', "/"), class.name) }
        } else { class.name.clone() };
        self.current_class_name = Some(internal_name.clone());
        self.current_class_decl = Some(class.clone());
        let deferred_this_class_name = internal_name.clone();
        let deferred_super_class_name: String = class
            .extends
            .as_ref()
            .map(|t| super::classpath::resolve_class_name_with_fallback(&t.name, self.package_name.as_deref()))
            .unwrap_or("java/lang/Object".to_string());
        
        // Set access flags
        let mut access_flags = access_flags::ACC_SUPER;
        if class.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        if class.modifiers.contains(&Modifier::Abstract) {
            access_flags |= access_flags::ACC_ABSTRACT;
        }
        if class.modifiers.contains(&Modifier::Final) {
            access_flags |= access_flags::ACC_FINAL;
        }
        self.class_file.access_flags = access_flags;
        
        // Defer SourceFile attribute until after members for javac-like ordering
        
        // Defer adding interfaces until after methods to avoid touching Class Utf8s early
        let deferred_interfaces: Vec<String> = class.implements.iter().map(|itf| {
            let interface_name = &itf.name;
            // If interface name doesn't contain package, try to resolve it
            if !interface_name.contains('.') {
                // Try to resolve using classpath first
                if let Some(resolved) = crate::codegen::classpath::resolve_class_name(interface_name) {
                    resolved.to_string()
                } else if crate::consts::JAVA_LANG_SIMPLE_TYPES.contains(&interface_name.as_str()) {
                    // java.lang types
                    format!("java/lang/{}", interface_name)
                } else if interface_name == "Comparator" || interface_name == "Iterator" || interface_name == "Collection" || 
                          interface_name == "List" || interface_name == "Set" || interface_name == "Map" || 
                          interface_name == "Deque" || interface_name == "Queue" || interface_name == "Iterable" {
                    // java.util types
                    format!("java/util/{}", interface_name)
                } else if interface_name == "Serializable" || interface_name == "Closeable" || interface_name == "Flushable" {
                    // java.io types
                    format!("java/io/{}", interface_name)
                } else {
                    // Default to java.lang if we can't determine
                    format!("java/lang/{}", interface_name)
                }
            } else {
                // Interface already has package name
                interface_name.replace('.', "/")
            }
        }).collect();
        
        // Track whether user-defined constructor exists
        let has_user_ctor = class.body.iter().any(|m| matches!(m, ClassMember::Constructor(_)));

        // First pass: collect code bodies for constructors and methods to touch CP early without adding attribute names
        #[derive(Clone)]
        struct PendingCode {
            access_flags: u16,
            name: String,
            descriptor: String,
            code_bytes: Vec<u8>,
            max_locals: u16,
            exceptions: Vec<crate::codegen::attribute::ExceptionTableEntry>,
            locals: Vec<crate::codegen::bytecode::LocalSlot>,
            line_numbers: Vec<(u16, u16)>,
        }
        let mut pending_methods: Vec<PendingCode> = Vec::new();

        // Helper to collect a method body
        // Collect method bodies (no closures to avoid borrow checker conflict)
        fn collect_method_into(
            cw: &mut ClassWriter,
            method: &MethodDecl,
            out: &mut Vec<PendingCode>,
        ) -> Result<()> {
            let constant_pool_rc = cw.cp_shared.as_ref().unwrap().clone();
            let mut code_writer = BodyWriter::new_with_constant_pool_and_class_decl(constant_pool_rc, cw.current_class_decl.clone().ok_or_else(|| crate::error::Error::Internal { message: "current_class_decl not set".into() })?);
            if let Some(all_types) = &cw.all_types {
                code_writer.set_all_types(all_types.clone());
            }
            code_writer.generate_method_body(method)?;
            let (code_bytes, _max_stack, max_locals, exceptions, locals, line_numbers) = code_writer.finalize();
            let access_flags = modifiers_to_flags(&method.modifiers);
            let descriptor = cw.generate_method_descriptor(method);
            out.push(PendingCode { access_flags, name: method.name.clone(), descriptor, code_bytes, max_locals: max_locals as u16, exceptions, locals, line_numbers });
            Ok(())
        }

        // Helper to collect default constructor body
        fn collect_default_ctor_into(cw: &mut ClassWriter, class: &ClassDecl, out: &mut Vec<PendingCode>) -> Result<()> {
            let mut code_bytes = Vec::new();
            code_bytes.push(opcodes::ALOAD_0);
            let super_class_name = class
                .extends
                .as_ref()
                .map(|t| super::classpath::resolve_class_name_with_fallback(&t.name, cw.package_name.as_deref()))
                .unwrap_or("java/lang/Object".to_string());
            let mref = { let mut cp = cw.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(&super_class_name, "<init>", "()V") }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            code_bytes.push(opcodes::INVOKESPECIAL);
            code_bytes.extend_from_slice(&mref.to_be_bytes());
            code_bytes.push(opcodes::RETURN);
            let access_flags = access_flags::ACC_PUBLIC;
            let name = "<init>".to_string();
            let descriptor = "()V".to_string();
            let exceptions: Vec<crate::codegen::attribute::ExceptionTableEntry> = Vec::new();
            let locals: Vec<crate::codegen::bytecode::LocalSlot> = Vec::new();
            let line_numbers = vec![(0, class.span.start.line as u16).into()];
            out.push(PendingCode { access_flags, name, descriptor, code_bytes, max_locals: 1, exceptions, locals, line_numbers });
            Ok(())
        }

        // Collect constructor bodies first (to touch Object.<init>), then methods
        if has_user_ctor {
            for member in &class.body {
                if let ClassMember::Constructor(constructor) = member {
                    // Build constructor prologue and invoke the immediate super constructor
                    let mut code_bytes = Vec::new();
                    // load `this`
                    code_bytes.push(opcodes::ALOAD_0);

                    // Determine how many arguments to pass to super: use explicit `super(...)` if present; otherwise 0
                    let super_arg_count: usize = match &constructor.explicit_invocation {
                        Some(crate::ast::ExplicitCtorInvocation::Super { arg_count }) => *arg_count,
                        _ => 0,
                    };

                    // Emit loads for the first `super_arg_count` parameters from local slots
                    let mut local_index: u16 = 1; // slot 0 is `this`
                    for (i, param) in constructor.parameters.iter().enumerate() {
                        if i >= super_arg_count { break; }
                        let ty_name = param.type_ref.name.as_str();
                        // Choose load opcode based on type and index
                        let (opcode, width): (u8, u16) = match ty_name {
                            "long" => {
                                let op = match local_index {
                                    0 => opcodes::LLOAD_0,
                                    1 => opcodes::LLOAD_1,
                                    2 => opcodes::LLOAD_2,
                                    3 => opcodes::LLOAD_3,
                                    _ => opcodes::LLOAD,
                                };
                                (op, 2)
                            }
                            "double" => {
                                let op = match local_index {
                                    0 => opcodes::DLOAD_0,
                                    1 => opcodes::DLOAD_1,
                                    2 => opcodes::DLOAD_2,
                                    3 => opcodes::DLOAD_3,
                                    _ => opcodes::DLOAD,
                                };
                                (op, 2)
                            }
                            "float" => {
                                let op = match local_index {
                                    0 => opcodes::FLOAD_0,
                                    1 => opcodes::FLOAD_1,
                                    2 => opcodes::FLOAD_2,
                                    3 => opcodes::FLOAD_3,
                                    _ => opcodes::FLOAD,
                                };
                                (op, 1)
                            }
                            "int" | "short" | "byte" | "char" | "boolean" => {
                                let op = match local_index {
                                    0 => opcodes::ILOAD_0,
                                    1 => opcodes::ILOAD_1,
                                    2 => opcodes::ILOAD_2,
                                    3 => opcodes::ILOAD_3,
                                    _ => opcodes::ILOAD,
                                };
                                (op, 1)
                            }
                            _ => {
                                // reference or array
                                let op = match local_index {
                                    0 => opcodes::ALOAD_0,
                                    1 => opcodes::ALOAD_1,
                                    2 => opcodes::ALOAD_2,
                                    3 => opcodes::ALOAD_3,
                                    _ => opcodes::ALOAD,
                                };
                                (op, 1)
                            }
                        };
                        code_bytes.push(opcode);
                        if opcode == opcodes::ILOAD || opcode == opcodes::LLOAD || opcode == opcodes::FLOAD || opcode == opcodes::DLOAD || opcode == opcodes::ALOAD {
                            // generic form requires a u8 index operand
                            code_bytes.push(local_index as u8);
                        }
                        local_index = local_index.saturating_add(width);
                    }

                    // Determine super class internal name
                    let super_class_name = class
                        .extends
                        .as_ref()
                        .map(|t| super::classpath::resolve_class_name_with_fallback(&t.name, self.package_name.as_deref()))
                        .unwrap_or_else(|| "java/lang/Object".to_string());

                    // Build descriptor for the chosen number of super args
                    let mut super_params_desc = String::new();
                    for (i, p) in constructor.parameters.iter().enumerate() {
                        if i >= super_arg_count { break; }
                        super_params_desc.push_str(&super::descriptor::type_to_descriptor(&p.type_ref));
                    }
                    let super_descriptor = format!("({})V", super_params_desc);

                    // INVOKESPECIAL super.<init>(...)
                    let mref = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(&super_class_name, "<init>", &super_descriptor) }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    code_bytes.push(opcodes::INVOKESPECIAL);
                    code_bytes.extend_from_slice(&mref.to_be_bytes());

                    // RETURN
                    code_bytes.push(opcodes::RETURN);

                    let access_flags = modifiers_to_flags(&constructor.modifiers);
                    let name = "<init>".to_string();
                    let descriptor = self.generate_constructor_descriptor(constructor);
                    let exceptions: Vec<crate::codegen::attribute::ExceptionTableEntry> = Vec::new();
                    let locals: Vec<crate::codegen::bytecode::LocalSlot> = Vec::new();
                    let line_numbers = vec![(0, constructor.span.start.line as u16).into()];
                    // Compute max_locals: this (1) + parameter slots (account for wide types)
                    let mut max_locals: u16 = 1;
                    for p in &constructor.parameters {
                        let add = match p.type_ref.name.as_str() { "long" | "double" => 2, _ => 1 };
                        max_locals = max_locals.saturating_add(add);
                    }
                    pending_methods.push(PendingCode { access_flags, name, descriptor, code_bytes, max_locals, exceptions, locals, line_numbers });
                }
            }
        } else {
            collect_default_ctor_into(self, class, &mut pending_methods)?;
        }
        for member in &class.body {
            if let ClassMember::Method(m) = member { collect_method_into(self, m, &mut pending_methods)?; }
        }
        
        // Add class and super at the end to match javac ordering (after methods' CP touches)
        self.class_file.this_class = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class(&deferred_this_class_name) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        self.class_file.super_class = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class(&deferred_super_class_name) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;

        // Now add interfaces (if any)
        for iname in deferred_interfaces {
            let interface_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class(&iname) }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            self.class_file.interfaces.push(interface_index);
        }

        // Defer SourceFile attribute until after methods are written (javac order)

        // Second pass: emit fields (none in HelloWorld), then methods with Code/LNT names now
        for member in &class.body {
            if let ClassMember::Field(field) = member { self.generate_field(field)?; }
        }
        // Build MethodInfo in the order: first constructor(s), then methods
        for pc in &pending_methods {
            // Build Code attribute payload first, defer interning of "Code" name until after
            let mut attribute_bytes = Vec::new();
            // compute max_stack by lightweight simulation
            let mut sim_depth: i32 = 0; let mut sim_max: i32 = 0; let mut i = 0usize;
            while i < pc.code_bytes.len() {
                let op = pc.code_bytes[i];
                match op {
                    // ALOAD_0..3, ILOAD_0..3, FLOAD_0..3, reference loads push 1
                    0x2a | 0x2b | 0x2c | 0x2d | // ALOAD_0..3
                    0x1a | 0x1b | 0x1c | 0x1d | // ILOAD_0..3
                    0x22 | 0x23 | 0x24 | 0x25   // FLOAD_0..3
                        => { sim_depth += 1; }
                    // LLOAD_0..3, DLOAD_0..3 push 2
                    0x1e | 0x1f | 0x20 | 0x21 | // LLOAD_0..3
                    0x26 | 0x27 | 0x28 | 0x29   // DLOAD_0..3
                        => { sim_depth += 2; }
                    // Generic loads with index operand
                    0x15 /* ILOAD */ => { sim_depth += 1; i += 1; }
                    0x16 /* LLOAD */ => { sim_depth += 2; i += 1; }
                    0x17 /* FLOAD */ => { sim_depth += 1; i += 1; }
                    0x18 /* DLOAD */ => { sim_depth += 2; i += 1; }
                    0x19 /* ALOAD */ => { sim_depth += 1; i += 1; }
                    // LDC and LDC_W push 1 (or 2 for LDC2_W)
                    0x12 /* LDC */ => { sim_depth += 1; i += 1; }
                    0x13 /* LDC_W */ => { sim_depth += 1; i += 2; }
                    0x14 /* LDC2_W */ => { sim_depth += 2; i += 2; }
                    // GETSTATIC of reference/primitive assumed to push 1 (approx)
                    0xb2 /* GETSTATIC */ => { sim_depth += 1; i += 2; }
                    // INVOKEVIRTUAL/INTERFACE approx: pop receiver+args (unknown), but max will be measured before call
                    0xb6 /* INVOKEVIRTUAL */ => { i += 2; }
                    // INVOKESPECIAL: pop receiver+args (unknown). We don't adjust depth precisely.
                    0xb7 /* INVOKESPECIAL */ => { i += 2; }
                    // INVOKESTATIC could pop args; ignore
                    0xb8 /* INVOKESTATIC */ => { i += 2; }
                    // RETURN no stack effect
                    0xb1 /* RETURN */ => {}
                    _ => {}
                }
                if sim_depth > sim_max { sim_max = sim_depth; }
                i += 1;
            }
            let computed_max_stack = sim_max.max(0) as u16;
            attribute_bytes.extend_from_slice(&computed_max_stack.to_be_bytes());
            attribute_bytes.extend_from_slice(&pc.max_locals.to_be_bytes());
            attribute_bytes.extend_from_slice(&(pc.code_bytes.len() as u32).to_be_bytes());
            attribute_bytes.extend_from_slice(&pc.code_bytes);
            attribute_bytes.extend_from_slice(&(pc.exceptions.len() as u16).to_be_bytes());
            for e in &pc.exceptions { attribute_bytes.extend_from_slice(&e.to_bytes()); }
            // Sub-attributes: LNT always, LVT in debug
            let mut sub_attrs: Vec<NamedAttribute> = Vec::new();
            {
                let mut lnt = LineNumberTableAttribute::new();
                if pc.line_numbers.is_empty() {
                    let _ = lnt.add_line_number(0, 1);
                } else {
                    // Strictly emulate javac: ensure a decl-line at pc=0 and keep the first statement line
                    // possibly also at pc=0 if provided by collector.
                    let mut saw_pc0 = false;
                    for (pcv, _ln) in &pc.line_numbers {
                        if *pcv == 0 { saw_pc0 = true; break; }
                    }
                    if !saw_pc0 {
                        // If collector didn't add pc=0, inject using the first line number
                        let first_line = pc.line_numbers.first().map(|(_, l)| *l).unwrap_or(1);
                        let _ = lnt.add_line_number(0, first_line);
                    }
                    for (pcv, ln) in &pc.line_numbers { let _ = lnt.add_line_number(*pcv, *ln); }
                }
                sub_attrs.push(make_line_number_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lnt)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
            }
            if self.config.debug {
                let mut lvt = LocalVariableTableAttribute::new();
                // Add implicit 'this' for instance methods at slot 0
                let is_static = (pc.access_flags & access_flags::ACC_STATIC) == access_flags::ACC_STATIC;
                if !is_static {
                    let this_name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("this") }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let this_desc = format!("L{};", self.current_class_name.clone().unwrap_or_else(|| class.name.replace('.', "/")));
                    let this_desc_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&this_desc) }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let entry = LocalVariableEntry { start_pc: 0, length: pc.code_bytes.len() as u16, name_index: this_name_idx, descriptor_index: this_desc_idx, index: 0 };
                    lvt.entries.push(entry).map_err(|e| crate::error::Error::CodeGen { message: format!("LocalVariableTable push failed: {}", e) })?;
                }
                // Add explicit locals (skip synthetic/internal names)
                for lv in &pc.locals {
                    if lv.name.starts_with('$') { continue; }
                    let desc = lv.var_type.descriptor(); if desc.is_empty() { continue; }
                    let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&lv.name) }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let desc_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&desc) }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let entry = LocalVariableEntry { start_pc: lv.start_pc, length: if lv.length == 0 { pc.code_bytes.len() as u16 - lv.start_pc } else { lv.length }, name_index, descriptor_index: desc_index, index: lv.index };
                    lvt.entries.push(entry).map_err(|e| crate::error::Error::CodeGen { message: format!("LocalVariableTable push failed: {}", e) })?;
                }
                sub_attrs.push(make_local_variable_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lvt)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
            }
            attribute_bytes.extend_from_slice(&(sub_attrs.len() as u16).to_be_bytes());
            for a in sub_attrs {
                attribute_bytes.extend_from_slice(&a.name.as_u16().to_be_bytes());
                let payload = { let cp = self.cp_shared.as_ref().unwrap().borrow(); a.info.to_bytes(&cp) };
                attribute_bytes.extend_from_slice(&(payload.len() as u32).to_be_bytes());
                attribute_bytes.extend_from_slice(&payload);
            }
            // Now intern method name/descriptor like javac
            let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&pc.name) }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&pc.descriptor) }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let mut method_info = MethodInfo::new(pc.access_flags, name_index, descriptor_index);
            // Now intern the Code attribute name, after code-related CP touches
            let code_name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Code") }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let named = NamedAttribute::new(code_name_index.into(), AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload: attribute_bytes }));
            method_info.attributes.push(named);

            self.class_file.methods.push(method_info);
        }

        // Synthesize bridge methods for common erased generics patterns (subset).
        // Supports:
        // - Comparable<T>#compareTo(T) -> bridge compareTo(Object)
        // - Comparator<T>#compare(T,T) -> bridge compare(Object,Object)
        // - List<E>#get(int):E -> bridge get(int):Object
        self.synthesize_bridges(class)?;

        // Class-level type-use annotations: extends / implements / class type parameter bounds
        {
            let mut visible: Vec<crate::codegen::attribute::TypeAnnotationEntry> = Vec::new();
            let mut invisible: Vec<crate::codegen::attribute::TypeAnnotationEntry> = Vec::new();
            if let Some(ext) = &class.extends { self.collect_type_use_entries_split(ext, &mut visible, &mut invisible)?; }
            for imp in &class.implements { self.collect_type_use_entries_split(imp, &mut visible, &mut invisible)?; }
            for tp in &class.type_params { for b in &tp.bounds { self.collect_type_use_entries_split(b, &mut visible, &mut invisible)?; } }
            if !visible.is_empty() {
                let name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("RuntimeVisibleTypeAnnotations") }
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                let attr = crate::codegen::attribute::RuntimeVisibleTypeAnnotationsAttribute { annotations: visible };
                self.class_file.attributes.push(NamedAttribute::new(name_idx.into(), AttributeInfo::RuntimeVisibleTypeAnnotations(attr)));
            }
            if !invisible.is_empty() {
                let name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("RuntimeInvisibleTypeAnnotations") }
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                let attr = crate::codegen::attribute::RuntimeInvisibleTypeAnnotationsAttribute { annotations: invisible };
                self.class_file.attributes.push(NamedAttribute::new(name_idx.into(), AttributeInfo::RuntimeInvisibleTypeAnnotations(attr)));
            }
        }

        // Add SourceFile attribute after methods
        let filename = format!("{}.java", class.name);
        if let Ok(attr) = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); crate::codegen::attribute::NamedAttribute::new_source_file_attribute(&mut cp, filename) } {
            self.class_file.attributes.push(attr);
        }

        // Add Signature attribute if class has generic type parameters
        if !class.type_params.is_empty() {
            let type_resolver = TypeNameResolver::with_default_mappings();
            let signature = crate::codegen::signature::class_to_signature(
                class, 
                self.package_name.as_deref(), 
                self.current_class_name.as_deref(), 
                &type_resolver
            );
            let signature_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&signature) }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let signature_attr = crate::codegen::attribute::SignatureAttribute { 
                signature: crate::codegen::typed_index::ConstPoolIndex::from(signature_index) 
            };
            let name_index = self.class_file.constant_pool.try_add_utf8("Signature")
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let named_attr = crate::codegen::attribute::NamedAttribute::new(
                crate::codegen::typed_index::ConstPoolIndex::from(name_index), 
                AttributeInfo::Signature(signature_attr)
            );
            self.class_file.attributes.push(named_attr);
        }

        // Finalize: write back shared pool
        if let Some(cp) = &self.cp_shared { self.class_file.constant_pool = cp.borrow().clone(); }
        self.cp_shared = None;
        
        Ok(())
    }

    fn synthesize_bridges(&mut self, class: &ClassDecl) -> Result<()> {
        // Heuristic: if class implements Comparable and has compareTo with a single reference-typed parameter
        // that is not Object, generate a bridge: public synthetic bridge int compareTo(Object o) { return this.compareTo((T)o); }
        let implements_comparable = class.implements.iter().any(|t| t.name == "Comparable" || t.name.ends_with("Comparable"));
        if !implements_comparable { return Ok(()); }
        // Find concrete compareTo(T) candidate
        let candidate = class.body.iter().find_map(|m| {
            if let ClassMember::Method(md) = m {
                if md.name == "compareTo" && md.parameters.len() == 1 {
                    let pty = &md.parameters[0].type_ref.name;
                    let is_primitive = matches!(pty.as_str(),
                        "int"|"long"|"float"|"double"|"boolean"|"char"|"short"|"byte");
                    if !is_primitive && pty != "Object" { return Some(md); }
                }
            }
            None
        });
        let Some(target) = candidate else { return Ok(()); };
        // Build bridge body: aload_0; aload_1; checkcast <T>; invokevirtual this.compareTo(T)I; ireturn
        let mut code_bytes: Vec<u8> = Vec::new();
        code_bytes.push(opcodes::ALOAD_0); // this
        code_bytes.push(opcodes::ALOAD_1); // param
        // CHECKCAST to parameter type
        let param_desc = super::descriptor::type_to_descriptor(&target.parameters[0].type_ref);
        // Convert descriptor Lpkg/Type; -> internal name pkg/Type for constant pool
        let internal_class_name = if param_desc.starts_with('L') && param_desc.ends_with(';') {
            param_desc[1..param_desc.len()-1].to_string()
        } else if param_desc.ends_with("[]") {
            // Arrays: no checkcast for primitives arrays handling here; emit as Object cast (safe no-op for our subset)
            "java/lang/Object".to_string()
        } else {
            "java/lang/Object".to_string()
        };
        let class_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class(&internal_class_name) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::CHECKCAST);
        code_bytes.extend_from_slice(&class_index.to_be_bytes());
        // INVOKEVIRTUAL this.compareTo(T)I
        let this_internal = self.current_class_name.clone().unwrap_or_else(|| class.name.replace('.', "/"));
        let method_ref = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut();
            let desc_concrete = format!("({})I", super::descriptor::type_to_descriptor(&target.parameters[0].type_ref));
            cp.try_add_method_ref(&this_internal, "compareTo", &desc_concrete)
        }.map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::INVOKEVIRTUAL);
        code_bytes.extend_from_slice(&method_ref.to_be_bytes());
        code_bytes.push(opcodes::IRETURN);

        // Compute simple max_stack
        let mut sim_depth: i32 = 0; let mut sim_max: i32 = 0; let mut i = 0usize;
        while i < code_bytes.len() { let op = code_bytes[i]; match op {
            0x2A /* ALOAD_0 */ => { sim_depth += 1; },
            0x2B /* ALOAD_1 */ => { sim_depth += 1; },
            0xC0 /* CHECKCAST */ => { i += 2; },
            0xB6 /* INVOKEVIRTUAL */ => { sim_depth -= 1; i += 2; },
            0xAC /* IRETURN */ => {},
            _ => {}
        } if sim_depth > sim_max { sim_max = sim_depth; } i += 1; }
        let computed_max_stack = sim_depth.max(sim_max).max(1) as u16;

        // Build Code attribute payload
        let mut attribute_bytes = Vec::new();
        attribute_bytes.extend_from_slice(&computed_max_stack.to_be_bytes());
        attribute_bytes.extend_from_slice(&2u16.to_be_bytes()); // max_locals: this + param
        attribute_bytes.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        attribute_bytes.extend_from_slice(&code_bytes);
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes()); // exceptions count
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes()); // attributes count

        // MethodInfo: public | bridge | synthetic
        let access = access_flags::ACC_PUBLIC | access_flags::ACC_BRIDGE | access_flags::ACC_SYNTHETIC;
        let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("compareTo") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("(Ljava/lang/Object;)I") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let mut method_info = MethodInfo::new(access, name_index, descriptor_index);
        // Name of Code attribute
        let code_name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Code") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let named = NamedAttribute::new(code_name_index.into(), AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload: attribute_bytes }));
        method_info.attributes.push(named);
        self.class_file.methods.push(method_info);
        // Comparator<T>#compare(T,T) -> compare(Object,Object)I
        self.synthesize_comparator_bridge(class)?;
        // List<E>#get(I)TE -> get(I)Ljava/lang/Object;
        self.synthesize_list_get_bridge(class)?;
        Ok(())
    }

    fn synthesize_comparator_bridge(&mut self, class: &ClassDecl) -> Result<()> {
        let implements_comparator = class.implements.iter().any(|t| t.name == "Comparator" || t.name.ends_with("Comparator"));
        if !implements_comparator { return Ok(()) }
        // Find concrete compare(T,T)
        let target = class.body.iter().find_map(|m| {
            if let ClassMember::Method(md) = m {
                if md.name == "compare" && md.parameters.len() == 2 {
                    let p0 = &md.parameters[0].type_ref.name; let p1 = &md.parameters[1].type_ref.name;
                    let is_prim = |s: &str| matches!(s, "int"|"long"|"float"|"double"|"boolean"|"char"|"short"|"byte");
                    if !is_prim(p0) && !is_prim(p1) && p0 != "Object" && p1 != "Object" {
                        return Some(md);
                    }
                }
            }
            None
        });
        let Some(target) = target else { return Ok(()) };
        let mut code_bytes: Vec<u8> = Vec::new();
        // aload_0, aload_1, checkcast T0, aload_2, checkcast T1, invokevirtual compare(T0,T1)I, ireturn
        code_bytes.push(opcodes::ALOAD_0);
        code_bytes.push(opcodes::ALOAD_1);
        let t0_desc = super::descriptor::type_to_descriptor(&target.parameters[0].type_ref);
        let t0_cls = if t0_desc.starts_with('L') && t0_desc.ends_with(';') { t0_desc[1..t0_desc.len()-1].to_string() } else { "java/lang/Object".to_string() };
        let t0_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class(&t0_cls) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::CHECKCAST); code_bytes.extend_from_slice(&t0_idx.to_be_bytes());
        code_bytes.push(opcodes::ALOAD_2);
        let t1_desc = super::descriptor::type_to_descriptor(&target.parameters[1].type_ref);
        let t1_cls = if t1_desc.starts_with('L') && t1_desc.ends_with(';') { t1_desc[1..t1_desc.len()-1].to_string() } else { "java/lang/Object".to_string() };
        let t1_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class(&t1_cls) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::CHECKCAST); code_bytes.extend_from_slice(&t1_idx.to_be_bytes());
        let this_internal = self.current_class_name.clone().unwrap_or_else(|| class.name.replace('.', "/"));
        let desc_concrete = format!("({}{})I", super::descriptor::type_to_descriptor(&target.parameters[0].type_ref), super::descriptor::type_to_descriptor(&target.parameters[1].type_ref));
        let mref = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(&this_internal, "compare", &desc_concrete) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::INVOKEVIRTUAL); code_bytes.extend_from_slice(&mref.to_be_bytes());
        code_bytes.push(opcodes::IRETURN);
        // simple max_stack
        let computed_max_stack: u16 = 3; // this + 2 checked casts is safe upper bound for this stub
        let mut payload = Vec::new();
        payload.extend_from_slice(&computed_max_stack.to_be_bytes());
        payload.extend_from_slice(&3u16.to_be_bytes()); // max_locals: this, o1, o2
        payload.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes()); payload.extend_from_slice(&code_bytes);
        payload.extend_from_slice(&0u16.to_be_bytes()); payload.extend_from_slice(&0u16.to_be_bytes());
        let access = access_flags::ACC_PUBLIC | access_flags::ACC_BRIDGE | access_flags::ACC_SYNTHETIC;
        let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("compare") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("(Ljava/lang/Object;Ljava/lang/Object;)I") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let mut mi = MethodInfo::new(access, name_index, descriptor_index);
        let code_name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Code") }.map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        mi.attributes.push(NamedAttribute::new(code_name_index.into(), AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload })));
        self.class_file.methods.push(mi);
        Ok(())
    }

    fn synthesize_list_get_bridge(&mut self, class: &ClassDecl) -> Result<()> {
        let implements_list = class.implements.iter().any(|t| t.name == "List" || t.name.ends_with("List"));
        if !implements_list { return Ok(()) }
        // Find concrete get(int)
        let target = class.body.iter().find_map(|m| {
            if let ClassMember::Method(md) = m {
                if md.name == "get" && md.parameters.len() == 1 && md.parameters[0].type_ref.name == "int" {
                    // Return type must be reference and not Object to warrant a bridge
                    if let Some(rt) = &md.return_type { if rt.name != "Object" { return Some(md); } }
                }
            }
            None
        });
        let Some(target) = target else { return Ok(()) };
        // aload_0, iload_1, invokevirtual get(I)T, areturn
        let mut code_bytes: Vec<u8> = Vec::new();
        code_bytes.push(opcodes::ALOAD_0);
        code_bytes.push(0x1B); // ILOAD_1
        let this_internal = self.current_class_name.clone().unwrap_or_else(|| class.name.replace('.', "/"));
        let desc_concrete = format!("(I){}", super::descriptor::type_to_descriptor(&target.return_type.clone().unwrap()));
        let mref = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(&this_internal, "get", &desc_concrete) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::INVOKEVIRTUAL); code_bytes.extend_from_slice(&mref.to_be_bytes());
        code_bytes.push(opcodes::ARETURN);
        let computed_max_stack: u16 = 2;
        let mut payload = Vec::new();
        payload.extend_from_slice(&computed_max_stack.to_be_bytes());
        payload.extend_from_slice(&2u16.to_be_bytes()); // this + int
        payload.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes()); payload.extend_from_slice(&code_bytes);
        payload.extend_from_slice(&0u16.to_be_bytes()); payload.extend_from_slice(&0u16.to_be_bytes());
        let access = access_flags::ACC_PUBLIC | access_flags::ACC_BRIDGE | access_flags::ACC_SYNTHETIC;
        let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("get") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("(I)Ljava/lang/Object;") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let mut mi = MethodInfo::new(access, name_index, descriptor_index);
        let code_name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Code") }.map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        mi.attributes.push(NamedAttribute::new(code_name_index.into(), AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload })));
        self.class_file.methods.push(mi);
        Ok(())
    }
    
    /// Generate bytecode for an interface field (implicitly public, static, final)
    fn generate_interface_field(&mut self, field: &FieldDecl) -> Result<()> {
        let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&field.name) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&type_to_descriptor(&field.type_ref)) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Interface fields are implicitly public, static, and final
        let access_flags = access_flags::ACC_PUBLIC | access_flags::ACC_STATIC | access_flags::ACC_FINAL;
        
        let field_info = FieldInfo {
            access_flags,
            name_index,
            descriptor_index,
            attributes: vec![],
        };
        
        self.class_file.fields.push(field_info);
        Ok(())
    }
    
    /// Generate bytecode for an interface method (implicitly public and abstract)
    fn generate_interface_method(&mut self, method: &MethodDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.try_add_utf8(&method.name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = self.class_file.constant_pool.try_add_utf8(&self.generate_method_descriptor(method))
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Interface methods are implicitly public and abstract
        let access_flags = access_flags::ACC_PUBLIC | access_flags::ACC_ABSTRACT;
        
        let mut method_info = MethodInfo {
            access_flags,
            name_index,
            descriptor_index,
            attributes: vec![],
        };

        // Add Signature attribute if method has generic type parameters or uses generic types
        if !method.type_params.is_empty() ||
           method.parameters.iter().any(|p| !p.type_ref.type_args.is_empty() || (p.type_ref.name.len() == 1 && p.type_ref.name.chars().next().unwrap().is_ascii_uppercase())) ||
           (method.return_type.as_ref().map(|rt| !rt.type_args.is_empty() || (rt.name.len() == 1 && rt.name.chars().next().unwrap().is_ascii_uppercase())).unwrap_or(false)) {
            use crate::codegen::signature::{method_to_signature, TypeNameResolver};
            let type_resolver = TypeNameResolver::with_default_mappings();
            let signature_string = method_to_signature(method, self.package_name.as_deref(), self.current_class_name.as_deref(), &type_resolver);
            let signature_index = self.class_file.constant_pool.try_add_utf8(&signature_string)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let signature_attr = crate::codegen::attribute::SignatureAttribute { 
                signature: crate::codegen::typed_index::ConstPoolIndex::from(signature_index) 
            };
            let name_index = self.class_file.constant_pool.try_add_utf8("Signature")
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let named_attr = crate::codegen::attribute::NamedAttribute::new(
                crate::codegen::typed_index::ConstPoolIndex::from(name_index), 
                AttributeInfo::Signature(signature_attr)
            );
            method_info.attributes.push(named_attr);
        }

        // Add Exceptions attribute if method has throws declarations
        if !method.throws.is_empty() {
            let mut exception_indexes = Vec::new();
            for throws_type in &method.throws {
                // Convert throws type to internal name format
                let internal_name = if throws_type.name.contains('.') || throws_type.name.contains('/') {
                    throws_type.name.clone()
                } else if crate::consts::JAVA_LANG_SIMPLE_TYPES.contains(&throws_type.name.as_str()) {
                    format!("java/lang/{}", throws_type.name)
                } else if throws_type.name == "IOException" {
                    "java/io/IOException".to_string()
                } else if let Some(resolved_name) = crate::codegen::classpath::resolve_class_name(&throws_type.name) {
                    resolved_name.to_string()
                } else {
                    // Fallback: assume it's in the same package
                    if let Some(pkg) = &self.package_name {
                        format!("{}/{}", pkg.replace('.', "/"), throws_type.name)
                    } else {
                        throws_type.name.clone()
                    }
                };
                
                let exception_index = self.class_file.constant_pool.try_add_class(&internal_name)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                exception_indexes.push(exception_index.into());
            }
            
            let exceptions_attr = crate::codegen::attribute::NamedAttribute::new_exceptions_attribute(
                &mut self.class_file.constant_pool, 
                exception_indexes
            ).map_err(|e| crate::error::Error::CodeGen { message: format!("const pool:Exception: {}", e) })?;
            method_info.attributes.push(exceptions_attr);
        }
        
        self.class_file.methods.push(method_info);
        Ok(())
    }
    
    /// Generate bytecode for an enum constant
    fn generate_enum_constant(&mut self, constant: &EnumConstant, enum_name: &str) -> Result<()> {
        let name_index = self.class_file.constant_pool.try_add_utf8(&constant.name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = self.class_file.constant_pool.try_add_utf8(&format!("L{};", enum_name))
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Enum constants are public static final
        let access_flags = access_flags::ACC_PUBLIC | access_flags::ACC_STATIC | access_flags::ACC_FINAL;
        
        let field_info = FieldInfo {
            access_flags,
            name_index,
            descriptor_index,
            attributes: vec![],
        };
        
        self.class_file.fields.push(field_info);
        Ok(())
    }
    
    /// Generate bytecode for an enum constructor
    fn generate_enum_constructor(&mut self, _enum_decl: &EnumDecl) -> Result<()> {
        // Enum constructors are private and take name and ordinal parameters
        let name_index = self.class_file.constant_pool.try_add_utf8("<init>")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = self.class_file.constant_pool.try_add_utf8("(Ljava/lang/String;I)V")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        let access_flags = access_flags::ACC_PRIVATE;
        
        let method_info = MethodInfo {
            access_flags,
            name_index,
            descriptor_index,
            attributes: vec![],
        };
        
        self.class_file.methods.push(method_info);
        Ok(())
    }
    
    /// Generate bytecode for an annotation declaration
    pub fn generate_annotation(&mut self, annotation: &AnnotationDecl) -> Result<()> {
        use super::annotation::generate_annotation;
        generate_annotation(annotation, &mut self.class_file)
    }
    
    /// Generate bytecode for a field
    fn generate_field(&mut self, field: &FieldDecl) -> Result<()> {
        // Use cp_shared if available, otherwise fall back to class_file.constant_pool
        let name_index = if let Some(cp) = &self.cp_shared {
            cp.borrow_mut().try_add_utf8(&field.name)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?
        } else {
            self.class_file.constant_pool.try_add_utf8(&field.name)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?
        };
        
        let descriptor_index = if let Some(cp) = &self.cp_shared {
            cp.borrow_mut().try_add_utf8(&type_to_descriptor(&field.type_ref))
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?
        } else {
            self.class_file.constant_pool.try_add_utf8(&type_to_descriptor(&field.type_ref))
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?
        };
        
        let access_flags = modifiers_to_flags(&field.modifiers);
        
        let mut field_info = FieldInfo::new(access_flags, name_index, descriptor_index);
        // ConstantValue for static final primitives or String literals
        if field.modifiers.iter().any(|m| matches!(m, Modifier::Static))
            && field.modifiers.iter().any(|m| matches!(m, Modifier::Final))
        {
            if let Some(init) = &field.initializer {
                use crate::ast::{Expr, Literal};
                let add_const_attr = |cw: &mut ClassWriter, _tag: &str, idx: u16, fi: &mut FieldInfo| -> Result<()> {
                    // ConstantValue attribute payload: u2 constantvalue_index
                    let name_index = { let mut cp = cw.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("ConstantValue") }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    fi.attributes.push(NamedAttribute::new(name_index.into(), AttributeInfo::ConstantValue(crate::codegen::attribute::ConstantValueAttribute { value: idx.into() })));
                    Ok(())
                };
                // Evaluate compile-time constant expression if possible
                let folded = Self::eval_compile_time_constant(init).or_else(|| match init { Expr::Literal(l) => Some(l.value.clone()), _ => None });
                if let Some(val) = folded {
                    match &val {
                        Literal::Integer(i) => {
                            let idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.add_integer(*i as i32) };
                            add_const_attr(self, "Integer", idx, &mut field_info)?;
                        }
                        Literal::Float(f) => {
                            // Our AST uses f64 for all floats; choose ConstantFloat if fits, else ConstantDouble
                            if (*f as f32) as f64 == *f {
                                let idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.add_float(*f as f32) };
                                add_const_attr(self, "Float", idx, &mut field_info)?;
                            } else {
                                let idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.add_double(*f) };
                                add_const_attr(self, "Double", idx, &mut field_info)?;
                            }
                        }
                        Literal::Char(c) => {
                            let idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.add_integer(*c as i32) };
                            add_const_attr(self, "Float", idx, &mut field_info)?;
                        }
                        Literal::Boolean(b) => {
                            let v = if *b { 1 } else { 0 };
                            let idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.add_integer(v) };
                            add_const_attr(self, "Boolean", idx, &mut field_info)?;
                        }
                        Literal::String(s) => {
                            let idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.add_string(s) };
                            add_const_attr(self, "String", idx, &mut field_info)?;
                        }
                        _ => {}
                    }
                }
            }
        }
        // Emit field type-use annotations (visible/invisible)
        {
            let mut visible: Vec<crate::codegen::attribute::TypeAnnotationEntry> = Vec::new();
            let mut invisible: Vec<crate::codegen::attribute::TypeAnnotationEntry> = Vec::new();
            self.collect_type_use_entries_split(&field.type_ref, &mut visible, &mut invisible)?;
            if !visible.is_empty() {
                let name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("RuntimeVisibleTypeAnnotations") }
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                let attr = crate::codegen::attribute::RuntimeVisibleTypeAnnotationsAttribute { annotations: visible };
                field_info.attributes.push(NamedAttribute::new(name_idx.into(), AttributeInfo::RuntimeVisibleTypeAnnotations(attr)));
            }
            if !invisible.is_empty() {
                let name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("RuntimeInvisibleTypeAnnotations") }
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                let attr = crate::codegen::attribute::RuntimeInvisibleTypeAnnotationsAttribute { annotations: invisible };
                field_info.attributes.push(NamedAttribute::new(name_idx.into(), AttributeInfo::RuntimeInvisibleTypeAnnotations(attr)));
            }
        }
        self.class_file.fields.push(field_info);
        
        Ok(())
    }
    
    /// Generate bytecode for a method
    fn generate_method(&mut self, method: &MethodDecl) -> Result<()> {
        let access_flags = modifiers_to_flags(&method.modifiers);
        
        // Generate method body first for CP ordering parity
        let code_attr_info_opt = if !method.modifiers.contains(&Modifier::Abstract) {
            Some(self.generate_code_attribute(method)?)
        } else { None };
        
        // Now add name/descriptor to CP
        let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&method.name) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor = self.generate_method_descriptor(method);
        let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&descriptor) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        if let Some(code_attr_info) = code_attr_info_opt {
            let _name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Code") }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let named = NamedAttribute::new(_name_index.into(), code_attr_info);
            method_info.attributes.push(named);
        }

        // Add Signature attribute if method has generic type parameters or uses generic types
        let has_generics = !method.type_params.is_empty() ||
           method.parameters.iter().any(|p| !p.type_ref.type_args.is_empty() || 
               (p.type_ref.name.len() == 1 && p.type_ref.name.chars().next().unwrap().is_ascii_uppercase()) ||
               p.type_ref.name.contains("List") || p.type_ref.name.contains("Map") || p.type_ref.name.contains("Set")) ||
           (method.return_type.as_ref().map(|rt| !rt.type_args.is_empty() || 
               (rt.name.len() == 1 && rt.name.chars().next().unwrap().is_ascii_uppercase()) ||
               rt.name.contains("List") || rt.name.contains("Map") || rt.name.contains("Set")).unwrap_or(false));
        
        if has_generics {
            use crate::codegen::signature::{method_to_signature, TypeNameResolver};
            let type_resolver = TypeNameResolver::with_default_mappings();
            let signature_string = method_to_signature(method, self.package_name.as_deref(), self.current_class_name.as_deref(), &type_resolver);
            let signature_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&signature_string) }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let signature_attr = crate::codegen::attribute::SignatureAttribute {
                signature: crate::codegen::typed_index::ConstPoolIndex::from(signature_index)
            };
            let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Signature") }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let named_attr = crate::codegen::attribute::NamedAttribute::new(
                crate::codegen::typed_index::ConstPoolIndex::from(name_index),
                AttributeInfo::Signature(signature_attr)
            );
            method_info.attributes.push(named_attr);
        }

        // Add Exceptions attribute if method has throws declarations
        if !method.throws.is_empty() {
            let mut exception_indexes: Vec<crate::codegen::typed_index::ClassIndex> = Vec::new();
            for throws_type in &method.throws {
                // Convert throws type to internal name format
                let internal_name = if throws_type.name.contains('.') || throws_type.name.contains('/') {
                    throws_type.name.clone()
                } else if crate::consts::JAVA_LANG_SIMPLE_TYPES.contains(&throws_type.name.as_str()) {
                    format!("java/lang/{}", throws_type.name)
                } else if throws_type.name == "IOException" {
                    "java/io/IOException".to_string()
                } else if let Some(resolved_name) = crate::codegen::classpath::resolve_class_name(&throws_type.name) {
                    resolved_name.to_string()
                } else {
                    // Fallback: assume it's in the same package
                    if let Some(pkg) = &self.package_name {
                        format!("{}/{}", pkg.replace('.', "/"), throws_type.name)
                    } else {
                        throws_type.name.clone()
                    }
                };

                let exception_index_u16 = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class(&internal_name) }
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                exception_indexes.push(crate::codegen::typed_index::ConstPoolIndex::from(exception_index_u16));
            }

            let exceptions_attr = {
                let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut();
                crate::codegen::attribute::NamedAttribute::new_exceptions_attribute(&mut cp, exception_indexes.into_iter().map(|i| i.into()).collect())
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool:Exception: {}", e) })?
            };
            method_info.attributes.push(exceptions_attr);
        }

        // Emit RuntimeVisible/InvisibleTypeAnnotations for method return/params/throws and method type parameter bounds
        let mut visible: Vec<crate::codegen::attribute::TypeAnnotationEntry> = Vec::new();
        let mut invisible: Vec<crate::codegen::attribute::TypeAnnotationEntry> = Vec::new();
        if let Some(rt) = &method.return_type { self.collect_type_use_entries_split(rt, &mut visible, &mut invisible)?; }
        for p in &method.parameters { self.collect_type_use_entries_split(&p.type_ref, &mut visible, &mut invisible)?; }
        for t in &method.throws { self.collect_type_use_entries_split(t, &mut visible, &mut invisible)?; }
        for tp in &method.type_params { for b in &tp.bounds { self.collect_type_use_entries_split(b, &mut visible, &mut invisible)?; } }
        if !visible.is_empty() {
            let name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("RuntimeVisibleTypeAnnotations") }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let attr = crate::codegen::attribute::RuntimeVisibleTypeAnnotationsAttribute { annotations: visible };
            method_info.attributes.push(NamedAttribute::new(name_idx.into(), AttributeInfo::RuntimeVisibleTypeAnnotations(attr)));
        }
        if !invisible.is_empty() {
            let name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("RuntimeInvisibleTypeAnnotations") }
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let attr = crate::codegen::attribute::RuntimeInvisibleTypeAnnotationsAttribute { annotations: invisible };
            method_info.attributes.push(NamedAttribute::new(name_idx.into(), AttributeInfo::RuntimeInvisibleTypeAnnotations(attr)));
        }
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }

    fn collect_type_use_entries_deep(&mut self, tref: &TypeRef, out: &mut Vec<crate::codegen::attribute::TypeAnnotationEntry>) -> Result<()> {
        // Current node
        if !tref.annotations.is_empty() {
            for ann in &tref.annotations {
                let type_name_utf = {
                    let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut();
                    let desc = if ann.name.starts_with('L') && ann.name.ends_with(';') {
                        ann.name.clone()
                    } else {
                        let mut s = ann.name.replace('.', "/");
                        if !s.starts_with('L') { s = format!("L{};", s); }
                        s
                    };
                    cp.try_add_utf8(&desc).map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?
                };
                out.push(crate::codegen::attribute::TypeAnnotationEntry {
                    type_name: type_name_utf.into(),
                    retention: Some(crate::codegen::attribute::RetentionPolicy::Runtime),
                    targets: vec![crate::codegen::attribute::AnnotationTarget::TypeUse],
                });
            }
        }
        // Recurse into type arguments
        for ta in &tref.type_args {
            match ta {
                crate::ast::TypeArg::Type(inner) => { self.collect_type_use_entries_deep(inner, out)?; }
                crate::ast::TypeArg::Wildcard(w) => {
                    if let Some((_k, btr)) = &w.bound { self.collect_type_use_entries_deep(btr, out)?; }
                }
            }
        }
        Ok(())
    }

    fn collect_type_use_entries_split(&mut self, tref: &TypeRef, out_visible: &mut Vec<crate::codegen::attribute::TypeAnnotationEntry>, out_invisible: &mut Vec<crate::codegen::attribute::TypeAnnotationEntry>) -> Result<()> {
        // Current node
        if !tref.annotations.is_empty() {
            for ann in &tref.annotations {
                let type_name_utf = {
                    let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut();
                    let desc = if ann.name.starts_with('L') && ann.name.ends_with(';') {
                        ann.name.clone()
                    } else {
                        let mut s = ann.name.replace('.', "/");
                        if !s.starts_with('L') { s = format!("L{};", s); }
                        s
                    };
                    cp.try_add_utf8(&desc).map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?
                };
                let retention = self.infer_annotation_retention(&ann.name);
                let entry = crate::codegen::attribute::TypeAnnotationEntry {
                    type_name: type_name_utf.into(),
                    retention: retention.clone(),
                    targets: vec![crate::codegen::attribute::AnnotationTarget::TypeUse],
                };
                match retention {
                    Some(crate::codegen::attribute::RetentionPolicy::Runtime) => out_visible.push(entry),
                    Some(crate::codegen::attribute::RetentionPolicy::Class) => out_invisible.push(entry),
                    _ => {}
                }
            }
        }
        // Recurse into type arguments and wildcard bounds
        for ta in &tref.type_args {
            match ta {
                crate::ast::TypeArg::Type(inner) => { self.collect_type_use_entries_split(inner, out_visible, out_invisible)?; }
                crate::ast::TypeArg::Wildcard(w) => {
                    if let Some((_k, btr)) = &w.bound { self.collect_type_use_entries_split(btr, out_visible, out_invisible)?; }
                }
            }
        }
        Ok(())
    }

    fn infer_annotation_retention(&self, ann_name: &str) -> Option<crate::codegen::attribute::RetentionPolicy> {
        // Try exact match
        if let Some(p) = self.annotation_retention.get(ann_name) { return Some(p.clone()); }
        // Try fully-qualified with current package
        if !ann_name.contains('.') {
            if let Some(pkg) = &self.package_name {
                let fq = format!("{}.{}", pkg, ann_name);
                if let Some(p) = self.annotation_retention.get(&fq) { return Some(p.clone()); }
            }
        }
        // Try simple-name from fq keys
        if !ann_name.contains('.') {
            for (k, v) in &self.annotation_retention { if k.rsplit('.').next() == Some(ann_name) { return Some(v.clone()); } }
        }
        // Default to Runtime when unknown
        Some(crate::codegen::attribute::RetentionPolicy::Runtime)
    }
    
    /// Generate bytecode for a constructor
    fn generate_constructor(&mut self, constructor: &ConstructorDecl) -> Result<()> {
        let access_flags = modifiers_to_flags(&constructor.modifiers);
        
        // Generate constructor body first for CP ordering parity
        let code_attr_info = self.generate_constructor_code_attribute_from_body(constructor)?;
        
        // Now add method info
        let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("<init>") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor = self.generate_constructor_descriptor(constructor);
        let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&descriptor) }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        let _name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Code") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let named = NamedAttribute::new(_name_index.into(), code_attr_info);
        method_info.attributes.push(named);
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }
    
    /// Generate default constructor for a class
    fn generate_default_constructor(&mut self, class: &ClassDecl) -> Result<()> {
        let access_flags = access_flags::ACC_PUBLIC;
        // Generate constructor body first for CP ordering parity
        let code_attr_info = self.generate_constructor_code_attribute(class)?;
        
        // Now add method info
        let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("<init>") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("()V") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        let _name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("Code") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let named = NamedAttribute::new(_name_index.into(), code_attr_info);
        method_info.attributes.push(named);
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }
    
    /// Generate method descriptor
    fn generate_method_descriptor(&self, method: &MethodDecl) -> String {
        let mut descriptor = String::new();
        descriptor.push('(');
        
        // Parameter types
        for param in &method.parameters {
            descriptor.push_str(&type_to_descriptor(&param.type_ref));
        }
        
        descriptor.push(')');
        
        // Return type
        if let Some(return_type) = &method.return_type {
            descriptor.push_str(&type_to_descriptor(return_type));
        } else {
            descriptor.push('V'); // void
        }
        
        descriptor
    }
    
    /// Generate constructor descriptor
    fn generate_constructor_descriptor(&self, constructor: &ConstructorDecl) -> String {
        let mut descriptor = String::new();
        descriptor.push('(');
        
        // Parameter types
        for param in &constructor.parameters {
            descriptor.push_str(&type_to_descriptor(&param.type_ref));
        }
        
        descriptor.push(')');
        
        // Return type (void)
        descriptor.push('V');
        
        descriptor
    }
    
    /// Generate code attribute for a method
    fn generate_code_attribute(&mut self, method: &MethodDecl) -> Result<AttributeInfo> {
        // Generate bytecode for method body, using shared pool if available
        let constant_pool_rc = if let Some(rc) = &self.cp_shared { rc.clone() } else { std::rc::Rc::new(std::cell::RefCell::new(self.class_file.constant_pool.clone())) };
        let mut code_writer = BodyWriter::new_with_constant_pool_and_class_decl(constant_pool_rc.clone(), self.current_class_decl.clone().ok_or_else(|| crate::error::Error::Internal { message: "current_class_decl not set".into() })?);
        if let Some(all_types) = &self.all_types {
            code_writer.set_all_types(all_types.clone());
        }
        code_writer.generate_method_body(method)?;
        let (code_bytes, _max_stack, max_locals, exceptions, locals, line_numbers) = code_writer.finalize();
        // Merge back constant pool if not using shared
        if self.cp_shared.is_none() { self.class_file.constant_pool = constant_pool_rc.borrow().clone(); }
        
        // Prefer the max_stack computed by BytecodeBuilder's StackState
        let computed_max_stack = _max_stack;

        // Build sub-attributes after body emissions (so CP first-touches are from code first)
        let mut sub_attrs: Vec<NamedAttribute> = Vec::new();
        if self.config.emit_frames {
            // Build handler types map from exception table entries
            let mut handlers: Vec<(u16, Option<String>)> = Vec::new();
            {
                let cp_ref = self.cp_shared.as_ref().unwrap();
                let cp_borrow = cp_ref.borrow();
                for e in &exceptions {
                    let ty = if e.catch_type != 0 {
                        // Resolve Class name index → Utf8
                        if let Some(crate::codegen::constpool::Constant::Class(name_idx)) = cp_borrow.constants.get((e.catch_type - 1) as usize) {
                            if let Some(crate::codegen::constpool::Constant::Utf8(s)) = cp_borrow.constants.get((*name_idx - 1) as usize) {
                                Some(s.clone())
                            } else { None }
                        } else { None }
                    } else { None };
                    handlers.push((e.handler_pc, ty));
                }
            }
            let is_static = method.modifiers.iter().any(|m| matches!(m, Modifier::Static));
            let is_constructor = false;
            let this_internal = self.current_class_name.clone().unwrap_or_default();
            let method_desc = self.generate_method_descriptor(method);
            let smt = FrameBuilder::new().compute_with_types_with_handlers(
                &code_bytes,
                &handlers,
                is_static,
                is_constructor,
                &this_internal,
                &method_desc,
                &mut self.cp_shared.as_ref().unwrap().borrow_mut(),
            );
            if self.config.debug {
                for line in describe_stack_map_frames(&smt) { eprintln!("[frames] {}", line); }
            }
            sub_attrs.push(make_stack_map_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &smt.frames)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        // Always emit LineNumberTable to match javac default
        {
            let mut lnt = LineNumberTableAttribute::new();
            // Always add a method-declaration line entry at pc=0, like javac
            let decl_line = (method.span.start.line as u16).max(1);
            lnt.add_line_number(0, decl_line).map_err(|e| crate::error::Error::CodeGen { message: format!("add_line_number failed: {}", e) })?;
            // Heuristic: if the first non-zero-pc entry has the same line as decl_line,
            // bump it by +1 to better match javac (which maps the first range [0..next) to the decl line).
            let mut first_nonzero_seen = false;
            for (pc, line) in &line_numbers {
                if *pc == 0 { continue; }
                let mut out_line = *line;
                if !first_nonzero_seen {
                    first_nonzero_seen = true;
                    if out_line == decl_line { out_line = decl_line.saturating_add(1); }
                }
                lnt.add_line_number(*pc, out_line).map_err(|e| crate::error::Error::CodeGen { message: format!("add_line_number failed: {}", e) })?;
            }
            sub_attrs.push(make_line_number_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lnt)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        // LocalVariableTable only in debug mode
            if self.config.debug {
                let mut lvt = LocalVariableTableAttribute::new();
                // Emit implicit 'this' for instance methods
                let is_static_method = method.modifiers.iter().any(|m| matches!(m, Modifier::Static));
                if !is_static_method {
                    let this_name_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8("this") }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let this_internal = self.current_class_name.clone().unwrap_or_default();
                    let this_desc = if this_internal.is_empty() { "Ljava/lang/Object;".to_string() } else { format!("L{};", this_internal) };
                    let this_desc_idx = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&this_desc) }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let entry = LocalVariableEntry { start_pc: 0, length: code_bytes.len() as u16, name_index: this_name_idx, descriptor_index: this_desc_idx, index: 0 };
                    lvt.entries.push(entry).map_err(|e| crate::error::Error::CodeGen { message: format!("LocalVariableTable push failed: {}", e) })?;
                }
                for lv in &locals {
                    if lv.name.starts_with('$') { continue; }
                    let desc = lv.var_type.descriptor();
                    if desc.is_empty() { continue; }
                let name_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&lv.name) }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                let desc_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_utf8(&desc) }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let entry = LocalVariableEntry {
                        start_pc: lv.start_pc,
                        length: if lv.length == 0 { code_bytes.len() as u16 - lv.start_pc } else { lv.length },
                        name_index,
                        descriptor_index: desc_index,
                        index: lv.index,
                    };
                    lvt.entries.push(entry).map_err(|e| crate::error::Error::CodeGen { message: format!("LocalVariableTable push failed: {}", e) })?;
                }
            sub_attrs.push(make_local_variable_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lvt)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        // Now build the Code attribute bytes at the end to ensure name Utf8 comes after code touches
        let mut attribute_bytes = Vec::new();
        // Max stack and max locals (from BytecodeBuilder)
        attribute_bytes.extend_from_slice(&computed_max_stack.to_be_bytes());
        attribute_bytes.extend_from_slice(&(max_locals as u16).to_be_bytes());
        // Code length and code
        attribute_bytes.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        attribute_bytes.extend_from_slice(&code_bytes);
        // Exception table
        attribute_bytes.extend_from_slice(&(exceptions.len() as u16).to_be_bytes());
        for e in &exceptions { attribute_bytes.extend_from_slice(&e.to_bytes()); }
        // Sub-attributes
        attribute_bytes.extend_from_slice(&(sub_attrs.len() as u16).to_be_bytes());
        for a in sub_attrs {
            attribute_bytes.extend_from_slice(&a.name.as_u16().to_be_bytes());
            let payload = { let cp = self.cp_shared.as_ref().unwrap().borrow(); a.info.to_bytes(&cp) };
            attribute_bytes.extend_from_slice(&(payload.len() as u32).to_be_bytes());
            attribute_bytes.extend_from_slice(&payload);
        }
        
        Ok(AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload: attribute_bytes }))
    }
    
    /// Generate constructor code attribute
    fn generate_constructor_code_attribute(&mut self, class: &ClassDecl) -> Result<AttributeInfo> {
        // Generate simple constructor bytecode manually
        let mut code_bytes = Vec::new();
        
        // ALOAD_0 (load this)
        code_bytes.push(opcodes::ALOAD_0);
        
        // INVOKESPECIAL java/lang/Object.<init>()V
        let super_class_name = class.extends.as_ref().map(|t| t.name.as_str()).unwrap_or("java/lang/Object");
        let method_ref_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(super_class_name, "<init>", "()V") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::INVOKESPECIAL);
        code_bytes.extend_from_slice(&method_ref_index.to_be_bytes());
        
        // RETURN
        code_bytes.push(opcodes::RETURN);
        
        // Create code attribute
        let mut attribute_bytes = Vec::new();
        
        // Max stack and max locals
        attribute_bytes.extend_from_slice(&1u16.to_be_bytes()); // max_stack = 1
        attribute_bytes.extend_from_slice(&1u16.to_be_bytes()); // max_locals = 1
        
        // Code length and code
        attribute_bytes.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        attribute_bytes.extend_from_slice(&code_bytes);
        
        // Exception table (empty for now)
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes());
        
        // Sub-attributes of Code
        let mut sub_attrs: Vec<NamedAttribute> = Vec::new();
        if self.config.emit_frames {
            let handlers: Vec<(u16, Option<String>)> = Vec::new();
            let is_static = false;
            let is_constructor = true;
            let this_internal = self.current_class_name.clone().unwrap_or_else(|| class.name.replace('.', "/"));
            let method_desc = "()V".to_string();
            let smt = FrameBuilder::new().compute_with_types_with_handlers(
                &code_bytes,
                &handlers,
                is_static,
                is_constructor,
                &this_internal,
                &method_desc,
                &mut self.cp_shared.as_ref().unwrap().borrow_mut(),
            );
            if self.config.debug {
                for line in describe_stack_map_frames(&smt) { eprintln!("[frames] {}", line); }
            }
            sub_attrs.push(make_stack_map_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &smt.frames)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        if self.config.debug {
            let mut lnt = LineNumberTableAttribute::new();
            // javac includes a declaration line at pc=0 for constructors as well
            let line = class.span.start.line as u16;
            let _ = lnt.add_line_number(0, line.max(1));
            sub_attrs.push(make_line_number_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lnt)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        attribute_bytes.extend_from_slice(&(sub_attrs.len() as u16).to_be_bytes());
        for a in sub_attrs {
            attribute_bytes.extend_from_slice(&a.name.as_u16().to_be_bytes());
            let payload = { let cp = self.cp_shared.as_ref().unwrap().borrow(); a.info.to_bytes(&cp) };
            attribute_bytes.extend_from_slice(&(payload.len() as u32).to_be_bytes());
            attribute_bytes.extend_from_slice(&payload);
        }
        
        Ok(AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload: attribute_bytes }))
    }

    /// Generate constructor code attribute from a constructor body
    fn generate_constructor_code_attribute_from_body(&mut self, constructor: &ConstructorDecl) -> Result<AttributeInfo> {
        // Generate constructor bytecode manually
        let mut code_bytes = Vec::new();
        
        // ALOAD_0 (load this)
        code_bytes.push(opcodes::ALOAD_0);
        
        // INVOKESPECIAL java/lang/Object.<init>()V
        let super_class_name = "java/lang/Object";
        let method_ref_index = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(super_class_name, "<init>", "()V") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        code_bytes.push(opcodes::INVOKESPECIAL);
        code_bytes.extend_from_slice(&method_ref_index.to_be_bytes());
        
        // Generate constructor body statements (simplified for now)
        // TODO: Implement proper statement generation
        
        // RETURN
        code_bytes.push(opcodes::RETURN);
        
        // Create code attribute
        let mut attribute_bytes = Vec::new();
        
        // Calculate max stack and max locals based on actual code
        let max_stack = self.calculate_max_stack(&code_bytes);
        let max_locals = self.calculate_max_locals(constructor);
        attribute_bytes.extend_from_slice(&max_stack.to_be_bytes());
        attribute_bytes.extend_from_slice(&max_locals.to_be_bytes());
        
        // Code length and code
        attribute_bytes.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        attribute_bytes.extend_from_slice(&code_bytes);
        
        // Exception table (empty for now)
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes());
        
        // Sub-attributes of Code
        let mut sub_attrs: Vec<NamedAttribute> = Vec::new();
        if self.config.emit_frames {
            let handlers: Vec<(u16, Option<String>)> = Vec::new();
            let is_static = false;
            let is_constructor = true;
            let this_internal = self.current_class_name.clone().unwrap_or_else(|| constructor.name.clone());
            let method_desc = self.generate_constructor_descriptor(constructor);
            let smt = FrameBuilder::new().compute_with_types_with_handlers(
                &code_bytes,
                &handlers,
                is_static,
                is_constructor,
                &this_internal,
                &method_desc,
                &mut self.cp_shared.as_ref().unwrap().borrow_mut(),
            );
            if self.config.debug {
                for line in describe_stack_map_frames(&smt) { eprintln!("[frames] {}", line); }
            }
            sub_attrs.push(make_stack_map_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &smt.frames)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        if self.config.debug {
            let mut lnt = LineNumberTableAttribute::new();
            let line = constructor.span.start.line as u16;
            let _ = lnt.add_line_number(0, line.max(1));
            sub_attrs.push(make_line_number_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lnt)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        attribute_bytes.extend_from_slice(&(sub_attrs.len() as u16).to_be_bytes());
        for a in sub_attrs {
            attribute_bytes.extend_from_slice(&a.name.as_u16().to_be_bytes());
            let payload = { let cp = self.cp_shared.as_ref().unwrap().borrow(); a.info.to_bytes(&cp) };
            attribute_bytes.extend_from_slice(&(payload.len() as u32).to_be_bytes());
            attribute_bytes.extend_from_slice(&payload);
        }
        
        Ok(AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload: attribute_bytes }))
    }
    
    /// Get the generated class file
    pub fn get_class_file(self) -> ClassFile {
        self.class_file
    }
    
    /// Calculate max stack size for bytecode
    fn calculate_max_stack(&self, code_bytes: &[u8]) -> u16 {
        let mut stack_depth = 0i32;
        let mut max_stack = 0i32;
        let mut i = 0usize;
        
        while i < code_bytes.len() {
            let opcode = code_bytes[i];
            match opcode {
                opcodes::ALOAD_0 | opcodes::ALOAD_1 | opcodes::ALOAD_2 | opcodes::ALOAD_3 => {
                    stack_depth += 1;
                }
                opcodes::INVOKESPECIAL => {
                    stack_depth -= 1; // Pop receiver
                    // Method call doesn't push result for <init>
                }
                opcodes::RETURN => {
                    // No stack effect
                }
                _ => {
                    // Default: assume no stack effect
                }
            }
            
            if stack_depth > max_stack {
                max_stack = stack_depth;
            }
            
            i += 1;
        }
        
        max_stack.max(0) as u16
    }
    
    /// Calculate max locals for constructor
    fn calculate_max_locals(&self, constructor: &ConstructorDecl) -> u16 {
        let mut max_locals = 1; // 'this' reference
        
        // Add parameters
        max_locals += constructor.parameters.len() as u16;
        
        max_locals
    }

    // Minimal compile-time constant evaluator for ConstantValue eligibility
    fn eval_compile_time_constant(expr: &Expr) -> Option<Literal> {
        use crate::ast::{Expr::*, Literal as L, BinaryOp, UnaryOp};
        fn lit(e: &Expr) -> Option<L> { if let Literal(le) = e { Some(le.value.clone()) } else { None } }
        // Numeric promotion helper: returns (lhs, rhs, kind) coerced to a common numeric kind
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum NumKind { Int, Float }
        fn promote_numeric(a: L, b: L) -> Option<(L, L, NumKind)> {
            match (a, b) {
                (L::Float(x), L::Float(y)) => Some((L::Float(x), L::Float(y), NumKind::Float)),
                (L::Float(x), L::Integer(y)) => Some((L::Float(x), L::Float(y as f64), NumKind::Float)),
                (L::Integer(x), L::Float(y)) => Some((L::Float(x as f64), L::Float(y), NumKind::Float)),
                (L::Integer(x), L::Integer(y)) => Some((L::Integer(x), L::Integer(y), NumKind::Int)),
                (L::Char(x), L::Integer(y)) => Some((L::Integer(x as i64), L::Integer(y), NumKind::Int)),
                (L::Integer(x), L::Char(y)) => Some((L::Integer(x), L::Integer(y as i64), NumKind::Int)),
                (L::Char(x), L::Char(y)) => Some((L::Integer(x as i64), L::Integer(y as i64), NumKind::Int)),
                _ => None,
            }
        }
        // Java shift count masks
        fn mask_shift_for_int(n: i64) -> u32 { ((n as i64) & 0x1F) as u32 }
        fn mask_shift_for_long(n: i64) -> u32 { ((n as i64) & 0x3F) as u32 }
        fn infer_shift_is_long(left_expr: &Expr) -> bool {
            fn is_long_cast(e: &Expr) -> bool {
                match e {
                    Cast(c) => c.target_type.name == "long",
                    Parenthesized(p) => is_long_cast(p),
                    Unary(u) => is_long_cast(&u.operand),
                    _ => false,
                }
            }
            is_long_cast(left_expr)
        }
        match expr {
            Literal(le) => Some(le.value.clone()),
            Parenthesized(inner) => Self::eval_compile_time_constant(inner),
            Cast(c) => {
                let v = Self::eval_compile_time_constant(&c.expr).or_else(|| lit(&c.expr))?;
                match c.target_type.name.as_str() {
                    "int" => match v { L::Integer(i) => Some(L::Integer(i)), L::Char(ch) => Some(L::Integer(ch as i64)), L::Float(f) => Some(L::Integer(f as i64)), L::Boolean(b) => Some(L::Integer(if b {1} else {0})), _ => None },
                    "float" | "double" => match v { L::Integer(i) => Some(L::Float(i as f64)), L::Char(ch) => Some(L::Float((ch as i64) as f64)), L::Float(f) => Some(L::Float(f)), _ => None },
                    "char" => match v { L::Integer(i) => Some(L::Char(((i as i64) as u32 as u8) as char)), L::Char(ch) => Some(L::Char(ch)), _ => None },
                    "boolean" => match v { L::Boolean(b) => Some(L::Boolean(b)), L::Integer(i) => Some(L::Boolean(i != 0)), _ => None },
                    _ => None,
                }
            }
            Unary(u) => {
                match u.operator {
                    UnaryOp::Plus => Self::eval_compile_time_constant(&u.operand),
                    UnaryOp::Minus => match Self::eval_compile_time_constant(&u.operand)? { L::Integer(i) => Some(L::Integer(-i)), L::Float(f) => Some(L::Float(-f)), _ => None },
                    UnaryOp::BitNot => match Self::eval_compile_time_constant(&u.operand)? { L::Integer(i) => Some(L::Integer(!(i))), _ => None },
                    UnaryOp::Not => match Self::eval_compile_time_constant(&u.operand)? { L::Boolean(b) => Some(L::Boolean(!b)), _ => None },
                    _ => None,
                }
            }
            Binary(b) => {
                use BinaryOp::*;
                let l = Self::eval_compile_time_constant(&b.left).or_else(|| lit(&b.left))?;
                let r = Self::eval_compile_time_constant(&b.right).or_else(|| lit(&b.right))?;
                // Heuristic long detection without explicit cast: if constant lhs is outside i32 range
                let lhs_is_long = infer_shift_is_long(&b.left) || matches!(l, L::Integer(iv) if iv < i32::MIN as i64 || iv > i32::MAX as i64);
                match (b.operator.clone(), l, r) {
                    // int arithmetic and bitwise
                    (Add, L::Integer(a), L::Integer(b)) => Some(L::Integer(a + b)),
                    (Sub, L::Integer(a), L::Integer(b)) => Some(L::Integer(a - b)),
                    (Mul, L::Integer(a), L::Integer(b)) => Some(L::Integer(a * b)),
                    (Div, L::Integer(a), L::Integer(b)) => { if b == 0 { return None; } Some(L::Integer(a / b)) },
                    (Mod, L::Integer(a), L::Integer(b)) => { if b == 0 { return None; } Some(L::Integer(a % b)) },
                    // bitwise ops (integral promotions): when chars appear, promote to int; treat long-effective lhs transparently via i64
                    (And, L::Integer(a), L::Integer(bv)) => Some(L::Integer(a & bv)),
                    (Or,  L::Integer(a), L::Integer(bv)) => Some(L::Integer(a | bv)),
                    (Xor, L::Integer(a), L::Integer(bv)) => Some(L::Integer(a ^ bv)),
                    (And, L::Char(a), L::Integer(bv))    => Some(L::Integer((a as i64) & bv)),
                    (And, L::Integer(av), L::Char(b))    => Some(L::Integer(av & (b as i64))),
                    (And, L::Char(a), L::Char(b))        => Some(L::Integer((a as i64) & (b as i64))),
                    (Or,  L::Char(a), L::Integer(bv))    => Some(L::Integer((a as i64) | bv)),
                    (Or,  L::Integer(av), L::Char(b))    => Some(L::Integer(av | (b as i64))),
                    (Or,  L::Char(a), L::Char(b))        => Some(L::Integer((a as i64) | (b as i64))),
                    (Xor, L::Char(a), L::Integer(bv))    => Some(L::Integer((a as i64) ^ bv)),
                    (Xor, L::Integer(av), L::Char(b))    => Some(L::Integer(av ^ (b as i64))),
                    (Xor, L::Char(a), L::Char(b))        => Some(L::Integer((a as i64) ^ (b as i64))),
                    (LShift, L::Integer(a), L::Integer(rv)) => {
                        let sh = if lhs_is_long { mask_shift_for_long(rv) } else { mask_shift_for_int(rv) };
                        Some(L::Integer(a << sh))
                    }
                    (RShift, L::Integer(a), L::Integer(rv)) => {
                        let sh = if lhs_is_long { mask_shift_for_long(rv) } else { mask_shift_for_int(rv) };
                        Some(L::Integer(a >> sh))
                    }
                    (URShift, L::Integer(a), L::Integer(rv)) => {
                        let sh = if lhs_is_long { mask_shift_for_long(rv) } else { mask_shift_for_int(rv) };
                        Some(L::Integer(((a as u64) >> sh) as i64))
                    }
                    // double arithmetic
                    (Add, L::Float(a), L::Float(b)) => Some(L::Float(a + b)),
                    (Sub, L::Float(a), L::Float(b)) => Some(L::Float(a - b)),
                    (Mul, L::Float(a), L::Float(b)) => Some(L::Float(a * b)),
                    (Div, L::Float(a), L::Float(b)) => Some(L::Float(a / b)),
                    (Mod, L::Float(a), L::Float(b)) => Some(L::Float(a % b)),
                    // mixed numeric promotions
                    (op @ Add, l, r) | (op @ Sub, l, r) | (op @ Mul, l, r) | (op @ Div, l, r) | (op @ Mod, l, r) => {
                        if let Some((pl, pr, kind)) = promote_numeric(l.clone(), r.clone()) {
                            match (op, kind, pl, pr) {
                                (Add, NumKind::Float, L::Float(a), L::Float(b)) => Some(L::Float(a + b)),
                                (Sub, NumKind::Float, L::Float(a), L::Float(b)) => Some(L::Float(a - b)),
                                (Mul, NumKind::Float, L::Float(a), L::Float(b)) => Some(L::Float(a * b)),
                                (Div, NumKind::Float, L::Float(a), L::Float(b)) => Some(L::Float(a / b)),
                                (Mod, NumKind::Float, L::Float(a), L::Float(b)) => Some(L::Float(a % b)),
                                (Add, NumKind::Int, L::Integer(a), L::Integer(b)) => Some(L::Integer(a + b)),
                                (Sub, NumKind::Int, L::Integer(a), L::Integer(b)) => Some(L::Integer(a - b)),
                                (Mul, NumKind::Int, L::Integer(a), L::Integer(b)) => Some(L::Integer(a * b)),
                                (Div, NumKind::Int, L::Integer(a), L::Integer(b)) => { if b == 0 { None } else { Some(L::Integer(a / b)) } },
                                (Mod, NumKind::Int, L::Integer(a), L::Integer(b)) => { if b == 0 { None } else { Some(L::Integer(a % b)) } },
                                _ => None,
                            }
                        } else { None }
                    }
                    // relational comparisons over numeric with promotion
                    (Lt, l, r) | (Le, l, r) | (Gt, l, r) | (Ge, l, r) | (Eq, l, r) | (Ne, l, r) => {
                        let op = b.operator.clone();
                        if let Some((pl, pr, kind)) = promote_numeric(l.clone(), r.clone()) {
                            match (kind, pl, pr) {
                                (NumKind::Float, L::Float(a), L::Float(bf)) => Some(L::Boolean(match op { Lt => a < bf, Le => a <= bf, Gt => a > bf, Ge => a >= bf, Eq => a == bf, Ne => a != bf, _ => false })),
                                (NumKind::Int, L::Integer(a), L::Integer(bi)) => Some(L::Boolean(match op { Lt => a < bi, Le => a <= bi, Gt => a > bi, Ge => a >= bi, Eq => a == bi, Ne => a != bi, _ => false })),
                                _ => None,
                            }
                        } else { None }
                    }
                    // boolean logical ops
                    (And, L::Boolean(a), L::Boolean(b)) => Some(L::Boolean(a & b)),
                    (Or,  L::Boolean(a), L::Boolean(b)) => Some(L::Boolean(a | b)),
                    (Xor, L::Boolean(a), L::Boolean(b)) => Some(L::Boolean(a ^ b)),
                    // char promotions (to int)
                    // string/char concatenations are folded earlier; no additional patterns here
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
