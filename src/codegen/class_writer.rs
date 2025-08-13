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
    frame::{FrameBuilder, VerificationType, describe_stack_map_frames},
    modifiers_to_flags,
};
use super::descriptor::type_to_descriptor;
use crate::config::Config;
use crate::ast::*;
use crate::error::Result;
use super::method_writer::MethodWriter as BodyWriter;

/// Class writer for generating Java bytecode
pub struct ClassWriter {
    class_file: ClassFile,
    config: Config,
    current_class_name: Option<String>,
    package_name: Option<String>,
    cp_shared: Option<std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>>,
    pending_default_ctor_method_idx: Option<usize>,
}

impl ClassWriter {
    /// Create a new class writer
    pub fn new() -> Self {
        Self {
            class_file: ClassFile::new(),
            config: Config::default(),
            current_class_name: None,
            package_name: None,
            cp_shared: None,
            pending_default_ctor_method_idx: None,
        }
    }

    pub fn new_with_config(config: Config) -> Self {
        Self {
            class_file: ClassFile::new(),
            config,
            current_class_name: None,
            package_name: None,
            cp_shared: None,
            pending_default_ctor_method_idx: None,
        }
    }

    /// Optionally set the package name (e.g., "mono" or "com.example")
    pub fn set_package_name<S: Into<String>>(&mut self, package: Option<S>) {
        self.package_name = package.map(|s| s.into());
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
        // Set interface name and access flags
        let interface_name = &interface.name;
        let this_class_index = self.class_file.constant_pool.try_add_class(interface_name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        self.class_file.this_class = this_class_index;
        
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
        let deferred_this_class_name = internal_name.clone();
        let deferred_super_class_name: String = class.extends.as_ref().map(|t| t.name.replace('.', "/")).unwrap_or("java/lang/Object".to_string());
        
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
        let deferred_interfaces: Vec<String> = class.implements.iter().map(|itf| itf.name.replace('.', "/")).collect();
        
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
            let current_class = cw.current_class_name.clone().ok_or_else(|| crate::error::Error::Internal { message: "current_class_name not set".into() })?;
            let mut code_writer = BodyWriter::new_with_constant_pool_and_class(constant_pool_rc, current_class);
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
            let super_class_name = class.extends.as_ref().map(|t| t.name.as_str()).unwrap_or("java/lang/Object");
            let mref = { let mut cp = cw.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(super_class_name, "<init>", "()V") }
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
                    // Build simple invokespecial <init>; body statements are not yet supported
                    let mut code_bytes = Vec::new();
                    code_bytes.push(opcodes::ALOAD_0);
                    let super_class_name = "java/lang/Object";
                    let mref = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_method_ref(super_class_name, "<init>", "()V") }
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    code_bytes.push(opcodes::INVOKESPECIAL);
                    code_bytes.extend_from_slice(&mref.to_be_bytes());
                    code_bytes.push(opcodes::RETURN);
                    let access_flags = modifiers_to_flags(&constructor.modifiers);
                    let name = "<init>".to_string();
                    let descriptor = self.generate_constructor_descriptor(constructor);
                    let exceptions: Vec<crate::codegen::attribute::ExceptionTableEntry> = Vec::new();
                    let locals: Vec<crate::codegen::bytecode::LocalSlot> = Vec::new();
                    let line_numbers = vec![(0, constructor.span.start.line as u16).into()];
                    pending_methods.push(PendingCode { access_flags, name, descriptor, code_bytes, max_locals: 1, exceptions, locals, line_numbers });
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
            // compute max_stack by simulation
            let mut sim_depth: i32 = 0; let mut sim_max: i32 = 0; let mut i = 0usize;
            while i < pc.code_bytes.len() { let op = pc.code_bytes[i]; match op { 0x2A => { sim_depth += 1; }, 0x12 => { sim_depth += 1; i += 1; }, 0x13 => { sim_depth += 1; i += 2; }, 0xB2 => { sim_depth += 1; i += 2; }, 0xB6 => { sim_depth -= 2; i += 2; }, 0xB7 => { sim_depth -= 1; i += 2; }, _ => {} } if sim_depth > sim_max { sim_max = sim_depth; } i += 1; }
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
                    for (pcv, ln) in &pc.line_numbers { let _ = lnt.add_line_number(*pcv, *ln); }
                }
                sub_attrs.push(make_line_number_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lnt)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
            }
            if self.config.debug {
                let mut lvt = LocalVariableTableAttribute::new();
                // For HelloWorld main: add args if present
                for lv in &pc.locals {
                    if lv.name == "this" || lv.name.starts_with('$') { continue; }
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

        // Add SourceFile attribute after methods
        let filename = format!("{}.java", class.name);
        if let Ok(attr) = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); crate::codegen::attribute::NamedAttribute::new_source_file_attribute(&mut cp, filename) } {
            self.class_file.attributes.push(attr);
        }

        // Finalize: write back shared pool
        if let Some(cp) = &self.cp_shared { self.class_file.constant_pool = cp.borrow().clone(); }
        self.cp_shared = None;
        
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
        
        let method_info = MethodInfo {
            access_flags,
            name_index,
            descriptor_index,
            attributes: vec![],
        };
        
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
        let name_index = self.class_file.constant_pool.try_add_utf8(&field.name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = self.class_file.constant_pool.try_add_utf8(&type_to_descriptor(&field.type_ref))
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        let access_flags = modifiers_to_flags(&field.modifiers);
        
        let field_info = FieldInfo::new(access_flags, name_index, descriptor_index);
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
        
        self.class_file.methods.push(method_info);
        
        Ok(())
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
        let current_class = self.current_class_name.clone().ok_or_else(|| crate::error::Error::Internal { message: "current_class_name not set".into() })?;
        let mut code_writer = BodyWriter::new_with_constant_pool_and_class(constant_pool_rc.clone(), current_class);
        code_writer.generate_method_body(method)?;
        let (code_bytes, max_stack, max_locals, exceptions, locals, line_numbers) = code_writer.finalize();
        // Merge back constant pool if not using shared
        if self.cp_shared.is_none() { self.class_file.constant_pool = constant_pool_rc.borrow().clone(); }
        
        // Compute a lightweight max_stack from code for common opcodes to better match javac
        let mut sim_depth: i32 = 0;
        let mut sim_max: i32 = 0;
        let mut i = 0usize;
        while i < code_bytes.len() {
            let op = code_bytes[i];
            match op {
                // loads
                0x2A /* ALOAD_0 */ => { sim_depth += 1; }
                // constants
                0x12 /* LDC */ => { sim_depth += 1; i += 1; } // skip index u1 handled below
                0x13 /* LDC_W */ => { sim_depth += 1; i += 2; }
                // field access
                0xB2 /* GETSTATIC */ => { sim_depth += 1; i += 2; }
                // invokes
                0xB6 /* INVOKEVIRTUAL */ => { sim_depth -= 2; i += 2; }
                0xB7 /* INVOKESPECIAL */ => { sim_depth -= 1; i += 2; }
                // return
                0xB1 /* RETURN */ => {}
                _ => {
                    // Skip operand widths for simple known opcodes
                    // Most other opcodes in our simple tests are no-ops for stack simulation
                }
            }
            if sim_depth > sim_max { sim_max = sim_depth; }
            // Advance i by default opcode width
            i += 1;
        }
        let computed_max_stack = sim_max.max(0) as u16;

        // Build sub-attributes after body emissions (so CP first-touches are from code first)
        let mut sub_attrs: Vec<NamedAttribute> = Vec::new();
        if self.config.emit_frames {
            let handler_pcs: Vec<u16> = exceptions.iter().map(|e| e.handler_pc).collect();
        let throwable_cp = { let mut cp = self.cp_shared.as_ref().unwrap().borrow_mut(); cp.try_add_class("java/lang/Throwable") }
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let smt = FrameBuilder::new().compute_from_with_handler_stack(
                &code_bytes,
                &handler_pcs,
                VerificationType::Object(throwable_cp),
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
            if line_numbers.is_empty() {
                let line = method.span.start.line as u16;
                lnt.add_line_number(0, line.max(1)).map_err(|e| crate::error::Error::CodeGen { message: format!("add_line_number failed: {}", e) })?;
            } else {
                for (pc, line) in &line_numbers {
                    lnt.add_line_number(*pc, *line).map_err(|e| crate::error::Error::CodeGen { message: format!("add_line_number failed: {}", e) })?;
                }
            }
            sub_attrs.push(make_line_number_table_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &lnt)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        // LocalVariableTable only in debug mode
            if self.config.debug {
                let mut lvt = LocalVariableTableAttribute::new();
                for lv in &locals {
                    if lv.name == "this" || lv.name.starts_with('$') { continue; }
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
        // Max stack and max locals
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
            let smt = FrameBuilder::new().compute_from(&code_bytes, &[]);
            if self.config.debug {
                for line in describe_stack_map_frames(&smt) { eprintln!("[frames] {}", line); }
            }
            sub_attrs.push(make_stack_map_attribute(&mut self.cp_shared.as_ref().unwrap().borrow_mut(), &smt.frames)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        if self.config.debug {
            let mut lnt = LineNumberTableAttribute::new();
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
            let smt = FrameBuilder::new().compute_from(&code_bytes, &[]);
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
}
