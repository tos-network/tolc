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
}

impl ClassWriter {
    /// Create a new class writer
    pub fn new() -> Self {
        Self {
            class_file: ClassFile::new(),
            config: Config::default(),
            current_class_name: None,
        }
    }

    pub fn new_with_config(config: Config) -> Self {
        Self {
            class_file: ClassFile::new(),
            config,
            current_class_name: None,
        }
    }
    
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
        // Set class name and access flags
        let class_name = &class.name;
        let this_class_index = self.class_file.constant_pool.try_add_class(class_name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        self.class_file.this_class = this_class_index;
        self.current_class_name = Some(class_name.to_string());
        
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
        
        // Set superclass (default to java.lang.Object if not specified)
        let super_class_name = class.extends.as_ref().map(|t| t.name.as_str()).unwrap_or("java/lang/Object");
        let super_class_index = self.class_file.constant_pool.try_add_class(super_class_name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        self.class_file.super_class = super_class_index;
        
        // Add interfaces
        for interface in &class.implements {
            let interface_index = self.class_file.constant_pool.try_add_class(&interface.name)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            self.class_file.interfaces.push(interface_index);
        }
        
        // Generate fields and methods from class body
        for member in &class.body {
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
        
        // Generate default constructor if no constructors are present
        if !class.body.iter().any(|m| {
            if let ClassMember::Constructor(_) = m { true } else { false }
        }) {
            self.generate_default_constructor(class)?;
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an interface field (implicitly public, static, final)
    fn generate_interface_field(&mut self, field: &FieldDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.try_add_utf8(&field.name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = self.class_file.constant_pool.try_add_utf8(&type_to_descriptor(&field.type_ref))
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
        let name_index = self.class_file.constant_pool.try_add_utf8(&method.name)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Generate method descriptor
        let descriptor = self.generate_method_descriptor(method);
        let descriptor_index = self.class_file.constant_pool.try_add_utf8(&descriptor)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        let access_flags = modifiers_to_flags(&method.modifiers);
        
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        
        // Generate method body if not abstract
        if !method.modifiers.contains(&Modifier::Abstract) {
            let code_attr_info = self.generate_code_attribute(method)?;
            let _name_index = self.class_file.constant_pool.try_add_utf8("Code")
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let named = NamedAttribute::new(_name_index.into(), code_attr_info);
            method_info.attributes.push(named);
        }
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }
    
    /// Generate bytecode for a constructor
    fn generate_constructor(&mut self, constructor: &ConstructorDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.try_add_utf8("<init>")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Generate constructor descriptor
        let descriptor = self.generate_constructor_descriptor(constructor);
        let descriptor_index = self.class_file.constant_pool.try_add_utf8(&descriptor)
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        let access_flags = modifiers_to_flags(&constructor.modifiers);
        
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        
        // Generate constructor body
        let code_attr_info = self.generate_constructor_code_attribute_from_body(constructor)?;
        let _name_index = self.class_file.constant_pool.try_add_utf8("Code")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let named = NamedAttribute::new(_name_index.into(), code_attr_info);
        method_info.attributes.push(named);
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }
    
    /// Generate default constructor for a class
    fn generate_default_constructor(&mut self, class: &ClassDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.try_add_utf8("<init>")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        let descriptor_index = self.class_file.constant_pool.try_add_utf8("()V")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        let access_flags = access_flags::ACC_PUBLIC;
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        
        // Generate constructor body
        let code_attr_info = self.generate_constructor_code_attribute(class)?;
        let _name_index = self.class_file.constant_pool.try_add_utf8("Code")
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
        let _name_index = self.class_file.constant_pool.try_add_utf8("Code")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Generate bytecode for method body
        let constant_pool_rc = std::rc::Rc::new(std::cell::RefCell::new(self.class_file.constant_pool.clone()));
        let current_class = self.current_class_name.clone().ok_or_else(|| crate::error::Error::Internal { message: "current_class_name not set".into() })?;
        let mut code_writer = BodyWriter::new_with_constant_pool_and_class(constant_pool_rc, current_class);
        code_writer.generate_method_body(method)?;
        let (code_bytes, max_stack, max_locals, exceptions, locals, line_numbers) = code_writer.finalize();
        
        // Create code attribute
        let mut attribute_bytes = Vec::new();
        
        // Max stack and max locals
        attribute_bytes.extend_from_slice(&(max_stack as u16).to_be_bytes());
        attribute_bytes.extend_from_slice(&(max_locals as u16).to_be_bytes());
        
        // Code length and code
        attribute_bytes.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        attribute_bytes.extend_from_slice(&code_bytes);
        
        // Exception table
        attribute_bytes.extend_from_slice(&(exceptions.len() as u16).to_be_bytes());
        for e in &exceptions {
            attribute_bytes.extend_from_slice(&e.to_bytes());
        }
        
        // Sub-attributes of Code
        let mut sub_attrs: Vec<NamedAttribute> = Vec::new();
        if self.config.emit_frames {
            let handler_pcs: Vec<u16> = exceptions.iter().map(|e| e.handler_pc).collect();
        let throwable_cp = self.class_file.constant_pool.try_add_class("java/lang/Throwable")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
            let smt = FrameBuilder::new().compute_from_with_handler_stack(
                &code_bytes,
                &handler_pcs,
                VerificationType::Object(throwable_cp),
            );
            if self.config.debug {
                for line in describe_stack_map_frames(&smt) { eprintln!("[frames] {}", line); }
            }
            sub_attrs.push(make_stack_map_attribute(&mut self.class_file.constant_pool, &smt.frames)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        if self.config.debug {
            let mut lnt = LineNumberTableAttribute::new();
            if line_numbers.is_empty() {
                // Fallback: minimal entry
                let line = method.span.start.line as u16;
                lnt.add_line_number(0, line.max(1)).map_err(|e| crate::error::Error::CodeGen { message: format!("add_line_number failed: {}", e) })?;
            } else {
                for (pc, line) in &line_numbers {
                    lnt.add_line_number(*pc, *line).map_err(|e| crate::error::Error::CodeGen { message: format!("add_line_number failed: {}", e) })?;
                }
            }
            sub_attrs.push(make_line_number_table_attribute(&mut self.class_file.constant_pool, &lnt)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
            // Local variable table
            if self.config.debug {
                let mut lvt = LocalVariableTableAttribute::new();
                for lv in &locals {
                    if lv.name == "this" || lv.name.starts_with('$') { continue; }
                    let desc = lv.var_type.descriptor();
                    if desc.is_empty() { continue; }
                    let name_index = self.class_file.constant_pool.try_add_utf8(&lv.name)
                        .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
                    let desc_index = self.class_file.constant_pool.try_add_utf8(&desc)
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
                sub_attrs.push(make_local_variable_table_attribute(&mut self.class_file.constant_pool, &lvt)
                    .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
            }
        }
        attribute_bytes.extend_from_slice(&(sub_attrs.len() as u16).to_be_bytes());
        for a in sub_attrs {
            attribute_bytes.extend_from_slice(&a.name.as_u16().to_be_bytes());
            let payload = a.info.to_bytes(&self.class_file.constant_pool);
            attribute_bytes.extend_from_slice(&(payload.len() as u32).to_be_bytes());
            attribute_bytes.extend_from_slice(&payload);
        }
        
        Ok(AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload: attribute_bytes }))
    }
    
    /// Generate constructor code attribute
    fn generate_constructor_code_attribute(&mut self, class: &ClassDecl) -> Result<AttributeInfo> {
        let _name_index = self.class_file.constant_pool.try_add_utf8("Code")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Generate simple constructor bytecode manually
        let mut code_bytes = Vec::new();
        
        // ALOAD_0 (load this)
        code_bytes.push(opcodes::ALOAD_0);
        
        // INVOKESPECIAL java/lang/Object.<init>()V
        let super_class_name = class.extends.as_ref().map(|t| t.name.as_str()).unwrap_or("java/lang/Object");
        let method_ref_index = self.class_file.constant_pool.try_add_method_ref(super_class_name, "<init>", "()V")
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
            sub_attrs.push(make_stack_map_attribute(&mut self.class_file.constant_pool, &smt.frames)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        if self.config.debug {
            let mut lnt = LineNumberTableAttribute::new();
            let line = class.span.start.line as u16;
            let _ = lnt.add_line_number(0, line.max(1));
            sub_attrs.push(make_line_number_table_attribute(&mut self.class_file.constant_pool, &lnt)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        attribute_bytes.extend_from_slice(&(sub_attrs.len() as u16).to_be_bytes());
        for a in sub_attrs {
            attribute_bytes.extend_from_slice(&a.name.as_u16().to_be_bytes());
            let payload = a.info.to_bytes(&self.class_file.constant_pool);
            attribute_bytes.extend_from_slice(&(payload.len() as u32).to_be_bytes());
            attribute_bytes.extend_from_slice(&payload);
        }
        
        Ok(AttributeInfo::Custom(crate::codegen::attribute::CustomAttribute { payload: attribute_bytes }))
    }

    /// Generate constructor code attribute from a constructor body
    fn generate_constructor_code_attribute_from_body(&mut self, constructor: &ConstructorDecl) -> Result<AttributeInfo> {
        let _name_index = self.class_file.constant_pool.try_add_utf8("Code")
            .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?;
        
        // Generate constructor bytecode manually
        let mut code_bytes = Vec::new();
        
        // ALOAD_0 (load this)
        code_bytes.push(opcodes::ALOAD_0);
        
        // INVOKESPECIAL java/lang/Object.<init>()V
        let super_class_name = "java/lang/Object";
        let method_ref_index = self.class_file.constant_pool.try_add_method_ref(super_class_name, "<init>", "()V")
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
            sub_attrs.push(make_stack_map_attribute(&mut self.class_file.constant_pool, &smt.frames)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        if self.config.debug {
            let mut lnt = LineNumberTableAttribute::new();
            let line = constructor.span.start.line as u16;
            let _ = lnt.add_line_number(0, line.max(1));
            sub_attrs.push(make_line_number_table_attribute(&mut self.class_file.constant_pool, &lnt)
                .map_err(|e| crate::error::Error::CodeGen { message: format!("const pool: {}", e) })?);
        }
        attribute_bytes.extend_from_slice(&(sub_attrs.len() as u16).to_be_bytes());
        for a in sub_attrs {
            attribute_bytes.extend_from_slice(&a.name.as_u16().to_be_bytes());
            let payload = a.info.to_bytes(&self.class_file.constant_pool);
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
