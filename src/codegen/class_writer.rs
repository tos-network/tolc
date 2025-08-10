//! Class writer for generating Java .class files
//! 
//! This module handles the conversion of AST class declarations into Java bytecode.

use super::bytecode::*;
use crate::ast::*;
use crate::error::Result;
use super::method_writer::MethodWriter as BodyWriter;

/// Class writer for generating Java bytecode
pub struct ClassWriter {
    class_file: ClassFile,
}

impl ClassWriter {
    /// Create a new class writer
    pub fn new() -> Self {
        Self {
            class_file: ClassFile::new(),
        }
    }
    
    /// Generate bytecode for an enum declaration
    pub fn generate_enum(&mut self, enum_decl: &EnumDecl) -> Result<()> {
        // Set enum name and access flags
        let enum_name = &enum_decl.name;
        let this_class_index = self.class_file.constant_pool.add_class(enum_name);
        self.class_file.this_class = this_class_index;
        
        // Set access flags - enums are always final
        let mut access_flags = access_flags::ACC_FINAL | access_flags::ACC_SUPER;
        if enum_decl.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        self.class_file.access_flags = access_flags;
        
        // Set superclass to java.lang.Enum
        let super_class_index = self.class_file.constant_pool.add_class("java/lang/Enum");
        self.class_file.super_class = super_class_index;
        
        // Add implemented interfaces
        for interface in &enum_decl.implements {
            let interface_index = self.class_file.constant_pool.add_class(&interface.name);
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
                ClassMember::TypeDecl(type_decl) => {
                    // TODO: Handle nested type declarations
                    // For now, skip nested types as they would be separate class files
                }
                ClassMember::Initializer(initializer) => {
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
        let this_class_index = self.class_file.constant_pool.add_class(interface_name);
        self.class_file.this_class = this_class_index;
        
        // Set access flags - interfaces are always abstract
        let mut access_flags = access_flags::ACC_INTERFACE | access_flags::ACC_ABSTRACT;
        if interface.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        self.class_file.access_flags = access_flags;
        
        // Set superclass to java.lang.Object (interfaces implicitly extend Object)
        let super_class_index = self.class_file.constant_pool.add_class("java/lang/Object");
        self.class_file.super_class = super_class_index;
        
        // Add extended interfaces
        for extended_interface in &interface.extends {
            let interface_index = self.class_file.constant_pool.add_class(&extended_interface.name);
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
                InterfaceMember::TypeDecl(_) => {
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
        let this_class_index = self.class_file.constant_pool.add_class(class_name);
        self.class_file.this_class = this_class_index;
        
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
        let super_class_index = self.class_file.constant_pool.add_class(super_class_name);
        self.class_file.super_class = super_class_index;
        
        // Add interfaces
        for interface in &class.implements {
            let interface_index = self.class_file.constant_pool.add_class(&interface.name);
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
                ClassMember::TypeDecl(type_decl) => {
                    // TODO: Handle nested type declarations
                    // For now, skip nested types as they would be separate class files
                }
                ClassMember::Initializer(initializer) => {
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
        let name_index = self.class_file.constant_pool.add_utf8(&field.name);
        let descriptor_index = self.class_file.constant_pool.add_utf8(&self.type_to_descriptor(&field.type_ref));
        
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
        let name_index = self.class_file.constant_pool.add_utf8(&method.name);
        let descriptor_index = self.class_file.constant_pool.add_utf8(&self.generate_method_descriptor(method));
        
        // Interface methods are implicitly public and abstract
        let mut access_flags = access_flags::ACC_PUBLIC | access_flags::ACC_ABSTRACT;
        
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
        let name_index = self.class_file.constant_pool.add_utf8(&constant.name);
        let descriptor_index = self.class_file.constant_pool.add_utf8(&format!("L{};", enum_name));
        
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
    fn generate_enum_constructor(&mut self, enum_decl: &EnumDecl) -> Result<()> {
        // Enum constructors are private and take name and ordinal parameters
        let name_index = self.class_file.constant_pool.add_utf8("<init>");
        let descriptor_index = self.class_file.constant_pool.add_utf8("(Ljava/lang/String;I)V");
        
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
        // Set annotation name and access flags
        let annotation_name = &annotation.name;
        let this_class_index = self.class_file.constant_pool.add_class(annotation_name);
        self.class_file.this_class = this_class_index;
        
        // Set access flags - annotations are always interfaces
        let mut access_flags = access_flags::ACC_INTERFACE | access_flags::ACC_ABSTRACT | access_flags::ACC_ANNOTATION;
        if annotation.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        self.class_file.access_flags = access_flags;
        
        // Set superclass to java.lang.annotation.Annotation
        let super_class_index = self.class_file.constant_pool.add_class("java/lang/annotation/Annotation");
        self.class_file.super_class = super_class_index;
        
        // Generate annotation members
        for member in &annotation.body {
            self.generate_annotation_member(member)?;
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an annotation member
    fn generate_annotation_member(&mut self, member: &AnnotationMember) -> Result<()> {
        let name_index = self.class_file.constant_pool.add_utf8(&member.name);
        let descriptor_index = self.class_file.constant_pool.add_utf8(&self.type_to_descriptor(&member.type_ref));
        
        // Annotation methods are implicitly public and abstract
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
    
    /// Generate bytecode for a field
    fn generate_field(&mut self, field: &FieldDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.add_utf8(&field.name);
        let descriptor_index = self.class_file.constant_pool.add_utf8(&self.type_to_descriptor(&field.type_ref));
        
        let mut access_flags = 0;
        if field.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        if field.modifiers.contains(&Modifier::Private) {
            access_flags |= access_flags::ACC_PRIVATE;
        }
        if field.modifiers.contains(&Modifier::Protected) {
            access_flags |= access_flags::ACC_PROTECTED;
        }
        if field.modifiers.contains(&Modifier::Static) {
            access_flags |= access_flags::ACC_STATIC;
        }
        if field.modifiers.contains(&Modifier::Final) {
            access_flags |= access_flags::ACC_FINAL;
        }
        
        let field_info = FieldInfo::new(access_flags, name_index, descriptor_index);
        self.class_file.fields.push(field_info);
        
        Ok(())
    }
    
    /// Generate bytecode for a method
    fn generate_method(&mut self, method: &MethodDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.add_utf8(&method.name);
        
        // Generate method descriptor
        let descriptor = self.generate_method_descriptor(method);
        let descriptor_index = self.class_file.constant_pool.add_utf8(&descriptor);
        
        let mut access_flags = 0;
        if method.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        if method.modifiers.contains(&Modifier::Private) {
            access_flags |= access_flags::ACC_PRIVATE;
        }
        if method.modifiers.contains(&Modifier::Protected) {
            access_flags |= access_flags::ACC_PROTECTED;
        }
        if method.modifiers.contains(&Modifier::Static) {
            access_flags |= access_flags::ACC_STATIC;
        }
        if method.modifiers.contains(&Modifier::Final) {
            access_flags |= access_flags::ACC_FINAL;
        }
        if method.modifiers.contains(&Modifier::Abstract) {
            access_flags |= access_flags::ACC_ABSTRACT;
        }
        
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        
        // Generate method body if not abstract
        if !method.modifiers.contains(&Modifier::Abstract) {
            let code_attribute = self.generate_code_attribute(method)?;
            method_info.attributes.push(code_attribute);
        }
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }
    
    /// Generate bytecode for a constructor
    fn generate_constructor(&mut self, constructor: &ConstructorDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.add_utf8("<init>");
        
        // Generate constructor descriptor
        let descriptor = self.generate_constructor_descriptor(constructor);
        let descriptor_index = self.class_file.constant_pool.add_utf8(&descriptor);
        
        let mut access_flags = 0;
        if constructor.modifiers.contains(&Modifier::Public) {
            access_flags |= access_flags::ACC_PUBLIC;
        }
        if constructor.modifiers.contains(&Modifier::Private) {
            access_flags |= access_flags::ACC_PRIVATE;
        }
        if constructor.modifiers.contains(&Modifier::Protected) {
            access_flags |= access_flags::ACC_PROTECTED;
        }
        
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        
        // Generate constructor body
        let code_attribute = self.generate_constructor_code_attribute_from_body(constructor)?;
        method_info.attributes.push(code_attribute);
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }
    
    /// Generate default constructor for a class
    fn generate_default_constructor(&mut self, class: &ClassDecl) -> Result<()> {
        let name_index = self.class_file.constant_pool.add_utf8("<init>");
        let descriptor_index = self.class_file.constant_pool.add_utf8("()V");
        
        let access_flags = access_flags::ACC_PUBLIC;
        let mut method_info = MethodInfo::new(access_flags, name_index, descriptor_index);
        
        // Generate constructor body
        let code_attribute = self.generate_constructor_code_attribute(class)?;
        method_info.attributes.push(code_attribute);
        
        self.class_file.methods.push(method_info);
        
        Ok(())
    }
    
    /// Generate method descriptor
    fn generate_method_descriptor(&self, method: &MethodDecl) -> String {
        let mut descriptor = String::new();
        descriptor.push('(');
        
        // Parameter types
        for param in &method.parameters {
            descriptor.push_str(&self.type_to_descriptor(&param.type_ref));
        }
        
        descriptor.push(')');
        
        // Return type
        if let Some(return_type) = &method.return_type {
            descriptor.push_str(&self.type_to_descriptor(return_type));
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
            descriptor.push_str(&self.type_to_descriptor(&param.type_ref));
        }
        
        descriptor.push(')');
        
        // Return type (void)
        descriptor.push('V');
        
        descriptor
    }
    
    /// Convert Terminos type to Java descriptor
    fn type_to_descriptor(&self, ty: &TypeRef) -> String {
        // Handle array dimensions first
        let mut descriptor = String::new();
        for _ in 0..ty.array_dims {
            descriptor.push('[');
        }
        
        // Handle the base type
        let base_descriptor = match ty.name.as_str() {
            "int" => "I".to_string(),
            "long" => "J".to_string(), 
            "float" => "F".to_string(),
            "double" => "D".to_string(),
            "boolean" => "Z".to_string(),
            "char" => "C".to_string(),
            "byte" => "B".to_string(),
            "short" => "S".to_string(),
            "void" => "V".to_string(),
            _ => {
                // Reference type - convert package separators to path separators
                format!("L{};", ty.name.replace('.', "/"))
            }
        };
        
        descriptor.push_str(&base_descriptor);
        descriptor
    }
    
    /// Generate code attribute for a method
    fn generate_code_attribute(&mut self, method: &MethodDecl) -> Result<AttributeInfo> {
        let name_index = self.class_file.constant_pool.add_utf8("Code");
        
        // Generate bytecode for method body
        let mut code_writer = BodyWriter::new();
        code_writer.generate_method_body(method)?;
        let (code_bytes, max_stack, max_locals, exceptions) = code_writer.finalize();
        
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
        for e in exceptions {
            attribute_bytes.extend_from_slice(&e.to_bytes());
        }
        
        // Attributes (empty for now)
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes());
        
        Ok(AttributeInfo::new(name_index, attribute_bytes))
    }
    
    /// Generate constructor code attribute
    fn generate_constructor_code_attribute(&mut self, class: &ClassDecl) -> Result<AttributeInfo> {
        let name_index = self.class_file.constant_pool.add_utf8("Code");
        
        // Generate constructor bytecode
        let mut code_writer = MethodWriter::new();
        code_writer.generate_constructor_body(class)?;
        let max_stack = code_writer.max_stack;
        let max_locals = code_writer.max_locals;
        let code_bytes = code_writer.get_bytecode();
        
        // Create code attribute
        let mut attribute_bytes = Vec::new();
        
        // Max stack and max locals
        attribute_bytes.extend_from_slice(&(max_stack as u16).to_be_bytes());
        attribute_bytes.extend_from_slice(&(max_locals as u16).to_be_bytes());
        
        // Code length and code
        attribute_bytes.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        attribute_bytes.extend_from_slice(&code_bytes);
        
        // Exception table (empty for now)
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes());
        
        // Attributes (empty for now)
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes());
        
        Ok(AttributeInfo::new(name_index, attribute_bytes))
    }

    /// Generate constructor code attribute from a constructor body
    fn generate_constructor_code_attribute_from_body(&mut self, constructor: &ConstructorDecl) -> Result<AttributeInfo> {
        let name_index = self.class_file.constant_pool.add_utf8("Code");
        
        // Generate constructor bytecode
        let mut code_writer = MethodWriter::new();
        code_writer.generate_constructor_body_from_decl(constructor)?;
        let max_stack = code_writer.max_stack;
        let max_locals = code_writer.max_locals;
        let code_bytes = code_writer.get_bytecode();
        
        // Create code attribute
        let mut attribute_bytes = Vec::new();
        
        // Max stack and max locals
        attribute_bytes.extend_from_slice(&(max_stack as u16).to_be_bytes());
        attribute_bytes.extend_from_slice(&(max_locals as u16).to_be_bytes());
        
        // Code length and code
        attribute_bytes.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        attribute_bytes.extend_from_slice(&code_bytes);
        
        // Exception table (empty for now)
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes());
        
        // Attributes (empty for now)
        attribute_bytes.extend_from_slice(&0u16.to_be_bytes());
        
        Ok(AttributeInfo::new(name_index, attribute_bytes))
    }
    
    /// Get the generated class file
    pub fn get_class_file(self) -> ClassFile {
        self.class_file
    }
}

/// Method writer for generating method bytecode
struct MethodWriter {
    bytecode: Vec<u8>,
    max_stack: u16,
    max_locals: u16,
}

impl MethodWriter {
    fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            max_stack: 0,
            max_locals: 0,
        }
    }
    
    fn generate_method_body(&mut self, method: &MethodDecl) -> Result<()> {
        // Generate parameter loading
        for (i, param) in method.parameters.iter().enumerate() {
            self.load_parameter(i, &param.type_ref)?;
        }
        
        // Generate method body
        if let Some(body) = &method.body {
            for stmt in &body.statements {
                self.generate_statement(stmt)?;
            }
        }
        
        // Generate return
        if let Some(return_type) = &method.return_type {
            self.generate_return(return_type)?;
        } else {
            // Create a void type reference for methods without return type
            let void_type = TypeRef {
                name: "void".to_string(),
                type_args: vec![],
                array_dims: 0,
                span: method.span,
            };
            self.generate_return(&void_type)?;
        }
        
        Ok(())
    }
    
    fn generate_constructor_body(&mut self, class: &ClassDecl) -> Result<()> {
        // Load 'this' reference
        self.bytecode.push(opcodes::ALOAD_0);
        
        // Call super constructor
        let super_class_name = class.extends.as_ref().map(|t| t.name.as_str()).unwrap_or("java/lang/Object");
        let method_ref_index = self.add_method_ref(super_class_name, "constructor", "()V");
        
        // INVOKESPECIAL instruction
        self.bytecode.push(opcodes::INVOKESPECIAL);
        self.bytecode.extend_from_slice(&method_ref_index.to_be_bytes());
        
        // Return
        self.bytecode.push(opcodes::RETURN);
        
        self.max_stack = 1;
        self.max_locals = 1;
        
        Ok(())
    }
    
    fn generate_constructor_body_from_decl(&mut self, constructor: &ConstructorDecl) -> Result<()> {
        // Load 'this' reference
        self.bytecode.push(opcodes::ALOAD_0);
        
        // Call super constructor (default to java/lang/Object)
        let super_class_name = "java/lang/Object";
        let method_ref_index = self.add_method_ref(super_class_name, "<init>", "()V");
        
        // INVOKESPECIAL instruction
        self.bytecode.push(opcodes::INVOKESPECIAL);
        self.bytecode.extend_from_slice(&method_ref_index.to_be_bytes());
        
        // Generate constructor body statements
        for stmt in &constructor.body.statements {
            self.generate_statement(stmt)?;
        }
        
        // Return
        self.bytecode.push(opcodes::RETURN);
        
        self.max_stack = 1;
        self.max_locals = 1;
        
        Ok(())
    }
    
    fn generate_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.generate_expression(&expr_stmt.expr)?;
                // Pop result if not used
                self.bytecode.push(opcodes::POP);
            }
            Stmt::Return(return_stmt) => {
                if let Some(expr) = &return_stmt.value {
                    self.generate_expression(expr)?;
                }
                // Return will be handled by caller
            }
            Stmt::If(if_stmt) => {
                self.generate_if_statement(if_stmt)?;
            }
            Stmt::While(while_stmt) => {
                self.generate_while_statement(while_stmt)?;
            }
            Stmt::Block(block) => {
                for stmt in &block.statements {
                    self.generate_statement(stmt)?;
                }
            }
            Stmt::Declaration(var_decl_stmt) => {
                // TODO: Handle variable declarations
                // For now, just generate the initializer if present
                for var in &var_decl_stmt.variables {
                    if let Some(initializer) = &var.initializer {
                        self.generate_expression(initializer)?;
                    }
                }
            }
            _ => {
                // TODO: Handle other statement types
            }
        }
        
        Ok(())
    }
    
    fn generate_expression(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Literal(lit_expr) => {
                self.generate_literal(&lit_expr.value)?;
            }
            Expr::Identifier(ident_expr) => {
                self.generate_identifier(&ident_expr.name)?;
            }
            Expr::Binary(bin_expr) => {
                self.generate_binary_operation(bin_expr)?;
            }
            Expr::Unary(unary_expr) => {
                self.generate_unary_operation(unary_expr)?;
            }
            Expr::MethodCall(method_call_expr) => {
                // TODO: Handle method calls
                // For now, just generate the target and arguments
                if let Some(target) = &method_call_expr.target {
                    self.generate_expression(target)?;
                }
                for arg in &method_call_expr.arguments {
                    self.generate_expression(arg)?;
                }
            }
            Expr::Assignment(assign_expr) => {
                // TODO: Handle assignments
                // For now, just generate the value
                self.generate_expression(&assign_expr.value)?;
            }
            _ => {
                // TODO: Handle other expression types
            }
        }
        
        Ok(())
    }
    
    fn generate_literal(&mut self, lit: &Literal) -> Result<()> {
        match lit {
            Literal::Integer(value) => {
                match *value {
                    0 => self.bytecode.push(opcodes::ICONST_0),
                    1 => self.bytecode.push(opcodes::ICONST_1),
                    2 => self.bytecode.push(opcodes::ICONST_2),
                    3 => self.bytecode.push(opcodes::ICONST_3),
                    4 => self.bytecode.push(opcodes::ICONST_4),
                    5 => self.bytecode.push(opcodes::ICONST_5),
                    -1 => self.bytecode.push(opcodes::ICONST_M1),
                    _ => {
                        if *value >= -128 && *value <= 127 {
                            self.bytecode.push(opcodes::BIPUSH);
                            self.bytecode.push(*value as u8);
                        } else if *value >= -32768 && *value <= 32767 {
                            self.bytecode.push(opcodes::SIPUSH);
                            self.bytecode.extend_from_slice(&(*value as i16).to_be_bytes());
                        } else {
                            // Use LDC for larger constants
                            let const_index = self.add_integer_constant(*value as i32);
                            self.bytecode.push(opcodes::LDC);
                            self.bytecode.push(const_index as u8);
                        }
                    }
                }
            }
            Literal::String(value) => {
                let const_index = self.add_string_constant(value);
                self.bytecode.push(opcodes::LDC);
                self.bytecode.push(const_index as u8);
            }
            Literal::Boolean(value) => {
                if *value {
                    self.bytecode.push(opcodes::ICONST_1);
                } else {
                    self.bytecode.push(opcodes::ICONST_0);
                }
            }
            Literal::Null => {
                self.bytecode.push(opcodes::ACONST_NULL);
            }
            _ => {
                // TODO: Handle other literal types (Float, Char)
            }
        }
        
        Ok(())
    }
    
    fn generate_identifier(&mut self, ident: &str) -> Result<()> {
        // For now, assume it's a field or local variable
        // This is a simplified implementation
        self.bytecode.push(opcodes::ALOAD_0);
        let field_ref_index = self.add_field_ref("", ident, "I"); // Assume int for now
        self.bytecode.push(opcodes::GETFIELD);
        self.bytecode.extend_from_slice(&field_ref_index.to_be_bytes());
        
        Ok(())
    }
    
    fn generate_binary_operation(&mut self, bin_expr: &BinaryExpr) -> Result<()> {
        // Generate left operand
        self.generate_expression(&bin_expr.left)?;
        
        // Generate right operand
        self.generate_expression(&bin_expr.right)?;
        
        // Generate operation
        match bin_expr.operator {
            BinaryOp::Add => {
                self.bytecode.push(opcodes::IADD);
            }
            BinaryOp::Sub => {
                self.bytecode.push(opcodes::ISUB);
            }
            BinaryOp::Mul => {
                self.bytecode.push(opcodes::IMUL);
            }
            BinaryOp::Div => {
                self.bytecode.push(opcodes::IDIV);
            }
            BinaryOp::Mod => {
                self.bytecode.push(opcodes::IREM);
            }
            BinaryOp::Eq => {
                self.bytecode.push(opcodes::IF_ICMPEQ);
                // Branch target will be filled in later
                self.bytecode.extend_from_slice(&0u16.to_be_bytes());
            }
            BinaryOp::Ne => {
                self.bytecode.push(opcodes::IF_ICMPNE);
                self.bytecode.extend_from_slice(&0u16.to_be_bytes());
            }
            BinaryOp::Lt => {
                self.bytecode.push(opcodes::IF_ICMPLT);
                self.bytecode.extend_from_slice(&0u16.to_be_bytes());
            }
            BinaryOp::Le => {
                self.bytecode.push(opcodes::IF_ICMPLE);
                self.bytecode.extend_from_slice(&0u16.to_be_bytes());
            }
            BinaryOp::Gt => {
                self.bytecode.push(opcodes::IF_ICMPGT);
                self.bytecode.extend_from_slice(&0u16.to_be_bytes());
            }
            BinaryOp::Ge => {
                self.bytecode.push(opcodes::IF_ICMPGE);
                self.bytecode.extend_from_slice(&0u16.to_be_bytes());
            }
            _ => {
                // TODO: Handle other binary operations
            }
        }
        
        Ok(())
    }
    
    fn generate_unary_operation(&mut self, unary_expr: &UnaryExpr) -> Result<()> {
        // Generate operand
        self.generate_expression(&unary_expr.operand)?;
        
        // Generate operation
        match unary_expr.operator {
            UnaryOp::Minus => {
                self.bytecode.push(opcodes::INEG);
            }
            UnaryOp::Not => {
                // For boolean NOT, we need to handle this specially
                // This is a simplified implementation
                self.bytecode.push(opcodes::ICONST_1);
                self.bytecode.push(opcodes::IXOR);
            }
            _ => {
                // TODO: Handle other unary operations
            }
        }
        
        Ok(())
    }
    
    fn generate_method_call(&mut self, call: &MethodCallExpr) -> Result<()> {
        // Generate receiver if present
        if let Some(target) = &call.target {
            self.generate_expression(target)?;
        } else {
            // Assume 'this' for instance methods
            self.bytecode.push(opcodes::ALOAD_0);
        }
        
        // Generate arguments
        for arg in &call.arguments {
            self.generate_expression(arg)?;
        }
        
        // Generate method call
        let method_ref_index = self.add_method_ref("", &call.name, "()V"); // Simplified
        self.bytecode.push(opcodes::INVOKEVIRTUAL);
        self.bytecode.extend_from_slice(&method_ref_index.to_be_bytes());
        
        Ok(())
    }
    
    fn generate_assignment(&mut self, assign: &AssignmentExpr) -> Result<()> {
        // Generate value
        self.generate_expression(&assign.value)?;
        
        // Generate assignment
        // This is a simplified implementation
        self.bytecode.push(opcodes::ALOAD_0);
        // For now, assume the target is a field name (simplified)
        let field_ref_index = self.add_field_ref("", "field", "I"); // Assume int for now
        self.bytecode.push(opcodes::PUTFIELD);
        self.bytecode.extend_from_slice(&field_ref_index.to_be_bytes());
        
        Ok(())
    }
    
    fn generate_if_statement(&mut self, if_stmt: &IfStmt) -> Result<()> {
        // Generate condition
        self.generate_expression(&if_stmt.condition)?;
        
        // Generate if branch
        let if_start = self.bytecode.len();
        self.generate_statement(&if_stmt.then_branch)?;
        let if_end = self.bytecode.len();
        
        // Generate else branch if present
        if let Some(else_branch) = &if_stmt.else_branch {
            let else_start = self.bytecode.len();
            self.generate_statement(else_branch)?;
            let else_end = self.bytecode.len();
            
            // Update branch targets
            // This is a simplified implementation
        }
        
        Ok(())
    }
    
    fn generate_while_statement(&mut self, while_stmt: &WhileStmt) -> Result<()> {
        // Generate condition
        let condition_start = self.bytecode.len();
        self.generate_expression(&while_stmt.condition)?;
        
        // Generate body
        let body_start = self.bytecode.len();
        self.generate_statement(&while_stmt.body)?;
        
        // Generate loop back
        self.bytecode.push(opcodes::GOTO);
        let loop_target = (condition_start - body_start) as i16;
        self.bytecode.extend_from_slice(&loop_target.to_be_bytes());
        
        Ok(())
    }
    
    fn generate_variable_declaration(&mut self, var_decl: &VariableDeclarator) -> Result<()> {
        // Generate initializer if present
        if let Some(initializer) = &var_decl.initializer {
            self.generate_expression(initializer)?;
        } else {
            // Generate default value - assume int for now (simplified)
            self.bytecode.push(opcodes::ICONST_0);
        }
        
        // Store in local variable
        // This is a simplified implementation
        
        Ok(())
    }
    
    fn load_parameter(&mut self, index: usize, param_type: &TypeRef) -> Result<()> {
        match index {
            0 => self.bytecode.push(opcodes::ILOAD_0),
            1 => self.bytecode.push(opcodes::ILOAD_1),
            2 => self.bytecode.push(opcodes::ILOAD_2),
            3 => self.bytecode.push(opcodes::ILOAD_3),
            _ => {
                self.bytecode.push(opcodes::ILOAD);
                self.bytecode.push(index as u8);
            }
        }
        
        Ok(())
    }
    
    fn generate_return(&mut self, return_type: &TypeRef) -> Result<()> {
        match return_type.name.as_str() {
            "void" => {
                self.bytecode.push(opcodes::RETURN);
            }
            "int" | "boolean" | "char" | "byte" | "short" => {
                self.bytecode.push(opcodes::IRETURN);
            }
            "long" => {
                self.bytecode.push(opcodes::LRETURN);
            }
            "float" => {
                self.bytecode.push(opcodes::FRETURN);
            }
            "double" => {
                self.bytecode.push(opcodes::DRETURN);
            }
            _ => {
                self.bytecode.push(opcodes::ARETURN);
            }
        }
        
        Ok(())
    }
    
    fn add_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        // This is a placeholder - in a real implementation, this would add to the constant pool
        1
    }
    
    fn add_field_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        // This is a placeholder - in a real implementation, this would add to the constant pool
        1
    }
    
    fn add_integer_constant(&mut self, value: i32) -> u16 {
        // This is a placeholder - in a real implementation, this would add to the constant pool
        1
    }
    
    fn add_string_constant(&mut self, value: &str) -> u16 {
        // This is a placeholder - in a real implementation, this would add to the constant pool
        1
    }
    
    fn get_bytecode(self) -> Vec<u8> {
        self.bytecode
    }
}
