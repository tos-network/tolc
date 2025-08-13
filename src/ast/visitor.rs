use super::*;

/// AST visitor trait for traversing and processing AST nodes
pub trait AstVisitor {
    type Output;
    
    // AST root
    fn visit_ast(&mut self, ast: &Ast) -> Self::Output;
    
    // Package and imports
    fn visit_package_decl(&mut self, package: &PackageDecl) -> Self::Output;
    fn visit_import_decl(&mut self, import: &ImportDecl) -> Self::Output;
    
    // Type declarations
    fn visit_class_decl(&mut self, class: &ClassDecl) -> Self::Output;
    fn visit_interface_decl(&mut self, interface: &InterfaceDecl) -> Self::Output;
    fn visit_enum_decl(&mut self, enum_decl: &EnumDecl) -> Self::Output;
    fn visit_annotation_decl(&mut self, annotation: &AnnotationDecl) -> Self::Output;
    fn visit_type_decl(&mut self, type_decl: &TypeDecl) -> Self::Output;
    
    // Class and interface members
    fn visit_field_decl(&mut self, field: &FieldDecl) -> Self::Output;
    fn visit_method_decl(&mut self, method: &MethodDecl) -> Self::Output;
    fn visit_constructor_decl(&mut self, constructor: &ConstructorDecl) -> Self::Output;
    fn visit_parameter(&mut self, parameter: &Parameter) -> Self::Output;
    
    // Statements
    fn visit_block(&mut self, block: &Block) -> Self::Output;
    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output;
    
    // Expressions
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;
    fn visit_literal_expr(&mut self, literal: &LiteralExpr) -> Self::Output;
    fn visit_identifier_expr(&mut self, identifier: &IdentifierExpr) -> Self::Output;
    fn visit_binary_expr(&mut self, binary: &BinaryExpr) -> Self::Output;
    fn visit_unary_expr(&mut self, unary: &UnaryExpr) -> Self::Output;
    fn visit_assignment_expr(&mut self, assignment: &AssignmentExpr) -> Self::Output;
    fn visit_method_call_expr(&mut self, method_call: &MethodCallExpr) -> Self::Output;
    fn visit_field_access_expr(&mut self, field_access: &FieldAccessExpr) -> Self::Output;
    fn visit_array_access_expr(&mut self, array_access: &ArrayAccessExpr) -> Self::Output;
    fn visit_cast_expr(&mut self, cast: &CastExpr) -> Self::Output;
    fn visit_instance_of_expr(&mut self, instance_of: &InstanceOfExpr) -> Self::Output;
    fn visit_conditional_expr(&mut self, conditional: &ConditionalExpr) -> Self::Output;
    fn visit_new_expr(&mut self, new: &NewExpr) -> Self::Output;
    
    // Types
    fn visit_type_ref(&mut self, type_ref: &TypeRef) -> Self::Output;
    fn visit_type_param(&mut self, type_param: &TypeParam) -> Self::Output;
    
    // Annotations
    fn visit_annotation(&mut self, annotation: &Annotation) -> Self::Output;
}

/// Default implementation for AST visitor
/// This provides a basic traversal implementation that can be overridden
impl<T> AstVisitor for T
where
    T: Default,
{
    type Output = T;
    
    fn visit_ast(&mut self, ast: &Ast) -> Self::Output {
        // Visit package declaration if present
        if let Some(ref package) = ast.package_decl {
            self.visit_package_decl(package);
        }
        
        // Visit imports
        for import in &ast.imports {
            self.visit_import_decl(import);
        }
        
        // Visit type declarations
        for type_decl in &ast.type_decls {
            match type_decl {
                TypeDecl::Class(c) => self.visit_class_decl(c),
                TypeDecl::Interface(i) => self.visit_interface_decl(i),
                TypeDecl::Enum(e) => self.visit_enum_decl(e),
                TypeDecl::Annotation(a) => self.visit_annotation_decl(a),
            };
        }
        
        // Return a default value
        T::default()
    }
    
    fn visit_package_decl(&mut self, _package: &PackageDecl) -> Self::Output {
        T::default()
    }
    
    fn visit_import_decl(&mut self, _import: &ImportDecl) -> Self::Output {
        T::default()
    }
    
    fn visit_class_decl(&mut self, class: &ClassDecl) -> Self::Output {
        // Visit type parameters
        for type_param in &class.type_params {
            self.visit_type_param(type_param);
        }
        
        // Visit extends clause
        if let Some(ref extends) = class.extends {
            self.visit_type_ref(extends);
        }
        
        // Visit implements clauses
        for implements in &class.implements {
            self.visit_type_ref(implements);
        }
        
        // Visit class body
        for member in &class.body {
            match member {
                ClassMember::Field(f) => self.visit_field_decl(f),
                ClassMember::Method(m) => self.visit_method_decl(m),
                ClassMember::Constructor(c) => self.visit_constructor_decl(c),
                ClassMember::Initializer(i) => self.visit_block(&i.body),
                ClassMember::TypeDecl(t) => match t {
                    TypeDecl::Class(c) => self.visit_class_decl(c),
                    TypeDecl::Interface(i) => self.visit_interface_decl(i),
                    TypeDecl::Enum(e) => self.visit_enum_decl(e),
                    TypeDecl::Annotation(a) => self.visit_annotation_decl(a),
                },
            };
        }
        
        T::default()
    }
    
    fn visit_interface_decl(&mut self, interface: &InterfaceDecl) -> Self::Output {
        // Visit type parameters
        for type_param in &interface.type_params {
            self.visit_type_param(type_param);
        }
        
        // Visit extends clauses
        for extends in &interface.extends {
            self.visit_type_ref(extends);
        }
        
        // Visit interface body
        for member in &interface.body {
            match member {
                InterfaceMember::Method(m) => self.visit_method_decl(m),
                InterfaceMember::Field(f) => self.visit_field_decl(f),
                InterfaceMember::TypeDecl(t) => match t {
                    TypeDecl::Class(c) => self.visit_class_decl(c),
                    TypeDecl::Interface(i) => self.visit_interface_decl(i),
                    TypeDecl::Enum(e) => self.visit_enum_decl(e),
                    TypeDecl::Annotation(a) => self.visit_annotation_decl(a),
                },
            };
        }
        
        T::default()
    }
    
    fn visit_enum_decl(&mut self, _enum_decl: &EnumDecl) -> Self::Output {
        T::default()
    }
    
    fn visit_annotation_decl(&mut self, _annotation: &AnnotationDecl) -> Self::Output {
        T::default()
    }
    
    fn visit_type_decl(&mut self, type_decl: &TypeDecl) -> Self::Output {
        match type_decl {
            TypeDecl::Class(c) => self.visit_class_decl(c),
            TypeDecl::Interface(i) => self.visit_interface_decl(i),
            TypeDecl::Enum(e) => self.visit_enum_decl(e),
            TypeDecl::Annotation(a) => self.visit_annotation_decl(a),
        }
    }
    
    fn visit_field_decl(&mut self, field: &FieldDecl) -> Self::Output {
        // Visit annotations
        for annotation in &field.annotations {
            self.visit_annotation(annotation);
        }
        
        // Visit type reference
        self.visit_type_ref(&field.type_ref);
        
        // Visit initializer if present
        if let Some(ref initializer) = field.initializer {
            self.visit_expr(initializer);
        }
        
        T::default()
    }
    
    fn visit_method_decl(&mut self, method: &MethodDecl) -> Self::Output {
        // Visit annotations
        for annotation in &method.annotations {
            self.visit_annotation(annotation);
        }
        
        // Visit type parameters
        for type_param in &method.type_params {
            self.visit_type_param(type_param);
        }
        
        // Visit return type
        if let Some(ref return_type) = method.return_type {
            self.visit_type_ref(return_type);
        }
        
        // Visit parameters
        for parameter in &method.parameters {
            self.visit_parameter(parameter);
        }
        
        // Visit throws clauses
        for throws in &method.throws {
            self.visit_type_ref(throws);
        }
        
        // Visit method body
        if let Some(ref body) = method.body {
            self.visit_block(body);
        }
        
        T::default()
    }
    
    fn visit_constructor_decl(&mut self, constructor: &ConstructorDecl) -> Self::Output {
        // Visit annotations
        for annotation in &constructor.annotations {
            self.visit_annotation(annotation);
        }
        
        // Visit parameters
        for parameter in &constructor.parameters {
            self.visit_parameter(parameter);
        }
        
        // Visit throws clauses
        for throws in &constructor.throws {
            self.visit_type_ref(throws);
        }
        
        // Visit constructor body
        self.visit_block(&constructor.body);
        
        T::default()
    }
    
    fn visit_parameter(&mut self, parameter: &Parameter) -> Self::Output {
        // Visit annotations
        for annotation in &parameter.annotations {
            self.visit_annotation(annotation);
        }
        
        // Visit type reference
        self.visit_type_ref(&parameter.type_ref);
        
        T::default()
    }
    
    fn visit_block(&mut self, block: &Block) -> Self::Output {
        // Visit all statements in the block
        for stmt in &block.statements {
            self.visit_stmt(stmt);
        }
        
        T::default()
    }
    
    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output {
        match stmt {
            Stmt::Expression(expr_stmt) => self.visit_expr(&expr_stmt.expr),
            Stmt::Declaration(var_decl) => {
                self.visit_type_ref(&var_decl.type_ref);
                for var in &var_decl.variables {
                    if let Some(ref initializer) = var.initializer {
                        self.visit_expr(initializer);
                    }
                }
                T::default()
            }
            Stmt::If(if_stmt) => {
                self.visit_expr(&if_stmt.condition);
                self.visit_stmt(&if_stmt.then_branch);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.visit_stmt(else_branch);
                }
                T::default()
            }
            Stmt::TypeDecl(type_decl) => {
                self.visit_type_decl(type_decl)
            }
            Stmt::While(while_stmt) => {
                self.visit_expr(&while_stmt.condition);
                self.visit_stmt(&while_stmt.body);
                T::default()
            }
            Stmt::For(for_stmt) => {
                for init in &for_stmt.init {
                    self.visit_stmt(init);
                }
                if let Some(ref condition) = for_stmt.condition {
                    self.visit_expr(condition);
                }
                for update in &for_stmt.update {
                    self.visit_expr(&update.expr);
                }
                self.visit_stmt(&for_stmt.body);
                T::default()
            }
            Stmt::Switch(switch_stmt) => {
                self.visit_expr(&switch_stmt.expression);
                for case in &switch_stmt.cases {
                    for label in &case.labels {
                        self.visit_expr(label);
                    }
                    for stmt in &case.statements {
                        self.visit_stmt(stmt);
                    }
                }
                T::default()
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref value) = return_stmt.value {
                    self.visit_expr(value);
                }
                T::default()
            }
            Stmt::Break(_) | Stmt::Continue(_) => T::default(),
            Stmt::Try(try_stmt) => {
                // Resources
                for res in &try_stmt.resources {
                    match res {
                        TryResource::Var { type_ref, initializer, .. } => {
                            self.visit_type_ref(type_ref);
                            self.visit_expr(initializer);
                        }
                        TryResource::Expr { expr, .. } => {
                            self.visit_expr(expr);
                        }
                    }
                }
                self.visit_block(&try_stmt.try_block);
                for catch in &try_stmt.catch_clauses {
                    self.visit_parameter(&catch.parameter);
                    for alt in &catch.alt_types { self.visit_type_ref(alt); }
                    self.visit_block(&catch.block);
                }
                if let Some(ref finally_block) = try_stmt.finally_block {
                    self.visit_block(finally_block);
                }
                T::default()
            }
            Stmt::Throw(throw_stmt) => {
                self.visit_expr(&throw_stmt.expr);
                T::default()
            }
            Stmt::Assert(assert_stmt) => {
                self.visit_expr(&assert_stmt.condition);
                if let Some(msg) = &assert_stmt.message { self.visit_expr(msg); }
                T::default()
            }
            Stmt::Synchronized(sync_stmt) => {
                self.visit_expr(&sync_stmt.lock);
                self.visit_block(&sync_stmt.body);
                T::default()
            }
            Stmt::Labeled(labeled_stmt) => {
                self.visit_stmt(&labeled_stmt.statement);
                T::default()
            }
            Stmt::Block(block) => self.visit_block(block),
            Stmt::Empty => T::default()
        }
    }
    
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        match expr {
            Expr::Literal(literal) => self.visit_literal_expr(literal),
            Expr::Identifier(identifier) => self.visit_identifier_expr(identifier),
            Expr::Binary(binary) => self.visit_binary_expr(binary),
            Expr::Unary(unary) => self.visit_unary_expr(unary),
            Expr::Assignment(assignment) => self.visit_assignment_expr(assignment),
            Expr::MethodCall(method_call) => self.visit_method_call_expr(method_call),
            Expr::FieldAccess(field_access) => self.visit_field_access_expr(field_access),
            Expr::ArrayAccess(array_access) => self.visit_array_access_expr(array_access),
            Expr::Cast(cast) => self.visit_cast_expr(cast),
            Expr::InstanceOf(instance_of) => self.visit_instance_of_expr(instance_of),
            Expr::Conditional(conditional) => self.visit_conditional_expr(conditional),
            Expr::New(new) => self.visit_new_expr(new),
            Expr::Parenthesized(expr) => self.visit_expr(expr),
            Expr::ArrayInitializer(values) => {
                for v in values { self.visit_expr(v); }
                T::default()
            }
        }
    }
    
    fn visit_literal_expr(&mut self, _literal: &LiteralExpr) -> Self::Output {
        // No child nodes to visit for primitive literals
        // Return a default value or handle as needed by the specific visitor
        T::default()
    }
    
    fn visit_identifier_expr(&mut self, _identifier: &IdentifierExpr) -> Self::Output {
        // No child nodes to visit for identifiers
        // Return a default value or handle as needed by the specific visitor
        T::default()
    }
    
    fn visit_binary_expr(&mut self, binary: &BinaryExpr) -> Self::Output {
        self.visit_expr(&binary.left);
        self.visit_expr(&binary.right);
        T::default()
    }
    
    fn visit_unary_expr(&mut self, unary: &UnaryExpr) -> Self::Output {
        self.visit_expr(&unary.operand);
        T::default()
    }
    
    fn visit_assignment_expr(&mut self, assignment: &AssignmentExpr) -> Self::Output {
        self.visit_expr(&assignment.target);
        self.visit_expr(&assignment.value);
        T::default()
    }
    
    fn visit_method_call_expr(&mut self, method_call: &MethodCallExpr) -> Self::Output {
        if let Some(ref target) = method_call.target {
            self.visit_expr(target);
        }
        for arg in &method_call.arguments {
            self.visit_expr(arg);
        }
        T::default()
    }
    
    fn visit_field_access_expr(&mut self, field_access: &FieldAccessExpr) -> Self::Output {
        if let Some(ref target) = field_access.target {
            self.visit_expr(target);
        }
        T::default()
    }
    
    fn visit_array_access_expr(&mut self, array_access: &ArrayAccessExpr) -> Self::Output {
        self.visit_expr(&array_access.array);
        self.visit_expr(&array_access.index);
        T::default()
    }
    
    fn visit_cast_expr(&mut self, cast: &CastExpr) -> Self::Output {
        self.visit_type_ref(&cast.target_type);
        self.visit_expr(&cast.expr);
        T::default()
    }
    
    fn visit_instance_of_expr(&mut self, instance_of: &InstanceOfExpr) -> Self::Output {
        self.visit_expr(&instance_of.expr);
        self.visit_type_ref(&instance_of.target_type);
        T::default()
    }
    
    fn visit_conditional_expr(&mut self, conditional: &ConditionalExpr) -> Self::Output {
        self.visit_expr(&conditional.condition);
        self.visit_expr(&conditional.then_expr);
        self.visit_expr(&conditional.else_expr);
        T::default()
    }
    
    fn visit_new_expr(&mut self, new: &NewExpr) -> Self::Output {
        self.visit_type_ref(&new.target_type);
        for arg in &new.arguments {
            self.visit_expr(arg);
        }
        T::default()
    }
    
    fn visit_type_ref(&mut self, _type_ref: &TypeRef) -> Self::Output {
        T::default()
    }
    
    fn visit_type_param(&mut self, _type_param: &TypeParam) -> Self::Output {
        T::default()
    }
    
    fn visit_annotation(&mut self, _annotation: &Annotation) -> Self::Output {
        T::default()
    }
}
