use super::nodes::*;
use super::visitor::AstVisitor;
use super::Ast;

/// AST printer for debugging and output
pub struct AstPrinter {
    indent_level: usize,
    output: String,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            output: String::new(),
        }
    }
    
    pub fn print(&mut self, ast: &Ast) -> String {
        self.output.clear();
        self.visit_ast(ast);
        self.output.clone()
    }
    
    fn indent(&mut self) {
        self.indent_level += 2;
    }
    
    fn dedent(&mut self) {
        if self.indent_level >= 2 {
            self.indent_level -= 2;
        }
    }
    
    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.output.push(' ');
        }
    }
    
    fn writeln(&mut self, s: &str) {
        self.write_indent();
        self.output.push_str(s);
        self.output.push('\n');
    }
}

impl AstVisitor for AstPrinter {
    type Output = ();
    
    fn visit_ast(&mut self, ast: &Ast) {
        if let Some(ref package) = ast.package_decl {
            self.visit_package_decl(package);
        }
        
        for import in &ast.imports {
            self.visit_import_decl(import);
        }
        
        for type_decl in &ast.type_decls {
            self.visit_type_decl(type_decl);
        }
    }
    
    fn visit_package_decl(&mut self, package: &PackageDecl) {
        self.writeln(&format!("package {};", package.name));
        self.output.push('\n');
    }
    
    fn visit_import_decl(&mut self, import: &ImportDecl) {
        let mut import_str = String::new();
        if import.is_static {
            import_str.push_str("import static ");
        } else {
            import_str.push_str("import ");
        }
        
        if import.is_wildcard {
            import_str.push_str(&format!("{}.*;", import.name));
        } else {
            import_str.push_str(&format!("{};", import.name));
        }
        
        self.writeln(&import_str);
    }
    
    fn visit_class_decl(&mut self, class: &ClassDecl) {
        self.write_indent();
        // Modifiers
        for modifier in &class.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        self.output.push_str("class ");
        self.output.push_str(&class.name);
        
        if !class.type_params.is_empty() {
            self.output.push('<');
            for (i, param) in class.type_params.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(&param.name);
            }
            self.output.push('>');
        }
        
        if let Some(ref extends) = class.extends {
            self.output.push_str(" extends ");
            self.visit_type_ref(extends);
        }
        
        if !class.implements.is_empty() {
            self.output.push_str(" implements ");
            for (i, implements) in class.implements.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.visit_type_ref(implements);
            }
        }
        
        self.output.push_str(" {\n");
        self.indent();
        
        for member in &class.body {
            match member {
                ClassMember::Field(f) => self.visit_field_decl(f),
                ClassMember::Method(m) => self.visit_method_decl(m),
                ClassMember::Constructor(c) => self.visit_constructor_decl(c),
                ClassMember::Initializer(i) => self.visit_block(&i.body),
                ClassMember::TypeDecl(t) => self.visit_type_decl(t),
            }
        }
        
        self.dedent();
        self.writeln("}");
        self.output.push('\n');
    }
    
    fn visit_interface_decl(&mut self, interface: &InterfaceDecl) {
        self.write_indent();
        // Modifiers
        for modifier in &interface.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        self.output.push_str("interface ");
        self.output.push_str(&interface.name);
        
        if !interface.type_params.is_empty() {
            self.output.push('<');
            for (i, param) in interface.type_params.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(&param.name);
            }
            self.output.push('>');
        }
        
        if !interface.extends.is_empty() {
            self.output.push_str(" extends ");
            for (i, extends) in interface.extends.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.visit_type_ref(extends);
            }
        }
        
        self.output.push_str(" {\n");
        self.indent();
        
        for member in &interface.body {
            match member {
                InterfaceMember::Method(m) => self.visit_method_decl(m),
                InterfaceMember::Field(f) => self.visit_field_decl(f),
                InterfaceMember::TypeDecl(t) => self.visit_type_decl(t),
            }
        }
        
        self.dedent();
        self.writeln("}");
        self.output.push('\n');
    }
    
    fn visit_enum_decl(&mut self, enum_decl: &EnumDecl) {
        self.write_indent();
        // Modifiers
        for modifier in &enum_decl.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        self.output.push_str("enum ");
        self.output.push_str(&enum_decl.name);

        if !enum_decl.implements.is_empty() {
            self.output.push_str(" implements ");
            for (i, imp) in enum_decl.implements.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.visit_type_ref(imp);
            }
        }

        self.output.push_str(" {\n");
        self.indent();

        // Enum constants
        if !enum_decl.constants.is_empty() {
            self.write_indent();
            for (i, c) in enum_decl.constants.iter().enumerate() {
                if i > 0 { self.output.push_str(", "); }
                self.output.push_str(&c.name);
                if !c.arguments.is_empty() {
                    self.output.push('(');
                    for (j, arg) in c.arguments.iter().enumerate() {
                        if j > 0 { self.output.push_str(", "); }
                        self.visit_expr(arg);
                    }
                    self.output.push(')');
                }
                if let Some(ref body) = c.body {
                    self.output.push_str(" {\n");
                    self.indent();
                    for m in &body.body {
                        match m {
                            ClassMember::Field(f) => self.visit_field_decl(f),
                            ClassMember::Method(m) => self.visit_method_decl(m),
                            ClassMember::Constructor(k) => self.visit_constructor_decl(k),
                            ClassMember::Initializer(i) => self.visit_block(&i.body),
                            ClassMember::TypeDecl(t) => self.visit_type_decl(t),
                        }
                    }
                    self.dedent();
                    self.writeln("}");
                }
            }
            self.output.push_str(";\n");
        }

        // Members
        for member in &enum_decl.body {
            match member {
                ClassMember::Field(f) => self.visit_field_decl(f),
                ClassMember::Method(m) => self.visit_method_decl(m),
                ClassMember::Constructor(k) => self.visit_constructor_decl(k),
                ClassMember::Initializer(i) => self.visit_block(&i.body),
                ClassMember::TypeDecl(t) => self.visit_type_decl(t),
            }
        }

        self.dedent();
        self.writeln("}");
        self.output.push('\n');
    }
    
    fn visit_annotation_decl(&mut self, annotation: &AnnotationDecl) {
        self.write_indent();
        // Modifiers
        for modifier in &annotation.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        self.output.push_str("@interface ");
        self.output.push_str(&annotation.name);
        self.output.push_str(" {\n");
        self.indent();
        for member in &annotation.body {
            self.write_indent();
            self.visit_type_ref(&member.type_ref);
            self.output.push(' ');
            self.output.push_str(&member.name);
            self.output.push('(');
            self.output.push(')');
            if let Some(ref dv) = member.default_value {
                self.output.push_str(" default ");
                self.visit_expr(dv);
            }
            self.output.push_str(";\n");
        }
        self.dedent();
        self.writeln("}");
        self.output.push('\n');
    }
    
    fn visit_type_decl(&mut self, type_decl: &TypeDecl) {
        match type_decl {
            TypeDecl::Class(c) => self.visit_class_decl(c),
            TypeDecl::Interface(i) => self.visit_interface_decl(i),
            TypeDecl::Enum(e) => self.visit_enum_decl(e),
            TypeDecl::Annotation(a) => self.visit_annotation_decl(a),
        }
    }
    
    fn visit_field_decl(&mut self, field: &FieldDecl) {
        // Annotations
        for ann in &field.annotations {
            self.write_indent();
            self.visit_annotation(ann);
            self.output.push('\n');
        }
        self.write_indent();
        
        // Modifiers
        for modifier in &field.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        
        // Type
        self.visit_type_ref(&field.type_ref);
        self.output.push(' ');
        
        // Name
        self.output.push_str(&field.name);
        
        // Array dimensions are already handled in visit_type_ref
        
        // Initializer
        if let Some(ref initializer) = field.initializer {
            self.output.push_str(" = ");
            self.visit_expr(initializer);
        }
        
        self.output.push_str(";\n");
    }
    
    fn visit_method_decl(&mut self, method: &MethodDecl) {
        // Annotations
        for ann in &method.annotations {
            self.write_indent();
            self.visit_annotation(ann);
            self.output.push('\n');
        }
        self.write_indent();
        
        // Modifiers
        for modifier in &method.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        
        // Method type parameters
        if !method.type_params.is_empty() {
            self.output.push('<');
            for (i, tp) in method.type_params.iter().enumerate() {
                if i > 0 { self.output.push_str(", "); }
                self.output.push_str(&tp.name);
            }
            self.output.push_str("> ");
        }

        // Return type
        if let Some(ref return_type) = method.return_type {
            self.visit_type_ref(return_type);
        } else {
            self.output.push_str("void");
        }
        self.output.push(' ');
        
        // Name
        self.output.push_str(&method.name);
        
        // Parameters
        self.output.push('(');
        for (i, param) in method.parameters.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.visit_parameter(param);
        }
        self.output.push(')');
        
        // Throws
        if !method.throws.is_empty() {
            self.output.push_str(" throws ");
            for (i, throws) in method.throws.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.visit_type_ref(throws);
            }
        }
        
        // Body
        if let Some(ref body) = method.body {
            self.output.push_str(" {\n");
            self.indent();
            self.visit_block(body);
            self.dedent();
            self.writeln("}");
        } else {
            self.output.push_str(";\n");
        }
    }
    
    fn visit_constructor_decl(&mut self, constructor: &ConstructorDecl) {
        // Annotations
        for ann in &constructor.annotations {
            self.write_indent();
            self.visit_annotation(ann);
            self.output.push('\n');
        }
        self.write_indent();
        
        // Modifiers
        for modifier in &constructor.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        
        // Name
        self.output.push_str(&constructor.name);
        
        // Parameters
        self.output.push('(');
        for (i, param) in constructor.parameters.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.visit_parameter(param);
        }
        self.output.push(')');
        
        // Throws
        if !constructor.throws.is_empty() {
            self.output.push_str(" throws ");
            for (i, throws) in constructor.throws.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.visit_type_ref(throws);
            }
        }
        
        // Body
        self.output.push_str(" {\n");
        self.indent();
        self.visit_block(&constructor.body);
        self.dedent();
        self.writeln("}");
    }
    
    fn visit_parameter(&mut self, parameter: &Parameter) {
        // Modifiers
        for modifier in &parameter.modifiers {
            self.output.push_str(&format!("{:?} ", modifier).to_lowercase());
        }
        // Annotations
        for ann in &parameter.annotations {
            self.visit_annotation(ann);
            self.output.push(' ');
        }
        
        // Type
        self.visit_type_ref(&parameter.type_ref);
        
        // Varargs
        if parameter.varargs {
            self.output.push_str("...");
        }
        
        self.output.push(' ');
        self.output.push_str(&parameter.name);
    }
    
    fn visit_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.visit_stmt(stmt);
        }
    }
    
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.write_indent();
                self.visit_expr(&expr_stmt.expr);
                self.output.push_str(";\n");
            }
            Stmt::Declaration(var_decl) => {
                self.write_indent();
                self.visit_type_ref(&var_decl.type_ref);
                self.output.push(' ');
                
                for (i, var) in var_decl.variables.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.output.push_str(&var.name);
                    
                    for _ in 0..var.array_dims {
                        self.output.push_str("[]");
                    }
                    
                    if let Some(ref initializer) = var.initializer {
                        self.output.push_str(" = ");
                        self.visit_expr(initializer);
                    }
                }
                self.output.push_str(";\n");
            }
            Stmt::If(if_stmt) => {
                self.write_indent();
                self.output.push_str("if (");
                self.visit_expr(&if_stmt.condition);
                self.output.push_str(") ");
                self.visit_stmt(&if_stmt.then_branch);
                
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.write_indent();
                    self.output.push_str("else ");
                    self.visit_stmt(else_branch);
                }
            }
            Stmt::TypeDecl(type_decl) => {
                self.visit_type_decl(type_decl);
            }
            Stmt::While(while_stmt) => {
                self.write_indent();
                self.output.push_str("while (");
                self.visit_expr(&while_stmt.condition);
                self.output.push_str(") ");
                self.visit_stmt(&while_stmt.body);
            }
            Stmt::DoWhile(do_while_stmt) => {
                self.write_indent();
                self.output.push_str("do ");
                self.visit_stmt(&do_while_stmt.body);
                self.output.push_str(" while (");
                self.visit_expr(&do_while_stmt.condition);
                self.output.push_str(");\n");
            }
            Stmt::For(for_stmt) => {
                self.write_indent();
                self.output.push_str("for (");
                
                // Init
                for (i, init) in for_stmt.init.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.visit_stmt(init);
                }
                self.output.push_str("; ");
                
                // Condition
                if let Some(ref condition) = for_stmt.condition {
                    self.visit_expr(condition);
                }
                self.output.push_str("; ");
                
                // Update
                for (i, update) in for_stmt.update.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.visit_expr(&update.expr);
                }
                
                self.output.push_str(") ");
                self.visit_stmt(&for_stmt.body);
            }
            Stmt::Return(return_stmt) => {
                self.write_indent();
                self.output.push_str("return");
                if let Some(ref value) = return_stmt.value {
                    self.output.push(' ');
                    self.visit_expr(value);
                }
                self.output.push_str(";\n");
            }
            Stmt::Break(break_stmt) => {
                self.write_indent();
                self.output.push_str("break");
                if let Some(ref label) = break_stmt.label {
                    self.output.push_str(&format!(" {}", label));
                }
                self.output.push_str(";\n");
            }
            Stmt::Continue(continue_stmt) => {
                self.write_indent();
                self.output.push_str("continue");
                if let Some(ref label) = continue_stmt.label {
                    self.output.push_str(&format!(" {}", label));
                }
                self.output.push_str(";\n");
            }
            Stmt::Try(try_stmt) => {
                self.write_indent();
                if !try_stmt.resources.is_empty() {
                    self.output.push_str("try (");
                    for (i, res) in try_stmt.resources.iter().enumerate() {
                        if i > 0 { self.output.push_str("; "); }
                        match res {
                            TryResource::Var { type_ref, name, initializer, .. } => {
                                self.visit_type_ref(type_ref);
                                self.output.push(' ');
                                self.output.push_str(name);
                                self.output.push_str(" = ");
                                self.visit_expr(initializer);
                            }
                            TryResource::Expr { expr, .. } => {
                                self.visit_expr(expr);
                            }
                        }
                    }
                    self.output.push_str(") ");
                } else {
                    self.output.push_str("try ");
                }
                self.visit_block(&try_stmt.try_block);
                
                for catch in &try_stmt.catch_clauses {
                    self.write_indent();
                    self.output.push_str("catch (");
                    // print main type and alternatives if present
                    self.visit_type_ref(&catch.parameter.type_ref);
                    for alt in &catch.alt_types {
                        self.output.push_str(" | ");
                        self.visit_type_ref(alt);
                    }
                    self.output.push_str(" ");
                    self.output.push_str(&catch.parameter.name);
                    self.output.push_str(") ");
                    self.visit_block(&catch.block);
                }
                
                if let Some(ref finally_block) = try_stmt.finally_block {
                    self.write_indent();
                    self.output.push_str("finally ");
                    self.visit_block(finally_block);
                }
            }
            Stmt::Throw(throw_stmt) => {
                self.write_indent();
                self.output.push_str("throw ");
                self.visit_expr(&throw_stmt.expr);
                self.output.push_str(";\n");
            }
            Stmt::Assert(a) => {
                self.write_indent();
                self.output.push_str("assert ");
                self.visit_expr(&a.condition);
                if let Some(m) = &a.message {
                    self.output.push_str(" : ");
                    self.visit_expr(m);
                }
                self.output.push_str(";\n");
            }
            Stmt::Synchronized(s) => {
                self.write_indent();
                self.output.push_str("synchronized (");
                self.visit_expr(&s.lock);
                self.output.push_str(") ");
                self.output.push_str("{\n");
                self.indent();
                self.visit_block(&s.body);
                self.dedent();
                self.writeln("}");
            }
            Stmt::Labeled(l) => {
                self.write_indent();
                self.output.push_str(&format!("{}: ", l.label));
                self.visit_stmt(&l.statement);
            }
            Stmt::Block(block) => {
                self.output.push_str("{\n");
                self.indent();
                self.visit_block(block);
                self.dedent();
                self.writeln("}");
            }
            Stmt::Empty => {}
            Stmt::Switch(switch_stmt) => {
                self.write_indent();
                self.output.push_str("switch (");
                self.visit_expr(&switch_stmt.expression);
                self.output.push_str(") {\n");
                self.indent();
                for case in &switch_stmt.cases {
                    if case.labels.is_empty() {
                        self.write_indent();
                        self.output.push_str("default:\n");
                    } else {
                        for label in &case.labels {
                            self.write_indent();
                            self.output.push_str("case ");
                            self.visit_expr(label);
                            self.output.push_str(":\n");
                        }
                    }
                    for s in &case.statements {
                        self.visit_stmt(s);
                    }
                }
                self.dedent();
                self.writeln("}");
            }
        }
    }
    
    fn visit_expr(&mut self, expr: &Expr) {
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
            Expr::Parenthesized(expr) => {
                self.output.push('(');
                self.visit_expr(expr);
                self.output.push(')');
            }
            Expr::ArrayInitializer(values) => {
                self.output.push('{');
                for (i, v) in values.iter().enumerate() {
                    if i > 0 { self.output.push_str(", "); }
                    self.visit_expr(v);
                }
                self.output.push('}');
            }
            Expr::Lambda(lambda) => self.visit_lambda_expr(lambda),
            Expr::MethodReference(method_ref) => self.visit_method_reference_expr(method_ref),
        }
    }
    
    fn visit_literal_expr(&mut self, literal: &LiteralExpr) {
        match &literal.value {
            Literal::Integer(i) => self.output.push_str(&i.to_string()),
            Literal::Float(f) => self.output.push_str(&f.to_string()),
            Literal::Boolean(b) => self.output.push_str(&b.to_string()),
            Literal::String(s) => {
                self.output.push('"');
                self.output.push_str(s);
                self.output.push('"');
            }
            Literal::Char(c) => {
                self.output.push('\'');
                self.output.push(*c);
                self.output.push('\'');
            }
            Literal::Long(l) => {
                self.output.push_str(&l.to_string());
                self.output.push('L');
            },
            Literal::Double(d) => {
                self.output.push_str(&d.to_string());
                if !d.to_string().contains('.') && !d.to_string().contains('e') && !d.to_string().contains('E') {
                    self.output.push_str(".0");
                }
                self.output.push('D');
            },
            Literal::Null => self.output.push_str("null"),
        }
    }
    
    fn visit_identifier_expr(&mut self, identifier: &IdentifierExpr) {
        self.output.push_str(&identifier.name);
    }
    
    fn visit_binary_expr(&mut self, binary: &BinaryExpr) {
        self.visit_expr(&binary.left);
        self.output.push(' ');
        
        // Use proper operator symbols instead of debug format
        let op_str = match binary.operator {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-", 
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::And => "&",
            BinaryOp::Or => "|",
            BinaryOp::Xor => "^",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",
            BinaryOp::URShift => ">>>",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
        };
        
        self.output.push_str(op_str);
        self.output.push(' ');
        self.visit_expr(&binary.right);
    }
    
    fn visit_unary_expr(&mut self, unary: &UnaryExpr) {
        match unary.operator {
            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::Plus | UnaryOp::Minus | UnaryOp::Not | UnaryOp::BitNot => {
                self.output.push_str(&format!("{:?}", unary.operator).to_lowercase());
                self.visit_expr(&unary.operand);
            }
            UnaryOp::PostInc | UnaryOp::PostDec => {
                self.visit_expr(&unary.operand);
                self.output.push_str(&format!("{:?}", unary.operator).to_lowercase());
            }
        }
    }
    
    fn visit_assignment_expr(&mut self, assignment: &AssignmentExpr) {
        self.visit_expr(&assignment.target);
        self.output.push_str(&format!(" {:?} ", assignment.operator).to_lowercase());
        self.visit_expr(&assignment.value);
    }
    
    fn visit_method_call_expr(&mut self, method_call: &MethodCallExpr) {
        if let Some(ref target) = method_call.target {
            self.visit_expr(target);
            self.output.push('.');
        }
        
        self.output.push_str(&method_call.name);
        self.output.push('(');
        
        for (i, arg) in method_call.arguments.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.visit_expr(arg);
        }
        
        self.output.push(')');
    }
    
    fn visit_field_access_expr(&mut self, field_access: &FieldAccessExpr) {
        if let Some(ref target) = field_access.target {
            self.visit_expr(target);
            self.output.push('.');
        }
        
        self.output.push_str(&field_access.name);
    }
    
    fn visit_array_access_expr(&mut self, array_access: &ArrayAccessExpr) {
        self.visit_expr(&array_access.array);
        self.output.push('[');
        self.visit_expr(&array_access.index);
        self.output.push(']');
    }
    
    fn visit_cast_expr(&mut self, cast: &CastExpr) {
        self.output.push('(');
        self.visit_type_ref(&cast.target_type);
        self.output.push(')');
        self.visit_expr(&cast.expr);
    }
    
    fn visit_instance_of_expr(&mut self, instance_of: &InstanceOfExpr) {
        self.visit_expr(&instance_of.expr);
        self.output.push_str(" instanceof ");
        self.visit_type_ref(&instance_of.target_type);
    }
    
    fn visit_conditional_expr(&mut self, conditional: &ConditionalExpr) {
        self.visit_expr(&conditional.condition);
        self.output.push_str(" ? ");
        self.visit_expr(&conditional.then_expr);
        self.output.push_str(" : ");
        self.visit_expr(&conditional.else_expr);
    }
    
    fn visit_new_expr(&mut self, new: &NewExpr) {
        self.output.push_str("new ");
        self.visit_type_ref(&new.target_type);
        self.output.push('(');
        
        for (i, arg) in new.arguments.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.visit_expr(arg);
        }
        
        self.output.push(')');
        if let Some(body) = &new.anonymous_body {
            self.output.push_str(" {");
            self.output.push('\n');
            self.indent();
            for m in &body.body {
                match m {
                    ClassMember::Field(f) => self.visit_field_decl(f),
                    ClassMember::Method(m) => self.visit_method_decl(m),
                    ClassMember::Constructor(k) => self.visit_constructor_decl(k),
                    ClassMember::Initializer(i) => self.visit_block(&i.body),
                    ClassMember::TypeDecl(t) => self.visit_type_decl(t),
                }
            }
            self.dedent();
            self.writeln("}");
        }
    }
    
    fn visit_lambda_expr(&mut self, lambda: &LambdaExpr) {
        // Print parameters
        if lambda.parameters.len() == 1 && lambda.parameters[0].type_ref.is_none() {
            // Single parameter without type - no parentheses needed
            self.output.push_str(&lambda.parameters[0].name);
        } else {
            // Multiple parameters or typed parameters - need parentheses
            self.output.push('(');
            for (i, param) in lambda.parameters.iter().enumerate() {
                if i > 0 { self.output.push_str(", "); }
                if let Some(ref type_ref) = param.type_ref {
                    self.visit_type_ref(type_ref);
                    self.output.push(' ');
                }
                self.output.push_str(&param.name);
            }
            self.output.push(')');
        }
        
        self.output.push_str(" -> ");
        
        // Print body
        match &lambda.body {
            LambdaBody::Expression(expr) => self.visit_expr(expr),
            LambdaBody::Block(block) => self.visit_block(block),
        }
    }
    
    fn visit_method_reference_expr(&mut self, method_ref: &MethodReferenceExpr) {
        if let Some(ref target) = method_ref.target {
            self.visit_expr(target);
        } else {
            // Static method reference - class name should be inferred from context
            self.output.push_str("Class");
        }
        self.output.push_str("::");
        if method_ref.is_constructor {
            self.output.push_str("new");
        } else {
            self.output.push_str(&method_ref.method_name);
        }
    }
    
    fn visit_type_ref(&mut self, type_ref: &TypeRef) {
        self.output.push_str(&type_ref.name);
        
        if !type_ref.type_args.is_empty() {
            self.output.push('<');
            for (i, arg) in type_ref.type_args.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                match arg {
                    crate::ast::TypeArg::Type(t) => self.visit_type_ref(t),
                    crate::ast::TypeArg::Wildcard(w) => {
                        self.output.push('?');
                        if let Some((bk, tr)) = &w.bound {
                            match bk { crate::ast::BoundKind::Extends => self.output.push_str(" extends "), crate::ast::BoundKind::Super => self.output.push_str(" super "), }
                            self.visit_type_ref(tr);
                        }
                    }
                }
            }
            self.output.push('>');
        }
        
        for _ in 0..type_ref.array_dims {
            self.output.push_str("[]");
        }
    }
    
    fn visit_type_param(&mut self, type_param: &TypeParam) {
        self.output.push_str(&type_param.name);
        
        if !type_param.bounds.is_empty() {
            self.output.push_str(" extends ");
            for (i, bound) in type_param.bounds.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(" & ");
                }
                self.visit_type_ref(bound);
            }
        }
    }
    
    fn visit_annotation(&mut self, annotation: &Annotation) {
        self.output.push('@');
        self.output.push_str(&annotation.name);
        
        if !annotation.arguments.is_empty() {
            self.output.push('(');
            for (i, arg) in annotation.arguments.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                match arg {
                    AnnotationArg::Value(expr) => self.visit_expr(expr),
                    AnnotationArg::Named(name, expr) => {
                        self.output.push_str(name);
                        self.output.push_str(" = ");
                        self.visit_expr(expr);
                    }
                }
            }
            self.output.push(')');
        }
    }
}
