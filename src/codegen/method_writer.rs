//! Method writer for generating Java bytecode
//! 
//! This module handles the conversion of AST method declarations into Java bytecode instructions.

use super::bytecode::*;
use super::opcodes;
use super::opcodor::OpcodeGenerator;    
use crate::ast::*;
use crate::codegen::attribute::ExceptionTableEntry;
use crate::error::{Result, Error};

/// Method writer for generating Java bytecode
pub struct MethodWriter {
    bytecode: Vec<u8>,
    stack_state: StackState,
    labels: Vec<Label>,
    next_label_id: u16,
    loop_stack: Vec<LoopContext>,
    scope_stack: Vec<Scope>,
    pending_exception_entries: Vec<PendingExceptionEntry>,
    line_numbers: Vec<(u16, u16)>,
    constant_pool: Option<std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>>,
    opcode_generator: OpcodeGenerator,
    current_class_name: Option<String>,
}

impl MethodWriter {
    /// Create a new method writer
    pub fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            stack_state: StackState::new(),
            labels: Vec::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
            constant_pool: None,
            opcode_generator: OpcodeGenerator::new(),
            current_class_name: None,
        }
    }
    
    /// Create a new method writer with access to constant pool
    pub fn new_with_constant_pool(constant_pool: std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>) -> Self {
        Self {
            bytecode: Vec::new(),
            stack_state: StackState::new(),
            labels: Vec::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
            constant_pool: Some(constant_pool.clone()),
            opcode_generator: OpcodeGenerator::new_with_constant_pool(constant_pool),
            current_class_name: None,
        }
    }
    
    /// Create a new method writer with access to constant pool and current class name
    pub fn new_with_constant_pool_and_class(constant_pool: std::rc::Rc<std::cell::RefCell<super::constpool::ConstantPool>>, class_name: String) -> Self {
        Self {
            bytecode: Vec::new(),
            stack_state: StackState::new(),
            labels: Vec::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
            constant_pool: Some(constant_pool.clone()),
            opcode_generator: OpcodeGenerator::new_with_constant_pool(constant_pool),
            current_class_name: Some(class_name),
        }
    }
    
    /// Generate bytecode for a method body
    pub fn generate_method_body(&mut self, method: &MethodDecl) -> Result<()> {
        // Initialize local variables for parameters
        self.initialize_parameters(method)?;
        
        // Generate method body
        if let Some(body) = &method.body {
            self.generate_block(body)?;
        }
        
        // Generate return statement
        if let Some(return_type) = &method.return_type {
            self.generate_return(return_type)?;
        } else {
            // Create a void type reference for void methods
            let void_type = TypeRef { name: "void".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: method.span };
            self.generate_return(&void_type)?;
        }
        
        Ok(())
    }
    
    /// Initialize local variables for method parameters
    fn initialize_parameters(&mut self, method: &MethodDecl) -> Result<()> {
        // 'this' reference is always at index 0 for instance methods
        if !method.modifiers.contains(&Modifier::Static) {
            let this_type = LocalType::Reference(self.current_class_name.clone().unwrap_or_default());
            self.stack_state.frame.allocate("this".to_string(), this_type);
        }
        
        // Add parameters
        for (i, param) in method.parameters.iter().enumerate() {
            // Note: index calculation is kept for future use in local variable management
            let _index = if method.modifiers.contains(&Modifier::Static) {
                i
            } else {
                i + 1
            };
            
            let local_type = self.convert_type_ref_to_local_type(&param.type_ref);
            self.stack_state.frame.allocate(param.name.clone(), local_type);
        }
        
        Ok(())
    }
    
    /// Convert AST TypeRef to LocalType
    fn convert_type_ref_to_local_type(&self, type_ref: &TypeRef) -> LocalType {
        if type_ref.array_dims > 0 {
            let element_type = self.convert_type_ref_to_local_type(&TypeRef { name: type_ref.name.clone(), type_args: type_ref.type_args.clone(), annotations: Vec::new(), array_dims: 0, span: type_ref.span });
            LocalType::Array(Box::new(element_type))
        } else {
            match type_ref.name.as_str() {
                "int" | "boolean" | "byte" | "short" | "char" => LocalType::Int,
                "long" => LocalType::Long,
                "float" => LocalType::Float,
                "double" => LocalType::Double,
                "void" => LocalType::Int, // void is represented as int in some contexts
                _ => LocalType::Reference(type_ref.name.clone()),
            }
        }
    }
    
    /// Generate bytecode for a block
    fn generate_block(&mut self, block: &Block) -> Result<()> {
        // Enter new lexical scope
        self.scope_stack.push(Scope::default());
        for stmt in &block.statements {
            // Generate statement first, then record its source line at the next pc.
            // This avoids colliding with the method-declaration line at pc=0 (javac style).
            self.generate_statement(stmt)?;
            self.record_stmt_line(stmt);
        }
        // Exit scope: close locals (length=end-start)
        if let Some(scope) = self.scope_stack.pop() {
            let end_pc = self.bytecode.len() as u16;
            for idx in scope.locals { 
                self.stack_state.frame.update_lifetime(idx as u16, 0, end_pc);
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for a statement
    fn generate_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.generate_expression(&expr_stmt.expr)?;
                // Pop only if expression likely leaves a value on stack. Method calls to println return void.
                let should_pop = match &expr_stmt.expr {
                    Expr::MethodCall(mc) => mc.name != "println",
                    Expr::Assignment(_) => true,
                    Expr::Identifier(_) | Expr::Literal(_) | Expr::Binary(_) | Expr::Unary(_) | Expr::ArrayAccess(_) | Expr::FieldAccess(_) | Expr::Cast(_) | Expr::Conditional(_) | Expr::New(_) | Expr::Parenthesized(_) | Expr::InstanceOf(_) | Expr::ArrayInitializer(_) => true,
                };
                if should_pop { self.emit_opcode(self.opcode_generator.pop()); }
            }
            Stmt::Declaration(var_decl) => {
                self.generate_variable_declaration(var_decl)?;
            }
            Stmt::If(if_stmt) => {
                self.generate_if_statement(if_stmt)?;
            }
            Stmt::While(while_stmt) => {
                self.generate_while_statement_labeled(None, while_stmt)?;
            }
            Stmt::For(for_stmt) => {
                self.generate_for_statement(for_stmt)?;
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
                self.generate_return(&TypeRef { name: "void".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: return_stmt.span })?;
            }
            Stmt::Break(break_stmt) => {
                let target = if let Some(ref name) = break_stmt.label {
                    self.find_loop_break_label(Some(name))
                } else {
                    self.find_loop_break_label(None)
                };
                if let Some(label_id) = target {
                    self.emit_opcode(self.opcode_generator.goto(0)); // TODO: Calculate actual offset
                    self.emit_label_reference(label_id);
                } else {
                    // Fallback: placeholder
                    self.emit_opcode(self.opcode_generator.goto(0));
                    self.emit_short(0);
                }
            }
            Stmt::Continue(continue_stmt) => {
                let target = if let Some(ref name) = continue_stmt.label {
                    self.find_loop_continue_label(Some(name))
                } else {
                    self.find_loop_continue_label(None)
                };
                if let Some(label_id) = target {
                    self.emit_opcode(self.opcode_generator.goto(0)); // TODO: Calculate actual offset
                    self.emit_label_reference(label_id);
                } else {
                    // Fallback: placeholder
                    self.emit_opcode(self.opcode_generator.goto(0));
                    self.emit_short(0);
                }
            }
            Stmt::Try(try_stmt) => {
                // mark source line for try
                self.record_line_number(try_stmt.span.start.line as u16);
                // try-with-resources with exceptional path auto close and addSuppressed
                let mut res_locals: Vec<(u16, TypeRef)> = Vec::new();
                for (idx, res) in try_stmt.resources.iter().enumerate() {
                    match res {
                        TryResource::Var { type_ref, name, initializer, .. } => {
                            self.generate_expression(initializer)?;
                            let local_index = self.allocate_local_variable(name, type_ref);
                            let local_type = self.convert_type_ref_to_local_type(type_ref);
                            self.store_local_variable(local_index, &local_type)?;
                            res_locals.push((local_index, type_ref.clone()));
                        }
                        TryResource::Expr { expr, .. } => {
                            self.generate_expression(expr)?;
                            let tref = TypeRef { name: "java/lang/AutoCloseable".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: try_stmt.span };
                            let local_index = self.allocate_local_variable(&format!("$res{}", idx), &tref);
                            let local_type = self.convert_type_ref_to_local_type(&tref);
                            self.store_local_variable(local_index, &local_type)?;
                            res_locals.push((local_index, tref));
                        }
                    }
                }
                // Outer try/catch-all
                let try_start = self.create_label();
                let try_end = self.create_label();
                let handler = self.create_label();
                let after = self.create_label();
                self.mark_label(try_start);
                self.generate_block(&try_stmt.try_block)?;
                self.mark_label(try_end);
                // Normal close
                for (local_index, tref) in res_locals.iter().rev() {
                    self.generate_close_for_local(*local_index, tref)?;
                }
                // jump over handler
                self.emit_instruction(opcodes::GOTO);
                self.emit_label_reference(after);
                // Handler
                self.mark_label(handler);
                let thr_t = TypeRef { name: "java/lang/Throwable".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: try_stmt.span };
                let primary_exc = self.allocate_local_variable("$primary_exc", &thr_t);
                let thr_local_type = self.convert_type_ref_to_local_type(&thr_t);
                self.store_local_variable(primary_exc, &thr_local_type)?;
                // Close with addSuppressed
                for (local_index, _tref) in res_locals.iter().rev() {
                    let skip = self.create_label();
                    self.emit_opcode(self.opcode_generator.aload(0)); self.emit_byte(*local_index as u8);
                    self.emit_opcode(self.opcode_generator.ifnull(0)); self.emit_label_reference(skip);
                    let inner_start = self.create_label();
                    let inner_end = self.create_label();
                    let inner_handler = self.create_label();
                    let inner_after = self.create_label();
                    self.mark_label(inner_start);
                    self.emit_opcode(self.opcode_generator.aload(0)); self.emit_byte(*local_index as u8);
                    self.emit_opcode(self.opcode_generator.invokeinterface(0, 0));
                    self.emit_short(1); self.emit_byte(1); self.emit_byte(0);
                    self.mark_label(inner_end);
                    self.emit_instruction(opcodes::GOTO); self.emit_label_reference(inner_after);
                    self.mark_label(inner_handler);
                    let suppressed = self.allocate_local_variable("$suppressed", &thr_t);
                    self.store_local_variable(suppressed, &thr_local_type)?;
                    self.emit_opcode(self.opcode_generator.aload(0)); self.emit_byte(primary_exc as u8);
                    self.emit_opcode(self.opcode_generator.aload(0)); self.emit_byte(suppressed as u8);
                    self.emit_opcode(self.opcode_generator.invokevirtual(0));
                    self.emit_short(1);
                    self.mark_label(inner_after);
                    self.add_exception_handler_labels(inner_start, inner_end, inner_handler, 0);
                    self.mark_label(skip);
                }
                // rethrow
                self.emit_opcode(self.opcode_generator.aload(0)); self.emit_byte(primary_exc as u8);
                self.emit_opcode(self.opcode_generator.athrow());
                // add outer entry
                self.add_exception_handler_labels(try_start, try_end, handler, 0);
                // after
                self.mark_label(after);
                if let Some(finally_block) = &try_stmt.finally_block { self.generate_block(finally_block)?; }
                // close lifetimes of resource locals at end of try-with-resources
                let end_pc = self.bytecode.len() as u16;
                for (local_index, _) in &res_locals {
                    self.set_local_length((*local_index) as usize, end_pc);
                }
            }
            Stmt::Throw(throw_stmt) => {
                self.record_line_number(throw_stmt.span.start.line as u16);
                self.generate_expression(&throw_stmt.expr)?;
                self.emit_opcode(self.opcode_generator.athrow());
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
                self.emit_opcode(self.opcode_generator.ifne(0));
                self.emit_label_reference(end_label);
                // Construct AssertionError
                // NEW java/lang/AssertionError
                self.emit_opcode(self.opcode_generator.new_object(0));
                let _cls = self.add_class_constant("java/lang/AssertionError");
                self.emit_short(_cls as i16);
                // DUP
                self.emit_opcode(self.opcode_generator.dup());
                // If message present, load it and call (Ljava/lang/Object;)V or (Ljava/lang/String;)V
                if let Some(msg) = &assert_stmt.message {
                    self.generate_expression(msg)?;
                    // INVOKESPECIAL <init>(Ljava/lang/Object;)V (placeholder)
                    self.emit_opcode(self.opcode_generator.invokespecial(0));
                    let _mref = self.add_method_ref("java/lang/AssertionError", "<init>", "(Ljava/lang/Object;)V");
                    self.emit_short(_mref as i16);
                } else {
                    // INVOKESPECIAL <init>()V
                    self.emit_opcode(self.opcode_generator.invokespecial(0));
                    let _mref = self.add_method_ref("java/lang/AssertionError", "<init>", "()V");
                    self.emit_short(_mref as i16);
                }
                // ATHROW
                self.emit_opcode(self.opcode_generator.athrow());
                // end
                self.mark_label(end_label);
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
            Expr::ArrayInitializer(_values) => {
                // Only used in annotations; no code emission
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for a binary expression
    fn generate_binary_expression(&mut self, binary: &BinaryExpr) -> Result<()> {
        // Generate left operand
        self.generate_expression(&binary.left)?;
        
        // Generate right operand
        self.generate_expression(&binary.right)?;
        
        // Generate operation
        match binary.operator {
            BinaryOp::Add => self.emit_opcode(self.opcode_generator.iadd()),
            BinaryOp::Sub => self.emit_opcode(self.opcode_generator.isub()),
            BinaryOp::Mul => self.emit_opcode(self.opcode_generator.imul()),
            BinaryOp::Div => self.emit_opcode(self.opcode_generator.idiv()),
            BinaryOp::Mod => self.emit_opcode(self.opcode_generator.irem()),
            BinaryOp::Lt => {
                // For comparison operators, we need to handle them differently
                // since they don't leave a result on the stack
                self.emit_opcode(self.opcode_generator.if_icmplt(0));
                self.emit_short(0); // Placeholder offset
            }
            BinaryOp::Le => {
                self.emit_opcode(self.opcode_generator.if_icmple(0));
                self.emit_short(0); // Placeholder offset
            }
            BinaryOp::Gt => {
                self.emit_opcode(self.opcode_generator.if_icmpgt(0));
                self.emit_short(0); // Placeholder offset
            }
            BinaryOp::Ge => {
                self.emit_opcode(self.opcode_generator.if_icmpge(0));
                self.emit_short(0); // Placeholder offset
            }
            BinaryOp::Eq => {
                self.emit_opcode(self.opcode_generator.if_icmpeq(0));
                self.emit_short(0); // Placeholder offset
            }
            BinaryOp::Ne => {
                self.emit_opcode(self.opcode_generator.if_icmpne(0));
                self.emit_short(0); // Placeholder offset
            }
            BinaryOp::And => self.emit_opcode(self.opcode_generator.iand()),
            BinaryOp::Or => self.emit_opcode(self.opcode_generator.ior()),
            BinaryOp::Xor => self.emit_opcode(self.opcode_generator.ixor()),
            BinaryOp::LShift => self.emit_opcode(self.opcode_generator.ishl()),
            BinaryOp::RShift => self.emit_opcode(self.opcode_generator.ishr()),
            BinaryOp::URShift => self.emit_opcode(self.opcode_generator.iushr()),
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a unary expression
    fn generate_unary_expression(&mut self, unary: &UnaryExpr) -> Result<()> {
        match unary.operator {
            UnaryOp::Plus => {
                // No-op for unary plus
                self.generate_expression(&unary.operand)?;
            }
            UnaryOp::Minus => {
                self.generate_expression(&unary.operand)?;
                self.emit_opcode(self.opcode_generator.ineg());
            }
            UnaryOp::Not => {
                self.generate_expression(&unary.operand)?;
                // Logical NOT: convert to boolean and negate
                // First, convert to boolean (0 = false, non-zero = true)
                self.emit_opcode(self.opcode_generator.ifne(0));
                self.emit_short(0); // Placeholder offset
                // If not zero, push false (0)
                self.emit_opcode(self.opcode_generator.iconst_0());
                // Jump to end
                self.emit_opcode(self.opcode_generator.goto(0));
                self.emit_short(0); // Placeholder offset
                // Otherwise, push true (1)
                self.emit_opcode(self.opcode_generator.iconst_1());
            }
            UnaryOp::BitNot => {
                self.generate_expression(&unary.operand)?;
                self.emit_opcode(self.opcode_generator.iconst_m1());
                self.emit_opcode(self.opcode_generator.ixor());
            }
                        UnaryOp::PreInc => {
                // Pre-increment: ++x
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Load current value
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                        // Increment
                        self.emit_opcode(self.opcode_generator.iconst_1());
                        self.emit_opcode(self.opcode_generator.iadd());
                        // Store back
                        self.store_local_variable(local_var.index, &local_var.var_type)?;
                        // Load again for result
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                    }
                }
            }
            UnaryOp::PostInc => {
                // Post-increment: x++
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Load current value
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                        // DUP to keep copy for result
                        self.emit_opcode(self.opcode_generator.dup());
                        // Increment
                        self.emit_opcode(self.opcode_generator.iconst_1());
                        self.emit_opcode(self.opcode_generator.iadd());
                        // Store back
                        self.store_local_variable(local_var.index, &local_var.var_type)?;
                        // Original value is still on stack
                    }
                }
            }
            UnaryOp::PreDec => {
                // Pre-decrement: --x
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Load current value
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                        // Decrement
                        self.emit_opcode(self.opcode_generator.iconst_1());
                        self.emit_opcode(self.opcode_generator.isub());
                        // Store back
                        self.store_local_variable(local_var.index, &local_var.var_type)?;
                        // Load again for result
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                    }
                }
            }
            UnaryOp::PostDec => {
                // Post-decrement: x--
                if let Expr::Identifier(ident) = &*unary.operand {
                    let local_var = self.find_local_variable(&ident.name).cloned();
                    if let Some(local_var) = local_var {
                        // Load current value
                        self.load_local_variable(local_var.index, &local_var.var_type)?;
                        // DUP to keep copy for result
                        self.emit_opcode(self.opcode_generator.dup());
                        // Decrement
                        self.emit_opcode(self.opcode_generator.iconst_1());
                        self.emit_opcode(self.opcode_generator.isub());
                        // Store back
                        self.store_local_variable(local_var.index, &local_var.var_type)?;
                        // Original value is still on stack
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a literal expression
    fn generate_literal_expression(&mut self, literal: &LiteralExpr) -> Result<()> {
        self.generate_literal(&literal.value)
    }
    
    /// Generate bytecode for a literal
    fn generate_literal(&mut self, lit: &Literal) -> Result<()> {
        match lit {
            Literal::Integer(value) => {
                match *value {
                    0 => self.emit_opcode(self.opcode_generator.iconst_0()),
                    1 => self.emit_opcode(self.opcode_generator.iconst_1()),
                    2 => self.emit_opcode(self.opcode_generator.iconst_2()),
                    3 => self.emit_opcode(self.opcode_generator.iconst_3()),
                    4 => self.emit_opcode(self.opcode_generator.iconst_4()),
                    5 => self.emit_opcode(self.opcode_generator.iconst_5()),
                    -1 => self.emit_opcode(self.opcode_generator.iconst_m1()),
                    _ => {
                        if *value >= -128 && *value <= 127 {
                            self.emit_opcode(self.opcode_generator.bipush(*value as i8));
                        } else if *value >= -32768 && *value <= 32767 {
                            self.emit_opcode(self.opcode_generator.sipush(*value as i16));
                        } else {
                            // For larger values, we need to use LDC
                            // This is a simplified approach
                            self.emit_opcode(self.opcode_generator.ldc(1)); // Constant pool index
                        }
                    }
                }
            }
            Literal::Float(value) => {
                if *value == 0.0 {
                    self.emit_opcode(self.opcode_generator.fconst_0());
                } else if *value == 1.0 {
                    self.emit_opcode(self.opcode_generator.fconst_1());
                } else if *value == 2.0 {
                    self.emit_opcode(self.opcode_generator.fconst_2());
                } else {
                    // For other values, we need to use LDC
                    self.emit_opcode(self.opcode_generator.ldc(1)); // Constant pool index
                }
            }
            Literal::Boolean(value) => {
                if *value {
                    self.emit_opcode(self.opcode_generator.iconst_1());
                } else {
                    self.emit_opcode(self.opcode_generator.iconst_0());
                }
            }
            Literal::String(value) => {
                // Add string to constant pool and emit LDC
                if let Some(cp) = &self.constant_pool {
                    let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.add_string(value) };
                    self.emit_opcode(self.opcode_generator.ldc(idx));
                } else {
                    self.emit_opcode(self.opcode_generator.ldc(1));
                }
            }
            Literal::Char(value) => {
                let int_value = *value as i32;
                if int_value >= 0 && int_value <= 5 {
                    match int_value {
                        0 => self.emit_opcode(self.opcode_generator.iconst_0()),
                        1 => self.emit_opcode(self.opcode_generator.iconst_1()),
                        2 => self.emit_opcode(self.opcode_generator.iconst_2()),
                        3 => self.emit_opcode(self.opcode_generator.iconst_3()),
                        4 => self.emit_opcode(self.opcode_generator.iconst_4()),
                        5 => self.emit_opcode(self.opcode_generator.iconst_5()),
                        _ => unreachable!(),
                    }
                } else {
                    self.emit_opcode(self.opcode_generator.bipush(int_value as i8));
                }
            }
            Literal::Null => {
                self.emit_opcode(self.opcode_generator.aconst_null());
            }
        }
        
        Ok(())
    }
    
    /// Generate bytecode for an identifier expression
    fn generate_identifier(&mut self, ident: &str) -> Result<()> {
        // Look up local variable
        if let Some(local_var) = self.find_local_variable(ident) {
            let var_type = local_var.var_type.clone();
            self.load_local_variable(local_var.index, &var_type)?;
        } else {
            // Assume it's a field access on 'this'
            self.emit_opcode(self.opcode_generator.aload(0));
            // TODO: Add field reference to constant pool
            self.emit_opcode(self.opcode_generator.getfield(1)); // Constant pool index
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a method call
    fn generate_method_call(&mut self, call: &MethodCallExpr) -> Result<()> {
        // Handle System.out.println specially
        if call.name == "println" {
            let is_system_out = match &call.target {
                Some(t) => match &**t {
                    Expr::FieldAccess(fa) => matches!(fa.target.as_deref(), Some(Expr::Identifier(id)) if id.name == "System") && fa.name == "out",
                    _ => false,
                },
                None => false,
            };
            if is_system_out {
                // Align CP ordering with javac: Fieldref(System.out), then String literal, then Methodref(println)
                let field_ref = if let Some(cp) = &self.constant_pool { let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_field_ref("java/lang/System", "out", "Ljava/io/PrintStream;").unwrap() }; idx } else { 1 };
                self.emit_opcode(self.opcode_generator.getstatic(field_ref));
                for arg in &call.arguments { self.generate_expression(arg)?; }
                let mref = if let Some(cp) = &self.constant_pool { let idx = { let mut cp_ref = cp.borrow_mut(); cp_ref.try_add_method_ref("java/io/PrintStream", "println", "(Ljava/lang/String;)V").unwrap() }; idx } else { 1 };
                self.emit_opcode(self.opcode_generator.invokevirtual(mref));
                return Ok(());
            }
        }

        // General receiver + args
        if let Some(receiver) = &call.target { self.generate_expression(receiver)?; } else { self.emit_opcode(self.opcode_generator.aload(0)); }
        for arg in &call.arguments { self.generate_expression(arg)?; }

        // Generate method descriptor based on arguments
        let descriptor = self.generate_method_descriptor(&call.arguments);
        
        // Determine the class for the method call
        let class_name = if call.target.is_some() {
            // If there's a target, we need to determine the class from the target
            // For now, assume it's a method call on the target object
            // This is a simplified approach - in a full implementation, we'd need type inference
            "java/lang/Object".to_string()
        } else {
            // No target means calling on 'this' - use current class name
            self.current_class_name.as_ref()
                .ok_or_else(|| Error::codegen_error("Cannot resolve method call: no current class name available"))?
                .clone()
        };
        
        // Special handling for Comparable.compareTo method
        if call.name == "compareTo" {
            // This is likely a Comparable interface method call
            let method_ref_index = self.add_method_ref("java/lang/Comparable", &call.name, "(Ljava/lang/Object;)I");
            self.emit_opcode(self.opcode_generator.invokeinterface(method_ref_index, 2));
        } else {
            let method_ref_index = self.add_method_ref(&class_name, &call.name, &descriptor);
            self.emit_opcode(self.opcode_generator.invokevirtual(method_ref_index));
        }
        
        Ok(())
    }

    /// Generate method descriptor from arguments
    fn generate_method_descriptor(&self, args: &[Expr]) -> String {
        let mut descriptor = "(".to_string();
        
        for arg in args {
            descriptor.push_str(&self.type_to_descriptor(arg));
        }
        
        descriptor.push_str(")V"); // Assume void return for now
        descriptor
    }

    /// Convert expression type to JVM descriptor
    fn type_to_descriptor(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(lit) => match lit.value {
                Literal::Integer(_) => "I".to_string(),
                Literal::Float(_) => "F".to_string(),
                Literal::Boolean(_) => "Z".to_string(),
                Literal::String(_) => "Ljava/lang/String;".to_string(),
                Literal::Char(_) => "C".to_string(),
                Literal::Null => "Ljava/lang/Object;".to_string(),
            },
            Expr::Identifier(_) => "I".to_string(), // Assume int for now
            _ => "I".to_string(), // Default to int
        }
    }

    /// Generate bytecode for field access
    fn generate_field_access(&mut self, field_access: &FieldAccessExpr) -> Result<()> {
        // Generate receiver expression if present
        if let Some(receiver) = &field_access.target {
            self.generate_expression(receiver)?;
        } else {
            // Assume 'this' for instance fields
            self.emit_opcode(self.opcode_generator.aload(0));
        }
        
        // Generate field access
        // TODO: Get proper field type and class name from context
        let field_class = self.current_class_name.clone().unwrap_or_else(|| "java/lang/Object".to_string());
        let field_descriptor = "I"; // TODO: Resolve actual field type
        let field_ref_index = self.add_field_ref(&field_class, &field_access.name, field_descriptor);
        self.emit_opcode(self.opcode_generator.getfield(0));
        self.emit_short(field_ref_index as i16);
        
        Ok(())
    }

    /// Generate bytecode for instanceof expression
    fn generate_instanceof_expression(&mut self, instance_of: &InstanceOfExpr) -> Result<()> {
        // Generate expression to check
        self.generate_expression(&instance_of.expr)?;
        
        // Generate instanceof check
        let class_ref_index = self.add_class_constant(&instance_of.target_type.name);
        self.emit_opcode(self.opcode_generator.instanceof(0));
        self.emit_short(class_ref_index as i16);
        
        Ok(())
    }

    /// Generate bytecode for new expression
    fn generate_new_expression(&mut self, new_expr: &NewExpr) -> Result<()> {
        // Check if it's an array creation
        if new_expr.target_type.array_dims > 0 {
            self.generate_array_creation(new_expr)?;
        } else {
            // Regular object creation
            let class_ref_index = self.add_class_constant(&new_expr.target_type.name);
            
            // NEW instruction
            self.emit_opcode(self.opcode_generator.new_object(0));
            self.emit_short(class_ref_index as i16);
            
            // DUP to keep reference for constructor call
            self.emit_opcode(self.opcode_generator.dup());
            
            // Generate constructor arguments
            for arg in &new_expr.arguments {
                self.generate_expression(arg)?;
            }
            
            // Call constructor
            let method_ref_index = self.add_method_ref(&new_expr.target_type.name, "<init>", "()V");
            self.emit_opcode(self.opcode_generator.invokespecial(0));
            self.emit_short(method_ref_index as i16);
        }
        
        Ok(())
    }

    fn generate_close_for_local(&mut self, index: u16, _tref: &TypeRef) -> Result<()> {
        // Load local
        self.emit_opcode(self.opcode_generator.aload(0));
        self.emit_byte(index as u8);
        // ifnull skip
        let end_label = self.create_label();
        self.emit_opcode(self.opcode_generator.ifnull(0));
        self.emit_label_reference(end_label);
        // invoke interface close()V (simplified; no constant pool wired)
        self.emit_opcode(self.opcode_generator.aload(0));
        self.emit_byte(index as u8);
        self.emit_opcode(self.opcode_generator.invokeinterface(0, 0));
        self.emit_short(1);
        self.emit_byte(1);
        self.emit_byte(0);
        // end label
        self.mark_label(end_label);
        Ok(())
    }
    
    /// Generate bytecode for an assignment expression
    fn generate_assignment(&mut self, assign: &AssignmentExpr) -> Result<()> {
        // Handle compound assignments
        if assign.operator != AssignmentOp::Assign {
            // For compound assignments, we need to load the target first
            if let Expr::Identifier(ident) = &*assign.target {
                if let Some(local_var) = self.find_local_variable(&ident.name) {
                    // Extract local variable info to avoid borrow checker issues
                    let index = local_var.index;
                    let var_type = local_var.var_type.clone();
                    
                    // Load current value
                    self.load_local_variable(index, &var_type)?;
                    // Generate right operand
                    self.generate_expression(&assign.value)?;
                    // Apply operation
                    self.generate_compound_assignment(assign.operator.clone())?;
                    // Store result
                    self.store_local_variable(index, &var_type)?;
                    return Ok(());
                }
            }
        }
        
        // Regular assignment: generate by target kind to preserve correct operand order
        match &*assign.target {
            Expr::Identifier(ident) => {
                // x = value → evaluate RHS then store to local (or this.field fallback)
                self.generate_expression(&assign.value)?;
                if let Some(local_var) = self.find_local_variable(&ident.name) {
                    let var_type = local_var.var_type.clone();
                    self.store_local_variable(local_var.index, &var_type)?;
                } else {
                    // Assume it's a field on 'this'
                    self.emit_opcode(self.opcode_generator.aload(0));
                    // objectref is under value; swap to [objectref, value]
                    self.emit_opcode(self.opcode_generator.swap());
                    // Placeholder CP index; descriptor/class are resolved later in full impl
                    self.emit_opcode(self.opcode_generator.putfield(1));
                }
            }
            Expr::FieldAccess(field_access) => {
                // target.field = value
                // Evaluate receiver first to get objectref
                if let Some(receiver) = &field_access.target {
                    self.generate_expression(receiver)?;
                } else {
                    // Implicit this
                    self.emit_opcode(self.opcode_generator.aload(0));
                }
                // Then evaluate RHS value
                self.generate_expression(&assign.value)?;
                // Stack: objectref, value → putfield
                // Use placeholder CP index
                self.emit_opcode(self.opcode_generator.putfield(1));
            }
            Expr::ArrayAccess(array_access) => {
                // arr[idx] = value
                // Evaluate array and index first
                self.generate_expression(&array_access.array)?;
                self.generate_expression(&array_access.index)?;
                // Then evaluate RHS value
                self.generate_expression(&assign.value)?;
                // Choose store opcode; use int-array store as a reasonable default
                self.emit_opcode(self.opcode_generator.iastore());
            }
            other => {
                return Err(Error::codegen_error(format!("Unsupported assignment target: {:?}", other)));
            }
        }
        
        Ok(())
    }

    /// Generate bytecode for compound assignment operations
    fn generate_compound_assignment(&mut self, op: AssignmentOp) -> Result<()> {
        match op {
            AssignmentOp::AddAssign => self.emit_opcode(self.opcode_generator.iadd()),
            AssignmentOp::SubAssign => self.emit_opcode(self.opcode_generator.isub()),
            AssignmentOp::MulAssign => self.emit_opcode(self.opcode_generator.imul()),
            AssignmentOp::DivAssign => self.emit_opcode(self.opcode_generator.idiv()),
            AssignmentOp::ModAssign => self.emit_opcode(self.opcode_generator.irem()),
            AssignmentOp::AndAssign => self.emit_opcode(self.opcode_generator.iand()),
            AssignmentOp::OrAssign => self.emit_opcode(self.opcode_generator.ior()),
            AssignmentOp::XorAssign => self.emit_opcode(self.opcode_generator.ixor()),
            AssignmentOp::LShiftAssign => self.emit_opcode(self.opcode_generator.ishl()),
            AssignmentOp::RShiftAssign => self.emit_opcode(self.opcode_generator.ishr()),
            AssignmentOp::URShiftAssign => self.emit_opcode(self.opcode_generator.iushr()),
            AssignmentOp::Assign => {
                // Should not happen here
                return Err(Error::codegen_error("Unexpected Assign operator in compound assignment".to_string()));
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for an array access expression
    fn generate_array_access(&mut self, array_access: &ArrayAccessExpr) -> Result<()> {
        // Generate array expression
        self.generate_expression(&array_access.array)?;
        
        // Generate index expression
        self.generate_expression(&array_access.index)?;
        
        // Generate array access
        self.emit_opcode(self.opcode_generator.iaload()); // Assume int array for now
        
        Ok(())
    }
    
    /// Generate bytecode for an array creation expression
    fn generate_array_creation(&mut self, array_creation: &NewExpr) -> Result<()> {
        // Generate arguments
        for arg in &array_creation.arguments {
            self.generate_expression(arg)?;
        }
        
        // Generate new array
        // TODO: Handle different array types
        self.emit_opcode(self.opcode_generator.newarray(0));
        self.emit_byte(10); // T_INT
        
        Ok(())
    }

    fn generate_switch_statement(&mut self, switch_stmt: &SwitchStmt) -> Result<()> {
        // Evaluate switch expression (assume int)
        self.generate_expression(&switch_stmt.expression)?;
        // For each case label, duplicate value, compare, and jump
        let end_label = self.create_label();
        let mut case_labels: Vec<(u16, usize)> = Vec::new(); // (label_id, case_index)
        for (idx, case) in switch_stmt.cases.iter().enumerate() {
            if case.labels.is_empty() { continue; }
            for label_expr in &case.labels {
                // duplicate switch value
                self.emit_opcode(self.opcode_generator.dup());
                self.generate_expression(label_expr)?;
                self.emit_opcode(self.opcode_generator.if_icmpeq(0));
                let target = self.create_label();
                self.emit_label_reference(target);
                case_labels.push((target, idx));
            }
        }
        // No match: drop value and jump to default (if any) else end
        self.emit_opcode(self.opcode_generator.pop());
        let mut default_label = None;
        for (idx, case) in switch_stmt.cases.iter().enumerate() {
            if case.labels.is_empty() {
                let dl = self.create_label();
                default_label = Some((dl, idx));
                self.emit_instruction(opcodes::GOTO);
                self.emit_label_reference(dl);
                break;
            }
        }
        if default_label.is_none() {
            self.emit_instruction(opcodes::GOTO);
            self.emit_label_reference(end_label);
        }
        // Emit case bodies
        let mut case_end_labels: Vec<u16> = Vec::new();
        for (idx, case) in switch_stmt.cases.iter().enumerate() {
            // mark labels that jump here
            for (lbl, _i) in case_labels.iter().filter(|(_, i)| *i == idx) { self.mark_label(*lbl); }
            if let Some((dl, i)) = default_label { if i == idx { self.mark_label(dl); } }
            // emit statements
            for stmt in &case.statements {
                self.generate_statement(stmt)?;
            }
            // if case does not end with break (we cannot know), fallthrough into next
            // insert explicit goto end to simplify
            let after_case = self.create_label();
            self.emit_instruction(opcodes::GOTO);
            self.emit_label_reference(after_case);
            case_end_labels.push(after_case);
        }
        // mark end
        self.mark_label(end_label);
        for l in case_end_labels { self.mark_label(l); }
        Ok(())
    }
    
    /// Generate bytecode for a cast expression
    fn generate_cast(&mut self, cast: &CastExpr) -> Result<()> {
        // Generate expression to cast
        self.generate_expression(&cast.expr)?;
        
        // Generate cast bytecode based on target type
        match cast.target_type.name.as_str() {
            "int" | "boolean" | "byte" | "short" | "char" => {
                // No cast needed for int types, they're all compatible
            }
            "long" => {
                // Convert int to long
                self.emit_opcode(self.opcode_generator.i2l());
            }
            "float" => {
                // Convert int to float
                self.emit_opcode(self.opcode_generator.i2f());
            }
            "double" => {
                // Convert int to double
                self.emit_opcode(self.opcode_generator.i2d());
            }
            _ => {
                // Reference type cast - checkcast instruction
                let class_ref_index = self.add_class_constant(&cast.target_type.name);
                self.emit_opcode(self.opcode_generator.checkcast(class_ref_index));
            }
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a ternary expression
    fn generate_ternary_expression(&mut self, ternary: &ConditionalExpr) -> Result<()> {
        // Generate condition
        self.generate_expression(&ternary.condition)?;
        
        // Create labels for then and else branches
        let else_label = self.create_label();
        let end_label = self.create_label();
        
        // Jump to else if condition is false
        self.emit_opcode(self.opcode_generator.ifeq(0));
        self.emit_label_reference(else_label);
        
        // Generate then expression
        self.generate_expression(&ternary.then_expr)?;
        
        // Jump to end
        self.emit_instruction(opcodes::GOTO);
        self.emit_label_reference(end_label);
        
        // Mark else label
        self.mark_label(else_label);
        
        // Generate else expression
        self.generate_expression(&ternary.else_expr)?;
        
        // Mark end label
        self.mark_label(end_label);
        
        Ok(())
    }
    
    /// Generate bytecode for an if statement
    fn generate_if_statement(&mut self, if_stmt: &IfStmt) -> Result<()> {
        // Generate condition
        self.generate_expression(&if_stmt.condition)?;
        
        // Create labels
        let else_label = self.create_label();
        let end_label = self.create_label();
        
        // Jump to else if condition is false
        self.emit_opcode(self.opcode_generator.ifeq(0));
        self.emit_label_reference(else_label);
        
        // Generate then branch
        self.generate_statement(&if_stmt.then_branch)?;
        
        // Jump to end
        self.emit_instruction(opcodes::GOTO);
        self.emit_label_reference(end_label);
        
        // Mark else label
        self.mark_label(else_label);
        
        // Generate else branch if present
        if let Some(else_branch) = &if_stmt.else_branch {
            self.generate_statement(else_branch)?;
        }
        
        // Mark end label
        self.mark_label(end_label);
        
        Ok(())
    }
    
    /// Generate bytecode for a while statement
    fn generate_while_statement_labeled(&mut self, label: Option<&str>, while_stmt: &WhileStmt) -> Result<()> {
        // Create labels
        let start_label = self.create_label();
        let end_label = self.create_label();
        // Push loop context
        self.loop_stack.push(LoopContext { label: label.map(|s| s.to_string()), continue_label: start_label, break_label: end_label });
        
        // Mark start label
        self.mark_label(start_label);
        
        // Generate condition
        self.generate_expression(&while_stmt.condition)?;
        
        // Jump to end if condition is false
        self.emit_opcode(self.opcode_generator.ifeq(0));
        self.emit_label_reference(end_label);
        
        // Generate body
        self.generate_statement(&while_stmt.body)?;
        
        // Jump back to start
        self.emit_instruction(opcodes::GOTO);
        self.emit_label_reference(start_label);
        
        // Mark end label
        self.mark_label(end_label);
        // Pop loop context
        self.loop_stack.pop();
        
        Ok(())
    }
    
    /// Generate bytecode for a for statement
    fn generate_for_statement(&mut self, for_stmt: &ForStmt) -> Result<()> {
        // Init
        for init in &for_stmt.init {
            self.generate_statement(init)?;
        }
        // Labels
        let start_label = self.create_label();
        let end_label = self.create_label();
        let continue_label = self.create_label();
        self.loop_stack.push(LoopContext { label: None, continue_label, break_label: end_label });
        // Condition
        self.mark_label(start_label);
        if let Some(cond) = &for_stmt.condition {
            self.generate_expression(cond)?;
            self.emit_opcode(self.opcode_generator.ifeq(0));
            self.emit_label_reference(end_label);
        }
        // Body
        self.generate_statement(&for_stmt.body)?;
        // Continue label and updates
        self.mark_label(continue_label);
        for upd in &for_stmt.update {
            self.generate_expression(&upd.expr)?;
            self.emit_opcode(self.opcode_generator.pop());
        }
        // Loop back
        self.emit_instruction(opcodes::GOTO);
        self.emit_label_reference(start_label);
        // End
        self.mark_label(end_label);
        self.loop_stack.pop();
        Ok(())
    }
    
    /// Generate bytecode for a variable declaration
    fn generate_variable_declaration(&mut self, var_decl: &VarDeclStmt) -> Result<()> {
        for variable in &var_decl.variables {
            // Allocate local variable
            let index = self.allocate_local_variable(&variable.name, &var_decl.type_ref);
            
            // Generate initializer if present
            if let Some(initializer) = &variable.initializer {
                self.generate_expression(initializer)?;
                let local_type = self.convert_type_ref_to_local_type(&var_decl.type_ref);
                self.store_local_variable(index, &local_type)?;
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for a return statement
    fn generate_return(&mut self, return_type: &TypeRef) -> Result<()> {
        // Check if it's a void return type
        if return_type.name == "void" {
            self.emit_opcode(self.opcode_generator.return_void());
        } else {
            // For primitive types, use appropriate return instruction
            match return_type.name.as_str() {
                "int" | "boolean" | "byte" | "short" | "char" => {
                    self.emit_opcode(self.opcode_generator.ireturn());
                }
                "long" => {
                    self.emit_opcode(self.opcode_generator.lreturn());
                }
                "float" => {
                    self.emit_opcode(self.opcode_generator.freturn());
                }
                "double" => {
                    self.emit_opcode(self.opcode_generator.dreturn());
                }
                _ => {
                    // Reference type
                    self.emit_opcode(self.opcode_generator.areturn());
                }
            }
        }
        
        Ok(())
    }
    
    /// Load a local variable
    fn load_local_variable(&mut self, index: u16, var_type: &LocalType) -> Result<()> {
        match var_type {
            LocalType::Int => {
                self.emit_opcode(self.opcode_generator.iload(index));
            }
            LocalType::Long => {
                self.emit_opcode(self.opcode_generator.lload(index));
            }
            LocalType::Float => {
                self.emit_opcode(self.opcode_generator.fload(index));
            }
            LocalType::Double => {
                self.emit_opcode(self.opcode_generator.dload(index));
            }
            LocalType::Reference(_) | LocalType::Array(_) => {
                // Reference type
                self.emit_opcode(self.opcode_generator.aload(index));
            }
        }
        
        Ok(())
    }
    
    /// Store a local variable
    fn store_local_variable(&mut self, index: u16, var_type: &LocalType) -> Result<()> {
        match var_type {
            LocalType::Int => {
                match index {
                    0 => self.emit_opcode(self.opcode_generator.istore(0)),
                    1 => self.emit_instruction(opcodes::ISTORE_1),
                    2 => self.emit_instruction(opcodes::ISTORE_2),
                    3 => self.emit_instruction(opcodes::ISTORE_3),
                    _ => {
                        self.emit_instruction(opcodes::ISTORE);
                        self.emit_byte(index as u8);
                    }
                }
            }
            LocalType::Long => {
                match index {
                    0 => self.emit_instruction(opcodes::LSTORE_0),
                    1 => self.emit_instruction(opcodes::LSTORE_1),
                    2 => self.emit_instruction(opcodes::LSTORE_2),
                    3 => self.emit_instruction(opcodes::LSTORE_3),
                    _ => {
                        self.emit_instruction(opcodes::LSTORE);
                        self.emit_byte(index as u8);
                    }
                }
            }
            LocalType::Float => {
                match index {
                    0 => self.emit_opcode(self.opcode_generator.fstore(0)),
                    1 => self.emit_instruction(opcodes::FSTORE_1),
                    2 => self.emit_instruction(opcodes::FSTORE_2),
                    3 => self.emit_instruction(opcodes::FSTORE_3),
                    _ => {
                        self.emit_instruction(opcodes::FSTORE);
                        self.emit_byte(index as u8);
                    }
                }
            }
            LocalType::Double => {
                match index {
                    0 => self.emit_instruction(opcodes::DSTORE_0),
                    1 => self.emit_instruction(opcodes::DSTORE_1),
                    2 => self.emit_instruction(opcodes::DSTORE_2),
                    3 => self.emit_instruction(opcodes::DSTORE_3),
                    _ => {
                        self.emit_instruction(opcodes::DSTORE);
                        self.emit_byte(index as u8);
                    }
                }
            }
            LocalType::Reference(_) | LocalType::Array(_) => {
                // Reference type
                match index {
                    0 => self.emit_instruction(opcodes::ASTORE_0),
                    1 => self.emit_instruction(opcodes::ASTORE_1),
                    2 => self.emit_instruction(opcodes::ASTORE_2),
                    3 => self.emit_instruction(opcodes::ASTORE_3),
                    _ => {
                        self.emit_instruction(opcodes::ASTORE);
                        self.emit_byte(index as u8);
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Find a local variable by name
    fn find_local_variable(&self, name: &str) -> Option<&LocalSlot> {
        self.stack_state.frame.get_by_name(name)
    }
    
    /// Allocate a new local variable
    fn allocate_local_variable(&mut self, name: &str, var_type: &TypeRef) -> u16 {
        let index = self.stack_state.frame.allocate(name.to_string(), self.convert_type_ref_to_local_type(var_type));
        // Track in current scope if any
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.locals.push(index as usize);
        }
        index
    }
    
    /// Create a new label
    fn create_label(&mut self) -> u16 {
        let label_id = self.next_label_id;
        self.next_label_id += 1;
        self.labels.push(Label {
            id: label_id,
            position: 0,
            references: Vec::new(),
        });
        label_id
    }
    
    /// Mark a label at the current position
    fn mark_label(&mut self, label_id: u16) {
        if let Some(label) = self.labels.iter_mut().find(|l| l.id == label_id) {
            label.position = self.bytecode.len() as u16;
        }
    }
    
    /// Emit a label reference (placeholder for branch instructions)
    fn emit_label_reference(&mut self, label_id: u16) {
        if let Some(label) = self.labels.iter_mut().find(|l| l.id == label_id) {
            label.references.push(LabelReference {
                position: self.bytecode.len() as u16,
            });
        }
        // Emit placeholder bytes for the branch offset
        self.emit_short(0);
    }

    /// Record an exception handler table entry using labels
    fn add_exception_handler_labels(&mut self, start: u16, end: u16, handler: u16, catch_type: u16) {
        self.pending_exception_entries.push(PendingExceptionEntry { start_label: start, end_label: end, handler_label: handler, catch_type });
    }
    
    /// Emit an opcode and update stack state
    fn emit_opcode(&mut self, opcode: Vec<u8>) {
        self.bytecode.extend_from_slice(&opcode);
        
        // Update stack state based on the opcode
        if let Some(first_byte) = opcode.first() {
            match first_byte {
                // Load instructions - push values onto stack
                0x19 => { // aload - push reference
                    let _ = self.stack_state.push(1);
                }
                0x2A => { // aload_0 - push reference
                    let _ = self.stack_state.push(1);
                }
                0x2B => { // aload_1 - push reference
                    let _ = self.stack_state.push(1);
                }
                0x2C => { // aload_2 - push reference
                    let _ = self.stack_state.push(1);
                }
                0x2D => { // aload_3 - push reference
                    let _ = self.stack_state.push(1);
                }
                
                // Store instructions - pop values from stack
                0x3A => { // astore - pop reference
                    let _ = self.stack_state.pop(1);
                }
                0x4B => { // astore_0 - pop reference
                    let _ = self.stack_state.pop(1);
                }
                0x4C => { // astore_1 - pop reference
                    let _ = self.stack_state.pop(1);
                }
                0x4D => { // astore_2 - pop reference
                    let _ = self.stack_state.pop(1);
                }
                0x4E => { // astore_3 - pop reference
                    let _ = self.stack_state.pop(1);
                }
                
                // Method invocation - pop arguments, push return value
                0xB6 => { // invokevirtual
                    // For Comparable.compareTo, we have 2 arguments and return 1 value
                    let _ = self.stack_state.update(2, 1); // pop 2, push 1
                }
                0xB9 => { // invokeinterface
                    // For Comparable.compareTo, we have 2 arguments and return 1 value
                    let _ = self.stack_state.update(2, 1); // pop 2, push 1
                }
                
                // Type conversion - no stack change
                0xC0 => { // checkcast - no stack change
                    // checkcast doesn't change stack depth
                }
                
                // Return instructions - pop return value
                0xAC => { // ireturn
                    let _ = self.stack_state.pop(1);
                }
                0xB0 => { // return
                    // void return, no stack change
                }
                
                // Other instructions - no stack change for now
                _ => {}
            }
        }
    }
    
    /// Emit an instruction (for backward compatibility)
    fn emit_instruction(&mut self, opcode: u8) {
        self.bytecode.push(opcode);
        // For single-byte instructions, we don't have enough context to update stack state
        // This is a simplified version for backward compatibility
    }
    
    /// Emit a byte value
    fn emit_byte(&mut self, value: u8) {
        self.bytecode.push(value);
    }
    
    /// Emit a short value
    fn emit_short(&mut self, value: i16) {
        self.bytecode.extend_from_slice(&value.to_be_bytes());
    }
    
    /// Update stack and locals tracking
    fn update_stack_and_locals(&mut self) {
        // TODO: Implement proper stack and locals tracking
        // For now, just increment max_stack
        self.stack_state.max_stack = self.stack_state.max_stack.saturating_add(1);
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
                          name == "Deque" || name == "Queue" || name == "Iterable" {
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
    fn add_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        if let Some(cp) = &self.constant_pool {
            let mut cp_ref = cp.borrow_mut();
            match cp_ref.try_add_method_ref(class, name, descriptor) {
                Ok(idx) => idx,
                Err(_) => 1,
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
                Err(_) => 1,
            }
        } else {
            // Fallback for backward compatibility
            1
        }
    }
    
    /// Get the generated bytecode
    pub fn get_bytecode(self) -> Vec<u8> { self.bytecode }

    /// Finalize and return code, max stack, max locals, and resolved exception table
    pub(crate) fn finalize(self) -> (Vec<u8>, u16, u16, Vec<ExceptionTableEntry>, Vec<LocalSlot>, Vec<(u16,u16)>) {
        let mut exceptions: Vec<ExceptionTableEntry> = Vec::new();
        for pe in &self.pending_exception_entries {
            let start_pc = self.labels.iter().find(|l| l.id == pe.start_label).map(|l| l.position).unwrap_or(0);
            let end_pc = self.labels.iter().find(|l| l.id == pe.end_label).map(|l| l.position).unwrap_or(0);
            let handler_pc = self.labels.iter().find(|l| l.id == pe.handler_label).map(|l| l.position).unwrap_or(0);
            exceptions.push(ExceptionTableEntry::new(start_pc, end_pc, handler_pc, pe.catch_type));
        }
        (self.bytecode, self.stack_state.max_stack(), self.stack_state.max_locals(), exceptions, self.stack_state.frame.locals, self.line_numbers)
    }
    
    /// Get the maximum stack size
    pub fn get_max_stack(&self) -> u16 {
        self.stack_state.max_stack()
    }
    
    /// Get the maximum number of local variables
    pub fn get_max_locals(&self) -> u16 {
        self.stack_state.max_locals()
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
        let pc = self.bytecode.len() as u16;
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
}
