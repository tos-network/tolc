//! Method writer for generating Java bytecode
//! 
//! This module handles the conversion of AST method declarations into Java bytecode instructions.

use super::bytecode::*;
use crate::ast::*;
use crate::codegen::ExceptionTableEntry;
use crate::error::{Result, Error};

/// Method writer for generating Java bytecode
pub struct MethodWriter {
    bytecode: Vec<u8>,
    max_stack: u16,
    max_locals: u16,
    local_vars: Vec<LocalVariable>,
    labels: Vec<Label>,
    next_label_id: u16,
    loop_stack: Vec<LoopContext>,
    scope_stack: Vec<Scope>,
    pending_exception_entries: Vec<PendingExceptionEntry>,
    line_numbers: Vec<(u16, u16)>,
}

impl MethodWriter {
    /// Create a new method writer
    pub fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            max_stack: 0,
            max_locals: 0,
            local_vars: Vec::new(),
            labels: Vec::new(),
            next_label_id: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            pending_exception_entries: Vec::new(),
            line_numbers: Vec::new(),
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
            let void_type = TypeRef {
                name: "void".to_string(),
                type_args: Vec::new(),
                array_dims: 0,
                span: method.span,
            };
            self.generate_return(&void_type)?;
        }
        
        // Update max stack and locals
        self.update_stack_and_locals();
        
        Ok(())
    }
    
    /// Initialize local variables for method parameters
    fn initialize_parameters(&mut self, method: &MethodDecl) -> Result<()> {
        // 'this' reference is always at index 0 for instance methods
        if !method.modifiers.contains(&Modifier::Static) {
            let this_type = TypeRef {
                name: "".to_string(),
                type_args: Vec::new(),
                array_dims: 0,
                span: method.span,
            };
            self.local_vars.push(LocalVariable {
                name: "this".to_string(),
                var_type: this_type,
                index: 0,
                start_pc: 0,
                length: 0,
            });
            self.max_locals = 1;
        }
        
        // Add parameters
        for (i, param) in method.parameters.iter().enumerate() {
            let index = if method.modifiers.contains(&Modifier::Static) {
                i
            } else {
                i + 1
            };
            
            self.local_vars.push(LocalVariable {
                name: param.name.clone(),
                var_type: param.type_ref.clone(),
                index: index as u16,
                start_pc: 0,
                length: 0,
            });
            
            self.max_locals = self.max_locals.max((index + 1) as u16);
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a block
    fn generate_block(&mut self, block: &Block) -> Result<()> {
        // Enter new lexical scope
        self.scope_stack.push(Scope::default());
        for stmt in &block.statements {
            // record a line number entry at the start of each statement
            self.record_stmt_line(stmt);
            self.generate_statement(stmt)?;
        }
        // Exit scope: close locals (length=end-start)
        if let Some(scope) = self.scope_stack.pop() {
            let end_pc = self.bytecode.len() as u16;
            for idx in scope.locals { self.set_local_length(idx, end_pc); }
        }
        Ok(())
    }
    
    /// Generate bytecode for a statement
    fn generate_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.generate_expression(&expr_stmt.expr)?;
                // Pop result if not used
                self.emit_instruction(opcodes::POP);
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
                self.generate_return(&TypeRef {
                    name: "void".to_string(),
                    type_args: Vec::new(),
                    array_dims: 0,
                    span: return_stmt.span,
                })?;
            }
            Stmt::Break(break_stmt) => {
                let target = if let Some(ref name) = break_stmt.label {
                    self.find_loop_break_label(Some(name))
                } else {
                    self.find_loop_break_label(None)
                };
                if let Some(label_id) = target {
                    self.emit_instruction(opcodes::GOTO);
                    self.emit_label_reference(label_id);
                } else {
                    // Fallback: placeholder
                    self.emit_instruction(opcodes::GOTO);
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
                    self.emit_instruction(opcodes::GOTO);
                    self.emit_label_reference(label_id);
                } else {
                    // Fallback: placeholder
                    self.emit_instruction(opcodes::GOTO);
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
                            self.store_local_variable(local_index, type_ref)?;
                            res_locals.push((local_index, type_ref.clone()));
                        }
                        TryResource::Expr { expr, .. } => {
                            self.generate_expression(expr)?;
                            let tref = TypeRef { name: "java/lang/AutoCloseable".to_string(), type_args: Vec::new(), array_dims: 0, span: try_stmt.span };
                            let local_index = self.allocate_local_variable(&format!("$res{}", idx), &tref);
                            self.store_local_variable(local_index, &tref)?;
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
                let thr_t = TypeRef { name: "java/lang/Throwable".to_string(), type_args: Vec::new(), array_dims: 0, span: try_stmt.span };
                let primary_exc = self.allocate_local_variable("$primary_exc", &thr_t);
                self.store_local_variable(primary_exc, &thr_t)?;
                // Close with addSuppressed
                for (local_index, tref) in res_locals.iter().rev() {
                    let skip = self.create_label();
                    self.emit_instruction(opcodes::ALOAD); self.emit_byte(*local_index as u8);
                    self.emit_instruction(opcodes::IFNULL); self.emit_label_reference(skip);
                    let inner_start = self.create_label();
                    let inner_end = self.create_label();
                    let inner_handler = self.create_label();
                    let inner_after = self.create_label();
                    self.mark_label(inner_start);
                    self.emit_instruction(opcodes::ALOAD); self.emit_byte(*local_index as u8);
                    self.emit_instruction(opcodes::INVOKEINTERFACE);
                    self.emit_short(1); self.emit_byte(1); self.emit_byte(0);
                    self.mark_label(inner_end);
                    self.emit_instruction(opcodes::GOTO); self.emit_label_reference(inner_after);
                    self.mark_label(inner_handler);
                    let suppressed = self.allocate_local_variable("$suppressed", &thr_t);
                    self.store_local_variable(suppressed, &thr_t)?;
                    self.emit_instruction(opcodes::ALOAD); self.emit_byte(primary_exc as u8);
                    self.emit_instruction(opcodes::ALOAD); self.emit_byte(suppressed as u8);
                    self.emit_instruction(opcodes::INVOKEVIRTUAL);
                    self.emit_short(1);
                    self.mark_label(inner_after);
                    self.add_exception_handler_labels(inner_start, inner_end, inner_handler, 0);
                    self.mark_label(skip);
                }
                // rethrow
                self.emit_instruction(opcodes::ALOAD); self.emit_byte(primary_exc as u8);
                self.emit_instruction(opcodes::ATHROW);
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
                self.emit_instruction(opcodes::ATHROW);
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
                self.emit_instruction(opcodes::IFNE);
                self.emit_label_reference(end_label);
                // Construct AssertionError
                // NEW java/lang/AssertionError
                self.emit_instruction(opcodes::NEW);
                let _cls = self.add_class_constant("java/lang/AssertionError");
                self.emit_short(_cls as i16);
                // DUP
                self.emit_instruction(opcodes::DUP);
                // If message present, load it and call (Ljava/lang/Object;)V or (Ljava/lang/String;)V
                if let Some(msg) = &assert_stmt.message {
                    self.generate_expression(msg)?;
                    // INVOKESPECIAL <init>(Ljava/lang/Object;)V (placeholder)
                    self.emit_instruction(opcodes::INVOKESPECIAL);
                    let _mref = self.add_method_ref("java/lang/AssertionError", "<init>", "(Ljava/lang/Object;)V");
                    self.emit_short(_mref as i16);
                } else {
                    // INVOKESPECIAL <init>()V
                    self.emit_instruction(opcodes::INVOKESPECIAL);
                    let _mref = self.add_method_ref("java/lang/AssertionError", "<init>", "()V");
                    self.emit_short(_mref as i16);
                }
                // ATHROW
                self.emit_instruction(opcodes::ATHROW);
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
                // TODO: Implement field access
            }
            Expr::ArrayAccess(array_access) => {
                self.generate_array_access(array_access)?;
            }
            Expr::Cast(cast_expr) => {
                self.generate_cast(cast_expr)?;
            }
            Expr::InstanceOf(instance_of) => {
                // TODO: Implement instanceof
            }
            Expr::Conditional(conditional) => {
                self.generate_ternary_expression(conditional)?;
            }
            Expr::New(new_expr) => {
                // TODO: Implement new expression
            }
            Expr::Parenthesized(expr) => {
                self.generate_expression(expr)?;
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
            BinaryOp::Add => self.emit_instruction(opcodes::IADD),
            BinaryOp::Sub => self.emit_instruction(opcodes::ISUB),
            BinaryOp::Mul => self.emit_instruction(opcodes::IMUL),
            BinaryOp::Div => self.emit_instruction(opcodes::IDIV),
            BinaryOp::Mod => self.emit_instruction(opcodes::IREM),
            BinaryOp::Lt => self.emit_instruction(opcodes::IF_ICMPLT),
            BinaryOp::Le => self.emit_instruction(opcodes::IF_ICMPLE),
            BinaryOp::Gt => self.emit_instruction(opcodes::IF_ICMPGT),
            BinaryOp::Ge => self.emit_instruction(opcodes::IF_ICMPGE),
            BinaryOp::Eq => self.emit_instruction(opcodes::IF_ICMPEQ),
            BinaryOp::Ne => self.emit_instruction(opcodes::IF_ICMPNE),
            BinaryOp::And => self.emit_instruction(opcodes::IAND),
            BinaryOp::Or => self.emit_instruction(opcodes::IOR),
            BinaryOp::Xor => self.emit_instruction(opcodes::IXOR),
            BinaryOp::LShift => self.emit_instruction(opcodes::ISHL),
            BinaryOp::RShift => self.emit_instruction(opcodes::ISHR),
            BinaryOp::URShift => self.emit_instruction(opcodes::IUSHR),
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
                self.emit_instruction(opcodes::INEG);
            }
            UnaryOp::Not => {
                self.generate_expression(&unary.operand)?;
                self.emit_instruction(opcodes::IFEQ);
            }
            UnaryOp::BitNot => {
                self.generate_expression(&unary.operand)?;
                self.emit_instruction(opcodes::ICONST_M1);
                self.emit_instruction(opcodes::IXOR);
            }
            UnaryOp::PreInc | UnaryOp::PostInc => {
                // TODO: Implement increment
                self.generate_expression(&unary.operand)?;
            }
            UnaryOp::PreDec | UnaryOp::PostDec => {
                // TODO: Implement decrement
                self.generate_expression(&unary.operand)?;
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
                    0 => self.emit_instruction(opcodes::ICONST_0),
                    1 => self.emit_instruction(opcodes::ICONST_1),
                    2 => self.emit_instruction(opcodes::ICONST_2),
                    3 => self.emit_instruction(opcodes::ICONST_3),
                    4 => self.emit_instruction(opcodes::ICONST_4),
                    5 => self.emit_instruction(opcodes::ICONST_5),
                    -1 => self.emit_instruction(opcodes::ICONST_M1),
                    _ => {
                        if *value >= -128 && *value <= 127 {
                            self.emit_instruction(opcodes::BIPUSH);
                            self.emit_byte((*value as i8) as u8);
                        } else if *value >= -32768 && *value <= 32767 {
                            self.emit_instruction(opcodes::SIPUSH);
                            self.emit_short(*value as i16);
                        } else {
                            // For larger values, we need to use LDC
                            // This is a simplified approach
                            self.emit_instruction(opcodes::LDC);
                            self.emit_byte(1); // Constant pool index
                        }
                    }
                }
            }
            Literal::Float(value) => {
                if *value == 0.0 {
                    self.emit_instruction(opcodes::FCONST_0);
                } else if *value == 1.0 {
                    self.emit_instruction(opcodes::FCONST_1);
                } else if *value == 2.0 {
                    self.emit_instruction(opcodes::FCONST_2);
                } else {
                    // For other values, we need to use LDC
                    self.emit_instruction(opcodes::LDC);
                    self.emit_byte(1); // Constant pool index
                }
            }
            Literal::Boolean(value) => {
                if *value {
                    self.emit_instruction(opcodes::ICONST_1);
                } else {
                    self.emit_instruction(opcodes::ICONST_0);
                }
            }
            Literal::String(value) => {
                // TODO: Add string to constant pool
                self.emit_instruction(opcodes::LDC);
                self.emit_byte(1); // Constant pool index
            }
            Literal::Char(value) => {
                let int_value = *value as i32;
                if int_value >= 0 && int_value <= 5 {
                    match int_value {
                        0 => self.emit_instruction(opcodes::ICONST_0),
                        1 => self.emit_instruction(opcodes::ICONST_1),
                        2 => self.emit_instruction(opcodes::ICONST_2),
                        3 => self.emit_instruction(opcodes::ICONST_3),
                        4 => self.emit_instruction(opcodes::ICONST_4),
                        5 => self.emit_instruction(opcodes::ICONST_5),
                        _ => unreachable!(),
                    }
                } else {
                    self.emit_instruction(opcodes::BIPUSH);
                    self.emit_byte(int_value as u8);
                }
            }
            Literal::Null => {
                self.emit_instruction(opcodes::ACONST_NULL);
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
            self.emit_instruction(opcodes::ALOAD_0);
            // TODO: Add field reference to constant pool
            self.emit_instruction(opcodes::GETFIELD);
            self.emit_short(1); // Constant pool index
        }
        
        Ok(())
    }
    
    /// Generate bytecode for a method call
    fn generate_method_call(&mut self, call: &MethodCallExpr) -> Result<()> {
        // Generate receiver if present
        if let Some(receiver) = &call.target {
            self.generate_expression(receiver)?;
        } else {
            // Assume 'this' for instance methods
            self.emit_instruction(opcodes::ALOAD_0);
        }
        
        // Generate arguments
        for arg in &call.arguments {
            self.generate_expression(arg)?;
        }
        
        // Generate method call
        let method_ref_index = self.add_method_ref("", &call.name, "()V"); // Simplified
        self.emit_instruction(opcodes::INVOKEVIRTUAL);
        self.emit_short(method_ref_index as i16);
        
        Ok(())
    }

    fn generate_close_for_local(&mut self, index: u16, _tref: &TypeRef) -> Result<()> {
        // Load local
        self.emit_instruction(opcodes::ALOAD);
        self.emit_byte(index as u8);
        // ifnull skip
        let end_label = self.create_label();
        self.emit_instruction(opcodes::IFNULL);
        self.emit_label_reference(end_label);
        // invoke interface close()V (simplified; no constant pool wired)
        self.emit_instruction(opcodes::ALOAD);
        self.emit_byte(index as u8);
        self.emit_instruction(opcodes::INVOKEINTERFACE);
        // placeholder CP index and count-and-0
        self.emit_short(1);
        self.emit_byte(1);
        self.emit_byte(0);
        // end label
        self.mark_label(end_label);
        Ok(())
    }
    
    /// Generate bytecode for an assignment expression
    fn generate_assignment(&mut self, assign: &AssignmentExpr) -> Result<()> {
        // Generate value first
        self.generate_expression(&assign.value)?;
        
        // Generate target
        match &*assign.target {
            Expr::Identifier(ident) => {
                // Store in local variable
                if let Some(local_var) = self.find_local_variable(&ident.name) {
                    let var_type = local_var.var_type.clone();
                    self.store_local_variable(local_var.index, &var_type)?;
                } else {
                    // Assume it's a field assignment on 'this'
                    self.emit_instruction(opcodes::ALOAD_0);
                    self.emit_instruction(opcodes::PUTFIELD);
                    self.emit_short(1); // Constant pool index
                }
            }
            _ => {
                // TODO: Handle other assignment targets
                return Err(Error::codegen_error(format!("Unsupported assignment target: {:?}", assign.target)));
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
        self.emit_instruction(opcodes::IALOAD); // Assume int array for now
        
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
        self.emit_instruction(opcodes::NEWARRAY);
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
                self.emit_instruction(opcodes::DUP);
                self.generate_expression(label_expr)?;
                self.emit_instruction(opcodes::IF_ICMPEQ);
                let target = self.create_label();
                self.emit_label_reference(target);
                case_labels.push((target, idx));
            }
        }
        // No match: drop value and jump to default (if any) else end
        self.emit_instruction(opcodes::POP);
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
            for (lbl, i) in case_labels.iter().filter(|(_, i)| *i == idx) { self.mark_label(*lbl); }
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
        
        // TODO: Generate actual cast bytecode based on target type
        // For now, just leave the value on the stack
        
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
        self.emit_instruction(opcodes::IFEQ);
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
        self.emit_instruction(opcodes::IFEQ);
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
    fn generate_while_statement(&mut self, while_stmt: &WhileStmt) -> Result<()> {
        self.generate_while_statement_labeled(None, while_stmt)
    }

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
        self.emit_instruction(opcodes::IFEQ);
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
            self.emit_instruction(opcodes::IFEQ);
            self.emit_label_reference(end_label);
        }
        // Body
        self.generate_statement(&for_stmt.body)?;
        // Continue label and updates
        self.mark_label(continue_label);
        for upd in &for_stmt.update {
            self.generate_expression(&upd.expr)?;
            self.emit_instruction(opcodes::POP);
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
                self.store_local_variable(index, &var_decl.type_ref)?;
            }
        }
        Ok(())
    }
    
    /// Generate bytecode for a break statement
    fn generate_break_statement(&mut self) -> Result<()> {
        // TODO: Implement break statement with proper label handling
        self.emit_instruction(opcodes::GOTO);
        self.emit_short(0); // Placeholder
        
        Ok(())
    }
    
    /// Generate bytecode for a continue statement
    fn generate_continue_statement(&mut self) -> Result<()> {
        // TODO: Implement continue statement with proper label handling
        self.emit_instruction(opcodes::GOTO);
        self.emit_short(0); // Placeholder
        
        Ok(())
    }
    
    /// Generate bytecode for a return statement
    fn generate_return(&mut self, return_type: &TypeRef) -> Result<()> {
        // Check if it's a void return type
        if return_type.name == "void" {
            self.emit_instruction(opcodes::RETURN);
        } else {
            // For primitive types, use appropriate return instruction
            match return_type.name.as_str() {
                "int" | "boolean" | "byte" | "short" | "char" => {
                    self.emit_instruction(opcodes::IRETURN);
                }
                "long" => {
                    self.emit_instruction(opcodes::LRETURN);
                }
                "float" => {
                    self.emit_instruction(opcodes::FRETURN);
                }
                "double" => {
                    self.emit_instruction(opcodes::DRETURN);
                }
                _ => {
                    // Reference type
                    self.emit_instruction(opcodes::ARETURN);
                }
            }
        }
        
        Ok(())
    }
    
    /// Load a local variable
    fn load_local_variable(&mut self, index: u16, var_type: &TypeRef) -> Result<()> {
        match var_type.name.as_str() {
            "int" | "boolean" | "byte" | "short" | "char" => {
                match index {
                    0 => self.emit_instruction(opcodes::ILOAD_0),
                    1 => self.emit_instruction(opcodes::ILOAD_1),
                    2 => self.emit_instruction(opcodes::ILOAD_2),
                    3 => self.emit_instruction(opcodes::ILOAD_3),
                    _ => {
                        self.emit_instruction(opcodes::ILOAD);
                        self.emit_byte(index as u8);
                    }
                }
            }
            "long" => {
                match index {
                    0 => self.emit_instruction(opcodes::LLOAD_0),
                    1 => self.emit_instruction(opcodes::LLOAD_1),
                    2 => self.emit_instruction(opcodes::LLOAD_2),
                    3 => self.emit_instruction(opcodes::LLOAD_3),
                    _ => {
                        self.emit_instruction(opcodes::LLOAD);
                        self.emit_byte(index as u8);
                    }
                }
            }
            "float" => {
                match index {
                    0 => self.emit_instruction(opcodes::FLOAD_0),
                    1 => self.emit_instruction(opcodes::FLOAD_1),
                    2 => self.emit_instruction(opcodes::FLOAD_2),
                    3 => self.emit_instruction(opcodes::FLOAD_3),
                    _ => {
                        self.emit_instruction(opcodes::FLOAD);
                        self.emit_byte(index as u8);
                    }
                }
            }
            "double" => {
                match index {
                    0 => self.emit_instruction(opcodes::DLOAD_0),
                    1 => self.emit_instruction(opcodes::DLOAD_1),
                    2 => self.emit_instruction(opcodes::DLOAD_2),
                    3 => self.emit_instruction(opcodes::DLOAD_3),
                    _ => {
                        self.emit_instruction(opcodes::DLOAD);
                        self.emit_byte(index as u8);
                    }
                }
            }
            _ => {
                // Reference type
                match index {
                    0 => self.emit_instruction(opcodes::ALOAD_0),
                    1 => self.emit_instruction(opcodes::ALOAD_1),
                    2 => self.emit_instruction(opcodes::ALOAD_2),
                    3 => self.emit_instruction(opcodes::ALOAD_3),
                    _ => {
                        self.emit_instruction(opcodes::ALOAD);
                        self.emit_byte(index as u8);
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Store a local variable
    fn store_local_variable(&mut self, index: u16, var_type: &TypeRef) -> Result<()> {
        match var_type.name.as_str() {
            "int" | "boolean" | "byte" | "short" | "char" => {
                match index {
                    0 => self.emit_instruction(opcodes::ISTORE_0),
                    1 => self.emit_instruction(opcodes::ISTORE_1),
                    2 => self.emit_instruction(opcodes::ISTORE_2),
                    3 => self.emit_instruction(opcodes::ISTORE_3),
                    _ => {
                        self.emit_instruction(opcodes::ISTORE);
                        self.emit_byte(index as u8);
                    }
                }
            }
            "long" => {
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
            "float" => {
                match index {
                    0 => self.emit_instruction(opcodes::FSTORE_0),
                    1 => self.emit_instruction(opcodes::FSTORE_1),
                    2 => self.emit_instruction(opcodes::FSTORE_2),
                    3 => self.emit_instruction(opcodes::FSTORE_3),
                    _ => {
                        self.emit_instruction(opcodes::FSTORE);
                        self.emit_byte(index as u8);
                    }
                }
            }
            "double" => {
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
            _ => {
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
    fn find_local_variable(&self, name: &str) -> Option<&LocalVariable> {
        self.local_vars.iter().find(|var| var.name == name)
    }
    
    /// Allocate a new local variable
    fn allocate_local_variable(&mut self, name: &str, var_type: &TypeRef) -> u16 {
        let index = self.max_locals;
        let lv = LocalVariable {
            name: name.to_string(),
            var_type: var_type.clone(),
            index,
            start_pc: self.bytecode.len() as u16,
            length: 0,
        };
        let idx_in_vec = self.local_vars.len();
        self.local_vars.push(lv);
        // Track in current scope if any
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.locals.push(idx_in_vec);
        }
        self.max_locals += 1;
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
    
    /// Emit an instruction
    fn emit_instruction(&mut self, opcode: u8) {
        self.bytecode.push(opcode);
        self.update_stack_and_locals();
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
        self.max_stack = self.max_stack.saturating_add(1);
    }
    
    /// Add an integer constant to the constant pool
    fn add_integer_constant(&mut self, _value: i32) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Add a long constant to the constant pool
    fn add_long_constant(&mut self, _value: i64) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Add a float constant to the constant pool
    fn add_float_constant(&mut self, _value: f32) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Add a double constant to the constant pool
    fn add_double_constant(&mut self, _value: f64) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Add a string constant to the constant pool
    fn add_string_constant(&mut self, _value: &str) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Add a class constant to the constant pool
    fn add_class_constant(&mut self, _name: &str) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Add a method reference to the constant pool
    fn add_method_ref(&mut self, _class: &str, _name: &str, _descriptor: &str) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Add a field reference to the constant pool
    fn add_field_ref(&mut self, _class: &str, _name: &str, _descriptor: &str) -> u16 {
        // TODO: Implement constant pool management
        1
    }
    
    /// Get the generated bytecode
    pub fn get_bytecode(self) -> Vec<u8> { self.bytecode }

    /// Finalize and return code, max stack, max locals, and resolved exception table
    pub(crate) fn finalize(self) -> (Vec<u8>, u16, u16, Vec<ExceptionTableEntry>, Vec<LocalVariable>, Vec<(u16,u16)>) {
        let mut exceptions: Vec<ExceptionTableEntry> = Vec::new();
        for pe in &self.pending_exception_entries {
            let start_pc = self.labels.iter().find(|l| l.id == pe.start_label).map(|l| l.position).unwrap_or(0);
            let end_pc = self.labels.iter().find(|l| l.id == pe.end_label).map(|l| l.position).unwrap_or(0);
            let handler_pc = self.labels.iter().find(|l| l.id == pe.handler_label).map(|l| l.position).unwrap_or(0);
            exceptions.push(ExceptionTableEntry::new(start_pc, end_pc, handler_pc, pe.catch_type));
        }
        (self.bytecode, self.max_stack, self.max_locals, exceptions, self.local_vars, self.line_numbers)
    }
    
    /// Get the maximum stack size
    pub fn get_max_stack(&self) -> u16 {
        self.max_stack
    }
    
    /// Get the maximum number of local variables
    pub fn get_max_locals(&self) -> u16 {
        self.max_locals
    }
}

/// Local variable information
#[derive(Debug)]
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

    fn set_local_length(&mut self, local_vec_index: usize, end_pc: u16) {
        if let Some(lv) = self.local_vars.get_mut(local_vec_index) {
            if lv.length == 0 {
                // length is end - start
                lv.length = end_pc.saturating_sub(lv.start_pc);
            }
        }
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
