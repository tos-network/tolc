//! Lower phase - Syntactic sugar desugaring
//! 
//! Corresponds to JavaC's `com.sun.tools.javac.comp.Lower` class.
//! This phase transforms high-level language constructs into simpler forms
//! that are easier to compile to bytecode. This includes:
//! - Enhanced for loops → traditional for loops with iterators
//! - String concatenation → StringBuilder operations
//! - Autoboxing/unboxing → explicit constructor/method calls
//! - Try-with-resources → traditional try-catch-finally blocks
//! - Class literals → synthetic accessor methods
//! - Assertions → conditional throws

use crate::ast::{Ast, TypeDecl, ClassDecl, Stmt, Expr, MethodDecl, ConstructorDecl, EnhancedForStmt, 
                 Block, TypeRef, VarDeclStmt, VariableDeclarator, LiteralExpr, Literal};
use crate::common::error::Result;
use std::collections::HashMap;

/// Lowering statistics
#[derive(Debug, Default)]
pub struct LoweringStats {
    pub enhanced_for_loops: usize,
    pub string_concatenations: usize,
    pub constant_optimizations: usize,
    pub autoboxing_ops: usize,
    pub lambda_expressions: usize,
    pub try_with_resources: usize,
    pub class_literals: usize,
    pub assertions: usize,
    pub inner_classes: usize,
    pub total_transformations: usize,
    pub transformation_time_ms: u64,
}

/// AST transformation phases - order matters for correctness
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TransformationPhase {
    /// Phase 1: Class literals and static initializers
    ClassLiterals,
    /// Phase 2: Inner classes and local classes
    InnerClasses,  
    /// Phase 3: Lambda expressions and method references
    LambdaExpressions,
    /// Phase 4: Try-with-resources statements
    TryWithResources,
    /// Phase 5: Enhanced for loops
    EnhancedForLoops,
    /// Phase 6: String concatenation optimization
    StringConcatenation,
    /// Phase 7: Autoboxing and unboxing
    Autoboxing,
    /// Phase 8: Assertions
    Assertions,
    /// Phase 9: Final cleanup and validation
    FinalCleanup,
}

/// Result of an AST transformation
#[derive(Debug, Clone)]
pub struct TransformationResult {
    pub transformed: bool,
    pub phase: TransformationPhase,
    pub description: String,
    pub synthetic_elements: Vec<SyntheticElement>,
}

/// Synthetic elements generated during transformation
#[derive(Debug, Clone)]
pub enum SyntheticElement {
    Method(MethodDecl),
    Field { name: String, type_name: String, initializer: Option<Expr> },
    Class { name: String, super_class: Option<String> },
    Variable { name: String, type_name: String },
}

/// Context for lowering transformations
#[derive(Debug, Clone)]
pub struct LoweringContext {
    /// Current class being processed
    pub current_class: Option<String>,
    /// Current method being processed
    pub current_method: Option<String>,
    /// Variable counter for synthetic variables
    pub var_counter: usize,
    /// Generated synthetic methods
    pub synthetic_methods: Vec<MethodDecl>,
    /// Generated synthetic fields
    pub synthetic_fields: Vec<String>,
    /// Transformation results for each phase
    pub transformation_results: HashMap<TransformationPhase, Vec<TransformationResult>>,
    /// Current transformation phase
    pub current_phase: Option<TransformationPhase>,
    /// Nested transformation depth (for debugging)
    pub transformation_depth: usize,
    /// Generated synthetic elements
    pub synthetic_elements: Vec<SyntheticElement>,
    /// Transformation options and flags
    pub transformation_flags: TransformationFlags,
}

/// Configuration flags for transformations
#[derive(Debug, Clone)]
pub struct TransformationFlags {
    /// Enable string concatenation optimization
    pub optimize_string_concat: bool,
    /// Enable lambda expression transformation
    pub transform_lambdas: bool,
    /// Enable enhanced for loop desugaring
    pub desugar_enhanced_for: bool,
    /// Enable try-with-resources transformation
    pub transform_try_with_resources: bool,
    /// Enable assertion transformation
    pub transform_assertions: bool,
    /// Enable inner class transformation
    pub transform_inner_classes: bool,
    /// Generate debug information in synthetic elements
    pub debug_synthetic_elements: bool,
}

impl Default for TransformationFlags {
    fn default() -> Self {
        Self {
            optimize_string_concat: true,
            transform_lambdas: true,
            desugar_enhanced_for: true,
            transform_try_with_resources: true,
            transform_assertions: true,
            transform_inner_classes: true,
            debug_synthetic_elements: false,
        }
    }
}

/// Lower phase processor - corresponds to JavaC's Lower class
pub struct Lower {
    pub stats: LoweringStats,
    pub context: LoweringContext,
    /// Map from original constructs to their lowered forms
    pub lowering_cache: HashMap<String, String>,
}

impl Lower {
    pub fn new() -> Self {
        Self {
            stats: LoweringStats::default(),
            context: LoweringContext {
                current_class: None,
                current_method: None,
                var_counter: 0,
                synthetic_methods: Vec::new(),
                synthetic_fields: Vec::new(),
                transformation_results: HashMap::new(),
                current_phase: None,
                transformation_depth: 0,
                synthetic_elements: Vec::new(),
                transformation_flags: TransformationFlags::default(),
            },
            lowering_cache: HashMap::new(),
        }
    }
    
    /// Create Lower with custom transformation flags
    pub fn with_flags(flags: TransformationFlags) -> Self {
        let mut lower = Self::new();
        lower.context.transformation_flags = flags;
        lower
    }
    
    /// Process AST through Lower phase - desugar syntax
    /// Corresponds to JavaC's Lower.translateTopLevelClass() method
    pub fn process(&mut self, ast: Ast) -> Result<Ast> {
        self.process_with_types(ast, &std::collections::HashMap::new())
    }
    
    /// Process AST with erased type information from TransTypes phase
    pub fn process_with_types(&mut self, mut ast: Ast, erased_types: &std::collections::HashMap<String, String>) -> Result<Ast> {
        eprintln!("🔍 LOWER: Starting systematic AST transformations");
        eprintln!("📊 LOWER: Using {} erased type mappings", erased_types.len());
        
        let start_time = std::time::Instant::now();
        
        // Store erased types for use in lowering decisions
        // (In a full implementation, this would inform casting and bridge method decisions)
        
        // Execute all transformation phases in order
        ast = self.execute_transformation_phases(ast)?;
        
        // Record timing
        self.stats.transformation_time_ms = start_time.elapsed().as_millis() as u64;
        
        eprintln!("✅ LOWER: AST transformations complete in {}ms", self.stats.transformation_time_ms);
        eprintln!("📊 LOWER: Stats - Enhanced for: {}, String concat: {}, Autoboxing: {}, Lambda: {}, Try-with-resources: {}, Class literals: {}, Assertions: {}, Inner classes: {}", 
                 self.stats.enhanced_for_loops,
                 self.stats.string_concatenations, 
                 self.stats.autoboxing_ops,
                 self.stats.lambda_expressions,
                 self.stats.try_with_resources,
                 self.stats.class_literals,
                 self.stats.assertions,
                 self.stats.inner_classes);
        eprintln!("🔧 LOWER: Total transformations: {}, Synthetic elements: {}", 
                 self.stats.total_transformations,
                 self.context.synthetic_elements.len());
        
        Ok(ast)
    }
    
    /// Execute all transformation phases in the correct order
    /// This ensures proper dependency management between transformations
    fn execute_transformation_phases(&mut self, mut ast: Ast) -> Result<Ast> {
        let phases = vec![
            TransformationPhase::ClassLiterals,
            TransformationPhase::InnerClasses,
            TransformationPhase::LambdaExpressions,
            TransformationPhase::TryWithResources,
            TransformationPhase::EnhancedForLoops,
            TransformationPhase::StringConcatenation,
            TransformationPhase::Autoboxing,
            TransformationPhase::Assertions,
            TransformationPhase::FinalCleanup,
        ];
        
        for phase in phases {
            eprintln!("🔄 LOWER: Executing transformation phase: {:?}", phase);
            self.context.current_phase = Some(phase);
            self.context.transformation_depth = 0;
            
            let phase_start = std::time::Instant::now();
            let initial_transformations = self.stats.total_transformations;
            
            // Execute phase transformation
            ast = self.execute_phase_transformation(ast, phase)?;
            
            let phase_transformations = self.stats.total_transformations - initial_transformations;
            let phase_time = phase_start.elapsed().as_millis();
            
            eprintln!("✅ LOWER: Phase {:?} complete - {} transformations in {}ms", 
                     phase, phase_transformations, phase_time);
        }
        
        self.context.current_phase = None;
        
        // Apply all accumulated synthetic elements
        self.apply_synthetic_elements(&mut ast)?;
        
        Ok(ast)
    }
    
    /// Execute transformations for a specific phase
    fn execute_phase_transformation(&mut self, mut ast: Ast, phase: TransformationPhase) -> Result<Ast> {
        // Check if this phase is enabled
        if !self.is_phase_enabled(phase) {
            eprintln!("⏭️ LOWER: Phase {:?} disabled, skipping", phase);
            return Ok(ast);
        }
        
        // Process all type declarations with the current phase
        for type_decl in &mut ast.type_decls {
            self.transform_type_declaration_for_phase(type_decl, phase)?;
        }
        
        Ok(ast)
    }
    
    /// Check if a transformation phase is enabled
    fn is_phase_enabled(&self, phase: TransformationPhase) -> bool {
        match phase {
            TransformationPhase::ClassLiterals => true, // Always enabled
            TransformationPhase::InnerClasses => self.context.transformation_flags.transform_inner_classes,
            TransformationPhase::LambdaExpressions => self.context.transformation_flags.transform_lambdas,
            TransformationPhase::TryWithResources => self.context.transformation_flags.transform_try_with_resources,
            TransformationPhase::EnhancedForLoops => self.context.transformation_flags.desugar_enhanced_for,
            TransformationPhase::StringConcatenation => self.context.transformation_flags.optimize_string_concat,
            TransformationPhase::Autoboxing => true, // Always needed for correctness
            TransformationPhase::Assertions => self.context.transformation_flags.transform_assertions,
            TransformationPhase::FinalCleanup => true, // Always enabled
        }
    }
    
    /// Transform type declaration for a specific phase
    fn transform_type_declaration_for_phase(&mut self, type_decl: &mut TypeDecl, phase: TransformationPhase) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.transform_class_for_phase(class_decl, phase)?;
            }
            TypeDecl::Interface(interface_decl) => {
                self.transform_interface_for_phase(interface_decl, phase)?;
            }
            TypeDecl::Enum(enum_decl) => {
                self.transform_enum_for_phase(enum_decl, phase)?;
            }
            TypeDecl::Annotation(annotation_decl) => {
                self.transform_annotation_for_phase(annotation_decl, phase)?;
            }
        }
        Ok(())
    }
    
    /// Transform class declaration for a specific phase
    fn transform_class_for_phase(&mut self, class_decl: &mut ClassDecl, phase: TransformationPhase) -> Result<()> {
        // Set current class context
        let previous_class = self.context.current_class.clone();
        self.context.current_class = Some(class_decl.name.clone());
        
        // Process class members for this phase
        for member in &mut class_decl.body {
            self.transform_class_member_for_phase(member, phase)?;
        }
        
        // Restore previous class context
        self.context.current_class = previous_class;
        Ok(())
    }
    
    /// Transform interface declaration for a specific phase
    fn transform_interface_for_phase(&mut self, interface_decl: &mut crate::ast::InterfaceDecl, phase: TransformationPhase) -> Result<()> {
        let previous_class = self.context.current_class.clone();
        self.context.current_class = Some(interface_decl.name.clone());
        
        for member in &mut interface_decl.body {
            if let crate::ast::InterfaceMember::Method(method) = member {
                self.transform_method_for_phase(method, phase)?;
            }
        }
        
        self.context.current_class = previous_class;
        Ok(())
    }
    
    /// Transform enum declaration for a specific phase
    fn transform_enum_for_phase(&mut self, enum_decl: &mut crate::ast::EnumDecl, phase: TransformationPhase) -> Result<()> {
        let previous_class = self.context.current_class.clone();
        self.context.current_class = Some(enum_decl.name.clone());
        
        // TODO: Transform enum constants and methods
        eprintln!("🔧 LOWER: Phase {:?} processing enum: {}", phase, enum_decl.name);
        
        self.context.current_class = previous_class;
        Ok(())
    }
    
    /// Transform annotation declaration for a specific phase
    fn transform_annotation_for_phase(&mut self, annotation_decl: &mut crate::ast::AnnotationDecl, phase: TransformationPhase) -> Result<()> {
        let previous_class = self.context.current_class.clone();
        self.context.current_class = Some(annotation_decl.name.clone());
        
        // Annotations typically don't need much transformation
        eprintln!("🔧 LOWER: Phase {:?} processing annotation: {}", phase, annotation_decl.name);
        
        self.context.current_class = previous_class;
        Ok(())
    }
    
    /// Transform class member for a specific phase
    fn transform_class_member_for_phase(&mut self, member: &mut crate::ast::ClassMember, phase: TransformationPhase) -> Result<()> {
        match member {
            crate::ast::ClassMember::Method(method) => {
                self.transform_method_for_phase(method, phase)?;
            }
            crate::ast::ClassMember::Constructor(constructor) => {
                self.transform_constructor_for_phase(constructor, phase)?;
            }
            crate::ast::ClassMember::Field(field) => {
                self.transform_field_for_phase(field, phase)?;
            }
            crate::ast::ClassMember::TypeDecl(nested_type) => {
                self.transform_type_declaration_for_phase(nested_type, phase)?;
            }
            crate::ast::ClassMember::Initializer(initializer) => {
                self.transform_initializer_for_phase(initializer, phase)?;
            }
        }
        Ok(())
    }
    
    /// Transform method for a specific phase
    fn transform_method_for_phase(&mut self, method: &mut MethodDecl, phase: TransformationPhase) -> Result<()> {
        let previous_method = self.context.current_method.clone();
        self.context.current_method = Some(method.name.clone());
        
        if let Some(ref mut body) = method.body {
            for stmt in &mut body.statements {
                self.transform_statement_for_phase(stmt, phase)?;
            }
        }
        
        self.context.current_method = previous_method;
        Ok(())
    }
    
    /// Transform constructor for a specific phase
    fn transform_constructor_for_phase(&mut self, constructor: &mut ConstructorDecl, phase: TransformationPhase) -> Result<()> {
        let previous_method = self.context.current_method.clone();
        self.context.current_method = Some("<init>".to_string());
        
        for stmt in &mut constructor.body.statements {
            self.transform_statement_for_phase(stmt, phase)?;
        }
        
        self.context.current_method = previous_method;
        Ok(())
    }
    
    /// Transform field for a specific phase
    fn transform_field_for_phase(&mut self, field: &mut crate::ast::FieldDecl, phase: TransformationPhase) -> Result<()> {
        match phase {
            TransformationPhase::ClassLiterals => {
                // Transform class literal initializers
                if let Some(ref mut init) = field.initializer {
                    self.transform_expression_for_phase(init, phase)?;
                }
            }
            _ => {
                // Other phases may not need field transformation
            }
        }
        Ok(())
    }
    
    /// Transform initializer block for a specific phase
    fn transform_initializer_for_phase(&mut self, initializer: &mut crate::ast::InitializerBlock, phase: TransformationPhase) -> Result<()> {
        for stmt in &mut initializer.body.statements {
            self.transform_statement_for_phase(stmt, phase)?;
        }
        Ok(())
    }
    
    /// Transform statement for a specific phase
    fn transform_statement_for_phase(&mut self, stmt: &mut Stmt, phase: TransformationPhase) -> Result<()> {
        self.context.transformation_depth += 1;
        
        let result = match phase {
            TransformationPhase::EnhancedForLoops => self.transform_enhanced_for_loops(stmt),
            TransformationPhase::StringConcatenation => self.transform_string_concatenation(stmt),
            TransformationPhase::TryWithResources => self.transform_try_with_resources(stmt),
            TransformationPhase::Assertions => self.transform_assertions(stmt),
            TransformationPhase::LambdaExpressions => self.transform_lambda_expressions(stmt),
            TransformationPhase::Autoboxing => self.transform_autoboxing(stmt),
            _ => {
                // Default: process nested statements
                self.transform_nested_statements(stmt, phase)
            }
        };
        
        self.context.transformation_depth -= 1;
        result
    }
    
    /// Transform expression for a specific phase
    fn transform_expression_for_phase(&mut self, expr: &mut Expr, phase: TransformationPhase) -> Result<()> {
        match phase {
            TransformationPhase::StringConcatenation => {
                self.transform_string_concatenation_expr(expr)?;
            }
            TransformationPhase::Autoboxing => {
                self.transform_autoboxing_expr(expr)?;
            }
            TransformationPhase::LambdaExpressions => {
                self.transform_lambda_expression(expr)?;
            }
            _ => {
                // Recursively transform nested expressions
                self.transform_nested_expressions(expr, phase)?;
            }
        }
        Ok(())
    }
    
    /// Apply accumulated synthetic elements to the AST
    fn apply_synthetic_elements(&mut self, ast: &mut Ast) -> Result<()> {
        eprintln!("🔧 LOWER: Applying {} synthetic elements", self.context.synthetic_elements.len());
        
        // Group synthetic elements by class
        let mut class_elements: HashMap<String, Vec<SyntheticElement>> = HashMap::new();
        
        for element in &self.context.synthetic_elements {
            match element {
                SyntheticElement::Method(method) => {
                    if let Some(ref current_class) = self.context.current_class {
                        class_elements.entry(current_class.clone())
                            .or_insert_with(Vec::new)
                            .push(element.clone());
                    }
                }
                SyntheticElement::Field { .. } => {
                    if let Some(ref current_class) = self.context.current_class {
                        class_elements.entry(current_class.clone())
                            .or_insert_with(Vec::new)
                            .push(element.clone());
                    }
                }
                _ => {
                    // Other types of synthetic elements
                }
            }
        }
        
        // Apply elements to their respective classes
        for type_decl in &mut ast.type_decls {
            if let TypeDecl::Class(class_decl) = type_decl {
                if let Some(elements) = class_elements.get(&class_decl.name) {
                    self.apply_synthetic_elements_to_class(class_decl, elements)?;
                }
            }
        }
        
        Ok(())
    }
    
    /// Apply synthetic elements to a specific class
    fn apply_synthetic_elements_to_class(&mut self, class_decl: &mut ClassDecl, elements: &[SyntheticElement]) -> Result<()> {
        for element in elements {
            match element {
                SyntheticElement::Method(method) => {
                    class_decl.body.push(crate::ast::ClassMember::Method(method.clone()));
                    eprintln!("➕ LOWER: Added synthetic method: {}", method.name);
                }
                SyntheticElement::Field { name, .. } => {
                    eprintln!("➕ LOWER: Synthetic field marked for addition: {}", name);
                    // TODO: Create and add field declaration
                }
                _ => {
                    eprintln!("➕ LOWER: Applied synthetic element: {:?}", element);
                }
            }
        }
        Ok(())
    }
    
    // ================================================================================
    // PHASE-SPECIFIC TRANSFORMATION METHODS - JavaC Lower patterns
    // ================================================================================
    
    /// Transform enhanced for loops in the current statement
    fn transform_enhanced_for_loops(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::EnhancedFor(enhanced_for_stmt) => {
                eprintln!("🔄 LOWER: Transforming enhanced for loop");
                self.stats.enhanced_for_loops += 1;
                self.stats.total_transformations += 1;
                
                // Use existing desugar_enhanced_for method
                let lowered_for = self.desugar_enhanced_for(enhanced_for_stmt)?;
                *stmt = lowered_for;
                Ok(())
            }
            _ => {
                // Recursively process nested statements
                self.transform_nested_statements(stmt, TransformationPhase::EnhancedForLoops)
            }
        }
    }
    
    /// Transform string concatenation operations
    fn transform_string_concatenation(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.transform_string_concatenation_expr(&mut expr_stmt.expr)?;
            }
            Stmt::Declaration(var_decl) => {
                for variable in &mut var_decl.variables {
                    if let Some(ref mut init) = variable.initializer {
                        self.transform_string_concatenation_expr(init)?;
                    }
                }
            }
            _ => {
                // Recursively process nested statements
                self.transform_nested_statements(stmt, TransformationPhase::StringConcatenation)?;
            }
        }
        Ok(())
    }
    
    /// Transform string concatenation in expressions
    fn transform_string_concatenation_expr(&mut self, expr: &mut Expr) -> Result<()> {
        match expr {
            Expr::Binary(binary_expr) => {
                // Check for string concatenation patterns
                if binary_expr.operator == crate::ast::BinaryOp::Add {
                    if self.is_string_concatenation(&binary_expr) {
                        eprintln!("🔄 LOWER: Transforming string concatenation");
                        self.stats.string_concatenations += 1;
                        self.stats.total_transformations += 1;
                        
                        let lowered_expr = self.lower_string_concatenation(binary_expr)?;
                        *expr = lowered_expr;
                        return Ok(());
                    }
                }
                
                // Recursively process operands
                self.transform_string_concatenation_expr(&mut binary_expr.left)?;
                self.transform_string_concatenation_expr(&mut binary_expr.right)?;
            }
            _ => {
                // Recursively process nested expressions
                self.transform_nested_expressions(expr, TransformationPhase::StringConcatenation)?;
            }
        }
        Ok(())
    }
    
    /// Transform try-with-resources statements
    fn transform_try_with_resources(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Try(try_stmt) => {
                if !try_stmt.resources.is_empty() {
                    eprintln!("🔄 LOWER: Transforming try-with-resources");
                    self.stats.try_with_resources += 1;
                    self.stats.total_transformations += 1;
                    
                    // Transform try-with-resources into traditional try-catch-finally
                    // TODO: Implement full try-with-resources transformation
                    eprintln!("⚠️ LOWER: Try-with-resources transformation not fully implemented");
                }
                
                // Process nested statements in try/catch/finally blocks
                for stmt in &mut try_stmt.try_block.statements {
                    self.transform_statement_for_phase(stmt, TransformationPhase::TryWithResources)?;
                }
                
                for catch_clause in &mut try_stmt.catch_clauses {
                    for stmt in &mut catch_clause.block.statements {
                        self.transform_statement_for_phase(stmt, TransformationPhase::TryWithResources)?;
                    }
                }
                
                if let Some(ref mut finally_block) = try_stmt.finally_block {
                    for stmt in &mut finally_block.statements {
                        self.transform_statement_for_phase(stmt, TransformationPhase::TryWithResources)?;
                    }
                }
            }
            _ => {
                self.transform_nested_statements(stmt, TransformationPhase::TryWithResources)?;
            }
        }
        Ok(())
    }
    
    /// Transform assertion statements
    fn transform_assertions(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Assert(assert_stmt) => {
                eprintln!("🔄 LOWER: Transforming assertion");
                self.stats.assertions += 1;
                self.stats.total_transformations += 1;
                
                // Transform assertion into conditional throw
                // TODO: Implement full assertion transformation
                self.transform_expression_for_phase(&mut assert_stmt.condition, TransformationPhase::Assertions)?;
                if let Some(ref mut message) = assert_stmt.message {
                    self.transform_expression_for_phase(message, TransformationPhase::Assertions)?;
                }
            }
            _ => {
                self.transform_nested_statements(stmt, TransformationPhase::Assertions)?;
            }
        }
        Ok(())
    }
    
    /// Transform lambda expressions and method references
    fn transform_lambda_expressions(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.transform_lambda_expression(&mut expr_stmt.expr)?;
            }
            _ => {
                self.transform_nested_statements(stmt, TransformationPhase::LambdaExpressions)?;
            }
        }
        Ok(())
    }
    
    /// Transform lambda expression into anonymous class
    fn transform_lambda_expression(&mut self, expr: &mut Expr) -> Result<()> {
        match expr {
            Expr::Lambda(_lambda_expr) => {
                eprintln!("🔄 LOWER: Transforming lambda expression");
                self.stats.lambda_expressions += 1;
                self.stats.total_transformations += 1;
                
                // Transform lambda into anonymous class
                // TODO: Implement full lambda transformation
                eprintln!("⚠️ LOWER: Lambda transformation not fully implemented");
            }
            _ => {
                // Recursively process nested expressions
                self.transform_nested_expressions(expr, TransformationPhase::LambdaExpressions)?;
            }
        }
        Ok(())
    }
    
    /// Transform autoboxing and unboxing operations
    fn transform_autoboxing(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr_stmt) => {
                self.transform_autoboxing_expr(&mut expr_stmt.expr)?;
            }
            _ => {
                self.transform_nested_statements(stmt, TransformationPhase::Autoboxing)?;
            }
        }
        Ok(())
    }
    
    /// Transform autoboxing in expressions
    fn transform_autoboxing_expr(&mut self, expr: &mut Expr) -> Result<()> {
        match expr {
            Expr::MethodCall(method_call) => {
                // Check for autoboxing patterns
                if self.is_autoboxing_operation(method_call) {
                    eprintln!("🔄 LOWER: Transforming autoboxing operation");
                    self.stats.autoboxing_ops += 1;
                    self.stats.total_transformations += 1;
                    
                    // Transform autoboxing into explicit constructor/method calls
                    // TODO: Implement full autoboxing transformation
                }
                
                // Process arguments
                for arg in &mut method_call.arguments {
                    self.transform_autoboxing_expr(arg)?;
                }
            }
            _ => {
                self.transform_nested_expressions(expr, TransformationPhase::Autoboxing)?;
            }
        }
        Ok(())
    }
    
    /// Transform nested statements for a given phase
    fn transform_nested_statements(&mut self, stmt: &mut Stmt, phase: TransformationPhase) -> Result<()> {
        match stmt {
            Stmt::Block(block) => {
                for stmt in &mut block.statements {
                    self.transform_statement_for_phase(stmt, phase)?;
                }
            }
            Stmt::If(if_stmt) => {
                self.transform_expression_for_phase(&mut if_stmt.condition, phase)?;
                self.transform_statement_for_phase(&mut if_stmt.then_branch, phase)?;
                if let Some(ref mut else_stmt) = if_stmt.else_branch {
                    self.transform_statement_for_phase(else_stmt, phase)?;
                }
            }
            Stmt::While(while_stmt) => {
                self.transform_expression_for_phase(&mut while_stmt.condition, phase)?;
                self.transform_statement_for_phase(&mut while_stmt.body, phase)?;
            }
            Stmt::For(for_stmt) => {
                for init_stmt in &mut for_stmt.init {
                    self.transform_statement_for_phase(init_stmt, phase)?;
                }
                if let Some(ref mut condition) = for_stmt.condition {
                    self.transform_expression_for_phase(condition, phase)?;
                }
                for update_stmt in &mut for_stmt.update {
                    self.transform_expression_for_phase(&mut update_stmt.expr, phase)?;
                }
                self.transform_statement_for_phase(&mut for_stmt.body, phase)?;
            }
            _ => {
                // Base case: no nested statements
            }
        }
        Ok(())
    }
    
    /// Transform nested expressions for a given phase
    fn transform_nested_expressions(&mut self, expr: &mut Expr, phase: TransformationPhase) -> Result<()> {
        match expr {
            Expr::Binary(binary_expr) => {
                self.transform_expression_for_phase(&mut binary_expr.left, phase)?;
                self.transform_expression_for_phase(&mut binary_expr.right, phase)?;
            }
            Expr::Unary(unary_expr) => {
                self.transform_expression_for_phase(&mut unary_expr.operand, phase)?;
            }
            Expr::MethodCall(method_call) => {
                if let Some(ref mut target) = method_call.target {
                    self.transform_expression_for_phase(target, phase)?;
                }
                for arg in &mut method_call.arguments {
                    self.transform_expression_for_phase(arg, phase)?;
                }
            }
            Expr::New(new_expr) => {
                for arg in &mut new_expr.arguments {
                    self.transform_expression_for_phase(arg, phase)?;
                }
                if let Some(ref mut anonymous_body) = new_expr.anonymous_body {
                    for member in &mut anonymous_body.body {
                        // Transform class members (methods, fields, etc.)
                        // TODO: Add proper ClassMember transformation
                    }
                }
            }
            _ => {
                // Base case: no nested expressions
            }
        }
        Ok(())
    }
    
    /// Record a transformation result
    fn record_transformation(&mut self, phase: TransformationPhase, description: String) -> TransformationResult {
        let result = TransformationResult {
            transformed: true,
            phase,
            description: description.clone(),
            synthetic_elements: vec![],
        };
        
        self.context.transformation_results
            .entry(phase)
            .or_insert_with(Vec::new)
            .push(result);
        
        eprintln!("📝 LOWER: Recorded transformation: {}", description);
        
        TransformationResult {
            transformed: true,
            phase,
            description,
            synthetic_elements: vec![],
        }
    }
    
    /// Add a synthetic element to be applied later
    fn add_synthetic_element(&mut self, element: SyntheticElement) {
        eprintln!("🔧 LOWER: Adding synthetic element: {:?}", element);
        self.context.synthetic_elements.push(element);
    }
    
    /// Get transformation statistics
    pub fn get_transformation_stats(&self) -> &LoweringStats {
        &self.stats
    }
    
    /// Get transformation results for debugging
    pub fn get_transformation_results(&self) -> &HashMap<TransformationPhase, Vec<TransformationResult>> {
        &self.context.transformation_results
    }
    
    /// Lower a type declaration - corresponds to JavaC's visitClassDef
    fn lower_type_declaration(&mut self, type_decl: &mut TypeDecl) -> Result<()> {
        match type_decl {
            TypeDecl::Class(class_decl) => {
                self.lower_class_declaration(class_decl)?;
            }
            TypeDecl::Interface(interface_decl) => {
                eprintln!("🔧 LOWER: Processing interface: {}", interface_decl.name);
                // Interfaces typically don't need much lowering
                for member in &mut interface_decl.body {
                    if let crate::ast::InterfaceMember::Method(method) = member {
                        self.context.current_method = Some(method.name.clone());
                        self.lower_method_declaration(method)?;
                        self.context.current_method = None;
                    }
                }
            }
            TypeDecl::Enum(enum_decl) => {
                eprintln!("🔧 LOWER: Processing enum: {}", enum_decl.name);
                // TODO: Implement enum lowering (synthetic methods, values(), valueOf(), etc.)
            }
            TypeDecl::Annotation(annotation_decl) => {
                eprintln!("🔧 LOWER: Processing annotation: {}", annotation_decl.name);
                // Annotations typically don't need lowering
            }
        }
        Ok(())
    }
    
    /// Lower a class declaration - corresponds to JavaC's translateClass
    fn lower_class_declaration(&mut self, class_decl: &mut ClassDecl) -> Result<()> {
        eprintln!("🔧 LOWER: Processing class: {}", class_decl.name);
        
        // Set current class context
        self.context.current_class = Some(class_decl.name.clone());
        
        // Process class members
        for member in &mut class_decl.body {
            self.lower_class_member(member)?;
        }
        
        // Add any synthetic methods that were generated
        for synthetic_method in &self.context.synthetic_methods {
            class_decl.body.push(crate::ast::ClassMember::Method(synthetic_method.clone()));
        }
        self.context.synthetic_methods.clear();
        
        // Clear current class context
        self.context.current_class = None;
        
        Ok(())
    }
    
    /// Lower a class member
    fn lower_class_member(&mut self, member: &mut crate::ast::ClassMember) -> Result<()> {
        match member {
            crate::ast::ClassMember::Method(method) => {
                self.context.current_method = Some(method.name.clone());
                self.lower_method_declaration(method)?;
                self.context.current_method = None;
            }
            crate::ast::ClassMember::Constructor(constructor) => {
                self.context.current_method = Some("<init>".to_string());
                self.lower_constructor_declaration(constructor)?;
                self.context.current_method = None;
            }
            crate::ast::ClassMember::Field(field) => {
                eprintln!("🔧 LOWER: Processing field: {}", field.name);
                // Fields typically don't need lowering unless they have complex initializers
            }
            crate::ast::ClassMember::TypeDecl(nested_type) => {
                eprintln!("🔧 LOWER: Processing nested type");
                self.stats.inner_classes += 1;
                self.lower_type_declaration(nested_type)?;
            }
            crate::ast::ClassMember::Initializer(initializer) => {
                eprintln!("🔧 LOWER: Processing initializer block");
                for stmt in &mut initializer.body.statements {
                    self.lower_statement(stmt)?;
                }
            }
        }
        Ok(())
    }
    
    /// Lower a method declaration
    fn lower_method_declaration(&mut self, method: &mut MethodDecl) -> Result<()> {
        eprintln!("🔧 LOWER: Processing method: {}", method.name);
        
        if let Some(ref mut body) = method.body {
            for stmt in &mut body.statements {
                self.lower_statement(stmt)?;
            }
        }
        
        Ok(())
    }
    
    /// Lower a constructor declaration
    fn lower_constructor_declaration(&mut self, constructor: &mut ConstructorDecl) -> Result<()> {
        eprintln!("🔧 LOWER: Processing constructor");
        
        for stmt in &mut constructor.body.statements {
            self.lower_statement(stmt)?;
        }
        
        Ok(())
    }
    
    /// Lower a statement - this is where most syntactic sugar is handled
    fn lower_statement(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            // Enhanced for loops and try-with-resources are not in current AST
            // They would be handled as part of For and Try statements respectively
            Stmt::Try(try_stmt) => {
                if !try_stmt.resources.is_empty() {
                    eprintln!("🔧 LOWER: Lowering try-with-resources");
                    self.stats.try_with_resources += 1;
                    // TODO: Transform try-with-resources into traditional try-catch-finally
                }
                
                for stmt in &mut try_stmt.try_block.statements {
                    self.lower_statement(stmt)?;
                }
                
                for catch_clause in &mut try_stmt.catch_clauses {
                    for stmt in &mut catch_clause.block.statements {
                        self.lower_statement(stmt)?;
                    }
                }
                
                if let Some(ref mut finally_block) = try_stmt.finally_block {
                    for stmt in &mut finally_block.statements {
                        self.lower_statement(stmt)?;
                    }
                }
            }
            Stmt::Expression(expr_stmt) => {
                self.lower_expression(&mut expr_stmt.expr)?;
            }
            Stmt::Declaration(var_decl) => {
                for variable in &mut var_decl.variables {
                    if let Some(ref mut init) = variable.initializer {
                        self.lower_expression(init)?;
                    }
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref mut expr) = return_stmt.value {
                    self.lower_expression(expr)?;
                }
            }
            Stmt::If(if_stmt) => {
                self.lower_expression(&mut if_stmt.condition)?;
                self.lower_statement(&mut if_stmt.then_branch)?;
                if let Some(ref mut else_branch) = if_stmt.else_branch {
                    self.lower_statement(else_branch)?;
                }
            }
            Stmt::While(while_stmt) => {
                self.lower_expression(&mut while_stmt.condition)?;
                self.lower_statement(&mut while_stmt.body)?;
            }
            Stmt::For(for_stmt) => {
                for init_stmt in &mut for_stmt.init {
                    self.lower_statement(init_stmt)?;
                }
                if let Some(ref mut condition) = for_stmt.condition {
                    self.lower_expression(condition)?;
                }
                for update_stmt in &mut for_stmt.update {
                    self.lower_expression(&mut update_stmt.expr)?;
                }
                self.lower_statement(&mut for_stmt.body)?;
            }
            Stmt::EnhancedFor(enhanced_for_stmt) => {
                eprintln!("🔧 LOWER: Lowering enhanced for loop");
                self.stats.enhanced_for_loops += 1;
                
                // Transform enhanced for into traditional for loop
                let lowered_for = self.desugar_enhanced_for(enhanced_for_stmt)?;
                *stmt = lowered_for;
            }
            Stmt::Block(block) => {
                for stmt in &mut block.statements {
                    self.lower_statement(stmt)?;
                }
            }
            Stmt::Assert(assert_stmt) => {
                eprintln!("🔧 LOWER: Lowering assertion");
                self.stats.assertions += 1;
                // TODO: Transform assertion into conditional throw
                self.lower_expression(&mut assert_stmt.condition)?;
                if let Some(ref mut message) = assert_stmt.message {
                    self.lower_expression(message)?;
                }
            }
            _ => {
                // Other statement types don't need lowering
            }
        }
        Ok(())
    }
    
    /// Lower an expression - handle autoboxing, string concatenation, etc.
    fn lower_expression(&mut self, expr: &mut Expr) -> Result<()> {
        match expr {
            Expr::Binary(binary_expr) => {
                self.lower_expression(&mut binary_expr.left)?;
                self.lower_expression(&mut binary_expr.right)?;
                
                // Check for string concatenation
                if binary_expr.operator == crate::ast::BinaryOp::Add {
                    if self.is_string_concatenation(&binary_expr) {
                        eprintln!("🔧 LOWER: Lowering string concatenation");
                        self.stats.string_concatenations += 1;
                        
                        // Transform: "str" + value -> StringBuilder.append() chain
                        let lowered_expr = self.lower_string_concatenation(binary_expr)?;
                        *expr = lowered_expr;
                        return Ok(());
                    }
                }
            }
            Expr::MethodCall(method_call) => {
                if let Some(ref mut target) = method_call.target {
                    self.lower_expression(target)?;
                }
                for arg in &mut method_call.arguments {
                    self.lower_expression(arg)?;
                }
                
                // Check for autoboxing/unboxing
                if self.is_autoboxing_operation(&method_call) {
                    eprintln!("🔧 LOWER: Lowering autoboxing operation");
                    self.stats.autoboxing_ops += 1;
                    // TODO: Transform autoboxing into explicit constructor/method calls
                }
            }
            Expr::Lambda(lambda_expr) => {
                eprintln!("🔧 LOWER: Lowering lambda expression");
                self.stats.lambda_expressions += 1;
                // TODO: Transform lambda into anonymous class
                // Lambda body can be expression or block - handle accordingly
                match &mut lambda_expr.body {
                    crate::ast::LambdaBody::Expression(expr) => {
                        self.lower_expression(expr)?;
                    }
                    crate::ast::LambdaBody::Block(block) => {
                        for stmt in &mut block.statements {
                            self.lower_statement(stmt)?;
                        }
                    }
                }
            }
            // Class literals not in current AST - would be special FieldAccess
            Expr::Assignment(assign_expr) => {
                self.lower_expression(&mut assign_expr.value)?;
                // Note: target is typically an identifier, no need to lower
            }
            Expr::Unary(unary_expr) => {
                self.lower_expression(&mut unary_expr.operand)?;
            }
            Expr::Conditional(cond_expr) => {
                self.lower_expression(&mut cond_expr.condition)?;
                self.lower_expression(&mut cond_expr.then_expr)?;
                self.lower_expression(&mut cond_expr.else_expr)?;
            }
            Expr::FieldAccess(field_access) => {
                if let Some(ref mut target) = field_access.target {
                    self.lower_expression(target)?;
                }
            }
            Expr::ArrayAccess(array_access) => {
                self.lower_expression(&mut array_access.array)?;
                self.lower_expression(&mut array_access.index)?;
            }
            Expr::New(new_expr) => {
                for arg in &mut new_expr.arguments {
                    self.lower_expression(arg)?;
                }
                // NewExpr doesn't have initializer - anonymous_body is for anonymous classes
                if let Some(ref mut anonymous_body) = new_expr.anonymous_body {
                    // Handle anonymous class body - would need to iterate through ClassMember
                    // For now, just note that this is where anonymous class lowering would happen
                    eprintln!("🔧 LOWER: Anonymous class detected");
                }
            }
            _ => {
                // Literals, identifiers, etc. don't need lowering
            }
        }
        Ok(())
    }
    
    /// Check if a binary expression represents string concatenation
    fn is_string_concatenation(&self, binary_expr: &crate::ast::BinaryExpr) -> bool {
        if binary_expr.operator != crate::ast::BinaryOp::Add {
            return false;
        }
        
        // Check if either operand is a string literal
        self.is_string_type(&binary_expr.left) || self.is_string_type(&binary_expr.right)
    }
    
    /// Check if an expression is likely a string type
    fn is_string_type(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(literal_expr) => {
                matches!(literal_expr.value, crate::ast::Literal::String(_))
            }
            Expr::MethodCall(method_call) => {
                // Methods that return String
                matches!(method_call.name.as_str(), "toString" | "concat" | "substring")
            }
            Expr::Binary(binary_expr) => {
                // Nested string concatenation
                binary_expr.operator == crate::ast::BinaryOp::Add && 
                (self.is_string_type(&binary_expr.left) || self.is_string_type(&binary_expr.right))
            }
            _ => false,
        }
    }
    
    /// Transform string concatenation into StringBuilder method calls (JavaC-aligned)
    /// Performs sophisticated analysis of concatenation chains and generates optimal StringBuilder usage
    /// "Hello " + variable + "!" -> new StringBuilder().append("Hello ").append(variable).append("!").toString()
    fn lower_string_concatenation(&mut self, binary_expr: &crate::ast::BinaryExpr) -> Result<Expr> {
        use crate::ast::*;
        
        // Collect all expressions in the concatenation chain
        let mut concat_expressions = Vec::new();
        let mut estimated_capacity = 0;
        self.collect_string_concat_chain(&Expr::Binary(binary_expr.clone()), &mut concat_expressions, &mut estimated_capacity)?;
        
        eprintln!("🔧 LOWER: String concatenation chain with {} expressions, estimated capacity: {}", 
                 concat_expressions.len(), estimated_capacity);
        
        // Optimize constant concatenations at compile time
        if let Some(constant_result) = self.optimize_constant_string_concat(&concat_expressions) {
            eprintln!("🚀 LOWER: Optimized constant concatenation to: {}", constant_result);
            self.stats.constant_optimizations += 1;
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::String(constant_result),
                span: binary_expr.span,
            }));
        }
        
        // Create StringBuilder with estimated capacity (JavaC pattern)
        let string_builder_new = if estimated_capacity > 16 {
            // StringBuilder(int capacity) constructor for large estimated capacity
            Expr::New(NewExpr {
                target_type: TypeRef {
                    name: "StringBuilder".to_string(),
                    type_args: vec![],
                    span: binary_expr.span,
                    annotations: vec![],
                    array_dims: 0,
                },
                arguments: vec![Expr::Literal(LiteralExpr {
                    value: Literal::Integer(estimated_capacity as i64),
                    span: binary_expr.span,
                })],
                anonymous_body: None,
                span: binary_expr.span,
            })
        } else {
            // Default StringBuilder() constructor
            Expr::New(NewExpr {
                target_type: TypeRef {
                    name: "StringBuilder".to_string(),
                    type_args: vec![],
                    span: binary_expr.span,
                    annotations: vec![],
                    array_dims: 0,
                },
                arguments: vec![],
                anonymous_body: None,
                span: binary_expr.span,
            })
        };
        
        // Chain .append() calls for each expression in order
        let mut current_expr = string_builder_new;
        
        for concat_expr in concat_expressions {
            current_expr = self.create_append_call(current_expr, concat_expr, binary_expr.span)?;
        }
        
        // Finally call .toString()
        let result = Expr::MethodCall(MethodCallExpr {
            target: Some(Box::new(current_expr)),
            name: "toString".to_string(),
            arguments: vec![],
            span: binary_expr.span,
        });
        
        Ok(result)
    }
    
    /// Create a StringBuilder.append() call
    fn create_append_call(&self, target: Expr, argument: Expr, span: crate::ast::Span) -> Result<Expr> {
        use crate::ast::*;
        
        Ok(Expr::MethodCall(MethodCallExpr {
            target: Some(Box::new(target)),
            name: "append".to_string(),
            arguments: vec![argument],
            span,
        }))
    }
    
    /// Collect all expressions in a string concatenation chain (JavaC-aligned)
    /// Recursively traverses binary + operations to build a flat list of expressions
    fn collect_string_concat_chain(&mut self, expr: &Expr, expressions: &mut Vec<Expr>, estimated_capacity: &mut usize) -> Result<()> {
        match expr {
            Expr::Binary(binary_expr) if binary_expr.operator == crate::ast::BinaryOp::Add => {
                // Check if this is string concatenation
                if self.is_string_concatenation(&binary_expr) {
                    // Recursively collect from both sides
                    self.collect_string_concat_chain(&binary_expr.left, expressions, estimated_capacity)?;
                    self.collect_string_concat_chain(&binary_expr.right, expressions, estimated_capacity)?;
                } else {
                    // Not string concatenation, treat as single expression
                    *estimated_capacity += 16; // Average estimate for non-string expressions
                    expressions.push(expr.clone());
                }
            }
            Expr::Literal(literal_expr) => {
                if let crate::ast::Literal::String(s) = &literal_expr.value {
                    *estimated_capacity += s.len();
                } else {
                    *estimated_capacity += 8; // Estimate for other literals
                }
                expressions.push(expr.clone());
            }
            _ => {
                *estimated_capacity += 16; // Average estimate for complex expressions
                expressions.push(expr.clone());
            }
        }
        Ok(())
    }
    
    /// Optimize constant string concatenation at compile time (JavaC-aligned)
    /// If all expressions are string literals, concatenate them at compile time
    fn optimize_constant_string_concat(&self, expressions: &[Expr]) -> Option<String> {
        let mut result = String::new();
        
        for expr in expressions {
            match expr {
                Expr::Literal(literal_expr) => {
                    if let crate::ast::Literal::String(s) = &literal_expr.value {
                        result.push_str(s);
                    } else {
                        // Non-string literal, can't optimize fully
                        return None;
                    }
                }
                _ => {
                    // Non-literal expression, can't optimize
                    return None;
                }
            }
        }
        
        Some(result)
    }
    
    /// Check if a method call represents an autoboxing operation
    fn is_autoboxing_operation(&self, method_call: &crate::ast::MethodCallExpr) -> bool {
        // Check for common autoboxing method names
        matches!(method_call.name.as_str(), 
                "valueOf" | "intValue" | "doubleValue" | "booleanValue" | 
                "byteValue" | "shortValue" | "longValue" | "floatValue" | "charValue")
    }
    
    /// Generate a synthetic variable name
    fn generate_synthetic_variable(&mut self, prefix: &str) -> String {
        self.context.var_counter += 1;
        format!("{}${}", prefix, self.context.var_counter)
    }
    
    /// Generate a synthetic method name
    fn generate_synthetic_method(&mut self, prefix: &str) -> String {
        self.context.var_counter += 1;
        format!("{}${}", prefix, self.context.var_counter)
    }
    
    // ================================================================================
    // ENHANCED FOR LOOP DESUGARING - JavaC Lower.visitForeachLoop patterns
    // ================================================================================
    
    /// Desugar enhanced for loop into traditional for loop
    /// Corresponds to JavaC Lower.visitForeachLoop()
    pub fn desugar_enhanced_for(&mut self, enhanced_for: &EnhancedForStmt) -> Result<Stmt> {
        eprintln!("🔄 DESUGAR: Enhanced for loop with variable '{}' over expression", enhanced_for.variable_name);
        
        // Determine the type of the iterable expression
        let iterable_type = self.infer_iterable_type(&enhanced_for.iterable)?;
        eprintln!("🔍 DESUGAR: Iterable type inferred as: {:?}", iterable_type);
        
        match iterable_type {
            IterableType::Array(component_type) => {
                eprintln!("✅ DESUGAR: Using array-based enhanced for loop");
                self.desugar_array_enhanced_for(enhanced_for, &component_type)
            }
            IterableType::Iterable(element_type) => {
                eprintln!("✅ DESUGAR: Using iterator-based enhanced for loop");
                self.desugar_iterable_enhanced_for(enhanced_for, &element_type)
            }
            IterableType::Unknown => {
                eprintln!("⚠️ DESUGAR: Unknown iterable type, using iterator fallback");
                self.desugar_iterable_enhanced_for(enhanced_for, &TypeRef {
                    name: "java.lang.Object".to_string(),
                    type_args: vec![],
                    annotations: vec![],
                    array_dims: 0,
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                })
            }
        }
    }
    
    /// Infer the type of an iterable expression
    fn infer_iterable_type(&self, expr: &Expr) -> Result<IterableType> {
        // Simple type inference - in practice would use attr.rs results
        match expr {
            Expr::Identifier(ident) => {
                // Check variable type from context (simplified)
                if ident.name.contains("array") || ident.name.contains("Array") {
                    Ok(IterableType::Array(TypeRef {
                        name: "java.lang.Object".to_string(),
                        type_args: Vec::new(),
                        annotations: Vec::new(),
                        array_dims: 0,
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    }))
                } else if ident.name.contains("list") || ident.name.contains("List") {
                    Ok(IterableType::Iterable(TypeRef {
                        name: "java.lang.Object".to_string(),
                        type_args: Vec::new(),
                        annotations: Vec::new(),
                        array_dims: 0,
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    }))
                } else {
                    Ok(IterableType::Unknown)
                }
            }
            Expr::MethodCall(_) => {
                // Method calls typically return collections
                Ok(IterableType::Iterable(TypeRef {
                    name: "java.lang.Object".to_string(),
                    type_args: Vec::new(),
                    annotations: Vec::new(),
                    array_dims: 0,
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                }))
            }
            Expr::New(new_expr) => {
                if new_expr.target_type.name.contains("[]") || new_expr.target_type.name.contains("Array") {
                    Ok(IterableType::Array(TypeRef {
                        name: "java.lang.Object".to_string(),
                        type_args: Vec::new(),
                        annotations: Vec::new(),
                        array_dims: 0,
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    }))
                } else if self.implements_iterable(&new_expr.target_type.name) {
                    Ok(IterableType::Iterable(TypeRef {
                        name: "java.lang.Object".to_string(),
                        type_args: Vec::new(),
                        annotations: Vec::new(),
                        array_dims: 0,
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    }))
                } else {
                    Ok(IterableType::Unknown)
                }
            }
            _ => Ok(IterableType::Unknown)
        }
    }
    
    /// Check if a class name implements Iterable interface
    fn implements_iterable(&self, class_name: &str) -> bool {
        class_name.contains("List") ||
        class_name.contains("Set") ||
        class_name.contains("Collection") ||
        class_name == "java.util.ArrayList" ||
        class_name == "java.util.LinkedList" ||
        class_name == "java.util.HashSet" ||
        class_name == "java.util.TreeSet" ||
        class_name == "java.util.Vector"
    }
    
    /// Desugar array-based enhanced for loop
    /// Translates: for (T item : arrayExpr) stmt;
    /// To: for ({T[] #arr = arrayExpr; int #len = #arr.length; int #i = 0;} #i < #len; #i++) { T item = #arr[#i]; stmt; }
    fn desugar_array_enhanced_for(&mut self, enhanced_for: &EnhancedForStmt, component_type: &TypeRef) -> Result<Stmt> {
        // Generate synthetic variable names
        let array_var = self.generate_synthetic_variable("arr");
        let length_var = self.generate_synthetic_variable("len");
        let index_var = self.generate_synthetic_variable("i");
        
        // Create array variable declaration: T[] #arr = arrayExpr;
        let array_decl = Stmt::Declaration(VarDeclStmt {
            modifiers: vec![],
            type_ref: TypeRef {
                name: format!("{}[]", component_type.name),
                type_args: Vec::new(),
                annotations: Vec::new(),
                array_dims: 0,
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            },
            variables: vec![VariableDeclarator {
                name: array_var.clone(),
                array_dims: 0,
                initializer: Some(enhanced_for.iterable.clone()),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }],
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create length variable declaration: int #len = #arr.length;
        let length_decl = Stmt::Declaration(VarDeclStmt {
            modifiers: vec![],
            type_ref: TypeRef {
                name: "int".to_string(),
                type_args: Vec::new(),
                annotations: Vec::new(),
                array_dims: 0,
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            },
            variables: vec![VariableDeclarator {
                name: length_var.clone(),
                array_dims: 0,
                initializer: Some(Expr::FieldAccess(crate::ast::FieldAccessExpr {
                    target: Some(Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                        name: array_var.clone(),
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    }))),
                    name: "length".to_string(),
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                })),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }],
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create index variable declaration: int #i = 0;
        let index_decl = Stmt::Declaration(VarDeclStmt {
            modifiers: vec![],
            type_ref: TypeRef {
                name: "int".to_string(),
                type_args: Vec::new(),
                annotations: Vec::new(),
                array_dims: 0,
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            },
            variables: vec![VariableDeclarator {
                name: index_var.clone(),
                array_dims: 0,
                initializer: Some(Expr::Literal(LiteralExpr {
                    value: Literal::Integer(0),
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                })),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }],
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create loop condition: #i < #len
        let condition = Some(Expr::Binary(crate::ast::BinaryExpr {
            left: Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                name: index_var.clone(),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            })),
            operator: crate::ast::BinaryOp::Lt,
            right: Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                name: length_var.clone(),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            })),
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        }));
        
        // Create loop update: #i++
        let update = vec![crate::ast::ExprStmt {
            expr: Expr::Unary(crate::ast::UnaryExpr {
                operator: crate::ast::UnaryOp::PostInc,
                operand: Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                    name: index_var.clone(),
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                })),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }),
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        }];
        
        // Create loop variable assignment: T item = #arr[#i];
        let loop_var_decl = Stmt::Declaration(VarDeclStmt {
            modifiers: vec![],
            type_ref: component_type.clone(),
            variables: vec![VariableDeclarator {
                name: enhanced_for.variable_name.clone(),
                array_dims: 0,
                initializer: Some(Expr::ArrayAccess(crate::ast::ArrayAccessExpr {
                    array: Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                        name: array_var.clone(),
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    })),
                    index: Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                        name: index_var.clone(),
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    })),
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                })),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }],
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create loop body with variable declaration + original body
        let mut loop_body_stmts = vec![loop_var_decl];
        match enhanced_for.body.as_ref() {
            Stmt::Block(block) => {
                loop_body_stmts.extend(block.statements.clone());
            }
            single_stmt => {
                loop_body_stmts.push(single_stmt.clone());
            }
        }
        
        let loop_body = Stmt::Block(Block {
            statements: loop_body_stmts,
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create the traditional for loop
        let traditional_for = Stmt::For(crate::ast::ForStmt {
            init: vec![array_decl, length_decl, index_decl],
            condition,
            update,
            body: Box::new(loop_body),
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        Ok(traditional_for)
    }
    
    /// Desugar iterator-based enhanced for loop
    /// Translates: for (T item : iterableExpr) stmt;
    /// To: for (Iterator<T> #iter = iterableExpr.iterator(); #iter.hasNext();) { T item = #iter.next(); stmt; }
    fn desugar_iterable_enhanced_for(&mut self, enhanced_for: &EnhancedForStmt, element_type: &TypeRef) -> Result<Stmt> {
        // Generate synthetic variable name
        let iterator_var = self.generate_synthetic_variable("iter");
        
        // Create iterator variable declaration: Iterator<T> #iter = iterableExpr.iterator();
        let iterator_decl = Stmt::Declaration(VarDeclStmt {
            modifiers: vec![],
            type_ref: TypeRef {
                name: format!("java.util.Iterator<{}>", element_type.name),
                type_args: Vec::new(),
                annotations: Vec::new(),
                array_dims: 0,
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            },
            variables: vec![VariableDeclarator {
                name: iterator_var.clone(),
                array_dims: 0,
                initializer: Some(Expr::MethodCall(crate::ast::MethodCallExpr {
                    target: Some(Box::new(enhanced_for.iterable.clone())),
                    name: "iterator".to_string(),
                    arguments: vec![],
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                })),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }],
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create loop condition: #iter.hasNext()
        let condition = Some(Expr::MethodCall(crate::ast::MethodCallExpr {
            target: Some(Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                name: iterator_var.clone(),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }))),
            name: "hasNext".to_string(),
            arguments: vec![],
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        }));
        
        // Create loop variable assignment: T item = #iter.next();
        let loop_var_decl = Stmt::Declaration(VarDeclStmt {
            modifiers: vec![],
            type_ref: element_type.clone(),
            variables: vec![VariableDeclarator {
                name: enhanced_for.variable_name.clone(),
                array_dims: 0,
                initializer: Some(Expr::MethodCall(crate::ast::MethodCallExpr {
                    target: Some(Box::new(Expr::Identifier(crate::ast::IdentifierExpr {
                        name: iterator_var.clone(),
                        span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                    }))),
                    name: "next".to_string(),
                    arguments: vec![],
                    span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
                })),
                span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
            }],
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create loop body with variable declaration + original body
        let mut loop_body_stmts = vec![loop_var_decl];
        match enhanced_for.body.as_ref() {
            Stmt::Block(block) => {
                loop_body_stmts.extend(block.statements.clone());
            }
            single_stmt => {
                loop_body_stmts.push(single_stmt.clone());
            }
        }
        
        let loop_body = Stmt::Block(Block {
            statements: loop_body_stmts,
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        // Create the traditional for loop
        let traditional_for = Stmt::For(crate::ast::ForStmt {
            init: vec![iterator_decl],
            condition,
            update: vec![], // No update needed for iterator loop
            body: Box::new(loop_body),
            span: crate::ast::Span {
                        start: crate::ast::Location { line: 0, column: 0, offset: 0 },
                        end: crate::ast::Location { line: 0, column: 0, offset: 0 }
                    },
        });
        
        Ok(traditional_for)
    }
}

/// Types of iterable expressions
#[derive(Debug, Clone)]
enum IterableType {
    Array(TypeRef),
    Iterable(TypeRef),
    Unknown,
}

impl Default for Lower {
    fn default() -> Self {
        Self::new()
    }
}