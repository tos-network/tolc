//! Semantic analysis pipeline - JavaC-aligned compilation phases
//! 
//! This module implements the semantic analysis phases that JavaC uses between
//! parsing and code generation. Each phase corresponds to a JavaC compilation stage:
//! 
//! - Enter: Symbol table construction and import resolution
//! - Attr: Type checking and method resolution 
//! - Flow: Definite assignment and reachability analysis
//! - TransTypes: Generic type erasure and bridge method generation
//! - Lower: Syntactic sugar desugaring (enhanced for loops, string concat, etc.)

pub mod enter;
pub mod attr;
pub mod flow;
pub mod trans_types;
pub mod lower;

use crate::ast::Ast;
use crate::error::Result;

/// Main semantic analysis pipeline that orchestrates all phases
/// Follows JavaC's compilation flow: Enter → Attr → Flow → TransTypes → Lower
pub struct SemanticAnalyzer {
    pub enter: enter::Enter,
    pub attr: attr::Attr,
    pub flow: flow::Flow,
    pub trans_types: trans_types::TransTypes,
    pub lower: lower::Lower,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            enter: enter::Enter::new(),
            attr: attr::Attr::new(),
            flow: flow::Flow::new(),
            trans_types: trans_types::TransTypes::new(),
            lower: lower::Lower::new(),
        }
    }
    
    /// Run complete semantic analysis pipeline on AST
    /// Returns semantically analyzed and transformed AST ready for code generation
    pub fn analyze(&mut self, mut ast: Ast) -> Result<Ast> {
        eprintln!("🔍 WASH: Starting semantic analysis pipeline");
        
        // Phase 1: Enter - Build symbol tables
        ast = self.enter.process(ast)?;
        let symbol_env = self.enter.get_symbol_environment();
        
        // Phase 2: Attr - Type checking and resolution (with symbols)
        ast = self.attr.process_with_symbols(ast, Some(symbol_env))?;
        
        // Phase 3: Flow - Definite assignment analysis (with symbols)
        ast = self.flow.process_with_symbols(ast, Some(symbol_env))?;
        
        // Phase 4: TransTypes - Generic type erasure (with type info)
        ast = self.trans_types.process_with_types(ast, &self.attr.get_type_information())?;
        
        // Phase 5: Lower - Desugar syntax (with type info)
        ast = self.lower.process_with_types(ast, &self.trans_types.get_erased_types())?;
        
        eprintln!("✅ WASH: Semantic analysis pipeline complete");
        Ok(ast)
    }
    
    /// Get generic signatures stored during TransTypes phase
    pub fn get_generic_signatures(&self) -> &std::collections::HashMap<String, String> {
        &self.trans_types.generic_signatures
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}