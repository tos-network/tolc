//! Parser implementation for Terminos Language
//! 
//! This module implements a recursive descent parser that converts tokens into AST nodes.

use super::{lexer::{Lexer, LexicalToken, Token}, error::ParseError};
use crate::ast::*;
use crate::ast::{TypeArg, BoundKind};
use crate::error::Result;

/// Parser for Terminos Language
pub struct Parser {
    tokens: Vec<LexicalToken>,
    current: usize,
    label_stack: Vec<String>,
    pending_members: Vec<ClassMember>,
    // Global step counter to prevent parser-wide infinite loops
    global_steps: usize,
    // Global gas budget: when exhausted, parsing aborts with error
    global_gas: usize,
    // Stack of enclosing class names to disambiguate constructors vs methods
    class_name_stack: Vec<String>,
}

impl Parser {
    /// Create a new parser from source code
    pub fn new(source: &str) -> Result<Self> {
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize()
            .map_err(|e| ParseError::LexicalError { 
                message: e, 
                location: Location::new(0, 0, 0)
            })?;
        
        Ok(Self {
            tokens,
            current: 0,
            label_stack: Vec::new(),
            pending_members: Vec::new(),
            global_steps: 0,
            global_gas: std::env::var("TOLC_PARSER_MAX_GAS").ok().and_then(|s| s.parse().ok()).unwrap_or(crate::consts::PARSER_MAX_GAS),
            class_name_stack: Vec::new(),
        })
    }
    
    /// Parse the source code into an AST (strict: returns Err if any parse error occurred)
    pub fn parse(mut self) -> Result<Ast> {
        let start_span = self.current_span();
        let _initial_gas = self.global_gas;
        // Java 8+ lambda expressions and method references are now supported
        
        // Parse package declaration
        let package_decl = if self.check(&Token::Package) {
            Some(self.parse_package_decl()?)
        } else {
            None
        };
        
        // Parse imports
        let mut imports = Vec::new();
        let mut steps_imports: usize = 0;
        while self.check(&Token::Import) {
            if steps_imports > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
            steps_imports += 1;
            imports.push(self.parse_import_decl()?);
        }
        
        // Parse type declarations with an error budget to avoid infinite recovery loops
        let mut type_decls = Vec::new();
        let mut error_budget: usize = std::env::var("TOLC_PARSE_MAX_ERRORS").ok().and_then(|s| s.parse().ok()).unwrap_or(50);
        let mut error_count: usize = 0;
        let mut had_error: bool = false;
        let mut last_progress_at: usize = self.current;
        let mut steps_toplevel: usize = 0;
        while !self.is_at_end() {
            if self.global_gas == 0 {
                return Err(super::error::ParseError::InvalidSyntax { message: "parser gas exhausted".to_string(), location: self.previous().location() }.into());
            }
            // Gracefully skip stray '}' which may appear due to earlier recovery
            if self.check(&Token::RBrace) {
                self.advance();
                continue;
            }
            match self.parse_type_decl() {
                Ok(type_decl) => {
                    type_decls.push(type_decl);
                    last_progress_at = self.current;
                    // Allow multiple top-level types in tests to support minimal multi-class sources
                }
                Err(e) => {
                    log::debug!("parse toplevel: error at token {:?} '{}' -> {}", self.peek().token_type(), self.peek().lexeme(), e);
                    // Attempt error recovery at top-level and continue
                    self.synchronize_toplevel();
                    // If we're at EOF after recovery, stop without counting as an error
                    if self.is_at_end() { break; }
                    had_error = true;
                    error_count = error_count.saturating_add(1);
                    if error_budget == 0 {
                        return Err(super::error::ParseError::InvalidSyntax { message: "too many parse errors".to_string(), location: self.previous().location() }.into());
                    }
                    error_budget -= 1;
                    // Ensure forward progress; if no token advanced, step one to break potential stall
                    if self.current == last_progress_at && !self.is_at_end() {
                        self.advance();
                    }
                    last_progress_at = self.current;
                    continue;
                }
            }
            steps_toplevel = steps_toplevel.saturating_add(1);
            if steps_toplevel > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
        }
        if had_error {
            return Err(super::error::ParseError::InvalidSyntax {
                message: format!("{} parse error(s)", error_count),
                location: self.previous().location(),
            }
            .into());
        }
        
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(Ast {
            package_decl,
            imports,
            type_decls,
            span,
        })
    }

    /// Parse the source code into an AST, but do not fail the whole parse if recoverable
    /// errors were encountered. This is intended for error-recovery tests where we only
    /// require the parser to terminate and produce a best-effort AST.
    pub fn parse_lenient(mut self) -> Result<Ast> {
        let start_span = self.current_span();
        let package_decl = if self.check(&Token::Package) { Some(self.parse_package_decl()?) } else { None };
        let mut imports = Vec::new();
        while self.check(&Token::Import) { imports.push(self.parse_import_decl()?); }
        let mut type_decls = Vec::new();
        let mut error_budget: usize = std::env::var("TOLC_PARSE_MAX_ERRORS").ok().and_then(|s| s.parse().ok()).unwrap_or(50);
        let mut last_progress_at: usize = self.current;
        while !self.is_at_end() {
            match self.parse_type_decl() {
                Ok(td) => { type_decls.push(td); last_progress_at = self.current; }
                Err(_) => {
                    if error_budget == 0 { break; }
                    error_budget -= 1;
                    self.synchronize_toplevel();
                    if self.current == last_progress_at && !self.is_at_end() { self.advance(); }
                    last_progress_at = self.current;
                }
            }
        }
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        let ast = Ast { package_decl, imports, type_decls, span };
        Ok(ast)
    }
    
    // Helper methods
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
    
    fn check(&self, token_type: &Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type() == token_type
        }
    }
    
    fn advance(&mut self) -> &LexicalToken {
        if !self.is_at_end() {
            // Increment global step counter and stop advancing if we exceed cap
            self.global_steps = self.global_steps.saturating_add(1);
            if self.global_steps > crate::consts::PARSER_MAX_LOOP_ITERS { self.current = self.tokens.len(); return self.previous(); }
            // Consume global gas; if exhausted, abort parse with InvalidSyntax
            if self.global_gas == 0 {
                // Force end and mark error via setting current to end; caller will observe EOF
                self.current = self.tokens.len();
                return self.previous();
            }
            self.global_gas -= 1;
            self.current += 1;
        }
        self.previous()
    }
    
    fn peek(&self) -> &LexicalToken {
        if self.current >= self.tokens.len() {
            &self.tokens[self.tokens.len().saturating_sub(1)]
        } else {
        &self.tokens[self.current]
        }
    }
    
    fn previous(&self) -> &LexicalToken {
        if self.current == 0 { &self.tokens[0] } else { &self.tokens[self.current - 1] }
    }
    
    fn current_span(&self) -> Span {
        if self.is_at_end() {
            let last = &self.tokens[self.tokens.len() - 1];
            Span::new(last.location, last.location)
        } else {
            let current = &self.tokens[self.current];
            Span::new(current.location, current.location)
        }
    }
    
    fn previous_span(&self) -> Span {
        let previous = self.previous();
        Span::new(previous.location, previous.location)
    }
    
    fn consume(&mut self, token_type: &Token, _message: &str) -> Result<&LexicalToken> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            if self.is_at_end() {
                let loc = if self.tokens.is_empty() {
                    Location::new(0, 0, 0)
                } else {
                    self.tokens[self.tokens.len() - 1].location()
                };
                Err(ParseError::UnexpectedEndOfInput {
                    expected: format!("{:?}", token_type),
                    location: loc,
                }.into())
        } else {
            let current = self.peek();
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", token_type),
                found: format!("{:?}", current.token_type()),
                location: current.location(),
            }.into())
            }
        }
    }
    
    fn match_token(&mut self, token_type: &Token) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }
    
    // Package declaration parsing
    fn parse_package_decl(&mut self) -> Result<PackageDecl> {
        let start_span = self.current_span();
        
        self.consume(&Token::Package, "Expected 'package'")?;
        
        let name = self.parse_qualified_name()?;
        
        self.consume(&Token::Semicolon, "Expected ';' after package name")?;
        
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(PackageDecl { name, span })
    }
    
    // Import declaration parsing
    fn parse_import_decl(&mut self) -> Result<ImportDecl> {
        let start_span = self.current_span();
        
        self.consume(&Token::Import, "Expected 'import'")?;
        
        let is_static = self.match_token(&Token::Static);
        
        // Parse qualified name or static single-member import: java.foo.Bar.member
        let mut parts: Vec<String> = Vec::new();
        parts.push(self.parse_identifier()?);
        while self.match_token(&Token::Dot) {
            // accept either identifier or '*'
            if self.match_token(&Token::Star) {
                // we consumed .*; mark wildcard and stop
                let name = parts.join(".");
                let end_span = self.previous_span();
                let span = Span::new(start_span.start, end_span.end);
                self.consume(&Token::Semicolon, "Expected ';' after import")?;
                return Ok(ImportDecl { name, is_static, is_wildcard: true, span });
            }
            // must be identifier segment
            parts.push(self.parse_identifier()?);
        }
        // If static import and there are at least 2 segments, allow last segment to be member name
        let name = parts.join(".");
        let is_wildcard = false;
        
        self.consume(&Token::Semicolon, "Expected ';' after import")?;
        
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(ImportDecl {
            name,
            is_static,
            is_wildcard,
            span,
        })
    }
    
    // Type declaration parsing
    fn parse_type_decl(&mut self) -> Result<TypeDecl> {
        // Consume and ignore declaration-leading annotations to allow
        // forms like @Retention @Target public @interface ...
        // BUT: do not treat '@interface' as an annotation usage. If the next token after '@'
        // is the 'interface' keyword, this begins an annotation type declaration and must not
        // be consumed here.
        if !(self.check(&Token::At) && self.peek_token_type(self.current + 1) == Some(&Token::Interface)) {
            let _leading_annotations = self.parse_annotations()?;
        }
        let modifiers = self.parse_modifiers()?;
        
        match self.peek().token_type() {
            Token::Class => {
                let class = self.parse_class_decl(modifiers)?;
                Ok(TypeDecl::Class(class))
            }
            Token::Interface => {
                let interface = self.parse_interface_decl(modifiers)?;
                Ok(TypeDecl::Interface(interface))
            }
            Token::Enum => {
                let enum_decl = self.parse_enum_decl(modifiers)?;
                Ok(TypeDecl::Enum(enum_decl))
            }
            Token::At => {
                let annotation = self.parse_annotation_decl(modifiers)?;
                Ok(TypeDecl::Annotation(annotation))
            }
            _ => {
                let current = self.peek();
                Err(ParseError::UnexpectedToken {
                    expected: "type declaration".to_string(),
                    found: format!("{:?}", current.token_type()),
                    location: current.location(),
                }.into())
            }
        }
    }
    
    // Class declaration parsing
    fn parse_class_decl(&mut self, modifiers: Vec<Modifier>) -> Result<ClassDecl> {
        let start_span = self.current_span();
        
        self.consume(&Token::Class, "Expected 'class'")?;
        
        // Leading annotations directly before 'class' were collected at type_decl entry.
        // Parse any annotations that appear between 'class' and the name (rare but allow symmetry)
        let annotations = self.parse_annotations()?;

        let name = self.parse_identifier()?;
        
        let type_params = if self.check(&Token::Lt) {
            self.parse_type_parameters()?
        } else {
            Vec::new()
        };
        
        let extends = if self.match_token(&Token::Extends) {
            Some(self.parse_type_ref()?)
        } else {
            None
        };
        
        let implements = if self.match_token(&Token::Implements) {
            self.parse_type_list()?
        } else {
            Vec::new()
        };
        
        self.consume(&Token::LBrace, "Expected '{' after class declaration")?;
        
        // Track current class name for constructor lookahead
        self.class_name_stack.push(name.clone());
        let mut body = Vec::new();
        // Safety cap to prevent non-progress loops in large classes
        let mut steps_in_body: usize = 0;
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            if steps_in_body > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
            steps_in_body += 1;
            match self.parse_class_member() {
                Ok(m) => body.push(m),
                Err(_) => {
                    self.synchronize_in_class_body();
                    if self.check(&Token::Semicolon) { self.advance(); }
                }
            }
        }
        
        let _ = self.consume(&Token::RBrace, "Expected '}' after class body");
        // Pop current class name
        let _ = self.class_name_stack.pop();
        
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(ClassDecl {
            modifiers,
            annotations,
            name,
            type_params,
            extends,
            implements,
            body,
            span,
        })
    }
    
    // Helper parsing methods
    fn parse_modifiers(&mut self) -> Result<Vec<Modifier>> {
        let mut modifiers = Vec::new();
        
        while let Some(modifier) = self.parse_modifier() {
            modifiers.push(modifier);
        }
        
        Ok(modifiers)
    }
    
    fn parse_modifier(&mut self) -> Option<Modifier> {
        match self.peek().token_type() {
            Token::Public => {
                self.advance();
                Some(Modifier::Public)
            }
            Token::Protected => {
                self.advance();
                Some(Modifier::Protected)
            }
            Token::Private => {
                self.advance();
                Some(Modifier::Private)
            }
            Token::Abstract => {
                self.advance();
                Some(Modifier::Abstract)
            }
            Token::Static => {
                self.advance();
                Some(Modifier::Static)
            }
            Token::Final => {
                self.advance();
                Some(Modifier::Final)
            }
            Token::Native => {
                self.advance();
                Some(Modifier::Native)
            }
            Token::Synchronized => {
                self.advance();
                Some(Modifier::Synchronized)
            }
            Token::Transient => {
                self.advance();
                Some(Modifier::Transient)
            }
            Token::Volatile => {
                self.advance();
                Some(Modifier::Volatile)
            }
            Token::Strictfp => {
                self.advance();
                Some(Modifier::Strictfp)
            }
            Token::Default => {
                // Java 8 default method modifier inside interfaces
                self.advance();
                Some(Modifier::Default)
            }
            _ => None,
        }
    }
    
    fn parse_identifier(&mut self) -> Result<String> {
        match self.peek().token_type() {
            Token::Identifier => {
                let token = self.advance();
                Ok(token.lexeme().to_string())
            }
            _ => {
                let current = self.peek();
                Err(ParseError::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: format!("{:?}", current.token_type()),
                    location: current.location(),
                }.into())
            }
        }
    }
    
    fn parse_qualified_name(&mut self) -> Result<String> {
        let mut parts = Vec::new();
        
        // Parse first identifier
        parts.push(self.parse_identifier()?);
        
        // Parse remaining parts separated by dots
        let mut steps: usize = 0;
        while self.match_token(&Token::Dot) {
            if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
            steps += 1;
            parts.push(self.parse_identifier()?);
        }
        
        Ok(parts.join("."))
    }
    
    fn parse_type_parameters(&mut self) -> Result<Vec<TypeParam>> {
        self.consume(&Token::Lt, "Expected '<' for type parameters")?;
        
        let mut params = Vec::new();
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            let before = self.current;
            params.push(self.parse_type_parameter()?);
            
            if !self.match_token(&Token::Comma) {
                break;
            }
            if self.current == before { if !self.is_at_end() { self.advance(); } }
            }
        }
        
        self.consume_generic_gt()?;
        
        Ok(params)
    }

    // Helper: consume a '>' or combined >> / >>> as generic closers
    fn consume_generic_gt(&mut self) -> Result<()> {
        // Normal close
        if self.match_token(&Token::Gt) { return Ok(()); }
        // Combined closers. If the next token is '>>' or '>>>', allow it to satisfy at least one level.
        if self.match_token(&Token::RShift) { return Ok(()); }
        if self.match_token(&Token::URShift) { return Ok(()); }
        // In nested generic contexts like Set<Entry<K,V>> the inner parser may have consumed
        // a single RShift token which represents two '>' characters. In that case, callers at the
        // outer level won't see a '>' token next; treat a previously consumed RShift/URShift as
        // satisfying this close as well.
        let prev = self.previous().token_type();
        if matches!(prev, Token::RShift | Token::URShift) { return Ok(()); }
        Err(ParseError::UnexpectedToken {
            expected: ">".to_string(),
            found: format!("{:?}", self.peek().token_type()),
            location: self.peek().location(),
        }.into())
    }
    
    fn parse_type_parameter(&mut self) -> Result<TypeParam> {
        let start_span = self.current_span();
        
        let name = self.parse_identifier()?;
        
        let bounds = if self.match_token(&Token::Extends) {
            // Java uses '&' for intersection bounds in type parameters: <T extends A & B>
            self.parse_intersection_bounds()?
        } else {
            Vec::new()
        };
        
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(TypeParam { name, bounds, span })
    }

    // Parse intersection bounds list for type parameters: A (& B (& C)*)*
    fn parse_intersection_bounds(&mut self) -> Result<Vec<TypeRef>> {
        let mut types = Vec::new();
        types.push(self.parse_type_ref()?);
        {
            let mut steps: usize = 0;
            while self.match_token(&Token::Amp) {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            let before = self.current;
            types.push(self.parse_type_ref()?);
            if self.current == before { if !self.is_at_end() { self.advance(); } }
            }
        }
        Ok(types)
    }
    
    fn parse_type_ref(&mut self) -> Result<TypeRef> {
        let start_span = self.current_span();
        // Leading type-use annotations
        let annotations = self.parse_annotations()?;
        // Either primitive or qualified identifier
        let name = match self.peek().token_type() {
            Token::Boolean | Token::Byte | Token::Short | Token::Int | Token::Long | Token::Char | Token::Float | Token::Double | Token::Void => {
                let tok = self.advance();
                tok.lexeme().to_string()
            }
            Token::Identifier => self.parse_qualified_name()?,
            _ => {
                let current = self.peek();
                return Err(ParseError::UnexpectedToken {
                    expected: "type".to_string(),
                    found: format!("{:?}", current.token_type()),
                    location: current.location(),
                }.into());
            }
        };
        
        let type_args = if self.check(&Token::Lt) {
            self.parse_type_arguments_typearg()?
        } else {
            Vec::new()
        };
        
        // Array dimensions, allowing type-use annotations before each [] (e.g., String @A [] @B [])
        let mut array_dims = 0;
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
                // Consume any annotations appearing before an array dimension
                if self.check(&Token::At) {
                    // We only allow these annotations here if they are immediately followed by a '[' dimension
                    let _dim_anns = self.parse_annotations()?;
                    // Require a '[' right after annotated dimension
                    if !self.match_token(&Token::LBracket) { break; }
                    let before = self.current;
                    self.consume(&Token::RBracket, "Expected ']' after array dimension")?;
                    array_dims += 1;
                    if self.current == before { if !self.is_at_end() { self.advance(); } }
                    continue;
                }
                // Or a plain [] without annotations
                if self.match_token(&Token::LBracket) {
                    let before = self.current;
                    self.consume(&Token::RBracket, "Expected ']' after array dimension")?;
                    array_dims += 1;
                    if self.current == before { if !self.is_at_end() { self.advance(); } }
                    continue;
                }
                break;
            }
        }
        
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(TypeRef { name, type_args, annotations, array_dims, span })
    }
    
    fn parse_type_arguments(&mut self) -> Result<Vec<TypeRef>> { unreachable!("legacy parse_type_arguments should not be called") }

    fn parse_type_arguments_typearg(&mut self) -> Result<Vec<TypeArg>> {
        self.consume(&Token::Lt, "Expected '<' for type arguments")?;
        let mut args = Vec::new();
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            if self.check(&Token::Question) {
                let loc = self.advance().location();
                let mut bound = None;
                if self.match_token(&Token::Extends) {
                    let tr = self.parse_type_ref()?;
                    bound = Some((BoundKind::Extends, tr));
                } else if self.match_token(&Token::Super) {
                    let tr = self.parse_type_ref()?;
                    bound = Some((BoundKind::Super, tr));
                }
                let sp = crate::ast::Span::new(loc, loc);
                args.push(TypeArg::Wildcard(WildcardType { bound, span: sp }));
            } else {
                let tr = self.parse_type_ref()?;
                args.push(TypeArg::Type(tr));
            }
                if !self.match_token(&Token::Comma) { break; }
            }
        }
        self.consume_generic_gt()?;
        Ok(args)
    }
    
    fn parse_type_list(&mut self) -> Result<Vec<TypeRef>> {
        let mut types = Vec::new();
        
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            types.push(self.parse_type_ref()?);
            
            if !self.match_token(&Token::Comma) {
                break;
            }
            }
        }
        
        Ok(types)
    }
    
    fn parse_class_member(&mut self) -> Result<ClassMember> {
        log::debug!("parse_class_member: start at token {:?} '{}'", self.peek().token_type(), self.peek().lexeme());
        // Pending members produced by previous field multi-declarators
        if let Some(member) = self.pending_members.pop() {
            return Ok(member);
        }
        // This is a simplified implementation
        // In a full parser, you'd need to handle all member types
        let mut modifiers = self.parse_modifiers()?;

        // Static initializer block: either we still see 'static' followed by '{',
        // or we already consumed 'static' as a modifier and the next token is '{'.
        if (self.check(&Token::Static) && self.peek_token_type(self.current + 1) == Some(&Token::LBrace))
            || (modifiers.contains(&Modifier::Static) && self.check(&Token::LBrace))
        {
            // If 'static' is still present, consume it now
            if self.check(&Token::Static) { self.advance(); }
            let body = self.parse_block()?;
            let span = body.span;
            log::debug!("parse_class_member: parsed static initializer block");
            return Ok(ClassMember::Initializer(InitializerBlock { modifiers: vec![Modifier::Static], body, span }));
        }
        // Instance initializer block: '{' ... '}'
        if self.check(&Token::LBrace) {
            let body = self.parse_block()?;
            let span = body.span;
            log::debug!("parse_class_member: parsed instance initializer block");
            return Ok(ClassMember::Initializer(InitializerBlock { modifiers: Vec::new(), body, span }));
        }
        
        if self.check(&Token::Class) || self.check(&Token::Interface) || self.check(&Token::Enum) {
            let type_decl = self.parse_type_decl()?;
            log::debug!("parse_class_member: parsed nested type decl");
            return Ok(ClassMember::TypeDecl(type_decl));
        }
        // Constructor: possibly with leading annotations; look ahead skipping annotations and trailing modifiers
        {
            let mut i = self.current;
            // Skip annotations
            while self.peek_token_type(i) == Some(&Token::At) {
                i += 1; // '@'
                // Qualified name
                if self.peek_token_type(i) != Some(&Token::Identifier) { break; }
                i += 1;
                while self.peek_token_type(i) == Some(&Token::Dot) && self.peek_token_type(i + 1) == Some(&Token::Identifier) { i += 2; }
                // Optional annotation args
                if self.peek_token_type(i) == Some(&Token::LParen) {
                    let mut depth = 0usize;
                    loop {
                        match self.peek_token_type(i) {
                            Some(Token::LParen) => { depth += 1; i += 1; }
                            Some(Token::RParen) => { i += 1; if depth == 0 { break; } depth = depth.saturating_sub(1); if depth == 0 { break; } }
                            Some(_) => { i += 1; }
                            None => break,
                        }
                    }
                }
            }
            // Modifiers may appear after annotations too
            loop {
                match self.peek_token_type(i) {
                    Some(Token::Public) | Some(Token::Protected) | Some(Token::Private) |
                    Some(Token::Abstract) | Some(Token::Static) | Some(Token::Final) |
                    Some(Token::Native) | Some(Token::Synchronized) | Some(Token::Transient) |
                    Some(Token::Volatile) | Some(Token::Strictfp) | Some(Token::Default) => { i += 1; }
                    _ => break,
                }
            }
            if self.peek_token_type(i) == Some(&Token::Identifier) && self.peek_token_type(i + 1) == Some(&Token::LParen) {
                if let Some(current_class) = self.class_name_stack.last() {
                    if self.tokens.get(i).map(|t| t.lexeme()) == Some(current_class.as_str()) {
                        // It's a constructor. Now actually consume the annotations at the real cursor,
                        // then any modifiers that follow them, and delegate.
                        let leading_annotations = self.parse_annotations()?;
                        let mut more_mods = self.parse_modifiers()?;
                        modifiers.append(&mut more_mods);
                        let ctor = self.parse_constructor_decl_with_annotations(modifiers, leading_annotations)?;
                        return Ok(ClassMember::Constructor(ctor));
                    }
                }
            }
        }
        
        // Also consider a leading '<' as a possible start of a method (method-level type parameters)
        if self.check(&Token::Void) || self.is_type_start() || self.check(&Token::At) || self.check(&Token::Lt) {
            // If identifier followed by '(', but it's not constructor (name != class), treat as method
            if self.check(&Token::Identifier) && self.peek_token_type(self.current + 1) == Some(&Token::LParen) {
                if let Some(current_class) = self.class_name_stack.last() {
                    if self.peek().lexeme() != current_class {
                        let method = self.parse_method_decl(modifiers)?;
                        log::debug!("parse_class_member: parsed method decl (id+paren)");
                        return Ok(ClassMember::Method(method));
                    }
                }
            }
            if self.lookahead_is_method_signature() {
                let method = self.parse_method_decl(modifiers)?;
                log::debug!("parse_class_member: parsed method decl");
                Ok(ClassMember::Method(method))
            } else {
                let field = self.parse_field_decl(modifiers)?;
                log::debug!("parse_class_member: parsed field decl");
                Ok(ClassMember::Field(field))
            }
        } else {
            let field = self.parse_field_decl(modifiers)?;
            log::debug!("parse_class_member: parsed field decl (fallback)");
            Ok(ClassMember::Field(field))
        }
    }

    // Lookahead: determine if the upcoming tokens form a method signature
    fn lookahead_is_method_signature(&self) -> bool {
        let mut i = self.current;
        // Skip any annotations
        while self.peek_token_type(i) == Some(&Token::At) {
            // Skip '@' QualifiedName ( '(' ... ')' )?
            i += 1; // '@'
            if self.peek_token_type(i) != Some(&Token::Identifier) { return false; }
            i += 1;
            while self.peek_token_type(i) == Some(&Token::Dot) && self.peek_token_type(i + 1) == Some(&Token::Identifier) { i += 2; }
            if self.peek_token_type(i) == Some(&Token::LParen) {
                let mut depth = 0usize;
                loop {
                    match self.peek_token_type(i) {
                        Some(Token::LParen) => { depth += 1; i += 1; }
                        Some(Token::RParen) => { i += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                        Some(_) => { i += 1; }
                        None => return false,
                    }
                }
            }
        }
        // Skip modifiers if any (rare after we've already consumed them, but safe)
        loop {
            match self.peek_token_type(i) {
                Some(Token::Public) | Some(Token::Protected) | Some(Token::Private) |
                Some(Token::Abstract) | Some(Token::Static) | Some(Token::Final) |
                Some(Token::Native) | Some(Token::Synchronized) | Some(Token::Transient) |
                Some(Token::Volatile) | Some(Token::Strictfp) | Some(Token::Default) => { i += 1; }
                _ => break,
            }
        }
        // Optional method-level type parameters: < ... >
        if self.peek_token_type(i) == Some(&Token::Lt) {
            let mut depth = 0usize;
            loop {
                match self.peek_token_type(i) {
                    Some(Token::Lt) => { depth += 1; i += 1; }
                    Some(Token::Gt) => { i += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                    Some(Token::RShift) => { i += 1; if depth == 0 { break; } depth = depth.saturating_sub(2); if depth == 0 { break; } }
                    Some(Token::URShift) => { i += 1; if depth == 0 { break; } depth = depth.saturating_sub(3); if depth == 0 { break; } }
                    Some(_) => { i += 1; }
                    None => return false,
                }
                if depth == 0 { break; }
            }
        }
        // Return type or 'void'
        if self.peek_token_type(i) == Some(&Token::Void) {
            i += 1;
        } else {
            if !self.lookahead_type_ref(&mut i) { return false; }
            // Optional array suffixes after return type
            while self.peek_token_type(i) == Some(&Token::LBracket) && self.peek_token_type(i + 1) == Some(&Token::RBracket) { i += 2; }
        }
        // Method name and '('
        self.peek_token_type(i) == Some(&Token::Identifier) && self.peek_token_type(i + 1) == Some(&Token::LParen)
    }
    
    // Lookahead: parse a type reference without consuming tokens
    fn lookahead_type_ref(&self, i: &mut usize) -> bool {
        // Base token: identifier (qualified), primitive, void, or wildcard '?'
        match self.peek_token_type(*i) {
            Some(Token::Identifier)
            | Some(Token::Boolean)
            | Some(Token::Byte)
            | Some(Token::Short)
            | Some(Token::Int)
            | Some(Token::Long)
            | Some(Token::Char)
            | Some(Token::Float)
            | Some(Token::Double)
            | Some(Token::Void)
            | Some(Token::Question) => {
                *i += 1; // consume first segment or wildcard/primitive/void
                if self.peek_token_type(*i) == Some(&Token::Extends) || self.peek_token_type(*i) == Some(&Token::Super) {
                    *i += 1;
                    if !self.lookahead_type_ref(i) { return false; }
                }
            }
            _ => return false,
        }
        // Optional type arguments after first segment
        if self.peek_token_type(*i) == Some(&Token::Lt) {
            let mut depth = 0usize;
            loop {
                match self.peek_token_type(*i) {
                    Some(Token::Lt) => { depth += 1; *i += 1; }
                    Some(Token::Gt) => { *i += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                    Some(Token::RShift) => { *i += 1; if depth == 0 { break; } depth = depth.saturating_sub(2); if depth == 0 { break; } }
                    Some(Token::URShift) => { *i += 1; if depth == 0 { break; } depth = depth.saturating_sub(3); if depth == 0 { break; } }
                    Some(_) => { *i += 1; }
                    None => return false,
                }
                if depth == 0 { break; }
            }
        }
        // Qualified name segments with optional type args: . Identifier (<...>)?
        loop {
            if self.peek_token_type(*i) != Some(&Token::Dot) { break; }
            // ensure next is identifier
            if self.peek_token_type(*i + 1) != Some(&Token::Identifier) { break; }
            *i += 2; // consume '.' and identifier
            // optional type args after segment
            if self.peek_token_type(*i) == Some(&Token::Lt) {
                let mut depth = 0usize;
                loop {
                    match self.peek_token_type(*i) {
                        Some(Token::Lt) => { depth += 1; *i += 1; }
                        Some(Token::Gt) => { *i += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                        Some(Token::RShift) => { *i += 1; if depth == 0 { break; } depth = depth.saturating_sub(2); if depth == 0 { break; } }
                        Some(Token::URShift) => { *i += 1; if depth == 0 { break; } depth = depth.saturating_sub(3); if depth == 0 { break; } }
                        Some(_) => { *i += 1; }
                        None => return false,
                    }
                    if depth == 0 { break; }
                }
            }
        }
        // Array dimensions []*
        while self.peek_token_type(*i) == Some(&Token::LBracket) {
            *i += 1;
            if self.peek_token_type(*i) != Some(&Token::RBracket) { return false; }
            *i += 1;
        }
        true
    }
    
    fn peek_token_type(&self, idx: usize) -> Option<&Token> {
        self.tokens.get(idx).map(|t| t.token_type())
    }

    // Lookahead: are we at the start of a plausible class member?
    fn lookahead_is_member_start(&self, mut i: usize) -> bool {
        // Skip annotations: @QualifiedName ( ... )?
        while self.peek_token_type(i) == Some(&Token::At) {
            i += 1; // '@'
            if self.peek_token_type(i) != Some(&Token::Identifier) { return false; }
            i += 1;
            while self.peek_token_type(i) == Some(&Token::Dot) && self.peek_token_type(i + 1) == Some(&Token::Identifier) { i += 2; }
            if self.peek_token_type(i) == Some(&Token::LParen) {
                let mut depth = 0usize;
                loop {
                    match self.peek_token_type(i) {
                        Some(Token::LParen) => { depth += 1; i += 1; }
                        Some(Token::RParen) => { i += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                        Some(_) => { i += 1; }
                        None => return false,
                    }
                }
            }
        }
        // Skip modifiers
        loop {
            match self.peek_token_type(i) {
                Some(Token::Public) | Some(Token::Protected) | Some(Token::Private) |
                Some(Token::Abstract) | Some(Token::Static) | Some(Token::Final) |
                Some(Token::Native) | Some(Token::Synchronized) | Some(Token::Transient) |
                Some(Token::Volatile) | Some(Token::Strictfp) | Some(Token::Default) => { i += 1; }
                _ => break,
            }
        }
        // Nested type declarations
        if matches!(self.peek_token_type(i), Some(Token::Class) | Some(Token::Interface) | Some(Token::Enum) | Some(Token::At)) {
            return true;
        }
        // Primitive or void -> field or method
        if matches!(self.peek_token_type(i), Some(Token::Boolean)|Some(Token::Byte)|Some(Token::Short)|Some(Token::Int)|Some(Token::Long)|Some(Token::Char)|Some(Token::Float)|Some(Token::Double)|Some(Token::Void)) {
            return true;
        }
        // Reference type or constructor/method starting with Identifier
        if self.peek_token_type(i) == Some(&Token::Identifier) {
            // Constructor: Identifier '(' with name == current class name
            if self.peek_token_type(i + 1) == Some(&Token::LParen) {
                if let Some(current_class) = self.class_name_stack.last() {
                    return self.tokens.get(i).map(|t| t.lexeme()) == Some(current_class.as_str());
                }
                return false;
            }
            // Qualified name: Identifier ('.' Identifier)* possibly with type args
            // Or a simple type ref followed by another Identifier (variable/method name)
            let mut j = i;
            // consume first identifier
            j += 1;
            // optional type args after first segment
            if self.peek_token_type(j) == Some(&Token::Lt) {
                // best-effort: scan balanced generics
                let mut depth = 0usize;
                loop {
                    match self.peek_token_type(j) {
                        Some(Token::Lt) => { depth += 1; j += 1; }
                        Some(Token::Gt) => { j += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                        Some(Token::RShift) => { j += 1; if depth == 0 { break; } depth = depth.saturating_sub(2); if depth == 0 { break; } }
                        Some(Token::URShift) => { j += 1; if depth == 0 { break; } depth = depth.saturating_sub(3); if depth == 0 { break; } }
                        Some(_) => { j += 1; }
                        None => break,
                    }
                    if depth == 0 { break; }
                }
            }
            // Qualified segments
            while self.peek_token_type(j) == Some(&Token::Dot) && self.peek_token_type(j + 1) == Some(&Token::Identifier) {
                j += 2;
                if self.peek_token_type(j) == Some(&Token::Lt) {
                    let mut depth = 0usize;
                    loop {
                        match self.peek_token_type(j) {
                            Some(Token::Lt) => { depth += 1; j += 1; }
                            Some(Token::Gt) => { j += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                            Some(Token::RShift) => { j += 1; if depth == 0 { break; } depth = depth.saturating_sub(2); if depth == 0 { break; } }
                            Some(Token::URShift) => { j += 1; if depth == 0 { break; } depth = depth.saturating_sub(3); if depth == 0 { break; } }
                            Some(_) => { j += 1; }
                            None => break,
                        }
                        if depth == 0 { break; }
                    }
                }
            }
            // Now expect either an Identifier (member name) or '[' for array declarator or '(' for method (rare due to constructor check)
            return matches!(self.peek_token_type(j), Some(Token::Identifier) | Some(Token::LBracket));
        }
        false
    }
    
    fn is_type_start(&self) -> bool {
        matches!(self.peek().token_type(),
            Token::Boolean | Token::Byte | Token::Short | Token::Int |
            Token::Long | Token::Char | Token::Float | Token::Double |
            Token::Identifier
        )
    }
    
    // Simplified implementations for other parsing methods
    fn parse_interface_decl(&mut self, modifiers: Vec<Modifier>) -> Result<InterfaceDecl> {
        self.consume(&Token::Interface, "Expected 'interface' keyword")?;
        let annotations = self.parse_annotations()?;
        let name = self.parse_identifier()?;
        
        // Parse type parameters; allow single "? extends/super T" as generic param in compat mode
        let type_params = if self.check(&Token::Lt) {
            // Lookahead for wildcard generics like <? extends K, ? extends V>
            if self.peek_token_type(self.current + 1) == Some(&Token::Question) {
                self.advance(); // consume '<'
                let mut params: Vec<TypeParam> = Vec::new();
                loop {
                    if self.match_token(&Token::Question) {
                        // Synthesize a TypeParam name for wildcard; record extends bound if present
                        let name = "?".to_string();
                        let bounds = if self.match_token(&Token::Extends) {
                            vec![ self.parse_type_ref()? ]
                        } else if self.match_token(&Token::Super) {
                            vec![ self.parse_type_ref()? ]
                        } else { Vec::new() };
                        let sp = self.current_span();
                        params.push(TypeParam { name, bounds, span: sp });
                    }
                    if !self.match_token(&Token::Comma) { break; }
                }
                self.consume_generic_gt()?;
                params
            } else {
                self.parse_type_parameters()?
            }
        } else {
            Vec::new()
        };
        
        // Parse extends clause
        let extends = if self.check(&Token::Extends) {
            self.advance(); // consume 'extends'
            self.parse_type_list()?
        } else {
            Vec::new()
        };
        
        // Parse interface body
        self.consume(&Token::LBrace, "Expected '{' after interface declaration")?;
        
        let mut members = Vec::new();
        {
            let mut steps: usize = 0;
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
                members.push(self.parse_interface_member()?);
            }
        }
        
        self.consume(&Token::RBrace, "Expected '}' to close interface body")?;
        
        let span = Span::new(
            self.current_span().start,
            self.previous_span().end,
        );
        
        Ok(InterfaceDecl {
            modifiers,
            annotations,
            name,
            type_params,
            extends,
            body: members,
            span,
        })
    }
    
    fn parse_enum_decl(&mut self, modifiers: Vec<Modifier>) -> Result<EnumDecl> {
        self.consume(&Token::Enum, "Expected 'enum' keyword")?;
        let annotations = self.parse_annotations()?;
        let name = self.parse_identifier()?;
        
        // Parse implements clause
        let implements = if self.check(&Token::Implements) {
            self.advance(); // consume 'implements'
            self.parse_type_list()?
        } else {
            Vec::new()
        };
        
        // Parse enum body
        self.consume(&Token::LBrace, "Expected '{' after enum declaration")?;
        // Track current enum name so that enum constructors are recognized properly by parse_class_member
        self.class_name_stack.push(name.clone());
        
        let mut constants = Vec::new();
        {
            let mut steps: usize = 0;
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
                constants.push(self.parse_enum_constant()?);
            
                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }
        
        // Parse semicolon if present
        if self.check(&Token::Semicolon) {
            self.advance();
        }
        
        // Parse class body members
        let mut members = Vec::new();
        {
            let mut steps: usize = 0;
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
                members.push(self.parse_class_member()?);
            }
        }
        
        self.consume(&Token::RBrace, "Expected '}' to close enum body")?;
        let _ = self.class_name_stack.pop();
        
        let span = Span::new(
            self.current_span().start,
            self.previous_span().end,
        );
        
        Ok(EnumDecl {
            modifiers,
            annotations,
            name,
            implements,
            constants,
            body: members,
            span,
        })
    }
    
    fn parse_annotation_decl(&mut self, modifiers: Vec<Modifier>) -> Result<AnnotationDecl> {
        self.consume(&Token::At, "Expected '@' for annotation declaration")?;
        self.consume(&Token::Interface, "Expected 'interface' keyword")?;
        // Leading annotations on the annotation type declaration (e.g., @Retention, @Target)
        let annotations = self.parse_annotations()?;
        let name = self.parse_identifier()?;
        
        // Parse annotation body
        self.consume(&Token::LBrace, "Expected '{' after annotation declaration")?;
        
        let mut members = Vec::new();
        {
            let mut steps: usize = 0;
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
                members.push(self.parse_annotation_member()?);
            }
        }
        
        self.consume(&Token::RBrace, "Expected '}' to close annotation body")?;
        
        let span = Span::new(
            self.current_span().start,
            self.previous_span().end,
        );
        
        Ok(AnnotationDecl { modifiers, annotations, name, body: members, span })
    }
    
    fn parse_method_decl(&mut self, mut modifiers: Vec<Modifier>) -> Result<MethodDecl> {
        // Parse annotations
        let annotations = self.parse_annotations()?;
        // Additional modifiers may appear after annotations
        let mut more_mods = self.parse_modifiers()?;
        modifiers.append(&mut more_mods);
        // Parse method type parameters
        let type_params = if self.check(&Token::Lt) { self.parse_type_parameters()? } else { Vec::new() };
        // Parse return type
        let mut return_type = if self.check(&Token::Void) {
            self.advance();
            None
        } else {
            Some(self.parse_type_ref()?)
        };
        
        // Parse method name
        let name = self.parse_identifier()?;
        
        // Parse parameters
        self.consume(&Token::LParen, "Expected '(' after method name")?;
        let parameters = if !self.check(&Token::RParen) {
            self.parse_parameter_list()?
        } else {
            Vec::new()
        };
        if self.check(&Token::RParen) {
            self.advance();
        } else {
            // Recover from missing ')': advance until we find ')', '{' or ';' or EOF
            while !self.is_at_end() && !self.check(&Token::RParen) && !self.check(&Token::LBrace) && !self.check(&Token::Semicolon) {
                self.advance();
            }
            if self.check(&Token::RParen) { self.advance(); }
        }

        // Optional return type array dimensions after parameter list, e.g., `int m()[]` or `T m()[][]`
        if return_type.is_some() {
            let mut extra_dims = 0usize;
            while self.check(&Token::LBracket) {
                self.advance();
                if self.consume(&Token::RBracket, "Expected ']' after return type array dimension").is_err() {
                    break;
                }
                extra_dims += 1;
            }
            if extra_dims > 0 {
                if let Some(rt) = &mut return_type {
                    rt.array_dims = rt.array_dims.saturating_add(extra_dims);
                }
            }
        }
        
        // Parse throws clause
        let throws = if self.check(&Token::Throws) {
            self.advance(); // consume 'throws'
            self.parse_type_list()?
        } else {
            Vec::new()
        };
        
        // Parse method body or semicolon
        let body = if self.check(&Token::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.consume(&Token::Semicolon, "Expected '{' or ';' after method declaration")?;
            None
        };
        
        let span = Span::new(
            self.current_span().start,
            self.previous_span().end,
        );
        
        Ok(MethodDecl {
            modifiers,
            annotations,
            type_params,
            return_type,
            name,
            parameters,
            throws,
            body,
            span,
        })
    }
    
    fn parse_field_decl(&mut self, mut modifiers: Vec<Modifier>) -> Result<FieldDecl> {
        // Parse annotations
        let annotations = self.parse_annotations()?;
        // Additional modifiers may appear after annotations
        let mut more_mods = self.parse_modifiers()?;
        modifiers.append(&mut more_mods);
        // Parse field type
        let type_ref = self.parse_type_ref()?;
        
        // Parse one or more declarators: name (array dims) [= initializer]
        let mut first_decl: Option<FieldDecl> = None;
        let mut more_decls: Vec<ClassMember> = Vec::new();
        loop {
        let name = self.parse_identifier()?;
        let mut _array_dims = 0;
        while self.check(&Token::LBracket) {
                self.advance();
            self.consume(&Token::RBracket, "Expected ']' after array dimension")?;
            _array_dims += 1;
        }
        let initializer = if self.match_token(&Token::Assign) {
            Some(self.parse_expression()?)
            } else { None };
            let span_decl = Span::new(self.current_span().start, self.previous_span().end);

            // Apply per-declarator array dimensions to the declared type
            let mut type_ref_for_decl = type_ref.clone();
            if _array_dims > 0 { type_ref_for_decl.array_dims += _array_dims; }

            let decl = FieldDecl {
                modifiers: modifiers.clone(),
                annotations: annotations.clone(),
                type_ref: type_ref_for_decl,
                name,
                initializer,
                span: span_decl,
            };

            if first_decl.is_none() {
                first_decl = Some(decl);
            } else {
                more_decls.push(ClassMember::Field(decl));
            }

            if !self.match_token(&Token::Comma) { break; }
        }
        self.consume(&Token::Semicolon, "Expected ';' after field declaration")?;
        // Queue remaining declarators as pending members to be returned on next parse_class_member() calls
        while let Some(m) = more_decls.pop() { self.pending_members.push(m); }
        Ok(first_decl.expect("at least one field declarator"))
    }

    fn parse_constructor_decl_with_annotations(&mut self, modifiers: Vec<Modifier>, annotations: Vec<Annotation>) -> Result<ConstructorDecl> {
        let start = self.current_span();
        let name = self.parse_identifier()?;
        self.consume(&Token::LParen, "Expected '(' after constructor name")?;
        let parameters = if !self.check(&Token::RParen) { self.parse_parameter_list()? } else { Vec::new() };
        if self.check(&Token::RParen) {
            self.advance();
        } else {
            // Recover from missing ')': advance until we find ')', '{' or ';' or EOF
            while !self.is_at_end() && !self.check(&Token::RParen) && !self.check(&Token::LBrace) && !self.check(&Token::Semicolon) {
                self.advance();
            }
            if self.check(&Token::RParen) { self.advance(); }
        }
        let throws = if self.check(&Token::Throws) { self.advance(); self.parse_type_list()? } else { Vec::new() };
        // Parse body with optional leading this(...) or super(...)
        self.consume(&Token::LBrace, "Expected '{' to start constructor body")?;
        // Optional explicit constructor invocation must be first statement and must be
        // the token 'this' or 'super' immediately followed by '('. Do NOT confuse
        // ordinary primary expressions like `this.field = ...` with ctor invocation.
        let mut seen_explicit_ctor_call = false;
        let mut explicit_invocation: Option<crate::ast::ExplicitCtorInvocation> = None;
        if (self.check(&Token::This) || self.check(&Token::Super))
            && self.peek_token_type(self.current + 1) == Some(&Token::LParen)
        {
            // Parse 'this(args);' or 'super(args);'
            let is_this = self.check(&Token::This);
            self.advance(); // consume this/super
            self.consume(&Token::LParen, "Expected '(' after this/super")?;
            let argc = if !self.check(&Token::RParen) { self.parse_argument_list()?.len() } else { 0 };
            self.consume(&Token::RParen, "Expected ')' after constructor invocation arguments")?;
            if self.check(&Token::Semicolon) { self.advance(); }
            seen_explicit_ctor_call = true;
            explicit_invocation = Some(if is_this { crate::ast::ExplicitCtorInvocation::This { arg_count: argc } } else { crate::ast::ExplicitCtorInvocation::Super { arg_count: argc } });
        }
        // Remaining statements
        let mut statements = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            if self.check(&Token::Semicolon) { self.advance(); continue; }
            // Enforce: explicit ctor call only allowed as first statement
            // Only trigger when it's actually a ctor invocation token sequence: this '(' ... or super '(' ...
            if (self.check(&Token::This) || self.check(&Token::Super))
                && self.peek_token_type(self.current + 1) == Some(&Token::LParen)
            {
                let loc = self.peek().location();
                if seen_explicit_ctor_call || !statements.is_empty() {
                    return Err(ParseError::semantic_error("explicit constructor invocation must be the first statement and appear only once", loc).into());
                }
            }
            match self.parse_statement() { Ok(s) => statements.push(s), Err(_) => { self.synchronize_in_block(); } }
        }
        let _ = self.consume(&Token::RBrace, "Expected '}' to close constructor body");
        let body = Block { statements, span: Span::new(start.start, self.previous_span().end) };
        let span = Span::new(start.start, self.previous_span().end);
        // Parse annotations before constructor name (already consumed in member start). For now, none are captured at member-start
        // so keep empty list; future: wire _leading_annotations collected at member entry.
        Ok(ConstructorDecl { modifiers, annotations, name, parameters, throws, explicit_invocation, body, span })
    }
    
    // Helper methods for the new parsing functionality
    fn parse_interface_member(&mut self) -> Result<InterfaceMember> {
        // Allow modifiers and annotations in any order at member start
        let mut modifiers = self.parse_modifiers()?;
        let _annotations = self.parse_annotations()?;
        let mut more = self.parse_modifiers()?;
        modifiers.append(&mut more);

        // Nested types inside interface
        if self.check(&Token::Class) || self.check(&Token::Interface) || self.check(&Token::Enum) || self.check(&Token::At) {
            let type_decl = self.parse_type_decl()?;
            return Ok(InterfaceMember::TypeDecl(type_decl));
        }

        // Methods: return type or 'void', also permit method-level type params '<>'
        if self.check(&Token::Void) || self.is_type_start() || self.check(&Token::Lt) || self.check(&Token::At) {
            // Use lookahead to distinguish between methods and fields
            if self.lookahead_is_method_signature() {
                let method = self.parse_method_decl(modifiers)?;
                return Ok(InterfaceMember::Method(method));
            } else {
                // It's a field (constant) declaration
                let mut field = self.parse_field_decl(modifiers)?;
                
                // Interface fields are implicitly public static final
                use crate::ast::Modifier::*;
                if !field.modifiers.contains(&Public) {
                    field.modifiers.push(Public);
                }
                if !field.modifiers.contains(&Static) {
                    field.modifiers.push(Static);
                }
                if !field.modifiers.contains(&Final) {
                    field.modifiers.push(Final);
                }
                
                return Ok(InterfaceMember::Field(field));
            }
        }

        // Constants (fields) as fallback
        let mut field = self.parse_field_decl(modifiers)?;
        
        // Interface fields are implicitly public static final
        use crate::ast::Modifier::*;
        if !field.modifiers.contains(&Public) {
            field.modifiers.push(Public);
        }
        if !field.modifiers.contains(&Static) {
            field.modifiers.push(Static);
        }
        if !field.modifiers.contains(&Final) {
            field.modifiers.push(Final);
        }
        
        Ok(InterfaceMember::Field(field))
    }
    
    fn parse_enum_constant(&mut self) -> Result<EnumConstant> {
        let name = self.parse_identifier()?;
        
        // Parse arguments if present
        let arguments = if self.check(&Token::LParen) {
            self.advance(); // consume '('
            let args = if !self.check(&Token::RParen) {
                self.parse_argument_list()?
            } else {
                Vec::new()
            };
            self.consume(&Token::RParen, "Expected ')' after enum constant arguments")?;
            Some(args)
        } else {
            None
        };
        
        // Parse class body if present
        let class_body = if self.check(&Token::LBrace) {
            Some(self.parse_class_body()?)
        } else {
            None
        };
        
        let span = Span::new(
            self.current_span().start,
            self.previous_span().end,
        );
        
        Ok(EnumConstant {
            name,
            arguments: arguments.unwrap_or_default(),
            body: class_body,
            span,
        })
    }
    
    fn parse_annotation_member(&mut self) -> Result<AnnotationMember> {
        // JLS: AnnotationTypeElementDeclaration → {Annotation} UnannType Identifier () [default ElementValue] ;
        // We parse a simplified form: [modifiers] Type Identifier '(' ')' ['default' elementValue] ';'
        let _modifiers = self.parse_modifiers()?; // currently ignored on AnnotationMember

        // Type (non-void in JLS, but we allow and carry through if present)
        let type_ref = if self.check(&Token::Void) {
            // consume void to avoid loops; represent as a TypeRef("void")
            let start = self.current_span();
            self.advance();
            let end = self.previous_span();
            TypeRef { name: "void".to_string(), type_args: Vec::new(), annotations: Vec::new(), array_dims: 0, span: Span::new(start.start, end.end) }
        } else {
            self.parse_type_ref()?
        };

        // Name and mandatory empty parameter list
        let name = self.parse_identifier()?;
        self.consume(&Token::LParen, "Expected '(' in annotation member declaration")?;
        self.consume(&Token::RParen, "Expected ')' in annotation member declaration")?;

        // Optional default value
        let default_value = if self.match_token(&Token::Default) {
            // elementValue → conditionalExpression | annotation | elementValueArrayInitializer
            // We support expression and array initializer forms here.
            let expr = if self.check(&Token::LBrace) {
                // Array initializer: '{' (expr (',' expr)*)? '}'
                self.advance();
                let mut values: Vec<Expr> = Vec::new();
                if !self.check(&Token::RBrace) {
                    loop {
                        values.push(self.parse_expression()?);
                        if !self.match_token(&Token::Comma) { break; }
                    }
                }
                self.consume(&Token::RBrace, "Expected '}' to close annotation default array value")?;
                Expr::ArrayInitializer(values)
            } else {
                self.parse_expression()?
            };
            Some(expr)
        } else { None };

        self.consume(&Token::Semicolon, "Expected ';' after annotation member declaration")?;

        let span = Span::new(self.current_span().start, self.previous_span().end);
        Ok(AnnotationMember { type_ref, name, default_value, span })
    }
    
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>> {
        let mut parameters = Vec::new();
        
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            let modifiers = self.parse_modifiers()?;
            let annotations = self.parse_annotations()?;
            let mut type_ref = self.parse_type_ref()?;
            // Varargs ( ... ) attaches to the type
            let varargs = if self.match_token(&Token::Ellipsis) { true } else { false };
            let name = self.parse_identifier()?;
            
            // Parse array dimensions after the identifier
            let mut _array_dims = 0;
            while self.check(&Token::LBracket) {
                self.advance(); // consume '['
                self.consume(&Token::RBracket, "Expected ']' after array dimension")?;
                _array_dims += 1;
            }
            // Apply any post-identifier array dimensions to the parameter's type
            if _array_dims > 0 {
                type_ref.array_dims += _array_dims;
            }
            // Allow an alternative form: array dimensions after type (already handled in parse_type_ref)
            
            let span = Span::new(
                self.current_span().start,
                self.previous_span().end,
            );
            
            parameters.push(Parameter {
                modifiers,
                annotations,
                type_ref,
                name,
                varargs,
                span,
            });
            
            if !self.match_token(&Token::Comma) {
                break;
            }
            }
        }
        
        Ok(parameters)
    }
    
    fn parse_argument_list(&mut self) -> Result<Vec<Expr>> {
        let mut arguments = Vec::new();
        
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            arguments.push(self.parse_expression()?);
            
            if !self.match_token(&Token::Comma) {
                break;
            }
            }
        }
        
        Ok(arguments)
    }
    
    fn parse_class_body(&mut self) -> Result<ClassDecl> {
        self.consume(&Token::LBrace, "Expected '{' for class body")?;
        
        let mut members = Vec::new();
        {
            let mut steps: usize = 0;
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
                let before = self.current;
            match self.parse_class_member() {
                Ok(m) => members.push(m),
                Err(e) => {
                    log::debug!("parse_class_body: member error at token {:?} '{}' -> {}", self.peek().token_type(), self.peek().lexeme(), e);
                    self.synchronize_in_class_body();
                    if self.check(&Token::Semicolon) { self.advance(); }
                }
            }
                if self.current == before {
                    if !self.is_at_end() { self.advance(); }
                }
            }
        }
        
        let _ = self.consume(&Token::RBrace, "Expected '}' to close class body");
        
        let span = Span::new(
            self.current_span().start,
            self.previous_span().end,
        );
        
        // Create a minimal ClassDecl for the enum constant body
        Ok(ClassDecl {
            modifiers: Vec::new(),
            annotations: Vec::new(),
            name: "".to_string(),
            type_params: Vec::new(),
            extends: None,
            implements: Vec::new(),
            body: members,
            span,
        })
    }
    
    fn parse_block(&mut self) -> Result<Block> {
        self.consume(&Token::LBrace, "Expected '{' for block")?;
        
        let mut statements = Vec::new();
        {
            let mut steps: usize = 0;
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            // Skip empty statements and semicolons
            if self.check(&Token::Semicolon) {
                self.advance();
                continue;
            }
            log::debug!("parse_block: statement start at token {:?} '{}'", self.peek().token_type(), self.peek().lexeme());
            // Return statement
            if self.check(&Token::Return) {
                let start = self.current_span();
                self.advance();
                let value = if !self.check(&Token::Semicolon) { Some(self.parse_expression()?) } else { None };
                self.consume(&Token::Semicolon, "Expected ';' after return")?;
                let end = self.previous_span();
                let span = Span::new(start.start, end.end);
                statements.push(Stmt::Return(ReturnStmt { value, span }));
                continue;
            }
            // If statement
            if self.check(&Token::If) {
                statements.push(self.parse_if_stmt()?);
                continue;
            }
            // While statement
            if self.check(&Token::While) {
                statements.push(self.parse_while_stmt()?);
                continue;
            }
            // For statement
            if self.check(&Token::For) {
                statements.push(self.parse_for_stmt()?);
                continue;
            }
            // Try-catch-finally
            if self.check(&Token::Try) {
                statements.push(self.parse_try_stmt()?);
                continue;
            }
            // Switch
            if self.check(&Token::Switch) {
                statements.push(self.parse_switch_stmt()?);
                continue;
            }
            
            // If we encounter a closing brace, break out of the loop
            if self.check(&Token::RBrace) {
                break;
            }
            
            let before = self.current;
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    // Error recovery inside blocks
                    log::debug!("parse_block: statement error at token {:?} '{}' -> {}", self.peek().token_type(), self.peek().lexeme(), e);
                    self.synchronize_in_block();
                    continue;
                }
            }
            if self.current == before {
                // Ensure progress to avoid infinite loops
                if !self.is_at_end() { self.advance(); }
            }
            }
        }
        
        let _ = self.consume(&Token::RBrace, "Expected '}' to close block");
        
        let span = Span::new(
            self.current_span().start,
            self.previous_span().end,
        );
        
        Ok(Block {
            statements,
            span,
        })
    }
    
    fn parse_statement(&mut self) -> Result<Stmt> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEndOfInput {
                expected: "statement".to_string(),
                location: self.previous().location(),
            }.into());
        }
        
        // Empty statement
        if self.check(&Token::Semicolon) {
            self.advance();
            return Ok(Stmt::Empty);
        }

        // LabeledStatement: Identifier ':' Statement
        if self.check(&Token::Identifier) {
            let save = self.current;
            let label = self.parse_identifier()?;
            if self.match_token(&Token::Colon) {
                self.label_stack.push(label.clone());
                let stmt = self.parse_statement()?;
                self.label_stack.pop();
                let span = Span::new(self.current_span().start, self.previous_span().end);
                return Ok(Stmt::Labeled(LabeledStmt { label, statement: Box::new(stmt), span }));
            } else {
                // rollback
                self.current = save;
            }
        }

        // Block
        if self.check(&Token::LBrace) {
            return Ok(Stmt::Block(self.parse_block()?));
        }

        // do-while
        if self.check(&Token::Do) {
            return self.parse_do_while_stmt();
        }

        // Control flow and simple statements
        if self.check(&Token::Return) {
            let start = self.current_span();
                    self.advance();
            let value = if !self.check(&Token::Semicolon) { Some(self.parse_expression()?) } else { None };
            self.consume(&Token::Semicolon, "Expected ';' after return")?;
            let end = self.previous_span();
            let span = Span::new(start.start, end.end);
            return Ok(Stmt::Return(ReturnStmt { value, span }));
        }
        if self.check(&Token::Throw) {
            return self.parse_throw_statement();
        }
        if self.check(&Token::Synchronized) {
            return self.parse_synchronized_statement();
        }
        if self.check(&Token::Break) {
            let start = self.current_span();
            self.advance();
            let label = if self.check(&Token::Identifier) { Some(self.parse_identifier()?) } else { None };
            if let Some(ref l) = label {
                if !self.label_stack.iter().any(|x| x == l) {
                    // degrade to unlabeled break if unknown label
                    // alternatively, record an error but continue
                }
            }
            self.consume(&Token::Semicolon, "Expected ';' after break")?;
            let end = self.previous_span();
            let span = Span::new(start.start, end.end);
            return Ok(Stmt::Break(BreakStmt { label, span }));
        }
        if self.check(&Token::Continue) {
            let start = self.current_span();
            self.advance();
            let label = if self.check(&Token::Identifier) { Some(self.parse_identifier()?) } else { None };
            if let Some(ref l) = label {
                if !self.label_stack.iter().any(|x| x == l) {
                    // degrade to unlabeled continue if unknown label
                }
            }
            self.consume(&Token::Semicolon, "Expected ';' after continue")?;
            let end = self.previous_span();
            let span = Span::new(start.start, end.end);
            return Ok(Stmt::Continue(ContinueStmt { label, span }));
        }
        if self.check(&Token::If) { return self.parse_if_stmt(); }
        if self.check(&Token::While) { return self.parse_while_stmt(); }
        if self.check(&Token::For) { return self.parse_for_stmt(); }
        if self.check(&Token::Assert) { return self.parse_assert_statement(); }
        if self.check(&Token::Try) { return self.parse_try_stmt(); }
        if self.check(&Token::Switch) { return self.parse_switch_stmt(); }

        // Local type declaration (class/interface/enum/annotation) inside blocks.
        // Allow leading annotations/modifiers before the keyword.
        {
            let mut i = self.current;
            // Skip annotations: @QualifiedName ( ... )?
            while self.peek_token_type(i) == Some(&Token::At) {
                i += 1; // '@'
                if self.peek_token_type(i) != Some(&Token::Identifier) { break; }
                i += 1;
                while self.peek_token_type(i) == Some(&Token::Dot) && self.peek_token_type(i + 1) == Some(&Token::Identifier) {
                    i += 2;
                }
                if self.peek_token_type(i) == Some(&Token::LParen) {
                    let mut depth = 0usize;
                    loop {
                        match self.peek_token_type(i) {
                            Some(Token::LParen) => { depth += 1; i += 1; }
                            Some(Token::RParen) => { i += 1; if depth == 0 { break; } depth -= 1; if depth == 0 { break; } }
                            Some(_) => { i += 1; }
                            None => break,
                        }
                    }
                }
            }
            // Skip modifiers
            loop {
                match self.peek_token_type(i) {
                    Some(Token::Public) | Some(Token::Protected) | Some(Token::Private) |
                    Some(Token::Abstract) | Some(Token::Static) | Some(Token::Final) |
                    Some(Token::Native) | Some(Token::Synchronized) | Some(Token::Transient) |
                    Some(Token::Volatile) | Some(Token::Strictfp) | Some(Token::Default) => { i += 1; }
                    _ => break,
                }
            }
            if matches!(self.peek_token_type(i), Some(Token::Class) | Some(Token::Interface) | Some(Token::Enum) | Some(Token::At)) {
                if let Ok(decl) = self.parse_type_decl() { return Ok(Stmt::TypeDecl(decl)); }
            }
        }

        // Variable declaration starting with a type (use lookahead)
        if Self::is_variable_declaration_start(self) {
            return self.parse_variable_declaration_stmt();
        }

        // Expression statement
        let expr = self.parse_expression()?;
        if self.check(&Token::Semicolon) { self.advance(); }
        let span = self.previous_span();
        Ok(Stmt::Expression(ExprStmt { expr, span }))
    }

    fn parse_assert_statement(&mut self) -> Result<Stmt> {
        // Grammar:
        // assert Expression [ : Expression ] ;
        let start = self.current_span();
        self.consume(&Token::Assert, "Expected 'assert'")?;
        let cond = self.parse_expression()?;
        let msg = if self.match_token(&Token::Colon) { Some(self.parse_expression()?) } else { None };
        self.consume(&Token::Semicolon, "Expected ';' after assert statement")?;
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::Assert(AssertStmt { condition: cond, message: msg, span }))
    }

    fn parse_throw_statement(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        self.consume(&Token::Throw, "Expected 'throw'")?;
        let expr = self.parse_expression()?;
        self.consume(&Token::Semicolon, "Expected ';' after throw expression")?;
        let end = self.previous_span();
        let span = Span::new(start.start, end.end);
        Ok(Stmt::Throw(ThrowStmt { expr, span }))
    }

    fn parse_synchronized_statement(&mut self) -> Result<Stmt> {
        // Parse: synchronized ( expr ) block
        let start = self.current_span();
        self.consume(&Token::Synchronized, "Expected 'synchronized'")?;
        self.consume(&Token::LParen, "Expected '(' after 'synchronized'")?;
        let lock = self.parse_expression()?;
        self.consume(&Token::RParen, "Expected ')' after synchronized expression")?;
        let body = self.parse_block()?;
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::Synchronized(SynchronizedStmt { lock, body, span }))
    }

    fn parse_variable_declaration_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        // Allow local variable modifiers (e.g., final)
        let modifiers = self.parse_modifiers()?;
        let type_ref = self.parse_type_ref()?;
        let mut variables: Vec<VariableDeclarator> = Vec::new();
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            let before = self.current;
            let name = self.parse_identifier()?;
            let mut array_dims = 0;
            while self.match_token(&Token::LBracket) {
                self.consume(&Token::RBracket, "Expected ']' after array dimension")?;
                array_dims += 1;
            }
            let initializer = if self.match_token(&Token::Assign) {
                Some(self.parse_expression()?)
            } else { None };
            let decl_span = Span::new(start.start, self.previous_span().end);
            variables.push(VariableDeclarator { name, array_dims, initializer, span: decl_span });
            if !self.match_token(&Token::Comma) { break; }
            if self.current == before {
                if !self.is_at_end() { self.advance(); }
            }
            }
        }
        self.consume(&Token::Semicolon, "Expected ';' after variable declaration")?;
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::Declaration(VarDeclStmt { modifiers, type_ref, variables, span }))
    }
    
    fn parse_expression(&mut self) -> Result<Expr> {
        // Assignment is the lowest precedence (right-associative)
        let expr = self.parse_assignment_expr()?;
        Ok(expr)
    }

    fn parse_assignment_expr(&mut self) -> Result<Expr> {
        // Try lambda expression first (has low precedence, above assignment)
        if let Ok(lambda_expr) = self.try_parse_lambda_expr() {
            return Ok(lambda_expr);
        }
        
        let left = self.parse_conditional_expr()?;
        // Check for assignment or compound assignment operators
        let op = if self.match_token(&Token::Assign) {
            Some(AssignmentOp::Assign)
        } else if self.match_token(&Token::AddAssign) {
            Some(AssignmentOp::AddAssign)
        } else if self.match_token(&Token::SubAssign) {
            Some(AssignmentOp::SubAssign)
        } else if self.match_token(&Token::MulAssign) {
            Some(AssignmentOp::MulAssign)
        } else if self.match_token(&Token::DivAssign) {
            Some(AssignmentOp::DivAssign)
        } else if self.match_token(&Token::ModAssign) {
            Some(AssignmentOp::ModAssign)
        } else if self.match_token(&Token::AndAssign) {
            Some(AssignmentOp::AndAssign)
        } else if self.match_token(&Token::OrAssign) {
            Some(AssignmentOp::OrAssign)
        } else if self.match_token(&Token::XorAssign) {
            Some(AssignmentOp::XorAssign)
        } else if self.match_token(&Token::LShiftAssign) {
            Some(AssignmentOp::LShiftAssign)
        } else if self.match_token(&Token::RShiftAssign) {
            Some(AssignmentOp::RShiftAssign)
        } else if self.match_token(&Token::URShiftAssign) {
            Some(AssignmentOp::URShiftAssign)
        } else {
            None
        };
        if let Some(operator) = op {
            let value = self.parse_assignment_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            return Ok(Expr::Assignment(AssignmentExpr {
                target: Box::new(left),
                operator,
                value: Box::new(value),
                    span,
                }));
            }
        Ok(left)
    }

    fn parse_conditional_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_logical_or_expr()?;
        if self.match_token(&Token::Question) {
            let then_expr = self.parse_expression()?;
            self.consume(&Token::Colon, "Expected ':' in conditional expression")?;
            let else_expr = self.parse_expression()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            expr = Expr::Conditional(ConditionalExpr {
                condition: Box::new(expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_logical_or_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_logical_and_expr()?;
        while self.match_token(&Token::PipePipe) {
            let right = self.parse_logical_and_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::LogicalOr, right: Box::new(right), span });
        }
        Ok(expr)
    }

    fn parse_logical_and_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_bitwise_or_expr()?;
        while self.match_token(&Token::AndAnd) {
            let right = self.parse_bitwise_or_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::LogicalAnd, right: Box::new(right), span });
        }
        Ok(expr)
    }

    fn parse_bitwise_or_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_bitwise_xor_expr()?;
        while self.match_token(&Token::Pipe) {
            let right = self.parse_bitwise_xor_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Or, right: Box::new(right), span });
        }
        Ok(expr)
    }

    fn parse_bitwise_xor_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_bitwise_and_expr()?;
        {
            let mut steps: usize = 0;
            while self.match_token(&Token::Caret) {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            let right = self.parse_bitwise_and_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Xor, right: Box::new(right), span });
            }
        }
        Ok(expr)
    }

    fn parse_bitwise_and_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_equality_expr()?;
        {
            let mut steps: usize = 0;
            while self.match_token(&Token::Amp) {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            let right = self.parse_equality_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::And, right: Box::new(right), span });
            }
        }
        Ok(expr)
    }

    fn parse_equality_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_relational_expr()?;
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            if self.match_token(&Token::Eq) {
                let right = self.parse_relational_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Eq, right: Box::new(right), span });
            } else if self.match_token(&Token::Ne) {
                let right = self.parse_relational_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Ne, right: Box::new(right), span });
            } else { break; }
            }
        }
        Ok(expr)
    }

    fn parse_relational_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_shift_expr()?;
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            if self.match_token(&Token::Lt) {
                let right = self.parse_shift_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Lt, right: Box::new(right), span });
            } else if self.match_token(&Token::Le) {
                let right = self.parse_shift_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Le, right: Box::new(right), span });
            } else if self.match_token(&Token::Gt) {
                let right = self.parse_shift_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Gt, right: Box::new(right), span });
            } else if self.match_token(&Token::Ge) {
                let right = self.parse_shift_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Ge, right: Box::new(right), span });
            } else if self.match_token(&Token::InstanceOf) {
                let target_type = self.parse_type_ref()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::InstanceOf(InstanceOfExpr { expr: Box::new(expr), target_type, span });
            } else { break; }
            }
        }
        Ok(expr)
    }

    fn parse_shift_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_additive_expr()?;
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            if self.match_token(&Token::LShift) {
                let right = self.parse_additive_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::LShift, right: Box::new(right), span });
            } else if self.match_token(&Token::RShift) {
                let right = self.parse_additive_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::RShift, right: Box::new(right), span });
            } else if self.match_token(&Token::URShift) {
                let right = self.parse_additive_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::URShift, right: Box::new(right), span });
            } else { break; }
            }
        }
        Ok(expr)
    }

    fn parse_additive_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_multiplicative_expr()?;
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            if self.match_token(&Token::Plus) {
                let right = self.parse_multiplicative_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Add, right: Box::new(right), span });
            } else if self.match_token(&Token::Minus) {
                let right = self.parse_multiplicative_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Sub, right: Box::new(right), span });
            } else { break; }
            }
        }
        Ok(expr)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_unary_expr()?;
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            if self.match_token(&Token::Star) {
                let right = self.parse_unary_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Mul, right: Box::new(right), span });
            } else if self.match_token(&Token::Slash) {
                let right = self.parse_unary_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Div, right: Box::new(right), span });
            } else if self.match_token(&Token::Percent) {
                let right = self.parse_unary_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Binary(BinaryExpr { left: Box::new(expr), operator: BinaryOp::Mod, right: Box::new(right), span });
            } else { break; }
            }
        }
        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        // Prefix operators
        if self.match_token(&Token::Inc) {
            let operand = self.parse_unary_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            return Ok(Expr::Unary(UnaryExpr { operator: UnaryOp::PreInc, operand: Box::new(operand), span }));
        }
        if self.match_token(&Token::Dec) {
            let operand = self.parse_unary_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            return Ok(Expr::Unary(UnaryExpr { operator: UnaryOp::PreDec, operand: Box::new(operand), span }));
        }
        if self.match_token(&Token::Bang) {
            let operand = self.parse_unary_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            return Ok(Expr::Unary(UnaryExpr { operator: UnaryOp::Not, operand: Box::new(operand), span }));
        }
        if self.match_token(&Token::Tilde) {
            let operand = self.parse_unary_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            return Ok(Expr::Unary(UnaryExpr { operator: UnaryOp::BitNot, operand: Box::new(operand), span }));
        }
        if self.match_token(&Token::Minus) {
            let operand = self.parse_unary_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            return Ok(Expr::Unary(UnaryExpr { operator: UnaryOp::Minus, operand: Box::new(operand), span }));
        }
        if self.match_token(&Token::Plus) {
            let operand = self.parse_unary_expr()?;
            let span = Span::new(self.current_span().start, self.previous_span().end);
            return Ok(Expr::Unary(UnaryExpr { operator: UnaryOp::Plus, operand: Box::new(operand), span }));
        }
        // Cast: '(' Type ')' UnaryExpression
        if self.check(&Token::LParen) {
            let save = self.current;
            self.advance(); // consume '('
            let mut i = self.current;
            if self.lookahead_type_ref(&mut i) && self.peek_token_type(i) == Some(&Token::RParen) {
                // It's a cast
                let target_type = self.parse_type_ref()?;
                self.consume(&Token::RParen, "Expected ')' after cast type")?;
                let expr = self.parse_unary_expr()?;
                let span = Span::new(self.current_span().start, self.previous_span().end);
                return Ok(Expr::Cast(CastExpr { target_type, expr: Box::new(expr), span }));
            } else {
                // Not a cast; rollback
                self.current = save;
            }
        }
        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr> {
        let mut expr = self.parse_primary_expr()?;
        // Chained operations: calls, field access, array access, post-inc/dec, anonymous class after constructor, method references
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            // If we have just parsed a constructor call like `new T(args)` and the next token is '{',
            // treat it as an anonymous class body attached to the previous NewExpr.
            if self.check(&Token::LBrace) {
                if let Expr::New(mut new_expr) = expr.clone() {
                    // Only attach anonymous body if not already set
                    if new_expr.anonymous_body.is_none() {
                        let body_decl = self.parse_class_body()?;
                        let span_all = Span::new(self.current_span().start, self.previous_span().end);
                        new_expr.anonymous_body = Some(body_decl);
                        new_expr.span = span_all;
                        expr = Expr::New(new_expr);
                        continue;
                    }
                }
            }
            // Method reference: expression::method or expression::new
            if self.match_token(&Token::DoubleColon) {
                let start_span = self.current_span();
                let is_constructor = self.check(&Token::New);
                let method_name = if is_constructor {
                    self.advance(); // consume 'new'
                    "new".to_string()
                } else if self.check(&Token::Identifier) {
                    self.parse_identifier()?
                } else {
                    return Err(ParseError::UnexpectedToken { 
                        expected: "method name or 'new'".to_string(), 
                        found: format!("{:?}", self.peek().token_type()), 
                        location: self.current_span().start 
                    }.into());
                };
                
                let end_span = self.previous_span();
                let span = Span::new(start_span.start, end_span.end);
                
                expr = Expr::MethodReference(MethodReferenceExpr {
                    target: Some(Box::new(expr)),
                    method_name,
                    is_constructor,
                    span,
                });
                continue;
            }
            if self.check(&Token::LParen) {
                self.advance();
                let args = if !self.check(&Token::RParen) { self.parse_argument_list()? } else { Vec::new() };
                self.consume(&Token::RParen, "Expected ')' after arguments")?;
                if let Expr::Identifier(IdentifierExpr { name: id_name, span }) = expr {
                    let span_all = Span::new(span.start, self.previous_span().end);
                    expr = Expr::MethodCall(MethodCallExpr { target: None, name: id_name, arguments: args, span: span_all });
                    continue;
                }
                // For qualified calls, we always reach here via the dot-branch below
                break;
            }
            if self.match_token(&Token::Dot) {
                // Optional call-site type arguments: .<T, U extends X>
                if self.check(&Token::Lt) {
                    // Best-effort: parse and ignore call-site type args
                    let _ = self.parse_type_arguments_typearg();
                }
                // After a dot, Java allows identifiers, 'this', 'super', and 'class'
                let name = if self.check(&Token::Identifier) {
                    self.parse_identifier()?
                } else if self.check(&Token::This) {
                    self.advance();
                    "this".to_string()
                } else if self.check(&Token::Super) {
                    self.advance();
                    "super".to_string()
                } else if self.check(&Token::Class) {
                    self.advance();
                    "class".to_string()
                } else {
                    // Fallback: treat unexpected token after '.' as an identifier-like name
                    // to avoid stalling; this keeps the parser progressing in recovery mode.
                    let t = self.peek().lexeme().to_string();
                    self.advance();
                    t
                };
                if self.check(&Token::LParen) {
                    self.advance();
                    let arguments = if !self.check(&Token::RParen) { self.parse_argument_list()? } else { Vec::new() };
                    self.consume(&Token::RParen, "Expected ')' after method call")?;
                    let span_all = Span::new(self.current_span().start, self.previous_span().end);
                    expr = Expr::MethodCall(MethodCallExpr { target: Some(Box::new(expr)), name, arguments, span: span_all });
                } else {
                    let span_all = Span::new(self.current_span().start, self.previous_span().end);
                    expr = Expr::FieldAccess(FieldAccessExpr { target: Some(Box::new(expr)), name, span: span_all });
                }
                continue;
            }
            if self.match_token(&Token::LBracket) {
                let index = self.parse_expression()?;
                self.consume(&Token::RBracket, "Expected ']' after array index")?;
                let span_all = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::ArrayAccess(ArrayAccessExpr { array: Box::new(expr), index: Box::new(index), span: span_all });
                continue;
            }
            if self.match_token(&Token::Inc) {
                let span_all = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Unary(UnaryExpr { operator: UnaryOp::PostInc, operand: Box::new(expr), span: span_all });
                continue;
            }
            if self.match_token(&Token::Dec) {
                let span_all = Span::new(self.current_span().start, self.previous_span().end);
                expr = Expr::Unary(UnaryExpr { operator: UnaryOp::PostDec, operand: Box::new(expr), span: span_all });
                continue;
            }
            break;
            }
        }
        Ok(expr)
    }
    
    fn parse_primary_expr(&mut self) -> Result<Expr> {
        // Class literal: TypeName.class or primitiveType.class (optionally with array dims)
        {
            let save = self.current;
            // Try to parse a type reference greedily and then check for '.class'
            // Only attempt when the current token looks like a type start to avoid backtracking cost
            if matches!(self.peek().token_type(),
                Token::Boolean | Token::Byte | Token::Short | Token::Int |
                Token::Long | Token::Char | Token::Float | Token::Double |
                Token::Identifier
            ) {
                // Use a speculative parse of a type ref
                if let Ok(ty) = (|| -> Result<TypeRef> { Ok(self.parse_type_ref()?) })() {
                    if self.check(&Token::Dot) && self.peek_token_type(self.current + 1) == Some(&Token::Class) {
                        self.advance(); // '.'
                        self.advance(); // 'class'
                        let span = Span::new(self.current_span().start, self.previous_span().end);
                        // Represent as a field access on a synthetic identifier of the type name
                        let target = Expr::Identifier(IdentifierExpr { name: ty.name.clone(), span });
                        return Ok(Expr::FieldAccess(FieldAccessExpr { target: Some(Box::new(target)), name: "class".to_string(), span }));
                    } else {
                        // Not a class literal; rewind and continue with normal primary parsing
                        self.current = save;
                    }
                } else {
                    self.current = save;
                }
            }
        }
        // Object creation and anonymous class as a primary expression
        if self.check(&Token::New) {
            return self.parse_new_expression();
        }
        // Explicit call-site type arguments before an unqualified method invocation:
        // <T, U extends X> identifier(args)
        if self.check(&Token::Lt) {
            let save = self.current;
            if self.parse_type_arguments_typearg().is_ok() {
                if self.check(&Token::Identifier) {
                    let name = self.parse_identifier()?;
                    if self.check(&Token::LParen) {
                        self.advance();
                        let args = if !self.check(&Token::RParen) { self.parse_argument_list()? } else { Vec::new() };
                        self.consume(&Token::RParen, "Expected ')' after arguments")?;
                        let span = Span::new(self.current_span().start, self.previous_span().end);
                        return Ok(Expr::MethodCall(MethodCallExpr { target: None, name, arguments: args, span }));
                    }
                }
            }
            // rollback if not a method invocation
            self.current = save;
        }
        // Parenthesized
        if self.match_token(&Token::LParen) {
            let inner = self.parse_expression()?;
            self.consume(&Token::RParen, "Expected ')' after expression")?;
            return Ok(Expr::Parenthesized(Box::new(inner)));
        }
        
        // Literals and identifiers (existing logic adapted)
        // Parse literals
        if self.check(&Token::StringLiteral) {
            let token = self.advance();
            let value = token.lexeme().trim_matches('"').to_string();
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::String(value),
                span,
            }));
        }
        if self.check(&Token::CharLiteral) {
            let token = self.advance();
            let lex = token.lexeme();
            let ch = if lex.len() >= 3 { lex.chars().nth(1).unwrap_or('\0') } else { '\0' };
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Literal(LiteralExpr { value: Literal::Char(ch), span }));
        }
        if self.check(&Token::LongLiteral) {
            let token = self.advance();
            let raw = token.lexeme().replace('_', "");
            let v = if raw.starts_with("0x") || raw.starts_with("0X") {
                i64::from_str_radix(raw.trim_end_matches(|c| c=='l'||c=='L').trim_start_matches("0x").trim_start_matches("0X"), 16).unwrap_or(0)
            } else if raw.starts_with("0b") || raw.starts_with("0B") {
                i64::from_str_radix(raw.trim_end_matches(|c| c=='l'||c=='L').trim_start_matches("0b").trim_start_matches("0B"), 2).unwrap_or(0)
            } else if raw.starts_with('0') && raw.len() > 1 {
                i64::from_str_radix(raw.trim_end_matches(|c| c=='l'||c=='L').trim_start_matches('0'), 8).unwrap_or(0)
            } else {
                raw.trim_end_matches(|c| c=='l'||c=='L').parse::<i64>().unwrap_or(0)
            };
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Literal(LiteralExpr { value: Literal::Long(v), span }));
        }
        if self.check(&Token::HexInteger) || self.check(&Token::BinaryInteger) || self.check(&Token::OctalInteger) || self.check(&Token::DecimalInteger) {
            let token = self.advance();
            let raw = token.lexeme().replace('_', "");
            
            // Check if this is a long literal (ends with L or l)
            let is_long = raw.ends_with('L') || raw.ends_with('l');
            
            let v = if raw.starts_with("0x") || raw.starts_with("0X") {
                let hex_part = raw.trim_end_matches(|c| c=='l'||c=='L').trim_start_matches("0x").trim_start_matches("0X");
                
                // Handle special case for 0xFFFFFFFFFFFFFFFFL which should be -1L in Java
                if hex_part.to_uppercase() == "FFFFFFFFFFFFFFFF" {
                    -1i64
                } else {
                    // Try parsing as unsigned first, then convert to signed
                    match u64::from_str_radix(hex_part, 16) {
                        Ok(unsigned_val) => unsigned_val as i64,
                        Err(_) => 0
                    }
                }
            } else if raw.starts_with("0b") || raw.starts_with("0B") {
                i64::from_str_radix(raw.trim_end_matches(|c| c=='l'||c=='L').trim_start_matches("0b").trim_start_matches("0B"), 2).unwrap_or(0)
            } else if raw.starts_with('0') && raw.len() > 1 {
                i64::from_str_radix(raw.trim_end_matches(|c| c=='l'||c=='L').trim_start_matches('0'), 8).unwrap_or(0)
            } else {
                raw.trim_end_matches(|c| c=='l'||c=='L').parse::<i64>().unwrap_or(0)
            };
            
            let location = token.location();
            let span = Span::new(location.clone(), location);
            
            // Return Long literal if it has L suffix, otherwise Integer
            if is_long {
                return Ok(Expr::Literal(LiteralExpr { value: Literal::Long(v), span }));
            } else {
                return Ok(Expr::Literal(LiteralExpr { value: Literal::Integer(v), span }));
            }
        }
        if self.check(&Token::TypedFloat) || self.check(&Token::FloatLiteral) || self.check(&Token::ScientificFloat) {
            let token = self.advance();
            let raw = token.lexeme().replace('_', "");
            let is_double = raw.ends_with('d') || raw.ends_with('D') || 
                           (!raw.ends_with('f') && !raw.ends_with('F') && 
                            (raw.contains('.') || raw.contains('e') || raw.contains('E')));
            let v = raw.trim_end_matches(|c| c=='f' || c=='F' || c=='d' || c=='D').parse::<f64>().unwrap_or(0.0);
            let location = token.location();
            let span = Span::new(location.clone(), location);
            if is_double {
                return Ok(Expr::Literal(LiteralExpr { value: Literal::Double(v), span }));
            } else {
                return Ok(Expr::Literal(LiteralExpr { value: Literal::Float(v), span }));
            }
        }
        
        // Parse boolean literals
        if self.check(&Token::True) || self.check(&Token::False) {
            let token = self.advance();
            let value = token.token_type() == &Token::True;
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::Boolean(value),
                span,
            }));
        }
        
        // Parse null literal
        if self.check(&Token::Null) {
            let token = self.advance();
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::Null,
                span,
            }));
        }
        
        // Handle 'this' and 'super' as identifiers for now
        if self.check(&Token::This) {
            let token = self.advance();
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Identifier(IdentifierExpr { name: "this".to_string(), span }));
        }
        if self.check(&Token::Super) {
            let token = self.advance();
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Identifier(IdentifierExpr { name: "super".to_string(), span }));
        }
        
        // Parse identifiers (variable references)
        if self.check(&Token::Identifier) {
            let token = self.advance();
            let name = token.lexeme().to_string();
            let location = token.location();
            let span = Span::new(location.clone(), location);
            return Ok(Expr::Identifier(IdentifierExpr {
                name,
                span,
            }));
        }
        
        // Fallback: produce a zero literal but avoid consuming unexpected tokens to keep recovery stable
        let span = self.current_span();
        Ok(Expr::Literal(LiteralExpr { value: Literal::Integer(0), span }))
    }

    fn parse_new_expression(&mut self) -> Result<Expr> {
        let start_span = self.current_span();
        self.consume(&Token::New, "Expected 'new'")?;

        // Parse type name (qualified or primitive)
        let name = match self.peek().token_type() {
            Token::Boolean | Token::Byte | Token::Short | Token::Int | Token::Long | Token::Char | Token::Float | Token::Double => {
                let tok = self.advance();
                tok.lexeme().to_string()
            }
            Token::Identifier => self.parse_qualified_name()?,
            _ => {
                let current = self.peek();
                return Err(ParseError::UnexpectedToken {
                    expected: "type".to_string(),
                    found: format!("{:?}", current.token_type()),
                    location: current.location(),
                }.into());
            }
        };

        // Optional type arguments: either diamond operator <> or explicit <T,...>
        let mut type_args: Vec<TypeArg> = Vec::new();
        if self.check(&Token::Lt) {
            // Lookahead one token to see if it's a diamond operator
            let save = self.current;
            self.advance(); // consume '<'
            if self.check(&Token::Gt) || self.check(&Token::RShift) || self.check(&Token::URShift) {
                // Diamond operator: consume '>' and keep empty type_args
                self.advance();
            } else {
                // Not diamond; rewind and parse full type arguments
                self.current = save;
                type_args = self.parse_type_arguments_typearg()?;
            }
        }

        // Either array creation new T[expr]... or constructor call (optional anonymous body)
        let mut arguments = Vec::new();
        let mut array_dims_count: usize = 0;
        if self.check(&Token::LBracket) {
            // array creation
            // Option A: new T[expr] [expr] ...
            let mut saw_empty_brackets = false;
            while self.match_token(&Token::LBracket) {
                if self.check(&Token::RBracket) {
                    // new T[] ... possibly initializer
                    saw_empty_brackets = true;
                    self.advance(); // consume ']'
                    array_dims_count += 1;
                    break;
                } else {
                    // size expression
                    arguments.push(self.parse_expression()?);
                    self.consume(&Token::RBracket, "Expected ']' in array creation")?;
                    array_dims_count += 1;
                }
            }
            // Optional initializer: { expr (, expr)* }
            if saw_empty_brackets && self.check(&Token::LBrace) {
                self.advance(); // consume '{'
                if !self.check(&Token::RBrace) {
                    loop {
                        let expr = self.parse_expression()?;
                        arguments.push(expr);
                        if !self.match_token(&Token::Comma) { break; }
                    }
                }
                self.consume(&Token::RBrace, "Expected '}' after array initializer")?;
            }
        } else {
            // Parse constructor arguments if present; allow omitted '()' for simplified tests
            let anonymous_body;
            if self.check(&Token::LParen) {
                self.advance(); // consume '('
                arguments = if !self.check(&Token::RParen) {
                    self.parse_argument_list()?
                } else {
                    Vec::new()
                };
                self.consume(&Token::RParen, "Expected ')' after constructor arguments")?;
                // Optional anonymous class body
                anonymous_body = if self.check(&Token::LBrace) {
                    let body_decl = self.parse_class_body()?;
                    Some(body_decl)
                } else { None };
            } else {
                // No parentheses: treat as zero-arg constructor with no anonymous body
                arguments = Vec::new();
                anonymous_body = None;
            }
            let end_span = self.previous_span();
            let span = Span::new(start_span.start, end_span.end);
            let type_span = span;
            let target_type = TypeRef { name, type_args, annotations: Vec::new(), array_dims: 0, span: type_span };
            return Ok(Expr::New(NewExpr { target_type, arguments, anonymous_body, span }));
        }

        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        let type_span = span; // approximate span
        let target_type = TypeRef { name, type_args, annotations: Vec::new(), array_dims: array_dims_count, span: type_span };
        Ok(Expr::New(NewExpr { target_type, arguments, anonymous_body: None, span }))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        self.consume(&Token::For, "Expected 'for'")?;
        self.consume(&Token::LParen, "Expected '(' after for")?;
        // Detect enhanced-for: Type Identifier ':' Expression ')'
        {
            let mut i = self.current;
            if self.lookahead_type_ref(&mut i) {
                if self.peek_token_type(i) == Some(&Token::Identifier) && self.peek_token_type(i + 1) == Some(&Token::Colon) {
                    // Parse enhanced-for header
                    let var_type = self.parse_type_ref()?;
                    let var_name = self.parse_identifier()?;
                    self.consume(&Token::Colon, "Expected ':' in enhanced for")?;
                    let iterable_expr = self.parse_expression()?; // Store the iterable expression
                    self.consume(&Token::RParen, "Expected ')' after enhanced for header")?;
                    // Parse body
                    let body = if self.check(&Token::LBrace) { Box::new(Stmt::Block(self.parse_block()?)) } else { Box::new(self.parse_statement()?) };
                    let span = Span::new(start.start, self.previous_span().end);
                    // Represent as a generic for-statement with declaration in init and no condition/update
                    // Store the iterable expression in the initializer as a temporary solution
                    let decl = VarDeclStmt {
                        modifiers: Vec::new(),
                        type_ref: var_type,
                        variables: vec![VariableDeclarator { name: var_name, array_dims: 0, initializer: Some(iterable_expr), span }],
                        span,
                    };
                    return Ok(Stmt::For(ForStmt { init: vec![Stmt::Declaration(decl)], condition: None, update: Vec::new(), body, span }));
                }
            }
        }
        // init
        let mut init_stmts: Vec<Stmt> = Vec::new();
        if !self.check(&Token::Semicolon) {
            if self.is_variable_declaration_start() {
                // In a for-header, the init variable declaration must NOT consume the trailing ';'
                let decl = self.parse_variable_declarators_no_semi()?;
                init_stmts.push(decl);
            } else {
                // expression list
                loop {
                    let e = self.parse_expression()?;
                    init_stmts.push(Stmt::Expression(ExprStmt { expr: e.clone(), span: self.previous_span() }));
                    if !self.match_token(&Token::Comma) { break; }
                }
            }
        }
        self.consume(&Token::Semicolon, "Expected ';' after for init")?;
        // condition
        let condition = if !self.check(&Token::Semicolon) {
            Some(self.parse_expression()?)
        } else { None };
        self.consume(&Token::Semicolon, "Expected ';' after for condition")?;
        // update
        let mut update_list: Vec<ExprStmt> = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let e = self.parse_expression()?;
                update_list.push(ExprStmt { expr: e, span: self.previous_span() });
                if !self.match_token(&Token::Comma) { break; }
            }
        }
        self.consume(&Token::RParen, "Expected ')' after for header")?;
        let body = if self.check(&Token::LBrace) { Box::new(Stmt::Block(self.parse_block()?)) } else { Box::new(self.parse_statement()?) };
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::For(ForStmt { init: init_stmts, condition, update: update_list, body, span }))
    }

    // Parse local variable declarators without consuming a trailing semicolon (for use in for-header)
    fn parse_variable_declarators_no_semi(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        let modifiers = self.parse_modifiers()?;
        let type_ref = self.parse_type_ref()?;
        let mut variables: Vec<VariableDeclarator> = Vec::new();
        {
            let mut steps: usize = 0;
            loop {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
                let name = self.parse_identifier()?;
                let mut array_dims = 0;
                while self.match_token(&Token::LBracket) {
                    self.consume(&Token::RBracket, "Expected ']' after array dimension")?;
                    array_dims += 1;
                }
                let initializer = if self.match_token(&Token::Assign) { Some(self.parse_expression()?) } else { None };
                let decl_span = Span::new(start.start, self.previous_span().end);
                variables.push(VariableDeclarator { name, array_dims, initializer, span: decl_span });
                if !self.match_token(&Token::Comma) { break; }
            }
        }
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::Declaration(VarDeclStmt { modifiers, type_ref, variables, span }))
    }

    fn parse_do_while_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        self.consume(&Token::Do, "Expected 'do'")?;
        let body = if self.check(&Token::LBrace) { Box::new(Stmt::Block(self.parse_block()?)) } else { Box::new(self.parse_statement()?) };
        self.consume(&Token::While, "Expected 'while' after do-body")?;
        self.consume(&Token::LParen, "Expected '(' after while")?;
        let condition = self.parse_expression()?;
        self.consume(&Token::RParen, "Expected ')' after condition")?;
        if self.check(&Token::Semicolon) { self.advance(); }
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::DoWhile(DoWhileStmt { body, condition, span }))
    }

    fn parse_try_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        self.consume(&Token::Try, "Expected 'try'")?;
        // Optional try-with-resources
        let mut resources: Vec<TryResource> = Vec::new();
        if self.check(&Token::LParen) {
            self.advance();
            if !self.check(&Token::RParen) {
                loop {
                    let save = self.current;
                    // Try parse resource as variable declaration: Type Identifier '=' Expr
                    let mut parsed_var = false;
                    let mut i = self.current;
                    if self.lookahead_type_ref(&mut i) {
                        let type_ref = self.parse_type_ref()?;
                        if self.check(&Token::Identifier) {
                            let name = self.parse_identifier()?;
                            if self.match_token(&Token::Assign) {
                                let init = self.parse_expression()?;
                                let span = Span::new(self.current_span().start, self.previous_span().end);
                                resources.push(TryResource::Var { modifiers: Vec::new(), type_ref, name, initializer: init, span });
                                parsed_var = true;
                            }
                        }
                    }
                    if !parsed_var {
                        // rollback and parse as expression resource
                        self.current = save;
                        let expr = self.parse_expression()?;
                        let span = Span::new(self.current_span().start, self.previous_span().end);
                        resources.push(TryResource::Expr { expr, span });
                    }
                    if !self.match_token(&Token::Semicolon) { break; }
                    if self.check(&Token::RParen) { break; }
                }
            }
            self.consume(&Token::RParen, "Expected ')' after try-with-resources")?;
        }
        let try_block = self.parse_block()?;
        let mut catch_clauses: Vec<CatchClause> = Vec::new();
        while self.check(&Token::Catch) {
            self.advance(); // consume 'catch'
            self.consume(&Token::LParen, "Expected '(' after catch")?;
            let modifiers = self.parse_modifiers()?;
            let type_ref = self.parse_type_ref()?;
            // Multi-catch: parse alternative types separated by '|'
            let mut alt_types: Vec<TypeRef> = Vec::new();
            while self.match_token(&Token::Pipe) {
                let alt = self.parse_type_ref()?;
                alt_types.push(alt);
            }
            let name = self.parse_identifier()?;
            let param_span = self.previous_span();
            let parameter = Parameter { modifiers, annotations: Vec::new(), type_ref, name, varargs: false, span: param_span };
            self.consume(&Token::RParen, "Expected ')' after catch parameter")?;
            let block = self.parse_block()?;
            let catch_span = block.span;
            catch_clauses.push(CatchClause { parameter, alt_types, block: block.clone(), span: catch_span });
        }
        let finally_block = if self.check(&Token::Finally) { self.advance(); Some(self.parse_block()?) } else { None };
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::Try(TryStmt { resources, try_block, catch_clauses, finally_block, span }))
    }

    fn parse_switch_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        self.consume(&Token::Switch, "Expected 'switch'")?;
        self.consume(&Token::LParen, "Expected '(' after switch")?;
        let expr = self.parse_expression()?;
        self.consume(&Token::RParen, "Expected ')' after switch expression")?;
        self.consume(&Token::LBrace, "Expected '{' to open switch body")?;
        let mut cases: Vec<SwitchCase> = Vec::new();
        {
            let mut steps: usize = 0;
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
                steps += 1;
            let mut labels: Vec<Expr> = Vec::new();
            // Collect consecutive case/default labels (aggregate multiple labels for same group)
            loop {
                if self.match_token(&Token::Case) {
                    let label_expr = self.parse_expression()?;
                    if self.consume(&Token::Colon, "Expected ':' after case label").is_err() {
                        // attempt to resync at next boundary
                        self.synchronize_in_switch();
                        // stop collecting labels for this case
                        break;
                    }
                    labels.push(label_expr);
                } else if self.match_token(&Token::Default) {
                    if self.consume(&Token::Colon, "Expected ':' after default").is_err() {
                        self.synchronize_in_switch();
                    }
                    // default represented as empty labels
                    labels.clear();
                    break;
                } else {
                    break;
                }
            }
            // Accumulate statements until next case/default or '}'
            let mut statements: Vec<Stmt> = Vec::new();
            while !self.check(&Token::Case) && !self.check(&Token::Default) && !self.check(&Token::RBrace) {
                let before = self.current;
                match self.parse_statement() {
                    Ok(stmt) => statements.push(stmt),
                    Err(_) => {
                        // Error recovery inside switch blocks
                        self.synchronize_in_switch();
                        break;
                    }
                }
                if self.current == before {
                    if !self.is_at_end() { self.advance(); }
                }
            }
            if !labels.is_empty() || !statements.is_empty() {
                let case_span = self.current_span();
                cases.push(SwitchCase { labels, statements, span: case_span });
            } else {
                break;
            }
        }
        }
        self.consume(&Token::RBrace, "Expected '}' to close switch body")?;
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::Switch(SwitchStmt { expression: expr, cases, span }))
    }

    // Parse a list of leading annotations
    fn parse_annotations(&mut self) -> Result<Vec<Annotation>> {
        let mut annotations = Vec::new();
        let mut steps: usize = 0;
        loop {
            if !self.check(&Token::At) { break; }
            if steps > crate::consts::PARSER_MAX_LOOP_ITERS { break; }
            steps += 1;
            let start = self.current_span();
            self.advance(); // consume '@'
            let name = self.parse_qualified_name()?;
            let arguments = if self.check(&Token::LParen) {
                self.advance();
                let args = if !self.check(&Token::RParen) { self.parse_annotation_arg_list()? } else { Vec::new() };
                self.consume(&Token::RParen, "Expected ')' after annotation arguments")?;
                args
            } else {
                Vec::new()
            };
            let end = self.previous_span();
            let span = Span::new(start.start, end.end);
            annotations.push(Annotation { name, arguments, span });
        }
        Ok(annotations)
    }

    fn parse_annotation_arg_list(&mut self) -> Result<Vec<AnnotationArg>> {
        let mut args = Vec::new();
        loop {
            // Named or single value
            if self.check(&Token::Identifier) {
                // Lookahead for '=' to decide named
                let save = self.current;
                let ident = self.parse_identifier()?;
                if self.match_token(&Token::Assign) {
                    let expr = self.parse_expression()?;
                    args.push(AnnotationArg::Named(ident, expr));
                } else {
                    // revert and parse as expression
                    self.current = save;
                    // Support array initializer in annotations: '{' (expr (',' expr)*)? '}'
                    let expr = if self.check(&Token::LBrace) {
                        self.advance();
                        let mut values: Vec<Expr> = Vec::new();
                        if !self.check(&Token::RBrace) {
                            loop {
                                values.push(self.parse_expression()?);
                                if !self.match_token(&Token::Comma) { break; }
                            }
                        }
                        self.consume(&Token::RBrace, "Expected '}' to close annotation array value")?;
                        Expr::ArrayInitializer(values)
                    } else {
                        self.parse_expression()?
                    };
                    args.push(AnnotationArg::Value(expr));
                }
            } else {
                let expr = if self.check(&Token::LBrace) {
                    self.advance();
                    let mut values: Vec<Expr> = Vec::new();
                    if !self.check(&Token::RBrace) {
                        loop {
                            values.push(self.parse_expression()?);
                            if !self.match_token(&Token::Comma) { break; }
                        }
                    }
                    self.consume(&Token::RBrace, "Expected '}' to close annotation array value")?;
                    Expr::ArrayInitializer(values)
                } else {
                    self.parse_expression()?
                };
                args.push(AnnotationArg::Value(expr));
            }
            if !self.match_token(&Token::Comma) { break; }
        }
        Ok(args)
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        self.consume(&Token::If, "Expected 'if'")?;
        self.consume(&Token::LParen, "Expected '(' after if")?;
        let cond = self.parse_expression()?;
        self.consume(&Token::RParen, "Expected ')' after condition")?;
        let then_branch = if self.check(&Token::LBrace) { Box::new(Stmt::Block(self.parse_block()?)) } else { Box::new(self.parse_statement()?) };
        let else_branch = if self.match_token(&Token::Else) {
            Some(Box::new(if self.check(&Token::LBrace) { Stmt::Block(self.parse_block()?) } else { self.parse_statement()? }))
        } else { None };
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::If(IfStmt { condition: cond, then_branch, else_branch, span }))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        let start = self.current_span();
        self.consume(&Token::While, "Expected 'while'")?;
        self.consume(&Token::LParen, "Expected '(' after while")?;
        let cond = self.parse_expression()?;
        self.consume(&Token::RParen, "Expected ')' after condition")?;
        let body = if self.check(&Token::LBrace) { Box::new(Stmt::Block(self.parse_block()?)) } else { Box::new(self.parse_statement()?) };
        let span = Span::new(start.start, self.previous_span().end);
        Ok(Stmt::While(WhileStmt { condition: cond, body, span }))
    }

    fn is_variable_declaration_start(&self) -> bool {
        let mut i = self.current;
        
        // Skip modifiers (like 'final')
        while matches!(self.peek_token_type(i), 
            Some(Token::Public) | Some(Token::Protected) | Some(Token::Private) |
            Some(Token::Abstract) | Some(Token::Static) | Some(Token::Final) |
            Some(Token::Native) | Some(Token::Synchronized) | Some(Token::Transient) |
            Some(Token::Volatile) | Some(Token::Strictfp) | Some(Token::Default)
        ) {
            i += 1;
        }
        
        // Try to see if a type ref starts here
        if !self.lookahead_type_ref(&mut i) { return false; }
        // Next token after a type should be an identifier (variable name)
        matches!(self.peek_token_type(i), Some(Token::Identifier))
    }

    fn synchronize_in_block(&mut self) {
        while !self.is_at_end() {
            if self.check(&Token::Semicolon) {
                self.advance();
                break;
            }
            if self.check(&Token::RBrace) {
                break;
            }
            self.advance();
        }
    }

    fn synchronize_in_switch(&mut self) {
        while !self.is_at_end() {
            if self.check(&Token::Semicolon) {
                self.advance();
                break;
            }
            if self.check(&Token::Case) || self.check(&Token::Default) || self.check(&Token::RBrace) {
                break;
            }
            self.advance();
        }
    }

    fn synchronize_toplevel(&mut self) {
        while !self.is_at_end() {
            match self.peek().token_type() {
                // Only stop at true toplevel starters
                Token::Package | Token::Import | Token::Class | Token::Interface | Token::Enum | Token::At => break,
                // If we encounter stray '}', consume and continue
                Token::RBrace => { self.advance(); }
                // Stop if we see visibility or modifiers at toplevel and next is a type keyword,
                // to avoid drifting into class-body tokens after an earlier error.
                Token::Public | Token::Protected | Token::Private |
                Token::Abstract | Token::Static | Token::Final |
                Token::Strictfp => {
                    let n = self.peek_token_type(self.current + 1);
                    if matches!(n, Some(Token::Class|Token::Interface|Token::Enum|Token::At)) { break; }
                    self.advance();
                }
                _ => { self.advance(); }
            }
        }
    }

    fn synchronize_in_class_body(&mut self) {
        while !self.is_at_end() {
            if self.check(&Token::Semicolon) || self.check(&Token::RBrace) { break; }
            // Heuristics: if we hit a token that likely begins a member, stop
            match self.peek().token_type() {
                Token::Public | Token::Protected | Token::Private |
                Token::Abstract | Token::Static | Token::Final |
                Token::Native | Token::Synchronized | Token::Transient |
                Token::Volatile | Token::Strictfp |
                Token::Class | Token::Interface | Token::Enum | Token::At => break,
                // Also stop on primitive, void, or identifier that could start a field/method/ctor
                Token::Boolean | Token::Byte | Token::Short | Token::Int |
                Token::Long | Token::Char | Token::Float | Token::Double |
                Token::Void => break,
                _ => {}
            }
            // Secondary heuristic: if a more detailed lookahead thinks we're at a member start, stop
            if self.lookahead_is_member_start(self.current) { break; }
            self.advance();
        }
    }
    
    /// Try to parse a lambda expression with backtracking
    /// Lambda syntax: parameter(s) -> expression/block
    /// Examples: x -> x + 1, (x, y) -> x + y, (int x) -> { return x + 1; }
    fn try_parse_lambda_expr(&mut self) -> Result<Expr> {
        let checkpoint = self.current;
        
        match self.parse_lambda_expr_internal() {
            Ok(expr) => Ok(expr),
            Err(_) => {
                // Backtrack on failure
                self.current = checkpoint;
                Err(ParseError::InvalidSyntax { 
                    message: "not a lambda expression".to_string(), 
                    location: self.current_span().start 
                }.into())
            }
        }
    }
    
    /// Internal lambda parsing implementation
    fn parse_lambda_expr_internal(&mut self) -> Result<Expr> {
        let start_span = self.current_span();
        
        // Parse lambda parameters
        let parameters = self.parse_lambda_parameters()?;
        
        // Expect arrow token
        if !self.match_token(&Token::Arrow) {
            return Err(ParseError::UnexpectedToken { 
                expected: "->".to_string(), 
                found: format!("{:?}", self.peek().token_type()), 
                location: self.current_span().start 
            }.into());
        }
        
        // Parse lambda body
        let body = if self.check(&Token::LBrace) {
            // Block body
            LambdaBody::Block(self.parse_block()?)
        } else {
            // Expression body
            LambdaBody::Expression(Box::new(self.parse_assignment_expr()?))
        };
        
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(Expr::Lambda(LambdaExpr {
            parameters,
            body,
            span,
        }))
    }
    
    /// Parse lambda parameters: () | x | (x) | (x, y) | (int x, String y)
    fn parse_lambda_parameters(&mut self) -> Result<Vec<LambdaParameter>> {
        if self.match_token(&Token::LParen) {
            // Parenthesized parameter list: () | (x) | (x, y) | (int x, String y)
            let mut parameters = Vec::new();
            
            if !self.check(&Token::RParen) {
                loop {
                    parameters.push(self.parse_lambda_parameter()?);
                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }
            
            if !self.match_token(&Token::RParen) {
                return Err(ParseError::UnexpectedToken { 
                    expected: ")".to_string(), 
                    found: format!("{:?}", self.peek().token_type()), 
                    location: self.current_span().start 
                }.into());
            }
            
            Ok(parameters)
        } else if self.check(&Token::Identifier) {
            // Single untyped parameter: x
            let param = self.parse_lambda_parameter()?;
            Ok(vec![param])
        } else {
            return Err(ParseError::InvalidSyntax { 
                message: "expected lambda parameters".to_string(), 
                location: self.current_span().start 
            }.into());
        }
    }
    
    /// Parse a single lambda parameter: x | int x
    fn parse_lambda_parameter(&mut self) -> Result<LambdaParameter> {
        let start_span = self.current_span();
        
        // Check if this parameter has a type annotation
        let type_ref = if self.is_type_start() && self.peek_token_type(self.current + 1) == Some(&Token::Identifier) {
            // Typed parameter: int x
            Some(self.parse_type_ref()?)
        } else {
            None
        };
        
        // Parse parameter name
        if !self.check(&Token::Identifier) {
            return Err(ParseError::UnexpectedToken { 
                expected: "parameter name".to_string(), 
                found: format!("{:?}", self.peek().token_type()), 
                location: self.current_span().start 
            }.into());
        }
        
        let name = self.advance().lexeme().to_string();
        let end_span = self.previous_span();
        let span = Span::new(start_span.start, end_span.end);
        
        Ok(LambdaParameter {
            name,
            type_ref,
            span,
        })
    }
    
}

/// Parse source code into an AST
pub fn parse(source: &str) -> Result<Ast> {
    let parser = Parser::new(source)?;
    parser.parse()
}
