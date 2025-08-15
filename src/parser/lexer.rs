use logos::Logos;
use crate::ast::Location;

/// Token types for Terminos Language
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    #[token("package")]
    Package,
    #[token("import")]
    Import,
    #[token("static")]
    Static,
    #[token("public")]
    Public,
    #[token("protected")]
    Protected,
    #[token("private")]
    Private,
    #[token("abstract")]
    Abstract,
    #[token("final")]
    Final,
    #[token("native")]
    Native,
    #[token("synchronized")]
    Synchronized,
    #[token("transient")]
    Transient,
    #[token("volatile")]
    Volatile,
    #[token("strictfp")]
    Strictfp,
    #[token("class")]
    Class,
    #[token("interface")]
    Interface,
    #[token("enum")]
    Enum,
    #[token("extends")]
    Extends,
    #[token("implements")]
    Implements,
    #[token("new")]
    New,
    #[token("this")]
    This,
    #[token("super")]
    Super,
    #[token("instanceof")]
    InstanceOf,
    #[token("void")]
    Void,
    #[token("boolean")]
    Boolean,
    #[token("byte")]
    Byte,
    #[token("short")]
    Short,
    #[token("int")]
    Int,
    #[token("long")]
    Long,
    #[token("char")]
    Char,
    #[token("float")]
    Float,
    #[token("double")]
    Double,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("do")]
    Do,
    #[token("switch")]
    Switch,
    #[token("case")]
    Case,
    #[token("default")]
    Default,
    #[token("assert")]
    Assert,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("throw")]
    Throw,
    #[token("throws")]
    Throws,
    #[token("try")]
    Try,
    #[token("catch")]
    Catch,
    #[token("finally")]
    Finally,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,
    
    // Operators
    #[token("=")]
    Assign,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    ModAssign,
    #[token("&=")]
    AndAssign,
    #[token("|=")]
    OrAssign,
    #[token("^=")]
    XorAssign,
    #[token("<<=")]
    LShiftAssign,
    #[token(">>=")]
    RShiftAssign,
    #[token(">>>=")]
    URShiftAssign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("++")]
    Inc,
    #[token("--")]
    Dec,
    #[token("!")]
    Bang,
    #[token("~")]
    Tilde,
    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("<<")]
    LShift,
    #[token(">>")]
    RShift,
    #[token(">>>")]
    URShift,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    PipePipe,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token("->")]
    Arrow,
    
    // Separators
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("@")]
    At,
    #[token("...")]
    Ellipsis,
    
    // Literals
    #[regex(r#""([^"\\]|\\u[0-9a-fA-F]{4}|\\.)*""#)]
    StringLiteral,
    // Support standard escapes and Unicode escapes in character literals
    #[regex(r"'([^'\\]|\\u[0-9a-fA-F]{4}|\\.)'")]
    CharLiteral,
    #[regex(r"0[xX][0-9a-fA-F]+[lL]?")]
    HexInteger,
    #[regex(r"0[bB][01]+")]
    BinaryInteger,
    #[regex(r"0[0-7]+")]
    OctalInteger,
    #[regex(r"[0-9][0-9_]*[lL]?")]
    DecimalInteger,
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?")]
    FloatLiteral,
    #[regex(r"[0-9]+[eE][+-]?[0-9]+")]
    ScientificFloat,
    #[regex(r"[0-9]+[lL]")]
    LongLiteral,
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?[fFdD]")]
    TypedFloat,
    
    // Identifiers
    #[regex(r"[a-zA-Z_$][a-zA-Z0-9_$]*")]
    Identifier,
    
    // Comments and whitespace
    #[regex(r"//[^\n]*")]
    LineComment,
    // Block/Javadoc comment (handles /**...*/, /*...*/, and multiple '*')
    // Pattern adapted from a common C-style comment regex
    #[regex(r"/\*[^*]*\*+([^/*][^*]*\*+)*/", priority = 2)]
    BlockComment,
    #[regex(r"[ \t\n\r]+", priority = 2)]
    Whitespace,

    // Unicode BOM (Byte Order Mark) - treat as ignorable whitespace
    #[token("\u{FEFF}")]
    Bom,
}

impl Token {
    /// Check if this token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(self,
            Token::Package | Token::Import | Token::Static |
            Token::Public | Token::Protected | Token::Private |
            Token::Abstract | Token::Final | Token::Native |
            Token::Synchronized | Token::Transient | Token::Volatile |
            Token::Strictfp | Token::Default | Token::Class | Token::Interface |
            Token::Enum | Token::Extends | Token::Implements |
            Token::New | Token::This | Token::Super |
            Token::InstanceOf | Token::Void | Token::Boolean |
            Token::Byte | Token::Short | Token::Int |
            Token::Long | Token::Char | Token::Float |
            Token::Double | Token::If | Token::Else |
            Token::For | Token::While | Token::Do |
            Token::Switch | Token::Case |
            Token::Break | Token::Continue | Token::Return |
            Token::Throw | Token::Throws | Token::Try |
            Token::Catch | Token::Finally | Token::True |
            Token::False | Token::Null
        )
    }
    
    /// Check if this token is a modifier
    pub fn is_modifier(&self) -> bool {
        matches!(self,
            Token::Public | Token::Protected | Token::Private |
            Token::Abstract | Token::Final | Token::Native |
            Token::Synchronized | Token::Transient | Token::Volatile |
            Token::Static | Token::Strictfp
        )
    }
    
    /// Check if this token is a primitive type
    pub fn is_primitive_type(&self) -> bool {
        matches!(self,
            Token::Boolean | Token::Byte | Token::Short |
            Token::Int | Token::Long | Token::Char |
            Token::Float | Token::Double | Token::Void
        )
    }
    
    /// Check if this token is a literal
    pub fn is_literal(&self) -> bool {
        matches!(self,
            Token::StringLiteral | Token::CharLiteral |
            Token::HexInteger | Token::BinaryInteger |
            Token::OctalInteger | Token::DecimalInteger |
            Token::FloatLiteral | Token::ScientificFloat |
            Token::LongLiteral | Token::TypedFloat |
            Token::True | Token::False | Token::Null
        )
    }
    
    /// Check if this token is an operator
    pub fn is_operator(&self) -> bool {
        matches!(self,
            Token::Assign | Token::AddAssign | Token::SubAssign |
            Token::MulAssign | Token::DivAssign | Token::ModAssign |
            Token::AndAssign | Token::OrAssign | Token::XorAssign |
            Token::LShiftAssign | Token::RShiftAssign | Token::URShiftAssign |
            Token::Plus | Token::Minus | Token::Star | Token::Slash |
            Token::Percent | Token::Inc | Token::Dec | Token::Bang |
            Token::Tilde | Token::Amp | Token::Pipe | Token::Caret |
            Token::LShift | Token::RShift | Token::URShift |
            Token::AndAnd | Token::PipePipe | Token::Eq | Token::Ne |
            Token::Lt | Token::Le | Token::Gt | Token::Ge |
            Token::Question | Token::Colon | Token::Arrow
        )
    }
}

/// Lexical token with location information
#[derive(Debug, Clone)]
pub struct LexicalToken {
    pub token: Token,
    pub lexeme: String,
    pub location: Location,
}

impl LexicalToken {
    pub fn new(token: Token, lexeme: String, location: Location) -> Self {
        Self { token, lexeme, location }
    }
    
    /// Get the token type
    pub fn token_type(&self) -> &Token {
        &self.token
    }
    
    /// Get the lexeme (actual text)
    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }
    
    /// Get the location
    pub fn location(&self) -> Location {
        self.location
    }
    
    /// Check if this token matches the given token type
    pub fn is(&self, token_type: &Token) -> bool {
        std::mem::discriminant(&self.token) == std::mem::discriminant(token_type)
    }
}

/// Lexer for Terminos Language
pub struct Lexer<'a> {
    lexer: logos::Lexer<'a, Token>,
    current_line: usize,
    current_column: usize,
    current_offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Token::lexer(source),
            current_line: 1,
            current_column: 1,
            current_offset: 0,
        }
    }
    
    /// Get the next token
    pub fn next_token(&mut self) -> Option<Result<LexicalToken, String>> {
        let token = self.lexer.next()?;
        
        match token {
            Ok(token) => {
                let lexeme = self.lexer.slice().to_string();
                let location = Location::new(
                    self.current_line,
                    self.current_column,
                    self.current_offset,
                );
                
                self.update_position(&lexeme);
                
                Some(Ok(LexicalToken::new(token, lexeme, location)))
            }
            Err(_) => {
                // Consume one character to avoid infinite loop on unknown input
                let slice = self.lexer.slice().to_string();
                if slice.is_empty() {
                    // advance by one char manually
                    if let Some(first) = self.lexer.remainder().chars().next() {
                        self.update_position(&first.to_string());
                    }
                }
                Some(Err(format!("Lexical error at {}:{}", self.current_line, self.current_column)))
            },
        }
    }
    
    /// Update the current position based on the lexeme
    fn update_position(&mut self, lexeme: &str) {
        for ch in lexeme.chars() {
            match ch {
                '\n' => {
                    self.current_line += 1;
                    self.current_column = 1;
                }
                '\r' => {
                    // Handle \r\n sequence
                    if self.current_column == 1 {
                        self.current_line -= 1;
                    }
                }
                _ => {
                    self.current_column += 1;
                }
            }
            self.current_offset += ch.len_utf8();
        }
    }
    
    /// Get all tokens from the source
    pub fn tokenize(mut self) -> Result<Vec<LexicalToken>, String> {
        let mut tokens = Vec::new();
        
        while let Some(result) = self.next_token() {
            match result {
                Ok(token) => {
                    // Skip whitespace, BOM and comments
                    if !matches!(token.token, Token::Whitespace | Token::Bom | Token::LineComment | Token::BlockComment) {
                        tokens.push(token);
                    }
                }
                Err(e) => return Err(e),
            }
        }
        
        Ok(tokens)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<LexicalToken, String>;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_lexer_keywords() {
        let source = "public class Test extends Object implements Interface";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        
        assert_eq!(tokens.len(), 7);
        assert!(tokens[0].is(&Token::Public));
        assert!(tokens[1].is(&Token::Class));
        assert!(tokens[2].is(&Token::Identifier));
        assert!(tokens[3].is(&Token::Extends));
        assert!(tokens[4].is(&Token::Identifier));
        assert!(tokens[5].is(&Token::Implements));
        assert!(tokens[6].is(&Token::Identifier));
    }
    
    #[test]
    fn test_lexer_literals() {
        let source = r#"42 "hello" 'a' true false null"#;
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        
        assert_eq!(tokens.len(), 6);
        assert!(tokens[0].is(&Token::DecimalInteger));
        assert!(tokens[1].is(&Token::StringLiteral));
        assert!(tokens[2].is(&Token::CharLiteral));
        assert!(tokens[3].is(&Token::True));
        assert!(tokens[4].is(&Token::False));
        assert!(tokens[5].is(&Token::Null));
    }
    
    #[test]
    fn test_lexer_operators() {
        let source = "+ - * / % = += -= *= /= %=";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        
        assert_eq!(tokens.len(), 11);
        assert!(tokens[0].is(&Token::Plus));
        assert!(tokens[1].is(&Token::Minus));
        assert!(tokens[2].is(&Token::Star));
        assert!(tokens[3].is(&Token::Slash));
        assert!(tokens[4].is(&Token::Percent));
        assert!(tokens[5].is(&Token::Assign));
        assert!(tokens[6].is(&Token::AddAssign));
        assert!(tokens[7].is(&Token::SubAssign));
        assert!(tokens[8].is(&Token::MulAssign));
        assert!(tokens[9].is(&Token::DivAssign));
        assert!(tokens[10].is(&Token::ModAssign));
    }
    
    #[test]
    fn test_lexer_comments() {
        let source = "// This is a comment\n/* This is a block comment */";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        
        // Comments should be skipped
        assert_eq!(tokens.len(), 0);
    }
}
