use tolc::parser::{Lexer};
use tolc::parser::lexer::Token;

#[test]
fn lexer_keywords() {
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
fn lexer_operators() {
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
fn lexer_comments_are_skipped() {
    let source = "// This is a comment\n/* This is a block comment */";
    let lexer = Lexer::new(source);
    let tokens = lexer.tokenize().expect("Failed to tokenize");

    assert_eq!(tokens.len(), 0);
}
