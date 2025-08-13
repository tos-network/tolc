use std::fs;
use std::path::PathBuf;
use tolc::parser::Lexer;
use tolc::parser::parse_tol_lenient as parse_tol;

fn class_java_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/java/lang/Class.java")
}

#[test]
fn debug_lex_and_parse_class_java() {
    let path = class_java_path();
    let source = fs::read_to_string(&path).expect("read Class.java");

    // Print lines 690..=710
    let lines: Vec<&str> = source.lines().collect();
    let start = 689.min(lines.len().saturating_sub(1));
    let end = 709.min(lines.len().saturating_sub(1));
    eprintln!("\n--- lines {}..{} ---", start+1, end+1);
    for (i, line) in lines[start..=end].iter().enumerate() {
        eprintln!("{:4}: {}", start + 1 + i, line);
    }
    eprintln!("---------------------\n");

    // Try lexing step-by-step to find first error and print context
    let mut lexer = Lexer::new(&source);
    let mut count = 0usize;
    while let Some(res) = lexer.next_token() {
        match res {
            Ok(tok) => {
                count += 1;
                if count <= 10 {
                    let lex = tok.lexeme();
                    let head = if lex.len() > 20 { &lex[..20] } else { lex };
                    eprintln!("tok {:04} {:>12?} {:?} @{}:{}", count, tok.token_type(), head, tok.location().line, tok.location().column);
                }
            }
            Err(e) => {
                eprintln!("lexer error: {} (after {} tokens)", e, count);
                // Extract the reported line/col from message
                if let Some((line, col)) = e.strip_prefix("Lexical error at ")
                    .and_then(|s| s.split_once(":"))
                    .and_then(|(l, r)| l.parse::<usize>().ok().zip(r.parse::<usize>().ok())) {
                    let mut cur_line = 1usize;
                    let mut cur_col = 1usize;
                    for (i, ch) in source.chars().enumerate() {
                        if cur_line == line && cur_col == col {
                            eprintln!("char at {}:{} = U+{:04X} '{}' (index {})", line, col, ch as u32, ch, i);
                            // print previous and next few chars
                            let prev: String = source.chars().skip(i.saturating_sub(5)).take(5).collect();
                            let next: String = source.chars().skip(i).take(10).collect();
                            eprintln!("context prev='{}' next='{}'", prev.escape_debug(), next.escape_debug());
                            break;
                        }
                        if ch == '\n' { cur_line += 1; cur_col = 1; } else { cur_col += 1; }
                    }
                }
                panic!("parse Class.java: {:?}", e);
            }
        }
    }

    // Try parse to trigger error location
    let _ = parse_tol(&source).expect("parse Class.java");
}
