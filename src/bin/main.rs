use clap::{Parser, Subcommand};
use std::path::PathBuf;
use anyhow::Result;
use tolc::parser::parse_tol;
use tolc::codegen::generate_bytecode;
use std::fs;

#[derive(Parser)]
#[command(name = "tolc")]
#[command(about = "Terminos Language Compiler")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a .tol file to .class files
    Compile {
        /// Input .tol file
        #[arg(value_name = "FILE")]
        input: PathBuf,
        
        /// Output directory for .class files
        #[arg(short, long, value_name = "DIR")]
        output: Option<PathBuf>,
        
        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },
    
    /// Parse a .tol file and show the AST
    Parse {
        /// Input .tol file
        #[arg(value_name = "FILE")]
        input: PathBuf,
        
        /// Show detailed AST information
        #[arg(short, long)]
        detailed: bool,
    },
    
    /// Lexically analyze a .tol file
    Lex {
        /// Input .tol file
        #[arg(value_name = "FILE")]
        input: PathBuf,
        
        /// Show token locations
        #[arg(short, long)]
        locations: bool,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    
    match &cli.command {
        Commands::Compile { input, output, verbose } => {
            compile_file(input, output.as_ref(), *verbose)?;
        }
        Commands::Parse { input, detailed } => {
            parse_file(input, *detailed)?;
        }
        Commands::Lex { input, locations } => {
            lex_file(input, *locations)?;
        }
    }
    
    Ok(())
}

fn compile_file(input: &PathBuf, output: Option<&PathBuf>, verbose: bool) -> Result<()> {
    if verbose {
        println!("Compiling {}...", input.display());
    }
    
    let source = fs::read_to_string(input)?;
    let ast = parse_tol(&source)?;
    
    let default_output = PathBuf::from(".");
    let output_dir = output.unwrap_or(&default_output);
    if !output_dir.exists() {
        fs::create_dir_all(output_dir)?;
    }
    
    let output_str = output_dir.to_string_lossy();
    generate_bytecode(&ast, &output_str, &tolc::config::Config::default())?;
    
    if verbose {
        println!("Compilation successful! Output directory: {}", output_dir.display());
    }
    
    Ok(())
}

fn parse_file(input: &PathBuf, detailed: bool) -> Result<()> {
    let source = fs::read_to_string(input)?;
    let ast = parse_tol(&source)?;
    
    if detailed {
        println!("{:#?}", ast);
    } else {
        println!("{}", ast);
    }
    
    Ok(())
}

fn lex_file(input: &PathBuf, locations: bool) -> Result<()> {
    let source = fs::read_to_string(input)?;
    let lexer = tolc::parser::Lexer::new(&source);
    let tokens = lexer.tokenize().map_err(|e| anyhow::anyhow!("Lexical error: {}", e))?;
    
    for token in tokens {
        if locations {
            println!("{:?} at {}:{}", token.token_type(), token.location().line, token.location().column);
        } else {
            println!("{:?}: '{}'", token.token_type(), token.lexeme());
        }
    }
    
    Ok(())
}
