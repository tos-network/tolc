use clap::{Parser, Subcommand};
use std::path::PathBuf;
use anyhow::Result;
use tolc::Config;
use tolc::parser::{parse_tol, parse_java};
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
    /// Compile Java source file(s) to .class files
    Compile {
        /// Input Java source file (.java) or .tol file (legacy)
        #[arg(value_name = "FILE")]
        input: PathBuf,
        
        /// Output directory for .class files
        #[arg(short, long, value_name = "DIR")]
        output: Option<PathBuf>,
        
        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
        /// Target Java classfile version (6..17)
        #[arg(long, value_name = "N", default_value_t = 8)]
        target_version: u8,

        /// Emit StackMapTable frames with extra diagnostics
        #[arg(long, default_value_t = false)]
        debug_frames: bool,

        /// Do not emit StackMapTable frames
        #[arg(long, default_value_t = false)]
        no_frames: bool,
    },
    
    /// Parse Java source file and show the AST
    Parse {
        /// Input Java source file (.java) or .tol file (legacy)
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
        Commands::Compile { input, output, verbose, target_version, debug_frames, no_frames } => {
            compile_file(input, output.as_ref(), *verbose, *target_version, *debug_frames, *no_frames)?;
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

fn compile_file(input: &PathBuf, output: Option<&PathBuf>, verbose: bool, target_version: u8, debug_frames: bool, no_frames: bool) -> Result<()> {
    if verbose {
        println!("Compiling {}...", input.display());
    }
    
    let default_output = PathBuf::from(".");
    let output_dir = output.unwrap_or(&default_output);
    if !output_dir.exists() {
        fs::create_dir_all(output_dir)?;
    }
    
    let output_str = output_dir.to_string_lossy();

    let mut config = Config::default()
        .with_target_java_version(target_version)
        .with_verbose(verbose)
        .with_output_dir(output_dir.clone());

    if debug_frames { config = config.with_debug(true); }
    if no_frames { config = config.with_emit_frames(false); }

    config.validate()?;

    // Determine file type and use appropriate compilation method
    let input_str = input.to_string_lossy();
    
    if input_str.ends_with(".java") {
        // Use new Java compilation pipeline
        tolc::compile_file(&input_str, &output_str, &config)
            .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))?;
    } else if input_str.ends_with(".tol") {
        // Use legacy .tol compilation
        if verbose {
            println!("Note: Using legacy .tol compilation. Consider using .java files for full pipeline support.");
        }
        
        let source = fs::read_to_string(input)?;
        let ast = parse_tol(&source)?;
        generate_bytecode(&ast, &output_str, &config, None)?;
    } else {
        return Err(anyhow::anyhow!("Unsupported file type. Expected .java or .tol file."));
    }
    
    if verbose {
        println!("Compilation successful! Output directory: {}", output_dir.display());
    }
    
    Ok(())
}

fn parse_file(input: &PathBuf, detailed: bool) -> Result<()> {
    let source = fs::read_to_string(input)?;
    let input_str = input.to_string_lossy();
    
    let ast = if input_str.ends_with(".java") {
        parse_java(&source)?
    } else if input_str.ends_with(".tol") {
        println!("Note: Parsing legacy .tol file. Consider using .java files.");
        parse_tol(&source)?
    } else {
        return Err(anyhow::anyhow!("Unsupported file type. Expected .java or .tol file."));
    };
    
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
