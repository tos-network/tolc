use tolc::codegen::jsr_ret_optimizer::{JsrRetOptimizer, OptimizationStrategy};
use tolc::codegen::fatcode_manager::JumpType;
use tolc::codegen::enhanced_string_optimizer::{EnhancedStringOptimizer, OptimizationStrategy as StringOptStrategy};
use tolc::codegen::opcode_enum::Opcode;
use tolc::ast::*;

#[test]
fn test_jsr_ret_basic_functionality() {
    let mut optimizer = JsrRetOptimizer::new();
    
    // Test subroutine creation and JSR generation
    optimizer.create_subroutine("finally_sub", 100).unwrap();
    let jsr_bytecode = optimizer.generate_jsr_call("finally_sub", 100).unwrap();
    
    assert_eq!(jsr_bytecode[0], Opcode::Jsr.to_byte());
    assert_eq!(optimizer.current_jsr_count, 1);
    
    // Test RET generation
    let ret_bytecode = optimizer.generate_ret_instruction(5).unwrap();
    assert_eq!(ret_bytecode[0], Opcode::Ret.to_byte());
    assert_eq!(ret_bytecode[1], 5);
    
    // Test wide RET for large local variable index
    let wide_ret_bytecode = optimizer.generate_ret_instruction(300).unwrap();
    assert_eq!(wide_ret_bytecode[0], Opcode::Wide.to_byte());
    assert_eq!(wide_ret_bytecode[1], Opcode::Ret.to_byte());
    
    // Finalize subroutine
    optimizer.finalize_subroutine("finally_sub", 200).unwrap();
    let subroutine = optimizer.subroutines.get("finally_sub").unwrap();
    assert_eq!(subroutine.size_bytes, 100);
    assert_eq!(subroutine.call_count, 1);
    
    println!("✅ JSR/RET basic functionality test passed");
}

#[test]
fn test_jsr_limit_enforcement() {
    let mut optimizer = JsrRetOptimizer::with_jsr_limit(3);
    
    // Should succeed for first 3 JSR calls
    optimizer.generate_jsr_call("sub1", 100).unwrap();
    optimizer.generate_jsr_call("sub2", 200).unwrap();
    optimizer.generate_jsr_call("sub3", 300).unwrap();
    
    assert_eq!(optimizer.current_jsr_count, 3);
    assert!(optimizer.is_jsr_limit_reached());
    
    // Should fail on 4th JSR call
    assert!(optimizer.generate_jsr_call("sub4", 400).is_err());
    
    println!("✅ JSR limit enforcement test passed");
}

#[test]
fn test_finally_optimization_decision() {
    let mut optimizer = JsrRetOptimizer::new();
    
    // Create a try-finally statement for testing
    let finally_block = Block {
        statements: vec![
            Stmt::Expression(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    value: Literal::String("cleanup".to_string()),
                    span: Span::from_to(0, 0, 0, 0),
                }),
                span: Span::from_to(0, 0, 0, 0),
            })
        ],
        span: Span::from_to(0, 0, 0, 0),
    };
    
    let try_stmt = TryStmt {
        try_block: Block {
            statements: vec![],
            span: Span::from_to(0, 0, 0, 0),
        },
        resources: vec![], // Add missing resources field
        catch_clauses: vec![],
        finally_block: Some(finally_block),
        span: Span::from_to(0, 0, 0, 0),
    };
    
    // Analyze the try-finally statement
    let optimization = optimizer.analyze_try_finally(&try_stmt).unwrap();
    
    // Small finally blocks should be inlined
    assert_eq!(optimization.strategy, OptimizationStrategy::Inline);
    
    // Test JSR limit exceeded scenario
    optimizer.current_jsr_count = optimizer.jsr_limit;
    let optimization2 = optimizer.analyze_try_finally(&try_stmt).unwrap();
    assert_eq!(optimization2.strategy, OptimizationStrategy::Inline);
    
    println!("✅ Finally optimization decision test passed");
}

#[test]
fn test_string_concatenation_constant_folding() {
    let mut optimizer = EnhancedStringOptimizer::new();
    
    // Create a simple string literal concatenation
    let left = Box::new(Expr::Literal(LiteralExpr {
        value: Literal::String("Hello".to_string()),
        span: Span::from_to(0, 0, 0, 0),
    }));
    let right = Box::new(Expr::Literal(LiteralExpr {
        value: Literal::String(" World".to_string()),
        span: Span::from_to(0, 0, 0, 0),
    }));
    
    let binary_expr = BinaryExpr {
        left,
        right,
        operator: BinaryOp::Add,
        span: Span::from_to(0, 0, 0, 0),
    };
    
    // Analyze the concatenation
    let optimization = optimizer.analyze_string_concatenation(&binary_expr).unwrap();
    
    assert_eq!(optimization.strategy, StringOptStrategy::ConstantFold);
    assert_eq!(optimization.constant_value, Some("Hello World".to_string()));
    assert_eq!(optimization.estimated_final_length, 11);
    assert!(optimization.performance_gain > 80); // High gain for constant folding
    
    println!("✅ String concatenation constant folding test passed");
}

#[test]
fn test_string_builder_optimization() {
    let mut optimizer = EnhancedStringOptimizer::new();
    
    // Simulate a longer concatenation chain that should trigger StringBuilder
    optimizer.concat_analyzer.add_element(tolc::codegen::enhanced_string_optimizer::ConcatenationElement {
        element_type: tolc::codegen::enhanced_string_optimizer::ConcatenationType::StringLiteral,
        estimated_length: 5,
        is_constant: false, // Make it non-constant to avoid folding
        constant_value: None,
        source_expr: None,
    });
    
    optimizer.concat_analyzer.add_element(tolc::codegen::enhanced_string_optimizer::ConcatenationElement {
        element_type: tolc::codegen::enhanced_string_optimizer::ConcatenationType::Integer,
        estimated_length: 3,
        is_constant: false,
        constant_value: None,
        source_expr: None,
    });
    
    optimizer.concat_analyzer.add_element(tolc::codegen::enhanced_string_optimizer::ConcatenationElement {
        element_type: tolc::codegen::enhanced_string_optimizer::ConcatenationType::StringLiteral,
        estimated_length: 4,
        is_constant: false,
        constant_value: None,
        source_expr: None,
    });
    
    // Make optimization decision
    let optimization = optimizer.decide_concatenation_optimization().unwrap();
    
    assert_eq!(optimization.strategy, StringOptStrategy::StringBuilder);
    assert_eq!(optimization.chain_length, 3);
    assert!(optimization.performance_gain > 0);
    assert_eq!(optimization.estimated_final_length, 12);
    
    println!("✅ StringBuilder optimization test passed");
}

#[test]
fn test_string_optimization_bytecode_generation() {
    let mut optimizer = EnhancedStringOptimizer::new();
    
    // Test constant folding bytecode generation
    let constant_optimization = tolc::codegen::enhanced_string_optimizer::StringConcatenationOptimization {
        strategy: StringOptStrategy::ConstantFold,
        chain_length: 2,
        estimated_final_length: 11,
        performance_gain: 90,
        size_change: -10,
        constant_value: Some("Hello World".to_string()),
    };
    
    let bytecode = optimizer.generate_optimized_concatenation(&constant_optimization).unwrap();
    assert!(!bytecode.is_empty());
    assert_eq!(bytecode[0], Opcode::Ldc.to_byte());
    
    // Test StringBuilder bytecode generation
    let stringbuilder_optimization = tolc::codegen::enhanced_string_optimizer::StringConcatenationOptimization {
        strategy: StringOptStrategy::StringBuilder,
        chain_length: 3,
        estimated_final_length: 20,
        performance_gain: 50,
        size_change: 5,
        constant_value: None,
    };
    
    let sb_bytecode = optimizer.generate_optimized_concatenation(&stringbuilder_optimization).unwrap();
    assert!(!sb_bytecode.is_empty());
    assert_eq!(sb_bytecode[0], Opcode::New.to_byte()); // NEW StringBuilder
    
    println!("✅ String optimization bytecode generation test passed");
}

#[test]
fn test_jsr_ret_statistics() {
    let mut optimizer = JsrRetOptimizer::new();
    
    // Create multiple subroutines
    optimizer.create_subroutine("sub1", 100).unwrap();
    optimizer.create_subroutine("sub2", 200).unwrap();
    
    // Generate JSR calls
    optimizer.generate_jsr_call("sub1", 100).unwrap();
    optimizer.generate_jsr_call("sub1", 100).unwrap(); // Call same subroutine twice
    optimizer.generate_jsr_call("sub2", 200).unwrap();
    
    // Generate RET instructions
    optimizer.generate_ret_instruction(1).unwrap();
    optimizer.generate_ret_instruction(2).unwrap();
    
    // Finalize subroutines
    optimizer.finalize_subroutine("sub1", 150).unwrap();
    optimizer.finalize_subroutine("sub2", 250).unwrap();
    
    let stats = optimizer.get_stats();
    assert_eq!(stats.total_subroutines, 2);
    assert_eq!(stats.total_jsr_instructions, 3);
    assert_eq!(stats.total_ret_instructions, 2);
    assert_eq!(stats.jsr_ret_subroutines, 2);
    
    // Check that bytes were saved
    assert!(stats.bytes_saved > 0);
    
    println!("✅ JSR/RET statistics test passed");
}

#[test]
fn test_string_optimization_statistics() {
    let mut optimizer = EnhancedStringOptimizer::new();
    
    // Simulate multiple optimizations
    optimizer.stats.total_concatenations = 10;
    optimizer.stats.string_builder_optimizations = 6;
    optimizer.stats.constant_folds = 3;
    optimizer.stats.capacity_optimizations = 4;
    optimizer.stats.bytes_saved = 150;
    optimizer.stats.calls_eliminated = 20;
    
    let stats = optimizer.get_stats();
    assert_eq!(stats.total_concatenations, 10);
    assert_eq!(stats.string_builder_optimizations, 6);
    assert_eq!(stats.constant_folds, 3);
    assert_eq!(stats.bytes_saved, 150);
    
    // Generate report
    let report = optimizer.generate_report();
    assert!(report.contains("Total Concatenations: 10"));
    assert!(report.contains("StringBuilder Optimizations: 6"));
    assert!(report.contains("Constant Folds: 3"));
    assert!(report.contains("Optimization Rate: 90%")); // (6+3)/10 * 100
    
    println!("✅ String optimization statistics test passed");
}

#[test]
fn test_integration_jsr_ret_and_string_optimization() {
    use tolc::codegen::method_writer::MethodWriter;
    
    let mut method_writer = MethodWriter::new();
    
    // Test JSR/RET integration
    method_writer.create_finally_subroutine("test_finally").unwrap();
    let jsr_bytecode = method_writer.generate_jsr_call("test_finally", 100).unwrap();
    assert!(!jsr_bytecode.is_empty());
    
    let ret_bytecode = method_writer.generate_ret_instruction(5).unwrap();
    assert!(!ret_bytecode.is_empty());
    
    method_writer.finalize_subroutine("test_finally").unwrap();
    
    // Test string optimization integration
    let binary_expr = BinaryExpr {
        left: Box::new(Expr::Literal(LiteralExpr {
            value: Literal::String("Test".to_string()),
            span: Span::from_to(0, 0, 0, 0),
        })),
        right: Box::new(Expr::Literal(LiteralExpr {
            value: Literal::String(" String".to_string()),
            span: Span::from_to(0, 0, 0, 0),
        })),
        operator: BinaryOp::Add,
        span: Span::from_to(0, 0, 0, 0),
    };
    
    let string_optimization = method_writer.optimize_string_concatenation(&binary_expr).unwrap();
    assert_eq!(string_optimization.strategy, StringOptStrategy::ConstantFold);
    
    let optimized_bytecode = method_writer.generate_optimized_string_concat(&string_optimization).unwrap();
    assert!(!optimized_bytecode.is_empty());
    
    // Test JSR limit checking
    assert!(!method_writer.is_jsr_limit_reached());
    
    // Test preference setting
    method_writer.set_prefer_inlining(true);
    
    println!("✅ Integration test for JSR/RET and string optimization passed");
}

#[test]
fn test_complex_string_concatenation_patterns() {
    let mut optimizer = EnhancedStringOptimizer::new();
    
    // Test mixed type concatenation
    optimizer.concat_analyzer.add_element(tolc::codegen::enhanced_string_optimizer::ConcatenationElement {
        element_type: tolc::codegen::enhanced_string_optimizer::ConcatenationType::StringLiteral,
        estimated_length: 6,
        is_constant: true,
        constant_value: Some("Count: ".to_string()),
        source_expr: None,
    });
    
    optimizer.concat_analyzer.add_element(tolc::codegen::enhanced_string_optimizer::ConcatenationElement {
        element_type: tolc::codegen::enhanced_string_optimizer::ConcatenationType::Integer,
        estimated_length: 3,
        is_constant: false, // Variable integer
        constant_value: None,
        source_expr: None,
    });
    
    optimizer.concat_analyzer.add_element(tolc::codegen::enhanced_string_optimizer::ConcatenationElement {
        element_type: tolc::codegen::enhanced_string_optimizer::ConcatenationType::Boolean,
        estimated_length: 4,
        is_constant: true,
        constant_value: Some("true".to_string()),
        source_expr: None,
    });
    
    let optimization = optimizer.decide_concatenation_optimization().unwrap();
    
    // Should use StringBuilder for mixed types
    assert_eq!(optimization.strategy, StringOptStrategy::StringBuilder);
    assert_eq!(optimization.chain_length, 3);
    assert_eq!(optimization.estimated_final_length, 13);
    
    // Generate bytecode
    let bytecode = optimizer.generate_optimized_concatenation(&optimization).unwrap();
    assert!(!bytecode.is_empty());
    
    println!("✅ Complex string concatenation patterns test passed");
}
