// Smart Branch Merging Tests - Phase 1.3 完成
// 测试智能分支合并和代码活跃性管理

use tolc::codegen::{Code, State, CodeType};
use tolc::codegen::chain::ChainOps;

#[test]
fn test_smart_state_merging() {
    // 测试智能状态合并功能
    let mut code = Code::new(5, true, true);
    
    // 创建两个不同的状态
    let mut state1 = State::new(5);
    state1.stacksize = 2;
    state1.locals[0] = CodeType::Int;
    state1.locals[1] = CodeType::Object("java/lang/String".to_string());

    let mut state2 = State::new(5);
    state2.stacksize = 2;
    state2.locals[0] = CodeType::Int;
    state2.locals[1] = CodeType::Double;

    // 测试合并相同栈大小但不同本地变量类型的状态
    code.state = state1;
    let result = code.merge_state(&state2);
    
    assert!(result.is_ok());
    // 应该选择Object作为通用类型
    if let CodeType::Object(name) = &code.state.locals[1] {
        assert_eq!(name, "java/lang/Object");
    } else {
        panic!("Expected Object type after merge");
    }
}

#[test]
fn test_dead_code_restoration() {
    // 测试代码死亡后通过跳转恢复
    let mut code = Code::new(5, true, true);
    
    // 标记代码为死亡
    code.mark_dead();
    assert!(!code.is_alive());
    
    // 创建一个跳转链
    let chain = ChainOps::single(10, 1, 5);
    
    // 通过resolve_with_merge恢复代码活跃性
    code.resolve_with_merge(chain);
    assert!(code.is_alive()); // 应该恢复活跃状态
}

#[test]
fn test_stack_size_mismatch_error() {
    // 测试栈大小不匹配时的错误处理
    let mut code = Code::new(5, true, false);
    
    let mut state1 = State::new(5);
    state1.stacksize = 2;

    let mut state2 = State::new(5);
    state2.stacksize = 3; // 不同的栈大小

    code.state = state1;
    let result = code.merge_state(&state2);
    
    assert!(result.is_err());
    if let Err(e) = result {
        match e {
            tolc::error::Error::CodeGen { message } => {
                assert!(message.contains("Stack size mismatch"));
            }
            _ => panic!("Expected CodeGen error")
        }
    }
}

#[test]
fn test_goto_optimization() {
    // 测试goto指令优化
    let mut code = Code::new(5, true, true);
    
    // 模拟goto指令的优化场景
    code.emit1(tolc::codegen::opcodes::GOTO);
    code.emit2(5); // 跳转偏移
    let goto_pc = code.cp - 3;
    
    // 创建跳转链指向goto指令后面
    let chain = ChainOps::single(goto_pc as u16, 1, 5);
    
    // 解析时应该进行优化
    code.resolve_optimized(chain, code.cp as u16);
    
    // 验证代码生成正确
    assert!(code.is_alive());
}

#[test]
fn test_enhanced_aliveness_tracking() {
    // 测试增强的代码活跃性跟踪
    let mut code = Code::new(5, true, true);
    
    // 初始状态应该是活跃的
    assert!(code.is_alive());
    
    // 添加一些指令
    code.emit1(tolc::codegen::opcodes::ICONST_1);
    code.emitop1(tolc::codegen::opcodes::ISTORE, 1);
    
    // 创建条件跳转
    let branch_chain = code.branch(tolc::codegen::opcodes::IFEQ);
    
    // 标记为死亡
    code.mark_dead();
    assert!(!code.alive); // 直接检查alive字段
    
    // 但is_alive应该考虑pending jumps
    if branch_chain.is_some() {
        // 如果有pending jumps，代码仍然被认为是活跃的
        code.pending_jumps = branch_chain;
        assert!(code.is_alive());
    }
}

#[test] 
fn test_entry_point_with_state() {
    // 测试带状态的入口点创建
    let mut code = Code::new(5, true, true);
    
    // 创建初始状态
    let mut initial_state = State::new(5);
    initial_state.stacksize = 1;
    initial_state.locals[0] = CodeType::Int;
    
    // 标记代码为死亡
    code.mark_dead();
    assert!(!code.is_alive());
    
    // 创建带状态的入口点
    let entry_pc = code.entry_point_with_state(initial_state.dup());
    
    // 验证代码恢复活跃且状态正确
    assert!(code.is_alive());
    assert_eq!(code.state.stacksize, 1);
    assert!(matches!(code.state.locals[0], CodeType::Int));
    assert_eq!(entry_pc, code.cp as u16);
}

#[test]
fn test_complex_branch_scenario() {
    // 测试复杂分支场景 - 模拟if-else-if链
    let mut code = Code::new(10, true, true);
    
    // 第一个条件分支
    code.emitop1(tolc::codegen::opcodes::ILOAD, 1);
    let branch1 = code.branch(tolc::codegen::opcodes::IFEQ);
    
    // then分支
    code.emit1(tolc::codegen::opcodes::ICONST_1);
    let goto_end1 = code.branch(tolc::codegen::opcodes::GOTO);
    
    // else分支开始点
    let else_pc = code.entry_point();
    code.resolve(branch1);
    
    // 第二个条件
    code.emitop1(tolc::codegen::opcodes::ILOAD, 2);  
    let branch2 = code.branch(tolc::codegen::opcodes::IFEQ);
    
    // 第二个then分支
    code.emit1(tolc::codegen::opcodes::ICONST_2);
    let goto_end2 = code.branch(tolc::codegen::opcodes::GOTO);
    
    // 最终else分支
    let final_else_pc = code.entry_point();
    code.resolve(branch2);
    code.emit1(tolc::codegen::opcodes::ICONST_3);
    
    // 最终汇合点
    let _end_pc = code.entry_point();
    code.resolve(goto_end1);
    code.resolve(goto_end2);
    
    // 验证所有路径都正确汇合
    assert!(code.is_alive());
    assert!(code.cp > else_pc as usize);
    assert!(code.cp > final_else_pc as usize);
}

#[test]
fn test_javac_style_alive_management() {
    // 测试javac风格的代码活跃性管理
    let mut code = Code::new(5, true, true);
    
    // 模拟javac的isAlive行为
    assert!(code.is_alive()); // 初始活跃
    
    // 执行return指令后应该标记为死亡
    code.emit1(tolc::codegen::opcodes::IRETURN);
    code.mark_dead();
    assert!(!code.alive);
    
    // 但如果有pending jumps，仍然被认为是活跃的
    let pending_chain = ChainOps::single(10, 1, 5);
    code.pending_jumps = pending_chain;
    assert!(code.is_alive()); // javac isAlive行为
    
    // 解析pending jumps后，代码重新活跃
    let chain_to_resolve = code.pending_jumps.take();
    code.resolve(chain_to_resolve);
    assert!(code.is_alive());
}