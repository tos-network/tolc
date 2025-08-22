use tolc::codegen::{
    OptimizedConstantPool, OptimizedBytecodeBuffer, InstructionCache, PerformanceMetrics
};
use std::time::Duration;

#[test]
fn test_optimized_constant_pool() {
    let mut pool = OptimizedConstantPool::new();
    
    // Test string interning
    let str1 = pool.add_utf8_optimized("test").unwrap();
    let str2 = pool.add_utf8_optimized("test").unwrap();
    assert_eq!(str1, str2, "Same strings should return same index");
    
    // Test class optimization
    let class1 = pool.add_class_optimized("java/lang/Object").unwrap();
    let class2 = pool.add_class_optimized("java/lang/Object").unwrap();
    assert_eq!(class1, class2, "Same classes should return same index");
    
    // Test serialization
    let serialized = pool.serialize_optimized();
    assert!(!serialized.is_empty(), "Serialized pool should not be empty");
    
    // Test size tracking
    let total_size = pool.total_serialized_size();
    assert!(total_size > 0, "Total size should be tracked");
}

#[test]
fn test_optimized_bytecode_buffer() {
    let mut buffer = OptimizedBytecodeBuffer::new();
    
    // Test basic operations
    buffer.emit_op(1); // nop
    buffer.emit_op(3); // iconst_0
    buffer.emit_op(60); // istore_1
    
    assert_eq!(buffer.instruction_count, 3, "Buffer should track instruction count");
    assert!(buffer.max_stack_depth() > 0, "Should track max stack depth");
    
    // Test constant loading optimization
    buffer.emit_load_constant(42i32);
    assert!(buffer.instruction_count > 3, "Constant loading should add instructions");
    
    // Test array load optimization
    buffer.emit_array_load(10); // T_INT
    
    // Test serialization
    let bytes = buffer.as_bytes();
    assert!(!bytes.is_empty(), "Buffer should serialize to bytes");
}

#[test]
fn test_instruction_cache() {
    let cache = InstructionCache::new();
    
    // Test cache initialization (cache is pre-populated with common sequences)
    assert!(!cache.is_empty(), "New cache should be pre-populated with common sequences");
    
    // Test cache functionality (basic structure test)
    // Note: More detailed tests would require the cache to be used in actual generation
}

#[test]
fn test_performance_metrics() {
    // Create metrics using Default trait (since new() is private)
    let metrics = PerformanceMetrics::default();
    
    // Test compilation time recording
    metrics.record_compile_time(Duration::from_millis(100));
    
    // Test method compilation recording
    metrics.record_method_compiled(256);
    
    // Test optimization recording
    metrics.record_constant_folded();
    metrics.record_branch_optimized();
    metrics.record_loop_optimized();
    
    // Test memory tracking
    metrics.record_allocation(1024);
    
    // Test report generation
    let report = metrics.get_report();
    assert!(report.total_compile_time.as_millis() > 0, "Report should track total time");
    assert!(report.methods_compiled > 0, "Report should track methods compiled");
    assert!(report.optimizations.constants_folded > 0, "Report should track optimizations");
}

#[test]
fn test_optimization_integration() {
    // Integration test showing optimizations working together
    let mut pool = OptimizedConstantPool::new();
    let mut buffer = OptimizedBytecodeBuffer::new();
    let metrics = PerformanceMetrics::default();
    
    // Record compilation time
    metrics.record_compile_time(Duration::from_millis(50));
    
    // Add some constants to pool
    pool.add_utf8_optimized("Hello").unwrap();
    pool.add_utf8_optimized("World").unwrap();
    pool.add_class_optimized("Test").unwrap();
    
    // Generate some bytecode
    buffer.emit_op(1); // nop
    buffer.emit_load_constant(100i32);
    buffer.emit_array_load(10); // T_INT
    
    metrics.record_method_compiled(buffer.instruction_count as usize);
    metrics.record_constant_folded();
    
    // Verify everything worked
    assert!(pool.len() > 0, "Pool should have constants");
    assert!(buffer.instruction_count > 0, "Buffer should have instructions");
    
    let report = metrics.get_report();
    assert!(report.total_compile_time.as_millis() > 0, "Should track compilation time");
    assert!(report.methods_compiled > 0, "Should track method compilation");
}