//! Performance Monitoring and Optimization Analysis
//! 
//! This module provides comprehensive performance tracking for code generation:
//! - Real-time performance metrics collection
//! - Optimization impact analysis
//! - Memory allocation tracking
//! - Benchmarking and profiling utilities

use std::time::{Duration, Instant};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

/// Global performance metrics collector
pub static PERF_METRICS: PerformanceMetrics = PerformanceMetrics::new();

/// Thread-safe performance metrics collection
pub struct PerformanceMetrics {
    /// Total compilation time
    pub total_compile_time: AtomicU64,
    
    /// Number of methods compiled
    pub methods_compiled: AtomicUsize,
    
    /// Total bytecode generated (bytes)
    pub bytecode_generated: AtomicUsize,
    
    /// Constant pool lookups
    pub constant_pool_lookups: AtomicU64,
    
    /// Cache hits for various optimizations
    pub cache_hits: AtomicU64,
    pub cache_misses: AtomicU64,
    
    /// Memory allocations tracked
    pub allocations_count: AtomicUsize,
    pub allocations_bytes: AtomicUsize,
    
    /// Optimization statistics
    pub constants_folded: AtomicUsize,
    pub dead_code_eliminated: AtomicUsize,
    pub branches_optimized: AtomicUsize,
    pub loops_optimized: AtomicUsize,
}

impl Default for PerformanceMetrics {
    fn default() -> Self {
        Self::new()
    }
}

impl PerformanceMetrics {
    pub const fn new() -> Self {
        Self {
            total_compile_time: AtomicU64::new(0),
            methods_compiled: AtomicUsize::new(0),
            bytecode_generated: AtomicUsize::new(0),
            constant_pool_lookups: AtomicU64::new(0),
            cache_hits: AtomicU64::new(0),
            cache_misses: AtomicU64::new(0),
            allocations_count: AtomicUsize::new(0),
            allocations_bytes: AtomicUsize::new(0),
            constants_folded: AtomicUsize::new(0),
            dead_code_eliminated: AtomicUsize::new(0),
            branches_optimized: AtomicUsize::new(0),
            loops_optimized: AtomicUsize::new(0),
        }
    }
    
    /// Record compilation time
    pub fn record_compile_time(&self, duration: Duration) {
        self.total_compile_time.fetch_add(duration.as_nanos() as u64, Ordering::Relaxed);
    }
    
    /// Record method compilation
    pub fn record_method_compiled(&self, bytecode_size: usize) {
        self.methods_compiled.fetch_add(1, Ordering::Relaxed);
        self.bytecode_generated.fetch_add(bytecode_size, Ordering::Relaxed);
    }
    
    /// Record constant pool lookup
    pub fn record_constant_pool_lookup(&self) {
        self.constant_pool_lookups.fetch_add(1, Ordering::Relaxed);
    }
    
    /// Record cache hit
    pub fn record_cache_hit(&self) {
        self.cache_hits.fetch_add(1, Ordering::Relaxed);
    }
    
    /// Record cache miss
    pub fn record_cache_miss(&self) {
        self.cache_misses.fetch_add(1, Ordering::Relaxed);
    }
    
    /// Record memory allocation
    pub fn record_allocation(&self, bytes: usize) {
        self.allocations_count.fetch_add(1, Ordering::Relaxed);
        self.allocations_bytes.fetch_add(bytes, Ordering::Relaxed);
    }
    
    /// Record optimization events
    pub fn record_constant_folded(&self) {
        self.constants_folded.fetch_add(1, Ordering::Relaxed);
    }
    
    pub fn record_dead_code_eliminated(&self) {
        self.dead_code_eliminated.fetch_add(1, Ordering::Relaxed);
    }
    
    pub fn record_branch_optimized(&self) {
        self.branches_optimized.fetch_add(1, Ordering::Relaxed);
    }
    
    pub fn record_loop_optimized(&self) {
        self.loops_optimized.fetch_add(1, Ordering::Relaxed);
    }
    
    /// Get current performance report
    pub fn get_report(&self) -> PerformanceReport {
        PerformanceReport {
            total_compile_time: Duration::from_nanos(self.total_compile_time.load(Ordering::Relaxed)),
            methods_compiled: self.methods_compiled.load(Ordering::Relaxed),
            bytecode_generated: self.bytecode_generated.load(Ordering::Relaxed),
            constant_pool_lookups: self.constant_pool_lookups.load(Ordering::Relaxed),
            cache_hit_rate: self.calculate_cache_hit_rate(),
            memory_allocated: self.allocations_bytes.load(Ordering::Relaxed),
            optimizations: OptimizationStats {
                constants_folded: self.constants_folded.load(Ordering::Relaxed),
                dead_code_eliminated: self.dead_code_eliminated.load(Ordering::Relaxed),
                branches_optimized: self.branches_optimized.load(Ordering::Relaxed),
                loops_optimized: self.loops_optimized.load(Ordering::Relaxed),
            },
        }
    }
    
    fn calculate_cache_hit_rate(&self) -> f64 {
        let hits = self.cache_hits.load(Ordering::Relaxed) as f64;
        let misses = self.cache_misses.load(Ordering::Relaxed) as f64;
        let total = hits + misses;
        
        if total > 0.0 {
            hits / total
        } else {
            0.0
        }
    }
    
    /// Reset all metrics
    pub fn reset(&self) {
        self.total_compile_time.store(0, Ordering::Relaxed);
        self.methods_compiled.store(0, Ordering::Relaxed);
        self.bytecode_generated.store(0, Ordering::Relaxed);
        self.constant_pool_lookups.store(0, Ordering::Relaxed);
        self.cache_hits.store(0, Ordering::Relaxed);
        self.cache_misses.store(0, Ordering::Relaxed);
        self.allocations_count.store(0, Ordering::Relaxed);
        self.allocations_bytes.store(0, Ordering::Relaxed);
        self.constants_folded.store(0, Ordering::Relaxed);
        self.dead_code_eliminated.store(0, Ordering::Relaxed);
        self.branches_optimized.store(0, Ordering::Relaxed);
        self.loops_optimized.store(0, Ordering::Relaxed);
    }
}

/// Performance report snapshot
#[derive(Debug, Clone)]
pub struct PerformanceReport {
    pub total_compile_time: Duration,
    pub methods_compiled: usize,
    pub bytecode_generated: usize,
    pub constant_pool_lookups: u64,
    pub cache_hit_rate: f64,
    pub memory_allocated: usize,
    pub optimizations: OptimizationStats,
}

/// Optimization statistics
#[derive(Debug, Clone)]
pub struct OptimizationStats {
    pub constants_folded: usize,
    pub dead_code_eliminated: usize,
    pub branches_optimized: usize,
    pub loops_optimized: usize,
}

impl PerformanceReport {
    /// Calculate average compilation time per method
    pub fn avg_compile_time_per_method(&self) -> Duration {
        if self.methods_compiled > 0 {
            self.total_compile_time / self.methods_compiled as u32
        } else {
            Duration::ZERO
        }
    }
    
    /// Calculate average bytecode size per method
    pub fn avg_bytecode_size_per_method(&self) -> f64 {
        if self.methods_compiled > 0 {
            self.bytecode_generated as f64 / self.methods_compiled as f64
        } else {
            0.0
        }
    }
    
    /// Calculate throughput (methods per second)
    pub fn methods_per_second(&self) -> f64 {
        if self.total_compile_time > Duration::ZERO {
            self.methods_compiled as f64 / self.total_compile_time.as_secs_f64()
        } else {
            0.0
        }
    }
    
    /// Calculate total optimization impact
    pub fn total_optimizations(&self) -> usize {
        self.optimizations.constants_folded +
        self.optimizations.dead_code_eliminated +
        self.optimizations.branches_optimized +
        self.optimizations.loops_optimized
    }
    
    /// Print detailed performance report
    pub fn print_detailed_report(&self) {
        println!("=== Performance Report ===");
        println!("Total Compile Time: {:?}", self.total_compile_time);
        println!("Methods Compiled: {}", self.methods_compiled);
        println!("Bytecode Generated: {} bytes", self.bytecode_generated);
        println!("Average Time per Method: {:?}", self.avg_compile_time_per_method());
        println!("Average Bytecode Size: {:.2} bytes", self.avg_bytecode_size_per_method());
        println!("Throughput: {:.2} methods/sec", self.methods_per_second());
        println!("Constant Pool Lookups: {}", self.constant_pool_lookups);
        println!("Cache Hit Rate: {:.2}%", self.cache_hit_rate * 100.0);
        println!("Memory Allocated: {} bytes", self.memory_allocated);
        println!();
        println!("=== Optimizations ===");
        println!("Constants Folded: {}", self.optimizations.constants_folded);
        println!("Dead Code Eliminated: {}", self.optimizations.dead_code_eliminated);
        println!("Branches Optimized: {}", self.optimizations.branches_optimized);
        println!("Loops Optimized: {}", self.optimizations.loops_optimized);
        println!("Total Optimizations: {}", self.total_optimizations());
    }
}

/// Timer for measuring compilation phases
pub struct CompilationTimer {
    start_time: Instant,
    phase_name: String,
}

impl CompilationTimer {
    /// Start timing a compilation phase
    pub fn start_phase(phase_name: &str) -> Self {
        Self {
            start_time: Instant::now(),
            phase_name: phase_name.to_string(),
        }
    }
    
    /// End the timing phase and record metrics
    pub fn end_phase(self) -> Duration {
        let duration = self.start_time.elapsed();
        PERF_METRICS.record_compile_time(duration);
        duration
    }
    
    /// Get elapsed time without ending the timer
    pub fn elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }
}

/// Memory allocation tracker
pub struct AllocationTracker {
    initial_allocations: usize,
    initial_bytes: usize,
}

impl AllocationTracker {
    /// Start tracking allocations
    pub fn start() -> Self {
        Self {
            initial_allocations: PERF_METRICS.allocations_count.load(Ordering::Relaxed),
            initial_bytes: PERF_METRICS.allocations_bytes.load(Ordering::Relaxed),
        }
    }
    
    /// Get allocation delta since tracking started
    pub fn get_delta(&self) -> (usize, usize) {
        let current_allocations = PERF_METRICS.allocations_count.load(Ordering::Relaxed);
        let current_bytes = PERF_METRICS.allocations_bytes.load(Ordering::Relaxed);
        
        (
            current_allocations - self.initial_allocations,
            current_bytes - self.initial_bytes,
        )
    }
}

/// Benchmark runner for performance testing
pub struct BenchmarkRunner {
    benchmarks: Vec<Benchmark>,
}

/// Individual benchmark case
pub struct Benchmark {
    name: String,
    setup: Box<dyn Fn() -> Box<dyn BenchmarkCase>>,
}

/// Trait for benchmark test cases
pub trait BenchmarkCase {
    fn run(&mut self) -> Duration;
    fn cleanup(&mut self) {}
}

impl BenchmarkRunner {
    /// Create new benchmark runner
    pub fn new() -> Self {
        Self {
            benchmarks: Vec::new(),
        }
    }
    
    /// Add a benchmark case
    pub fn add_benchmark<F, T>(&mut self, name: &str, setup: F)
    where
        F: Fn() -> T + 'static,
        T: BenchmarkCase + 'static,
    {
        self.benchmarks.push(Benchmark {
            name: name.to_string(),
            setup: Box::new(move || Box::new(setup())),
        });
    }
    
    /// Run all benchmarks
    pub fn run_all(&self) -> BenchmarkResults {
        let mut results = HashMap::new();
        
        for benchmark in &self.benchmarks {
            println!("Running benchmark: {}", benchmark.name);
            
            let mut test_case = (benchmark.setup)();
            let mut times = Vec::new();
            
            // Warm up
            for _ in 0..3 {
                test_case.run();
            }
            
            // Actual benchmark runs
            for _ in 0..10 {
                let duration = test_case.run();
                times.push(duration);
            }
            
            test_case.cleanup();
            
            let stats = BenchmarkStats::from_times(&times);
            results.insert(benchmark.name.clone(), stats);
        }
        
        BenchmarkResults { results }
    }
}

impl Default for BenchmarkRunner {
    fn default() -> Self {
        Self::new()
    }
}

/// Benchmark execution results
pub struct BenchmarkResults {
    results: HashMap<String, BenchmarkStats>,
}

/// Statistics for a single benchmark
#[derive(Debug, Clone)]
pub struct BenchmarkStats {
    pub min: Duration,
    pub max: Duration,
    pub mean: Duration,
    pub median: Duration,
    pub std_dev: Duration,
}

impl BenchmarkStats {
    fn from_times(times: &[Duration]) -> Self {
        let mut sorted_times = times.to_vec();
        sorted_times.sort();
        
        let min = *sorted_times.first().unwrap();
        let max = *sorted_times.last().unwrap();
        
        let total_nanos: u64 = sorted_times.iter().map(|d| d.as_nanos() as u64).sum();
        let mean = Duration::from_nanos(total_nanos / sorted_times.len() as u64);
        
        let median = if sorted_times.len() % 2 == 0 {
            let mid = sorted_times.len() / 2;
            let sum = sorted_times[mid - 1].as_nanos() + sorted_times[mid].as_nanos();
            Duration::from_nanos((sum / 2) as u64)
        } else {
            sorted_times[sorted_times.len() / 2]
        };
        
        // Calculate standard deviation
        let mean_nanos = mean.as_nanos() as f64;
        let variance: f64 = sorted_times.iter()
            .map(|d| {
                let diff = d.as_nanos() as f64 - mean_nanos;
                diff * diff
            })
            .sum::<f64>() / sorted_times.len() as f64;
        
        let std_dev = Duration::from_nanos(variance.sqrt() as u64);
        
        Self {
            min,
            max,
            mean,
            median,
            std_dev,
        }
    }
}

impl BenchmarkResults {
    /// Print benchmark results
    pub fn print_results(&self) {
        println!("\n=== Benchmark Results ===");
        
        for (name, stats) in &self.results {
            println!("\n{}", name);
            println!("  Min:    {:?}", stats.min);
            println!("  Max:    {:?}", stats.max);
            println!("  Mean:   {:?}", stats.mean);
            println!("  Median: {:?}", stats.median);
            println!("  StdDev: {:?}", stats.std_dev);
        }
    }
    
    /// Compare two benchmark results
    pub fn compare_with(&self, other: &BenchmarkResults) {
        println!("\n=== Benchmark Comparison ===");
        
        for (name, stats) in &self.results {
            if let Some(other_stats) = other.results.get(name) {
                let improvement = if other_stats.mean > stats.mean {
                    let speedup = other_stats.mean.as_nanos() as f64 / stats.mean.as_nanos() as f64;
                    format!("{:.2}x faster", speedup)
                } else {
                    let slowdown = stats.mean.as_nanos() as f64 / other_stats.mean.as_nanos() as f64;
                    format!("{:.2}x slower", slowdown)
                };
                
                println!("{}: {} -> {} ({})", name, other_stats.mean.as_micros(), stats.mean.as_micros(), improvement);
            }
        }
    }
}

/// Profiler for hot path analysis
pub struct HotPathProfiler {
    call_counts: HashMap<String, u64>,
    execution_times: HashMap<String, Duration>,
}

impl HotPathProfiler {
    pub fn new() -> Self {
        Self {
            call_counts: HashMap::new(),
            execution_times: HashMap::new(),
        }
    }
    
    /// Record function call
    pub fn record_call(&mut self, function_name: &str, duration: Duration) {
        *self.call_counts.entry(function_name.to_string()).or_insert(0) += 1;
        *self.execution_times.entry(function_name.to_string()).or_insert(Duration::ZERO) += duration;
    }
    
    /// Get hot paths (most frequently called functions)
    pub fn get_hot_paths(&self, limit: usize) -> Vec<(String, u64, Duration)> {
        let mut paths: Vec<_> = self.call_counts.iter()
            .map(|(name, &count)| {
                let total_time = self.execution_times.get(name).cloned().unwrap_or(Duration::ZERO);
                (name.clone(), count, total_time)
            })
            .collect();
        
        paths.sort_by_key(|(_, count, _)| std::cmp::Reverse(*count));
        paths.truncate(limit);
        paths
    }
    
    /// Print profiling results
    pub fn print_profile(&self) {
        println!("\n=== Hot Path Analysis ===");
        
        let hot_paths = self.get_hot_paths(10);
        for (name, count, total_time) in hot_paths {
            let avg_time = if count > 0 {
                total_time / count as u32
            } else {
                Duration::ZERO
            };
            
            println!("{}: {} calls, total: {:?}, avg: {:?}", name, count, total_time, avg_time);
        }
    }
}

impl Default for HotPathProfiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Macro for easy performance measurement
#[macro_export]
macro_rules! measure_time {
    ($phase:expr, $block:block) => {{
        let timer = $crate::codegen::performance_monitor::CompilationTimer::start_phase($phase);
        let result = $block;
        timer.end_phase();
        result
    }};
}

/// Macro for tracking allocations
#[macro_export]
macro_rules! track_allocations {
    ($block:block) => {{
        let tracker = $crate::codegen::performance_monitor::AllocationTracker::start();
        let result = $block;
        let (alloc_count, alloc_bytes) = tracker.get_delta();
        println!("Allocations: {} count, {} bytes", alloc_count, alloc_bytes);
        result
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_performance_metrics() {
        let metrics = PerformanceMetrics::new();
        
        metrics.record_method_compiled(100);
        metrics.record_cache_hit();
        metrics.record_cache_miss();
        metrics.record_constant_folded();
        
        let report = metrics.get_report();
        
        assert_eq!(report.methods_compiled, 1);
        assert_eq!(report.bytecode_generated, 100);
        assert_eq!(report.cache_hit_rate, 0.5);
        assert_eq!(report.optimizations.constants_folded, 1);
    }
    
    #[test]
    fn test_compilation_timer() {
        let timer = CompilationTimer::start_phase("test_phase");
        std::thread::sleep(Duration::from_millis(10));
        let duration = timer.end_phase();
        
        assert!(duration >= Duration::from_millis(10));
    }
    
    #[test]
    fn test_benchmark_stats() {
        let times = vec![
            Duration::from_millis(10),
            Duration::from_millis(20),
            Duration::from_millis(30),
        ];
        
        let stats = BenchmarkStats::from_times(&times);
        
        assert_eq!(stats.min, Duration::from_millis(10));
        assert_eq!(stats.max, Duration::from_millis(30));
        assert_eq!(stats.median, Duration::from_millis(20));
    }
}