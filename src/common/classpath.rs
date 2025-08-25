//! JavaC-aligned classpath resolution
//! 
//! This module implements the exact same classpath resolution logic as Oracle's javac,
//! following the priority order:
//! 1. -classpath or -cp command line arguments (highest priority)
//! 2. CLASSPATH environment variable (secondary priority)  
//! 3. Current directory "." (default fallback)
//!
//! Reference: com.sun.tools.javac.file.Locations$ClassPathLocationHandler.computePath()

use std::env;

/// JavaC-aligned classpath resolver
/// Follows the exact same priority order as Oracle's javac compiler
pub struct ClasspathResolver;

impl ClasspathResolver {
    /// Resolve classpath following JavaC's priority order
    /// 
    /// Priority (same as javac):
    /// 1. `-classpath` or `-cp` command line arguments (highest priority)
    /// 2. `CLASSPATH` environment variable (secondary priority)  
    /// 3. Current directory "." (default fallback)
    ///
    /// # Arguments
    /// 
    /// * `classpath_arg` - Value from -classpath argument
    /// * `cp_arg` - Value from -cp argument  
    /// 
    /// # Returns
    /// 
    /// The resolved classpath string, never None (always falls back to ".")
    pub fn resolve_classpath(classpath_arg: Option<&str>, cp_arg: Option<&str>) -> String {
        // Priority 1: Command line arguments (-cp has same priority as -classpath)
        // In javac, both -cp and -classpath are processed by the same handler,
        // so we give -cp priority if both are specified (more recent/common usage)
        if let Some(cp) = cp_arg {
            eprintln!("🔍 CLASSPATH: Using -cp argument: {}", cp);
            return cp.to_string();
        }
        
        if let Some(classpath) = classpath_arg {
            eprintln!("🔍 CLASSPATH: Using -classpath argument: {}", classpath);
            return classpath.to_string();
        }
        
        // Priority 2: CLASSPATH environment variable
        // javac checks env.class.path system property, which corresponds to CLASSPATH env var
        if let Ok(classpath_env) = env::var("CLASSPATH") {
            if !classpath_env.is_empty() {
                eprintln!("🔍 CLASSPATH: Using CLASSPATH environment variable: {}", classpath_env);
                return classpath_env;
            }
        }
        
        // Priority 3: Default to current directory (javac fallback)
        eprintln!("🔍 CLASSPATH: Using default (current directory): .");
        ".".to_string()
    }
    
    /// Legacy method for backward compatibility with TOLC_CLASSPATH
    /// This maintains compatibility with existing tolc usage while moving towards javac alignment
    pub fn resolve_classpath_with_tolc_fallback(classpath_arg: Option<&str>, cp_arg: Option<&str>) -> String {
        // First try standard javac resolution
        let result = Self::resolve_classpath(classpath_arg, cp_arg);
        
        // If we fell back to "." and TOLC_CLASSPATH exists, use it for backward compatibility
        if result == "." {
            if let Ok(tolc_classpath) = env::var("TOLC_CLASSPATH") {
                if !tolc_classpath.is_empty() {
                    eprintln!("🔍 CLASSPATH: Using TOLC_CLASSPATH for backward compatibility: {}", tolc_classpath);
                    return tolc_classpath;
                }
            }
        }
        
        result
    }
    
    /// Parse classpath string into individual entries
    /// Handles platform-specific path separators (: on Unix, ; on Windows)
    pub fn parse_classpath_entries(classpath: &str) -> Vec<String> {
        if classpath.is_empty() {
            return vec![];
        }
        
        let separator = if cfg!(windows) { ';' } else { ':' };
        classpath
            .split(separator)
            .map(|entry| entry.trim().to_string())
            .filter(|entry| !entry.is_empty())
            .collect()
    }
    
    /// Check if classpath contains a specific entry
    pub fn contains_entry(classpath: &str, target_entry: &str) -> bool {
        Self::parse_classpath_entries(classpath)
            .iter()
            .any(|entry| entry == target_entry)
    }
    
    /// Combine multiple classpath strings with proper separator
    pub fn combine_classpaths(classpaths: &[&str]) -> String {
        let separator = if cfg!(windows) { ";" } else { ":" };
        classpaths
            .iter()
            .filter(|cp| !cp.is_empty())
            .map(|cp| cp.to_string())
            .collect::<Vec<_>>()
            .join(separator)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_cp_argument_priority() {
        // -cp should have priority over -classpath when both are present
        let result = ClasspathResolver::resolve_classpath(Some("/path/classpath"), Some("/path/cp"));
        assert_eq!(result, "/path/cp");
    }
    
    #[test] 
    fn test_classpath_argument_fallback() {
        // -classpath should be used when -cp is not present
        let result = ClasspathResolver::resolve_classpath(Some("/path/classpath"), None);
        assert_eq!(result, "/path/classpath");
    }
    
    #[test]
    fn test_environment_variable_fallback() {
        // Set up environment for test
        env::set_var("CLASSPATH", "/env/path");
        
        let result = ClasspathResolver::resolve_classpath(None, None);
        assert_eq!(result, "/env/path");
        
        // Clean up
        env::remove_var("CLASSPATH");
    }
    
    #[test]
    fn test_default_fallback() {
        // Remove CLASSPATH to test default
        env::remove_var("CLASSPATH");
        
        let result = ClasspathResolver::resolve_classpath(None, None);
        assert_eq!(result, ".");
    }
    
    #[test]
    fn test_parse_classpath_entries() {
        let entries = ClasspathResolver::parse_classpath_entries("/path1:/path2:/path3");
        assert_eq!(entries, vec!["/path1", "/path2", "/path3"]);
    }
    
    #[test]
    fn test_contains_entry() {
        assert!(ClasspathResolver::contains_entry("/path1:/path2", "/path1"));
        assert!(!ClasspathResolver::contains_entry("/path1:/path2", "/path3"));
    }
    
    #[test]
    fn test_combine_classpaths() {
        let combined = ClasspathResolver::combine_classpaths(&["/path1", "/path2", ""]);
        let separator = if cfg!(windows) { ";" } else { ":" };
        assert_eq!(combined, format!("/path1{}/path2", separator));
    }
}