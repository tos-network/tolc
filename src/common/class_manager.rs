//! Class Management System - JavaC aligned
//!
//! This module provides comprehensive class management including:
//! - Dynamic classpath scanning and class discovery
//! - Method and field resolution with caching
//! - Type hierarchy analysis
//! - Integration with the entire compilation pipeline

use std::collections::HashMap;
use std::path::PathBuf;
use std::fs;
use crate::common::error::Result;

#[derive(Debug, Clone)]
pub struct ClassInfo {
    pub fully_qualified_name: String,
    pub simple_name: String,
    pub package_name: String,
    pub source_path: PathBuf,
    pub methods: Vec<MethodInfo>,
    pub fields: Vec<FieldInfo>,
    pub parent_class: Option<String>,
    pub interfaces: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub return_type: String,
    pub parameters: Vec<String>,
    pub is_static: bool,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub field_type: String,
    pub is_static: bool,
    pub is_public: bool,
}

#[derive(Debug)]
pub struct ClassManager {
    classpath_entries: Vec<PathBuf>,
    class_cache: HashMap<String, ClassInfo>,
    source_cache: HashMap<String, PathBuf>,
    package_cache: HashMap<String, Vec<String>>, // package -> list of classes
    method_cache: HashMap<String, MethodInfo>,   // class#method -> method info
    field_cache: HashMap<String, FieldInfo>,     // class.field -> field info
    inheritance_cache: HashMap<String, Vec<String>>, // class -> superclasses
}

impl ClassManager {
    /// Create ClassManager using JavaC-aligned classpath resolution
    /// 
    /// Priority order (same as javac):
    /// 1. Command line arguments (handled at CLI level)
    /// 2. CLASSPATH environment variable
    /// 3. Current directory "." (default)
    /// 
    /// For backward compatibility, also checks TOLC_CLASSPATH
    pub fn new() -> Self {
        use crate::common::classpath::ClasspathResolver;
        
        // Use JavaC-aligned classpath resolution (no command line args at this level)
        let classpath = ClasspathResolver::resolve_classpath_with_tolc_fallback(None, None);
            
        let entries: Vec<PathBuf> = if classpath.is_empty() {
            vec![PathBuf::from(".")]
        } else {
            classpath.split(if cfg!(windows) { ';' } else { ':' })
                .map(|p| PathBuf::from(p.trim()))
                .collect()
        };
        
        eprintln!("🔍 CLASSPATH: Initializing with {} entries (from {})", 
                  entries.len(),
                  if std::env::var("TOLC_CLASSPATH").is_ok() { "TOLC_CLASSPATH" } else { "current directory" });
        for entry in &entries {
            eprintln!("  📁 {}", entry.display());
        }
        
        ClassManager {
            classpath_entries: entries,
            class_cache: HashMap::new(),
            source_cache: HashMap::new(),
            package_cache: HashMap::new(),
            method_cache: HashMap::new(),
            field_cache: HashMap::new(),
            inheritance_cache: HashMap::new(),
        }
    }
    
    /// Create ClassManager with explicit classpath 
    /// Used for testing, CLI argument override, or special cases where
    /// command-line specified classpath should override environment variables
    pub fn with_classpath(classpath: &str) -> Self {
        use crate::common::classpath::ClasspathResolver;
        
        let entries: Vec<PathBuf> = if classpath.is_empty() {
            vec![PathBuf::from(".")]
        } else {
            ClasspathResolver::parse_classpath_entries(classpath)
                .iter()
                .map(|p| PathBuf::from(p))
                .collect()
        };
        
        eprintln!("🔍 CLASSPATH: Initializing with {} entries (explicit)", entries.len());
        for entry in &entries {
            eprintln!("  📁 {}", entry.display());
        }
        
        ClassManager {
            classpath_entries: entries,
            class_cache: HashMap::new(),
            source_cache: HashMap::new(),
            package_cache: HashMap::new(),
            method_cache: HashMap::new(),
            field_cache: HashMap::new(),
            inheritance_cache: HashMap::new(),
        }
    }
    
    /// Find Java source file by fully qualified class name
    /// e.g., "java.util.List" -> "/path/to/java/util/List.java"
    pub fn find_source_file(&mut self, fully_qualified_name: &str) -> Option<String> {
        self.find_class_source(fully_qualified_name).map(|p| p.to_string_lossy().to_string())
    }

    /// Find .class file by fully qualified class name
    /// e.g., "java.util.List" -> "/path/to/java/util/List.class"
    pub fn find_class_file(&mut self, fully_qualified_name: &str) -> Option<String> {
        let relative_path = format!("{}.class", fully_qualified_name.replace('.', "/"));
        
        for entry in &self.classpath_entries {
            let full_path = entry.join(&relative_path);
            if full_path.exists() && full_path.is_file() {
                eprintln!("✅ CLASSPATH: Found class file {} at {}", fully_qualified_name, full_path.display());
                return Some(full_path.to_string_lossy().to_string());
            }
        }
        
        eprintln!("⚠️  CLASSPATH: Could not find class file for {}", fully_qualified_name);
        None
    }

    /// Find Java source file by fully qualified class name
    /// e.g., "java.util.List" -> "/path/to/java/util/List.java"
    pub fn find_class_source(&mut self, fully_qualified_name: &str) -> Option<PathBuf> {
        // Check cache first
        if let Some(cached_path) = self.source_cache.get(fully_qualified_name) {
            return Some(cached_path.clone());
        }
        
        let relative_path = format!("{}.java", fully_qualified_name.replace('.', "/"));
        
        for entry in &self.classpath_entries {
            let full_path = entry.join(&relative_path);
            if full_path.exists() && full_path.is_file() {
                eprintln!("✅ CLASSPATH: Found {} at {}", fully_qualified_name, full_path.display());
                self.source_cache.insert(fully_qualified_name.to_string(), full_path.clone());
                return Some(full_path);
            }
        }
        
        eprintln!("⚠️  CLASSPATH: Class {} not found in classpath", fully_qualified_name);
        None
    }
    
    /// Find class by simple name, checking common packages
    /// e.g., "String" -> "java.lang.String"
    pub fn find_class_by_simple_name(&mut self, simple_name: &str) -> Option<String> {
        // Common Java packages to search
        let common_packages = [
            "java.lang",
            "java.util", 
            "java.io",
            "java.net",
            "java.text",
            "java.math",
        ];
        
        for package in &common_packages {
            let candidate = format!("{}.{}", package, simple_name);
            if self.find_class_source(&candidate).is_some() {
                eprintln!("✅ CLASSPATH: Resolved '{}' -> '{}'", simple_name, candidate);
                return Some(candidate);
            }
        }
        
        eprintln!("⚠️  CLASSPATH: Simple name '{}' not found in common packages", simple_name);
        None
    }
    
    /// Check if a class exists in the classpath
    pub fn class_exists(&mut self, fully_qualified_name: &str) -> bool {
        self.find_class_source(fully_qualified_name).is_some()
    }
    
    /// Scan a package for all available classes
    pub fn scan_package(&mut self, package_name: &str) -> Vec<String> {
        if let Some(cached_classes) = self.package_cache.get(package_name) {
            return cached_classes.clone();
        }
        
        let mut classes = Vec::new();
        let package_path = package_name.replace('.', "/");
        
        for entry in &self.classpath_entries {
            let package_dir = entry.join(&package_path);
            if package_dir.exists() && package_dir.is_dir() {
                if let Ok(dir_entries) = fs::read_dir(&package_dir) {
                    for dir_entry in dir_entries.flatten() {
                        let file_path = dir_entry.path();
                        if let Some(extension) = file_path.extension() {
                            if extension == "java" {
                                if let Some(file_name) = file_path.file_stem() {
                                    if let Some(class_name) = file_name.to_str() {
                                        let fully_qualified = format!("{}.{}", package_name, class_name);
                                        classes.push(fully_qualified);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        eprintln!("🔍 CLASSPATH: Scanned package '{}', found {} classes", package_name, classes.len());
        self.package_cache.insert(package_name.to_string(), classes.clone());
        classes
    }
    
    /// Get basic class information without full parsing
    /// This is a lightweight operation for type resolution
    pub fn get_class_info(&mut self, fully_qualified_name: &str) -> Option<ClassInfo> {
        if let Some(cached_info) = self.class_cache.get(fully_qualified_name) {
            return Some(cached_info.clone());
        }
        
        if let Some(source_path) = self.find_class_source(fully_qualified_name) {
            // For now, create basic info without full parsing
            // This can be enhanced later to do lightweight AST parsing
            let parts: Vec<&str> = fully_qualified_name.split('.').collect();
            let simple_name = parts.last().unwrap().to_string();
            let package_name = if parts.len() > 1 {
                parts[..parts.len()-1].join(".")
            } else {
                String::new()
            };
            
            let class_info = ClassInfo {
                fully_qualified_name: fully_qualified_name.to_string(),
                simple_name,
                package_name,
                source_path,
                methods: Vec::new(), // TODO: Parse from source
                fields: Vec::new(),  // TODO: Parse from source
                parent_class: None,  // TODO: Parse from source
                interfaces: Vec::new(), // TODO: Parse from source
            };
            
            self.class_cache.insert(fully_qualified_name.to_string(), class_info.clone());
            return Some(class_info);
        }
        
        None
    }
    
    /// Find method by class name, method name and parameter types
    pub fn find_method(&mut self, class_name: &str, method_name: &str, _param_types: &[String]) -> Option<MethodInfo> {
        let cache_key = format!("{}#{}", class_name, method_name);
        
        // Check cache first
        if let Some(cached_method) = self.method_cache.get(&cache_key) {
            return Some(cached_method.clone());
        }
        
        // Get class info and search for method
        if let Some(class_info) = self.get_class_info(class_name) {
            for method in &class_info.methods {
                if method.name == method_name {
                    // TODO: Implement parameter type matching
                    self.method_cache.insert(cache_key, method.clone());
                    return Some(method.clone());
                }
            }
        }
        
        None
    }
    
    /// Find field by class name and field name
    pub fn find_field(&mut self, class_name: &str, field_name: &str) -> Option<FieldInfo> {
        let cache_key = format!("{}.{}", class_name, field_name);
        
        // Check cache first
        if let Some(cached_field) = self.field_cache.get(&cache_key) {
            return Some(cached_field.clone());
        }
        
        // Get class info and search for field
        if let Some(class_info) = self.get_class_info(class_name) {
            for field in &class_info.fields {
                if field.name == field_name {
                    self.field_cache.insert(cache_key, field.clone());
                    return Some(field.clone());
                }
            }
        }
        
        None
    }
    
    /// Check if a class is a subclass of another class
    pub fn is_subclass_of(&mut self, subclass: &str, superclass: &str) -> bool {
        if subclass == superclass {
            return true;
        }
        
        // Check cache first
        if let Some(cached_supers) = self.inheritance_cache.get(subclass) {
            return cached_supers.contains(&superclass.to_string());
        }
        
        // Build inheritance chain
        let mut supers = Vec::new();
        if let Some(class_info) = self.get_class_info(subclass) {
            if let Some(ref parent) = class_info.parent_class {
                supers.push(parent.clone());
                // Recursively check parent classes
                if self.is_subclass_of(parent, superclass) {
                    supers.push(superclass.to_string());
                }
            }
            
            // Check interfaces
            for interface in &class_info.interfaces {
                supers.push(interface.clone());
                if interface == superclass {
                    supers.push(superclass.to_string());
                }
            }
        }
        
        let result = supers.contains(&superclass.to_string());
        self.inheritance_cache.insert(subclass.to_string(), supers);
        result
    }
    
    /// Preload commonly used packages for performance
    pub fn preload_common_packages(&mut self) -> Result<()> {
        let common_packages = ["java.lang", "java.util", "java.io", "java.net"];
        
        for package in &common_packages {
            eprintln!("🔍 PRELOAD: Scanning package {}", package);
            let classes = self.scan_package(package);
            eprintln!("✅ PRELOAD: Loaded {} classes from {}", classes.len(), package);
            
            // Preload class info for all classes in the package
            for class_name in &classes {
                self.get_class_info(class_name);
            }
        }
        
        Ok(())
    }
    
    /// Clear all caches
    pub fn clear_cache(&mut self) {
        self.class_cache.clear();
        self.source_cache.clear();
        self.package_cache.clear();
        self.method_cache.clear();
        self.field_cache.clear();
        self.inheritance_cache.clear();
    }
    
    /// Get all classpath entries
    pub fn get_classpath_entries(&self) -> &[PathBuf] {
        &self.classpath_entries
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_classpath_scanning() {
        // Create temporary directory structure
        let temp_dir = TempDir::new().unwrap();
        let java_dir = temp_dir.path().join("java");
        let lang_dir = java_dir.join("lang");
        let util_dir = java_dir.join("util");
        
        fs::create_dir_all(&lang_dir).unwrap();
        fs::create_dir_all(&util_dir).unwrap();
        
        // Create test files
        fs::write(lang_dir.join("String.java"), "package java.lang; public class String {}").unwrap();
        fs::write(lang_dir.join("Object.java"), "package java.lang; public class Object {}").unwrap();
        fs::write(util_dir.join("List.java"), "package java.util; public interface List {}").unwrap();
        
        // Test class manager
        let mut manager = ClassManager::with_classpath(&temp_dir.path().to_string_lossy());
        
        // Test finding classes
        assert!(manager.find_class_source("java.lang.String").is_some());
        assert!(manager.find_class_source("java.util.List").is_some());
        assert!(manager.find_class_source("java.lang.NonExistent").is_none());
        
        // Test simple name resolution
        assert_eq!(manager.find_class_by_simple_name("String"), Some("java.lang.String".to_string()));
        assert_eq!(manager.find_class_by_simple_name("List"), Some("java.util.List".to_string()));
        
        // Test package scanning
        let lang_classes = manager.scan_package("java.lang");
        assert!(lang_classes.contains(&"java.lang.String".to_string()));
        assert!(lang_classes.contains(&"java.lang.Object".to_string()));
    }
}