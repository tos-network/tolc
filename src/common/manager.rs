//! Dynamic Classpath Management - JavaC aligned
//!
//! This module provides dynamic classpath scanning and class discovery,
//! replacing the hardcoded approach in rt.rs and classpath.rs

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs;
use crate::common::error::{Result, Error};

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

pub struct ClasspathManager {
    classpath_entries: Vec<PathBuf>,
    class_cache: HashMap<String, ClassInfo>,
    source_cache: HashMap<String, PathBuf>,
    package_cache: HashMap<String, Vec<String>>, // package -> list of classes
}

impl ClasspathManager {
    pub fn new(default_classpath: &str) -> Self {
        // Check TOLC_CLASSPATH environment variable first
        let classpath = std::env::var("TOLC_CLASSPATH")
            .unwrap_or_else(|_| {
                // If TOLC_CLASSPATH is not set, use default or fallback to "."
                if default_classpath.is_empty() {
                    ".".to_string()
                } else {
                    default_classpath.to_string()
                }
            });
            
        let entries: Vec<PathBuf> = if classpath.is_empty() {
            vec![PathBuf::from(".")]
        } else {
            classpath.split(':')
                .map(|p| PathBuf::from(p.trim()))
                .filter(|p| p.exists())
                .collect()
        };
        
        eprintln!("🔍 CLASSPATH: Initializing with {} entries (from {})", 
                  entries.len(),
                  if std::env::var("TOLC_CLASSPATH").is_ok() { "TOLC_CLASSPATH" } else { "default" });
        for entry in &entries {
            eprintln!("  📁 {}", entry.display());
        }
        
        ClasspathManager {
            classpath_entries: entries,
            class_cache: HashMap::new(),
            source_cache: HashMap::new(),
            package_cache: HashMap::new(),
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
        
        // Test classpath manager
        let mut manager = ClasspathManager::new(&temp_dir.path().to_string_lossy());
        
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