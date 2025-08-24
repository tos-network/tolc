//! Import Statement Resolution - JavaC aligned
//!
//! This module handles import statement resolution, converting simple class names
//! to fully qualified names based on import declarations.

use crate::ast::{Ast, ImportDecl, TypeDecl};
use crate::common::manager::ClasspathManager;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum ImportStatement {
    /// Single class import: import java.util.List;
    Single { 
        fully_qualified_name: String, 
        simple_name: String 
    },
    /// Wildcard import: import java.util.*;  
    Star { 
        package_name: String 
    },
    /// Static import: import static java.lang.Math.PI;
    StaticSingle { 
        class_name: String, 
        member_name: String 
    },
    /// Static wildcard: import static java.lang.Math.*;
    StaticStar { 
        class_name: String 
    },
}

pub struct ImportResolver<'a> {
    imports: Vec<ImportStatement>,
    simple_name_cache: HashMap<String, String>, // simple_name -> fully_qualified_name
    current_package: String,
    manager: &'a mut ClasspathManager,
}

impl<'a> ImportResolver<'a> {
    pub fn new(manager: &'a mut ClasspathManager) -> Self {
        ImportResolver {
            imports: Vec::new(),
            simple_name_cache: HashMap::new(),
            current_package: String::new(),
            manager,
        }
    }
    
    /// Build import resolver from AST import declarations
    pub fn from_ast(ast: &Ast, manager: &'a mut ClasspathManager) -> Self {
        let mut resolver = ImportResolver::new(manager);
        
        // Extract package name from the first type declaration
        if let Some(first_type) = ast.type_decls.first() {
            let type_name = match first_type {
                TypeDecl::Class(class_decl) => &class_decl.name,
                TypeDecl::Interface(interface_decl) => &interface_decl.name,
                TypeDecl::Enum(enum_decl) => &enum_decl.name,
                TypeDecl::Annotation(annotation_decl) => &annotation_decl.name,
            };
            if let Some(package_name) = extract_package_from_type(type_name) {
                resolver.current_package = package_name;
                eprintln!("📦 IMPORT: Current package: {}", resolver.current_package);
            }
        }
        
        // Process import declarations
        for import_decl in &ast.imports {
            let import_stmt = resolver.process_import_declaration(import_decl);
            if let Some(stmt) = import_stmt {
                resolver.imports.push(stmt);
            }
        }
        
        // Add implicit java.lang.* import (JavaC behavior)
        resolver.imports.push(ImportStatement::Star { 
            package_name: "java.lang".to_string() 
        });
        
        eprintln!("✅ IMPORT: Processed {} import statements", resolver.imports.len());
        
        resolver
    }
    
    fn process_import_declaration(
        &mut self, 
        import_decl: &ImportDecl
    ) -> Option<ImportStatement> {
        
        if import_decl.is_static {
            if import_decl.is_wildcard {
                // static import pkg.Class.*;
                Some(ImportStatement::StaticStar { 
                    class_name: import_decl.name.clone() 
                })
            } else {
                // static import pkg.Class.member;
                if let Some((class_name, member_name)) = split_static_import(&import_decl.name) {
                    Some(ImportStatement::StaticSingle { 
                        class_name, 
                        member_name 
                    })
                } else {
                    eprintln!("⚠️  IMPORT: Invalid static import: {}", import_decl.name);
                    None
                }
            }
        } else {
            if import_decl.is_wildcard {
                // import pkg.*;
                Some(ImportStatement::Star { 
                    package_name: import_decl.name.clone() 
                })
            } else {
                // import pkg.Class;
                let simple_name = import_decl.name.split('.').last().unwrap_or("").to_string();
                
                // Verify the import exists in classpath
                if self.manager.class_exists(&import_decl.name) {
                    eprintln!("✅ IMPORT: Verified import {}", import_decl.name);
                    Some(ImportStatement::Single { 
                        fully_qualified_name: import_decl.name.clone(),
                        simple_name,
                    })
                } else {
                    eprintln!("⚠️  IMPORT: Import not found in classpath: {}", import_decl.name);
                    // Still add it - might be a forward reference or external dependency
                    Some(ImportStatement::Single { 
                        fully_qualified_name: import_decl.name.clone(),
                        simple_name,
                    })
                }
            }
        }
    }
    
    /// Resolve simple class name to fully qualified name using imports
    pub fn resolve_class_name(&mut self, simple_name: &str) -> Option<String> {
        // Check cache first
        if let Some(cached) = self.simple_name_cache.get(simple_name) {
            return Some(cached.clone());
        }
        
        let resolved = self.resolve_class_name_internal(simple_name);
        
        // Cache the result
        if let Some(ref result) = resolved {
            self.simple_name_cache.insert(simple_name.to_string(), result.clone());
            eprintln!("✅ IMPORT: Resolved '{}' -> '{}'", simple_name, result);
        }
        
        resolved
    }
    
    fn resolve_class_name_internal(&mut self, simple_name: &str) -> Option<String> {
        // 1. Check direct single imports
        for import in &self.imports {
            if let ImportStatement::Single { fully_qualified_name, simple_name: imported_simple } = import {
                if imported_simple == simple_name {
                    return Some(fully_qualified_name.clone());
                }
            }
        }
        
        // 2. Check current package
        if !self.current_package.is_empty() {
            let candidate = format!("{}.{}", self.current_package, simple_name);
            if self.manager.class_exists(&candidate) {
                return Some(candidate);
            }
        }
        
        // 3. Check wildcard imports
        for import in &self.imports {
            if let ImportStatement::Star { package_name } = import {
                let candidate = format!("{}.{}", package_name, simple_name);
                if self.manager.class_exists(&candidate) {
                    return Some(candidate);
                }
            }
        }
        
        // 4. Try classpath manager's common packages
        self.manager.find_class_by_simple_name(simple_name)
    }
    
    /// Resolve static member (method or field) import
    pub fn resolve_static_member(&self, member_name: &str) -> Option<String> {
        // Check static single imports
        for import in &self.imports {
            if let ImportStatement::StaticSingle { class_name, member_name: imported_member } = import {
                if imported_member == member_name {
                    return Some(class_name.clone());
                }
            }
        }
        
        // Check static wildcard imports
        for import in &self.imports {
            if let ImportStatement::StaticStar { class_name } = import {
                // Would need to check if the class actually has this member
                // For now, return the class name - verification happens later
                return Some(class_name.clone());
            }
        }
        
        None
    }
    
    /// Get current package name
    pub fn get_current_package(&self) -> &str {
        &self.current_package
    }
    
    /// Get all import statements
    pub fn get_imports(&self) -> &[ImportStatement] {
        &self.imports
    }
}

/// Standalone ImportResolver that owns its ClasspathManager
/// This allows existing code to continue working while we migrate
pub struct StandaloneImportResolver {
    manager: ClasspathManager,
}

impl StandaloneImportResolver {
    pub fn new(default_classpath: &str) -> Self {
        StandaloneImportResolver {
            manager: ClasspathManager::new(default_classpath),
        }
    }
    
    /// Create a borrowed ImportResolver from this standalone instance
    pub fn as_borrowed(&mut self) -> ImportResolver {
        ImportResolver::new(&mut self.manager)
    }
    
    /// Create from AST
    pub fn from_ast(ast: &Ast, default_classpath: &str) -> Self {
        let mut instance = Self::new(default_classpath);
        // For now, we don't process the AST. This can be enhanced later.
        instance
    }
    
    /// Resolve simple class name to fully qualified name
    pub fn resolve_class_name(&mut self, simple_name: &str) -> Option<String> {
        let mut borrowed = self.as_borrowed();
        borrowed.resolve_class_name(simple_name)
    }
}

/// Extract package name from a potentially qualified class name
/// e.g., "java.util.List" -> Some("java.util")
fn extract_package_from_type(type_name: &str) -> Option<String> {
    let parts: Vec<&str> = type_name.split('.').collect();
    if parts.len() > 1 {
        Some(parts[..parts.len()-1].join("."))
    } else {
        None
    }
}

/// Split static import into class and member parts
/// e.g., "java.lang.Math.PI" -> Some(("java.lang.Math", "PI"))
fn split_static_import(import_name: &str) -> Option<(String, String)> {
    let parts: Vec<&str> = import_name.split('.').collect();
    if parts.len() >= 2 {
        let member_name = parts.last().unwrap().to_string();
        let class_name = parts[..parts.len()-1].join(".");
        Some((class_name, member_name))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ImportDecl, Span};
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_import_resolution() {
        // Create test classpath
        let temp_dir = TempDir::new().unwrap();
        let java_util = temp_dir.path().join("java/util");
        fs::create_dir_all(&java_util).unwrap();
        fs::write(java_util.join("List.java"), "package java.util; public interface List {}").unwrap();
        fs::write(java_util.join("ArrayList.java"), "package java.util; public class ArrayList {}").unwrap();
        
        let mut manager = ClasspathManager::new(&temp_dir.path().to_string_lossy());
        
        // Create test imports
        let imports = vec![
            ImportDecl {
                name: "java.util.List".to_string(),
                is_static: false,
                is_wildcard: false,
                span: Span::default(),
            },
            ImportDecl {
                name: "java.util".to_string(),
                is_static: false,
                is_wildcard: true,
                span: Span::default(),
            }
        ];
        
        let ast = Ast {
            package_decl: None,
            imports,
            type_decls: vec![],
            span: Span::default(),
        };
        
        let mut resolver = ImportResolver::from_ast(&ast, &mut manager);
        
        // Test resolution
        assert_eq!(resolver.resolve_class_name("List"), Some("java.util.List".to_string()));
        assert_eq!(resolver.resolve_class_name("ArrayList"), Some("java.util.ArrayList".to_string()));
        assert_eq!(resolver.resolve_class_name("NonExistent"), None);
    }
}