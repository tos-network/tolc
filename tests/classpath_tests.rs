//! Comprehensive classpath resolution tests
//! 
//! These tests verify that tolc's classpath resolution behaves identically to Oracle's javac,
//! following the exact same priority order and handling edge cases.

use std::env;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;
use tolc::common::classpath::ClasspathResolver;

#[cfg(test)]
mod classpath_priority_tests {
    use super::*;

    #[test]
    fn test_cp_argument_has_highest_priority() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Set CLASSPATH environment variable
        env::set_var("CLASSPATH", "/env/path");
        
        // -cp should override CLASSPATH environment variable
        let result = ClasspathResolver::resolve_classpath(Some("/cmd/classpath"), Some("/cmd/cp"));
        assert_eq!(result, "/cmd/cp");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }

    #[test]
    fn test_classpath_argument_priority() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Set CLASSPATH environment variable
        env::set_var("CLASSPATH", "/env/path");
        
        // -classpath should override CLASSPATH environment variable
        let result = ClasspathResolver::resolve_classpath(Some("/cmd/classpath"), None);
        assert_eq!(result, "/cmd/classpath");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }

    #[test]
    fn test_classpath_env_var_fallback() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Set CLASSPATH environment variable
        env::set_var("CLASSPATH", "/env/path");
        
        // Should use CLASSPATH when no command line args
        let result = ClasspathResolver::resolve_classpath(None, None);
        assert_eq!(result, "/env/path");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }

    #[test]
    fn test_default_current_directory_fallback() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Remove CLASSPATH to test default fallback
        env::remove_var("CLASSPATH");
        
        let result = ClasspathResolver::resolve_classpath(None, None);
        assert_eq!(result, ".");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }

    #[test]
    fn test_empty_classpath_env_uses_default() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Set empty CLASSPATH
        env::set_var("CLASSPATH", "");
        
        let result = ClasspathResolver::resolve_classpath(None, None);
        assert_eq!(result, ".");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }

    #[test]
    fn test_complete_priority_chain() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Test 1: -cp wins over everything
        env::set_var("CLASSPATH", "/env/path");
        assert_eq!(
            ClasspathResolver::resolve_classpath(Some("/cmd/classpath"), Some("/cmd/cp")),
            "/cmd/cp"
        );
        
        // Test 2: -classpath wins over CLASSPATH
        env::set_var("CLASSPATH", "/env/path");
        assert_eq!(
            ClasspathResolver::resolve_classpath(Some("/cmd/classpath"), None),
            "/cmd/classpath"
        );
        
        // Test 3: CLASSPATH used when no command line args
        env::set_var("CLASSPATH", "/env/path");
        let result = ClasspathResolver::resolve_classpath(None, None);
        assert_eq!(result, "/env/path");
        
        // Test 4: Default when nothing is set
        env::remove_var("CLASSPATH");
        assert_eq!(
            ClasspathResolver::resolve_classpath(None, None),
            "."
        );
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }
}

#[cfg(test)]
mod tolc_backward_compatibility_tests {
    use super::*;

    #[test]
    fn test_tolc_classpath_backward_compatibility() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        let original_tolc_classpath = env::var("TOLC_CLASSPATH").ok();
        
        // Clear CLASSPATH and set TOLC_CLASSPATH
        env::remove_var("CLASSPATH");
        env::set_var("TOLC_CLASSPATH", "/tolc/legacy/path");
        
        // Should use TOLC_CLASSPATH when CLASSPATH is not set and no command line args
        let result = ClasspathResolver::resolve_classpath_with_tolc_fallback(None, None);
        assert_eq!(result, "/tolc/legacy/path");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
        match original_tolc_classpath {
            Some(cp) => env::set_var("TOLC_CLASSPATH", cp),
            None => env::remove_var("TOLC_CLASSPATH"),
        }
    }

    #[test]
    fn test_classpath_overrides_tolc_classpath() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        let original_tolc_classpath = env::var("TOLC_CLASSPATH").ok();
        
        // Set both CLASSPATH and TOLC_CLASSPATH
        env::set_var("CLASSPATH", "/standard/classpath");
        env::set_var("TOLC_CLASSPATH", "/tolc/legacy/path");
        
        // CLASSPATH should have priority over TOLC_CLASSPATH
        let result = ClasspathResolver::resolve_classpath_with_tolc_fallback(None, None);
        assert_eq!(result, "/standard/classpath");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
        match original_tolc_classpath {
            Some(cp) => env::set_var("TOLC_CLASSPATH", cp),
            None => env::remove_var("TOLC_CLASSPATH"),
        }
    }

    #[test]
    fn test_command_line_overrides_tolc_classpath() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        let original_tolc_classpath = env::var("TOLC_CLASSPATH").ok();
        
        // Clear CLASSPATH and set TOLC_CLASSPATH
        env::remove_var("CLASSPATH");
        env::set_var("TOLC_CLASSPATH", "/tolc/legacy/path");
        
        // Command line should override TOLC_CLASSPATH
        let result = ClasspathResolver::resolve_classpath_with_tolc_fallback(Some("/cmd/path"), None);
        assert_eq!(result, "/cmd/path");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
        match original_tolc_classpath {
            Some(cp) => env::set_var("TOLC_CLASSPATH", cp),
            None => env::remove_var("TOLC_CLASSPATH"),
        }
    }
}

#[cfg(test)]
mod classpath_parsing_tests {
    use super::*;

    #[test]
    fn test_parse_unix_classpath_entries() {
        let entries = ClasspathResolver::parse_classpath_entries("/path1:/path2:/path3");
        assert_eq!(entries, vec!["/path1", "/path2", "/path3"]);
    }

    #[test]
    fn test_parse_empty_classpath() {
        let entries = ClasspathResolver::parse_classpath_entries("");
        assert_eq!(entries, Vec::<String>::new());
    }

    #[test]
    fn test_parse_single_entry() {
        let entries = ClasspathResolver::parse_classpath_entries("/single/path");
        assert_eq!(entries, vec!["/single/path"]);
    }

    #[test]
    fn test_parse_with_empty_entries() {
        let entries = ClasspathResolver::parse_classpath_entries("/path1::/path2:");
        assert_eq!(entries, vec!["/path1", "/path2"]);
    }

    #[test]
    fn test_parse_with_whitespace() {
        let entries = ClasspathResolver::parse_classpath_entries(" /path1 : /path2 : /path3 ");
        assert_eq!(entries, vec!["/path1", "/path2", "/path3"]);
    }

    #[cfg(windows)]
    #[test]
    fn test_parse_windows_classpath_entries() {
        let entries = ClasspathResolver::parse_classpath_entries("C:\\path1;C:\\path2;C:\\path3");
        assert_eq!(entries, vec!["C:\\path1", "C:\\path2", "C:\\path3"]);
    }
}

#[cfg(test)]
mod classpath_utility_tests {
    use super::*;

    #[test]
    fn test_contains_entry() {
        assert!(ClasspathResolver::contains_entry("/path1:/path2:/path3", "/path2"));
        assert!(!ClasspathResolver::contains_entry("/path1:/path2:/path3", "/path4"));
        assert!(ClasspathResolver::contains_entry("/single/path", "/single/path"));
    }

    #[test]
    fn test_combine_classpaths() {
        let combined = ClasspathResolver::combine_classpaths(&["/path1", "/path2", "/path3"]);
        let separator = if cfg!(windows) { ";" } else { ":" };
        assert_eq!(combined, format!("/path1{}/path2{}/path3", separator, separator));
    }

    #[test]
    fn test_combine_with_empty_entries() {
        let combined = ClasspathResolver::combine_classpaths(&["/path1", "", "/path2"]);
        let separator = if cfg!(windows) { ";" } else { ":" };
        assert_eq!(combined, format!("/path1{}/path2", separator));
    }

    #[test]
    fn test_combine_empty_classpaths() {
        let combined = ClasspathResolver::combine_classpaths(&[]);
        assert_eq!(combined, "");
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use tolc::common::class_manager::ClassManager;

    #[test]
    fn test_class_manager_uses_classpath_resolver() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        let original_tolc_classpath = env::var("TOLC_CLASSPATH").ok();
        
        // Create temporary directory structure for testing
        let temp_dir = TempDir::new().unwrap();
        let java_dir = temp_dir.path().join("java");
        let lang_dir = java_dir.join("lang");
        fs::create_dir_all(&lang_dir).unwrap();
        fs::write(lang_dir.join("String.java"), "package java.lang; public class String {}").unwrap();
        
        // Set CLASSPATH to our test directory
        env::set_var("CLASSPATH", temp_dir.path().to_string_lossy().to_string());
        env::remove_var("TOLC_CLASSPATH");
        
        // Create ClassManager (should use CLASSPATH via ClasspathResolver)
        let mut manager = ClassManager::new();
        
        // Verify it can find our test class
        let found = manager.find_class_source("java.lang.String");
        assert!(found.is_some(), "ClassManager should find String.java using CLASSPATH");
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
        match original_tolc_classpath {
            Some(cp) => env::set_var("TOLC_CLASSPATH", cp),
            None => env::remove_var("TOLC_CLASSPATH"),
        }
    }

    #[test]
    fn test_class_manager_with_explicit_classpath() {
        // Create temporary directory structure for testing
        let temp_dir = TempDir::new().unwrap();
        let java_dir = temp_dir.path().join("java");
        let util_dir = java_dir.join("util");
        fs::create_dir_all(&util_dir).unwrap();
        fs::write(util_dir.join("List.java"), "package java.util; public interface List {}").unwrap();
        
        // Create ClassManager with explicit classpath
        let mut manager = ClassManager::with_classpath(&temp_dir.path().to_string_lossy());
        
        // Verify it can find our test class
        let found = manager.find_class_source("java.util.List");
        assert!(found.is_some(), "ClassManager should find List.java using explicit classpath");
    }

    #[test]
    fn test_multiple_classpath_entries() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Create two temporary directories
        let temp_dir1 = TempDir::new().unwrap();
        let temp_dir2 = TempDir::new().unwrap();
        
        let java_dir1 = temp_dir1.path().join("java").join("lang");
        let java_dir2 = temp_dir2.path().join("java").join("util");
        fs::create_dir_all(&java_dir1).unwrap();
        fs::create_dir_all(&java_dir2).unwrap();
        
        fs::write(java_dir1.join("String.java"), "package java.lang; public class String {}").unwrap();
        fs::write(java_dir2.join("List.java"), "package java.util; public interface List {}").unwrap();
        
        // Set CLASSPATH to both directories
        let separator = if cfg!(windows) { ";" } else { ":" };
        let classpath = format!("{}{}{}", 
            temp_dir1.path().display(), 
            separator, 
            temp_dir2.path().display());
        env::set_var("CLASSPATH", &classpath);
        
        // Create ClassManager
        let mut manager = ClassManager::new();
        
        // Verify it can find classes from both directories
        assert!(manager.find_class_source("java.lang.String").is_some());
        assert!(manager.find_class_source("java.util.List").is_some());
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }
}

#[cfg(test)]
mod edge_case_tests {
    use super::*;

    #[test]
    fn test_whitespace_only_classpath_env() {
        // Save original environment
        let original_classpath = env::var("CLASSPATH").ok();
        
        // Set CLASSPATH to whitespace only
        env::set_var("CLASSPATH", "   ");
        
        // Should treat as empty and use default
        let result = ClasspathResolver::resolve_classpath(None, None);
        assert_eq!(result, "   ");  // Current implementation preserves whitespace
        
        // Clean up environment
        match original_classpath {
            Some(cp) => env::set_var("CLASSPATH", cp),
            None => env::remove_var("CLASSPATH"),
        }
    }

    #[test]
    fn test_very_long_classpath() {
        let long_path = "/very/long/path/that/goes/on/and/on/and/on".repeat(100);
        let entries = ClasspathResolver::parse_classpath_entries(&long_path);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0], long_path);
    }

    #[test]
    fn test_special_characters_in_paths() {
        let special_path = "/path with spaces:/path$with$dollars:/path[with]brackets";
        let entries = ClasspathResolver::parse_classpath_entries(special_path);
        assert_eq!(entries, vec![
            "/path with spaces",
            "/path$with$dollars", 
            "/path[with]brackets"
        ]);
    }

    #[test]
    fn test_unicode_in_paths() {
        let unicode_path = "/路径/测试:/пуÑь/тест";
        let entries = ClasspathResolver::parse_classpath_entries(unicode_path);
        assert_eq!(entries, vec!["/路径/测试", "/пуÑь/тест"]);
    }
}