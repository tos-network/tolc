use std::collections::HashMap;
use std::sync::OnceLock;

/// Global classpath mapping from simple class names to full internal names
static CLASSPATH_MAP: OnceLock<HashMap<String, String>> = OnceLock::new();

/// Initialize the classpath mapping from the embedded classpath data
fn init_classpath_map() -> HashMap<String, String> {
    let mut map = HashMap::new();
    
    // Parse the classpath.txt content
    let classpath_content = include_str!("../classpath.txt");
    
    for line in classpath_content.lines() {
        let line = line.trim();
        if line.is_empty() || !line.ends_with(".class") {
            continue;
        }
        
        // Remove .class extension
        let internal_name = &line[..line.len() - 6];
        
        // Extract simple class name (after last '/' or '$')
        let simple_name = if let Some(pos) = internal_name.rfind('/') {
            &internal_name[pos + 1..]
        } else {
            internal_name
        };
        
        // Handle both outer and inner classes
        if let Some(dollar_pos) = simple_name.find('$') {
            // This is an inner class like "Class$ClassType"
            let outer_class = &simple_name[..dollar_pos]; // "Class"
            let inner_class = simple_name; // "Class$ClassType"
            
            // Map the outer class to its path (without inner class suffix)
            let outer_internal_name = &internal_name[..internal_name.len() - simple_name.len() + outer_class.len()];
            if !map.contains_key(outer_class) {
                map.insert(outer_class.to_string(), outer_internal_name.to_string());
            }
            
            // Map the inner class name to the full internal name
            if !map.contains_key(inner_class) {
                map.insert(inner_class.to_string(), internal_name.to_string());
            }
        } else {
            // This is a regular class
            if !map.contains_key(simple_name) {
                map.insert(simple_name.to_string(), internal_name.to_string());
            }
        }
    }
    
    map
}

/// Get the classpath mapping, initializing it if necessary
pub fn get_classpath_map() -> &'static HashMap<String, String> {
    CLASSPATH_MAP.get_or_init(init_classpath_map)
}

/// Resolve a simple class name to its full internal name using the classpath
pub fn resolve_class_name(simple_name: &str) -> Option<&'static str> {
    get_classpath_map().get(simple_name).map(|s| s.as_str())
}

/// Resolve a simple class name to its full internal name, with fallback logic
pub fn resolve_class_name_with_fallback(simple_name: &str, current_package: Option<&str>) -> String {
    // First try the classpath mapping
    if let Some(internal_name) = resolve_class_name(simple_name) {
        return internal_name.to_string();
    }
    
    // Fallback to package-based resolution
    if let Some(pkg) = current_package {
        if !pkg.is_empty() {
            return format!("{}/{}", pkg.replace('.', "/"), simple_name);
        }
    }
    
    // Last resort: return as-is
    simple_name.to_string()
}

/// Convert internal name to signature format (Linternal/name;)
pub fn internal_name_to_signature(internal_name: &str) -> String {
    format!("L{};", internal_name)
}

/// Resolve a simple class name directly to signature format
pub fn resolve_to_signature(simple_name: &str, current_package: Option<&str>) -> String {
    let internal_name = resolve_class_name_with_fallback(simple_name, current_package);
    internal_name_to_signature(&internal_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classpath_mapping() {
        let map = get_classpath_map();
        
        // Test some known mappings
        assert_eq!(map.get("Collection"), Some(&"java/util/Collection".to_string()));
        assert_eq!(map.get("List"), Some(&"java/util/List".to_string()));
        assert_eq!(map.get("Map"), Some(&"java/util/Map".to_string()));
        assert_eq!(map.get("Iterator"), Some(&"java/util/Iterator".to_string()));
        assert_eq!(map.get("Iterable"), Some(&"java/lang/Iterable".to_string()));
        assert_eq!(map.get("Object"), Some(&"java/lang/Object".to_string()));
        assert_eq!(map.get("String"), Some(&"java/lang/String".to_string()));
        assert_eq!(map.get("Serializable"), Some(&"java/io/Serializable".to_string()));
    }

    #[test]
    fn test_resolve_class_name() {
        assert_eq!(resolve_class_name("Collection"), Some("java/util/Collection"));
        assert_eq!(resolve_class_name("Iterable"), Some("java/lang/Iterable"));
        assert_eq!(resolve_class_name("Serializable"), Some("java/io/Serializable"));
        assert_eq!(resolve_class_name("NonExistent"), None);
    }

    #[test]
    fn test_resolve_to_signature() {
        assert_eq!(resolve_to_signature("Collection", None), "Ljava/util/Collection;");
        assert_eq!(resolve_to_signature("Iterable", None), "Ljava/lang/Iterable;");
        assert_eq!(resolve_to_signature("Serializable", None), "Ljava/io/Serializable;");
    }
}
