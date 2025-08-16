use std::fs;
use std::path::{Path, PathBuf};
use tolc::parser::parse_and_verify;
use std::env;
use walkdir::WalkDir;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::{TypeDecl, ClassMember};

fn java_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("java")
}

#[test]
#[ignore]
fn parse_all_java_files_under_tests_java() {
    // Initialize logger for diagnostics
    let _ = env_logger::builder()
        .is_test(true)
        .format_timestamp_millis()
        .filter_level(log::LevelFilter::Debug)
        .try_init();
    let root = java_root();
    assert!(root.exists(), "tests/java directory not found: {}", root.display());

    // Reference compiled classes root for javap parity
    let ref_root = classes_root();
    assert!(ref_root.exists(), "tests/classes/java not found: {}", ref_root.display());

    // Ensure classpath and compatibility mode are set when running under `cargo test`
    if env::var("TOLC_CLASSPATH").is_err() { env::set_var("TOLC_CLASSPATH", root.display().to_string()); }
    if env::var("TOLC_CLASSPATH_MAX_FILES").is_err() { env::set_var("TOLC_CLASSPATH_MAX_FILES", "60"); }
    if env::var("TOLC_CLASSPATH_MAX_FILE_SIZE").is_err() { env::set_var("TOLC_CLASSPATH_MAX_FILE_SIZE", "65536"); }
    if env::var("TOLC_JAVAC_COMPAT").is_err() { env::set_var("TOLC_JAVAC_COMPAT", "1"); }

    // Configuration for segmented testing
    let filter = std::env::var("JAVA_SUITE_FILTER").ok();
    let first_only = std::env::var("JAVA_SUITE_FIRST_ONLY").is_ok();
    
    // Segment configuration - run tests in chunks
    let segment_size = std::env::var("JAVA_SUITE_SEGMENT_SIZE")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(10); // Default: 10 files per segment
    
    let segment_number = std::env::var("JAVA_SUITE_SEGMENT")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(0); // Default: first segment
    
    // Limit total number of files to process for faster testing
    let max_files = std::env::var("TOLC_MAXFILES")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(usize::MAX);
    
    // Timeout configuration
    let timeout_seconds = std::env::var("JAVA_SUITE_TIMEOUT")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(10); // Default: 10 seconds

    eprintln!("[CONFIG] Segment: {}/?, Size: {}, Max files: {}, Timeout: {}s", 
              segment_number, segment_size, max_files, timeout_seconds);

    let mut failures: Vec<(String, String)> = Vec::new();
    let mut compared: usize = 0;
    let mut processed: usize = 0;
    let mut skipped: usize = 0;

    // Temp dir for our generated classes
    let out_dir = std::env::temp_dir().join("java_suite_compare");
    let _ = fs::create_dir_all(&out_dir);

    // Collect all Java files first for segmenting
    let mut java_files: Vec<_> = WalkDir::new(&root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| {
            let path = entry.path();
            entry.file_type().is_file() && 
            path.extension().map(|e| e == "java").unwrap_or(false)
        })
        .collect();
    
    // Skip the 10 known failing files for now to test compilation success
    let skip_files: &[&str] = &[
    ];
    
    java_files.retain(|entry| {
        let path_str = entry.path().display().to_string();
        !skip_files.iter().any(|skip| path_str.contains(skip))
    });
    
    eprintln!("[INFO] Skipping {} known failing files, remaining files: {}", skip_files.len(), java_files.len());
    
    // Apply filter if specified
    if let Some(f) = &filter {
        java_files.retain(|entry| entry.path().display().to_string().contains(f));
    }
    
    // Sort for consistent segmenting
    java_files.sort_by(|a, b| a.path().display().to_string().cmp(&b.path().display().to_string()));
    
    // Calculate segment boundaries
    let total_files = java_files.len();
    let start_idx = segment_number * segment_size;
    
    // Check if segment is out of bounds
    if start_idx >= total_files {
        eprintln!("[CONFIG] Total files: {}, Segment {} is out of bounds (start_idx: {})", 
                  total_files, segment_number, start_idx);
        return;
    }
    
    let end_idx = std::cmp::min(start_idx + segment_size, total_files);
    
    eprintln!("[CONFIG] Total files: {}, Processing segment {}-{} ({} files)", 
              total_files, start_idx, end_idx - 1, end_idx - start_idx);
    
    // Skip to our segment
    let segment_files = java_files.into_iter()
        .skip(start_idx)
        .take(end_idx - start_idx)
        .take(max_files);

    for entry in segment_files {
        let path = entry.path();
        let path_str = path.display().to_string();

        // Check timeout
        if processed > 0 && processed % 5 == 0 {
            eprintln!("[PROGRESS] Processed: {}, Compared: {}, Skipped: {}", processed, compared, skipped);
        }

        // Read source
        let file_name = path.file_stem().unwrap().to_string_lossy().to_string();
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => { 
                failures.push((path.display().to_string(), format!("IO error reading source: {}", e))); 
                if first_only { break; } else { continue; } 
            }
        };

        // Parse + review
        let ast = match parse_and_verify(&source) {
            Ok(a) => a,
            Err(e) => {
                let snippet: String = source.lines().take(30).collect::<Vec<_>>().join("\n");
                failures.push((path.display().to_string(), format!("parse error: {}\n--- snippet ---\n{}\n--------------", e, snippet)));
                if first_only { break; } else { continue; }
            }
        };

        // Determine package name and reference class path
        let package_name_opt = ast.package_decl.as_ref().map(|p| p.name.clone());
        let pkg_path_full = package_name_opt.as_deref().map(|p| p.replace('.', "/")).unwrap_or_else(|| "".to_string());
        let ref_pkg_sub = if let Some(pkg) = &package_name_opt { 
            if let Some(rest) = pkg.strip_prefix("java.") { 
                rest.replace('.', "/") 
            } else { 
                pkg.replace('.', "/") 
            } 
        } else { 
            String::new() 
        };
        let ref_class_path = if ref_pkg_sub.is_empty() { 
            ref_root.join(format!("{}.class", file_name)) 
        } else { 
            ref_root.join(&ref_pkg_sub).join(format!("{}.class", file_name)) 
        };
        
        if !ref_class_path.exists() { 
            eprintln!("[SKIP] No ref class: {}", ref_class_path.display()); 
            skipped += 1;
            continue; 
        }

        // Find matching top-level type by file name
        let type_decl_opt = ast.type_decls.iter().find(|td| match td { 
            TypeDecl::Class(c) => c.name == file_name, 
            TypeDecl::Interface(i) => i.name == file_name, 
            TypeDecl::Enum(e) => e.name == file_name, 
            TypeDecl::Annotation(a) => a.name == file_name 
        });
        let type_decl = match type_decl_opt { 
            Some(t) => t, 
            None => { 
                failures.push((path.display().to_string(), "no matching top-level type for file name".to_string())); 
                if first_only { break; } else { continue; } 
            } 
        };

        // Generate class with TOLC
        let mut cw = ClassWriter::new();
        cw.set_package_name(package_name_opt.as_deref());
        cw.set_debug(true);
        let gen_res = match type_decl {
            TypeDecl::Class(c) => cw.generate_class(c),
            TypeDecl::Interface(i) => cw.generate_interface(i),
            TypeDecl::Enum(e) => cw.generate_enum(e),
            TypeDecl::Annotation(a) => cw.generate_annotation(a),
        };
        if let Err(e) = gen_res { 
            failures.push((path.display().to_string(), format!("codegen error: {}", e))); 
            if first_only { break; } else { continue; } 
        }
        let class_file = cw.get_class_file();
        let class_bytes = class_file_to_bytes(&class_file);

        // Write our class to temp dir (package layout)
        let out_pkg_dir = if pkg_path_full.is_empty() { out_dir.clone() } else { out_dir.join(&pkg_path_full) };
        let _ = fs::create_dir_all(&out_pkg_dir);
        let our_class_path = out_pkg_dir.join(format!("{}.class", file_name));
        if let Err(e) = fs::write(&our_class_path, &class_bytes) { 
            failures.push((path.display().to_string(), format!("IO error writing our class: {}", e))); 
            if first_only { break; } else { continue; } 
        }

        // Run javap on both and compare normalized outputs
        let run_javap = |p: &Path| -> Result<String, String> {
            let out = std::process::Command::new("javap").arg("-c").arg("-verbose").arg(p).output().map_err(|e| format!("spawn javap failed: {}", e))?;
            if !out.status.success() { return Err(String::from_utf8_lossy(&out.stderr).to_string()); }
            let s = String::from_utf8_lossy(&out.stdout);
            let start = ["public class", "public abstract class", "public interface", "class "]
                .iter()
                .filter_map(|pat| s.find(pat))
                .min()
                .unwrap_or(0);
            let mut s = s[start..].replace("\r\n", "\n");
            s = strip_constant_pool(&s);
            s = strip_debug_tables(&s);
            s = strip_signatures_and_header_generics(&s);
            s = normalize_cp_indices(&s);
            s = collapse_spaces(&s);
            Ok(s)
        };

        // Check for inner class files
        let ref_dir = ref_class_path.parent().unwrap();
        let inner_class_pattern = format!("{}$", file_name);
        let mut inner_class_files = Vec::new();
        
        if let Ok(entries) = fs::read_dir(ref_dir) {
            for entry in entries.flatten() {
                let entry_name = entry.file_name().to_string_lossy().to_string();
                if entry_name.starts_with(&inner_class_pattern) && entry_name.ends_with(".class") {
                    inner_class_files.push(entry_name);
                }
            }
        }
        
        // If there are inner class files, report them for now
        if !inner_class_files.is_empty() {
            eprintln!("[INFO] {} has inner classes: {:?}", file_name, inner_class_files);
            // For now, we'll still compare the main class file, but note the inner classes
        }

        match (run_javap(&our_class_path), run_javap(&ref_class_path)) {
            (Ok(a), Ok(b)) => {
                if !compare_javap_outputs(&a, &b) {
                    failures.push((path.display().to_string(), format!("javap mismatch\n--- ours ---\n{}\n--- ref ---\n{}", a, b)));
                    if first_only { break; }
                } else {
                    compared += 1;
                }
            }
            (Err(e), _) => { 
                failures.push((our_class_path.display().to_string(), format!("javap error: {}", e))); 
                if first_only { break; } 
            }
            (_, Err(e)) => { 
                failures.push((ref_class_path.display().to_string(), format!("javap error: {}", e))); 
                if first_only { break; } 
            }
        }
        
        processed += 1;
    }

    eprintln!("[SUMMARY] Segment {}/{} complete. Processed: {}, Compared: {}, Skipped: {}, Failures: {}", 
              segment_number, (total_files + segment_size - 1) / segment_size, processed, compared, skipped, failures.len());

    if !failures.is_empty() {
        eprintln!("Failures in this segment:\n");
        for (p, e) in &failures { 
            eprintln!("- {} -> {}\n", p, e); 
        }
        panic!("javap parity failures in segment {}: {}", segment_number, failures.len());
    } else {
        eprintln!("Success! Segment {} completed with {} files processed, {} compared, {} skipped.", 
                  segment_number, processed, compared, skipped);
    }
}

fn strip_constant_pool(s: &str) -> String {
    let mut out = Vec::new();
    let mut in_cp = false;
    for line in s.lines() {
        if !in_cp && line.trim_start().starts_with("Constant pool:") { in_cp = true; continue; }
        if in_cp {
            if line.trim().is_empty() { in_cp = false; continue; }
            if line.trim_start().starts_with('#') { continue; }
            if line.trim_start().starts_with('{') { in_cp = false; out.push(line); continue; }
            continue;
        }
        out.push(line);
    }
    out.join("\n")
}

fn strip_debug_tables(s: &str) -> String {
    let mut out = Vec::new();
    let mut skip = false;
    for line in s.lines() {
        let trimmed = line.trim_start();
        if !skip && (trimmed.starts_with("LineNumberTable:") || trimmed.starts_with("LocalVariableTable:")) {
            skip = true;
            continue;
        }
        if skip {
            if trimmed.is_empty() {
                skip = false;
                continue;
            }
            if trimmed.starts_with("public ") || trimmed.starts_with("}") || trimmed.starts_with("Code:") || trimmed.starts_with("descriptor:") || trimmed.starts_with("flags:") {
                skip = false;
                out.push(line);
                continue;
            }
            continue;
        }
        out.push(line);
    }
    out.join("\n")
}

fn strip_signatures_and_header_generics(s: &str) -> String {
    let mut out = Vec::new();
    for line in s.lines() {
        // Simplified: just keep all lines as-is
        // This avoids complex parsing that might introduce bugs
        out.push(line.to_string());
    }
    out.join("\n")
}

fn normalize_cp_indices(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut it = s.chars().peekable();
    while let Some(ch) = it.next() {
        if ch == '#' {
            let mut saw_digit = false;
            while let Some(&next) = it.peek() {
                if next.is_ascii_digit() { saw_digit = true; it.next(); } else { break; }
            }
            if saw_digit { out.push_str("#X"); } else { out.push('#'); }
        } else {
            out.push(ch);
        }
    }
    out
}

fn collapse_spaces(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut prev_space = false;
    for ch in s.chars() {
        if ch == '\r' { continue; }
        let is_space = ch == ' ' || ch == '\t';
        if is_space {
            if !prev_space { out.push(' '); }
            prev_space = true;
        } else {
            out.push(ch);
            prev_space = false;
        }
    }
    out
        .lines()
        .map(|l| l.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
}

fn classes_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("classes").join("java")
}

/// Compare javap outputs line by line, ignoring attribute order differences
/// JVM doesn't care about the order of attributes, so we normalize them
fn compare_javap_outputs(ours: &str, reference: &str) -> bool {
    let our_lines: Vec<&str> = ours.lines().collect();
    let ref_lines: Vec<&str> = reference.lines().collect();
    
    // Quick check: if line counts are very different, they're probably not the same
    if (our_lines.len() as i32 - ref_lines.len() as i32).abs() > 2 {
        return false;
    }
    
    // Normalize both outputs by sorting attributes within their sections
    let normalized_ours = normalize_javap_output(&our_lines);
    let normalized_ref = normalize_javap_output(&ref_lines);
    
    // Compare normalized outputs
    normalized_ours == normalized_ref
}

/// Normalize javap output by sorting attributes within their sections
/// This handles cases where JVM attribute order might differ
fn normalize_javap_output(lines: &[&str]) -> Vec<String> {
    let mut result = Vec::new();
    let mut current_section = Vec::new();
    let mut in_method = false;
    let mut in_interface = false;
    
    for &line in lines {
        let trimmed = line.trim();
        
        // Detect section boundaries
        if trimmed.starts_with("public ") && (trimmed.contains("class") || trimmed.contains("interface")) {
            // Class/interface declaration - flush current section and start new
            if !current_section.is_empty() {
                result.extend(sort_section_attributes(&current_section));
                current_section.clear();
            }
            in_interface = trimmed.contains("interface");
            result.push(line.to_string());
            continue;
        }
        
        if trimmed.starts_with("public ") && trimmed.contains("(") && !trimmed.contains("class") && !trimmed.contains("interface") {
            // Method declaration - flush current section and start new
            if !current_section.is_empty() {
                result.extend(sort_section_attributes(&current_section));
                current_section.clear();
            }
            in_method = true;
            result.push(line.to_string());
            continue;
        }
        
        if trimmed == "}" {
            // End of section - flush current section
            if !current_section.is_empty() {
                result.extend(sort_section_attributes(&current_section));
                current_section.clear();
            }
            in_method = false;
            result.push(line.to_string());
            continue;
        }
        
        // Collect lines in current section
        current_section.push(line.to_string());
    }
    
    // Flush any remaining section
    if !current_section.is_empty() {
        result.extend(sort_section_attributes(&current_section));
    }
    
    result
}

/// Sort attributes within a section to normalize order
fn sort_section_attributes(lines: &[String]) -> Vec<String> {
    let mut result = Vec::new();
    let mut attributes = Vec::new();
    let mut other_lines = Vec::new();
    
    for line in lines {
        let trimmed = line.trim();
        
        // Identify attribute lines
        if trimmed.starts_with("descriptor:") || 
           trimmed.starts_with("flags:") || 
           trimmed.starts_with("Signature:") || 
           trimmed.starts_with("Exceptions:") ||
           trimmed.starts_with("SourceFile:") ||
           trimmed.starts_with("Code:") ||
           trimmed.starts_with("LineNumberTable:") ||
           trimmed.starts_with("LocalVariableTable:") ||
           trimmed.starts_with("InnerClasses:") ||
           trimmed.starts_with("RuntimeVisibleAnnotations:") ||
           trimmed.starts_with("RuntimeInvisibleAnnotations:") ||
           trimmed.starts_with("RuntimeVisibleParameterAnnotations:") ||
           trimmed.starts_with("RuntimeInvisibleParameterAnnotations:") ||
           trimmed.starts_with("AnnotationDefault:") ||
           trimmed.starts_with("Synthetic:") ||
           trimmed.starts_with("Deprecated:") ||
           trimmed.starts_with("EnclosingMethod:") ||
           trimmed.starts_with("BootstrapMethods:") ||
           trimmed.starts_with("MethodParameters:") ||
           trimmed.starts_with("Module:") ||
           trimmed.starts_with("ModulePackages:") ||
           trimmed.starts_with("ModuleMainClass:") ||
           trimmed.starts_with("NestHost:") ||
           trimmed.starts_with("NestMembers:") ||
           trimmed.starts_with("PermittedSubclasses:") ||
           trimmed.starts_with("Record:") ||
           trimmed.starts_with("throws ") {
            attributes.push(line.clone());
        } else {
            other_lines.push(line.clone());
        }
    }
    
    // Sort attributes for consistent ordering
    attributes.sort();
    
    // Add other lines first, then sorted attributes
    result.extend(other_lines);
    result.extend(attributes);
    
    result
}
