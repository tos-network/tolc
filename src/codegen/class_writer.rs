//! JavaC-aligned Class Writer for generating Java .class files
//! 
//! This module provides a Gen-based ClassWriter that uses the JavaC-aligned
//! code generation architecture to convert AST declarations into Java bytecode.

use super::gen::Gen;
use crate::codegen::ClassFile;
use crate::common::config::Config;
use crate::ast::*;
use crate::common::error::Result;
use std::collections::HashMap;

/// JavaC-aligned Class writer for generating Java bytecode
pub struct ClassWriter {
    /// The JavaC-aligned bytecode generator
    gen: Gen,
    /// Configuration settings
    config: Config,
    /// Current package name for class resolution
    package_name: Option<String>,
    /// Annotation retention index for the current compilation unit
    annotation_retention: HashMap<String, crate::codegen::attribute::RetentionPolicy>,
    /// All types in the current compilation unit for interface method resolution
    all_types: Option<Vec<crate::ast::TypeDecl>>,
    /// Generic signatures stored during TransTypes phase
    generic_signatures: Option<HashMap<String, String>>,
}

impl ClassWriter {
    /// Create a new JavaC-aligned class writer
    pub fn new() -> Self {
        Self {
            gen: Gen::new(),
            config: Config::default(),
            package_name: None,
            annotation_retention: HashMap::new(),
            all_types: None,
            generic_signatures: None,
        }
    }

    /// Create a new JavaC-aligned class writer with configuration
    pub fn new_with_config(config: Config) -> Self {
        Self {
            gen: Gen::new(),
            config,
            package_name: None,
            annotation_retention: HashMap::new(),
            all_types: None,
            generic_signatures: None,
        }
    }

    /// Set the package name for class resolution
    pub fn set_package_name<S: Into<String>>(&mut self, package: Option<S>) {
        self.package_name = package.map(|s| s.into());
    }
    
    /// Set all types for interface method resolution
    pub fn set_all_types(&mut self, all_types: Vec<crate::ast::TypeDecl>) {
        self.all_types = Some(all_types);
    }

    /// Set annotation retention index for the compilation unit
    pub fn set_annotation_retention_index(&mut self, idx: HashMap<String, crate::codegen::attribute::RetentionPolicy>) {
        self.annotation_retention = idx;
    }
    
    /// Set generic signatures from TransTypes phase
    pub fn set_generic_signatures(&mut self, signatures: &HashMap<String, String>) {
        self.generic_signatures = Some(signatures.clone());
    }
    
    /// Set inner class relationships for InnerClasses attribute generation
    pub fn set_inner_class_relationships(&mut self, relationships: &[crate::codegen::InnerClassInfo]) {
        self.gen.set_inner_class_relationships(relationships);
    }
    
    /// Set parent class name for inner class generation
    pub fn set_parent_class_name(&mut self, parent_name: Option<String>) {
        self.gen.set_parent_class_name(parent_name);
    }

    /// Enable or disable debug info generation
    pub fn set_debug(&mut self, debug: bool) { 
        self.config.debug = debug; 
    }

    /// Enable or disable StackMapTable frame emission
    pub fn set_emit_frames(&mut self, emit: bool) { 
        self.config.emit_frames = emit; 
    }

    /// Generate bytecode for a class declaration using JavaC-aligned Gen
    pub fn generate_class(&mut self, class: &ClassDecl) -> Result<()> {
        // Initialize the Gen with the class context
        if let Some(ref all_types) = self.all_types {
            self.gen.init_class(class.clone(), all_types.clone())?;
        }
        
        // Set configuration in Gen (for StackMapTable generation)
        self.gen.set_config(self.config.clone());
        
        // Set package context in Gen
        if let Some(ref package) = self.package_name {
            self.gen.set_package_context(package.clone());
        }
        
        // Set generic signatures in Gen
        self.gen.set_generic_signatures(self.generic_signatures.clone());
        
        // Set annotation retention context
        self.gen.set_annotation_retention(self.annotation_retention.clone());
        
        // Generate the class using JavaC patterns
        self.gen.generate_class_decl(class)?;
        
        Ok(())
    }

    /// Generate bytecode for an interface declaration using JavaC-aligned Gen
    pub fn generate_interface(&mut self, interface: &InterfaceDecl) -> Result<()> {
        // Initialize the Gen with interface context
        if let Some(ref all_types) = self.all_types {
            self.gen.init_interface(interface.clone(), all_types.clone())?;
        }
        
        // Set package context in Gen
        if let Some(ref package) = self.package_name {
            self.gen.set_package_context(package.clone());
        }
        
        // Set annotation retention context
        self.gen.set_annotation_retention(self.annotation_retention.clone());
        
        // Generate the interface using JavaC patterns
        self.gen.generate_interface_decl(interface)?;
        
        Ok(())
    }

    /// Generate bytecode for an enum declaration using JavaC-aligned Gen
    pub fn generate_enum(&mut self, enum_decl: &EnumDecl) -> Result<()> {
        // Initialize the Gen with enum context
        if let Some(ref all_types) = self.all_types {
            self.gen.init_enum(enum_decl.clone(), all_types.clone())?;
        }
        
        // Set package context in Gen
        if let Some(ref package) = self.package_name {
            self.gen.set_package_context(package.clone());
        }
        
        // Set annotation retention context
        self.gen.set_annotation_retention(self.annotation_retention.clone());
        
        // Generate the enum using JavaC patterns
        self.gen.generate_enum_decl(enum_decl)?;
        
        Ok(())
    }

    /// Generate bytecode for an annotation declaration using JavaC-aligned Gen
    pub fn generate_annotation(&mut self, annotation: &AnnotationDecl) -> Result<()> {
        // Initialize the Gen with annotation context
        if let Some(ref all_types) = self.all_types {
            self.gen.init_annotation(annotation.clone(), all_types.clone())?;
        }
        
        // Set package context in Gen
        if let Some(ref package) = self.package_name {
            self.gen.set_package_context(package.clone());
        }
        
        // Set annotation retention context
        self.gen.set_annotation_retention(self.annotation_retention.clone());
        
        // Generate the annotation using JavaC patterns
        self.gen.generate_annotation_decl(annotation)?;
        
        Ok(())
    }

    /// Get the generated class file
    /// 
    /// This consumes the ClassWriter and returns the final ClassFile
    pub fn get_class_file(self) -> ClassFile {
        self.gen.get_class_file()
    }

    /// Get a reference to the underlying Gen for advanced operations
    pub fn get_gen(&self) -> &Gen {
        &self.gen
    }

    /// Get a mutable reference to the underlying Gen for advanced operations
    pub fn get_gen_mut(&mut self) -> &mut Gen {
        &mut self.gen
    }

    /// Get the current configuration
    pub fn get_config(&self) -> &Config {
        &self.config
    }

    /// Set the configuration
    pub fn set_config(&mut self, config: Config) {
        self.config = config;
    }
}

impl Default for ClassWriter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Location};

    fn create_span() -> Span {
        Span::new(Location::new(0, 0, 0), Location::new(0, 0, 0))
    }

    #[test]
    fn test_class_writer_creation() {
        let writer = ClassWriter::new();
        assert!(writer.package_name.is_none());
        assert!(writer.all_types.is_none());
        assert!(writer.annotation_retention.is_empty());
    }

    #[test]
    fn test_class_writer_with_config() {
        let config = Config {
            debug: true,
            emit_frames: false,
            ..Default::default()
        };
        let writer = ClassWriter::new_with_config(config);
        assert!(writer.config.debug);
        assert!(!writer.config.emit_frames);
    }

    #[test]
    fn test_package_name_setting() {
        let mut writer = ClassWriter::new();
        writer.set_package_name(Some("com.example"));
        assert_eq!(writer.package_name, Some("com.example".to_string()));
        
        writer.set_package_name(None::<String>);
        assert!(writer.package_name.is_none());
    }

    #[test]
    fn test_configuration_methods() {
        let mut writer = ClassWriter::new();
        
        writer.set_debug(true);
        assert!(writer.config.debug);
        
        writer.set_emit_frames(false);
        assert!(!writer.config.emit_frames);
    }

    #[test]
    fn test_annotation_retention_setting() {
        let mut writer = ClassWriter::new();
        let mut retention_map = HashMap::new();
        retention_map.insert("TestAnnotation".to_string(), crate::codegen::attribute::RetentionPolicy::Runtime);
        
        writer.set_annotation_retention_index(retention_map);
        assert!(!writer.annotation_retention.is_empty());
        assert_eq!(writer.annotation_retention.get("TestAnnotation"), 
                   Some(&crate::codegen::attribute::RetentionPolicy::Runtime));
    }
}