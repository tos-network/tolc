use crate::codegen::attribute::AttributeInfo;
use crate::codegen::class::ClassFile;
use crate::codegen::flag::access_flags;
use crate::codegen::constpool::Constant;
use crate::codegen::attribute::{AnnotationEntry, TypeAnnotationEntry, RetentionPolicy, AnnotationTarget};

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum AttributesVerifyError {
    #[error("Attribute {0} requires class file version >= {1}")]
    AttributeRequiresVersion(&'static str, u16),
    #[error("Duplicate class attribute: {0}")]
    DuplicateClassAttribute(&'static str),
    #[error("Invalid Signature content")]
    InvalidSignature,
    #[error("Attribute {0} is not allowed before class file version {1}")]
    VersionGate(&'static str, u16),
    #[error("Invalid attribute content: {0}")]
    InvalidContent(&'static str),
}

pub type Result<T> = std::result::Result<T, AttributesVerifyError>;

pub fn verify(class_file: &ClassFile) -> Result<()> {
    let major = class_file.major_version;
    let mut has_signature = false;
    let mut has_enclosing_method = false;
    let mut enclosing_class_idx: Option<u16> = None;
    let mut has_inner_classes = false;
    let mut inner_for_this: Option<(u16 /*outer*/, u16 /*inner_name*/)> = None;
    let mut has_permitted_subclasses = false;
    let mut has_module_attr = false;
    for a in &class_file.attributes {
        match &a.info {
            AttributeInfo::PermittedSubclasses(_) => {
                if major < 59 { // Java 15+
                    return Err(AttributesVerifyError::AttributeRequiresVersion("PermittedSubclasses", 59));
                }
                has_permitted_subclasses = true;
            }
            AttributeInfo::Module(_)
            | AttributeInfo::ModulePackages(_)
            | AttributeInfo::ModuleMainClass(_) => {
                if major < 53 { // Java 9+
                    return Err(AttributesVerifyError::AttributeRequiresVersion("Module*", 53));
                }
                if let AttributeInfo::Module(m) = &a.info {
                    has_module_attr = true;
                    // module name/version must be Utf8
                    if !cp_is_utf8(class_file, m.name.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.name")); }
                    if !cp_is_utf8(class_file, m.version.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.version")); }
                    // requires
                    for r in m.requires.iter() {
                        if !cp_is_utf8(class_file, r.requires.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.requires")); }
                        if !cp_is_utf8(class_file, r.version.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.requires.version")); }
                        let _ = r.flags; // currently not validated
                    }
                    // exports
                    for e in m.exports.iter() {
                        if !cp_is_utf8(class_file, e.exports.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.exports")); }
                        for to in e.exports_to.iter() { if !cp_is_utf8(class_file, to.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.exports_to")); } }
                        let _ = e.flags;
                    }
                    // opens
                    for o in m.opens.iter() {
                        if !cp_is_utf8(class_file, o.opens.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.opens")); }
                        for to in o.opens_to.iter() { if !cp_is_utf8(class_file, to.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.opens_to")); } }
                        let _ = o.flags;
                    }
                    // uses/provides
                    for u in m.uses.iter() { if !cp_is_utf8(class_file, u.uses.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.uses")); } }
                    for p in m.provides.iter() {
                        if !cp_is_utf8(class_file, p.provides.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.provides")); }
                        for w in p.provides_with.iter() { if !cp_is_utf8(class_file, w.as_u16()) { return Err(AttributesVerifyError::InvalidContent("Module.provides_with")); } }
                    }
                }
            }
            AttributeInfo::NestHost(_)
            | AttributeInfo::NestMembers(_) => {
                if major < 55 { // Java 11+
                    return Err(AttributesVerifyError::AttributeRequiresVersion("Nest*", 55));
                }
            }
            AttributeInfo::Record(_) => {
                if major < 58 { // Java 14+
                    return Err(AttributesVerifyError::AttributeRequiresVersion("Record", 58));
                }
            }

            // Future gates (placeholders present only when such attributes exist in our writer)
            AttributeInfo::BootstrapMethods(bsm) => {
                // Basic index sanity: all bootstrap method refs and args indices within CP bounds
                for bm in &bsm.bootstrap_methods {
                    let mh_idx = bm.bootstrap_method.as_u16();
                    let mh_u = (mh_idx as usize).saturating_sub(1);
                    if class_file.constant_pool.constants.get(mh_u).is_none() {
                        return Err(AttributesVerifyError::InvalidContent("BootstrapMethods.method_ref"));
                    }
                    for arg in bm.bootstrap_arguments.iter() {
                        let u = arg.as_u16() as usize;
                        if class_file.constant_pool.constants.get(u.saturating_sub(1)).is_none() {
                            return Err(AttributesVerifyError::InvalidContent("BootstrapMethods.argument"));
                        }
                    }
                }
            }
            AttributeInfo::InnerClasses(ic) => {
                has_inner_classes = true;
                // Validate each entry indices kinds
                for entry in &ic.classes {
                    let inner = entry.inner_class.as_u16();
                    let outer = entry.outer_class.as_u16();
                    let iname = entry.inner_name.as_u16();
                    if !matches!(class_file.constant_pool.constants.get(inner.saturating_sub(1) as usize), Some(Constant::Class(_))) {
                        return Err(AttributesVerifyError::InvalidContent("InnerClasses.inner_class"));
                    }
                    if outer != 0 {
                        if !matches!(class_file.constant_pool.constants.get(outer.saturating_sub(1) as usize), Some(Constant::Class(_))) {
                            return Err(AttributesVerifyError::InvalidContent("InnerClasses.outer_class"));
                        }
                    }
                    if iname != 0 {
                        if !cp_is_utf8(class_file, iname) { return Err(AttributesVerifyError::InvalidContent("InnerClasses.inner_name")); }
                    }
                    // Try find entry for this class
                    if inner == class_file.this_class {
                        inner_for_this = Some((outer, iname));
                    }
                }
                // Cross-validate non-this entries: inner class name pattern matches owner qualification, when both present
                for entry in &ic.classes {
                    let inner = entry.inner_class.as_u16();
                    let outer = entry.outer_class.as_u16();
                    let iname = entry.inner_name.as_u16();
                    if inner == 0 || outer == 0 || iname == 0 { continue; }
                    // Resolve Utf8 names
                    let inner_class_name_utf = match class_file.constant_pool.constants.get((inner as usize).saturating_sub(1)) {
                        Some(Constant::Class(nidx)) => match class_file.constant_pool.constants.get((*nidx as usize).saturating_sub(1)) { Some(Constant::Utf8(s)) => s.clone(), _ => continue },
                        _ => continue,
                    };
                    let outer_class_name_utf = match class_file.constant_pool.constants.get((outer as usize).saturating_sub(1)) {
                        Some(Constant::Class(nidx)) => match class_file.constant_pool.constants.get((*nidx as usize).saturating_sub(1)) { Some(Constant::Utf8(s)) => s.clone(), _ => continue },
                        _ => continue,
                    };
                    let simple_inner_utf = match class_file.constant_pool.constants.get((iname as usize).saturating_sub(1)) { Some(Constant::Utf8(s)) => s.clone(), _ => continue };
                    // Expect inner name to be the segment after '$' or '.' naming, and outer to be the prefix
                    if !(inner_class_name_utf == format!("{}${}", outer_class_name_utf, simple_inner_utf)
                        || inner_class_name_utf == format!("{}/{}", outer_class_name_utf, simple_inner_utf)) {
                        return Err(AttributesVerifyError::InvalidContent("InnerClasses name/owner mismatch"));
                    }
                }
            }
            AttributeInfo::EnclosingMethod(em) => {
                has_enclosing_method = true;
                enclosing_class_idx = Some(em.class.as_u16());
            }
            AttributeInfo::Signature(sig_attr) => {
                if has_signature {
                    return Err(AttributesVerifyError::DuplicateClassAttribute("Signature"));
                }
                has_signature = true;
                // Validate signature string content
                if let Some(s) = super_signature_string(class_file, sig_attr.signature.as_u16()) {
                    if !crate::verify::signature::is_valid_class_signature(&s) { return Err(AttributesVerifyError::InvalidSignature); }
                    // extra: basic lexical constraints
                    if !is_valid_signature(&s) { return Err(AttributesVerifyError::InvalidSignature); }
                } else {
                    return Err(AttributesVerifyError::InvalidSignature);
                }
            }
            AttributeInfo::RuntimeVisibleAnnotations(rva) => { validate_annotations_targets(rva.annotations.as_slice(), true)?; }
            AttributeInfo::RuntimeInvisibleAnnotations(ria) => { validate_annotations_targets(ria.annotations.as_slice(), false)?; }
            AttributeInfo::RuntimeVisibleTypeAnnotations(rvta) => { validate_type_annotations_for_class(rvta.annotations.as_slice(), true)?; }
            AttributeInfo::RuntimeInvisibleTypeAnnotations(rita) => { validate_type_annotations_for_class(rita.annotations.as_slice(), false)?; }


            _ => {}
        }
    }
    // Sealed vs permitted subclasses consistency (only for Java 17+)
    // Note: ACC_SEALED (0x0020) shares the same bit as ACC_SUPER but has different meaning in Java 17+
    let is_sealed = major >= 61 && (class_file.access_flags & access_flags::ACC_SEALED) != 0;
    if is_sealed && !has_permitted_subclasses {
        return Err(AttributesVerifyError::InvalidContent("sealed class missing PermittedSubclasses"));
    }
    if !is_sealed && has_permitted_subclasses {
        return Err(AttributesVerifyError::InvalidContent("PermittedSubclasses without sealed flag"));
    }
    // Module attribute and ACC_MODULE must align
    let is_module_flag = (class_file.access_flags & access_flags::ACC_MODULE) != 0;
    if is_module_flag && !has_module_attr { return Err(AttributesVerifyError::InvalidContent("ACC_MODULE without Module attribute")); }
    if !is_module_flag && has_module_attr { return Err(AttributesVerifyError::InvalidContent("Module attribute without ACC_MODULE")); }
    // Cross-attribute consistency: EnclosingMethod implies this is an inner/anon class; require matching InnerClasses entry
    if has_enclosing_method {
        if !has_inner_classes { return Err(AttributesVerifyError::InvalidContent("EnclosingMethod without InnerClasses")); }
        if let Some(enc_cls) = enclosing_class_idx {
            if let Some((outer, _inner_name)) = inner_for_this {
                if outer != enc_cls { return Err(AttributesVerifyError::InvalidContent("InnerClasses.outer != EnclosingMethod.class")); }
            } else {
                return Err(AttributesVerifyError::InvalidContent("Missing InnerClasses entry for enclosed class"));
            }
        }
    }
    Ok(())
}

fn cp_is_utf8(cf: &ClassFile, idx: u16) -> bool {
    let i = (idx as usize).saturating_sub(1);
    matches!(cf.constant_pool.constants.get(i), Some(Constant::Utf8(_)))
}

fn validate_annotations_targets(anns: &[AnnotationEntry], runtime: bool) -> Result<()> {
    // Java 8 scope: Retention=Runtime must be in visible set; Retention=Class must be in invisible set is allowed (we don't enforce), Source is not emitted
    for a in anns {
        if let Some(RetentionPolicy::Runtime) = a.retention {
            if !runtime { return Err(AttributesVerifyError::InvalidContent("Runtime retention annotation in invisible set")); }
        }
        // Basic target sanity: if present, ensure not empty
        if !a.targets.is_empty() {
            // No context check here; per-element validation elsewhere
        }
    }
    Ok(())
}

fn validate_type_annotations_for_class(anns: &[TypeAnnotationEntry], runtime: bool) -> Result<()> {
    for a in anns {
        if let Some(RetentionPolicy::Runtime) = a.retention {
            if !runtime { return Err(AttributesVerifyError::InvalidContent("Runtime retention type-annotation in invisible set")); }
        }
        // On class: allow TYPE_USE and TYPE_PARAMETER; reject METHOD, FIELD, PARAMETER-only contexts
        for t in &a.targets {
            match t {
                AnnotationTarget::Type | AnnotationTarget::TypeParameter | AnnotationTarget::TypeUse | AnnotationTarget::AnnotationType | AnnotationTarget::Package => {}
                AnnotationTarget::Field | AnnotationTarget::Method | AnnotationTarget::Parameter | AnnotationTarget::Constructor | AnnotationTarget::LocalVariable => {
                    return Err(AttributesVerifyError::InvalidContent("Type annotation target not valid on class"));
                }
            }
        }
    }
    Ok(())
}

fn super_signature_string(class_file: &ClassFile, u: u16) -> Option<String> {
    let idx = (u as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(crate::codegen::constpool::Constant::Utf8(s)) => Some(s.clone()),
        _ => None,
    }
}

// Minimal JVMS signature content check (mid-term): non-empty, no whitespace, balanced '<' '>' pairs
pub fn is_valid_signature(sig: &str) -> bool {
    if sig.is_empty() { return false; }
    if sig.chars().any(|c| c.is_whitespace()) { return false; }
    let mut depth: i32 = 0;
    for ch in sig.chars() {
        if ch == '<' { depth += 1; }
        if ch == '>' { depth -= 1; if depth < 0 { return false; } }
    }
    depth == 0
}


