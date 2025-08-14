use super::{ReviewError, ReviewResult};
use crate::ast::{Annotation, AnnotationDecl, ConstructorDecl, FieldDecl, MethodDecl, Parameter, ClassDecl, InterfaceDecl, EnumDecl};

pub(crate) fn review_annotation(a: &AnnotationDecl) -> ReviewResult<()> {
    // Disallow duplicates on the annotation type itself
    check_no_duplicate_annotations(&a.annotations)
}

// Declaration-site annotation placement and repeat checks (subset)
// We model simple rules:
// - Disallow duplicate annotations with the same simple name on the same declaration
// - Allow annotations on class/interface/enum/annotation types, fields, methods, constructors, parameters
// Type-annotations placement is validated at verify stage; here we only enforce duplicates on declarations.

fn check_no_duplicate_annotations(anns: &[Annotation]) -> ReviewResult<()> {
    use std::collections::HashSet;
    let mut seen: HashSet<&str> = HashSet::new();
    for a in anns {
        if !seen.insert(a.name.as_str()) {
            return Err(ReviewError::DuplicateMember(format!("repeated annotation '@{}' not allowed here", a.name)));
        }
    }
    Ok(())
}

pub(crate) fn review_field_annotations(f: &FieldDecl) -> ReviewResult<()> {
    check_no_duplicate_annotations(&f.annotations)
}

pub(crate) fn review_method_annotations(m: &MethodDecl) -> ReviewResult<()> {
    check_no_duplicate_annotations(&m.annotations)?;
    for p in &m.parameters { review_parameter_annotations(p)?; }
    Ok(())
}

pub(crate) fn review_ctor_annotations(c: &ConstructorDecl) -> ReviewResult<()> {
    check_no_duplicate_annotations(&c.annotations)?;
    for p in &c.parameters { review_parameter_annotations(p)?; }
    Ok(())
}

pub(crate) fn review_parameter_annotations(p: &Parameter) -> ReviewResult<()> {
    check_no_duplicate_annotations(&p.annotations)
}

pub(crate) fn review_type_decl_annotations_class(c: &ClassDecl) -> ReviewResult<()> {
    check_no_duplicate_annotations(&c.annotations)
}

pub(crate) fn review_type_decl_annotations_interface(i: &InterfaceDecl) -> ReviewResult<()> {
    check_no_duplicate_annotations(&i.annotations)
}

pub(crate) fn review_type_decl_annotations_enum(e: &EnumDecl) -> ReviewResult<()> {
    check_no_duplicate_annotations(&e.annotations)
}


