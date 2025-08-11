use super::{ReviewError, ReviewResult};
use crate::review::types::GlobalMemberIndex;
use crate::ast::*;

pub(crate) fn review_fields_of_class(class: &ClassDecl, global: &GlobalMemberIndex) -> ReviewResult<()> {
    use std::collections::HashSet;
    use crate::ast::Modifier::*;
    let mut seen: HashSet<String> = HashSet::new();
    for member in &class.body {
        if let ClassMember::Field(f) = member {
            // field type cannot be void
            if f.type_ref.name == "void" {
                return Err(ReviewError::DuplicateType(format!(
                    "field '{}' cannot have type void",
                    f.name
                )));
            }
            // visibility exclusivity and basic combinations
            {
                let mods = &f.modifiers;
                let public_set = mods.contains(&Public) as u8;
                let protected_set = mods.contains(&Protected) as u8;
                let private_set = mods.contains(&Private) as u8;
                if public_set + protected_set + private_set > 1 {
                    return Err(ReviewError::DuplicateMember(format!(
                        "conflicting visibility modifiers for field '{}'",
                        f.name
                    )));
                }
                // final and volatile cannot be both set
                if mods.contains(&Final) && mods.contains(&Volatile) {
                    return Err(ReviewError::DuplicateMember(format!(
                        "field '{}' cannot be both final and volatile",
                        f.name
                    )));
                }
            }
            if !seen.insert(f.name.clone()) {
                return Err(ReviewError::DuplicateMember(format!("field '{}'", f.name)));
            }
            // Validate static import usage in field initializer expressions
            if let Some(init) = &f.initializer { review_field_initializer_static_imports(init, global)?; }
        }
    }
    Ok(())
}

pub(crate) fn review_fields_of_interface(iface: &InterfaceDecl, global: &GlobalMemberIndex) -> ReviewResult<()> {
    use crate::ast::Modifier::*;
    for member in &iface.body {
        if let InterfaceMember::Field(f) = member {
            if f.type_ref.name == "void" {
                return Err(ReviewError::DuplicateType(format!(
                    "interface field '{}' cannot have type void",
                    f.name
                )));
            }
            // interface fields must be public static final (javac convention)
            let mods = &f.modifiers;
            let is_public = mods.contains(&Public);
            let is_static = mods.contains(&Static);
            let is_final = mods.contains(&Final);
            if !(is_public && is_static && is_final) {
                return Err(ReviewError::IllegalInterfaceFieldModifiers(f.name.clone()));
            }
            if let Some(init) = &f.initializer { review_field_initializer_static_imports(init, global)?; }
        }
    }
    Ok(())
}

fn review_field_initializer_static_imports(expr: &Expr, global: &GlobalMemberIndex) -> ReviewResult<()> {
    match expr {
        // Disallow compound assignments and increments on finals in initializers context (redundant but safe)
        Expr::Unary(u) => review_field_initializer_static_imports(&u.operand, global),
        Expr::Identifier(id) => {
            if let Some(ty) = global.static_explicit.get(&id.name) {
                if let Some(mt) = global.by_type.get(ty) {
                    if let Some(is_static) = mt.fields_static.get(&id.name) {
                        if !*is_static { return Err(ReviewError::IllegalStaticCall { typename: ty.clone(), name: id.name.clone() }); }
                    }
                }
            } else {
                for ty in &global.static_wildcard {
                    if let Some(mt) = global.by_type.get(ty) {
                        if let Some(is_static) = mt.fields_static.get(&id.name) {
                            if !*is_static { return Err(ReviewError::IllegalStaticCall { typename: ty.clone(), name: id.name.clone() }); }
                            break;
                        }
                    }
                }
            }
            Ok(())
        }
        Expr::FieldAccess(fa) => {
            if let Some(t) = &fa.target { if let Expr::Identifier(id) = &**t {
                if let Some(mt) = super::statements::resolve_type_in_index(global, &id.name) {
                    if let Some(is_static) = mt.fields_static.get(&fa.name) { if !*is_static { return Err(ReviewError::IllegalStaticCall { typename: id.name.clone(), name: fa.name.clone() }); } }
                }
            } }
            if let Some(t) = &fa.target { review_field_initializer_static_imports(t, global)?; }
            Ok(())
        }
        Expr::MethodCall(mc) => { if let Some(t) = &mc.target { review_field_initializer_static_imports(t, global)?; } for a in &mc.arguments { review_field_initializer_static_imports(a, global)?; } Ok(()) }
        Expr::ArrayAccess(acc) => { review_field_initializer_static_imports(&acc.array, global)?; review_field_initializer_static_imports(&acc.index, global) }
        Expr::Binary(b) => { review_field_initializer_static_imports(&b.left, global)?; review_field_initializer_static_imports(&b.right, global) }
        Expr::Unary(u) => review_field_initializer_static_imports(&u.operand, global),
        Expr::Assignment(a) => { review_field_initializer_static_imports(&a.target, global)?; review_field_initializer_static_imports(&a.value, global) }
        Expr::Cast(c) => review_field_initializer_static_imports(&c.expr, global),
        Expr::Conditional(c) => { review_field_initializer_static_imports(&c.condition, global)?; review_field_initializer_static_imports(&c.then_expr, global)?; review_field_initializer_static_imports(&c.else_expr, global) }
        Expr::New(n) => { for a in &n.arguments { review_field_initializer_static_imports(a, global)?; } Ok(()) }
        Expr::Parenthesized(p) => review_field_initializer_static_imports(p, global),
        _ => Ok(())
    }
}


