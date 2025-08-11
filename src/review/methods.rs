use super::{ReviewError, ReviewResult};
use super::types::GlobalMemberIndex;
use crate::ast::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MethodKey { name: String, param_types: Vec<String> }

pub(crate) fn review_methods_of_class(class: &ClassDecl, global: &GlobalMemberIndex) -> ReviewResult<()> {
    use std::collections::HashSet;
    let mut seen: HashSet<MethodKey> = HashSet::new();
    // Detect multiple varargs constructors
    let mut varargs_ctor_count = 0usize;
    for member in &class.body {
        if let ClassMember::Method(m) = member {
            // visibility exclusivity: public/protected/private
            ensure_visibility_exclusive(&m.modifiers, &m.name)?;
            // basic illegal combos akin to Check.java subset
            use crate::ast::Modifier::*;
            if m.modifiers.contains(&Abstract) {
                if m.modifiers.contains(&Private)
                    || m.modifiers.contains(&Static)
                    || m.modifiers.contains(&Final)
                    || m.modifiers.contains(&Native)
                    || m.modifiers.contains(&Synchronized)
                {
                    return Err(ReviewError::DuplicateMember(format!(
                        "illegal modifier combination for abstract method '{}'",
                        m.name
                    )));
                }
            }
            // duplicate parameter names within a single method
            {
                use std::collections::HashSet;
                let mut pseen: HashSet<String> = HashSet::new();
                for p in &m.parameters {
                    if !pseen.insert(p.name.clone()) {
                        return Err(ReviewError::DuplicateParameter(p.name.clone()));
                    }
                }
            }
            let key = MethodKey {
                name: m.name.clone(),
                param_types: m.parameters.iter().map(|p| p.type_ref.name.clone()).collect(),
            };
            if !seen.insert(key.clone()) {
                return Err(ReviewError::DuplicateMember(format!(
                    "method '{}({})'",
                    key.name,
                    key.param_types.len()
                )));
            }
            // constructor rules placeholder: if name == class.name => treat as ctor; can extend later
            if m.name == class.name && m.parameters.iter().any(|p| p.varargs) {
                varargs_ctor_count += 1;
            }
            // basic must-return check for non-void methods with a body
            if m.body.is_some() {
                super::statements::review_method_body_return(m)?;
                // Arity check for unqualified calls inside this method using local method table
                let (
                    arities,
                    varargs_min,
                    signatures,
                    ctor_arities,
                    ctor_varargs_min,
                    ctor_signatures,
                    methods_static,
                ) = build_method_tables_for_class(class);
                if let Some(body) = &m.body {
                    super::statements::review_body_call_arity(&class.name, body, &arities, &signatures, &varargs_min, &ctor_arities, &ctor_signatures, &ctor_varargs_min, Some(global), Some(&methods_static), true)?;
                }
                // Local duplicate vars, literal initializer compatibility, and DA/DR seed
                if let Some(body) = &m.body {
                    use std::collections::HashSet;
                    let final_params: HashSet<String> = m.parameters.iter()
                        .filter(|p| p.modifiers.iter().any(|mm| matches!(mm, crate::ast::Modifier::Final)))
                        .map(|p| p.name.clone())
                        .collect();
                    super::statements::review_body_locals_and_inits(body, &final_params)?;
                    // Enforce final field rules in method bodies
                    enforce_final_field_rules_in_block(body, &class.name, global)?;
                }
            }
        } else if let ClassMember::Constructor(c) = member {
            // Constructor visibility exclusivity and illegal flags
            ensure_visibility_exclusive(&c.modifiers, &c.name)?;
            use crate::ast::Modifier::*;
            if c.modifiers.contains(&Abstract)
                || c.modifiers.contains(&Static)
                || c.modifiers.contains(&Final)
                || c.modifiers.contains(&Synchronized)
                || c.modifiers.contains(&Native)
            {
                return Err(ReviewError::IllegalConstructorModifiers(c.name.clone()));
            }
            if c.parameters.iter().any(|p| p.varargs) {
                varargs_ctor_count += 1;
            }
        }
    }
    if varargs_ctor_count > 1 { return Err(ReviewError::MultipleVarargsConstructors); }
    Ok(())
}

fn build_method_tables_for_class(class: &ClassDecl) -> (
    std::collections::HashMap<String, Vec<usize>>,
    std::collections::HashMap<String, usize>,
    std::collections::HashMap<String, Vec<Vec<String>>>,
    std::collections::HashMap<String, Vec<usize>>, // ctor arities keyed by class name
    std::collections::HashMap<String, usize>,      // ctor varargs min keyed by class name
    std::collections::HashMap<String, Vec<Vec<String>>>, // ctor signatures keyed by class name
    std::collections::HashMap<String, bool>, // local methods static flags by name
) {
    use std::collections::HashMap;
    let mut map: HashMap<String, Vec<usize>> = HashMap::new();
    let mut varargs_min: HashMap<String, usize> = HashMap::new();
    let mut signatures: HashMap<String, Vec<Vec<String>>> = HashMap::new();
    let mut ctor_map: HashMap<String, Vec<usize>> = HashMap::new();
    let mut ctor_varargs_min: HashMap<String, usize> = HashMap::new();
    let mut ctor_signatures: HashMap<String, Vec<Vec<String>>> = HashMap::new();
    let mut methods_static: HashMap<String, bool> = HashMap::new();
    for member in &class.body {
        if let ClassMember::Method(m) = member {
            let arity = m.parameters.len();
            map.entry(m.name.clone()).or_default().push(arity);
            if m.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                varargs_min.entry(m.name.clone()).and_modify(|min| { *min = (*min).min(arity - 1); }).or_insert(arity - 1);
            }
            let sig: Vec<String> = m.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
            signatures.entry(m.name.clone()).or_default().push(sig);
            let is_static = m.modifiers.iter().any(|mm| matches!(mm, crate::ast::Modifier::Static));
            methods_static.entry(m.name.clone()).or_insert(is_static);
        } else if let ClassMember::Constructor(c) = member {
            let arity = c.parameters.len();
            ctor_map.entry(class.name.clone()).or_default().push(arity);
            if c.parameters.last().map(|p| p.varargs).unwrap_or(false) {
                ctor_varargs_min.entry(class.name.clone()).and_modify(|min| { *min = (*min).min(arity - 1); }).or_insert(arity - 1);
            }
            let sig: Vec<String> = c.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
            ctor_signatures.entry(class.name.clone()).or_default().push(sig);
        }
    }
    // de-dup arities
    for v in map.values_mut() { v.sort_unstable(); v.dedup(); }
    for v in ctor_map.values_mut() { v.sort_unstable(); v.dedup(); }
    (map, varargs_min, signatures, ctor_map, ctor_varargs_min, ctor_signatures, methods_static)
}

fn ensure_visibility_exclusive(mods: &[crate::ast::Modifier], member_name: &str) -> ReviewResult<()> {
    use crate::ast::Modifier::*;
    let public_set = mods.contains(&Public) as u8;
    let protected_set = mods.contains(&Protected) as u8;
    let private_set = mods.contains(&Private) as u8;
    if public_set + protected_set + private_set > 1 {
        return Err(ReviewError::DuplicateMember(format!(
            "conflicting visibility modifiers for '{}'",
            member_name
        )));
    }
    Ok(())
}

pub(crate) fn review_methods_of_interface(iface: &InterfaceDecl) -> ReviewResult<()> {
    use crate::ast::Modifier::*;
    use std::collections::HashSet;
    let mut seen: HashSet<MethodKey> = HashSet::new();
    for member in &iface.body {
        if let InterfaceMember::Method(m) = member {
            let key = MethodKey {
                name: m.name.clone(),
                param_types: m.parameters.iter().map(|p| p.type_ref.name.clone()).collect(),
            };
            if !seen.insert(key.clone()) {
                return Err(ReviewError::DuplicateType(format!(
                    "duplicate interface method '{}({})'",
                    key.name, key.param_types.len()
                )));
            }
            // interface methods should be public abstract by default; reject illegal combos (subset)
            if m.modifiers.contains(&Private) || m.modifiers.contains(&Protected) || m.modifiers.contains(&Final) {
                return Err(ReviewError::IllegalInterfaceMethodModifiers(m.name.clone()));
            }
            // Only check body when present (default methods/static methods may have bodies)
            if m.body.is_some() {
                super::statements::review_method_body_return(m)?;
            }
        }
    }
    Ok(())
}


fn enforce_final_field_rules_in_block(block: &Block, current_class: &str, global: &GlobalMemberIndex) -> ReviewResult<()> {
    for s in &block.statements { enforce_final_field_rules_in_stmt(s, current_class, global)?; }
    Ok(())
}

fn enforce_final_field_rules_in_stmt(stmt: &Stmt, current_class: &str, global: &GlobalMemberIndex) -> ReviewResult<()> {
    match stmt {
        Stmt::Expression(es) => enforce_final_field_rules_in_expr(&es.expr, current_class, global),
        Stmt::If(i) => { enforce_final_field_rules_in_stmt(&i.then_branch, current_class, global)?; if let Some(e) = &i.else_branch { enforce_final_field_rules_in_stmt(e, current_class, global)?; } Ok(()) }
        Stmt::While(w) => enforce_final_field_rules_in_stmt(&w.body, current_class, global),
        Stmt::For(f) => { for s in &f.init { enforce_final_field_rules_in_stmt(s, current_class, global)?; } enforce_final_field_rules_in_stmt(&f.body, current_class, global) }
        Stmt::Block(b) => enforce_final_field_rules_in_block(b, current_class, global),
        Stmt::Try(t) => { enforce_final_field_rules_in_block(&t.try_block, current_class, global)?; for cc in &t.catch_clauses { enforce_final_field_rules_in_block(&cc.block, current_class, global)?; } if let Some(fin) = &t.finally_block { enforce_final_field_rules_in_block(fin, current_class, global)?; } Ok(()) }
        _ => Ok(()),
    }
}

fn enforce_final_field_rules_in_expr(expr: &Expr, current_class: &str, global: &GlobalMemberIndex) -> ReviewResult<()> {
    use crate::ast::AssignmentOp::*;
    match expr {
        Expr::Assignment(a) => {
            // Disallow compound assignments to final fields and any assignment to final fields
            if let Expr::FieldAccess(fa) = &*a.target {
                let is_final = is_final_field_access(fa, current_class, global);
                if is_final {
                    return Err(super::ReviewError::AssignToFinalField(fa.name.clone()));
                }
            }
            // recurse
            enforce_final_field_rules_in_expr(&a.value, current_class, global)
        }
        Expr::Unary(u) => {
            use crate::ast::UnaryOp::*;
            match u.operator {
                PreInc | PreDec | PostInc | PostDec => {
                    if let Expr::FieldAccess(fa) = &*u.operand {
                        if is_final_field_access(fa, current_class, global) {
                            return Err(super::ReviewError::AssignToFinalField(fa.name.clone()));
                        }
                    }
                    enforce_final_field_rules_in_expr(&u.operand, current_class, global)
                }
                _ => enforce_final_field_rules_in_expr(&u.operand, current_class, global),
            }
        }
        Expr::Binary(b) => { enforce_final_field_rules_in_expr(&b.left, current_class, global)?; enforce_final_field_rules_in_expr(&b.right, current_class, global) }
        Expr::MethodCall(mc) => { if let Some(t) = &mc.target { enforce_final_field_rules_in_expr(t, current_class, global)?; } for a in &mc.arguments { enforce_final_field_rules_in_expr(a, current_class, global)?; } Ok(()) }
        Expr::ArrayAccess(acc) => { enforce_final_field_rules_in_expr(&acc.array, current_class, global)?; enforce_final_field_rules_in_expr(&acc.index, current_class, global) }
        Expr::FieldAccess(fa) => { if let Some(t) = &fa.target { enforce_final_field_rules_in_expr(t, current_class, global)?; } Ok(()) }
        Expr::Cast(c) => enforce_final_field_rules_in_expr(&c.expr, current_class, global),
        Expr::Conditional(c) => { enforce_final_field_rules_in_expr(&c.condition, current_class, global)?; enforce_final_field_rules_in_expr(&c.then_expr, current_class, global)?; enforce_final_field_rules_in_expr(&c.else_expr, current_class, global) }
        Expr::New(n) => { for a in &n.arguments { enforce_final_field_rules_in_expr(a, current_class, global)?; } Ok(()) }
        Expr::Parenthesized(p) => enforce_final_field_rules_in_expr(p, current_class, global),
        _ => Ok(()),
    }
}

fn is_final_field_access(fa: &FieldAccessExpr, current_class: &str, global: &GlobalMemberIndex) -> bool {
    // Qualified by a type name means static field; check final flag via global index if available
    if let Some(t) = &fa.target {
        if let Expr::Identifier(id) = &**t {
            if let Some(mt) = global.by_type.get(&id.name) {
                return mt.fields_final.get(&fa.name).copied().unwrap_or(false);
            }
        }
        // Otherwise, assume instance field of some expression, cannot resolve; only enforce when unqualified (this.field)
        return false;
    } else {
        // Unqualified: treat as this.field in current_class
        if let Some(mt) = global.by_type.get(current_class) {
            return mt.fields_final.get(&fa.name).copied().unwrap_or(false);
        }
        false
    }
}

