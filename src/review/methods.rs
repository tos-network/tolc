use super::{ReviewError, ReviewResult};
use super::types::GlobalMemberIndex;
use super::types::Visibility;
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
                    // Build enum index once per class (AST not passed here; build from class-local view by scanning outer Ast would require refactor)
                    super::statements::review_body_locals_and_inits(body, &final_params, Some(global))?;
                    // Enforce final field rules in method bodies
                    enforce_final_field_rules_in_block(body, &class.name, global)?;
                    // Checked exceptions: ensure throws are declared or caught (basic)
                    let declared: Vec<String> = m.throws.iter().map(|t| t.name.clone()).collect();
                    super::statements::review_body_checked_exceptions(class, body, &declared, global)?;
                }
            }
            // Override/visibility checks against direct superclass if known in index
            // Override consistency across full super chain and interfaces
            {
                let sig_here: Vec<String> = m.parameters.iter().map(|p| p.type_ref.name.clone()).collect();
                let mut queue: Vec<String> = Vec::new();
                if let Some(sup) = class.extends.as_ref().map(|t| t.name.clone()) { queue.push(sup); }
                // seed with direct interfaces
                for itf in &class.implements { queue.push(itf.name.clone()); }
                // BFS over type hierarchy
                let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
                while let Some(tname) = queue.pop() {
                    if !seen.insert(tname.clone()) { continue; }
                    if let Some(mt) = global.by_type.get(&tname) {
                        // enqueue next
                        if let Some(sup2) = &mt.super_name { queue.push(sup2.clone()); }
                        for it in &mt.interfaces { queue.push(it.clone()); }
                        // check method match
                        if let Some(metas) = mt.methods_meta.get(&m.name) {
                            for meta in metas {
                                if meta.signature == sig_here {
                                    let is_static_here = m.modifiers.iter().any(|mm| matches!(mm, crate::ast::Modifier::Static));
                                    // static/instance consistency
                                    if meta.is_static && !is_static_here {
                                        return Err(ReviewError::DuplicateMember(format!("illegal override: '{}' overrides static method in '{}'", m.name, tname)));
                                    }
                                    if !meta.is_static && is_static_here {
                                        return Err(ReviewError::DuplicateMember(format!("illegal static method hides instance method '{}' in '{}'", m.name, tname)));
                                    }
                                    if !meta.is_static && !is_static_here {
                                        // visibility: cannot reduce
                                        let vis_here = super::types::visibility_of(&m.modifiers);
                                        let super_is_interface = mt.is_interface;
                                        if super_is_interface {
                                            if vis_here != super::types::Visibility::Public {
                                                return Err(ReviewError::DuplicateMember(format!("method '{}' must be public to implement interface method", m.name)));
                                            }
                                        } else {
                                            let pkg_here = global.package.as_deref();
                                            let pkg_super = mt.package_name.as_deref();
                                            if reduces_visibility_pkg(vis_here, meta.visibility, pkg_here, pkg_super) {
                                                return Err(ReviewError::DuplicateMember(format!("override reduces visibility for method '{}'", m.name)));
                                            }
                                        }
                                        // final cannot be overridden
                                        if meta.is_final {
                                            return Err(ReviewError::DuplicateMember(format!("cannot override final method '{}' in '{}'", m.name, tname)));
                                        }
                                        // return type covariance (reference)
                                        if let (Some(ret_here), Some(ret_super)) = (m.return_type.as_ref().map(|t| t.name.clone()), meta.return_type.clone()) {
                                            if ret_here != ret_super {
                                                let prims = ["int","long","float","double","boolean","char","short","byte","void"];
                                                let is_prim = |s: &str| prims.contains(&s);
                                                if is_prim(&ret_here) || is_prim(&ret_super) {
                                                    return Err(ReviewError::DuplicateMember(format!("incompatible return type for override of '{}'", m.name)));
                                                }
                                                if !super::statements::is_reference_assignable(global, &ret_here, &ret_super) {
                                                    return Err(ReviewError::DuplicateMember(format!("incompatible return type for override of '{}'", m.name)));
                                                }
                                            }
                                        }
                                        // throws: cannot declare broader checked exceptions than super
                                        if let Some(list) = mt.methods_throws_by_sig.get(&m.name) {
                                            for (sup_sig, sup_thr) in list {
                                                if sup_sig == &sig_here {
                                                    let here_thr: Vec<String> = m.throws.iter().map(|t| t.name.clone()).collect();
                                                    for ht in &here_thr {
                                                        if is_unchecked_exception_name(global, ht) { continue; }
                                                        // require exists st in sup_thr such that ht is assignable to st
                                                        let mut ok = false;
                                                        for st in sup_thr {
                                                            if super::statements::is_reference_assignable(global, ht, st) { ok = true; break; }
                                                        }
                                                        if !ok {
                                                            return Err(ReviewError::DuplicateMember(format!("overriding method '{}' throws incompatible exception '{}'", m.name, ht)));
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
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
            // Final fields definite assignment across constructor paths (basic):
            // - Collect final fields without initializers
            // - Scan constructor body for single assignments to these fields
            // - Error if assigned 0 or >1 times
            enforce_constructor_final_field_rules(class, c, global)?;
        }
    }
    if varargs_ctor_count > 1 { return Err(ReviewError::MultipleVarargsConstructors); }
    Ok(())
}

fn reduces_visibility(here: Visibility, superv: Visibility) -> bool {
    use Visibility::*;
    let rank = |v: Visibility| match v { Private => 0, Package => 1, Protected => 2, Public => 3 };
    rank(here) < rank(superv)
}

fn reduces_visibility_pkg(here: Visibility, superv: Visibility, pkg_here: Option<&str>, pkg_super: Option<&str>) -> bool {
    use Visibility::*;
    if reduces_visibility(here, superv) { return true; }
    // Protected is less visible than package outside package, but not less visible than package within subclass
    match (here, superv) {
        // Package vs Protected across different packages is a reduction
        (Package, Protected) => {
            if let (Some(ph), Some(ps)) = (pkg_here, pkg_super) { ph != ps } else { true }
        }
        _ => false,
    }
}

fn is_unchecked_exception_name(_global: &GlobalMemberIndex, name: &str) -> bool {
    name == "RuntimeException" || name == "Error"
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

fn enforce_constructor_final_field_rules(class: &ClassDecl, ctor: &ConstructorDecl, _global: &GlobalMemberIndex) -> ReviewResult<()> {
    use crate::ast::Modifier::Final;
    // Gather final fields and note which have initializers
    let mut final_fields: std::collections::HashMap<String, bool> = std::collections::HashMap::new(); // name -> has_initializer
    for m in &class.body {
        if let ClassMember::Field(f) = m {
            if f.modifiers.iter().any(|mm| matches!(mm, Final)) {
                final_fields.insert(f.name.clone(), f.initializer.is_some());
            }
        }
    }
    if final_fields.is_empty() { return Ok(()); }
    // Path-sensitive assignment ranges for each field
    #[derive(Clone, Copy, Debug)]
    struct Range { min: u8, max: u8, has_normal: bool }
    impl Range {
        fn zero() -> Self { Range { min: 0, max: 0, has_normal: true } }
        fn none() -> Self { Range { min: 0, max: 0, has_normal: false } }
        fn one() -> Self { Range { min: 1, max: 1, has_normal: true } }
        fn add(self, other: Self) -> Self {
            if !self.has_normal { return self; }
            if !other.has_normal { return Range { min: 0, max: 0, has_normal: false }; }
            let clamp = |v: u8| if v > 2 { 2 } else { v };
            Range { min: clamp(self.min.saturating_add(other.min)), max: clamp(self.max.saturating_add(other.max)), has_normal: true }
        }
        fn min_max_merge(a: Self, b: Self) -> Self {
            let has = a.has_normal || b.has_normal;
            if !has { return Range::none(); }
            let min = match (a.has_normal, b.has_normal) { (true, true) => a.min.min(b.min), (true, false) => a.min, (false, true) => b.min, (false, false) => 0 };
            let max = match (a.has_normal, b.has_normal) { (true, true) => a.max.max(b.max), (true, false) => a.max, (false, true) => b.max, (false, false) => 0 };
            Range { min, max, has_normal: has }
        }
    }

    fn count_in_stmt(field: &str, stmt: &Stmt) -> Range {
        match stmt {
            Stmt::Expression(es) => count_in_expr(field, &es.expr),
            Stmt::If(i) => {
                let t = count_in_stmt(field, &i.then_branch);
                if let Some(e) = &i.else_branch {
                    let ee = count_in_stmt(field, e);
                    Range::min_max_merge(t, ee)
                } else {
                    // else missing -> branch may skip
                    Range::min_max_merge(t, Range::zero())
                }
            }
            Stmt::While(w) => {
                // Heuristics: if condition is false literal => zero
                if is_boolean_false(&w.condition) { return Range::zero(); }
                let has_top_break = body_has_top_level_break(&w.body);
                if has_top_break {
                    // At most once if body breaks on first iteration
                    let assigned_before_break = unconditional_assign_before_top_break(field, &w.body);
                    let min = if is_boolean_true(&w.condition) && assigned_before_break { 1 } else { 0 };
                    return Range { min, max: 1, has_normal: true };
                }
                let body = count_in_stmt(field, &w.body);
                let body_has = body.max > 0 && body.has_normal;
                if body_has { Range { min: 0, max: 2, has_normal: true } } else { Range::zero() }
            }
            Stmt::For(f) => {
                if let Some(cond) = &f.condition {
                    if is_boolean_false(cond) { return Range::zero(); }
                }
                let has_top_break = body_has_top_level_break(&f.body);
                if has_top_break {
                    let assigned_before_break = unconditional_assign_before_top_break(field, &f.body);
                    let cond_true = f.condition.is_none() || f.condition.as_ref().map(is_boolean_true).unwrap_or(false);
                    let min = if cond_true && assigned_before_break { 1 } else { 0 };
                    return Range { min, max: 1, has_normal: true };
                }
                let body = count_in_stmt(field, &f.body);
                let body_has = body.max > 0 && body.has_normal;
                if body_has { Range { min: 0, max: 2, has_normal: true } } else { Range::zero() }
            }
            Stmt::Block(b) => {
                let mut acc = Range::zero();
                for s in &b.statements { let r = count_in_stmt(field, s); acc = acc.add(r); if !acc.has_normal { break; } }
                acc
            }
            Stmt::Try(t) => {
                let tr = count_in_block(field, &t.try_block);
                let mut crs: Vec<Range> = Vec::new();
                for cc in &t.catch_clauses { crs.push(count_in_block(field, &cc.block)); }
                let fr = if let Some(fin) = &t.finally_block { count_in_block(field, fin) } else { Range::zero() };
                // union paths (try or any catch), then add finally
                let mut union = tr;
                for r in crs { union = Range::min_max_merge(union, r); }
                union.add(fr)
            }
            Stmt::Return(_) | Stmt::Throw(_) => Range::none(),
            Stmt::Switch(sw) => {
                // accumulate per case then union; default absent means path may skip all cases -> zero
                let mut union: Option<Range> = None;
                for c in &sw.cases { let r = count_in_block(field, &Block { statements: c.statements.clone(), span: c.span }); union = Some(if let Some(u) = union { Range::min_max_merge(u, r) } else { r }); }
                let has_default = sw.cases.iter().any(|c| c.labels.is_empty());
                if let Some(u) = union { if has_default { u } else { Range::min_max_merge(u, Range::zero()) } } else { Range::zero() }
            }
            _ => Range::zero(),
        }
    }
    fn count_in_block(field: &str, block: &Block) -> Range {
        let mut acc = Range::zero();
        for s in &block.statements { let r = count_in_stmt(field, s); acc = acc.add(r); if !acc.has_normal { break; } }
        acc
    }
    fn count_in_expr(field: &str, expr: &Expr) -> Range {
        match expr {
            Expr::Assignment(a) => {
                let mut self_assign = false;
                if let Expr::FieldAccess(fa) = &*a.target { if fa.target.is_none() && fa.name == field { self_assign = true; } }
                let rhs = count_in_expr(field, &a.value);
                if self_assign { Range::one().add(rhs) } else { rhs }
            }
            Expr::Unary(u) => count_in_expr(field, &u.operand),
            Expr::Binary(b) => count_in_expr(field, &b.left).add(count_in_expr(field, &b.right)),
            Expr::MethodCall(mc) => { let mut acc = Range::zero(); if let Some(t) = &mc.target { acc = acc.add(count_in_expr(field, t)); } for a in &mc.arguments { acc = acc.add(count_in_expr(field, a)); } acc }
            Expr::ArrayAccess(acc) => count_in_expr(field, &acc.array).add(count_in_expr(field, &acc.index)),
            Expr::FieldAccess(fa) => { if let Some(t) = &fa.target { count_in_expr(field, t) } else { Range::zero() } }
            Expr::Cast(c) => count_in_expr(field, &c.expr),
            Expr::Conditional(c) => { Range::min_max_merge(count_in_expr(field, &c.then_expr), count_in_expr(field, &c.else_expr)) },
            Expr::New(n) => { let mut acc = Range::zero(); for a in &n.arguments { acc = acc.add(count_in_expr(field, a)); } acc }
            Expr::Parenthesized(p) => count_in_expr(field, p),
            _ => Range::zero(),
        }
    }
    fn is_boolean_true(e: &Expr) -> bool { matches!(e, Expr::Literal(crate::ast::LiteralExpr{ value: crate::ast::Literal::Boolean(true), ..})) }
    fn is_boolean_false(e: &Expr) -> bool { matches!(e, Expr::Literal(crate::ast::LiteralExpr{ value: crate::ast::Literal::Boolean(false), ..})) }
    fn body_has_top_level_break(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Block(b) => b.statements.iter().any(|s| matches!(s, Stmt::Break(_))),
            _ => false,
        }
    }
    fn unconditional_assign_before_top_break(field: &str, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Block(b) => {
                let mut assigned = false;
                for s in &b.statements {
                    if matches!(s, Stmt::Break(_)) { return assigned; }
                    if stmt_unconditionally_assigns_field(field, s) { assigned = true; }
                }
                false
            }
            _ => false,
        }
    }
    fn stmt_unconditionally_assigns_field(field: &str, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expression(es) => expr_assigns_field(field, &es.expr),
            Stmt::If(i) => {
                if let Some(e) = &i.else_branch {
                    stmt_unconditionally_assigns_field(field, &i.then_branch) && stmt_unconditionally_assigns_field(field, e)
                } else { false }
            }
            Stmt::Block(b) => b.statements.iter().any(|s| stmt_unconditionally_assigns_field(field, s)),
            _ => false,
        }
    }
    fn expr_assigns_field(field: &str, expr: &Expr) -> bool {
        match expr {
            Expr::Assignment(a) => matches!(&*a.target, Expr::FieldAccess(fa) if fa.target.is_none() && fa.name == field),
            Expr::Parenthesized(p) => expr_assigns_field(field, p),
            Expr::Conditional(c) => expr_assigns_field(field, &c.then_expr) && expr_assigns_field(field, &c.else_expr),
            _ => false,
        }
    }

    for (name, has_init) in final_fields.into_iter() {
        let r = count_in_block(&name, &ctor.body);
        if has_init {
            if r.max > 0 { return Err(ReviewError::FinalFieldAssignedInConstructorWithInitializer(name)); }
        } else {
            if r.has_normal {
                if r.min == 0 { return Err(ReviewError::FinalFieldNotAssigned(name)); }
                if r.max > 1 { return Err(ReviewError::FinalFieldMultipleAssignment(name)); }
            }
        }
    }
    Ok(())
}

