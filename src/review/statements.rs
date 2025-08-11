use super::{ReviewError, ReviewResult};
use crate::ast::*;
use std::collections::{HashMap, HashSet};

// Minimal must-return checker for non-void methods (structural subset)
pub(crate) fn review_method_body_return(m: &MethodDecl) -> ReviewResult<()> {
    if m.return_type.is_none() { return Ok(()); } // void
    if let Some(body) = &m.body {
        if block_guarantees_return(body) { return Ok(()); }
        return Err(ReviewError::DuplicateType(format!(
            "non-void method '{}' may not return on all paths (basic check)",
            m.name
        )));
    }
    Err(ReviewError::DuplicateType(format!(
        "non-void method '{}' missing body",
        m.name
    )))
}

fn block_guarantees_return(block: &Block) -> bool {
    // A block guarantees return if any contained statement guarantees return
    for s in &block.statements {
        if stmt_guarantees_return(s) { return true; }
    }
    false
}

fn stmt_guarantees_return(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return(_) => true,
        Stmt::Block(b) => block_guarantees_return(b),
        Stmt::If(ifstmt) => {
            if let Some(else_b) = &ifstmt.else_branch {
                // require both branches to guarantee return
                stmt_guarantees_return(&ifstmt.then_branch) && stmt_guarantees_return(else_b)
            } else {
                false
            }
        }
        Stmt::Switch(sw) => switch_guarantees_return(sw),
        Stmt::While(w) => {
            // trivial terminal: while (true) { body guarantees return }
            if expr_is_boolean_true(&w.condition) {
                stmt_guarantees_return(&w.body)
            } else {
                false
            }
        }
        Stmt::For(f) => {
            // heuristic: for(;;) ... == while(true)
            if f.condition.is_none() { return stmt_guarantees_return(&f.body); }
            false
        }
        Stmt::Try(t) => {
            // try-finally guarantees return if finally guarantees return
            if let Some(fin) = &t.finally_block { return block_guarantees_return(fin); }
            false
        }
        // break/continue/switch default: conservatively false in this basic pass
        _ => false,
    }
}

fn switch_guarantees_return(sw: &SwitchStmt) -> bool {
    // basic rule: has default and every case's statements guarantee return
    let has_default = sw.cases.iter().any(|c| c.labels.is_empty());
    if !has_default { return false; }
    for c in &sw.cases {
        // consider the last statement in each case
        if let Some(last) = c.statements.last() {
            if !stmt_guarantees_return(last) { return false; }
        } else {
            return false;
        }
    }
    true
}

fn expr_is_boolean_true(e: &Expr) -> bool {
    if let Expr::Literal(l) = e {
        matches!(l.value, crate::ast::Literal::Boolean(true))
    } else { false }
}

// Arity checking (subset): Walk statements and check simple call arity vs declared methods in the same class
pub(crate) fn review_statement_arity(_current_type: &str, _stmt: &Stmt) -> ReviewResult<()> {
    // Placeholder: actual arity checks will need expression tree and a symbol table of methods
    Ok(())
}

// New: walk a method body and check unqualified method call arity/types against a per-class method table
pub(crate) fn review_body_call_arity(
    current_class_name: &str,
    body: &Block,
    arities: &HashMap<String, Vec<usize>>,
    signatures: &HashMap<String, Vec<Vec<String>>>,
    varargs_min: &HashMap<String, usize>,
    ctor_arities: &HashMap<String, Vec<usize>>,
    ctor_signatures: &HashMap<String, Vec<Vec<String>>>,
    ctor_varargs_min: &HashMap<String, usize>,
    // Optional global index for cross-type checks
    global: Option<&crate::review::types::GlobalMemberIndex>,
    // Optional local static flags by method name
    local_methods_static: Option<&HashMap<String, bool>>,
    errors_as_name: bool,
) -> ReviewResult<()> {
    for stmt in &body.statements {
        walk_stmt(current_class_name, stmt, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
    }
    Ok(())
}

// Local variable duplicate detection and simple initializer compatibility in a single pass
pub(crate) fn review_body_locals_and_inits(body: &Block, final_params: &HashSet<String>) -> ReviewResult<()> {
    let mut scope_stack: Vec<HashMap<String, bool>> = vec![HashMap::new()]; // bool: definitely assigned
    walk_stmt_locals(body, &mut scope_stack, final_params, false)
}

fn walk_stmt_locals(block: &Block, scopes: &mut Vec<HashMap<String, bool>>, final_params: &HashSet<String>, inside_loop: bool) -> ReviewResult<()> {
    scopes.push(HashMap::new());
    let mut terminated = false;
    for s in &block.statements {
        if terminated { break; }
        match s {
            Stmt::Declaration(vd) => {
                // Borrow current scope immutably for checks, then mutate after
                let cur_assigned = scopes.last().cloned().unwrap_or_default();
                for var in &vd.variables {
                    if cur_assigned.contains_key(&var.name) {
                        return Err(ReviewError::DuplicateLocalVar(var.name.clone()));
                    }
                    if let Some(scope) = scopes.last_mut() { scope.insert(var.name.clone(), false); }
                    if let Some(init) = &var.initializer {
                        // Check DA on initializer
                        check_expr_definite_assignment(init, scopes, final_params)?;
                        // Only check literal compatibility for now
                        if let Some(found) = infer_literal_primitive_or_string(init) {
                            let expected = vd.type_ref.name.as_str();
                            if !is_assignable_literal(expected, found) {
                                return Err(ReviewError::IncompatibleInitializer { expected: expected.to_string(), found: found.to_string() });
                            }
                        }
                        // mark as definitely assigned
                        if let Some(scope) = scopes.last_mut() { if let Some(flag) = scope.get_mut(&var.name) { *flag = true; } }
                    }
                }
                // If declaration is final, consider variables definitely assigned (single-assignment)
                let is_final_decl = vd.modifiers.iter().any(|m| matches!(m, Modifier::Final));
                if is_final_decl {
                    if let Some(scope) = scopes.last_mut() {
                        for var in &vd.variables {
                            if let Some(flag) = scope.get_mut(&var.name) { *flag = true; }
                        }
                    }
                }
            }
            Stmt::Block(b) => { walk_stmt_locals(b, scopes, final_params, inside_loop)?; }
            Stmt::If(ifstmt) => {
                // Merge DA across branches: assigned only if assigned in both branches
                let current_names: Vec<String> = scopes.last().unwrap().keys().cloned().collect();
                let mut then_scopes = scopes.clone();
                let mut else_scopes = scopes.clone();
                walk_stmt_locals(&Block { statements: vec![(*ifstmt.then_branch.clone())], span: ifstmt.span }, &mut then_scopes, final_params, inside_loop)?;
                if let Some(else_b) = &ifstmt.else_branch {
                    walk_stmt_locals(&Block { statements: vec![(**else_b).clone()], span: ifstmt.span }, &mut else_scopes, final_params, inside_loop)?;
                }
                let then_top = then_scopes.last().unwrap();
                let else_top = else_scopes.last().unwrap();
                let cur_top = scopes.last_mut().unwrap();
                for name in current_names {
                    let t = then_top.get(&name).copied().unwrap_or(false);
                    let e = if ifstmt.else_branch.is_some() { else_top.get(&name).copied().unwrap_or(false) } else { false };
                    if t && e {
                        cur_top.insert(name, true);
                    }
                }
            }
            Stmt::While(w) => {
                if expr_is_boolean_true(&w.condition) {
                    // Prefer precise merge: names definitely assigned before a top-level break in first iteration
                    if let Some(names) = collect_unconditional_assigns_until_break(&w.body) {
                        for name in names {
                            if let Some(scope) = scopes.iter_mut().rev().find(|s| s.contains_key(&name)) {
                                scope.insert(name, true);
                            }
                        }
                    } else {
                        // Fallback to conservative propagation via body analysis
                        let mut then_scopes = scopes.clone();
                        walk_stmt_locals(&Block { statements: vec![(*w.body.clone())], span: w.span }, &mut then_scopes, final_params, true)?;
                        let then_top = then_scopes.last().unwrap();
                        let cur_top = scopes.last_mut().unwrap();
                        for (name, assigned) in then_top.iter() { if *assigned { cur_top.insert(name.clone(), true); } }
                    }
                } else {
                    // Non-constant loop condition: be conservative.
                    // Only propagate assignments that occur unconditionally before a top-level break in the first iteration.
                    if let Some(names) = collect_unconditional_assigns_until_break(&w.body) {
                        for name in names {
                            if let Some(scope) = scopes.iter_mut().rev().find(|s| s.contains_key(&name)) {
                                scope.insert(name, true);
                            }
                        }
                    }
                }
            }
            Stmt::For(f) => {
                if f.condition.is_none() {
                    let mut then_scopes = scopes.clone();
                    walk_stmt_locals(&Block { statements: vec![(*f.body.clone())], span: f.span }, &mut then_scopes, final_params, true)?;
                    let then_top = then_scopes.last().unwrap();
                    let cur_top = scopes.last_mut().unwrap();
                    for (name, assigned) in then_top.iter() {
                        if *assigned { cur_top.insert(name.clone(), true); }
                    }
                } else {
                    if let Some(names) = collect_unconditional_assigns_until_break(&f.body) {
                        for name in names {
                            if let Some(scope) = scopes.iter_mut().rev().find(|s| s.contains_key(&name)) {
                                scope.insert(name, true);
                            }
                        }
                    }
                }
            }
            Stmt::Try(t) => {
                // Compute DA across try/catch/finally
                let names: Vec<String> = scopes.last().unwrap().keys().cloned().collect();
                // try
                let mut try_scopes = scopes.clone();
                walk_stmt_locals(&Block { statements: t.try_block.statements.clone(), span: t.try_block.span }, &mut try_scopes, final_params, inside_loop)?;
                let try_top = try_scopes.last().unwrap();
                // catches
                let mut catches_tops: Vec<std::collections::HashMap<String, bool>> = Vec::new();
                for cc in &t.catch_clauses {
                    let mut c_scopes = scopes.clone();
                    walk_stmt_locals(&Block { statements: cc.block.statements.clone(), span: cc.block.span }, &mut c_scopes, final_params, inside_loop)?;
                    catches_tops.push(c_scopes.last().unwrap().clone());
                }
                // finally
                let finally_top_opt = if let Some(fin) = &t.finally_block {
                    let mut f_scopes = scopes.clone();
                    walk_stmt_locals(&Block { statements: fin.statements.clone(), span: fin.span }, &mut f_scopes, final_params, inside_loop)?;
                    Some(f_scopes.last().unwrap().clone())
                } else { None };
                // Merge rule (approx JVMS/JLS): DA_out = (DA_try) OR (intersection over all catches) OR (DA_finally)
                let cur_top = scopes.last_mut().unwrap();
                for n in names {
                    let da_try = try_top.get(&n).copied().unwrap_or(false);
                    let da_all_catches = if catches_tops.is_empty() { false } else { catches_tops.iter().all(|m| m.get(&n).copied().unwrap_or(false)) };
                    let da_finally = finally_top_opt.as_ref().map(|m| m.get(&n).copied().unwrap_or(false)).unwrap_or(false);
                    if da_try || da_all_catches || da_finally {
                        cur_top.insert(n, true);
                    }
                }
            }
            Stmt::Break(b) => { if inside_loop || b.label.is_some() { terminated = true; } }
            Stmt::Continue(b) => { if inside_loop || b.label.is_some() { terminated = true; } }
            Stmt::Switch(sw) => {
                // Improved merge: model fall-through by concatenating statements from each starting case to the end
                let names: Vec<String> = scopes.last().unwrap().keys().cloned().collect();
                let has_default = sw.cases.iter().any(|c| c.labels.is_empty());
                let mut exit_maps: Vec<std::collections::HashMap<String, bool>> = Vec::new();
                for start in 0..sw.cases.len() {
                    // Each labeled case start is a possible entry when its label matches
                    if sw.cases[start].labels.is_empty() {
                        // default is handled as a normal start too
                    }
                    let mut stmts: Vec<Stmt> = Vec::new();
                    for c in &sw.cases[start..] { stmts.extend(c.statements.clone()); }
                    let block = Block { statements: stmts, span: sw.span };
                    let mut c_scopes = scopes.clone();
                    walk_stmt_locals(&block, &mut c_scopes, final_params, inside_loop)?;
                    exit_maps.push(c_scopes.last().unwrap().clone());
                }
                // If no default, include the path that executes no case at all (falls through to after switch)
                if !has_default {
                    exit_maps.push(scopes.last().unwrap().clone());
                }
                let cur_top = scopes.last_mut().unwrap();
                for n in names {
                    if exit_maps.iter().all(|m| m.get(&n).copied().unwrap_or(false)) {
                        cur_top.insert(n, true);
                    }
                }
            }
            Stmt::Labeled(ls) => {
                // Evaluate labeled statement in a cloned scope but compare at the same scope depth
                let base_index = scopes.len() - 1;
                let names: Vec<String> = scopes[base_index].keys().cloned().collect();
                let mut tmp_scopes = scopes.clone();
                walk_stmt_locals(&Block { statements: vec![(*ls.statement.clone())], span: ls.span }, &mut tmp_scopes, final_params, inside_loop)?;
                let nested_same_depth = &tmp_scopes[base_index];
                let cur_top = scopes.last_mut().unwrap();
                for n in names {
                    if nested_same_depth.get(&n).copied().unwrap_or(false) { cur_top.insert(n, true); }
                }
            }
            Stmt::Expression(es) => check_expr_definite_assignment(&es.expr, scopes, final_params)?,
            Stmt::Return(ret) => { if let Some(e) = &ret.value { check_expr_definite_assignment(e, scopes, final_params)?; } terminated = true; },
            _ => {}
        }
    }
    scopes.pop();
    Ok(())
}

// Collect names that are definitely assigned on all paths before encountering a top-level break
// within a single pass over the immediate statements of the loop body. Nested blocks/ifs are
// handled conservatively: an assignment must occur on all branches before a break to qualify.
fn collect_unconditional_assigns_until_break(body: &Stmt) -> Option<Vec<String>> {
    use std::collections::HashSet;
    fn walker(stmt: &Stmt) -> (HashSet<String>, bool) {
        // returns (assigned_names, has_top_level_break)
        match stmt {
            Stmt::Break(_) => (HashSet::new(), true),
            Stmt::Expression(es) => {
                if let Expr::Assignment(a) = &es.expr {
                    if let Expr::Identifier(id) = &*a.target {
                        let mut s = HashSet::new();
                        s.insert(id.name.clone());
                        return (s, false);
                    }
                }
                (HashSet::new(), false)
            }
            Stmt::If(ifstmt) => {
                let (then_set, then_break) = walker(&ifstmt.then_branch);
                let (else_set, else_break) = if let Some(else_b) = &ifstmt.else_branch { walker(else_b) } else { (HashSet::new(), false) };
                let inter: HashSet<String> = then_set.intersection(&else_set).cloned().collect();
                (inter, then_break && else_break)
            }
            Stmt::Block(b) => {
                let mut acc: HashSet<String> = HashSet::new();
                let mut seen_break = false;
                for s in &b.statements {
                    let (set, has_break) = walker(s);
                    // accumulate only if we haven't hit a break yet; after break, subsequent statements are unreachable
                    if !seen_break {
                        for n in set { acc.insert(n); }
                    }
                    if has_break { seen_break = true; }
                }
                (acc, seen_break)
            }
            // continue does not contribute; reaching it means no break on that path
            Stmt::Continue(_) => (HashSet::new(), false),
            _ => (HashSet::new(), false),
        }
    }
    if let Stmt::Block(b) = body {
        let (set, has_break) = walker(&Stmt::Block(b.clone()));
        if has_break { return Some(set.into_iter().collect()); }
        return None;
    }
    None
}

fn lookup_assigned(scopes: &Vec<HashMap<String, bool>>, name: &str) -> Option<bool> {
    for scope in scopes.iter().rev() {
        if let Some(v) = scope.get(name) { return Some(*v); }
    }
    None
}

pub(crate) fn resolve_type_in_index<'a>(global: &'a crate::review::types::GlobalMemberIndex, name: &str) -> Option<&'a crate::review::types::MemberTables> {
    // Shadowing precedence: local type (by simple name) > explicit imports > wildcard imports
    // Try simple (local declared types are recorded by simple key)
    if let Some(mt) = global.by_type.get(name) { return Some(mt); }

    // Try explicit imports of types: match simple name against last segment
    for imp in &global.imports {
        if let Some(last) = imp.rsplit('.').next() { if last == name { if let Some(mt) = global.by_type.get(imp) { return Some(mt); } } }
    }

    // Try wildcard imports
    for wi in &global.wildcard_imports {
        let fq = format!("{}.{name}", wi);
        if let Some(mt) = global.by_type.get(&fq) { return Some(mt); }
    }

    // Fallback: fully-qualified name provided
    if let Some(mt) = global.by_type.get(name) { return Some(mt); }
    None
}

/// Minimal reference-type assignability via superclass chain.
/// Returns true if `src` is the same type as `dst`, or if any superclass of `src` (following `super_name`) matches `dst`.
/// Treats `Object` as a universal super type.
pub(crate) fn is_reference_assignable(global: &crate::review::types::GlobalMemberIndex, src: &str, dst: &str) -> bool {
    if dst == "Object" { return true; }
    if src == dst { return true; }
    // Walk up from src following super_name
    let mut cur_name = src.to_string();
    let mut seen = std::collections::HashSet::new();
    loop {
        if let Some(mt) = resolve_type_in_index(global, &cur_name) {
            if let Some(sup) = &mt.super_name {
                if sup == dst { return true; }
                if !seen.insert(sup.clone()) { return false; }
                cur_name = sup.clone();
                continue;
            }
        }
        return false;
    }
}

// Returns Some(arity) if the type is known locally or explicitly imported; unknown types via wildcard imports return None
pub(crate) fn lookup_type_param_count(global: &crate::review::types::GlobalMemberIndex, name: &str) -> Option<usize> {
    if let Some(mt) = global.by_type.get(name) { return Some(mt.type_param_count); }
    // explicit imports: match simple name against last segment
    for imp in &global.imports {
        if let Some(last) = imp.rsplit('.').next() {
            if last == name {
                // external type arity unknown; assume zero per simple generic arity rule
                return Some(0);
            }
        }
    }
    // fully qualified in use
    if name.contains('.') {
        // If the fully qualified exists in index use it; otherwise assume zero
        if let Some(mt) = global.by_type.get(name) { return Some(mt.type_param_count); }
        return Some(0);
    }
    None
}

fn check_expr_definite_assignment(expr: &Expr, scopes: &mut Vec<HashMap<String, bool>>, final_params: &HashSet<String>) -> ReviewResult<()> {
    match expr {
        Expr::Identifier(id) => {
            match lookup_assigned(scopes, &id.name) {
                Some(true) => Ok(()),
                Some(false) => Err(ReviewError::UseBeforeInit(id.name.clone())),
                None => Ok(()),
            }
        }
        Expr::Assignment(a) => {
            // For simple x = expr; mark x as assigned after checking rhs
            check_expr_definite_assignment(&a.value, scopes, final_params)?;
            if let Expr::Identifier(id) = &*a.target {
                // final parameter cannot be assigned
                if final_params.contains(&id.name) {
                    return Err(ReviewError::AssignToFinal(id.name.clone()));
                }
                // For fields/instances final enforcement is handled at class review; here we only handle locals/params
                if let Some(scope) = scopes.iter_mut().rev().find(|s| s.contains_key(&id.name)) {
                    scope.insert(id.name.clone(), true);
                }
            }
            Ok(())
        }
        Expr::Binary(b) => { check_expr_definite_assignment(&b.left, scopes, final_params)?; check_expr_definite_assignment(&b.right, scopes, final_params) }
        Expr::Unary(u) => check_expr_definite_assignment(&u.operand, scopes, final_params),
        Expr::MethodCall(mc) => { for a in &mc.arguments { check_expr_definite_assignment(a, scopes, final_params)?; } Ok(()) }
        Expr::ArrayAccess(acc) => { check_expr_definite_assignment(&acc.array, scopes, final_params)?; check_expr_definite_assignment(&acc.index, scopes, final_params) }
        Expr::FieldAccess(fa) => { if let Some(t) = &fa.target { check_expr_definite_assignment(t, scopes, final_params)?; } Ok(()) }
        Expr::Cast(c) => check_expr_definite_assignment(&c.expr, scopes, final_params),
        Expr::Conditional(c) => { check_expr_definite_assignment(&c.condition, scopes, final_params)?; check_expr_definite_assignment(&c.then_expr, scopes, final_params)?; check_expr_definite_assignment(&c.else_expr, scopes, final_params) }
        Expr::New(n) => { for a in &n.arguments { check_expr_definite_assignment(a, scopes, final_params)?; } Ok(()) }
        Expr::Parenthesized(p) => check_expr_definite_assignment(p, scopes, final_params),
        _ => Ok(()),
    }
}

fn walk_stmt(
    current_class_name: &str,
    stmt: &Stmt,
    arities: &HashMap<String, Vec<usize>>,
    signatures: &HashMap<String, Vec<Vec<String>>>,
    varargs_min: &HashMap<String, usize>,
    ctor_arities: &HashMap<String, Vec<usize>>,
    ctor_signatures: &HashMap<String, Vec<Vec<String>>>,
    ctor_varargs_min: &HashMap<String, usize>,
    global: Option<&crate::review::types::GlobalMemberIndex>,
    local_methods_static: Option<&HashMap<String, bool>>,
    errors_as_name: bool,
) -> ReviewResult<()> {
    match stmt {
        Stmt::Declaration(vd) => {
            for var in &vd.variables {
                if let Some(init) = &var.initializer {
                    walk_expr(current_class_name, init, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
                }
            }
            Ok(())
        }
        Stmt::Expression(es) => walk_expr(current_class_name, &es.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name),
        Stmt::Return(ret) => {
            if let Some(e) = &ret.value { walk_expr(current_class_name, e, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            Ok(())
        }
        Stmt::If(ifstmt) => {
            walk_expr(current_class_name, &ifstmt.condition, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
            walk_stmt(current_class_name, &ifstmt.then_branch, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
            if let Some(else_b) = &ifstmt.else_branch { walk_stmt(current_class_name, else_b, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            Ok(())
        }
        Stmt::While(w) => {
            walk_expr(current_class_name, &w.condition, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
            walk_stmt(current_class_name, &w.body, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)
        }
        Stmt::For(f) => {
            for init in &f.init { walk_stmt(current_class_name, init, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            if let Some(cond) = &f.condition { walk_expr(current_class_name, cond, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            for upd in &f.update { walk_expr(current_class_name, &upd.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            walk_stmt(current_class_name, &f.body, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)
        }
        Stmt::Block(b) => {
            for s in &b.statements { walk_stmt(current_class_name, s, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            Ok(())
        }
        Stmt::Switch(sw) => {
            walk_expr(current_class_name, &sw.expression, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
            for c in &sw.cases {
                for s in &c.statements { walk_stmt(current_class_name, s, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            }
            Ok(())
        }
        Stmt::Try(t) => {
            for r in &t.resources { match r { TryResource::Var { initializer, .. } => walk_expr(current_class_name, initializer, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?, TryResource::Expr { expr, .. } => walk_expr(current_class_name, expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?, } }
            walk_stmt(current_class_name, &Stmt::Block(t.try_block.clone()), arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
            for cc in &t.catch_clauses { walk_stmt(current_class_name, &Stmt::Block(cc.block.clone()), arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            if let Some(fin) = &t.finally_block { walk_stmt(current_class_name, &Stmt::Block(fin.clone()), arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn walk_expr(
    current_class_name: &str,
    expr: &Expr,
    arities: &HashMap<String, Vec<usize>>,
    signatures: &HashMap<String, Vec<Vec<String>>>,
    varargs_min: &HashMap<String, usize>,
    ctor_arities: &HashMap<String, Vec<usize>>,
    ctor_signatures: &HashMap<String, Vec<Vec<String>>>,
    ctor_varargs_min: &HashMap<String, usize>,
    global: Option<&crate::review::types::GlobalMemberIndex>,
    local_methods_static: Option<&HashMap<String, bool>>,
    errors_as_name: bool,
) -> ReviewResult<()> {
    match expr {
        Expr::Identifier(id) => {
            // Static imported member usage: ensure the imported member is static
            if let Some(g) = global {
                // Also check generic arity in type usage contexts like casts and instanceof are handled elsewhere; identifiers here can be fields or local vars
                if let Some(ty) = g.static_explicit.get(&id.name) {
                    if let Some(mt) = g.by_type.get(ty) {
                        // fields
                        if let Some(is_static) = mt.fields_static.get(&id.name) { if !*is_static { return Err(ReviewError::IllegalStaticCall { typename: ty.clone(), name: id.name.clone() }); } }
                        // methods (unqualified call without args looks like identifier; we do method check at call site)
                    }
                } else {
                    for ty in &g.static_wildcard {
                        if let Some(mt) = g.by_type.get(ty) {
                            if let Some(is_static) = mt.fields_static.get(&id.name) {
                                if !*is_static { return Err(ReviewError::IllegalStaticCall { typename: ty.clone(), name: id.name.clone() }); }
                                break;
                            }
                        }
                    }
                }
            }
            Ok(())
        }
        Expr::MethodCall(mc) => {
            // Check unqualified calls and qualified self-calls like ClassName.m(...)
            let is_unqualified = mc.target.is_none();
            let is_qualified_self = match &mc.target {
                Some(t) => match &**t {
                    Expr::Identifier(id) => id.name == current_class_name,
                    _ => false,
                },
                None => false,
            };
            if is_unqualified || is_qualified_self {
                if is_qualified_self {
                    // self qualified call must be static in this approximation
                    if let Some(map) = local_methods_static {
                        if let Some(is_static) = map.get(&mc.name) {
                            if !*is_static {
                                return Err(ReviewError::IllegalStaticCall { typename: current_class_name.to_string(), name: mc.name.clone() });
                            }
                        }
                    }
                }
                let found_arity = mc.arguments.len();
                if let Some(expected_list) = arities.get(&mc.name) {
                    let matches_fixed = expected_list.iter().any(|a| *a == found_arity);
                    let matches_varargs = varargs_min.get(&mc.name).map(|min| found_arity >= *min).unwrap_or(false);
                    if !matches_fixed && !matches_varargs {
                        let mut expected = expected_list.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                        if let Some(min) = varargs_min.get(&mc.name) { expected.push(format!("{}+", min)); }
                        let expected = expected.join(", ");
                        return Err(ReviewError::MethodCallArityMismatch { name: mc.name.clone(), expected, found: found_arity });
                    }
                    // If we can infer literal types for all args and have signatures, do a simple applicability check
                    if let Some(cands) = signatures.get(&mc.name) {
                        let found_types: Vec<Option<&'static str>> = mc.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                        if found_types.iter().all(|o| o.is_some()) {
                            let found_list: Vec<&str> = found_types.iter().map(|o| o.unwrap()).collect();
                            let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                            for sig in cands {
                                if sig.len() == found_list.len() {
                                    let mut ok = true;
                                    let mut cost: u32 = 0;
                                    let mut convs: u32 = 0;
                                    for (exp, f) in sig.iter().zip(found_list.iter()) {
                                        if !is_assignable_primitive_or_string(exp.as_str(), f) { ok = false; break; }
                                        if exp.as_str() != *f { convs += 1; }
                                        cost += widening_cost(exp.as_str(), f);
                                    }
                                    if ok { applicable.push((sig, cost, convs)); }
                                }
                            }
                            if applicable.is_empty() && !cands.is_empty() {
                                let expected = cands.iter().filter(|s| s.len()==found_list.len()).map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                let found = found_list.join(",");
                                return Err(ReviewError::InapplicableMethod { name: mc.name.clone(), expected, found });
                            } else if applicable.len() > 1 {
                                // prefer exact matches first
                                let mut exact: Vec<(&Vec<String>, u32, u32)> = applicable.iter().cloned().filter(|(_, _, k)| *k == 0).collect();
                                if exact.len() > 1 {
                                    let candidates = exact.iter().map(|(s, _, _)| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                    let found = found_list.join(",");
                                    return Err(ReviewError::AmbiguousMethod { name: mc.name.clone(), candidates, found });
                                } else if exact.len() == 1 {
                                    // single exact wins
                                } else {
                                    // prefer fewer conversions, then minimal cost
                                    let min_convs = applicable.iter().map(|(_, _, k)| *k).min().unwrap();
                                    applicable.retain(|(_, _, k)| *k == min_convs);
                                    let min_cost = applicable.iter().map(|(_, c, _)| *c).min().unwrap();
                                    let tied: Vec<&Vec<String>> = applicable.iter().filter(|(_, c, _)| *c == min_cost).map(|(s, _, _)| *s).collect();
                                    if tied.len() > 1 {
                                        let candidates = tied.iter().map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                        let found = found_list.join(",");
                                        return Err(ReviewError::AmbiguousMethod { name: mc.name.clone(), candidates, found });
                                    }
                                }
                            }
                        }
                    }
                }
                // Fallback: unqualified static-imported methods
                if is_unqualified {
                    if let Some(g) = global {
                        // explicit static import: member -> type
                        if let Some(ty) = g.static_explicit.get(&mc.name) {
                            if let Some(mt) = g.by_type.get(ty) {
                                if let Some(is_static) = mt.methods_static.get(&mc.name) {
                                    if !*is_static {
                                        return Err(ReviewError::IllegalStaticCall { typename: ty.clone(), name: mc.name.clone() });
                                    }
                                }
                                let found_arity = mc.arguments.len();
                                if let Some(expected_list) = mt.methods_arities.get(&mc.name) {
                                    let matches_fixed = expected_list.iter().any(|a| *a == found_arity);
                                    let matches_varargs = mt.methods_varargs_min.get(&mc.name).map(|min| found_arity >= *min).unwrap_or(false);
                                    if !matches_fixed && !matches_varargs {
                                        let mut expected = expected_list.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                                        if let Some(min) = mt.methods_varargs_min.get(&mc.name) { expected.push(format!("{}+", min)); }
                                        let expected = expected.join(", ");
                                        return Err(ReviewError::MethodCallArityMismatch { name: format!("{}::{}", ty, mc.name), expected, found: found_arity });
                                    }
                                }
                            }
                        } else {
                            // wildcard providers: pick first type offering the method
                            for ty in &g.static_wildcard {
                                if let Some(mt) = g.by_type.get(ty) {
                                    if mt.methods_arities.get(&mc.name).is_some() {
                                        if let Some(is_static) = mt.methods_static.get(&mc.name) {
                                            if !*is_static {
                                                return Err(ReviewError::IllegalStaticCall { typename: ty.clone(), name: mc.name.clone() });
                                            }
                                        }
                                        let found_arity = mc.arguments.len();
                                        if let Some(expected_list) = mt.methods_arities.get(&mc.name) {
                                            let matches_fixed = expected_list.iter().any(|a| *a == found_arity);
                                            let matches_varargs = mt.methods_varargs_min.get(&mc.name).map(|min| found_arity >= *min).unwrap_or(false);
                                            if !matches_fixed && !matches_varargs {
                                                let mut expected = expected_list.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                                                if let Some(min) = mt.methods_varargs_min.get(&mc.name) { expected.push(format!("{}+", min)); }
                                                let expected = expected.join(", ");
                                                return Err(ReviewError::MethodCallArityMismatch { name: format!("{}::{}", ty, mc.name), expected, found: found_arity });
                                            }
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Cross-type static call: TypeName.m(...)
            if let Some(t) = &mc.target {
                if let Expr::Identifier(id) = &**t {
                    if let Some(g) = global {
                        if let Some(mt) = resolve_type_in_index(g, &id.name) {
                            // static-only check (approximation): if recorded as non-static, reject qualified static call
                            if let Some(is_static) = mt.methods_static.get(&mc.name) {
                                if !*is_static {
                                    return Err(ReviewError::IllegalStaticCall { typename: id.name.clone(), name: mc.name.clone() });
                                }
                            }
                            let found_arity = mc.arguments.len();
                            if let Some(expected_list) = mt.methods_arities.get(&mc.name) {
                                let matches_fixed = expected_list.iter().any(|a| *a == found_arity);
                                let matches_varargs = mt.methods_varargs_min.get(&mc.name).map(|min| found_arity >= *min).unwrap_or(false);
                                if !matches_fixed && !matches_varargs {
                                    let mut expected = expected_list.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                                    if let Some(min) = mt.methods_varargs_min.get(&mc.name) { expected.push(format!("{}+", min)); }
                                    let expected = expected.join(", ");
                                    return Err(ReviewError::MethodCallArityMismatch { name: format!("{}::{}", id.name, mc.name), expected, found: found_arity });
                                }
                                if let Some(cands) = mt.methods_signatures.get(&mc.name) {
                                    let found_types: Vec<Option<&'static str>> = mc.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                                    if found_types.iter().all(|o| o.is_some()) {
                                        let found_list: Vec<&str> = found_types.iter().map(|o| o.unwrap()).collect();
                                        let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                                        for sig in cands {
                                            if sig.len() == found_list.len() {
                                                let mut ok = true;
                                                let mut cost: u32 = 0;
                                                let mut convs: u32 = 0;
                                                for (exp, f) in sig.iter().zip(found_list.iter()) {
                                                    if !is_assignable_primitive_or_string(exp.as_str(), f) { ok = false; break; }
                                                    if exp.as_str() != *f { convs += 1; }
                                                    cost += widening_cost(exp.as_str(), f);
                                                }
                                                if ok { applicable.push((sig, cost, convs)); }
                                            }
                                        }
                                        if applicable.is_empty() && !cands.is_empty() {
                                            let expected = cands.iter().filter(|s| s.len()==found_list.len()).map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                            let found = found_list.join(",");
                                            return Err(ReviewError::InapplicableMethod { name: format!("{}::{}", id.name, mc.name), expected, found });
                                        } else if applicable.len() > 1 {
                                            let mut exact: Vec<(&Vec<String>, u32, u32)> = applicable.iter().cloned().filter(|(_, _, k)| *k == 0).collect();
                                            if exact.len() > 1 {
                                                let candidates = exact.iter().map(|(s, _, _)| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                                let found = found_list.join(",");
                                                return Err(ReviewError::AmbiguousMethod { name: format!("{}::{}", id.name, mc.name), candidates, found });
                                            } else if exact.len() == 1 {
                                            } else {
                                                let min_convs = applicable.iter().map(|(_, _, k)| *k).min().unwrap();
                                                applicable.retain(|(_, _, k)| *k == min_convs);
                                                let min_cost = applicable.iter().map(|(_, c, _)| *c).min().unwrap();
                                                let tied: Vec<&Vec<String>> = applicable.iter().filter(|(_, c, _)| *c == min_cost).map(|(s, _, _)| *s).collect();
                                                if tied.len() > 1 {
                                                    let candidates = tied.iter().map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                                    let found = found_list.join(",");
                                                    return Err(ReviewError::AmbiguousMethod { name: format!("{}::{}", id.name, mc.name), candidates, found });
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
            // walk target and args
            if let Some(t) = &mc.target { walk_expr(current_class_name, t, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            for a in &mc.arguments { walk_expr(current_class_name, a, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            Ok(())
        }
        Expr::Binary(b) => { walk_expr(current_class_name, &b.left, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; walk_expr(current_class_name, &b.right, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name) }
        Expr::Unary(u) => walk_expr(current_class_name, &u.operand, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name),
        Expr::Assignment(a) => { walk_expr(current_class_name, &a.target, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; walk_expr(current_class_name, &a.value, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name) }
        Expr::Cast(c) => {
            // Generic arity check on cast target type
            if let Some(g) = global {
                if let Some(mt) = resolve_type_in_index(g, &c.target_type.name) {
                    if c.target_type.type_args.len() != mt.type_param_count {
                        return Err(ReviewError::GenericArityMismatch { typename: c.target_type.name.clone(), expected: mt.type_param_count, found: c.target_type.type_args.len() });
                    }
                    if !mt.type_param_bounds.is_empty() {
                        for (i, targ) in c.target_type.type_args.iter().enumerate() {
                            if let Some(bounds) = mt.type_param_bounds.get(i) {
                                for b in bounds {
                                    if !is_reference_assignable(g, &targ.name, b) {
                                        return Err(ReviewError::GenericBoundViolation { typename: c.target_type.name.clone(), bound: b.clone(), found: targ.name.clone() });
                                    }
                                }
                            }
                        }
                    }
                }
            }
            walk_expr(current_class_name, &c.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)
        },
        Expr::InstanceOf(io) => {
            if let Some(g) = global {
                if let Some(mt) = resolve_type_in_index(g, &io.target_type.name) {
                    if io.target_type.type_args.len() != mt.type_param_count {
                        return Err(ReviewError::GenericArityMismatch { typename: io.target_type.name.clone(), expected: mt.type_param_count, found: io.target_type.type_args.len() });
                    }
                    if !mt.type_param_bounds.is_empty() {
                        for (i, targ) in io.target_type.type_args.iter().enumerate() {
                            if let Some(bounds) = mt.type_param_bounds.get(i) {
                                for b in bounds {
                                    if !is_reference_assignable(g, &targ.name, b) {
                                        return Err(ReviewError::GenericBoundViolation { typename: io.target_type.name.clone(), bound: b.clone(), found: targ.name.clone() });
                                    }
                                }
                            }
                        }
                    }
                }
            }
            walk_expr(current_class_name, &io.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)
        }
        Expr::Conditional(c) => { walk_expr(current_class_name, &c.condition, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; walk_expr(current_class_name, &c.then_expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; walk_expr(current_class_name, &c.else_expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name) }
        Expr::ArrayAccess(acc) => { walk_expr(current_class_name, &acc.array, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; walk_expr(current_class_name, &acc.index, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name) }
        Expr::FieldAccess(fa) => {
            // Qualified static field: TypeName.field
            if let Some(t) = &fa.target {
                if let Expr::Identifier(id) = &**t {
                    if let Some(g) = global {
                        if let Some(mt) = resolve_type_in_index(g, &id.name) {
                            if let Some(is_static) = mt.fields_static.get(&fa.name) {
                                if !*is_static {
                                    return Err(ReviewError::IllegalStaticCall { typename: id.name.clone(), name: fa.name.clone() });
                                }
                            }
                        }
                    }
                }
                walk_expr(current_class_name, t, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?;
            }
            Ok(())
        }
        Expr::New(n) => {
            // validate constructor call only for current class
            if n.target_type.name == current_class_name {
                let found_arity = n.arguments.len();
                if let Some(expected_list) = ctor_arities.get(current_class_name) {
                    let matches_fixed = expected_list.iter().any(|a| *a == found_arity);
                    let matches_varargs = ctor_varargs_min.get(current_class_name).map(|min| found_arity >= *min).unwrap_or(false);
                    if !matches_fixed && !matches_varargs {
                        let mut expected = expected_list.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                        if let Some(min) = ctor_varargs_min.get(current_class_name) { expected.push(format!("{}+", min)); }
                        let expected = expected.join(", ");
                        return Err(ReviewError::MethodCallArityMismatch { name: format!("<init> {}", current_class_name), expected, found: found_arity });
                    }
                    if let Some(cands) = ctor_signatures.get(current_class_name) {
                        let found_types: Vec<Option<&'static str>> = n.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                        if found_types.iter().all(|o| o.is_some()) {
                            let found_list: Vec<&str> = found_types.iter().map(|o| o.unwrap()).collect();
                            let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                            for sig in cands {
                                if sig.len() == found_list.len() {
                                    let mut ok = true;
                                    let mut cost: u32 = 0;
                                    let mut convs: u32 = 0;
                                    for (exp, f) in sig.iter().zip(found_list.iter()) {
                                        if !is_assignable_primitive_or_string(exp.as_str(), f) { ok = false; break; }
                                        if exp.as_str() != *f { convs += 1; }
                                        cost += widening_cost(exp.as_str(), f);
                                    }
                                    if ok { applicable.push((sig, cost, convs)); }
                                }
                            }
                            if applicable.is_empty() && !cands.is_empty() {
                                let expected = cands.iter().filter(|s| s.len()==found_list.len()).map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                let found = found_list.join(",");
                                return Err(ReviewError::InapplicableMethod { name: format!("<init> {}", current_class_name), expected, found });
                            } else if applicable.len() > 1 {
                                let min_convs = applicable.iter().map(|(_, _, k)| *k).min().unwrap();
                                applicable.retain(|(_, _, k)| *k == min_convs);
                                let min_cost = applicable.iter().map(|(_, c, _)| *c).min().unwrap();
                                let tied: Vec<&Vec<String>> = applicable.iter().filter(|(_, c, _)| *c == min_cost).map(|(s, _, _)| *s).collect();
                                if tied.len() > 1 {
                                    let candidates = tied.iter().map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                    let found = found_list.join(",");
                                    return Err(ReviewError::AmbiguousMethod { name: format!("<init> {}", current_class_name), candidates, found });
                                }
                            }
                        }
                    }
                }
            } else if let Some(g) = global {
                if let Some(mt) = resolve_type_in_index(g, &n.target_type.name) {
                    // Simple generic arity check: number of <T> in type use must match declaration
                    if n.target_type.type_args.len() != mt.type_param_count {
                        return Err(ReviewError::GenericArityMismatch { typename: n.target_type.name.clone(), expected: mt.type_param_count, found: n.target_type.type_args.len() });
                    }
                    // Basic bounds check: if bounds recorded, ensure each argument type simple name matches or is assignable to the bound simple name
                    if !mt.type_param_bounds.is_empty() {
                        for (i, targ) in n.target_type.type_args.iter().enumerate() {
                            if let Some(bounds) = mt.type_param_bounds.get(i) {
                                for b in bounds {
                                    if !is_reference_assignable(g, &targ.name, b) {
                                        return Err(ReviewError::GenericBoundViolation { typename: n.target_type.name.clone(), bound: b.clone(), found: targ.name.clone() });
                                    }
                                }
                            }
                        }
                    }
                    let found_arity = n.arguments.len();
                    // Optional: ensure generic arity usage matches declaration (simple count check)
                    if let Some(expected_list) = mt.ctors_arities.get(&n.target_type.name) {
                        let matches_fixed = expected_list.iter().any(|a| *a == found_arity);
                        let matches_varargs = mt.ctors_varargs_min.get(&n.target_type.name).map(|min| found_arity >= *min).unwrap_or(false);
                        if !matches_fixed && !matches_varargs {
                            let mut expected = expected_list.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                            if let Some(min) = mt.ctors_varargs_min.get(&n.target_type.name) { expected.push(format!("{}+", min)); }
                            let expected = expected.join(", ");
                            return Err(ReviewError::MethodCallArityMismatch { name: format!("<init> {}", n.target_type.name), expected, found: found_arity });
                        }
                        if let Some(cands) = mt.ctors_signatures.get(&n.target_type.name) {
                            let found_types: Vec<Option<&'static str>> = n.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                            if found_types.iter().all(|o| o.is_some()) {
                                let found_list: Vec<&str> = found_types.iter().map(|o| o.unwrap()).collect();
                                let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                                for sig in cands {
                                    if sig.len() == found_list.len() {
                                        let mut ok = true;
                                        let mut cost: u32 = 0;
                                        let mut convs: u32 = 0;
                                        for (exp, f) in sig.iter().zip(found_list.iter()) {
                                            if !is_assignable_primitive_or_string(exp.as_str(), f) { ok = false; break; }
                                            if exp.as_str() != *f { convs += 1; }
                                            cost += widening_cost(exp.as_str(), f);
                                        }
                                        if ok { applicable.push((sig, cost, convs)); }
                                    }
                                }
                                if applicable.is_empty() && !cands.is_empty() {
                                    let expected = cands.iter().filter(|s| s.len()==found_list.len()).map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                    let found = found_list.join(",");
                                    return Err(ReviewError::InapplicableMethod { name: format!("<init> {}", n.target_type.name), expected, found });
                                } else if applicable.len() > 1 {
                                    let min_convs = applicable.iter().map(|(_, _, k)| *k).min().unwrap();
                                    applicable.retain(|(_, _, k)| *k == min_convs);
                                    let min_cost = applicable.iter().map(|(_, c, _)| *c).min().unwrap();
                                    let tied: Vec<&Vec<String>> = applicable.iter().filter(|(_, c, _)| *c == min_cost).map(|(s, _, _)| *s).collect();
                                    if tied.len() > 1 {
                                        let candidates = tied.iter().map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                        let found = found_list.join(",");
                                        return Err(ReviewError::AmbiguousMethod { name: format!("<init> {}", n.target_type.name), candidates, found });
                                    }
                                }
                            }
                        }
                    }
                } else if let Some(expected_params) = lookup_type_param_count(g, &n.target_type.name) {
                    if n.target_type.type_args.len() != expected_params {
                        return Err(ReviewError::GenericArityMismatch { typename: n.target_type.name.clone(), expected: expected_params, found: n.target_type.type_args.len() });
                    }
                }
            }
            for a in &n.arguments { walk_expr(current_class_name, a, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name)?; }
            Ok(())
        }
        Expr::Parenthesized(p) => walk_expr(current_class_name, p, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, errors_as_name),
        _ => Ok(()),
    }
}

fn infer_literal_primitive_or_string(expr: &Expr) -> Option<&'static str> {
    if let Expr::Literal(lit) = expr { match lit.value {
        crate::ast::Literal::Integer(_) => Some("int"),
        crate::ast::Literal::Float(_) => Some("double"),
        crate::ast::Literal::Boolean(_) => Some("boolean"),
        crate::ast::Literal::String(_) => Some("String"),
        crate::ast::Literal::Char(_) => Some("char"),
        crate::ast::Literal::Null => Some("null"),
    }} else { None }
}

fn is_assignable_literal(expected: &str, found: &str) -> bool {
    if found == "null" { return true; }
    match expected {
        "int" => found == "int",
        "double" => found == "double" || found == "int",
        "boolean" => found == "boolean",
        "char" => found == "char" || found == "int",
        "String" => found == "String",
        _ => true,
    }
}

fn is_assignable_primitive_or_string(expected: &str, found: &str) -> bool {
    if found == "null" { return expected != "int" && expected != "long" && expected != "double" && expected != "float" && expected != "boolean" && expected != "char"; }
    if expected == found { return true; }
    match expected {
        "double" => matches!(found, "float"|"int"|"long"),
        "float" => matches!(found, "int"|"long"),
        "long" => matches!(found, "int"),
        "int" => matches!(found, "char"),
        "String" => found == "String",
        _ => false,
    }
}

fn infer_expr_primitive_or_string(expr: &Expr) -> Option<&'static str> {
    match expr {
        // literals
        Expr::Literal(_) => infer_literal_primitive_or_string(expr),
        // parenthesized
        Expr::Parenthesized(inner) => infer_expr_primitive_or_string(inner),
        // cast to a known primitive or String
        Expr::Cast(c) => match c.target_type.name.as_str() {
            "int" | "long" | "float" | "double" | "boolean" | "char" | "String" => Some(
                match c.target_type.name.as_str() {
                    "int" => "int",
                    "long" => "long",
                    "float" => "float",
                    "double" => "double",
                    "boolean" => "boolean",
                    "char" => "char",
                    "String" => "String",
                    _ => unreachable!(),
                }
            ),
            _ => None,
        },
        // unary ops
        Expr::Unary(u) => {
            match u.operator {
                crate::ast::UnaryOp::Plus | crate::ast::UnaryOp::Minus => {
                    match infer_expr_primitive_or_string(&u.operand) {
                        Some("double") => Some("double"),
                        Some("float") => Some("float"),
                        Some("long") => Some("long"),
                        Some("int") => Some("int"),
                        Some("char") => Some("int"), // integral promotion
                        _ => None,
                    }
                }
                crate::ast::UnaryOp::Not => Some("boolean"),
                crate::ast::UnaryOp::BitNot => {
                    // integral promotion → at least int
                    match infer_expr_primitive_or_string(&u.operand) {
                        Some("double") | Some("float") => None,
                        Some("long") => Some("long"),
                        Some("int") | Some("char") => Some("int"),
                        _ => None,
                    }
                }
                _ => None,
            }
        }
        // binary ops
        Expr::Binary(b) => {
            let lt = infer_expr_primitive_or_string(&b.left);
            let rt = infer_expr_primitive_or_string(&b.right);
            use crate::ast::BinaryOp as B;
            match b.operator {
                B::Add => {
                    if matches!(lt, Some("String")) || matches!(rt, Some("String")) {
                        Some("String")
                    } else { numeric_promotion(lt, rt) }
                }
                B::Sub | B::Mul | B::Div | B::Mod => numeric_promotion(lt, rt),
                B::Lt | B::Le | B::Gt | B::Ge | B::Eq | B::Ne => Some("boolean"),
                B::And | B::Or | B::Xor => {
                    // boolean short-circuit or bitwise? approximate as boolean if both boolean
                    if matches!(lt, Some("boolean")) && matches!(rt, Some("boolean")) { Some("boolean") } else { None }
                }
                B::LShift | B::RShift | B::URShift => {
                    // result is left operand type after integral promotion
                    match lt { Some("long") => Some("long"), Some("int") | Some("char") => Some("int"), _ => None }
                }
            }
        }
        // new String(...)
        Expr::New(n) => if n.target_type.name == "String" { Some("String") } else { None },
        // pass-through for assignment/conditional: pick one side
        Expr::Assignment(a) => infer_expr_primitive_or_string(&a.value),
        Expr::Conditional(c) => {
            let t = infer_expr_primitive_or_string(&c.then_expr);
            let e = infer_expr_primitive_or_string(&c.else_expr);
            match (t, e) {
                (Some(tt), Some(ee)) if tt == ee => Some(tt),
                (Some(tt), Some(ee)) => {
                    // numeric promotion when both numeric
                    if is_numeric_primitive(tt) && is_numeric_primitive(ee) {
                        numeric_promotion(Some(tt), Some(ee))
                    } else if (tt == "String" && ee == "null") || (tt == "null" && ee == "String") {
                        Some("String")
                    } else if tt == "boolean" && ee == "boolean" {
                        Some("boolean")
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn numeric_promotion(lt: Option<&'static str>, rt: Option<&'static str>) -> Option<&'static str> {
    use std::cmp::Ordering;
    fn rank(t: &str) -> Option<u8> {
        match t { "double" => Some(4), "float" => Some(3), "long" => Some(2), "int" => Some(1), "char" => Some(0), _ => None }
    }
    let l = lt.and_then(rank);
    let r = rt.and_then(rank);
    match (l, r) {
        (Some(lr), Some(rr)) => {
            match lr.cmp(&rr) {
                Ordering::Greater => Some(match lt.unwrap() { "char" => "int", x => x }),
                Ordering::Less => Some(match rt.unwrap() { "char" => "int", x => x }),
                Ordering::Equal => Some(match lt.unwrap() { "char" => "int", x => x }),
            }
        }
        _ => None,
    }
}

fn is_numeric_primitive(t: &str) -> bool {
    matches!(t, "int" | "long" | "float" | "double" | "char")
}

fn widening_cost(expected: &str, found: &str) -> u32 {
    if expected == found { return 0; }
    // Assign a small cost per widening step; non-primitive or incompatible treated earlier
    match (expected, found) {
        ("double", "float") => 1,
        ("double", "long") => 2,
        ("double", "int") => 3,
        ("float", "int") => 1,
        ("float", "long") => 2, // not Java-widening strictly (long->float is allowed), cost higher
        ("long", "int") => 1,
        ("int", "char") => 1,
        ("String", "String") => 0,
        _ => 5,
    }
}


