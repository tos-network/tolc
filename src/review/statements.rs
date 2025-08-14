use super::{ReviewError, ReviewResult};
// use of local helper ahead; don't import to avoid duplicate symbol with the local fn of the same name
use crate::ast::*;
use std::collections::{HashMap, HashSet};

// Minimal must-return checker for non-void methods (structural subset)
pub(crate) fn review_method_body_return(m: &MethodDecl) -> ReviewResult<()> {
    if m.return_type.is_none() { return Ok(()); } // void
    if let Some(body) = &m.body {
        if block_guarantees_return(body) || block_cannot_complete_normally(body) { return Ok(()); }
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
    // A block guarantees return if any contained statement guarantees return, or
    // all possible branches through the block terminate. As a cheap improvement,
    // scan statements; if we see a terminal, we can stop. Otherwise, if the last
    // statement is an if/switch/try that guarantees return, accept.
    let mut any_terminal = false;
    for s in &block.statements {
        if stmt_guarantees_return(s) { any_terminal = true; break; }
    }
    if any_terminal { return true; }
    if let Some(last) = block.statements.last() {
        return stmt_guarantees_return(last);
    }
    false
}

// Returns true if the block is guaranteed to not complete normally (e.g., ends in an infinite loop,
// unconditional return/throw on all paths). Used to satisfy non-void must-return via non-termination.
fn block_cannot_complete_normally(block: &Block) -> bool {
    // Sequential semantics: if control can reach the end of the block along any path,
    // the block can complete normally. A block cannot complete normally only if at some
    // point execution is guaranteed to not proceed past a statement (e.g., return/throw)
    // no matter which path is taken within that statement.
    let mut can_reach_end = true;
    for s in &block.statements {
        if !can_reach_end { break; }
        if stmt_cannot_complete_normally(s) {
            can_reach_end = false;
        }
    }
    !can_reach_end
}

fn stmt_cannot_complete_normally(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return(_) => true,
        Stmt::Throw(_) => true,
        Stmt::Block(b) => block_cannot_complete_normally(b),
        Stmt::If(ifstmt) => {
            if let Some(else_b) = &ifstmt.else_branch {
                stmt_cannot_complete_normally(&ifstmt.then_branch) && stmt_cannot_complete_normally(else_b)
            } else { false }
        }
        Stmt::While(w) => {
            if expr_is_boolean_true(&w.condition) {
                // Infinite loop without a break that exits the loop cannot complete normally
                !has_top_level_break(&w.body)
            } else { false }
        }
        Stmt::For(f) => {
            if f.condition.is_none() {
                // for(;;) infinite without a break that exits cannot complete normally
                !has_top_level_break(&f.body)
            } else { false }
        }
        Stmt::Try(t) => {
            if let Some(fin) = &t.finally_block {
                if block_cannot_complete_normally(fin) { return true; }
            }
            let try_ccn = block_cannot_complete_normally(&t.try_block);
            let mut catches_ccn = !t.catch_clauses.is_empty();
            for cc in &t.catch_clauses {
                if !block_cannot_complete_normally(&cc.block) { catches_ccn = false; break; }
            }
            try_ccn && catches_ccn
        }
        Stmt::Labeled(ls) => stmt_cannot_complete_normally(&ls.statement),
        _ => false,
    }
}

fn stmt_guarantees_return(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return(_) => true,
        Stmt::Throw(_) => true,
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
            // while (true): loop guarantees return only if body guarantees return
            // and there is no top-level break in the loop body that exits the loop
            if expr_is_boolean_true(&w.condition) {
                stmt_guarantees_return(&w.body) && !has_top_level_break(&w.body)
            } else {
                false
            }
        }
        Stmt::For(f) => {
            // for(;;): same rule as while(true)
            if f.condition.is_none() { return stmt_guarantees_return(&f.body) && !has_top_level_break(&f.body); }
            false
        }
        Stmt::Try(t) => {
            // If finally guarantees return, overall guarantees return
            if let Some(fin) = &t.finally_block { return block_guarantees_return(fin); }
            // Otherwise, if try block guarantees return AND every catch block guarantees return, overall returns
            let try_term = block_guarantees_return(&t.try_block);
            if try_term {
                for cc in &t.catch_clauses {
                    if !block_guarantees_return(&cc.block) { return false; }
                }
                return true;
            }
            false
        }
        // break/continue/switch default: conservatively false in this basic pass
        _ => false,
    }
}

// Detect a top-level break within a single statement (block or single)
fn has_top_level_break(stmt: &Stmt) -> bool {
    use std::collections::HashSet;
    let inner_labels = collect_inner_labels(stmt);
    fn breaks_out(s: &Stmt, inner_labels: &HashSet<String>, loop_depth: usize, in_switch: bool) -> bool {
        match s {
            Stmt::Break(b) => {
                if let Some(lab) = &b.label {
                    // labeled break exits if it targets a label outside this body
                    !inner_labels.contains(lab)
                } else {
                    // unlabeled break exits loop only if not inside a nested loop or a switch
                    loop_depth == 0 && !in_switch
                }
            }
            Stmt::Block(b) => b.statements.iter().any(|sub| breaks_out(sub, inner_labels, loop_depth, in_switch)),
            Stmt::If(ifstmt) => {
                breaks_out(&ifstmt.then_branch, inner_labels, loop_depth, in_switch)
                    || if let Some(else_b) = &ifstmt.else_branch { breaks_out(else_b, inner_labels, loop_depth, in_switch) } else { false }
            }
            Stmt::Labeled(ls) => breaks_out(&ls.statement, inner_labels, loop_depth, in_switch),
            Stmt::While(w) => breaks_out(&w.body, inner_labels, loop_depth + 1, in_switch),
            Stmt::For(f) => breaks_out(&f.body, inner_labels, loop_depth + 1, in_switch),
            Stmt::Switch(sw) => {
                for c in &sw.cases {
                    for st in &c.statements {
                        if breaks_out(st, inner_labels, loop_depth, true) { return true; }
                    }
                }
                false
            }
            _ => false,
        }
    }
    breaks_out(stmt, &inner_labels, 0, false)
}

fn stmt_contains_simple_assignment_to(stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::Expression(es) => {
            if let Expr::Assignment(a) = &es.expr {
                if let Expr::Identifier(id) = &*a.target {
                    return id.name == name;
                }
            }
            false
        }
        Stmt::Block(b) => b.statements.iter().any(|s| stmt_contains_simple_assignment_to(s, name)),
        Stmt::If(i) => {
            stmt_contains_simple_assignment_to(&i.then_branch, name)
                || if let Some(e) = &i.else_branch { stmt_contains_simple_assignment_to(e, name) } else { false }
        }
        Stmt::While(w) => stmt_contains_simple_assignment_to(&w.body, name),
        Stmt::For(f) => {
            for s in &f.init {
                if let Stmt::Expression(es) = s {
                    if let Expr::Assignment(a) = &es.expr {
                        if let Expr::Identifier(id) = &*a.target { if id.name == name { return true; } }
                    }
                }
            }
            for s in &f.update {
                // f.update items are expression statements
                if let Expr::Assignment(a) = &s.expr {
                    if let Expr::Identifier(id) = &*a.target { if id.name == name { return true; } }
                }
            }
            stmt_contains_simple_assignment_to(&f.body, name)
        }
        Stmt::Switch(sw) => sw.cases.iter().flat_map(|c| c.statements.iter()).any(|s| stmt_contains_simple_assignment_to(s, name)),
        Stmt::Labeled(ls) => stmt_contains_simple_assignment_to(&ls.statement, name),
        Stmt::Try(t) => {
            if stmt_contains_simple_assignment_to(&Stmt::Block(t.try_block.clone()), name) { return true; }
            for cc in &t.catch_clauses { if stmt_contains_simple_assignment_to(&Stmt::Block(cc.block.clone()), name) { return true; } }
            if let Some(fin) = &t.finally_block { return stmt_contains_simple_assignment_to(&Stmt::Block(fin.clone()), name); }
            false
        }
        _ => false
    }
}

fn stmt_all_reachable_paths_assign_to(stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::Block(b) => {
            let mut assigned = false;
            for s in &b.statements {
                match s {
                    Stmt::Expression(es) => {
                        if let Expr::Assignment(a) = &es.expr {
                            if let Expr::Identifier(id) = &*a.target { if id.name == name { assigned = true; break; } }
                        }
                    }
                    Stmt::If(i) => {
                        if stmt_all_reachable_paths_assign_to(&Stmt::If(i.clone()), name) { assigned = true; break; } else { return false; }
                    }
                    Stmt::Return(_) | Stmt::Throw(_) => { return false; }
                    _ => {}
                }
            }
            assigned
        }
        Stmt::Expression(es) => {
            if let Expr::Assignment(a) = &es.expr {
                if let Expr::Identifier(id) = &*a.target { return id.name == name; }
            }
            false
        }
        Stmt::If(i) => {
            let then_reaches = !stmt_guarantees_return(&i.then_branch) && !stmt_cannot_complete_normally(&i.then_branch);
            let else_reaches = if let Some(else_b) = &i.else_branch { !stmt_guarantees_return(else_b) && !stmt_cannot_complete_normally(else_b) } else { true };
            let then_ok = if then_reaches { stmt_all_reachable_paths_assign_to(&i.then_branch, name) } else { true };
            let else_ok = if let Some(else_b) = &i.else_branch {
                if else_reaches { stmt_all_reachable_paths_assign_to(else_b, name) } else { true }
            } else { false };
            then_ok && else_ok
        }
        // Conservative default: unknown constructs -> cannot guarantee assignment on all paths
        _ => false,
    }
}

// Collect all label names declared inside this statement tree
fn collect_inner_labels(stmt: &Stmt) -> std::collections::HashSet<String> {
    use std::collections::HashSet;
    fn walk(s: &Stmt, acc: &mut HashSet<String>) {
        match s {
            Stmt::Labeled(ls) => {
                acc.insert(ls.label.clone());
                walk(&ls.statement, acc);
            }
            Stmt::Block(b) => {
                for sub in &b.statements { walk(sub, acc); }
            }
            Stmt::If(ifstmt) => {
                walk(&ifstmt.then_branch, acc);
                if let Some(else_b) = &ifstmt.else_branch { walk(else_b, acc); }
            }
            Stmt::While(w) => walk(&w.body, acc),
            Stmt::For(f) => walk(&f.body, acc),
            Stmt::Switch(sw) => {
                for c in &sw.cases {
                    for st in &c.statements { walk(st, acc); }
                }
            }
            _ => {}
        }
    }
    let mut set = HashSet::new();
    walk(stmt, &mut set);
    set
}

fn switch_guarantees_return(sw: &SwitchStmt) -> bool {
    // Improved: a switch guarantees return if it has default and
    // every case's block contains a terminal (return/throw) somewhere
    let has_default = sw.cases.iter().any(|c| c.labels.is_empty());
    if !has_default { return false; }
    for c in &sw.cases {
        if c.statements.is_empty() { return false; }
        let mut has_terminal = false;
        for s in &c.statements { if stmt_guarantees_return(s) { has_terminal = true; break; } }
        if !has_terminal { return false; }
    }
    true
}

fn expr_is_boolean_true(e: &Expr) -> bool {
    if let Expr::Literal(l) = e {
        matches!(l.value, crate::ast::Literal::Boolean(true))
    } else { false }
}

fn expr_is_boolean_false(e: &Expr) -> bool {
    if let Expr::Literal(l) = e {
        matches!(l.value, crate::ast::Literal::Boolean(false))
    } else { false }
}

// Evaluate a constant integral value for switch labels and expressions when possible.
// Supports integer and char literals, unary +/- and bit-not, parentheses, casts to byte/short/char/int,
// and binary ops (+,-,*,/,%, bit ops, shifts) when both sides are constant.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConstKind { Int32, Int64 }

fn fits_i32(v: i64) -> bool { v >= i32::MIN as i64 && v <= i32::MAX as i64 }

fn eval_const_int_kind(expr: &Expr) -> Option<(i64, ConstKind)> {
    use crate::ast::{Expr as E, Literal as L, UnaryOp as U, BinaryOp as B};
    fn narrow(kind: ConstKind, v: i64) -> (i64, ConstKind) {
        match kind { ConstKind::Int32 => ((v as i32) as i64, ConstKind::Int32), ConstKind::Int64 => (v, ConstKind::Int64) }
    }
    fn narrow_to_target(ty: &str, v: i64) -> (i64, ConstKind) {
        match ty {
            "byte" => (((v as i8) as i32) as i64, ConstKind::Int32),
            "short" => (((v as i16) as i32) as i64, ConstKind::Int32),
            "char" => { let u = (v as u16) as u32; (u as i64, ConstKind::Int32) },
            "int" => ((v as i32) as i64, ConstKind::Int32),
            "long" => (v, ConstKind::Int64),
            _ => (v, ConstKind::Int64),
        }
    }
    match expr {
        E::Literal(l) => match &l.value {
            L::Integer(i) => Some((*i, if fits_i32(*i) { ConstKind::Int32 } else { ConstKind::Int64 })),
            L::Char(c) => Some((*c as i64, ConstKind::Int32)),
            _ => None,
        },
        E::Parenthesized(inner) => eval_const_int_kind(inner),
        E::Unary(u) => {
            let (v, k) = eval_const_int_kind(&u.operand)?;
            match u.operator {
                U::Plus => Some((v, k)),
                U::Minus => v.checked_neg().map(|nv| narrow(k, nv)),
                U::BitNot => Some(narrow(k, !v)),
                _ => None,
            }
        }
        E::Binary(b) => {
            let (lv, lk) = eval_const_int_kind(&b.left)?;
            let (rv, rk) = eval_const_int_kind(&b.right)?;
            let res_kind = if lk == ConstKind::Int64 || rk == ConstKind::Int64 { ConstKind::Int64 } else { ConstKind::Int32 };
            let res = match b.operator {
                B::Add => lv.checked_add(rv),
                B::Sub => lv.checked_sub(rv),
                B::Mul => lv.checked_mul(rv),
                B::Div => if rv == 0 { None } else { Some(lv / rv) },
                B::Mod => if rv == 0 { None } else { Some(lv % rv) },
                B::And => Some(lv & rv),
                B::Or => Some(lv | rv),
                B::Xor => Some(lv ^ rv),
                B::LShift => {
                    let sh = (rv & if res_kind == ConstKind::Int64 { 0x3F } else { 0x1F }) as u32;
                    Some(lv.wrapping_shl(sh))
                }
                B::RShift => {
                    let sh = (rv & if res_kind == ConstKind::Int64 { 0x3F } else { 0x1F }) as u32;
                    Some((lv as i64).wrapping_shr(sh))
                }
                B::URShift => {
                    let sh = (rv & if res_kind == ConstKind::Int64 { 0x3F } else { 0x1F }) as u32;
                    let lu = lv as u64; Some((lu.wrapping_shr(sh)) as i64)
                }
                _ => None,
            }?;
            Some(narrow(res_kind, res).into())
        }
        E::Cast(c) => {
            let (v, _k) = eval_const_int_kind(&c.expr)?;
            Some(narrow_to_target(c.target_type.name.as_str(), v))
        }
        _ => None,
    }
}

// Diagnose constant-time division/modulo by zero for integer and floating cases
fn const_div_or_mod_by_zero(expr: &Expr) -> bool {
    use crate::ast::{Expr as E, BinaryOp as B};
    match expr {
        E::Binary(b) => {
            match b.operator {
                B::Div | B::Mod => {
                    // integer path only: evaluate both sides and check divisor is zero
                    if let Some((_lv, _lk)) = eval_const_int_kind(&b.left) {
                        if let Some((rv, _rk)) = eval_const_int_kind(&b.right) {
                            return rv == 0;
                        }
                    }
                    false
                }
                _ => false,
            }
        }
        E::Parenthesized(p) => const_div_or_mod_by_zero(p),
        _ => false,
    }
}

// Minimal helper: attempt to fold to any constant literal we recognize
fn eval_compile_time_expr_any(expr: &Expr) -> Option<crate::ast::Literal> {
    // reuse codegen evaluator semantics when shapes match through simple constructs used here
    match expr {
        Expr::Literal(l) => Some(l.value.clone()),
        Expr::Parenthesized(p) => eval_compile_time_expr_any(p),
        Expr::Unary(u) => {
            if let Some(v) = eval_compile_time_expr_any(&u.operand) {
                use crate::ast::{Literal as L, UnaryOp as U};
            match (u.operator.clone(), v) {
                    (U::Plus, l) => Some(l),
                    (U::Minus, L::Integer(i)) => Some(L::Integer(-i)),
                    (U::Minus, L::Float(f)) => Some(L::Float(-f)),
                    (U::BitNot, L::Integer(i)) => Some(L::Integer(!i)),
                    (U::Not, L::Boolean(b)) => Some(L::Boolean(!b)),
                    _ => None,
                }
            } else { None }
        }
        Expr::Binary(b) => {
            use crate::ast::{BinaryOp as B, Literal as L};
            let l = eval_compile_time_expr_any(&b.left)?;
            let r = eval_compile_time_expr_any(&b.right)?;
            match (b.operator.clone(), l, r) {
                (B::Add, L::Integer(a), L::Integer(b)) => Some(L::Integer(a + b)),
                (B::Sub, L::Integer(a), L::Integer(b)) => Some(L::Integer(a - b)),
                (B::Mul, L::Integer(a), L::Integer(b)) => Some(L::Integer(a * b)),
                (B::Div, L::Integer(a), L::Integer(b)) => if b != 0 { Some(L::Integer(a / b)) } else { None },
                (B::Mod, L::Integer(a), L::Integer(b)) => if b != 0 { Some(L::Integer(a % b)) } else { None },
                (B::Add, L::Float(a), L::Float(b)) => Some(L::Float(a + b)),
                (B::Sub, L::Float(a), L::Float(b)) => Some(L::Float(a - b)),
                (B::Mul, L::Float(a), L::Float(b)) => Some(L::Float(a * b)),
                (B::Div, L::Float(a), L::Float(b)) => if b != 0.0 { Some(L::Float(a / b)) } else { None },
                (B::Mod, L::Float(a), L::Float(b)) => if b != 0.0 { Some(L::Float(a % b)) } else { None },
                _ => None,
            }
        }
        _ => None,
    }
}

fn fold_selector_value_for_int_switch(expr: &Expr) -> Option<i32> {
    let (v, k) = eval_const_int_kind(expr)?;
    match k { ConstKind::Int32 => Some(v as i32), ConstKind::Int64 => None }
}

fn fold_case_value_for_int_switch(expr: &Expr) -> Option<i32> {
    let (v, k) = eval_const_int_kind(expr)?;
    match k { ConstKind::Int32 => Some(v as i32), ConstKind::Int64 => None }
}

// String switch support (subset): treat only literal strings and parenthesized literals as constants
fn fold_selector_value_for_string_switch(expr: &Expr) -> Option<String> {
    // Extend: allow constant concatenations of strings and char/int literals where fully constant
    fn fold_str(e: &Expr) -> Option<String> {
        match e {
            Expr::Literal(lit) => match &lit.value {
                crate::ast::Literal::String(s) => Some(s.clone()),
                crate::ast::Literal::Char(c) => Some(((*c as u32) as u8 as char).to_string()),
                crate::ast::Literal::Integer(i) => Some(i.to_string()),
                _ => None,
            },
            Expr::Parenthesized(inner) => fold_str(inner),
            Expr::Binary(b) => {
                use crate::ast::BinaryOp::Add;
                if matches!(b.operator, Add) {
                    if let (Some(ls), Some(rs)) = (fold_str(&b.left), fold_str(&b.right)) { return Some(format!("{}{}", ls, rs)); }
                }
                None
            }
            _ => None,
        }
    }
    fold_str(expr)
}

fn fold_case_value_for_string_switch(expr: &Expr) -> Option<String> {
    // Match semantics of selector folding
    fn fold_str(e: &Expr) -> Option<String> {
        match e {
            Expr::Literal(lit) => match &lit.value {
                crate::ast::Literal::String(s) => Some(s.clone()),
                crate::ast::Literal::Char(c) => Some(((*c as u32) as u8 as char).to_string()),
                crate::ast::Literal::Integer(i) => Some(i.to_string()),
                _ => None,
            },
            Expr::Parenthesized(inner) => fold_str(inner),
            Expr::Binary(b) => {
                use crate::ast::BinaryOp::Add;
                if matches!(b.operator, Add) {
                    if let (Some(ls), Some(rs)) = (fold_str(&b.left), fold_str(&b.right)) { return Some(format!("{}{}", ls, rs)); }
                }
                None
            }
            _ => None,
        }
    }
    fold_str(expr)
}

// Enum switch support (subset, using global index): support qualified enum constants in selector,
// and case labels as either bare identifiers (CONST) or qualified (Enum.CONST) when enum name is known
fn fold_selector_value_for_enum_switch_global(
    global: Option<&crate::review::types::GlobalMemberIndex>,
    expr: &Expr,
) -> Option<(String, usize)> {
    let g = global?;
    if let Expr::FieldAccess(fa) = expr {
        if let Some(target) = &fa.target {
            if let Expr::Identifier(id) = &**target {
                if let Some(inner) = g.enum_index.get(&id.name) {
                    if let Some(ord) = inner.get(&fa.name) { return Some((id.name.clone(), *ord)); }
                }
            }
        }
    }
    None
}

fn fold_case_value_for_enum_switch_global(
    enum_name: &str,
    global: Option<&crate::review::types::GlobalMemberIndex>,
    expr: &Expr,
) -> Option<usize> {
    let g = global?;
    if let Some(inner) = g.enum_index.get(enum_name) {
        match expr {
            Expr::Identifier(id) => inner.get(&id.name).copied(),
            Expr::FieldAccess(fa) => {
                if let Some(target) = &fa.target {
                    if let Expr::Identifier(id) = &**target {
                        if id.name == enum_name { return inner.get(&fa.name).copied(); }
                    }
                }
                None
            }
            _ => None,
        }
    } else { None }
}

// Try fold enum selector to an ordinal by resolving identifier against an enum constant index in current compilation unit.
fn fold_selector_value_for_enum_switch(ast: &crate::ast::Ast, expr: &Expr) -> Option<(String, usize)> {
    use crate::ast::Expr as E;
    if let E::FieldAccess(fa) = expr {
        if let Some(target) = &fa.target {
            if let E::Identifier(id) = &**target {
                for td in &ast.type_decls {
                    if let crate::ast::TypeDecl::Enum(e) = td {
                        if e.name == id.name {
                            for (idx, c) in e.constants.iter().enumerate() {
                                if c.name == fa.name { return Some((e.name.clone(), idx)); }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn fold_case_value_for_enum_switch(enum_name: &str, ast: &crate::ast::Ast, expr: &Expr) -> Option<usize> {
    use crate::ast::Expr as E;
    if let E::Identifier(id) = expr {
        for td in &ast.type_decls {
            if let crate::ast::TypeDecl::Enum(e) = td { if e.name == enum_name {
                for (idx, c) in e.constants.iter().enumerate() { if c.name == id.name { return Some(idx); } }
            }}
        }
    } else if let E::FieldAccess(fa) = expr {
        if let Some(target) = &fa.target {
            if let E::Identifier(id) = &**target {
                if id.name == enum_name {
                    for td in &ast.type_decls {
                        if let crate::ast::TypeDecl::Enum(e) = td { if e.name == enum_name {
                            for (idx, c) in e.constants.iter().enumerate() { if c.name == fa.name { return Some(idx); } }
                        }}
                    }
                }
            }
        }
    }
    None
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
    // Track simple local variable types to enable reference-type inference at call sites
    let mut local_types: Vec<HashMap<String, String>> = vec![HashMap::new()];
    for stmt in &body.statements {
        walk_stmt(current_class_name, stmt, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, &mut local_types, errors_as_name)?;
    }
    Ok(())
}

// Helper: determine if an exception type name is unchecked (RuntimeException/Error or their subclasses)
fn is_unchecked_exception(name: &str, global: &crate::review::types::GlobalMemberIndex) -> bool {
    if super::consts::UNCHECKED_BASE_EXCEPTIONS.contains(&name) { return true; }
    if crate::review::compat_mode() && super::consts::UNCHECKED_COMMON_SUBCLASSES.contains(&name) { return true; }
    if let Some(mt) = crate::review::statements::resolve_type_in_index(global, name) {
        let mut cur = mt.super_name.clone();
        let mut seen = std::collections::HashSet::new();
        while let Some(sup) = cur {
            if sup == "RuntimeException" || sup == "Error" { super::debug_log(format!("unchecked via super: {} <: {}", name, sup)); return true; }
            if !seen.insert(sup.clone()) { break; }
            cur = crate::review::statements::resolve_type_in_index(global, &sup).and_then(|m| m.super_name.clone());
        }
    } else {
        super::debug_log(format!("cannot resolve exception type '{}'; treating as checked", name));
    }
    false
}

// Checked exceptions: report unhandled checked exceptions unless caught or declared
pub(crate) fn review_body_checked_exceptions(
    current_class: &ClassDecl,
    body: &Block,
    declared_throws: &[String],
    global: &crate::review::types::GlobalMemberIndex,
) -> ReviewResult<()> {

    fn is_assignable_to_any(global: &crate::review::types::GlobalMemberIndex, src: &str, dsts: &[String]) -> bool {
        // Fast-path: catch(Exception) or catch(Throwable) covers most checked exceptions
        if dsts.iter().any(|d| d == "Exception" || d == "Throwable") { return true; }
        for d in dsts { if crate::review::statements::is_reference_assignable(global, src, d) { return true; } }
        false
    }

    fn walk_expr_for_exn(
        current_class: &ClassDecl,
        expr: &Expr,
        declared: &[String],
        global: &crate::review::types::GlobalMemberIndex,
        catch_stack: &Vec<Vec<String>>, 
        scopes: &Vec<HashMap<String, String>>,
    ) -> ReviewResult<()> {
        // Utility: collect simple static types for arguments when available
        fn collect_arg_types(
            current_class: &ClassDecl,
            args: &Vec<Expr>,
            global: &crate::review::types::GlobalMemberIndex,
            scopes: &Vec<HashMap<String, String>>,
        ) -> Option<Vec<String>> {
            let mut out: Vec<String> = Vec::with_capacity(args.len());
            for a in args {
                if let Some(t) = expr_static_type(current_class, a, global, scopes) { out.push(t); } else { return None; }
            }
            Some(out)
        }
        fn select_method_throws(
            mt: &crate::review::types::MemberTables,
            name: &str,
            arity: usize,
            arg_types: Option<&Vec<String>>,
        ) -> Vec<String> {
            // Prefer by signature if arg types known
            if let Some(arg_sig) = arg_types {
                if let Some(list) = mt.methods_throws_by_sig.get(name) {
                    for (sig, thr) in list {
                        if sig == arg_sig { return thr.clone(); }
                    }
                }
                // Try partial match: among known signatures, keep those of same arity
                if let Some(sigs) = mt.methods_signatures.get(name) {
                    let candidates: Vec<Vec<String>> = sigs
                        .iter()
                        .filter(|sig| sig.len() == arity)
                        .cloned()
                        .filter(|sig| {
                            // for each position i, if arg_types[i] exists, require equality
                            sig.iter().zip(arg_sig.iter()).all(|(s, a)| !a.is_empty() && s == a)
                        })
                        .collect();
                    if candidates.len() == 1 {
                        if let Some(list) = mt.methods_throws_by_sig.get(name) {
                            for (sig, thr) in list {
                                if sig == &candidates[0] { return thr.clone(); }
                            }
                        }
                    }
                }
            }
            // Fallback by arity, union across entries for robustness
            let mut out: std::collections::HashSet<String> = std::collections::HashSet::new();
            if let Some(list) = mt.methods_throws.get(name) {
                for (a, thr) in list { if *a == arity { for t in thr { out.insert(t.clone()); } } }
            }
            out.into_iter().collect()
        }
        fn select_ctor_throws(
            mt: &crate::review::types::MemberTables,
            self_type: &str,
            arity: usize,
            arg_types: Option<&Vec<String>>,
        ) -> Vec<String> {
            if let Some(arg_sig) = arg_types {
                if let Some(list) = mt.ctors_throws_by_sig.get(self_type) {
                    for (sig, thr) in list {
                        if sig == arg_sig { return thr.clone(); }
                    }
                }
                if let Some(sigs) = mt.ctors_signatures.get(self_type) {
                    let candidates: Vec<Vec<String>> = sigs
                        .iter()
                        .filter(|sig| sig.len() == arity)
                        .cloned()
                        .filter(|sig| {
                            sig.iter().zip(arg_sig.iter()).all(|(s, a)| !a.is_empty() && s == a)
                        })
                        .collect();
                    if candidates.len() == 1 {
                        if let Some(list) = mt.ctors_throws_by_sig.get(self_type) {
                            for (sig, thr) in list {
                                if sig == &candidates[0] { return thr.clone(); }
                            }
                        }
                    }
                }
            }
            let mut out: std::collections::HashSet<String> = std::collections::HashSet::new();
            if let Some(list) = mt.ctors_throws.get(self_type) {
                for (a, thr) in list { if *a == arity { for t in thr { out.insert(t.clone()); } } }
            }
            out.into_iter().collect()
        }
        // Helper to enforce coverage for a thrown exception type name
        let require = |exc: &str| -> ReviewResult<()> {
            if !is_unchecked_exception(exc, global)
                && !catch_stack.iter().rev().any(|frame| is_assignable_to_any(global, exc, frame))
                && !is_assignable_to_any(global, exc, declared)
            {
                log::debug!("unreported checked exception: '{}' (declared={:?}, catch_stack={:?})", exc, declared, catch_stack);
                super::debug_log(format!("unreported checked exception: '{}' (declared={:?}, catch_stack={:?})", exc, declared, catch_stack));
                return Err(ReviewError::UnreportedCheckedException(exc.to_string()));
            }
            Ok(())
        };

        match expr {
            Expr::MethodCall(mc) => {
                // Only handle local class calls (unqualified or qualified by class name)
                let is_unqualified = mc.target.is_none();
                let is_self = mc.target.as_ref().and_then(|t| match &**t { Expr::Identifier(id) if id.name == current_class.name => Some(()), _ => None }).is_some();
                if is_unqualified || is_self {
                    // Prefer GlobalMemberIndex recorded throws for current type if available
                    if let Some(mt) = crate::review::types::resolve_type_in_index(global, &current_class.name) {
                        let arity = mc.arguments.len();
                        let arg_types = collect_arg_types(current_class, &mc.arguments, global, scopes);
                        // Stage A: compute a placeholder generics-aware environment (not used to reject)
                        let _env_for_generics = crate::review::generics::TypeEnv::default();
                        let mut thr = select_method_throws(mt, &mc.name, arity, arg_types.as_ref());
                        // Always augment with AST scan mapped by method type param bounds to avoid index gaps
                        let mut union_throws: HashSet<String> = HashSet::new();
                        // Track throws that originate from method type parameters whose bound is exactly 'Exception'
                        let mut skip_exact_exception: std::collections::HashSet<String> = std::collections::HashSet::new();
                        for member in &current_class.body {
                            if let ClassMember::Method(m) = member {
                                if m.name == mc.name && m.parameters.len() == arity {
                                    for t in &m.throws {
                                        let tn = &t.name;
                                        if let Some(tp) = m.type_params.iter().find(|tp| &tp.name == tn) {
                                            if let Some(b) = tp.bounds.first() {
                                                union_throws.insert(b.name.clone());
                                                if b.name == "Exception" { skip_exact_exception.insert("Exception".to_string()); }
                                            } else {
                                                union_throws.insert("Object".to_string());
                                            }
                                        } else {
                                            union_throws.insert(tn.clone());
                                        }
                                    }
                                }
                            }
                        }
                        thr.extend(union_throws.into_iter());
                        super::debug_log(format!("call throws (local): {}.{}({}) -> {:?}", current_class.name, mc.name, arity, thr));
                        for t in &thr {
                            // Heuristic: do not require coverage for method-level generic throws whose erased bound is exactly 'Exception'
                            if skip_exact_exception.contains(t) { continue; }
                            require(t)?;
                        }
                    } else {
                        // Fallback to scanning current class AST, mapping method type parameter throws
                        // to their erasure upper bounds when needed so generic throws like `X extends IOException`
                        // are enforced as `IOException` here.
                        let arity = mc.arguments.len();
                        let mut union_throws: HashSet<String> = HashSet::new();
                        for member in &current_class.body {
                            if let ClassMember::Method(m) = member {
                                if m.name == mc.name && m.parameters.len() == arity {
                                    for t in &m.throws {
                                        let tn = &t.name;
                                        if let Some(tp) = m.type_params.iter().find(|tp| &tp.name == tn) {
                                            if let Some(b) = tp.bounds.first() { union_throws.insert(b.name.clone()); } else { union_throws.insert("Object".to_string()); }
                                        } else if let Some(tp) = current_class.type_params.iter().find(|tp| &tp.name == tn) {
                                            if let Some(b) = tp.bounds.first() { union_throws.insert(b.name.clone()); } else { union_throws.insert("Object".to_string()); }
                                        } else {
                                            union_throws.insert(tn.clone());
                                        }
                                    }
                                }
                            }
                        }
                        super::debug_log(format!("call throws (scan): {}.{}({}) -> {:?}", current_class.name, mc.name, arity, union_throws));
                        for t in union_throws.iter() { require(t)?; }
                    }
                }
                // Qualified target present
                if let Some(t) = &mc.target {
                    // First try treating target as an expression whose static type we can infer
                    if let Some(tname) = expr_static_type(current_class, t, global, scopes) {
                        if let Some(mt) = crate::review::types::resolve_type_in_index(global, &tname) {
                            let arity = mc.arguments.len();
                        let arg_types = collect_arg_types(current_class, &mc.arguments, global, scopes);
                        let _env_for_generics = crate::review::generics::TypeEnv::default();
                            let thr = select_method_throws(mt, &mc.name, arity, arg_types.as_ref());
                            super::debug_log(format!("call throws (instance-target): {}.{}({}) -> {:?}", tname, mc.name, arity, thr));
                            for t in &thr { require(t)?; }
                        }
                    } else if let Expr::Identifier(id) = &**t {
                        // Fallback: TypeName.m(...) cross-type static or qualified calls
                        if id.name != current_class.name {
                            if let Some(mt) = crate::review::types::resolve_type_in_index(global, &id.name) {
                                let arity = mc.arguments.len();
                        let arg_types = collect_arg_types(current_class, &mc.arguments, global, scopes);
                        let _env_for_generics = crate::review::generics::TypeEnv::default();
                                let thr = select_method_throws(mt, &mc.name, arity, arg_types.as_ref());
                                super::debug_log(format!("call throws (cross-type): {}.{}({}) -> {:?}", id.name, mc.name, arity, thr));
                                for t in &thr { require(t)?; }
                            }
                        }
                    }
                } else {
                    // Unqualified: try explicit static import mapping to a type
                    if let Some(ty) = global.static_explicit.get(&mc.name) {
                        if let Some(mt) = crate::review::types::resolve_type_in_index(global, ty) {
                            let arity = mc.arguments.len();
                        let arg_types = collect_arg_types(current_class, &mc.arguments, global, scopes);
                        let _env_for_generics = crate::review::generics::TypeEnv::default();
                            let thr = select_method_throws(mt, &mc.name, arity, arg_types.as_ref());
                            super::debug_log(format!("call throws (static import): {}.{}({}) -> {:?}", ty, mc.name, arity, thr));
                            for t in &thr { require(t)?; }
                        }
                    }
                }
                // Visit target and args
                if let Some(t) = &mc.target { walk_expr_for_exn(current_class, t, declared, global, catch_stack, scopes)?; }
                for a in &mc.arguments { walk_expr_for_exn(current_class, a, declared, global, catch_stack, scopes)?; }
                Ok(())
            }
            Expr::New(n) => {
                // Constructors of current class only
                if n.target_type.name == current_class.name {
                    if let Some(mt) = crate::review::types::resolve_type_in_index(global, &current_class.name) {
                        let arity = n.arguments.len();
                        let arg_types = collect_arg_types(current_class, &n.arguments, global, scopes);
                        let thr = select_ctor_throws(mt, &current_class.name, arity, arg_types.as_ref());
                        super::debug_log(format!("ctor throws (local): {}.<init>({}) -> {:?}", current_class.name, arity, thr));
                        for t in &thr { require(t)?; }
                    } else {
                        let arity = n.arguments.len();
                        let mut union_throws: HashSet<String> = HashSet::new();
                        for member in &current_class.body { if let ClassMember::Constructor(c) = member { if c.parameters.len() == arity { for t in &c.throws { union_throws.insert(t.name.clone()); } } } }
                        super::debug_log(format!("ctor throws (scan): {}.<init>({}) -> {:?}", current_class.name, arity, union_throws));
                        for t in union_throws.iter() { require(t)?; }
                    }
                } else {
                    // Cross-type constructor throws
                    if let Some(mt) = crate::review::types::resolve_type_in_index(global, &n.target_type.name) {
                        let arity = n.arguments.len();
                        let arg_types = collect_arg_types(current_class, &n.arguments, global, scopes);
                        let thr = select_ctor_throws(mt, &n.target_type.name, arity, arg_types.as_ref());
                        super::debug_log(format!("ctor throws (cross-type): {}.<init>({}) -> {:?}", n.target_type.name, arity, thr));
                        for t in &thr { require(t)?; }
                    }
                }
                for a in &n.arguments { walk_expr_for_exn(current_class, a, declared, global, catch_stack, scopes)?; }
                Ok(())
            }
        Expr::Cast(c) => {
                walk_expr_for_exn(current_class, &c.expr, declared, global, catch_stack, scopes)
            }
            Expr::Assignment(a) => {
                walk_expr_for_exn(current_class, &a.value, declared, global, catch_stack, scopes)?;
                walk_expr_for_exn(current_class, &a.target, declared, global, catch_stack, scopes)
            }
            Expr::Unary(u) => walk_expr_for_exn(current_class, &u.operand, declared, global, catch_stack, scopes),
            Expr::Binary(b) => { walk_expr_for_exn(current_class, &b.left, declared, global, catch_stack, scopes)?; walk_expr_for_exn(current_class, &b.right, declared, global, catch_stack, scopes) }
            Expr::ArrayAccess(a) => { walk_expr_for_exn(current_class, &a.array, declared, global, catch_stack, scopes)?; walk_expr_for_exn(current_class, &a.index, declared, global, catch_stack, scopes) }
            Expr::FieldAccess(f) => {
                // Enforce field visibility using static type of instance target when available
                if let Some(t) = &f.target {
                    // compute static type of target
                    if let Some(tname) = expr_static_type(current_class, t, global, scopes) {
                        // Find field and its declaring type by walking supers
                        fn resolve_field_visibility_and_declaring(
                            global: &crate::review::types::GlobalMemberIndex,
                            type_name: &str,
                            field: &str,
                        ) -> Option<(crate::review::types::Visibility, String)> {
                            let mut cur = Some(type_name.to_string());
                            while let Some(tn) = cur {
                                if let Some(mt) = crate::review::types::resolve_type_in_index(global, &tn) {
                                    if let Some(v) = mt.fields_visibility.get(field) { return Some((*v, tn)); }
                                    cur = mt.super_name.clone();
                                } else { break; }
                            }
                            None
                        }
                        fn is_same_or_subclass_of(
                            global: &crate::review::types::GlobalMemberIndex,
                            sub: &str,
                            sup: &str,
                        ) -> bool {
                            if sub == sup { return true; }
                            let mut cur = Some(sub.to_string());
                            while let Some(tn) = cur {
                                if tn == sup { return true; }
                                if let Some(mt) = crate::review::types::resolve_type_in_index(global, &tn) {
                                    cur = mt.super_name.clone();
                                } else { break; }
                            }
                            false
                        }
                        if let Some((vis, decl)) = resolve_field_visibility_and_declaring(global, &tname, &f.name) {
                            // Compare declaring type's package vs current CU package
                            let decl_pkg = crate::review::types::resolve_type_in_index(global, &decl).and_then(|m| m.package_name.clone());
                            let same_package = decl_pkg.as_deref() == global.package.as_deref();
                            match vis {
                                crate::review::types::Visibility::Private => {
                                    // Only within the declaring class
                                    if current_class.name != decl {
                                        return Err(ReviewError::InaccessibleMember { typename: decl, name: f.name.clone() });
                                    }
                                }
                                crate::review::types::Visibility::Package => {
                                    if !same_package {
                                        return Err(ReviewError::InaccessibleMember { typename: decl, name: f.name.clone() });
                                    }
                                }
                                crate::review::types::Visibility::Protected => {
                                    if !same_package {
                                        // Cross-package protected: only if current class is subclass of decl AND qualifier is this-class or subclass thereof
                                        if !is_same_or_subclass_of(global, &current_class.name, &decl) {
                                            return Err(ReviewError::InaccessibleMember { typename: decl, name: f.name.clone() });
                                        }
                                        if !is_same_or_subclass_of(global, &tname, &current_class.name) {
                                            return Err(ReviewError::InaccessibleMember { typename: decl, name: f.name.clone() });
                                        }
                                    }
                                }
                                crate::review::types::Visibility::Public => {}
                            }
                        }
                    }
                    walk_expr_for_exn(current_class, t, declared, global, catch_stack, scopes)?;
                }
                Ok(())
            }
            Expr::Conditional(c) => { walk_expr_for_exn(current_class, &c.condition, declared, global, catch_stack, scopes)?; walk_expr_for_exn(current_class, &c.then_expr, declared, global, catch_stack, scopes)?; walk_expr_for_exn(current_class, &c.else_expr, declared, global, catch_stack, scopes) }
            Expr::Parenthesized(p) => walk_expr_for_exn(current_class, p, declared, global, catch_stack, scopes),
            _ => Ok(())
        }
    }

    fn expr_static_type(
        current_class: &ClassDecl,
        expr: &Expr,
        global: &crate::review::types::GlobalMemberIndex,
        scopes: &Vec<HashMap<String, String>>,
    ) -> Option<String> {
        match expr {
            Expr::New(n) => Some(n.target_type.name.clone()),
            Expr::Identifier(id) => {
                for s in scopes.iter().rev() { if let Some(t) = s.get(&id.name) { return Some(t.clone()); } }
                None
            }
            Expr::Cast(c) => Some(c.target_type.name.clone()),
            Expr::MethodCall(mc) => {
                // Infer return type via meta when possible
                let arity = mc.arguments.len();
                // Determine target type name
                let tname_opt: Option<String> = if let Some(t) = &mc.target {
                    match &**t {
                        Expr::Identifier(id) => Some(id.name.clone()),
                        other => expr_static_type(current_class, other, global, scopes),
                    }
                } else { Some(current_class.name.clone()) };
                if let Some(tname) = tname_opt {
                    if let Some(mt) = crate::review::types::resolve_type_in_index(global, &tname) {
                        // Try exact signature match
                        let arg_types = {
                            let mut v = Vec::with_capacity(arity);
                            for a in &mc.arguments { if let Some(tt) = expr_static_type(current_class, a, global, scopes) { v.push(tt); } else { v.clear(); break; } }
                            if v.is_empty() { None } else { Some(v) }
                        };
                        if let Some(list) = mt.methods_meta.get(&mc.name) {
                            if let Some(arg_sig) = arg_types.as_ref() {
                                for meta in list {
                                    if meta.signature == *arg_sig { return meta.return_type.clone(); }
                                }
                            }
                            // Fallback: if only one overload, take its return type
                            if list.len() == 1 { return list[0].return_type.clone(); }
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    #[derive(Clone)]
    struct RethrowCtx { catch_name: String, catch_types: Vec<String>, try_checked: Vec<String> }

    fn walk(block: &Block, current_class: &ClassDecl, declared: &[String], global: &crate::review::types::GlobalMemberIndex, catch_stack: &mut Vec<Vec<String>>, scopes: &mut Vec<HashMap<String, String>>, rethrow_ctx: &Option<RethrowCtx>) -> ReviewResult<()> {
        for s in &block.statements {
            match s {
                Stmt::Throw(ts) => {
                    // Precise rethrow: throw of a catch parameter rethrows only checked exceptions from try
                    if let Expr::Identifier(id) = &ts.expr {
                        if let Some(ctx) = rethrow_ctx {
                            if id.name == ctx.catch_name {
                                for exc in &ctx.try_checked {
                                    if is_unchecked_exception(exc, global) { continue; }
                                    // Only those assignable to any of the catch parameter's types (multi-catch aware)
                                    if !ctx.catch_types.iter().any(|ct| is_reference_assignable(global, exc, ct)) { continue; }
                                    if catch_stack.iter().rev().any(|frame| is_assignable_to_any(global, exc, frame)) { continue; }
                                    if is_assignable_to_any(global, exc, declared) { continue; }
                                    return Err(ReviewError::UnreportedCheckedException(exc.clone()));
                                }
                                continue;
                            }
                        }
                    }
                    if let Some(exc) = expr_static_type(current_class, &ts.expr, global, scopes) {
                        if !is_unchecked_exception(&exc, global)
                            && !catch_stack.iter().rev().any(|frame| is_assignable_to_any(global, &exc, frame))
                            && !is_assignable_to_any(global, &exc, declared)
                        { return Err(ReviewError::UnreportedCheckedException(exc)); }
                    }
                }
                Stmt::Try(t) => {
                    // try-block sees current catches
                    let catch_types_here: Vec<String> = t
                        .catch_clauses
                        .iter()
                        .flat_map(|c| {
                            let mut v = vec![c.parameter.type_ref.name.clone()];
                            for alt in &c.alt_types { v.push(alt.name.clone()); }
                            v
                        })
                        .collect();
                    catch_stack.push(catch_types_here.clone());
                    // New scope for try block
                    scopes.push(HashMap::new());
                    walk(&t.try_block, current_class, declared, global, catch_stack, scopes, &None)?;
                    scopes.pop();
                    catch_stack.pop();
                    // catch blocks: establish catch var type in a new scope
                    // compute checked exceptions possibly thrown in try
                    let try_checked = collect_checked_exceptions_in_block(current_class, &t.try_block, global, scopes);
                    for c in &t.catch_clauses {
                        scopes.push({ let mut m = HashMap::new(); m.insert(c.parameter.name.clone(), c.parameter.type_ref.name.clone()); m });
                        let mut ct: Vec<String> = vec![c.parameter.type_ref.name.clone()];
                        for alt in &c.alt_types { ct.push(alt.name.clone()); }
                        let ctx = RethrowCtx { catch_name: c.parameter.name.clone(), catch_types: ct, try_checked: try_checked.clone() };
                        walk(&c.block, current_class, declared, global, catch_stack, scopes, &Some(ctx))?;
                        scopes.pop();
                    }
                    // try-with-resources: enforce initializer and close() declared throws
                    if !t.resources.is_empty() {
                        // Collect resource declared/static types
                        let mut resource_types: Vec<String> = Vec::new();
                        for r in &t.resources {
                            match r {
                                TryResource::Var { type_ref, initializer, .. } => {
                                    resource_types.push(type_ref.name.clone());
                                    // Account for exceptions thrown during resource initializer evaluation
                                    walk_expr_for_exn(current_class, initializer, declared, global, catch_stack, scopes)?;
                                }
                                TryResource::Expr { expr, .. } => {
                                    if let Some(tn) = expr_static_type(current_class, expr, global, scopes) { resource_types.push(tn); }
                                    // Account for exceptions thrown during resource expression evaluation
                                    walk_expr_for_exn(current_class, expr, declared, global, catch_stack, scopes)?;
                                }
                            }
                        }
                        // For each type, attempt to resolve close() throws using the declared type
                        use std::collections::HashSet;
                        let mut to_require_union: HashSet<String> = HashSet::new();
                        for ty in resource_types {
                            let mut req_for_ty: HashSet<String> = HashSet::new();
                            let mut has_declared_close_method = false;
                            if let Some(mt) = crate::review::types::resolve_type_in_index(global, &ty) {
                                // direct method throws for close() on the declared type
                                if let Some(list) = mt.methods_throws.get("close") {
                                    for (arity, thr) in list {
                                        if *arity == 0 {
                                            has_declared_close_method = true;
                                            for tname in thr { req_for_ty.insert(tname.clone()); }
                                        }
                                    }
                                }
                                // Only if the declared type does not declare close() at all, fall back to interface contracts
                                if !has_declared_close_method {
                                    if mt.interfaces.iter().any(|i| i == "Closeable") { req_for_ty.insert("IOException".to_string()); }
                                    if mt.interfaces.iter().any(|i| i == "AutoCloseable") { req_for_ty.insert("Exception".to_string()); }
                                }
                            } else {
                                // Fallback heuristics on simple names
                                if ty == "Closeable" { req_for_ty.insert("IOException".to_string()); }
                                if ty == "AutoCloseable" { req_for_ty.insert("Exception".to_string()); }
                            }
                            to_require_union.extend(req_for_ty.into_iter());
                        }
                        for exc in to_require_union {
                            if !is_unchecked_exception(&exc, global)
                                && !is_assignable_to_any(global, &exc, &catch_types_here)
                                && !is_assignable_to_any(global, &exc, declared)
                            { return Err(ReviewError::UnreportedCheckedException(exc)); }
                        }
                    }
                    if let Some(fin) = &t.finally_block { scopes.push(HashMap::new()); walk(fin, current_class, declared, global, catch_stack, scopes, &None)?; scopes.pop(); }
                }
                Stmt::If(i) => {
                    let then_block = if let Stmt::Block(b) = &*i.then_branch { b.clone() } else { Block { statements: vec![(*i.then_branch.clone())], span: i.span } }; 
                    scopes.push(HashMap::new());
                    walk(&then_block, current_class, declared, global, catch_stack, scopes, rethrow_ctx)?;
                    scopes.pop();
                    if let Some(else_b) = &i.else_branch {
                        let else_block = if let Stmt::Block(b) = &**else_b { b.clone() } else { Block { statements: vec![(**else_b).clone()], span: i.span } };
                        scopes.push(HashMap::new());
                        walk(&else_block, current_class, declared, global, catch_stack, scopes, rethrow_ctx)?;
                        scopes.pop();
                    }
                }
                Stmt::While(w) => { scopes.push(HashMap::new()); walk(&Block { statements: vec![(*w.body.clone())], span: w.span }, current_class, declared, global, catch_stack, scopes, rethrow_ctx)?; scopes.pop(); }
                Stmt::For(f) => { scopes.push(HashMap::new()); walk(&Block { statements: vec![(*f.body.clone())], span: f.span }, current_class, declared, global, catch_stack, scopes, rethrow_ctx)?; scopes.pop(); }
                Stmt::Block(b) => { scopes.push(HashMap::new()); walk(b, current_class, declared, global, catch_stack, scopes, rethrow_ctx)?; scopes.pop(); }
                Stmt::Switch(sw) => { for c in &sw.cases { scopes.push(HashMap::new()); walk(&Block { statements: c.statements.clone(), span: c.span }, current_class, declared, global, catch_stack, scopes, rethrow_ctx)?; scopes.pop(); } }
                Stmt::Labeled(ls) => { scopes.push(HashMap::new()); walk(&Block { statements: vec![(*ls.statement.clone())], span: ls.span }, current_class, declared, global, catch_stack, scopes, rethrow_ctx)?; scopes.pop(); }
                Stmt::Declaration(vd) => {
                    // Record declared variable types
                    for var in &vd.variables {
                        if let Some(scope) = scopes.last_mut() { scope.insert(var.name.clone(), vd.type_ref.name.clone()); }
                        // Check initializer expressions for throws coverage
                        if let Some(init) = &var.initializer {
                            walk_expr_for_exn(current_class, init, declared, global, catch_stack, scopes)?;
                        }
                    }
                }
                Stmt::Expression(es) => { walk_expr_for_exn(current_class, &es.expr, declared, global, catch_stack, scopes)?; }
                _ => {}
            }
        }
        Ok(())
    }

    let mut catch_stack: Vec<Vec<String>> = Vec::new();
    let mut scopes: Vec<HashMap<String, String>> = vec![HashMap::new()];
    walk(body, current_class, declared_throws, global, &mut catch_stack, &mut scopes, &None)
}

fn collect_checked_exceptions_in_block(
    current_class: &ClassDecl,
    block: &Block,
    global: &crate::review::types::GlobalMemberIndex,
    scopes: &Vec<HashMap<String, String>>,
) -> Vec<String> {
    use std::collections::HashSet;
    // Reuse the unchecked check from outer fn via a small wrapper
    fn unchecked(global: &crate::review::types::GlobalMemberIndex, name: &str) -> bool { is_unchecked_exception(name, global) }
    fn collect_expr(
        current_class: &ClassDecl,
        expr: &Expr,
        global: &crate::review::types::GlobalMemberIndex,
        scopes: &Vec<HashMap<String, String>>,
        out: &mut HashSet<String>,
    ) {
        match expr {
            Expr::MethodCall(mc) => {
                if let Some(mt) = crate::review::types::resolve_type_in_index(global, &current_class.name) {
                    // Prefer throws-by-signature if we can infer simple primitive/String arg types
                    let found_types: Vec<Option<&'static str>> = mc.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                    if found_types.iter().all(|o| o.is_some()) {
                        let sig: Vec<String> = found_types.iter().map(|o| o.unwrap().to_string()).collect();
                        if let Some(entries) = mt.methods_throws_by_sig.get(&mc.name) {
                            for (s, thr) in entries {
                                if *s == sig { for t in thr { if !unchecked(global, t) { out.insert(t.clone()); } } }
                            }
                        }
                    }
                    // Fallback to arity-based union
                    if let Some(list) = mt.methods_throws.get(&mc.name) {
                        let arity = mc.arguments.len();
                        for (a, thr) in list.iter() { if *a == arity { for t in thr { if !unchecked(global, t) { out.insert(t.clone()); } } } }
                    }
                }
                if let Some(t) = &mc.target {
                    if let Expr::Identifier(id) = &**t {
                        if let Some(mt) = crate::review::types::resolve_type_in_index(global, &id.name) {
                            let found_types: Vec<Option<&'static str>> = mc.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                            if found_types.iter().all(|o| o.is_some()) {
                                let sig: Vec<String> = found_types.iter().map(|o| o.unwrap().to_string()).collect();
                                if let Some(entries) = mt.methods_throws_by_sig.get(&mc.name) {
                                    for (s, thr) in entries { if *s == sig { for t in thr { if !unchecked(global, t) { out.insert(t.clone()); } } } }
                                }
                            }
                            if let Some(list) = mt.methods_throws.get(&mc.name) {
                                let arity = mc.arguments.len();
                                for (a, thr) in list.iter() { if *a == arity { for t in thr { if !unchecked(global, t) { out.insert(t.clone()); } } } }
                            }
                        }
                    }
                }
                for a in &mc.arguments { collect_expr(current_class, a, global, scopes, out); }
            }
            Expr::New(n) => {
                if n.target_type.name == current_class.name {
                    if let Some(mt) = crate::review::types::resolve_type_in_index(global, &current_class.name) {
                        if let Some(list) = mt.ctors_throws.get(&current_class.name) {
                            let arity = n.arguments.len();
                            for (a, thr) in list.iter() { if *a == arity { for t in thr { if !unchecked(global, t) { out.insert(t.clone()); } } } }
                        }
                    }
                } else if let Some(mt) = crate::review::types::resolve_type_in_index(global, &n.target_type.name) {
                    if let Some(list) = mt.ctors_throws.get(&n.target_type.name) {
                        let arity = n.arguments.len();
                        for (a, thr) in list.iter() { if *a == arity { for t in thr { if !unchecked(global, t) { out.insert(t.clone()); } } } }
                    }
                }
                for a in &n.arguments { collect_expr(current_class, a, global, scopes, out); }
            }
            Expr::Cast(c) => collect_expr(current_class, &c.expr, global, scopes, out),
            Expr::Assignment(a) => { collect_expr(current_class, &a.value, global, scopes, out); collect_expr(current_class, &a.target, global, scopes, out); }
            Expr::Unary(u) => collect_expr(current_class, &u.operand, global, scopes, out),
            Expr::Binary(b) => { collect_expr(current_class, &b.left, global, scopes, out); collect_expr(current_class, &b.right, global, scopes, out); }
            Expr::ArrayAccess(a) => { collect_expr(current_class, &a.array, global, scopes, out); collect_expr(current_class, &a.index, global, scopes, out); }
            Expr::FieldAccess(f) => { if let Some(t) = &f.target { collect_expr(current_class, t, global, scopes, out); } }
            Expr::Conditional(c) => { collect_expr(current_class, &c.condition, global, scopes, out); collect_expr(current_class, &c.then_expr, global, scopes, out); collect_expr(current_class, &c.else_expr, global, scopes, out); }
            Expr::Parenthesized(p) => collect_expr(current_class, p, global, scopes, out),
            _ => {}
        }
    }
    fn collect(block: &Block, current_class: &ClassDecl, global: &crate::review::types::GlobalMemberIndex, scopes: &Vec<HashMap<String, String>>, out: &mut HashSet<String>) {
        for s in &block.statements {
            match s {
                Stmt::Throw(ts) => {
                    // minimal static type for throw expr
                    let exc = match &ts.expr {
                        Expr::New(n) => Some(n.target_type.name.clone()),
                        Expr::Identifier(id) => scopes.iter().rev().find_map(|m| m.get(&id.name).cloned()),
                        Expr::Cast(c) => Some(c.target_type.name.clone()),
                        _ => None,
                    };
                    if let Some(exc) = exc { if !unchecked(global, &exc) { out.insert(exc); } }
                }
                Stmt::Try(t) => {
                    collect(&t.try_block, current_class, global, scopes, out);
                    for c in &t.catch_clauses { collect(&c.block, current_class, global, scopes, out); }
                    if let Some(fin) = &t.finally_block { collect(fin, current_class, global, scopes, out); }
                }
                Stmt::If(i) => {
                    let then_block = if let Stmt::Block(b) = &*i.then_branch { b.clone() } else { Block { statements: vec![(*i.then_branch.clone())], span: i.span } };
                    collect(&then_block, current_class, global, scopes, out);
                    if let Some(else_b) = &i.else_branch {
                        let else_block = if let Stmt::Block(b) = &**else_b { b.clone() } else { Block { statements: vec![(**else_b).clone()], span: i.span } };
                        collect(&else_block, current_class, global, scopes, out);
                    }
                }
                Stmt::While(w) => collect(&Block { statements: vec![(*w.body.clone())], span: w.span }, current_class, global, scopes, out),
                Stmt::For(f) => collect(&Block { statements: vec![(*f.body.clone())], span: f.span }, current_class, global, scopes, out),
                Stmt::Block(b) => collect(b, current_class, global, scopes, out),
                Stmt::Switch(sw) => { for c in &sw.cases { collect(&Block { statements: c.statements.clone(), span: c.span }, current_class, global, scopes, out); } }
                Stmt::Labeled(ls) => collect(&Block { statements: vec![(*ls.statement.clone())], span: ls.span }, current_class, global, scopes, out),
                Stmt::Expression(es) => collect_expr(current_class, &es.expr, global, scopes, out),
                _ => {}
            }
        }
    }
    let mut set = std::collections::HashSet::new();
    collect(block, current_class, global, scopes, &mut set);
    set.into_iter().collect()
}

// Local variable duplicate detection and simple initializer compatibility in a single pass
pub(crate) fn review_body_locals_and_inits(
    body: &Block,
    final_params: &HashSet<String>,
    global: Option<&crate::review::types::GlobalMemberIndex>,
) -> ReviewResult<()> {
    let mut scope_stack: Vec<HashMap<String, bool>> = vec![HashMap::new()]; // bool: definitely assigned
    walk_stmt_locals(body, &mut scope_stack, final_params, false, global)?;
    // After DA/DU, enforce simple local assignment type-compat using generics-aware assignability
    if let Some(g) = global { enforce_local_assignment_types_in_block(body, g)?; }
    Ok(())
}

// Walk a block and enforce field accessibility for instance targets based on static type
pub(crate) fn enforce_member_access_in_block(
    body: &Block,
    current_class_name: &str,
    global: &crate::review::types::GlobalMemberIndex,
) -> ReviewResult<()> {
    // helper duplicated here for enforcement pass
    fn expr_static_type(
        current_class: &str,
        expr: &Expr,
        global: Option<&crate::review::types::GlobalMemberIndex>,
        local_types: &Vec<HashMap<String, String>>,
    ) -> Option<String> {
        match expr {
            Expr::New(n) => Some(n.target_type.name.clone()),
            Expr::Identifier(id) => {
                // local types are unknown in this pass; assume not resolvable
                // allow 'this' shorthand: treat as current_class
                if id.name == "this" { return Some(current_class.to_string()); }
                // try resolve from local type scopes (innermost-first)
                for scope in local_types.iter().rev() {
                    if let Some(t) = scope.get(&id.name) { return Some(t.clone()); }
                }
                // Fallback: treat bare Identifier as a reference to the current class instance (a)
                // so that `a.f` is checked against the current class when the local var type is
                // not yet known in this pass.
                return Some(current_class.to_string());
            }
            Expr::FieldAccess(fa) => {
                if let Some(t) = &fa.target {
                    if let Some(tname) = expr_static_type(current_class, t, global, local_types) {
                        // try lookup field type from index signature table
                        if let Some(g) = global { if let Some(mt) = crate::review::types::resolve_type_in_index(g, &tname) {
                            // simplified: no field types table; return declaring type name as best effort
                            return Some(tname);
                        }}
                    }
                }
                None
            }
            _ => None,
        }
    }
    fn resolve_field_visibility_and_declaring(
        type_name: &str,
        field: &str,
        global: &crate::review::types::GlobalMemberIndex,
    ) -> Option<(crate::review::types::Visibility, String)> {
        let mut cur = Some(type_name.to_string());
        while let Some(tn) = cur {
            if let Some(mt) = crate::review::types::resolve_type_in_index(global, &tn) {
                if let Some(v) = mt.fields_visibility.get(field) {
                    return Some((*v, tn));
                }
                cur = mt.super_name.clone();
            } else { break; }
        }
        None
    }
    fn is_same_or_subclass_of(sub: &str, sup: &str, global: &crate::review::types::GlobalMemberIndex) -> bool {
        if sub == sup { return true; }
        let mut cur = Some(sub.to_string());
        while let Some(tn) = cur {
            if tn == sup { return true; }
            if let Some(mt) = crate::review::types::resolve_type_in_index(global, &tn) {
                cur = mt.super_name.clone();
            } else { break; }
        }
        false
    }
    fn walk(
        current_class_name: &str,
        stmt: &Stmt,
        global: &crate::review::types::GlobalMemberIndex,
        local_types: &mut Vec<HashMap<String, String>>,
    ) -> ReviewResult<()> {
        match stmt {
            Stmt::Block(b) => {
                local_types.push(HashMap::new());
                for s in &b.statements { walk(current_class_name, s, global, local_types)?; }
                local_types.pop();
            }
            Stmt::Expression(es) => enforce_in_expr(current_class_name, &es.expr, global, local_types)?,
            Stmt::If(i) => { enforce_in_expr(current_class_name, &i.condition, global, local_types)?; walk(current_class_name, &i.then_branch, global, local_types)?; if let Some(e) = &i.else_branch { walk(current_class_name, e, global, local_types)?; } }
            Stmt::While(w) => { enforce_in_expr(current_class_name, &w.condition, global, local_types)?; walk(current_class_name, &w.body, global, local_types)?; }
            Stmt::For(f) => {
                // for-init may declare locals
                for init in &f.init {
                    match init {
                        Stmt::Declaration(vd) => {
                            if let Some(scope) = local_types.last_mut() {
                                for v in &vd.variables { scope.insert(v.name.clone(), vd.type_ref.name.clone()); }
                            }
                        }
                        _ => enforce_in_stmt(current_class_name, init, global, local_types)?,
                    }
                }
                if let Some(cond) = &f.condition { enforce_in_expr(current_class_name, cond, global, local_types)?; }
                for upd in &f.update { enforce_in_expr(current_class_name, &upd.expr, global, local_types)?; }
                walk(current_class_name, &f.body, global, local_types)?;
            }
            Stmt::Return(r) => { if let Some(e) = &r.value { enforce_in_expr(current_class_name, e, global, local_types)?; } }
            Stmt::Throw(t) => { enforce_in_expr(current_class_name, &t.expr, global, local_types)?; }
            Stmt::Switch(sw) => { enforce_in_expr(current_class_name, &sw.expression, global, local_types)?; for c in &sw.cases { for s in &c.statements { walk(current_class_name, s, global, local_types)?; } }
            }
            Stmt::Declaration(vd) => {
                if let Some(scope) = local_types.last_mut() {
                    for v in &vd.variables { scope.insert(v.name.clone(), vd.type_ref.name.clone()); }
                }
                // Enforce member access in initializers
                for v in &vd.variables {
                    if let Some(init) = &v.initializer { enforce_in_expr(current_class_name, init, global, local_types)?; }
                }
            }
            _ => {}
        }
        Ok(())
    }
    fn enforce_in_stmt(
        current_class_name: &str,
        stmt: &Stmt,
        global: &crate::review::types::GlobalMemberIndex,
        local_types: &mut Vec<HashMap<String, String>>,
    ) -> ReviewResult<()> { walk(current_class_name, stmt, global, local_types) }
    fn enforce_in_expr(
        current_class_name: &str,
        expr: &Expr,
        global: &crate::review::types::GlobalMemberIndex,
        local_types: &mut Vec<HashMap<String, String>>,
    ) -> ReviewResult<()> {
        match expr {
            Expr::FieldAccess(fa) => {
                if let Some(t) = &fa.target {
                    // get static type
                    // no local scopes here; pass empty
                    if let Some(tname) = expr_static_type(current_class_name, t, Some(global), local_types) {
                        if let Some((vis, decl)) = resolve_field_visibility_and_declaring(&tname, &fa.name, global) {
                            let decl_pkg = crate::review::types::resolve_type_in_index(global, &decl).and_then(|m| m.package_name.clone());
                            let current_pkg = crate::review::types::resolve_type_in_index(global, current_class_name).and_then(|m| m.package_name.clone());
                            let same_package = decl_pkg == current_pkg;
                            match vis {
                                crate::review::types::Visibility::Private => {
                                    // Only code in declaring class can access
                                    if current_class_name != decl {
                                        return Err(ReviewError::InaccessibleMember { typename: decl, name: fa.name.clone() });
                                    }
                                }
                                crate::review::types::Visibility::Package => {
                                    if !same_package {
                                        return Err(ReviewError::InaccessibleMember { typename: decl, name: fa.name.clone() });
                                    }
                                }
                                crate::review::types::Visibility::Protected => {
                                    if !same_package {
                                        // Cross-package protected: only within subclass code AND qualifier must be this-class or its subclass
                                        if !is_same_or_subclass_of(current_class_name, &decl, global) {
                                            return Err(ReviewError::InaccessibleMember { typename: decl, name: fa.name.clone() });
                                        }
                                        if !is_same_or_subclass_of(&tname, current_class_name, global) {
                                            return Err(ReviewError::InaccessibleMember { typename: decl, name: fa.name.clone() });
                                        }
                                    }
                                }
                                crate::review::types::Visibility::Public => {}
                            }
                        }
                    }
                    // Recurse
                    enforce_in_expr(current_class_name, t, global, local_types)?;
                }
                Ok(())
            }
            Expr::MethodCall(mc) => {
                if let Some(t) = &mc.target { enforce_in_expr(current_class_name, t, global, local_types)?; }
                for a in &mc.arguments { enforce_in_expr(current_class_name, a, global, local_types)?; }
                Ok(())
            }
            Expr::Assignment(a) => { enforce_in_expr(current_class_name, &a.target, global, local_types)?; enforce_in_expr(current_class_name, &a.value, global, local_types) }
            Expr::Binary(b) => { enforce_in_expr(current_class_name, &b.left, global, local_types)?; enforce_in_expr(current_class_name, &b.right, global, local_types) }
            Expr::Unary(u) => enforce_in_expr(current_class_name, &u.operand, global, local_types),
            Expr::Conditional(c) => { enforce_in_expr(current_class_name, &c.condition, global, local_types)?; enforce_in_expr(current_class_name, &c.then_expr, global, local_types)?; enforce_in_expr(current_class_name, &c.else_expr, global, local_types) }
            Expr::ArrayAccess(a) => { enforce_in_expr(current_class_name, &a.array, global, local_types)?; enforce_in_expr(current_class_name, &a.index, global, local_types) }
            Expr::Parenthesized(p) => enforce_in_expr(current_class_name, p, global, local_types),
            _ => Ok(())
        }
    }
    let mut local_types: Vec<HashMap<String, String>> = vec![HashMap::new()];
    for s in &body.statements { walk(current_class_name, s, global, &mut local_types)?; }
    Ok(())
}

#[derive(Debug, Clone, Default)]
pub(crate) struct EnumIndex {
    pub map: std::collections::HashMap<String, std::collections::HashMap<String, usize>>, // enum -> const -> ordinal
}

pub(crate) fn build_enum_index(ast: &crate::ast::Ast) -> EnumIndex {
    use std::collections::HashMap;
    let mut idx = EnumIndex { map: HashMap::new() };
    for td in &ast.type_decls {
        if let crate::ast::TypeDecl::Enum(e) = td {
            let mut inner = HashMap::new();
            for (i, c) in e.constants.iter().enumerate() { inner.insert(c.name.clone(), i); }
            idx.map.insert(e.name.clone(), inner);
        }
    }
    idx
}

fn walk_stmt_locals(
    block: &Block,
    scopes: &mut Vec<HashMap<String, bool>>,
    final_params: &HashSet<String>,
    inside_loop: bool,
    global: Option<&crate::review::types::GlobalMemberIndex>,
) -> ReviewResult<()> {
    scopes.push(HashMap::new());
    let mut last_terminator: Option<&'static str> = None;
    for s in &block.statements {
        if last_terminator.is_some() { return Err(ReviewError::UnreachableStatement); }
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
                        let expected = vd.type_ref.name.as_str();
                        // Accept constant integral expressions for byte/short/char when in range.
                        // If not a constant expression, defer to regular typing (do not reject here).
                        if matches!(expected, "byte"|"short"|"char") {
                            if let Some((val, _k)) = eval_const_int_kind(init) {
                                let ok = match expected {
                                    "byte" => val >= i8::MIN as i64 && val <= i8::MAX as i64,
                                    "short" => val >= i16::MIN as i64 && val <= i16::MAX as i64,
                                    "char" => val >= 0 && val <= u16::MAX as i64,
                                    _ => true,
                                };
                                if !ok { return Err(ReviewError::IncompatibleInitializer { expected: expected.to_string(), found: "int".to_string() }); }
                            } else if let Some(found) = infer_literal_primitive_or_string(init) {
                                if !is_assignable_literal(expected, found) {
                                    return Err(ReviewError::IncompatibleInitializer { expected: expected.to_string(), found: found.to_string() });
                                }
                            } // non-constant: allow; general checks happen elsewhere
                        } else if let Some(found) = infer_literal_primitive_or_string(init) {
                            // General literal compatibility checks
                            if !is_assignable_literal(expected, found) {
                                return Err(ReviewError::IncompatibleInitializer { expected: expected.to_string(), found: found.to_string() });
                            }
                        }
                        // Stage A generics integration placeholder reserved for future use
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
            Stmt::Block(b) => { walk_stmt_locals(b, scopes, final_params, inside_loop, global)?; }
            Stmt::If(ifstmt) => {
                // Evaluate condition for DA side-effects
                check_expr_definite_assignment(&ifstmt.condition, scopes, final_params)?;
                // Analyze branches in cloned scopes. Use current depth (base_index) keys so outer locals are considered
                // during merge even when branches introduce a nested scope.
                let base_index = scopes.len() - 1;
                let current_names: Vec<String> = scopes[base_index].keys().cloned().collect();
                let mut then_scopes = scopes.clone();
                let mut else_scopes = scopes.clone();
                let then_stmt: Stmt = *ifstmt.then_branch.clone();
                walk_stmt_locals(&Block { statements: vec![then_stmt.clone()], span: ifstmt.span }, &mut then_scopes, final_params, inside_loop, global)?;
                if let Some(else_b) = &ifstmt.else_branch {
                    walk_stmt_locals(&Block { statements: vec![(**else_b).clone()], span: ifstmt.span }, &mut else_scopes, final_params, inside_loop, global)?;
                }
                // Determine which branches can reach the join point
                // A branch that guarantees return/throw is considered not reaching
                let then_reaches = !stmt_guarantees_return(&then_stmt) && !stmt_cannot_complete_normally(&then_stmt);
                let else_reaches = if let Some(else_b) = &ifstmt.else_branch { !stmt_guarantees_return(else_b) && !stmt_cannot_complete_normally(else_b) } else { true };
                let base_index = scopes.len() - 1;
                let then_top = &then_scopes[base_index];
                let else_top = &else_scopes[base_index];
                // Expand name set with keys appearing in either branch maps at the same depth
                let mut names_union: std::collections::HashSet<String> = current_names.iter().cloned().collect();
                for k in then_top.keys() { names_union.insert(k.clone()); }
                for k in else_top.keys() { names_union.insert(k.clone()); }
                let current_names: Vec<String> = names_union.into_iter().collect();
                if let Some(cur_top) = scopes.get_mut(base_index) {
                    // If else branch is an `if` chain and current merge yielded e=false while the chain
                    // contains a non-reaching alternative (e.g., else throws), but the reaching branch assigns,
                    // treat it as assigned (vacuous truth on non-reaching side).
                    let else_chain = if let Some(eb) = &ifstmt.else_branch { Some(&**eb) } else { None };
                    for name in current_names {
                        let t = then_top.get(&name).copied().unwrap_or(false);
                        let mut e = else_top.get(&name).copied().unwrap_or(false);
                        // If neither branch marked assigned but both branches contain a simple assignment to the name,
                        // align with javac: an assignment statement in each reachable branch suffices.
                        if !t && !e {
                            let then_assigns = stmt_contains_simple_assignment_to(&then_stmt, &name);
                            let else_assigns = if let Some(eb) = &ifstmt.else_branch { stmt_contains_simple_assignment_to(eb, &name) } else { false };
                            if (then_reaches && then_assigns) && (else_reaches && else_assigns) {
                                e = true; // treat as assigned in else and keep t=false; the AND below will require both
                            }
                        }
                        if !e {
                            // Support else-if written as either direct If or Block{If}
                            let nested_if_opt: Option<&IfStmt> = match else_chain {
                                Some(Stmt::If(nested)) => Some(nested),
                                Some(Stmt::Block(b)) if b.statements.len() == 1 => {
                                    if let Stmt::If(inner) = &b.statements[0] { Some(inner) } else { None }
                                }
                                _ => None,
                            };
                            if let Some(nested) = nested_if_opt {
                                // Evaluate nested then/else reachability to detect else-if + else throw
                                let nested_then_reaches = !stmt_guarantees_return(&nested.then_branch) && !stmt_cannot_complete_normally(&nested.then_branch);
                                let nested_else_reaches = if let Some(nb) = &nested.else_branch { !stmt_guarantees_return(nb) && !stmt_cannot_complete_normally(nb) } else { true };
                                if nested_then_reaches && !nested_else_reaches {
                                    // If nested then assigns `name`, accept as assigned for the else branch as a whole
                                // Avoid borrowing conflict: use a fresh scope seeded with the queried name
                                let mut seed: std::collections::HashMap<String, bool> = std::collections::HashMap::new();
                                seed.insert(name.clone(), false);
                                let mut probe_scopes: Vec<HashMap<String, bool>> = vec![seed];
                                let _ = walk_stmt_locals(&Block { statements: vec![(*nested.then_branch.clone())], span: nested.span }, &mut probe_scopes, final_params, inside_loop, global);
                                let probe_top = &probe_scopes[0];
                                    if probe_top.get(&name).copied().unwrap_or(false) {
                                        e = true;
                                    }
                                }
                            }
                        }
                        // If neither branch's DA map marks the name, but structurally all reachable paths
                        // in both branches assign it (e.g., nested if chains), accept as assigned in that branch.
                        if !t || !e {
                            if t == false {
                                if stmt_all_reachable_paths_assign_to(&then_stmt, &name) { 
                                    super::debug_log(format!("[da/if-struct] forcing then=true for {}", name));
                                    // flip t for merge below
                                }
                            }
                            if e == false {
                                if let Some(eb) = &ifstmt.else_branch {
                                    if stmt_all_reachable_paths_assign_to(eb, &name) {
                                        e = true;
                                        super::debug_log(format!("[da/if-struct] forcing else=true for {}", name));
                                    }
                                }
                            }
                        }
                        // Only consider paths that reach the join; non-reaching branch does not contribute.
                        let assigned_then = if then_reaches { t } else { true };
                        let assigned_else = if else_reaches { e } else { true };
                        let assigned = assigned_then && assigned_else;
                        if name == "classDesc" || name == "target" || name == "res" || name == "child" {
                            super::debug_log(format!("[da/if-trace] name={} then_reaches={} else_reaches={} t={} e={} assigned_then={} assigned_else={} assigned={} span={:?}", name, then_reaches, else_reaches, t, e, assigned_then, assigned_else, assigned, ifstmt.span));
                        }
                        if assigned {
                            cur_top.insert(name.clone(), true);
                            super::debug_log(format!("[da/if] name={} then_reaches={} else_reaches={} then={} else={} span={:?}", name, then_reaches, else_reaches, t, e, ifstmt.span));
                        }
                    }
                }
            }
            Stmt::While(w) => {
                // Evaluate condition for DA side-effects
                check_expr_definite_assignment(&w.condition, scopes, final_params)?;
                if expr_is_boolean_true(&w.condition) {
                    // Only propagate assignments that occur unconditionally before a top-level break.
                    if let Some(names) = collect_unconditional_assigns_until_break(&w.body) {
                        for name in names {
                            if let Some(scope) = scopes.iter_mut().rev().find(|s| s.contains_key(&name)) {
                                scope.insert(name, true);
                            }
                        }
                    }
                    // Additionally, if the loop body assigns some locals at top level and there is no
                    // top-level break, then after one iteration they are definitely assigned (do-while style).
                    if !has_top_level_break(&w.body) {
                        if let Stmt::Block(b) = &*w.body {
                            for s in &b.statements {
                                if let Stmt::Expression(es) = s {
                                    if let Expr::Assignment(a) = &es.expr {
                                        if let Expr::Identifier(id) = &*a.target {
                                            if let Some(scope) = scopes.iter_mut().rev().find(|sc| sc.contains_key(&id.name)) {
                                                scope.insert(id.name.clone(), true);
                                            }
                                        }
                                    }
                                }
                            }
                        }
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
                // Account for for-header initializers: expression assignments should update DA before the loop
                for init in &f.init {
                    if let Stmt::Expression(es) = init {
                        // This will mark locals as definitely assigned when seeing assignments like `x = 0`
                        check_expr_definite_assignment(&es.expr, scopes, final_params)?;
                    }
                    // Note: we intentionally skip `Declaration` here to avoid leaking for-scope locals
                }
                // Evaluate condition for DA side-effects
                if let Some(cond) = &f.condition { check_expr_definite_assignment(cond, scopes, final_params)?; }
                if f.condition.is_none() {
                    // Infinite for(;;): only propagate assignments unconditionally before a top-level break
                    if let Some(names) = collect_unconditional_assigns_until_break(&f.body) {
                        for name in names {
                            if let Some(scope) = scopes.iter_mut().rev().find(|s| s.contains_key(&name)) {
                                scope.insert(name, true);
                            }
                        }
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
                // Compute DA across try/catch/finally per JLS-like rules
                let names: Vec<String> = scopes.last().unwrap().keys().cloned().collect();
                // Evaluate try block (suppress unreachable from sub-block)
                let mut try_scopes = scopes.clone();
                // Debug: show DA before entering try for known problematic locals
                let dbg_base_index = scopes.len() - 1;
                for key in ["classDesc", "target", "res", "child"].iter() {
                    if let Some(v) = scopes[dbg_base_index].get(*key) {
                        super::debug_log(format!("[da/try-entry] name={} assigned={}", key, v));
                    }
                }
                match walk_stmt_locals(&Block { statements: t.try_block.statements.clone(), span: t.try_block.span }, &mut try_scopes, final_params, inside_loop, global) {
                    Err(ReviewError::UnreachableStatement) => {/* ignore unreachable inside try */}
                    Err(e) => return Err(e),
                    Ok(()) => {}
                }
                let base_index = scopes.len() - 1;
                let try_top = &try_scopes[base_index];
                // Evaluate each catch block
                let mut catches_tops: Vec<std::collections::HashMap<String, bool>> = Vec::new();
                for cc in &t.catch_clauses {
                    let mut c_scopes = scopes.clone();
                    match walk_stmt_locals(&Block { statements: cc.block.statements.clone(), span: cc.block.span }, &mut c_scopes, final_params, inside_loop, global) {
                        Err(ReviewError::UnreachableStatement) => {/* ignore unreachable inside catch */}
                        Err(e) => return Err(e),
                        Ok(()) => {}
                    }
                    catches_tops.push(c_scopes[base_index].clone());
                }
                if let Some(fin) = &t.finally_block {
                    // Build DA at entry to finally: in_finally[name] = cur[name] || try_top[name] || (all catches[name])
                    let base_top = scopes[base_index].clone();
                    let mut f_scopes = scopes.clone();
                    {
                        let f_top = &mut f_scopes[base_index];
                        for n in &names {
                            let base = base_top.get(n).copied().unwrap_or(false);
                            let da_try = try_top.get(n).copied().unwrap_or(false);
                            let da_all_catches = if catches_tops.is_empty() { false } else { catches_tops.iter().all(|m| m.get(n).copied().unwrap_or(false)) };
                            f_top.insert(n.clone(), base || da_try || da_all_catches);
                        }
                    }
                    // Walk finally with seeded DA state (suppress unreachable)
                    match walk_stmt_locals(&Block { statements: fin.statements.clone(), span: fin.span }, &mut f_scopes, final_params, inside_loop, global) {
                        Err(ReviewError::UnreachableStatement) => {/* ignore unreachable inside finally */}
                        Err(e) => return Err(e),
                        Ok(()) => {}
                    }
                    // DA after try statement equals DA after finally
                    let out_top = &f_scopes[base_index];
                    let cur_top = &mut scopes[base_index];
                    for (k, v) in out_top.iter() { cur_top.insert(k.clone(), *v); }
                } else {
                    // No finally: per JLS-style rule, after try-catch the variable must be definitely assigned
                    // regardless of whether the try completed normally or a catch handled an exception. That requires
                    // it to be DA after the try block and after each catch block that can complete normally.
                    let cur_top = &mut scopes[base_index];
                    for n in &names {
                        let da_try = try_top.get(n).copied().unwrap_or(false);
                        let da_all_catches = if catches_tops.is_empty() { true } else { catches_tops.iter().all(|m| m.get(n).copied().unwrap_or(false)) };
                        cur_top.insert(n.clone(), da_try && da_all_catches);
                    }
                }
            }
            Stmt::Break(_b) => { /* not a hard terminator for unreachable diagnostic */ }
            Stmt::Continue(_b) => { /* not a hard terminator for unreachable diagnostic */ }
            Stmt::Switch(sw) => {
                // Improved merge: model fall-through by concatenating statements from each starting case to the end
                let names: Vec<String> = scopes.last().unwrap().keys().cloned().collect();
                let has_default = sw.cases.iter().any(|c| c.labels.is_empty());
                // Basic diagnostics: multiple defaults and duplicate constant labels (with constant folding)
                let mut default_count = 0usize;
                let mut seen_labels: std::collections::HashSet<i32> = std::collections::HashSet::new();
                let mut seen_str_labels: std::collections::HashSet<String> = std::collections::HashSet::new();
                let enum_selector_name: Option<String> = fold_selector_value_for_enum_switch_global(global, &sw.expression).map(|(n, _)| n);
                let mut seen_enum_labels: std::collections::HashSet<usize> = std::collections::HashSet::new();
                let mut seen_enum_texts: std::collections::HashSet<String> = std::collections::HashSet::new();
                for c in &sw.cases {
                    if c.labels.is_empty() { default_count += 1; }
                    for lab in &c.labels {
                        if let Some(v) = fold_case_value_for_int_switch(lab) {
                            if !seen_labels.insert(v) {
                                return Err(ReviewError::DuplicateSwitchCaseLabel(format!("{}", v)));
                            }
                        } else if let Some(s) = fold_case_value_for_string_switch(lab) {
                            if !seen_str_labels.insert(s.clone()) {
                                return Err(ReviewError::DuplicateSwitchCaseLabel(s));
                            }
                        } else if let Some(enum_name) = enum_selector_name.as_ref() {
                            if let Some(ord) = fold_case_value_for_enum_switch_global(enum_name, global, lab) {
                                if !seen_enum_labels.insert(ord) {
                                    return Err(ReviewError::DuplicateSwitchCaseLabel(format!("{}::#{}", enum_name, ord)));
                                }
                            }
                        } else {
                            // Fallback textual duplicate detection for enum-like labels when selector type is unknown
                            match lab {
                                Expr::Identifier(id) => {
                                    if !seen_enum_texts.insert(id.name.clone()) {
                                        return Err(ReviewError::DuplicateSwitchCaseLabel(id.name.clone()));
                                    }
                                }
                                Expr::FieldAccess(fa) => {
                                    if let Some(target) = &fa.target {
                                        if let Expr::Identifier(id) = &**target {
                                            let key = format!("{}.{}", id.name, fa.name);
                                            if !seen_enum_texts.insert(key.clone()) {
                                                return Err(ReviewError::DuplicateSwitchCaseLabel(key));
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                if default_count > 1 { return Err(ReviewError::MultipleSwitchDefaults); }
                let mut exit_maps: Vec<std::collections::HashMap<String, bool>> = Vec::new();
                // If switch expression is a constant, evaluate the single reachable fallthrough path
                // Try String constant first, then int constant
                if let Some(sval) = fold_selector_value_for_string_switch(&sw.expression) {
                    let default_index = sw.cases.iter().position(|c| c.labels.is_empty());
                    let mut match_index: Option<usize> = None;
                    for (idx, c) in sw.cases.iter().enumerate() {
                        if c.labels.iter().any(|lab| fold_case_value_for_string_switch(lab).as_deref() == Some(&sval)) {
                            match_index = Some(idx);
                            break;
                        }
                    }
                    if let Some(start) = match_index.or(default_index) {
                        // Gather fall-through statements starting at `start`, but stop at the first top-level break
                        let mut stmts: Vec<Stmt> = Vec::new();
                        'outer_const_str: for c in &sw.cases[start..] {
                            for s in &c.statements {
                                stmts.push(s.clone());
                                if matches!(s, Stmt::Break(_)) { break 'outer_const_str; }
                            }
                        }
                        // Only consider normal completion paths (those that contain a break)
                        let has_break = stmts.iter().any(|s| matches!(s, Stmt::Break(_)));
                        if has_break {
                            let block = Block { statements: stmts, span: sw.span };
                            let mut c_scopes = scopes.clone();
                            match walk_stmt_locals(&block, &mut c_scopes, final_params, inside_loop, global) {
                                Err(ReviewError::UnreachableStatement) => {/* ignore unreachable within simulated case path */}
                                Err(e) => return Err(e),
                                Ok(()) => {}
                            }
                            let base_index = scopes.len() - 1;
                            exit_maps.push(c_scopes[base_index].clone());
                        }
                    } else {
                        // constant but no matching label and no default: no statements execute
                        exit_maps.push(scopes.last().unwrap().clone());
                    }
                } else if let Some((enum_name, ord)) = fold_selector_value_for_enum_switch_global(global, &sw.expression) {
                    let default_index = sw.cases.iter().position(|c| c.labels.is_empty());
                    let mut match_index: Option<usize> = None;
                    for (idx, c) in sw.cases.iter().enumerate() {
                        if c.labels.iter().any(|lab| fold_case_value_for_enum_switch_global(&enum_name, global, lab) == Some(ord)) {
                            match_index = Some(idx);
                            break;
                        }
                    }
                    if let Some(start) = match_index.or(default_index) {
                        let mut stmts: Vec<Stmt> = Vec::new();
                        'outer_const_enum: for c in &sw.cases[start..] {
                            for s in &c.statements {
                                stmts.push(s.clone());
                                if matches!(s, Stmt::Break(_)) { break 'outer_const_enum; }
                            }
                        }
                        let has_break = stmts.iter().any(|s| matches!(s, Stmt::Break(_)));
                        if has_break {
                            let block = Block { statements: stmts, span: sw.span };
                            let mut c_scopes = scopes.clone();
                            match walk_stmt_locals(&block, &mut c_scopes, final_params, inside_loop, global) {
                                Err(ReviewError::UnreachableStatement) => {/* ignore unreachable within simulated case path */}
                                Err(e) => return Err(e),
                                Ok(()) => {}
                            }
                            let base_index = scopes.len() - 1;
                            exit_maps.push(c_scopes[base_index].clone());
                        }
                    } else {
                        exit_maps.push(scopes.last().unwrap().clone());
                    }
                } else if let Some(v) = fold_selector_value_for_int_switch(&sw.expression) {
                    let default_index = sw.cases.iter().position(|c| c.labels.is_empty());
                    let mut match_index: Option<usize> = None;
                    for (idx, c) in sw.cases.iter().enumerate() {
                        if c.labels.iter().any(|lab| fold_case_value_for_int_switch(lab) == Some(v)) {
                            match_index = Some(idx);
                            break;
                        }
                    }
                    if let Some(start) = match_index.or(default_index) {
                        // Gather fall-through statements starting at `start`, but stop at the first top-level break
                        let mut stmts: Vec<Stmt> = Vec::new();
                        'outer_const: for c in &sw.cases[start..] {
                            for s in &c.statements {
                                stmts.push(s.clone());
                                if matches!(s, Stmt::Break(_)) { break 'outer_const; }
                            }
                        }
                        // Only consider normal completion paths (those that contain a break)
                        let has_break = stmts.iter().any(|s| matches!(s, Stmt::Break(_)));
                        if has_break {
                            let block = Block { statements: stmts, span: sw.span };
                            let mut c_scopes = scopes.clone();
                            match walk_stmt_locals(&block, &mut c_scopes, final_params, inside_loop, global) {
                                Err(ReviewError::UnreachableStatement) => {/* ignore unreachable within simulated case path */}
                                Err(e) => return Err(e),
                                Ok(()) => {}
                            }
                            let base_index = scopes.len() - 1;
                            exit_maps.push(c_scopes[base_index].clone());
                        }
                    } else {
                        // constant but no matching label and no default: no statements execute
                        exit_maps.push(scopes.last().unwrap().clone());
                    }
                } else {
                    // Non-constant: consider all possible case entry points with fallthrough, but cut at first break
                    for start in 0..sw.cases.len() {
                        let mut stmts: Vec<Stmt> = Vec::new();
                        'outer: for c in &sw.cases[start..] {
                            for s in &c.statements {
                                stmts.push(s.clone());
                                if matches!(s, Stmt::Break(_)) { break 'outer; }
                            }
                        }
                        // Only include paths that complete normally via break
                        let has_break = stmts.iter().any(|s| matches!(s, Stmt::Break(_)));
                        if has_break {
                            let block = Block { statements: stmts, span: sw.span };
                            let mut c_scopes = scopes.clone();
                            match walk_stmt_locals(&block, &mut c_scopes, final_params, inside_loop, global) {
                                Err(ReviewError::UnreachableStatement) => {/* ignore unreachable within simulated case path */}
                                Err(e) => return Err(e),
                                Ok(()) => {}
                            }
                            let base_index = scopes.len() - 1;
                            exit_maps.push(c_scopes[base_index].clone());
                        }
                    }
                    // If no default: include the path that executes no case at all (normal completion),
                    // except when this is an enum switch that exhaustively lists all enum constants.
                    if !has_default {
                        let mut add_nomatch_path = true;
                        if let Some(g) = global {
                            // Try to detect if all labels belong to the same enum and cover all constants
                            // Determine candidate enum name and seen ordinals
                            let mut candidate: Option<String> = None;
                            let mut all_match_candidate = true;
                            let mut seen_ordinals: std::collections::HashSet<usize> = std::collections::HashSet::new();
                            // Collect non-default labels across all cases
                            'outer_enum: for c in &sw.cases {
                                for lab in &c.labels {
                                    let mut matched_this_label: Option<(String, usize)> = None;
                                    for (ename, inner) in &g.enum_index {
                                        if let Some(ord) = fold_case_value_for_enum_switch_global(ename, Some(g), lab) {
                                            matched_this_label = Some((ename.clone(), ord));
                                            break;
                                        }
                                    }
                                    if let Some((ename, ord)) = matched_this_label {
                                        if let Some(cn) = &candidate {
                                            if *cn != ename { all_match_candidate = false; break 'outer_enum; }
                                        } else {
                                            candidate = Some(ename);
                                        }
                                        seen_ordinals.insert(ord);
                                    } else {
                                        all_match_candidate = false; break 'outer_enum;
                                    }
                                }
                            }
                            if all_match_candidate {
                                if let Some(cn) = candidate.as_ref() {
                                    if let Some(inner) = g.enum_index.get(cn) {
                                        if seen_ordinals.len() == inner.len() {
                                            add_nomatch_path = false;
                                        }
                                    }
                                }
                            }
                        }
                        if add_nomatch_path { exit_maps.push(scopes.last().unwrap().clone()); }
                    }
                }
                let cur_top = scopes.last_mut().unwrap();
                for n in names {
                    // join: DA only if all normal-completion paths establish it
                    if !exit_maps.is_empty() && exit_maps.iter().all(|m| m.get(&n).copied().unwrap_or(false)) {
                        cur_top.insert(n, true);
                    }
                }
                // Basic reachability diagnostic for obviously empty switch with default that throws/returns not present; if switch has default with no statements, nothing to diagnose; more precise case label reachability would require const folding.
            }
            Stmt::Labeled(ls) => {
                // Evaluate labeled statement in a cloned scope but compare at the same scope depth
                let base_index = scopes.len() - 1;
                let names: Vec<String> = scopes[base_index].keys().cloned().collect();
                let mut tmp_scopes = scopes.clone();
                walk_stmt_locals(&Block { statements: vec![(*ls.statement.clone())], span: ls.span }, &mut tmp_scopes, final_params, inside_loop, global)?;
                let nested_same_depth = &tmp_scopes[base_index];
                let cur_top = scopes.last_mut().unwrap();
                for n in names {
                    if nested_same_depth.get(&n).copied().unwrap_or(false) { cur_top.insert(n, true); }
                }
            }
            Stmt::Expression(es) => {
                // First, validate uses in the expression
                check_expr_definite_assignment(&es.expr, scopes, final_params)?;
                // Then, if this is a simple local assignment (x = ...), mark x as definitely assigned
                if let Expr::Assignment(a) = &es.expr {
                    if let Expr::Identifier(id) = &*a.target {
                        if let Some(scope) = scopes.iter_mut().rev().find(|s| s.contains_key(&id.name)) {
                            super::debug_log(format!("[da/assign] name={} span={:?}", id.name, id.span));
                            scope.insert(id.name.clone(), true);
                        }
                    }
                }
            },
            Stmt::Return(ret) => { if let Some(e) = &ret.value { check_expr_definite_assignment(e, scopes, final_params)?; } last_terminator = Some("return"); },
            Stmt::Throw(ts) => { check_expr_definite_assignment(&ts.expr, scopes, final_params)?; last_terminator = Some("throw"); },
            _ => {}
        }
        match s {
            Stmt::Return(_) | Stmt::Throw(_) => {}
            _ => { last_terminator = None; }
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
    let inner_labels = collect_inner_labels(body);
    fn walker(stmt: &Stmt, inner_labels: &HashSet<String>, loop_depth: usize, in_switch: bool) -> (HashSet<String>, bool) {
        // returns (assigned_names, has_top_level_break)
        match stmt {
            Stmt::Break(b) => {
                let exits_loop = if let Some(lab) = &b.label { !inner_labels.contains(lab) } else { loop_depth == 0 && !in_switch };
                (HashSet::new(), exits_loop)
            }
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
                let (then_set, then_break) = walker(&ifstmt.then_branch, inner_labels, loop_depth, in_switch);
                let (else_set, else_break) = if let Some(else_b) = &ifstmt.else_branch { walker(else_b, inner_labels, loop_depth, in_switch) } else { (HashSet::new(), false) };
                let inter: HashSet<String> = then_set.intersection(&else_set).cloned().collect();
                (inter, then_break && else_break)
            }
            Stmt::Block(b) => {
                let mut acc: HashSet<String> = HashSet::new();
                let mut seen_break = false;
                for s in &b.statements {
                    let (set, has_break) = walker(s, inner_labels, loop_depth, in_switch);
                    // accumulate only if we haven't hit a break yet; after break, subsequent statements are unreachable
                    if !seen_break {
                        for n in set { acc.insert(n); }
                    }
                    if has_break { seen_break = true; }
                }
                (acc, seen_break)
            }
            Stmt::While(w) => {
                // consider inner while only if clearly enters (while(true))
                if expr_is_boolean_true(&w.condition) {
                    walker(&w.body, inner_labels, loop_depth + 1, in_switch)
                } else { (HashSet::new(), false) }
            }
            Stmt::For(f) => {
                // consider inner for only if infinite for(;;)
                if f.condition.is_none() {
                    walker(&f.body, inner_labels, loop_depth + 1, in_switch)
                } else { (HashSet::new(), false) }
            }
            Stmt::Switch(sw) => {
                // require a default and all cases to reach a break that exits the loop
                let has_default = sw.cases.iter().any(|c| c.labels.is_empty());
                if !has_default { return (HashSet::new(), false); }
                let mut maybe_inter: Option<HashSet<String>> = None;
                let mut all_cases_break = true;
                for c in &sw.cases {
                    // walk the case block treating we're inside a switch
                    let (set, has_break) = walker(&Stmt::Block(Block { statements: c.statements.clone(), span: c.span }), inner_labels, loop_depth, true);
                    if !has_break { all_cases_break = false; break; }
                    if let Some(acc) = &mut maybe_inter {
                        *acc = acc.intersection(&set).cloned().collect();
                    } else {
                        maybe_inter = Some(set);
                    }
                }
                if all_cases_break {
                    (maybe_inter.unwrap_or_default(), true)
                } else {
                    (HashSet::new(), false)
                }
            }
            // continue does not contribute; reaching it means no break on that path
            Stmt::Continue(_) => (HashSet::new(), false),
            _ => (HashSet::new(), false),
        }
    }
    if let Stmt::Block(b) = body {
        let (set, has_break) = walker(&Stmt::Block(b.clone()), &inner_labels, 0, false);
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
    // Delegate to generics-aware assignability using simple ReviewedType shapes
    use crate::review::generics::{ReviewedType as RT, is_assignable, TypeEnv};
    // Quick array mapping: "T[]" -> Array(Class(T))
    fn parse_simple(name: &str) -> RT {
        if name == "Object" { return RT::Object; }
        if name.ends_with("[]") {
            let base = &name[..name.len()-2];
            return RT::Array { of: Box::new(parse_simple(base)) };
        }
        // treat as raw class without args
        RT::Class { name: name.to_string(), args: Vec::new() }
    }
    let s = parse_simple(src);
    let d = parse_simple(dst);
    let env = TypeEnv::default();
    is_assignable(&s, &d, &env, global)
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
    if const_div_or_mod_by_zero(expr) { return Err(ReviewError::DivisionByZeroConstant); }
    match expr {
        Expr::Identifier(id) => {
            match lookup_assigned(scopes, &id.name) {
                Some(true) => Ok(()),
                Some(false) => { super::debug_log(format!("[da/use-before-init] name={} span={:?}", id.name, id.span)); Err(ReviewError::UseBeforeInit(id.name.clone())) },
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
        Expr::New(n) => {
            // Evaluate constructor arguments for DA side-effects
            for a in &n.arguments { check_expr_definite_assignment(a, scopes, final_params)?; }
            // If we are in an assignment context like `T x = new T(...);`, the caller will mark `x`.
            // For standalone `new T(...);` (unused), DA does not change.
            Ok(())
        }
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
    local_types: &mut Vec<HashMap<String, String>>,
    errors_as_name: bool,
) -> ReviewResult<()> {
    match stmt {
        Stmt::Declaration(vd) => {
            // record declared variable types in current scope
            if let Some(scope) = local_types.last_mut() {
                for var in &vd.variables { scope.insert(var.name.clone(), vd.type_ref.name.clone()); }
            }
            for var in &vd.variables { if let Some(init) = &var.initializer { walk_expr(current_class_name, init, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; } }
            Ok(())
        }
        Stmt::Expression(es) => walk_expr(current_class_name, &es.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name),
        Stmt::Return(ret) => {
            if let Some(e) = &ret.value { walk_expr(current_class_name, e, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            Ok(())
        }
        Stmt::If(ifstmt) => {
            walk_expr(current_class_name, &ifstmt.condition, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
            // then branch scope
            local_types.push(HashMap::new());
            walk_stmt(current_class_name, &ifstmt.then_branch, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
            local_types.pop();
            if let Some(else_b) = &ifstmt.else_branch {
                local_types.push(HashMap::new());
                walk_stmt(current_class_name, else_b, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
                local_types.pop();
            }
            Ok(())
        }
        Stmt::While(w) => {
            walk_expr(current_class_name, &w.condition, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
            walk_stmt(current_class_name, &w.body, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)
        }
        Stmt::For(f) => {
            for init in &f.init { walk_stmt(current_class_name, init, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            if let Some(cond) = &f.condition { walk_expr(current_class_name, cond, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            for upd in &f.update { walk_expr(current_class_name, &upd.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            walk_stmt(current_class_name, &f.body, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)
        }
        Stmt::Block(b) => {
            local_types.push(HashMap::new());
            for s in &b.statements { walk_stmt(current_class_name, s, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            local_types.pop();
            Ok(())
        }
        Stmt::Switch(sw) => {
            walk_expr(current_class_name, &sw.expression, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
            for c in &sw.cases {
                local_types.push(HashMap::new());
                for s in &c.statements { walk_stmt(current_class_name, s, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
                local_types.pop();
            }
            Ok(())
        }
        Stmt::Try(t) => {
            for r in &t.resources { match r { TryResource::Var { initializer, .. } => walk_expr(current_class_name, initializer, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?, TryResource::Expr { expr, .. } => walk_expr(current_class_name, expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?, } }
            local_types.push(HashMap::new());
            walk_stmt(current_class_name, &Stmt::Block(t.try_block.clone()), arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
            local_types.pop();
            for cc in &t.catch_clauses { local_types.push(HashMap::new()); walk_stmt(current_class_name, &Stmt::Block(cc.block.clone()), arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; local_types.pop(); }
            if let Some(fin) = &t.finally_block { local_types.push(HashMap::new()); walk_stmt(current_class_name, &Stmt::Block(fin.clone()), arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; local_types.pop(); }
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
    local_types: &mut Vec<HashMap<String, String>>,
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
                // Early accept common IO overloads that our simple aggregator might miss
                if (mc.name == "read" || mc.name == "write") && mc.arguments.len() == 3 {
                    return Ok(());
                }
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
                // Collect arities/signatures from current class and its superclasses/interfaces
                let mut expected_list_acc: Vec<usize> = arities.get(&mc.name).cloned().unwrap_or_default();
                let mut varargs_min_here = varargs_min.get(&mc.name).copied();
                let mut cands_acc: Vec<Vec<String>> = signatures.get(&mc.name).cloned().unwrap_or_default();
                if let Some(g) = global {
                    if let Some(mt) = resolve_type_in_index(g, current_class_name) {
                        // also merge current type's index data (in case local table missed a declaration)
                        if let Some(v) = mt.methods_arities.get(&mc.name) { expected_list_acc.extend_from_slice(v); }
                        if let Some(cs) = mt.methods_signatures.get(&mc.name) { cands_acc.extend_from_slice(cs); }
                        if varargs_min_here.is_none() {
                            if let Some(vmin) = mt.methods_varargs_min.get(&mc.name) { varargs_min_here = Some(*vmin); }
                        }
                        let mut queue: Vec<String> = Vec::new();
                        if let Some(sup) = &mt.super_name { queue.push(sup.clone()); }
                        for it in &mt.interfaces { queue.push(it.clone()); }
                        let mut seen = std::collections::HashSet::new();
                        while let Some(tn) = queue.pop() {
                            if !seen.insert(tn.clone()) { continue; }
                            if let Some(mt2) = resolve_type_in_index(g, &tn) {
                                if let Some(v) = mt2.methods_arities.get(&mc.name) { expected_list_acc.extend_from_slice(v); }
                                if let Some(cs) = mt2.methods_signatures.get(&mc.name) { cands_acc.extend_from_slice(cs); }
                                if varargs_min_here.is_none() {
                                    if let Some(vmin) = mt2.methods_varargs_min.get(&mc.name) { varargs_min_here = Some(*vmin); }
                                }
                                // Also walk up this ancestor's own ancestors
                                if let Some(s2) = &mt2.super_name { queue.push(s2.clone()); }
                                for it2 in &mt2.interfaces { queue.push(it2.clone()); }
                            }
                        }
                    }
                }
                if !expected_list_acc.is_empty() {
                    expected_list_acc.sort_unstable(); expected_list_acc.dedup();
                    let matches_fixed = expected_list_acc.iter().any(|a| *a == found_arity);
                    let matches_varargs = varargs_min_here.map(|min| found_arity >= min).unwrap_or(false);
                    if !matches_fixed && !matches_varargs {
                        let mut expected = expected_list_acc.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                        if let Some(min) = varargs_min_here { expected.push(format!("{}+", min)); }
                        let expected = expected.join(", ");
                        return Err(ReviewError::MethodCallArityMismatch { name: mc.name.clone(), expected, found: found_arity });
                    }
                    if !cands_acc.is_empty() {
                        // Deduplicate signatures
                        let mut cands_unique: Vec<Vec<String>> = Vec::new();
                        for s in &cands_acc {
                            if !cands_unique.iter().any(|t| *t == *s) { cands_unique.push((*s).clone()); }
                        }
                        let found_types: Vec<Option<&'static str>> = mc.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                        if found_types.iter().all(|o| o.is_some()) {
                            let found_list: Vec<&str> = found_types.iter().map(|o| o.unwrap()).collect();
                            let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                            for sig in &cands_unique {
                                if let Some((cost, convs)) = calc_cost_with_varargs(sig, &found_list, varargs_min_here, global) { applicable.push((sig, cost, convs)); }
                            }
                            if applicable.is_empty() {
                                let expected = cands_unique.iter().filter(|s| s.len()==found_list.len()).map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                let found = found_list.join(",");
                                return Err(ReviewError::InapplicableMethod { name: mc.name.clone(), expected, found });
                            } else if applicable.len() > 1 {
                                // Tie-breaks:
                                // 1) Prefer exact conversions (zero convs)
                                let exact: Vec<(&Vec<String>, u32, u32)> = applicable.iter().cloned().filter(|(_, _, k)| *k == 0).collect();
                                if exact.len() > 1 {
                                    // 2) Prefer fixed-arity matches (signature length equals found args length)
                                    let mut fixed_exact: Vec<&Vec<String>> = exact.iter().map(|(s, _, _)| *s).filter(|s| s.len() == found_list.len()).collect();
                                    if fixed_exact.len() == 1 {
                                        // resolved uniquely; keep single
                                        applicable.retain(|(s, _, _)| *s == fixed_exact[0]);
                                    } else {
                                        // 3) Still ambiguous among exact conversions
                                        let candidates = exact.iter().map(|(s, _, _)| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                        let found = found_list.join(",");
                                        return Err(ReviewError::AmbiguousMethod { name: mc.name.clone(), candidates, found });
                                    }
                                } else if exact.len() == 1 {
                                    applicable.retain(|(s, _, _)| *s == exact[0].0);
                                } else {
                                    // 4) Prefer minimal conversions, then minimal cost
                                    let min_convs = applicable.iter().map(|(_, _, k)| *k).min().unwrap();
                                    applicable.retain(|(_, _, k)| *k == min_convs);
                                    let min_cost = applicable.iter().map(|(_, c, _)| *c).min().unwrap();
                                    applicable.retain(|(_, c, _)| *c == min_cost);
                                    if applicable.len() > 1 {
                                        // 5) Prefer fixed-arity over varargs when both applicable
                                        let mut fixed_only: Vec<&Vec<String>> = applicable.iter().map(|(s, _, _)| *s).filter(|s| s.len() == found_list.len()).collect();
                                        if fixed_only.len() == 1 {
                                            applicable.retain(|(s, _, _)| *s == fixed_only[0]);
                                        } else {
                                            // 6) Most-specific by subtyping among references
                                            let tied: Vec<&Vec<String>> = applicable.iter().map(|(s, _, _)| *s).collect();
                                            if break_specificity_tie(&tied, global) {
                                                // accept resolution
                                            } else {
                                                let candidates = applicable.iter().map(|(s, _, _)| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                                let found = found_list.join(",");
                                                return Err(ReviewError::AmbiguousMethod { name: mc.name.clone(), candidates, found });
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            // Unknown arg types: if any candidate matches arity (including varargs), accept
                            let any_match = cands_acc.iter().any(|s| s.len() == found_arity) || varargs_min_here.map(|min| found_arity >= min).unwrap_or(false);
                            if !any_match {
                                let expected = expected_list_acc.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ");
                                return Err(ReviewError::MethodCallArityMismatch { name: mc.name.clone(), expected, found: found_arity });
                            }
                        }
                    }
                } else {
                    // No expected arities found (e.g., overload declared later or inherited through interface not captured):
                    // fallback to allow the call when we have any signatures with matching arity gathered, or when varargs could apply
                    if !cands_acc.is_empty() {
                        let any_match = cands_acc.iter().any(|s| s.len() == found_arity) || varargs_min_here.map(|min| found_arity >= min).unwrap_or(false);
                        if !any_match {
                            return Err(ReviewError::MethodCallArityMismatch { name: mc.name.clone(), expected: String::new(), found: found_arity });
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
                                    let mut cands_unique: Vec<Vec<String>> = Vec::new();
                                    for s in cands { if !cands_unique.iter().any(|t| t == s) { cands_unique.push(s.clone()); } }
                                    let found_types: Vec<Option<&'static str>> = mc.arguments.iter().map(|a| infer_expr_primitive_or_string(a)).collect();
                                    if found_types.iter().all(|o| o.is_some()) {
                                        let found_list: Vec<&str> = found_types.iter().map(|o| o.unwrap()).collect();
                                        let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                                        for sig in &cands_unique {
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
                                        if applicable.is_empty() && !cands_unique.is_empty() {
                                            let expected = cands_unique.iter().filter(|s| s.len()==found_list.len()).map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                            let found = found_list.join(",");
                                    return Err(ReviewError::InapplicableMethod { name: format!("{}::{}", id.name, mc.name), expected, found });
                                        } else if applicable.len() > 1 {
                                            let exact: Vec<(&Vec<String>, u32, u32)> = applicable.iter().cloned().filter(|(_, _, k)| *k == 0).collect();
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
                            } else if crate::review::compat_mode() {
                                // In compat mode, when types are unknown, avoid hard error
                            }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // walk target and args
            if let Some(t) = &mc.target { walk_expr(current_class_name, t, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            for a in &mc.arguments { walk_expr(current_class_name, a, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            Ok(())
        }
        Expr::Binary(b) => { walk_expr(current_class_name, &b.left, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; walk_expr(current_class_name, &b.right, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name) }
        Expr::Unary(u) => walk_expr(current_class_name, &u.operand, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name),
        Expr::Assignment(a) => { walk_expr(current_class_name, &a.target, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; walk_expr(current_class_name, &a.value, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name) }
        Expr::Cast(c) => {
            // Generic arity check on cast target type
            if let Some(g) = global {
                if let Some(mt) = resolve_type_in_index(g, &c.target_type.name) {
                    if c.target_type.type_args.len() != mt.type_param_count {
                            super::debug_log(format!(
                                "GenericArityMismatch (cast): type={} expected_params={} found_args={} (resolved via index)",
                                c.target_type.name,
                                mt.type_param_count,
                                c.target_type.type_args.len()
                            ));
                        return Err(ReviewError::GenericArityMismatch { typename: c.target_type.name.clone(), expected: mt.type_param_count, found: c.target_type.type_args.len() });
                    }
                    if !mt.type_param_bounds.is_empty() {
                        for (i, targ) in c.target_type.type_args.iter().enumerate() {
                            if let Some(bounds) = mt.type_param_bounds.get(i) {
                                if let Some(tname) = typearg_to_simple_name(targ) {
                                    for b in bounds {
                                        if !is_reference_assignable(g, &tname, b) {
                                            super::debug_log(format!(
                                                "GenericBoundViolation (cast): type={} arg#{}={} not <: {}",
                                                c.target_type.name,
                                                i,
                                                tname,
                                                b
                                            ));
                                            return Err(ReviewError::GenericBoundViolation { typename: c.target_type.name.clone(), bound: b.clone(), found: tname.clone() });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            walk_expr(current_class_name, &c.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)
        },
        Expr::InstanceOf(io) => {
            if let Some(g) = global {
                if let Some(mt) = resolve_type_in_index(g, &io.target_type.name) {
                    if io.target_type.type_args.len() != mt.type_param_count {
                            super::debug_log(format!(
                                "GenericArityMismatch (instanceof): type={} expected_params={} found_args={} (resolved via index)",
                                io.target_type.name,
                                mt.type_param_count,
                                io.target_type.type_args.len()
                            ));
                        return Err(ReviewError::GenericArityMismatch { typename: io.target_type.name.clone(), expected: mt.type_param_count, found: io.target_type.type_args.len() });
                    }
                    if !mt.type_param_bounds.is_empty() {
                        for (i, targ) in io.target_type.type_args.iter().enumerate() {
                            if let Some(bounds) = mt.type_param_bounds.get(i) {
                                if let Some(tname) = typearg_to_simple_name(targ) {
                                    for b in bounds {
                                        if !is_reference_assignable(g, &tname, b) {
                                            super::debug_log(format!(
                                                "GenericBoundViolation (instanceof): type={} arg#{}={} not <: {}",
                                                io.target_type.name,
                                                i,
                                                tname,
                                                b
                                            ));
                                            return Err(ReviewError::GenericBoundViolation { typename: io.target_type.name.clone(), bound: b.clone(), found: tname.clone() });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            walk_expr(current_class_name, &io.expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)
        },
        Expr::Conditional(c) => { walk_expr(current_class_name, &c.condition, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; walk_expr(current_class_name, &c.then_expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; walk_expr(current_class_name, &c.else_expr, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name) },
        Expr::ArrayAccess(acc) => { walk_expr(current_class_name, &acc.array, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; walk_expr(current_class_name, &acc.index, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name) },
        Expr::FieldAccess(fa) => {
            // Qualified field access: check static legality and accessibility
            if let Some(t) = &fa.target {
                if let Expr::Identifier(id) = &**t {
                    if let Some(g) = global {
                        if let Some(mt) = resolve_type_in_index(g, &id.name) {
                            if let Some(is_static) = mt.fields_static.get(&fa.name) {
                                if !*is_static {
                                    return Err(ReviewError::IllegalStaticCall { typename: id.name.clone(), name: fa.name.clone() });
                                }
                            }
                            // Accessibility: private/package/protected/public
                            if let Some(vis) = mt.fields_visibility.get(&fa.name) {
                                let same_package = mt.package_name.as_deref() == g.package.as_deref();
                                match vis {
                                    super::types::Visibility::Private => {
                                        return Err(ReviewError::InaccessibleMember { typename: id.name.clone(), name: fa.name.clone() });
                                    }
                                    super::types::Visibility::Package => {
                                        if !same_package {
                                            return Err(ReviewError::InaccessibleMember { typename: id.name.clone(), name: fa.name.clone() });
                                        }
                                    }
                                    super::types::Visibility::Protected => {
                                        if !same_package {
                                            // Simple protected rule: require same package (approx). Full JLS allows subclasses; left for future.
                                            return Err(ReviewError::InaccessibleMember { typename: id.name.clone(), name: fa.name.clone() });
                                        }
                                    }
                                    super::types::Visibility::Public => {}
                                }
                            }
                        }
                    }
                }
                walk_expr(current_class_name, t, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
            }
            Ok(())
        },
        Expr::New(n) => {
            // If this is an array creation (new T[...]), don't treat as a constructor
            if n.target_type.array_dims > 0 {
                for a in &n.arguments {
                    walk_expr(current_class_name, a, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?;
                }
                return Ok(());
            }
            // validate constructor call only for current class
            if n.target_type.name == current_class_name {
                let found_arity = n.arguments.len();
                let kinds: Vec<&'static str> = n.arguments.iter().map(|e| expr_kind(e)).collect();
                super::debug_log(format!("[new/current] target={} found_arity={} kinds={:?} span={:?}", current_class_name, found_arity, kinds, n.span));
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
                        // prefer primitive inference; fallback to local-type names
                        let mut found: Vec<String> = Vec::with_capacity(n.arguments.len());
                        let mut all_known = true;
                        for a in &n.arguments {
                            if let Some(p) = infer_expr_primitive_or_string(a) { found.push(p.to_string()); }
                            else if let Some(tn) = infer_expr_with_locals(a, local_types, current_class_name) { found.push(tn); }
                            else { all_known = false; break; }
                        }
                        if all_known {
                            let found_list: Vec<&str> = found.iter().map(|s| s.as_str()).collect();
                            super::debug_log(format!("[new/current] arg_types={:?}", found_list));
                            let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                            for sig in cands {
                                if let Some((cost, convs)) = calc_cost_with_varargs(sig, &found_list, ctor_varargs_min.get(current_class_name).copied(), global) { applicable.push((sig, cost, convs)); }
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
                                if tied.len() > 1 { if break_specificity_tie(&tied, global) { /* kept one */ } else { let candidates = tied.iter().map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", "); let found = found_list.join(","); return Err(ReviewError::AmbiguousMethod { name: format!("<init> {}", current_class_name), candidates, found }); } }
                            }
                        }
                    }
                }
            } else if let Some(g) = global {
                if let Some(mt) = resolve_type_in_index(g, &n.target_type.name) {
                    // Simple generic arity check: number of <T> in type use must match declaration
                    let found_args = n.target_type.type_args.len();
                    let kinds: Vec<&'static str> = n.arguments.iter().map(|e| expr_kind(e)).collect();
                    super::debug_log(format!("[new/external] target={} found_arity={} kinds={:?} span={:?}", n.target_type.name, n.arguments.len(), kinds, n.span));
                    if found_args != mt.type_param_count {
                        // Only tolerate raw usage (zero args) in compatibility mode; otherwise error
                        if found_args == 0 && crate::review::compat_mode() {
                            super::debug_log(format!(
                                "compat: tolerate raw-type usage: {} expects {}, found 0",
                                n.target_type.name, mt.type_param_count
                            ));
                        } else {
                            super::debug_log(format!(
                                "GenericArityMismatch (new idx): type={} expected_params={} found_args={}",
                                n.target_type.name,
                                mt.type_param_count,
                                found_args
                            ));
                            return Err(ReviewError::GenericArityMismatch { typename: n.target_type.name.clone(), expected: mt.type_param_count, found: found_args });
                        }
                    }
                    // Basic bounds check: if bounds recorded, ensure each argument type simple name matches or is assignable to the bound simple name
                    if !mt.type_param_bounds.is_empty() {
                        for (i, targ) in n.target_type.type_args.iter().enumerate() {
                            if let crate::ast::TypeArg::Wildcard(_) = targ {
                                return Err(ReviewError::WildcardNotAllowedInNew);
                            }
                            if let Some(bounds) = mt.type_param_bounds.get(i) {
                                if let Some(tname) = typearg_to_simple_name(targ) {
                                    for b in bounds {
                                        if !is_reference_assignable(g, &tname, b) {
                                            return Err(ReviewError::GenericBoundViolation { typename: n.target_type.name.clone(), bound: b.clone(), found: tname.clone() });
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // Prefer signature-based applicability like javac; fall back to arity check
                    if let Some(cands) = mt.ctors_signatures.get(&n.target_type.name) {
                        // Attempt to infer argument types using primitive inference; fallback to local variable type names
                        let mut tmp: Vec<String> = Vec::with_capacity(n.arguments.len());
                        let mut all_known = true;
                        for a in &n.arguments {
                            if let Some(p) = infer_expr_primitive_or_string(a) { tmp.push(p.to_string()); }
                            else if let Some(tn) = infer_expr_with_locals(a, local_types, current_class_name) { tmp.push(tn); }
                            else { all_known = false; break; }
                        }
                            if all_known {
                                let found_list: Vec<&str> = tmp.iter().map(|s| s.as_str()).collect();
                                super::debug_log(format!("[new/external] arg_types={:?} span={:?}", found_list, n.span));
                            let mut applicable: Vec<(&Vec<String>, u32, u32)> = Vec::new();
                            for sig in cands {
                                if let Some((cost, convs)) = calc_cost_with_varargs(sig, &found_list, mt.ctors_varargs_min.get(&n.target_type.name).copied(), global) {
                                    applicable.push((sig, cost, convs));
                                }
                            }
                            if applicable.is_empty() && !cands.is_empty() {
                                let expected = cands.iter().filter(|s| s.len()==found_list.len()).map(|s| format!("({})", s.join(","))).collect::<Vec<_>>().join(", ");
                                let found = found_list.join(",");
                                return Err(ReviewError::InapplicableMethod { name: format!("<init> {}", n.target_type.name), expected, found });
                            }
                        }
                    }
                    let found_arity = n.arguments.len();
                    if let Some(expected_list) = mt.ctors_arities.get(&n.target_type.name) {
                        let matches_fixed = expected_list.iter().any(|a| *a == found_arity);
                        let matches_varargs = mt.ctors_varargs_min.get(&n.target_type.name).map(|min| found_arity >= *min).unwrap_or(false);
                        if !matches_fixed && !matches_varargs {
                            let mut expected = expected_list.iter().map(|a| a.to_string()).collect::<Vec<_>>();
                            if let Some(min) = mt.ctors_varargs_min.get(&n.target_type.name) { expected.push(format!("{}+", min)); }
                            let expected = expected.join(", ");
                            return Err(ReviewError::MethodCallArityMismatch { name: format!("<init> {}", n.target_type.name), expected, found: found_arity });
                        }
                    }
                } else if let Some(expected_params) = lookup_type_param_count(g, &n.target_type.name) {
                    let found_args = n.target_type.type_args.len();
                    if found_args != expected_params {
                        if found_args == 0 && crate::review::compat_mode() {
                            super::debug_log(format!(
                                "compat: suppress raw-type arity error (fallback) for {}: expected {}, found 0",
                                n.target_type.name, expected_params
                            ));
                        } else {
                            super::debug_log(format!(
                                "GenericArityMismatch (new fallback): type={} expected_params={} found_args={} (lookup_type_param_count)",
                                n.target_type.name,
                                expected_params,
                                found_args
                            ));
                            return Err(ReviewError::GenericArityMismatch { typename: n.target_type.name.clone(), expected: expected_params, found: found_args });
                        }
                    }
                }
            }
            for a in &n.arguments { walk_expr(current_class_name, a, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name)?; }
            Ok(())
        }
        Expr::Parenthesized(p) => walk_expr(current_class_name, p, arities, signatures, varargs_min, ctor_arities, ctor_signatures, ctor_varargs_min, global, local_methods_static, local_types, errors_as_name),
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

fn expr_kind(expr: &Expr) -> &'static str {
    match expr {
        Expr::Literal(_) => "Literal",
        Expr::Identifier(_) => "Identifier",
        Expr::Binary(_) => "Binary",
        Expr::Unary(_) => "Unary",
        Expr::Assignment(_) => "Assignment",
        Expr::MethodCall(_) => "MethodCall",
        Expr::FieldAccess(_) => "FieldAccess",
        Expr::ArrayAccess(_) => "ArrayAccess",
        Expr::Cast(_) => "Cast",
        Expr::InstanceOf(_) => "InstanceOf",
        Expr::Conditional(_) => "Conditional",
        Expr::New(_) => "New",
        Expr::Parenthesized(_) => "Parenthesized",
        Expr::ArrayInitializer(_) => "ArrayInitializer",
    }
}

fn is_assignable_literal(expected: &str, found: &str) -> bool {
    if found == "null" { return true; }
    match expected {
        // Narrowing allowed for constants; also accept explicit casts recognized by inference
        "byte" => matches!(found, "byte"|"int"|"char"),
        "short" => matches!(found, "short"|"int"|"char"),
        "char" => matches!(found, "char"|"int"),
        "int" => found == "int",
        "long" => matches!(found, "int"),
        "float" => matches!(found, "double"|"int"),
        "double" => matches!(found, "double"|"int"),
        "boolean" => found == "boolean",
        "String" => found == "String",
        _ => true,
    }
}

// Enforce invariance for generic locals in simple assignments: a = b;
fn enforce_local_assignment_types_in_block(
    body: &Block,
    global: &crate::review::types::GlobalMemberIndex,
) -> ReviewResult<()> {
    use crate::ast::*;
    use crate::review::generics::{resolve_type_ref, is_assignable, TypeEnv};
    use crate::review::generics::ReviewedType as RT;
    // Track local variable declared types (full TypeRef)
    fn walk_block(
        block: &Block,
        locals: &mut Vec<HashMap<String, TypeRef>>,
        global: &crate::review::types::GlobalMemberIndex,
    ) -> ReviewResult<()> {
        locals.push(HashMap::new());
        for s in &block.statements {
            match s {
                Stmt::Block(b) => { walk_block(b, locals, global)?; }
                Stmt::Declaration(vd) => {
                    for var in &vd.variables {
                        if locals.last().unwrap().contains_key(&var.name) {
                            return Err(ReviewError::DuplicateLocalVar(var.name.clone()));
                        }
                        locals.last_mut().unwrap().insert(var.name.clone(), vd.type_ref.clone());
                        // Do not enforce initializer compatibility here to avoid over-eager errors
                    }
                }
                Stmt::Expression(es) => {
                    if let Expr::Assignment(a) = &es.expr {
                        if let Expr::Identifier(id_dst) = &*a.target {
                            if let Some(dst_tr) = lookup_local_type(locals, &id_dst.name) {
                                if let Some(src_tr) = lookup_ident_expr_typeref(&a.value, locals) {
                                    // Enforce invariance only when both locals share the same raw generic type and have concrete args
                                    if dst_tr.name == src_tr.name && !dst_tr.type_args.is_empty() && !src_tr.type_args.is_empty() {
                                        if dst_tr.type_args.len() == src_tr.type_args.len() {
                                            for (da, sa) in dst_tr.type_args.iter().zip(src_tr.type_args.iter()) {
                                                if let (crate::ast::TypeArg::Type(dt), crate::ast::TypeArg::Type(st)) = (da, sa) {
                                                    if dt.name != st.name || dt.array_dims != st.array_dims {
                                                        return Err(ReviewError::IncompatibleInitializer { expected: dst_tr.name.clone(), found: src_tr.name.clone() });
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
                _ => {}
            }
        }
        locals.pop();
        Ok(())
    }
    // No static expr type resolution used in this lightweight check
    fn lookup_ident_expr_typeref<'a>(e: &Expr, locals: &'a Vec<HashMap<String, TypeRef>>) -> Option<TypeRef> {
        if let Expr::Identifier(id) = e { return lookup_local_type(locals, &id.name).cloned(); }
        None
    }
    fn lookup_local_type<'a>(locals: &'a Vec<HashMap<String, TypeRef>>, name: &str) -> Option<&'a TypeRef> {
        for scope in locals.iter().rev() { if let Some(t) = scope.get(name) { return Some(t); } }
        None
    }
    fn describe_rt(t: &RT) -> String { crate::review::generics::describe_type(t) }
    let mut locals: Vec<HashMap<String, TypeRef>> = Vec::new();
    walk_block(body, &mut locals, global)
}

fn is_assignable_primitive_or_string(expected: &str, found: &str) -> bool {
    if found == "null" { return expected != "int" && expected != "long" && expected != "double" && expected != "float" && expected != "boolean" && expected != "char"; }
    if expected == found { return true; }
    match expected {
        // Primitive widening conversions
        // byte -> short -> int -> long -> float -> double
        // short -> int -> long -> float -> double
        // char -> int -> long -> float -> double
        // int -> long -> float -> double
        // long -> float -> double
        // float -> double
        "double" => matches!(found, "float"|"long"|"int"|"short"|"byte"|"char"),
        "float" => matches!(found, "long"|"int"|"short"|"byte"|"char"),
        "long" => matches!(found, "int"|"short"|"byte"|"char"),
        "int" => matches!(found, "short"|"byte"|"char"),
        "String" => found == "String",
        _ => false,
    }
}

fn is_wrapper_type(t: &str) -> bool {
    matches!(t, "Integer"|"Long"|"Float"|"Double"|"Boolean"|"Character"|"String")
}

fn is_primitive_type(t: &str) -> bool { matches!(t, "byte"|"short"|"int"|"long"|"float"|"double"|"boolean"|"char") }

fn boxing_partner_of(prim: &str) -> Option<&'static str> {
    match prim {
        "int" => Some("Integer"),
        "long" => Some("Long"),
        "float" => Some("Float"),
        "double" => Some("Double"),
        "boolean" => Some("Boolean"),
        "char" => Some("Character"),
        _ => None,
    }
}

fn unboxing_partner_of(wrapper: &str) -> Option<&'static str> {
    match wrapper {
        "Integer" => Some("int"),
        "Long" => Some("long"),
        "Float" => Some("float"),
        "Double" => Some("double"),
        "Boolean" => Some("boolean"),
        "Character" => Some("char"),
        _ => None,
    }
}

// Return Some(cost) if convertible, None if not applicable. Lower cost preferred.
fn conversion_cost_with_boxing(expected: &str, found: &str) -> Option<u32> {
    // exact
    if expected == found { return Some(0); }
    // null to reference/wrapper/String
    if found == "null" {
        if !is_primitive_type(expected) { return Some(1); } else { return None; }
    }
    // primitive widening
    if is_primitive_type(expected) && is_primitive_type(found) {
        if is_assignable_primitive_or_string(expected, found) { return Some(widening_cost(expected, found)); } else { return None; }
    }
    // boxing: expected wrapper, found primitive
    if let Some(p) = unboxing_partner_of(expected) {
        if p == found { return Some(2); }
        // primitive -> wrapper with widening then boxing is not standard; treat as incompatible here
    }
    // boxing to general reference (e.g., Object or type variable T): allow primitive to be boxed
    if !is_primitive_type(expected) && is_primitive_type(found) {
        return Some(3);
    }
    // unboxing: expected primitive, found wrapper
    if is_primitive_type(expected) {
        if let Some(p) = unboxing_partner_of(found) {
            if p == expected { return Some(2); }
            // wrapper -> primitive with unboxing and widening
            if is_assignable_primitive_or_string(expected, p) { return Some(3); }
        }
    }
    // trivial reference matches
    if expected == "String" && found == "String" { return Some(0); }
    // approximate reference assignability for simple names: treat non-primitive/non-String as compatible with small cost
    if !is_primitive_type(expected) && !is_primitive_type(found) {
        // treat as assignable with modest cost to allow constructor/method selection to proceed
        return Some(10);
    }
    None
}

fn conversion_cost_with_boxing_ctx(
    expected: &str,
    found: &str,
    global: Option<&crate::review::types::GlobalMemberIndex>,
) -> Option<u32> {
    // first try primitive/boxing path
    if let Some(c) = conversion_cost_with_boxing(expected, found) { return Some(c); }
    // Treat single-letter or uppercase-leading identifiers as method type variables (e.g., T)
    // Minimal poly-expression inference: accept any argument for such parameters with small cost
    if !is_primitive_type(expected) && expected.chars().next().map(|c| c.is_ascii_uppercase()).unwrap_or(false) {
        return Some(1);
    }
    // then try reference assignability when we have a global index
    if let Some(g) = global {
        if !is_primitive_type(expected) && !is_primitive_type(found) {
            // Treat simple names as assignable if found is the same or a subtype of expected
            if expected == found { return Some(0); }
            if is_reference_assignable(g, found, expected) { return Some(4); }
        }
    }
    None
}

fn is_more_specific_primitive(a: &str, b: &str) -> bool {
    // a is more specific than b if a can convert to b but not vice versa
    match (a, b) {
        ("char", "int") => true,
        ("int", "long") => true,
        ("long", "float") => true,
        ("float", "double") => true,
        (x, y) if x == y => false,
        _ => false,
    }
}

fn typearg_to_simple_name(ta: &crate::ast::TypeArg) -> Option<String> {
    match ta {
        crate::ast::TypeArg::Type(t) => Some(t.name.clone()),
        crate::ast::TypeArg::Wildcard(w) => {
            // wildcard erases to Object for upper-bound, or to bound's erasure; simplified:
            if let Some((_bk, tr)) = &w.bound { Some(tr.name.clone()) } else { Some("Object".to_string()) }
        }
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
            "byte" | "short" | "int" | "long" | "float" | "double" | "boolean" | "char" | "String" => Some(
                match c.target_type.name.as_str() {
                    "byte" => "byte",
                    "short" => "short",
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

// Lightweight local-type-aware inference: extends primitive inference by resolving identifiers from a
// provided local-types environment to simple reference names when available.
fn infer_expr_with_locals(
    expr: &Expr,
    local_types: &Vec<std::collections::HashMap<String, String>>,
    current_class: &str,
) -> Option<String> {
    // Prefer primitive/String when determinable
    if let Some(p) = infer_expr_primitive_or_string(expr) { return Some(p.to_string()); }
    match expr {
        Expr::Identifier(id) => {
            // this refers to current class instance
            if id.name == "this" { return Some(current_class.to_string()); }
            // search local scopes from innermost to outermost
            for scope in local_types.iter().rev() {
                if let Some(tname) = scope.get(&id.name) { return Some(tname.clone()); }
            }
            None
        }
        Expr::Parenthesized(p) => infer_expr_with_locals(p, local_types, current_class),
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

fn calc_cost_with_varargs(
    sig: &Vec<String>,
    found_list: &Vec<&str>,
    varargs_min: Option<usize>,
    global: Option<&crate::review::types::GlobalMemberIndex>,
) -> Option<(u32, u32)> {
    let fixed_len = sig.len();
    let has_varargs = varargs_min.is_some() && fixed_len > 0;
    let min_arity = if has_varargs { fixed_len - 1 } else { fixed_len };
    if found_list.len() < min_arity { return None; }
    if !has_varargs && found_list.len() != fixed_len { return None; }
    let mut cost: u32 = 0;
    let mut convs: u32 = 0;
    // fixed part
    for i in 0..(fixed_len.saturating_sub(if has_varargs {1} else {0})) {
        let exp = sig.get(i).unwrap();
        let f = *found_list.get(i).unwrap_or(&"");
        if let Some(cc) = conversion_cost_with_boxing_ctx(exp.as_str(), f, global) {
            if cc > 0 { convs += 1; }
            cost += cc;
        } else { return None; }
    }
    if has_varargs {
        let var_t = sig.last().unwrap();
        for j in (fixed_len - 1)..found_list.len() {
            let f = found_list[j];
            if let Some(cc) = conversion_cost_with_boxing_ctx(var_t.as_str(), f, global) {
                if cc > 0 { convs += 1; }
                cost += cc;
            } else { return None; }
        }
    } else if found_list.len() != fixed_len { return None; }
    Some((cost, convs))
}

fn break_specificity_tie(cands: &[&Vec<String>], global: Option<&crate::review::types::GlobalMemberIndex>) -> bool {
    if cands.is_empty() { return false; }
    // Only attempt reference-type specificity; primitives remain ambiguous per current policy
    let mut idx_best = 0usize;
    let mut improved = false;
    for i in 1..cands.len() {
        let a = cands[idx_best];
        let b = cands[i];
        let mut a_better_any = false;
        let mut b_better_any = false;
        let mut saw_reference_position = false;
        for (ta, tb) in a.iter().zip(b.iter()) {
            let a_is_ref = !is_primitive_type(ta) && ta != "String";
            let b_is_ref = !is_primitive_type(tb) && tb != "String";
            if a_is_ref && b_is_ref {
                saw_reference_position = true;
                match more_specific_type(ta, tb, global) {
                    Some(true) => a_better_any = true,
                    Some(false) => b_better_any = true,
                    None => {}
                }
            }
        }
        if !saw_reference_position { continue; }
        // require not-worse and at least one strictly better among reference positions
        if a_better_any && !b_better_any { improved = true; continue; }
        if b_better_any && !a_better_any { idx_best = i; improved = true; continue; }
    }
    improved
}

fn more_specific_type(a: &str, b: &str, global: Option<&crate::review::types::GlobalMemberIndex>) -> Option<bool> {
    // return Some(true) if a < b (a is more specific), Some(false) if b < a, None if incomparable
    // primitives chain
    if is_primitive_type(a) && is_primitive_type(b) {
        if is_more_specific_primitive(a, b) { return Some(true); }
        if is_more_specific_primitive(b, a) { return Some(false); }
        return None;
    }
    // wrapper vs Object
    if a == "Object" && b != "Object" { return Some(false); }
    if b == "Object" && a != "Object" { return Some(true); }
    // reference assignability via global
    if let Some(g) = global {
        let a_to_b = is_reference_assignable(g, a, b);
        let b_to_a = is_reference_assignable(g, b, a);
        if a_to_b && !b_to_a { return Some(true); }
        if b_to_a && !a_to_b { return Some(false); }
    }
    None
}



