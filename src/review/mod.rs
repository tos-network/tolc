use crate::ast::*;
mod package;
mod imports;
mod types;
mod class;
mod interface;
mod annotation;
mod enums;
mod fields;
mod methods;
mod statements;
mod consts;

pub type ReviewResult<T> = Result<T, ReviewError>;

#[derive(thiserror::Error, Debug)]
pub enum ReviewError {
    #[error("Missing package name")]
    MissingPackage,
    #[error("Duplicate import: {0}")]
    DuplicateImport(String),
    #[error("Duplicate type declaration: {0}")]
    DuplicateType(String),
    #[error("Empty class name")]
    EmptyClassName,
    #[error("Class '{0}' cannot be both abstract and final")]
    ClassAbstractAndFinal(String),
    #[error("Interface '{0}' cannot be final")]
    InterfaceFinal(String),
    #[error("Duplicate member: {0}")]
    DuplicateMember(String),
    #[error("Illegal interface field modifiers for '{0}' (expected public static final)")]
    IllegalInterfaceFieldModifiers(String),
    #[error("Illegal interface method modifiers for '{0}'")]
    IllegalInterfaceMethodModifiers(String),
    #[error("Duplicate parameter name '{0}'")]
    DuplicateParameter(String),
    #[error("use of local variable '{0}' before definite assignment")]
    UseBeforeInit(String),
    #[error("cannot assign to final parameter '{0}'")]
    AssignToFinal(String),
    #[error("cannot assign to final field '{0}'")]
    AssignToFinalField(String),
    #[error("At most one varargs constructor is allowed per class")]
    MultipleVarargsConstructors,
    #[error("method call '{name}({found})' does not match any arity; expected one of: {expected}")]
    MethodCallArityMismatch { name: String, expected: String, found: usize },
    #[error("Duplicate local variable '{0}' in the same scope")]
    DuplicateLocalVar(String),
    #[error("Incompatible initializer: expected {expected}, found {found}")]
    IncompatibleInitializer { expected: String, found: String },
    #[error("inapplicable method '{name}' for argument types [{found}]; no match among [{expected}]")]
    InapplicableMethod { name: String, expected: String, found: String },
    #[error("ambiguous method '{name}' for argument types [{found}]; candidates: {candidates}")]
    AmbiguousMethod { name: String, candidates: String, found: String },
    #[error("illegal static call: '{typename}::{name}' is not static in this context")]
    IllegalStaticCall { typename: String, name: String },
    #[error("type '{typename}' expects {expected} type argument(s); found {found}")]
    GenericArityMismatch { typename: String, expected: usize, found: usize },
    #[error("type argument '{found}' for '{typename}' does not satisfy upper bound '{bound}'")]
    GenericBoundViolation { typename: String, bound: String, found: String },
    #[error("enum '{0}' constructors cannot be public or protected")]
    IllegalEnumConstructorVisibility(String),
    #[error("illegal modifier combination for constructor '{0}'")]
    IllegalConstructorModifiers(String),
    #[error("unreported checked exception '{0}' thrown")]
    UnreportedCheckedException(String),
    #[error("final field '{0}' must be assigned exactly once in each constructor path or have an initializer")]
    FinalFieldNotAssigned(String),
    #[error("final field '{0}' is assigned more than once in constructor")]
    FinalFieldMultipleAssignment(String),
    #[error("final field '{0}' already has an initializer; cannot assign again in constructor")]
    FinalFieldAssignedInConstructorWithInitializer(String),
    #[error("unreachable statement")]
    UnreachableStatement,
    #[error("duplicate switch case label '{0}'")]
    DuplicateSwitchCaseLabel(String),
    #[error("multiple default labels in switch")] 
    MultipleSwitchDefaults,
    #[error("conflicting default methods for '{0}'; class must override to resolve")] 
    ConflictingInterfaceDefaults(String),
    #[error("class must implement interface method '{0}'")]
    MissingInterfaceMethodImplementation(String),
    #[error("wildcard type arguments are not allowed in object instantiation")]
    WildcardNotAllowedInNew,
    #[error("inaccessible member '{name}' in '{typename}'")]
    InaccessibleMember { typename: String, name: String },
    #[error("division or modulo by zero in constant expression")]
    DivisionByZeroConstant,
}

/// AST-level review before codegen
pub fn review(ast: &Ast) -> ReviewResult<()> {
    log::debug!("review start: types={} imports={}", ast.type_decls.len(), ast.imports.len());
    self::package::review_package(ast)?;
    self::imports::review_imports(ast)?;
    types::review_types(ast)?;
    log::debug!("review end: ok");
    Ok(())
}

#[inline]
pub(crate) fn debug_log(msg: impl AsRef<str>) {
    if std::env::var("TOLC_DEBUG").is_ok() {
        eprintln!("[tolc-debug] {}", msg.as_ref());
    }
}

#[inline]
pub(crate) fn compat_mode() -> bool {
    std::env::var("TOLC_JAVAC_COMPAT").is_ok()
}

// Per-kind checks are implemented in dedicated modules for clarity

