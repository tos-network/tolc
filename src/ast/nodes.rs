use super::{AstNode, AstVisitor, Span};
use std::fmt;

// Package and Import Declarations
#[derive(Debug, Clone)]
pub struct PackageDecl {
    pub name: String,
    pub span: Span,
}

impl AstNode for PackageDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_package_decl(self)
    }
}

impl fmt::Display for PackageDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "package {};", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub name: String,
    pub is_static: bool,
    pub is_wildcard: bool,
    pub span: Span,
}

impl AstNode for ImportDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_import_decl(self)
    }
}

impl fmt::Display for ImportDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_static {
            write!(f, "import static ")?;
        } else {
            write!(f, "import ")?;
        }
        
        if self.is_wildcard {
            write!(f, "{}.*;", self.name)
        } else {
            write!(f, "{};", self.name)
        }
    }
}

// Type Declarations
#[derive(Debug, Clone)]
pub enum TypeDecl {
    Class(ClassDecl),
    Interface(InterfaceDecl),
    Enum(EnumDecl),
    Annotation(AnnotationDecl),
}

impl AstNode for TypeDecl {
    fn span(&self) -> Span {
        match self {
            TypeDecl::Class(c) => c.span(),
            TypeDecl::Interface(i) => i.span(),
            TypeDecl::Enum(e) => e.span(),
            TypeDecl::Annotation(a) => a.span(),
        }
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        match self {
            TypeDecl::Class(c) => c.accept(visitor),
            TypeDecl::Interface(i) => i.accept(visitor),
            TypeDecl::Enum(e) => e.accept(visitor),
            TypeDecl::Annotation(a) => a.accept(visitor),
        }
    }
}

impl fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeDecl::Class(c) => write!(f, "{}", c),
            TypeDecl::Interface(i) => write!(f, "{}", i),
            TypeDecl::Enum(e) => write!(f, "{}", e),
            TypeDecl::Annotation(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub extends: Option<TypeRef>,
    pub implements: Vec<TypeRef>,
    pub body: Vec<ClassMember>,
    pub span: Span,
}

impl AstNode for ClassDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_class_decl(self)
    }
}

impl fmt::Display for ClassDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "class {}", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub extends: Vec<TypeRef>,
    pub body: Vec<InterfaceMember>,
    pub span: Span,
}

impl AstNode for InterfaceDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_interface_decl(self)
    }
}

impl fmt::Display for InterfaceDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "interface {}", self.name)
    }
}

// Modifiers and Annotations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modifier {
    Public,
    Protected,
    Private,
    Abstract,
    Static,
    Final,
    Native,
    Synchronized,
    Transient,
    Volatile,
    Strictfp,
    // Java 8: default interface method
    Default,
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub name: String,
    pub arguments: Vec<AnnotationArg>,
    pub span: Span,
}

impl AstNode for Annotation {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_annotation(self)
    }
}

#[derive(Debug, Clone)]
pub enum AnnotationArg {
    Value(Expr),
    Named(String, Expr),
}

// Type References
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub name: String,
    pub type_args: Vec<TypeArg>,
    pub annotations: Vec<Annotation>,
    pub array_dims: usize,
    pub span: Span,
}

impl AstNode for TypeRef {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_type_ref(self)
    }
}

#[derive(Debug, Clone)]
pub enum TypeArg {
    Type(TypeRef),
    Wildcard(WildcardType),
}

#[derive(Debug, Clone)]
pub struct WildcardType {
    pub bound: Option<(BoundKind, TypeRef)>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum BoundKind { Extends, Super }

#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<TypeRef>,
    pub span: Span,
}

impl AstNode for TypeParam {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_type_param(self)
    }
}

// Class and Interface Members
#[derive(Debug, Clone)]
pub enum ClassMember {
    Field(FieldDecl),
    Method(MethodDecl),
    Constructor(ConstructorDecl),
    Initializer(InitializerBlock),
    TypeDecl(TypeDecl),
}

#[derive(Debug, Clone)]
pub enum InterfaceMember {
    Method(MethodDecl),
    Field(FieldDecl),
    TypeDecl(TypeDecl),
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub type_ref: TypeRef,
    pub name: String,
    pub initializer: Option<Expr>,
    pub span: Span,
}

impl AstNode for FieldDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_field_decl(self)
    }
}

#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub type_params: Vec<TypeParam>,
    pub return_type: Option<TypeRef>,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub throws: Vec<TypeRef>,
    pub body: Option<Block>,
    pub span: Span,
}

impl AstNode for MethodDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_method_decl(self)
    }
}

#[derive(Debug, Clone)]
pub struct ConstructorDecl {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub throws: Vec<TypeRef>,
    pub explicit_invocation: Option<ExplicitCtorInvocation>,
    pub body: Block,
    pub span: Span,
}

impl AstNode for ConstructorDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_constructor_decl(self)
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub type_ref: TypeRef,
    pub name: String,
    pub varargs: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExplicitCtorInvocation {
    This { arg_count: usize },
    Super { arg_count: usize },
}

impl AstNode for Parameter {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_parameter(self)
    }
}

// Statements and Expressions
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

impl AstNode for Block {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_block(self)
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(ExprStmt),
    Declaration(VarDeclStmt),
    TypeDecl(TypeDecl),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Switch(SwitchStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Try(TryStmt),
    Throw(ThrowStmt),
    Assert(AssertStmt),
    Synchronized(SynchronizedStmt),
    Labeled(LabeledStmt),
    Block(Block),
    Empty,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub modifiers: Vec<Modifier>,
    pub type_ref: TypeRef,
    pub variables: Vec<VariableDeclarator>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub name: String,
    pub array_dims: usize,
    pub initializer: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Vec<Stmt>,
    pub condition: Option<Expr>,
    pub update: Vec<ExprStmt>,
    pub body: Box<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SwitchStmt {
    pub expression: Expr,
    pub cases: Vec<SwitchCase>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub labels: Vec<Expr>, // empty labels indicates default
    pub statements: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub label: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt {
    pub label: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TryStmt {
    pub resources: Vec<TryResource>,
    pub try_block: Block,
    pub catch_clauses: Vec<CatchClause>,
    pub finally_block: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CatchClause {
    pub parameter: Parameter,
    // Additional alternatives for multi-catch: catch (A | B e)
    pub alt_types: Vec<TypeRef>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ThrowStmt {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TryResource {
    Var {
        modifiers: Vec<Modifier>,
        type_ref: TypeRef,
        name: String,
        initializer: Expr,
        span: Span,
    },
    Expr {
        expr: Expr,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct AssertStmt {
    pub condition: Expr,
    pub message: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SynchronizedStmt {
    pub lock: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LabeledStmt {
    pub label: String,
    pub statement: Box<Stmt>,
    pub span: Span,
}

// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralExpr),
    Identifier(IdentifierExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Assignment(AssignmentExpr),
    MethodCall(MethodCallExpr),
    FieldAccess(FieldAccessExpr),
    ArrayAccess(ArrayAccessExpr),
    Cast(CastExpr),
    InstanceOf(InstanceOfExpr),
    Conditional(ConditionalExpr),
    New(NewExpr),
    Parenthesized(Box<Expr>),
    // Used for annotation element array values like @Target({A,B})
    ArrayInitializer(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Char(char),
    Null,
}

#[derive(Debug, Clone)]
pub struct IdentifierExpr {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOp,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Lt, Le, Gt, Ge, Eq, Ne,
    And, Or, Xor, LShift, RShift, URShift,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus, Minus, Not, BitNot, PreInc, PreDec, PostInc, PostDec,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AssignmentOp {
    Assign, AddAssign, SubAssign, MulAssign, DivAssign, ModAssign,
    AndAssign, OrAssign, XorAssign, LShiftAssign, RShiftAssign, URShiftAssign,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub target: Box<Expr>,
    pub operator: AssignmentOp,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub target: Option<Box<Expr>>,
    pub name: String,
    pub arguments: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    pub target: Option<Box<Expr>>,
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayAccessExpr {
    pub array: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CastExpr {
    pub target_type: TypeRef,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct InstanceOfExpr {
    pub expr: Box<Expr>,
    pub target_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConditionalExpr {
    pub condition: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NewExpr {
    pub target_type: TypeRef,
    pub arguments: Vec<Expr>,
    pub anonymous_body: Option<ClassDecl>,
    pub span: Span,
}

// Additional structures
#[derive(Debug, Clone)]
pub struct InitializerBlock {
    pub modifiers: Vec<Modifier>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub implements: Vec<TypeRef>,
    pub constants: Vec<EnumConstant>,
    pub body: Vec<ClassMember>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumConstant {
    pub name: String,
    pub arguments: Vec<Expr>,
    pub body: Option<ClassDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AnnotationDecl {
    pub modifiers: Vec<Modifier>,
    pub annotations: Vec<Annotation>,
    pub name: String,
    pub body: Vec<AnnotationMember>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AnnotationMember {
    pub type_ref: TypeRef,
    pub name: String,
    pub default_value: Option<Expr>,
    pub span: Span,
}

// Type aliases for compatibility with codegen module
pub type Type = TypeRef;
pub type Statement = Stmt;
pub type Expression = Expr;
pub type BinaryOperator = BinaryOp;
pub type UnaryOperator = UnaryOp;

// Primitive type enum for codegen compatibility
#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Boolean,
    Char,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
}

// Reference type enum for codegen compatibility  
#[derive(Debug, Clone)]
pub enum ReferenceType {
    Class(String),
    Interface(String),
    Array(Box<TypeRef>),
}

// Extension trait to convert TypeRef to Type enum
pub trait TypeExt {
    fn as_type_enum(&self) -> TypeEnum;
}

impl TypeExt for TypeRef {
    fn as_type_enum(&self) -> TypeEnum {
        if self.array_dims > 0 {
            TypeEnum::Reference(ReferenceType::Array(Box::new(self.clone())))
        } else if self.name == "boolean" || self.name == "char" || self.name == "byte" || 
                  self.name == "short" || self.name == "int" || self.name == "long" || 
                  self.name == "float" || self.name == "double" {
            let prim_type = match self.name.as_str() {
                "boolean" => PrimitiveType::Boolean,
                "char" => PrimitiveType::Char,
                "byte" => PrimitiveType::Byte,
                "short" => PrimitiveType::Short,
                "int" => PrimitiveType::Int,
                "long" => PrimitiveType::Long,
                "float" => PrimitiveType::Float,
                "double" => PrimitiveType::Double,
                _ => PrimitiveType::Int, // fallback
            };
            TypeEnum::Primitive(prim_type)
        } else {
            TypeEnum::Reference(ReferenceType::Class(self.name.clone()))
        }
    }
}

impl AstNode for EnumDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_enum_decl(self)
    }
}

impl fmt::Display for EnumDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum {}", self.name)
    }
}

impl AstNode for AnnotationDecl {
    fn span(&self) -> Span {
        self.span
    }
    
    fn accept<V: AstVisitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_annotation_decl(self)
    }
}

impl fmt::Display for AnnotationDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@interface {}", self.name)
    }
}

// Type enum for codegen compatibility
#[derive(Debug, Clone)]
pub enum TypeEnum {
    Primitive(PrimitiveType),
    Reference(ReferenceType),
    Void,
}

impl From<TypeRef> for TypeEnum {
    fn from(type_ref: TypeRef) -> Self {
        type_ref.as_type_enum()
    }
}

impl From<&TypeRef> for TypeEnum {
    fn from(type_ref: &TypeRef) -> Self {
        type_ref.as_type_enum()
    }
}

