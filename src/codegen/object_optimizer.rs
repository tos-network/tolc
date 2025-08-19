/// Object creation and type operation optimization (javac-style)
/// Implements javac's sophisticated object instantiation and type checking optimizations

use crate::codegen::opcodes;
use crate::ast::{Expr, TypeRef, NewExpr};

#[derive(Debug, Clone)]
pub struct ObjectCreationPattern {
    pub target_type: TypeRef,
    pub constructor_args: Vec<Expr>,
    pub optimization_type: ObjectOptimizationType,
}

#[derive(Debug, Clone)]
pub enum ObjectOptimizationType {
    /// Standard new + dup + invokespecial pattern
    StandardConstruction,
    /// Array creation with known size
    ArrayCreation { dimensions: usize, element_type: TypeRef },
    /// Array initialization with literal values
    ArrayInitialization { elements: Vec<Expr> },
    /// String literal optimization
    StringLiteral(String),
    /// Wrapper type boxing
    BoxingOperation { primitive_type: String },
}

pub struct ObjectOptimizer;

impl ObjectOptimizer {
    /// Analyze object creation pattern (javac visitNewClass pattern)
    pub fn analyze_object_creation(expr: &NewExpr) -> ObjectCreationPattern {
        let target_type = expr.target_type.clone();
        let args = expr.arguments.clone();
        
        // Determine optimization type
        let optimization_type = if Self::is_array_type(&target_type) {
            if args.is_empty() {
                ObjectOptimizationType::ArrayCreation {
                    dimensions: target_type.array_dims as usize,
                    element_type: TypeRef {
                        name: target_type.name.clone(),
                        array_dims: 0,
                        type_args: target_type.type_args.clone(),
                        annotations: target_type.annotations.clone(),
                        span: target_type.span,
                    },
                }
            } else {
                ObjectOptimizationType::ArrayInitialization {
                    elements: args.clone(),
                }
            }
        } else if target_type.name == "String" && args.len() == 1 {
            // String constructor optimization
            if let Some(string_value) = Self::extract_string_literal(&args[0]) {
                ObjectOptimizationType::StringLiteral(string_value)
            } else {
                ObjectOptimizationType::StandardConstruction
            }
        } else if Self::is_wrapper_type(&target_type.name) {
            ObjectOptimizationType::BoxingOperation {
                primitive_type: Self::get_primitive_type(&target_type.name),
            }
        } else {
            ObjectOptimizationType::StandardConstruction
        };
        
        ObjectCreationPattern {
            target_type,
            constructor_args: args,
            optimization_type,
        }
    }
    
    /// Generate optimized object creation bytecode (javac visitNewClass pattern)
    pub fn generate_optimized_creation(pattern: &ObjectCreationPattern) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        match &pattern.optimization_type {
            ObjectOptimizationType::StandardConstruction => {
                // javac pattern: new + dup + args + invokespecial
                bytecode.push(opcodes::NEW);
                bytecode.extend_from_slice(&Self::type_constant_index(&pattern.target_type).to_be_bytes());
                
                bytecode.push(opcodes::DUP);
                
                // Generate constructor arguments (placeholder)
                for _arg in &pattern.constructor_args {
                    // Would generate argument loading code here
                }
                
                // invokespecial <init>
                bytecode.push(opcodes::INVOKESPECIAL);
                bytecode.extend_from_slice(&Self::constructor_method_index(&pattern.target_type).to_be_bytes());
            }
            
            ObjectOptimizationType::ArrayCreation { dimensions, element_type } => {
                // Generate array creation (javac visitNewArray pattern)
                bytecode.extend_from_slice(&Self::generate_array_creation(*dimensions, element_type));
            }
            
            ObjectOptimizationType::ArrayInitialization { elements } => {
                // Generate array with initialization (javac pattern)
                bytecode.extend_from_slice(&Self::generate_array_initialization(elements, &pattern.target_type));
            }
            
            ObjectOptimizationType::StringLiteral(value) => {
                // Optimize string constructor to direct ldc
                bytecode.push(opcodes::LDC);
                bytecode.push(Self::string_constant_index(value) as u8);
            }
            
            ObjectOptimizationType::BoxingOperation { primitive_type } => {
                // Generate boxing operation
                bytecode.extend_from_slice(&Self::generate_boxing_operation(primitive_type));
            }
        }
        
        bytecode
    }
    
    /// Generate array creation bytecode (javac visitNewArray pattern)
    fn generate_array_creation(dimensions: usize, element_type: &TypeRef) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        if dimensions == 1 {
            // Single dimension array
            if Self::is_primitive_type(&element_type.name) {
                // newarray for primitive types
                bytecode.push(opcodes::NEWARRAY);
                bytecode.push(Self::primitive_array_type_code(&element_type.name));
            } else {
                // anewarray for reference types
                bytecode.push(opcodes::ANEWARRAY);
                bytecode.extend_from_slice(&Self::type_constant_index(element_type).to_be_bytes());
            }
        } else {
            // Multi-dimensional array
            bytecode.push(opcodes::MULTIANEWARRAY);
            bytecode.extend_from_slice(&Self::type_constant_index(element_type).to_be_bytes());
            bytecode.push(dimensions as u8);
        }
        
        bytecode
    }
    
    /// Generate array initialization bytecode (javac pattern)
    fn generate_array_initialization(elements: &[Expr], array_type: &TypeRef) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        // Load array length (javac loadIntConst pattern)
        bytecode.extend_from_slice(&Self::load_int_const(elements.len()));
        
        // Create array
        let element_type = TypeRef {
            name: array_type.name.clone(),
            array_dims: array_type.array_dims - 1,
            type_args: array_type.type_args.clone(),
            annotations: array_type.annotations.clone(),
            span: array_type.span,
        };
        bytecode.extend_from_slice(&Self::generate_array_creation(1, &element_type));
        
        // Initialize elements (javac pattern: arr.duplicate + loadIntConst(i) + value + store)
        for (i, _element) in elements.iter().enumerate() {
            bytecode.push(opcodes::DUP); // arr.duplicate()
            bytecode.extend_from_slice(&Self::load_int_const(i)); // loadIntConst(i)
            
            // Generate element value (placeholder)
            // Would generate element expression here
            
            // Store element
            if Self::is_primitive_type(&element_type.name) {
                bytecode.push(Self::primitive_array_store_opcode(&element_type.name));
            } else {
                bytecode.push(opcodes::AASTORE);
            }
        }
        
        bytecode
    }
    
    /// Generate boxing operation (javac pattern)
    fn generate_boxing_operation(primitive_type: &str) -> Vec<u8> {
        let mut bytecode = Vec::new();
        
        let wrapper_type = Self::get_wrapper_type(primitive_type);
        
        // Load primitive value (would be generated by caller)
        // invokestatic WrapperType.valueOf(primitive)WrapperType
        bytecode.push(opcodes::INVOKESTATIC);
        bytecode.extend_from_slice(&Self::boxing_method_index(&wrapper_type).to_be_bytes());
        
        bytecode
    }
    
    /// Load integer constant (javac loadIntConst pattern)
    fn load_int_const(value: usize) -> Vec<u8> {
        match value {
            0 => vec![opcodes::ICONST_0],
            1 => vec![opcodes::ICONST_1],
            2 => vec![opcodes::ICONST_2],
            3 => vec![opcodes::ICONST_3],
            4 => vec![opcodes::ICONST_4],
            5 => vec![opcodes::ICONST_5],
            6..=127 => vec![opcodes::BIPUSH, value as u8],
            128..=32767 => {
                let mut bytes = vec![opcodes::SIPUSH];
                bytes.extend_from_slice(&(value as i16).to_be_bytes());
                bytes
            }
            _ => {
                // Use ldc for larger values
                vec![opcodes::LDC, 1] // Placeholder constant pool index
            }
        }
    }
    
    /// Check if type is array type
    fn is_array_type(type_ref: &TypeRef) -> bool {
        type_ref.array_dims > 0
    }
    
    /// Check if type is primitive
    fn is_primitive_type(type_name: &str) -> bool {
        matches!(type_name, "boolean" | "char" | "byte" | "short" | "int" | "long" | "float" | "double")
    }
    
    /// Check if type is wrapper type
    fn is_wrapper_type(type_name: &str) -> bool {
        matches!(type_name, "Boolean" | "Character" | "Byte" | "Short" | "Integer" | "Long" | "Float" | "Double")
    }
    
    /// Get primitive type for wrapper
    fn get_primitive_type(wrapper_type: &str) -> String {
        match wrapper_type {
            "Boolean" => "boolean",
            "Character" => "char",
            "Byte" => "byte",
            "Short" => "short",
            "Integer" => "int",
            "Long" => "long",
            "Float" => "float",
            "Double" => "double",
            _ => "int", // fallback
        }.to_string()
    }
    
    /// Get wrapper type for primitive
    fn get_wrapper_type(primitive_type: &str) -> String {
        match primitive_type {
            "boolean" => "Boolean",
            "char" => "Character",
            "byte" => "Byte",
            "short" => "Short",
            "int" => "Integer",
            "long" => "Long",
            "float" => "Float",
            "double" => "Double",
            _ => "Integer", // fallback
        }.to_string()
    }
    
    /// Get primitive array type code
    fn primitive_array_type_code(primitive_type: &str) -> u8 {
        match primitive_type {
            "boolean" => 4,
            "char" => 5,
            "float" => 6,
            "double" => 7,
            "byte" => 8,
            "short" => 9,
            "int" => 10,
            "long" => 11,
            _ => 10, // default to int
        }
    }
    
    /// Get primitive array store opcode
    fn primitive_array_store_opcode(primitive_type: &str) -> u8 {
        match primitive_type {
            "boolean" | "byte" => opcodes::BASTORE,
            "char" => opcodes::CASTORE,
            "short" => opcodes::SASTORE,
            "int" => opcodes::IASTORE,
            "long" => opcodes::LASTORE,
            "float" => opcodes::FASTORE,
            "double" => opcodes::DASTORE,
            _ => opcodes::IASTORE, // default
        }
    }
    
    /// Extract string literal from expression
    fn extract_string_literal(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Literal(literal_expr) => {
                match &literal_expr.value {
                    crate::ast::Literal::String(s) => Some(s.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }
    
    /// Placeholder constant pool indices (would be provided by caller)
    fn type_constant_index(_type_ref: &TypeRef) -> u16 { 1 }
    fn constructor_method_index(_type_ref: &TypeRef) -> u16 { 2 }
    fn string_constant_index(_value: &str) -> u16 { 3 }
    fn boxing_method_index(_wrapper_type: &str) -> u16 { 4 }
}

/// Type casting optimization (javac visitTypeCast pattern)
pub struct TypeCastOptimizer;

impl TypeCastOptimizer {
    /// Analyze type cast necessity (javac visitTypeCast logic)
    pub fn analyze_cast_necessity(source_type: &TypeRef, target_type: &TypeRef) -> CastAnalysis {
        if Self::is_primitive_cast(source_type, target_type) {
            CastAnalysis {
                needs_cast_instruction: false,
                cast_type: CastType::PrimitiveCoercion,
                optimization_benefit: OptimizationLevel::High,
            }
        } else if Self::is_upcast(source_type, target_type) {
            CastAnalysis {
                needs_cast_instruction: false,
                cast_type: CastType::Upcast,
                optimization_benefit: OptimizationLevel::High,
            }
        } else if Self::is_identity_cast(source_type, target_type) {
            CastAnalysis {
                needs_cast_instruction: false,
                cast_type: CastType::Identity,
                optimization_benefit: OptimizationLevel::Maximum,
            }
        } else {
            CastAnalysis {
                needs_cast_instruction: true,
                cast_type: CastType::Downcast,
                optimization_benefit: OptimizationLevel::None,
            }
        }
    }
    
    /// Generate optimized cast bytecode (javac pattern)
    pub fn generate_optimized_cast(analysis: &CastAnalysis, target_type: &TypeRef) -> Vec<u8> {
        if !analysis.needs_cast_instruction {
            // No checkcast needed - javac optimization
            Vec::new()
        } else {
            // Generate checkcast instruction
            let mut bytecode = Vec::new();
            bytecode.push(opcodes::CHECKCAST);
            bytecode.extend_from_slice(&ObjectOptimizer::type_constant_index(target_type).to_be_bytes());
            bytecode
        }
    }
    
    /// Check if cast is between primitive types
    fn is_primitive_cast(source: &TypeRef, target: &TypeRef) -> bool {
        ObjectOptimizer::is_primitive_type(&source.name) && ObjectOptimizer::is_primitive_type(&target.name)
    }
    
    /// Check if cast is an upcast (always safe)
    fn is_upcast(source: &TypeRef, target: &TypeRef) -> bool {
        // Simplified type hierarchy check
        if target.name == "Object" {
            return true; // Everything extends Object
        }
        
        // Check common inheritance patterns
        match (source.name.as_str(), target.name.as_str()) {
            ("String", "Object") => true,
            ("Integer", "Number") => true,
            ("Integer", "Object") => true,
            // Add more inheritance relationships as needed
            _ => false,
        }
    }
    
    /// Check if cast is identity (same type)
    fn is_identity_cast(source: &TypeRef, target: &TypeRef) -> bool {
        source.name == target.name && source.array_dims == target.array_dims
    }
}

#[derive(Debug, Clone)]
pub struct CastAnalysis {
    pub needs_cast_instruction: bool,
    pub cast_type: CastType,
    pub optimization_benefit: OptimizationLevel,
}

#[derive(Debug, Clone)]
pub enum CastType {
    Identity,
    Upcast,
    Downcast,
    PrimitiveCoercion,
}

#[derive(Debug, Clone)]
pub enum OptimizationLevel {
    None,
    Low,
    Medium,
    High,
    Maximum,
}

/// instanceof optimization (javac visitTypeTest pattern)
pub struct InstanceOfOptimizer;

impl InstanceOfOptimizer {
    /// Analyze instanceof operation
    pub fn analyze_instanceof(expr_type: &TypeRef, test_type: &TypeRef) -> InstanceOfAnalysis {
        if TypeCastOptimizer::is_identity_cast(expr_type, test_type) {
            InstanceOfAnalysis {
                can_optimize_to_constant: true,
                constant_result: Some(true),
                optimization_benefit: OptimizationLevel::Maximum,
            }
        } else if Self::is_impossible_instanceof(expr_type, test_type) {
            InstanceOfAnalysis {
                can_optimize_to_constant: true,
                constant_result: Some(false),
                optimization_benefit: OptimizationLevel::High,
            }
        } else {
            InstanceOfAnalysis {
                can_optimize_to_constant: false,
                constant_result: None,
                optimization_benefit: OptimizationLevel::None,
            }
        }
    }
    
    /// Generate optimized instanceof bytecode
    pub fn generate_optimized_instanceof(analysis: &InstanceOfAnalysis, test_type: &TypeRef) -> Vec<u8> {
        if let Some(constant_result) = analysis.constant_result {
            // Optimize to constant
            if constant_result {
                vec![opcodes::ICONST_1]
            } else {
                vec![opcodes::POP, opcodes::ICONST_0] // Pop expr, push false
            }
        } else {
            // Generate instanceof instruction
            let mut bytecode = Vec::new();
            bytecode.push(opcodes::INSTANCEOF);
            bytecode.extend_from_slice(&ObjectOptimizer::type_constant_index(test_type).to_be_bytes());
            bytecode
        }
    }
    
    /// Check if instanceof is impossible (disjoint types)
    fn is_impossible_instanceof(expr_type: &TypeRef, test_type: &TypeRef) -> bool {
        // Simplified disjoint type check
        if ObjectOptimizer::is_primitive_type(&expr_type.name) && !ObjectOptimizer::is_primitive_type(&test_type.name) {
            return true;
        }
        
        // Check for obviously disjoint types
        match (expr_type.name.as_str(), test_type.name.as_str()) {
            ("String", "Integer") => true,
            ("Integer", "String") => true,
            // Add more disjoint type pairs as needed
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstanceOfAnalysis {
    pub can_optimize_to_constant: bool,
    pub constant_result: Option<bool>,
    pub optimization_benefit: OptimizationLevel,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, LiteralExpr, Literal};
    
    #[test]
    fn test_object_creation_analysis() {
        let new_expr = NewExpr {
            target_type: TypeRef {
                name: "String".to_string(),
                array_dims: 0,
                type_args: Vec::new(),
                annotations: Vec::new(),
                span: Span::from_to(0, 0, 0, 0),
            },
            arguments: vec![
                Expr::Literal(LiteralExpr {
                    value: Literal::String("Hello".to_string()),
                    span: Span::from_to(0, 0, 0, 0),
                })
            ],
            anonymous_body: None,
            span: Span::from_to(0, 0, 0, 0),
        };
        
        let pattern = ObjectOptimizer::analyze_object_creation(&new_expr);
        
        match pattern.optimization_type {
            ObjectOptimizationType::StringLiteral(ref s) => {
                assert_eq!(s, "Hello");
            }
            _ => panic!("Expected StringLiteral optimization"),
        }
    }
    
    #[test]
    fn test_array_creation_analysis() {
        let new_expr = NewExpr {
            target_type: TypeRef {
                name: "int".to_string(),
                array_dims: 1,
                type_args: Vec::new(),
                annotations: Vec::new(),
                span: Span::from_to(0, 0, 0, 0),
            },
            arguments: Vec::new(),
            anonymous_body: None,
            span: Span::from_to(0, 0, 0, 0),
        };
        
        let pattern = ObjectOptimizer::analyze_object_creation(&new_expr);
        
        match pattern.optimization_type {
            ObjectOptimizationType::ArrayCreation { dimensions, .. } => {
                assert_eq!(dimensions, 1);
            }
            _ => panic!("Expected ArrayCreation optimization"),
        }
    }
    
    #[test]
    fn test_cast_analysis() {
        let source_type = TypeRef {
            name: "String".to_string(),
            array_dims: 0,
            type_args: Vec::new(),
            annotations: Vec::new(),
            span: Span::from_to(0, 0, 0, 0),
        };
        
        let target_type = TypeRef {
            name: "Object".to_string(),
            array_dims: 0,
            type_args: Vec::new(),
            annotations: Vec::new(),
            span: Span::from_to(0, 0, 0, 0),
        };
        
        let analysis = TypeCastOptimizer::analyze_cast_necessity(&source_type, &target_type);
        
        assert!(!analysis.needs_cast_instruction); // Upcast doesn't need checkcast
        assert!(matches!(analysis.cast_type, CastType::Upcast));
    }
    
    #[test]
    fn test_instanceof_analysis() {
        let expr_type = TypeRef {
            name: "String".to_string(),
            array_dims: 0,
            type_args: Vec::new(),
            annotations: Vec::new(),
            span: Span::from_to(0, 0, 0, 0),
        };
        
        let test_type = TypeRef {
            name: "String".to_string(),
            array_dims: 0,
            type_args: Vec::new(),
            annotations: Vec::new(),
            span: Span::from_to(0, 0, 0, 0),
        };
        
        let analysis = InstanceOfOptimizer::analyze_instanceof(&expr_type, &test_type);
        
        assert!(analysis.can_optimize_to_constant);
        assert_eq!(analysis.constant_result, Some(true));
    }
    
    #[test]
    fn test_load_int_const_optimization() {
        assert_eq!(ObjectOptimizer::load_int_const(0), vec![opcodes::ICONST_0]);
        assert_eq!(ObjectOptimizer::load_int_const(3), vec![opcodes::ICONST_3]);
        assert_eq!(ObjectOptimizer::load_int_const(10), vec![opcodes::BIPUSH, 10]);
        assert_eq!(ObjectOptimizer::load_int_const(1000), vec![opcodes::SIPUSH, 3, 232]); // 1000 = 0x03E8
    }
}
