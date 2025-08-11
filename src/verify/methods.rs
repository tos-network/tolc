use crate::codegen::class::ClassFile;
use crate::codegen::constpool::Constant;
use super::method_access_flags;
use crate::codegen::opcodes;
use crate::codegen::attribute::AttributeInfo;
// use crate::codegen::descriptor::type_to_descriptor; // currently unused

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum MethodVerifyError {
    #[error("Invalid constant pool index {0}")]
    InvalidConstantPoolIndex(u16),
    #[error("Invalid constant pool index type {0}")]
    InvalidConstantPoolIndexType(u16),
    #[error("Invalid method access flags: 0x{0:04x}")]
    InvalidMethodAccessFlags(u16),
    #[error("Method must have Code attribute unless abstract or native")]
    MissingCodeAttribute,
    #[error("Abstract or native method must not have Code attribute")]
    ForbiddenCodeAttribute,
    #[error("Invalid method attribute: {0}")]
    InvalidMethodAttribute(String),
    #[error("Duplicate method attribute: {0}")]
    DuplicateMethodAttribute(String),
    #[error("Return opcode does not match descriptor: expected {expected}, found {found}")]
    ReturnMismatch { expected: &'static str, found: String },
    #[error("Invalid method attribute: duplicate exceptions declared")]
    DuplicateExceptions,
}

pub type Result<T> = std::result::Result<T, MethodVerifyError>;

/// Verify the ClassFile methods
pub fn verify(class_file: &ClassFile) -> Result<()> {
    for method in &class_file.methods {
        // access flags
        if let Err(super::method_access_flags::MethodAccessFlagsError::Invalid(bits)) = 
            method_access_flags::verify(class_file, method)
        {
            return Err(MethodVerifyError::InvalidMethodAccessFlags(bits));
        }
        verify_name_index(class_file, method.name_index)?;
        verify_descriptor_index(class_file, method.descriptor_index)?;
        verify_method_attributes(class_file, method)?;
    }
    Ok(())
}

fn verify_name_index(class_file: &ClassFile, name_index: u16) -> Result<()> {
    let idx = (name_index as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(_)) => Ok(()),
        None => Err(MethodVerifyError::InvalidConstantPoolIndex(name_index)),
        _ => Err(MethodVerifyError::InvalidConstantPoolIndexType(name_index)),
    }
}

fn verify_descriptor_index(class_file: &ClassFile, descriptor_index: u16) -> Result<()> {
    let idx = (descriptor_index as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(_)) => Ok(()),
        None => Err(MethodVerifyError::InvalidConstantPoolIndex(descriptor_index)),
        _ => Err(MethodVerifyError::InvalidConstantPoolIndexType(descriptor_index)),
    }
}

fn verify_method_attributes(class_file: &ClassFile, method: &crate::codegen::method::MethodInfo) -> Result<()> {
    let mut has_code = false;
    let mut has_signature = false;
    let mut has_exceptions = false;
    let is_abstract = method.access_flags & crate::codegen::flag::access_flags::ACC_ABSTRACT != 0;
    let is_native = method.access_flags & crate::codegen::flag::access_flags::ACC_NATIVE != 0;

    for a in &method.attributes {
        match &a.info {
            AttributeInfo::Code(code_attr) => {
                if has_code { return Err(MethodVerifyError::DuplicateMethodAttribute("Code".to_string())); }
                has_code = true;
                // Code attribute: inner attributes sanity
                let mut saw_stackmap = false;
                let mut saw_lvt = false;
                let mut saw_lvtt = false;
                for inner in &code_attr.attributes {
                    match &inner.info {
                        AttributeInfo::StackMapTable(_) => {
                            if saw_stackmap {
                                return Err(MethodVerifyError::DuplicateMethodAttribute("StackMapTable".to_string()));
                            }
                            saw_stackmap = true;
                        }
                        AttributeInfo::LocalVariableTable(lvt) => {
                            if saw_lvt {
                                return Err(MethodVerifyError::DuplicateMethodAttribute("LocalVariableTable".to_string()));
                            }
                            saw_lvt = true;
                            let code_len_u16: u16 = code_attr.code.len().min(u16::MAX as usize) as u16;
                            for e in lvt.entries.iter() {
                                let end = e.start_pc.saturating_add(e.length);
                                if end > code_len_u16 {
                                    return Err(MethodVerifyError::InvalidMethodAttribute("LocalVariableTable out of range".to_string()));
                                }
                                let name_idx = (e.name_index as usize).saturating_sub(1);
                                let desc_idx = (e.descriptor_index as usize).saturating_sub(1);
                                match class_file.constant_pool.constants.get(name_idx) {
                                    Some(Constant::Utf8(_)) => {}
                                    None => return Err(MethodVerifyError::InvalidConstantPoolIndex(e.name_index)),
                                    _ => return Err(MethodVerifyError::InvalidConstantPoolIndexType(e.name_index)),
                                }
                                match class_file.constant_pool.constants.get(desc_idx) {
                                    Some(Constant::Utf8(_)) => {}
                                    None => return Err(MethodVerifyError::InvalidConstantPoolIndex(e.descriptor_index)),
                                    _ => return Err(MethodVerifyError::InvalidConstantPoolIndexType(e.descriptor_index)),
                                }
                            }
                        }
                        AttributeInfo::LocalVariableTypeTable(lvtt) => {
                            if saw_lvtt {
                                return Err(MethodVerifyError::DuplicateMethodAttribute("LocalVariableTypeTable".to_string()));
                            }
                            saw_lvtt = true;
                            for e in &lvtt.local_variable_type_table {
                                let name_idx = (e.name.as_u16() as usize).saturating_sub(1);
                                let sig_idx = (e.signature.as_u16() as usize).saturating_sub(1);
                                match class_file.constant_pool.constants.get(name_idx) {
                                    Some(Constant::Utf8(_)) => {}
                                    None => return Err(MethodVerifyError::InvalidConstantPoolIndex(e.name.as_u16())),
                                    _ => return Err(MethodVerifyError::InvalidConstantPoolIndexType(e.name.as_u16())),
                                }
                                match class_file.constant_pool.constants.get(sig_idx) {
                                    Some(Constant::Utf8(_)) => {}
                                    None => return Err(MethodVerifyError::InvalidConstantPoolIndex(e.signature.as_u16())),
                                    _ => return Err(MethodVerifyError::InvalidConstantPoolIndexType(e.signature.as_u16())),
                                }
                            }
                        }
                        _ => {}
                    }
                }
                // StackMapTable sanity: monotonic pc coverage within code length (minimal)
                {
                    use crate::codegen::frame::StackMapFrame;
                    let mut pc_accum: u32 = 0;
                    for inner in &code_attr.attributes {
                        if let AttributeInfo::StackMapTable(smt) = &inner.info {
                            let mut prev_locals_len: Option<usize> = None;
                            let mut prev_pc: i32 = -1; // per JVMS, first frame is at offset_delta from -1
                            for f in smt.stack_map_frames.iter() {
                                let (delta, locals_len, stack_len) = match f.clone() {
                                    StackMapFrame::Same { offset_delta }
                                        => (offset_delta as u32, prev_locals_len.unwrap_or(0), 0),
                                    StackMapFrame::SameLocals1StackItem { offset_delta, .. }
                                        => (offset_delta as u32, prev_locals_len.unwrap_or(0), 1),
                                    StackMapFrame::SameLocals1StackItemExtended { offset_delta, .. }
                                        => (offset_delta as u32, prev_locals_len.unwrap_or(0), 1),
                                    StackMapFrame::Chop { k, offset_delta } => {
                                        let cur = prev_locals_len.unwrap_or(0);
                                        let new_len = cur.saturating_sub(k as usize);
                                        (offset_delta as u32, new_len, 0)
                                    }
                                    StackMapFrame::SameExtended { offset_delta } => (offset_delta as u32, prev_locals_len.unwrap_or(0), 0),
                                    StackMapFrame::Append { k, offset_delta, .. } => {
                                        let cur = prev_locals_len.unwrap_or(0);
                                        let new_len = cur + k as usize;
                                        (offset_delta as u32, new_len, 0)
                                    }
                                    StackMapFrame::Full { offset_delta, locals, stack } => (offset_delta as u32, locals.len(), stack.len()),
                                };
                                // Convert to absolute PC (current frame location)
                                let current_pc = prev_pc.saturating_add(delta as i32 + 1);
                                if (current_pc as usize) > code_attr.code.len() { return Err(MethodVerifyError::InvalidMethodAttribute("StackMapTable pc exceeds code length".to_string())); }
                                // locals length must be within max_locals (approximate: count entries)
                                if locals_len > code_attr.max_locals as usize {
                                    return Err(MethodVerifyError::InvalidMethodAttribute("StackMapTable locals exceed max_locals".to_string()));
                                }
                                // stack length must be within max_stack
                                if stack_len > code_attr.max_stack as usize {
                                    return Err(MethodVerifyError::InvalidMethodAttribute("StackMapTable stack exceed max_stack".to_string()));
                                }
                                prev_locals_len = Some(locals_len);
                                prev_pc = current_pc;
                            }
                        }
                    }
                }
                
                // If method has a body, verify last opcode matches descriptor return
                let desc = cp_utf8(class_file, method.descriptor_index)?;
                if let Some(last) = code_attr.code.last() {
                    let expected = expected_return_for_descriptor(&desc);
                    if !matches_return_opcode(*last, &expected) {
                        return Err(MethodVerifyError::ReturnMismatch {
                            expected: expected.name,
                            found: format!("0x{:02x}", last),
                        });
                    }
                }
                // Optional sanity: ensure code length fits u32 declared length implicitly by vec size
                // and stack/local sizes are within u16 bounds (already by type), skip deeper checks here
            }
            AttributeInfo::Exceptions(ex_attr) => {
                if has_exceptions { return Err(MethodVerifyError::DuplicateMethodAttribute("Exceptions".to_string())); }
                has_exceptions = true;
                // Validate each exception index points to Class
                use std::collections::HashSet;
                let mut seen: HashSet<u16> = HashSet::new();
                for exc in ex_attr.exceptions.iter() {
                    let u = exc.as_u16();
                    if !seen.insert(u) { return Err(MethodVerifyError::DuplicateExceptions); }
                    let idx = (u as usize).saturating_sub(1);
                    match class_file.constant_pool.constants.get(idx) {
                        Some(Constant::Class(_)) => {}
                        None => return Err(MethodVerifyError::InvalidConstantPoolIndex(u)),
                        _ => return Err(MethodVerifyError::InvalidConstantPoolIndexType(u)),
                    }
                }
            }
            AttributeInfo::RuntimeVisibleParameterAnnotations(rvpa) => {
                let expected_params = descriptor_param_count(class_file, method.descriptor_index)?;
                if rvpa.annotations.len() != expected_params as usize {
                    return Err(MethodVerifyError::InvalidMethodAttribute(format!("RuntimeVisibleParameterAnnotations count {} != params {}", rvpa.annotations.len(), expected_params)));
                }
            }
            AttributeInfo::RuntimeInvisibleParameterAnnotations(ripa) => {
                let expected_params = descriptor_param_count(class_file, method.descriptor_index)?;
                if ripa.annotations.len() != expected_params as usize {
                    return Err(MethodVerifyError::InvalidMethodAttribute(format!("RuntimeInvisibleParameterAnnotations count {} != params {}", ripa.annotations.len(), expected_params)));
                }
            }
            AttributeInfo::Signature(sig_attr) => {
                if has_signature { return Err(MethodVerifyError::DuplicateMethodAttribute("Signature".to_string())); }
                has_signature = true;
                // Validate signature content minimally
                let s = cp_utf8(class_file, sig_attr.signature.as_u16())?;
                if !crate::verify::signature::is_valid_method_signature(&s) || !crate::verify::attributes::is_valid_signature(&s) {
                    return Err(MethodVerifyError::InvalidMethodAttribute("Invalid Signature".to_string()));
                }
            }
            AttributeInfo::Synthetic(_) | AttributeInfo::Deprecated(_) => {}
            AttributeInfo::RuntimeVisibleAnnotations(_)
            | AttributeInfo::RuntimeInvisibleAnnotations(_)
            | AttributeInfo::RuntimeVisibleTypeAnnotations(_)
            | AttributeInfo::RuntimeInvisibleTypeAnnotations(_)
            | AttributeInfo::MethodParameters(_) 
            | AttributeInfo::AnnotationDefault(_) => {}
            // Disallow non-method-only attributes
            AttributeInfo::ConstantValue(_)
            | AttributeInfo::LineNumberTable(_)
            | AttributeInfo::LocalVariableTable(_)
            | AttributeInfo::LocalVariableTypeTable(_)
            | AttributeInfo::StackMapTable(_)
            | AttributeInfo::InnerClasses(_)
            | AttributeInfo::EnclosingMethod(_)
            | AttributeInfo::SourceFile(_)
            | AttributeInfo::SourceDebugExtension(_)
            | AttributeInfo::BootstrapMethods(_)
            | AttributeInfo::Module(_)
            | AttributeInfo::ModulePackages(_)
            | AttributeInfo::ModuleMainClass(_)
            | AttributeInfo::NestHost(_)
            | AttributeInfo::NestMembers(_) => {
                return Err(MethodVerifyError::InvalidMethodAttribute(attribute_name(class_file, a)));
            }
                AttributeInfo::Custom(_) | AttributeInfo::Record(_) | AttributeInfo::PermittedSubclasses(_) => {
                return Err(MethodVerifyError::InvalidMethodAttribute(attribute_name(class_file, a)));
            }
        }
    }

    if is_abstract || is_native {
        if has_code { return Err(MethodVerifyError::ForbiddenCodeAttribute); }
    } else {
        if !has_code { return Err(MethodVerifyError::MissingCodeAttribute); }
    }

    Ok(())
}

fn descriptor_param_count(class_file: &ClassFile, desc_index: u16) -> Result<u16> {
    let idx = (desc_index as usize).saturating_sub(1);
    let desc = match class_file.constant_pool.constants.get(idx) {
        Some(crate::codegen::constpool::Constant::Utf8(s)) => s.clone(),
        None => return Err(MethodVerifyError::InvalidConstantPoolIndex(desc_index)),
        _ => return Err(MethodVerifyError::InvalidConstantPoolIndexType(desc_index)),
    };
    // Count parameters in a JVM descriptor like (I[Ljava/lang/String;D)V
    let mut chars = desc.chars();
    // expect '('
    while let Some(c) = chars.next() { if c == '(' { break; } }
    let mut count: u16 = 0;
    let mut _arr = false;
    while let Some(c) = chars.next() {
        match c {
            ')' => break,
            '[' => { _arr = true; },
            'B'|'C'|'D'|'F'|'I'|'J'|'S'|'Z' => { count += 1; _arr = false; },
            'L' => { // object; consume until ';'
                while let Some(cc) = chars.next() { if cc == ';' { break; } }
                count += 1; _arr = false;
            }
            _ => {}
        }
    }
    Ok(count)
}

fn attribute_name(class_file: &ClassFile, a: &crate::codegen::attribute::NamedAttribute) -> String {
    let idx = (a.name.as_u16() as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(s)) => s.clone(),
        _ => String::from("<invalid-name>"),
    }
}

struct ExpectedReturn { name: &'static str, opcodes: &'static [u8] }

fn expected_return_for_descriptor(descriptor: &str) -> ExpectedReturn {
    // descriptor format: (args)Ret
    let ret = descriptor.rsplit(')').next().unwrap_or("V");
    let mut chars = ret.chars();
    let c = chars.next().unwrap_or('V');
    match c {
        'V' => ExpectedReturn { name: "RETURN", opcodes: &[opcodes::RETURN] },
        'J' => ExpectedReturn { name: "LRETURN", opcodes: &[opcodes::LRETURN] },
        'F' => ExpectedReturn { name: "FRETURN", opcodes: &[opcodes::FRETURN] },
        'D' => ExpectedReturn { name: "DRETURN", opcodes: &[opcodes::DRETURN] },
        'I' | 'S' | 'B' | 'C' | 'Z' => ExpectedReturn { name: "IRETURN", opcodes: &[opcodes::IRETURN] },
        'L' | '[' => ExpectedReturn { name: "ARETURN", opcodes: &[opcodes::ARETURN] },
        _ => ExpectedReturn { name: "<unknown>", opcodes: &[] },
    }
}

fn matches_return_opcode(last: u8, expected: &ExpectedReturn) -> bool {
    expected.opcodes.contains(&last)
}

fn cp_utf8(class_file: &ClassFile, idx_u16: u16) -> Result<String> {
    let idx = (idx_u16 as usize).saturating_sub(1);
    match class_file.constant_pool.constants.get(idx) {
        Some(Constant::Utf8(s)) => Ok(s.clone()),
        None => Err(MethodVerifyError::InvalidConstantPoolIndex(idx_u16)),
        _ => Err(MethodVerifyError::InvalidConstantPoolIndexType(idx_u16)),
    }
}


