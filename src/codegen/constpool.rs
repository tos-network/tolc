use thiserror::Error;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ConstPoolError {
    #[error("Constant pool size limit exceeded: current={current}, adding={adding}, max={max}")]
    SizeLimitExceeded { current: usize, adding: usize, max: usize },
}

/// Constant pool and constants for Java class files

#[derive(Debug, Clone)]
pub enum Constant {
    Utf8(String),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    FieldRef(u16, u16),
    MethodRef(u16, u16),
    InterfaceMethodRef(u16, u16),
    NameAndType(u16, u16),
    MethodHandle(u8, u16),
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16),
}

mod constant_tags {
    pub const CONSTANT_UTF8: u8 = 1;
    pub const CONSTANT_INTEGER: u8 = 3;
    pub const CONSTANT_FLOAT: u8 = 4;
    pub const CONSTANT_LONG: u8 = 5;
    pub const CONSTANT_DOUBLE: u8 = 6;
    pub const CONSTANT_CLASS: u8 = 7;
    pub const CONSTANT_STRING: u8 = 8;
    pub const CONSTANT_FIELDREF: u8 = 9;
    pub const CONSTANT_METHODREF: u8 = 10;
    pub const CONSTANT_INTERFACEMETHODREF: u8 = 11;
    pub const CONSTANT_NAMEANDTYPE: u8 = 12;
    pub const CONSTANT_METHODHANDLE: u8 = 15;
    pub const CONSTANT_METHODTYPE: u8 = 16;
    pub const CONSTANT_DYNAMIC: u8 = 17;
    pub const CONSTANT_INVOKEDYNAMIC: u8 = 18;
    pub const CONSTANT_MODULE: u8 = 19;
    pub const CONSTANT_PACKAGE: u8 = 20;
}

impl Constant {
    pub fn to_bytes(&self) -> Vec<u8> {
        use constant_tags::*;
        let mut bytes = Vec::new();
        match self {
            Constant::Utf8(value) => {
                bytes.push(CONSTANT_UTF8);
                let utf8_bytes = value.as_bytes();
                bytes.extend_from_slice(&(utf8_bytes.len() as u16).to_be_bytes());
                bytes.extend_from_slice(utf8_bytes);
            }
            Constant::Integer(value) => {
                bytes.push(CONSTANT_INTEGER);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Float(value) => {
                bytes.push(CONSTANT_FLOAT);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Long(value) => {
                bytes.push(CONSTANT_LONG);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Double(value) => {
                bytes.push(CONSTANT_DOUBLE);
                bytes.extend_from_slice(&value.to_be_bytes());
            }
            Constant::Class(name_index) => {
                bytes.push(CONSTANT_CLASS);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::String(string_index) => {
                bytes.push(CONSTANT_STRING);
                bytes.extend_from_slice(&string_index.to_be_bytes());
            }
            Constant::FieldRef(class_index, name_and_type_index) => {
                bytes.push(CONSTANT_FIELDREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::MethodRef(class_index, name_and_type_index) => {
                bytes.push(CONSTANT_METHODREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::InterfaceMethodRef(class_index, name_and_type_index) => {
                bytes.push(CONSTANT_INTERFACEMETHODREF);
                bytes.extend_from_slice(&class_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::NameAndType(name_index, descriptor_index) => {
                bytes.push(CONSTANT_NAMEANDTYPE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
                bytes.extend_from_slice(&descriptor_index.to_be_bytes());
            }
            Constant::MethodHandle(reference_kind, reference_index) => {
                bytes.push(CONSTANT_METHODHANDLE);
                bytes.push(*reference_kind);
                bytes.extend_from_slice(&reference_index.to_be_bytes());
            }
            Constant::MethodType(descriptor_index) => {
                bytes.push(CONSTANT_METHODTYPE);
                bytes.extend_from_slice(&descriptor_index.to_be_bytes());
            }
            Constant::Dynamic(bootstrap_method_attr_index, name_and_type_index) => {
                bytes.push(CONSTANT_DYNAMIC);
                bytes.extend_from_slice(&bootstrap_method_attr_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::InvokeDynamic(bootstrap_method_attr_index, name_and_type_index) => {
                bytes.push(CONSTANT_INVOKEDYNAMIC);
                bytes.extend_from_slice(&bootstrap_method_attr_index.to_be_bytes());
                bytes.extend_from_slice(&name_and_type_index.to_be_bytes());
            }
            Constant::Module(name_index) => {
                bytes.push(CONSTANT_MODULE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
            Constant::Package(name_index) => {
                bytes.push(CONSTANT_PACKAGE);
                bytes.extend_from_slice(&name_index.to_be_bytes());
            }
        }
        bytes
    }
}

#[derive(Debug, Clone)]
enum Pending {
    Class(String),
    String(String),
    NameAndType { name: String, descriptor: String },
    FieldRef { class: String, name: String, descriptor: String },
    MethodRef { class: String, name: String, descriptor: String },
}

#[derive(Debug, Clone)]
pub struct ConstantPool {
    pub(crate) constants: Vec<Constant>,
    pending: Vec<Option<Pending>>, // parallel to constants; None if resolved
    top_touches: Vec<u16>,
    utf8_map: HashMap<String, u16>,
    class_map: HashMap<String, u16>,
    name_and_type_key_map: HashMap<(String, String), u16>,
    string_key_map: HashMap<String, u16>,
    fieldref_key_map: HashMap<(String, String, String), u16>,
    methodref_key_map: HashMap<(String, String, String), u16>,
    interfaceref_map: HashMap<(u16, u16), u16>,
}

impl ConstantPool {
    fn resolve_index(&mut self, idx: u16) {
        if idx == 0 { return; }
        let pos = (idx - 1) as usize;
        if pos >= self.constants.len() { return; }
        if self.pending.get(pos).cloned().unwrap_or(None).is_none() { return; }
        if let Some(p) = self.pending[pos].clone() {
            match p {
                Pending::Class(name) => {
                    let name_index = self.try_add_utf8(&name).unwrap();
                    self.resolve_index(name_index);
                    self.constants[pos] = Constant::Class(name_index);
                    self.pending[pos] = None;
                }
                Pending::String(val) => {
                    let utf8_index = self.try_add_utf8(&val).unwrap();
                    self.resolve_index(utf8_index);
                    self.constants[pos] = Constant::String(utf8_index);
                    self.pending[pos] = None;
                }
                Pending::NameAndType { name, descriptor } => {
                    let name_index = self.try_add_utf8(&name).unwrap();
                    let desc_index = self.try_add_utf8(&descriptor).unwrap();
                    self.resolve_index(name_index);
                    self.resolve_index(desc_index);
                    self.constants[pos] = Constant::NameAndType(name_index, desc_index);
                    self.pending[pos] = None;
                }
                Pending::FieldRef { class, name, descriptor } => {
                    let class_index = self.try_add_class(&class).unwrap();
                    self.resolve_index(class_index);
                    let nt_index = self.try_add_name_and_type(&name, &descriptor).unwrap();
                    self.resolve_index(nt_index);
                    self.constants[pos] = Constant::FieldRef(class_index, nt_index);
                    self.pending[pos] = None;
                }
                Pending::MethodRef { class, name, descriptor } => {
                    let class_index = self.try_add_class(&class).unwrap();
                    self.resolve_index(class_index);
                    let nt_index = self.try_add_name_and_type(&name, &descriptor).unwrap();
                    self.resolve_index(nt_index);
                    self.constants[pos] = Constant::MethodRef(class_index, nt_index);
                    self.pending[pos] = None;
                }
            }
        }
    }
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            pending: Vec::new(),
            top_touches: Vec::new(),
            utf8_map: HashMap::new(),
            class_map: HashMap::new(),
            name_and_type_key_map: HashMap::new(),
            string_key_map: HashMap::new(),
            fieldref_key_map: HashMap::new(),
            methodref_key_map: HashMap::new(),
            interfaceref_map: HashMap::new(),
        }
    }

    fn ensure_space(&self, adding: usize) -> Result<(), ConstPoolError> {
        // constant_pool_count is u16 and equals constants.len() + 1
        // Ensure after adding entries, count does not exceed u16::MAX
        let count_after = self.constants.len() + adding + 1;
        if count_after > u16::MAX as usize {
            return Err(ConstPoolError::SizeLimitExceeded { current: self.constants.len(), adding, max: (u16::MAX as usize) - 1 });
        }
        Ok(())
    }

    pub fn try_add_utf8(&mut self, value: &str) -> Result<u16, ConstPoolError> {
        if let Some(idx) = self.utf8_map.get(value) { return Ok(*idx); }
        self.ensure_space(1)?;
        let constant = Constant::Utf8(value.to_string());
        self.constants.push(constant);
        self.pending.push(None);
        let idx = self.constants.len() as u16;
        self.utf8_map.insert(value.to_string(), idx);
        Ok(idx)
    }

    pub fn add_utf8(&mut self, value: &str) -> u16 {
        self.try_add_utf8(value).unwrap()
    }
    
    pub fn try_add_class(&mut self, name: &str) -> Result<u16, ConstPoolError> {
        if let Some(idx) = self.class_map.get(name) { return Ok(*idx); }
        // Ensure space for Utf8 + Class (worst case)
        self.ensure_space(2)?;
        // First add Utf8 for the class name to stabilize index ordering like javac
        let name_utf8 = self.try_add_utf8(name)?;
        // Now add the Class entry pointing to the Utf8
        self.constants.push(Constant::Class(name_utf8));
        self.pending.push(None);
        let idx = self.constants.len() as u16;
        self.class_map.insert(name.to_string(), idx);
        Ok(idx)
    }

    pub fn add_class(&mut self, name: &str) -> u16 { self.try_add_class(name).unwrap() }
    
    pub fn try_add_name_and_type(&mut self, name: &str, descriptor: &str) -> Result<u16, ConstPoolError> {
        if let Some(idx) = self.name_and_type_key_map.get(&(name.to_string(), descriptor.to_string())) { return Ok(*idx); }
        // Ensure space for Utf8(name) + Utf8(descriptor) + NameAndType
        self.ensure_space(3)?;
        // Add dependencies first to stabilize ordering like javac
        let name_index = self.try_add_utf8(name)?;
        let desc_index = self.try_add_utf8(descriptor)?;
        // Now add the NameAndType entry pointing to the two Utf8 entries
        self.constants.push(Constant::NameAndType(name_index, desc_index));
        self.pending.push(None);
        let idx = self.constants.len() as u16;
        self.name_and_type_key_map.insert((name.to_string(), descriptor.to_string()), idx);
        Ok(idx)
    }

    pub fn add_name_and_type(&mut self, name: &str, descriptor: &str) -> u16 { self.try_add_name_and_type(name, descriptor).unwrap() }
    
    pub fn try_add_field_ref(&mut self, class: &str, name: &str, descriptor: &str) -> Result<u16, ConstPoolError> {
        let key = (class.to_string(), name.to_string(), descriptor.to_string());
        if let Some(idx) = self.fieldref_key_map.get(&key) { return Ok(*idx); }
        self.ensure_space(1)?;
        let idx = (self.constants.len() + 1) as u16;
        self.constants.push(Constant::FieldRef(0, 0));
        self.pending.push(Some(Pending::FieldRef { class: class.to_string(), name: name.to_string(), descriptor: descriptor.to_string() }));
        self.fieldref_key_map.insert(key, idx);
        self.top_touches.push(idx);
        self.resolve_index(idx);
        Ok(idx)
    }

    pub fn add_field_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 { self.try_add_field_ref(class, name, descriptor).unwrap() }
    
    pub fn try_add_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> Result<u16, ConstPoolError> {
        let key = (class.to_string(), name.to_string(), descriptor.to_string());
        if let Some(idx) = self.methodref_key_map.get(&key) { return Ok(*idx); }
        self.ensure_space(1)?;
        let idx = (self.constants.len() + 1) as u16;
        self.constants.push(Constant::MethodRef(0, 0));
        self.pending.push(Some(Pending::MethodRef { class: class.to_string(), name: name.to_string(), descriptor: descriptor.to_string() }));
        self.methodref_key_map.insert(key, idx);
        self.top_touches.push(idx);
        self.resolve_index(idx);
        Ok(idx)
    }

    pub fn add_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 { self.try_add_method_ref(class, name, descriptor).unwrap() }
    
    pub fn try_add_interface_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> Result<u16, ConstPoolError> {
        let class_index = self.try_add_class(class)?;
        let name_and_type_index = self.try_add_name_and_type(name, descriptor)?;
        if let Some(idx) = self.interfaceref_map.get(&(class_index, name_and_type_index)) { return Ok(*idx); }
        self.ensure_space(1)?;
        let constant = Constant::InterfaceMethodRef(class_index, name_and_type_index);
        self.constants.push(constant);
        let idx = self.constants.len() as u16;
        self.interfaceref_map.insert((class_index, name_and_type_index), idx);
        Ok(idx)
    }

    pub fn add_interface_method_ref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 { self.try_add_interface_method_ref(class, name, descriptor).unwrap() }
    
    pub fn try_add_string(&mut self, value: &str) -> Result<u16, ConstPoolError> {
        if let Some(idx) = self.string_key_map.get(value) { return Ok(*idx); }
        self.ensure_space(1)?;
        let idx = (self.constants.len() + 1) as u16;
        self.constants.push(Constant::String(0));
        self.pending.push(Some(Pending::String(value.to_string())));
        self.string_key_map.insert(value.to_string(), idx);
        self.top_touches.push(idx);
        self.resolve_index(idx);
        Ok(idx)
    }

    pub fn add_string(&mut self, value: &str) -> u16 { self.try_add_string(value).unwrap() }
    
    pub fn try_add_integer(&mut self, value: i32) -> Result<u16, ConstPoolError> {
        self.ensure_space(1)?;
        let constant = Constant::Integer(value);
        self.constants.push(constant);
        Ok(self.constants.len() as u16)
    }

    pub fn add_integer(&mut self, value: i32) -> u16 {
        let constant = Constant::Integer(value);
        self.constants.push(constant);
        self.pending.push(None);
        self.constants.len() as u16  // Indices start at 1
    }
    
    pub fn try_add_float(&mut self, value: f32) -> Result<u16, ConstPoolError> {
        self.ensure_space(1)?;
        let constant = Constant::Float(value);
        self.constants.push(constant);
        Ok(self.constants.len() as u16)
    }

    pub fn add_float(&mut self, value: f32) -> u16 {
        let constant = Constant::Float(value);
        self.constants.push(constant);
        self.pending.push(None);
        self.constants.len() as u16  // Indices start at 1
    }
    
    pub fn try_add_long(&mut self, value: i64) -> Result<u16, ConstPoolError> {
        self.ensure_space(1)?;
        let constant = Constant::Long(value);
        self.constants.push(constant);
        self.pending.push(None);
        Ok(self.constants.len() as u16)
    }

    pub fn add_long(&mut self, value: i64) -> u16 {
        let constant = Constant::Long(value);
        self.constants.push(constant);
        self.pending.push(None);
        self.constants.len() as u16  // Indices start at 1
    }
    
    pub fn try_add_double(&mut self, value: f64) -> Result<u16, ConstPoolError> {
        self.ensure_space(1)?;
        let constant = Constant::Double(value);
        self.constants.push(constant);
        self.pending.push(None);
        Ok(self.constants.len() as u16)
    }

    pub fn add_double(&mut self, value: f64) -> u16 {
        let constant = Constant::Double(value);
        self.constants.push(constant);
        self.pending.push(None);
        self.constants.len() as u16  // Indices start at 1
    }
    
    pub fn try_add_method_handle(&mut self, reference_kind: u8, reference_index: u16) -> Result<u16, ConstPoolError> {
        self.ensure_space(1)?;
        let constant = Constant::MethodHandle(reference_kind, reference_index);
        self.constants.push(constant);
        self.pending.push(None);
        Ok(self.constants.len() as u16)
    }

    pub fn add_method_handle(&mut self, reference_kind: u8, reference_index: u16) -> u16 {
        let constant = Constant::MethodHandle(reference_kind, reference_index);
        self.constants.push(constant);
        self.pending.push(None);
        self.constants.len() as u16  // Indices start at 1
    }
    
    pub fn try_add_method_type(&mut self, descriptor: &str) -> Result<u16, ConstPoolError> {
        self.ensure_space(2)?; // utf8 + method_type
        let descriptor_index = self.try_add_utf8(descriptor)?;
        let constant = Constant::MethodType(descriptor_index);
        self.constants.push(constant);
        self.pending.push(None);
        Ok(self.constants.len() as u16)
    }

    pub fn add_method_type(&mut self, descriptor: &str) -> u16 {
        let descriptor_index = self.add_utf8(descriptor);
        let constant = Constant::MethodType(descriptor_index);
        self.constants.push(constant);
        self.pending.push(None);
        self.constants.len() as u16  // Indices start at 1
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // Three-phase emit: A = top-level refs (MethodRef, FieldRef, String) by first-touch
        // B = their NameAndType in A order; C = remaining (Class/Utf8/others),
        // with debug Utf8 (Code/LineNumberTable/LocalVariableTable/SourceFile) moved to end.
        let mut cp = self.clone();
        // Ensure dependencies exist for all top-level pending entries
        let mut initial_len = cp.constants.len();
        for idx in 0..initial_len {
            if let Some(p) = cp.pending.get(idx).cloned().unwrap_or(None) {
                match p {
                    Pending::MethodRef { class, name, descriptor } => {
                        let _ = cp.try_add_class(&class);
                        let _ = cp.try_add_name_and_type(&name, &descriptor);
                    }
                    Pending::FieldRef { class, name, descriptor } => {
                        let _ = cp.try_add_class(&class);
                        let _ = cp.try_add_name_and_type(&name, &descriptor);
                    }
                    Pending::String(val) => { let _ = cp.try_add_utf8(&val); }
                    Pending::Class(_) | Pending::NameAndType { .. } => {}
                }
            }
        }
        // Recompute lengths in case appended
        initial_len = cp.constants.len();
        // Build A list (tops) exactly by recorded first-touch order, filtered to MethodRef/FieldRef/String and deduped
        let mut a_top: Vec<u16> = Vec::new();
        let mut seen_a: HashSet<u16> = HashSet::new();
        for old_idx in &cp.top_touches {
            let pos = (*old_idx - 1) as usize;
            let is_top = match (cp.constants.get(pos), cp.pending.get(pos).cloned().unwrap_or(None)) {
                (Some(Constant::MethodRef(_, _)), _) => true,
                (Some(Constant::FieldRef(_, _)), _) => true,
                (Some(Constant::String(_)), _) => true,
                (_, Some(Pending::MethodRef { .. })) => true,
                (_, Some(Pending::FieldRef { .. })) => true,
                (_, Some(Pending::String(_))) => true,
                _ => false,
            };
            if is_top && !seen_a.contains(old_idx) {
                seen_a.insert(*old_idx);
                a_top.push(*old_idx);
            }
        }
        // Build B list: NameAndType for tops in A order (unique, keep order)
        let mut b_nt: Vec<u16> = Vec::new();
        let mut seen_nt: HashSet<u16> = HashSet::new();
        for old_idx in &a_top {
            let pos = (*old_idx - 1) as usize;
            // Resolve NameAndType index from pending or resolved constant
            let nt_old: Option<u16> = match cp.pending.get(pos).cloned().unwrap_or(None) {
                Some(Pending::MethodRef { class: _, name, descriptor }) => {
                    cp.name_and_type_key_map.get(&(name, descriptor)).cloned()
                }
                Some(Pending::FieldRef { class: _, name, descriptor }) => {
                    cp.name_and_type_key_map.get(&(name, descriptor)).cloned()
                }
                _ => match cp.constants.get(pos) {
                    Some(Constant::MethodRef(_, nt)) => Some(*nt),
                    Some(Constant::FieldRef(_, nt)) => Some(*nt),
                    _ => None,
                },
            };
            if let Some(nt) = nt_old { if nt != 0 && seen_nt.insert(nt) { b_nt.push(nt); } }
        }
        // Build C list: remaining constants in first-touch order excluding A and B
        let in_a: HashSet<u16> = a_top.iter().cloned().collect();
        let in_b: HashSet<u16> = b_nt.iter().cloned().collect();
        // Identify debug Utf8 names to move last
        let is_debug_utf8 = |s: &str| s == "Code" || s == "LineNumberTable" || s == "LocalVariableTable" || s == "SourceFile";
        let mut c_rest: Vec<u16> = Vec::new();
        let mut c_debug_utf8: Vec<u16> = Vec::new();
        for i in 1..=initial_len as u16 {
            if in_a.contains(&i) || in_b.contains(&i) { continue; }
            let pos = (i - 1) as usize;
            // Skip NameAndType covered elsewhere
            let is_nt = matches!(cp.constants.get(pos), Some(Constant::NameAndType(_, _))) || matches!(cp.pending.get(pos).cloned().unwrap_or(None), Some(Pending::NameAndType { .. }));
            if is_nt { continue; }
            // Debug Utf8 last
            let is_utf8_debug = match cp.constants.get(pos) {
                Some(Constant::Utf8(ref s)) if is_debug_utf8(s) => true,
                _ => false,
            };
            if is_utf8_debug { c_debug_utf8.push(i); } else { c_rest.push(i); }
        }
        // Final order: ensure Utf8 entries for names appear before their Class entries.
        // Keep the existing A and B orders, then place non-debug Utf8 ahead of Class to stabilize indices as tests expect.
        let mut final_order: Vec<u16> = Vec::new();
        for x in &a_top { final_order.push(*x); }
        for x in &b_nt { final_order.push(*x); }
        // Partition c_rest: Utf8 first, then others, preserving original order
        let mut c_utf8_first: Vec<u16> = Vec::new();
        let mut c_non_utf8: Vec<u16> = Vec::new();
        for i in &c_rest {
            let pos = (*i - 1) as usize;
            match cp.constants.get(pos) {
                Some(Constant::Utf8(_)) => c_utf8_first.push(*i),
                _ => c_non_utf8.push(*i),
            }
        }
        for x in &c_utf8_first { final_order.push(*x); }
        for x in &c_non_utf8 { final_order.push(*x); }
        for x in &c_debug_utf8 { final_order.push(*x); }
        // Build new index map
        let mut new_index: HashMap<u16, u16> = HashMap::new();
        for (new_pos, old_idx) in final_order.iter().enumerate() {
            new_index.insert(*old_idx, (new_pos + 1) as u16);
        }
        // Helper to write an entry by old index according to mapping
        fn write_entry(cp: &ConstantPool, old_idx: u16, map: &HashMap<u16, u16>, out: &mut Vec<u8>) {
            let pos = (old_idx - 1) as usize;
            if let Some(p) = cp.pending.get(pos).cloned().unwrap_or(None) {
                match p {
                    Pending::Class(name) => {
                        out.push(constant_tags::CONSTANT_CLASS);
                        let name_old = cp.utf8_map.get(&name).cloned().unwrap_or(0);
                        let name_new = *map.get(&name_old).unwrap_or(&0);
                        out.extend_from_slice(&name_new.to_be_bytes());
                    }
                    Pending::String(val) => {
                        out.push(constant_tags::CONSTANT_STRING);
                        let utf8_old = cp.utf8_map.get(&val).cloned().unwrap_or(0);
                        let utf8_new = *map.get(&utf8_old).unwrap_or(&0);
                        out.extend_from_slice(&utf8_new.to_be_bytes());
                    }
                    Pending::NameAndType { name, descriptor } => {
                        out.push(constant_tags::CONSTANT_NAMEANDTYPE);
                        let n_old = cp.utf8_map.get(&name).cloned().unwrap_or(0);
                        let d_old = cp.utf8_map.get(&descriptor).cloned().unwrap_or(0);
                        let n_new = *map.get(&n_old).unwrap_or(&0);
                        let d_new = *map.get(&d_old).unwrap_or(&0);
                        out.extend_from_slice(&n_new.to_be_bytes());
                        out.extend_from_slice(&d_new.to_be_bytes());
                    }
                    Pending::FieldRef { class, name, descriptor } => {
                        out.push(constant_tags::CONSTANT_FIELDREF);
                        let c_old = cp.class_map.get(&class).cloned().unwrap_or(0);
                        let nt_old = cp.name_and_type_key_map.get(&(name, descriptor)).cloned().unwrap_or(0);
                        let c_new = *map.get(&c_old).unwrap_or(&0);
                        let nt_new = *map.get(&nt_old).unwrap_or(&0);
                        out.extend_from_slice(&c_new.to_be_bytes());
                        out.extend_from_slice(&nt_new.to_be_bytes());
                    }
                    Pending::MethodRef { class, name, descriptor } => {
                        out.push(constant_tags::CONSTANT_METHODREF);
                        let c_old = cp.class_map.get(&class).cloned().unwrap_or(0);
                        let nt_old = cp.name_and_type_key_map.get(&(name, descriptor)).cloned().unwrap_or(0);
                        let c_new = *map.get(&c_old).unwrap_or(&0);
                        let nt_new = *map.get(&nt_old).unwrap_or(&0);
                        out.extend_from_slice(&c_new.to_be_bytes());
                        out.extend_from_slice(&nt_new.to_be_bytes());
                    }
                }
                return;
            }
            // Resolved constant
            match cp.constants[pos].clone() {
                Constant::Utf8(s) => {
                    out.push(constant_tags::CONSTANT_UTF8);
                    let bs = s.as_bytes();
                    out.extend_from_slice(&(bs.len() as u16).to_be_bytes());
                    out.extend_from_slice(bs);
                }
                Constant::Integer(v) => { out.push(constant_tags::CONSTANT_INTEGER); out.extend_from_slice(&v.to_be_bytes()); }
                Constant::Float(v) => { out.push(constant_tags::CONSTANT_FLOAT); out.extend_from_slice(&v.to_be_bytes()); }
                Constant::Long(v) => { out.push(constant_tags::CONSTANT_LONG); out.extend_from_slice(&v.to_be_bytes()); }
                Constant::Double(v) => { out.push(constant_tags::CONSTANT_DOUBLE); out.extend_from_slice(&v.to_be_bytes()); }
                Constant::Class(name_idx) => {
                    out.push(constant_tags::CONSTANT_CLASS);
                    let mapped = *map.get(&name_idx).unwrap_or(&0);
                    out.extend_from_slice(&mapped.to_be_bytes());
                }
                Constant::String(utf8_idx) => {
                    out.push(constant_tags::CONSTANT_STRING);
                    let mapped = *map.get(&utf8_idx).unwrap_or(&0);
                    out.extend_from_slice(&mapped.to_be_bytes());
                }
                Constant::FieldRef(c, nt) => {
                    out.push(constant_tags::CONSTANT_FIELDREF);
                    out.extend_from_slice(&map.get(&c).unwrap_or(&0).to_be_bytes());
                    out.extend_from_slice(&map.get(&nt).unwrap_or(&0).to_be_bytes());
                }
                Constant::MethodRef(c, nt) => {
                    out.push(constant_tags::CONSTANT_METHODREF);
                    out.extend_from_slice(&map.get(&c).unwrap_or(&0).to_be_bytes());
                    out.extend_from_slice(&map.get(&nt).unwrap_or(&0).to_be_bytes());
                }
                Constant::InterfaceMethodRef(c, nt) => {
                    out.push(constant_tags::CONSTANT_INTERFACEMETHODREF);
                    out.extend_from_slice(&map.get(&c).unwrap_or(&0).to_be_bytes());
                    out.extend_from_slice(&map.get(&nt).unwrap_or(&0).to_be_bytes());
                }
                Constant::NameAndType(n, d) => {
                    out.push(constant_tags::CONSTANT_NAMEANDTYPE);
                    out.extend_from_slice(&map.get(&n).unwrap_or(&0).to_be_bytes());
                    out.extend_from_slice(&map.get(&d).unwrap_or(&0).to_be_bytes());
                }
                Constant::MethodHandle(kind, ref_idx) => {
                    out.push(constant_tags::CONSTANT_METHODHANDLE); out.push(kind); out.extend_from_slice(&map.get(&ref_idx).unwrap_or(&0).to_be_bytes());
                }
                Constant::MethodType(desc_idx) => {
                    out.push(constant_tags::CONSTANT_METHODTYPE); out.extend_from_slice(&map.get(&desc_idx).unwrap_or(&0).to_be_bytes());
                }
                Constant::Dynamic(bsm_idx, nt_idx) => {
                    out.push(constant_tags::CONSTANT_DYNAMIC);
                    out.extend_from_slice(&map.get(&bsm_idx).unwrap_or(&0).to_be_bytes());
                    out.extend_from_slice(&map.get(&nt_idx).unwrap_or(&0).to_be_bytes());
                }
                Constant::InvokeDynamic(bsm_idx, nt_idx) => {
                    out.push(constant_tags::CONSTANT_INVOKEDYNAMIC);
                    out.extend_from_slice(&map.get(&bsm_idx).unwrap_or(&0).to_be_bytes());
                    out.extend_from_slice(&map.get(&nt_idx).unwrap_or(&0).to_be_bytes());
                }
                Constant::Module(n) => { out.push(constant_tags::CONSTANT_MODULE); out.extend_from_slice(&map.get(&n).unwrap_or(&0).to_be_bytes()); }
                Constant::Package(n) => { out.push(constant_tags::CONSTANT_PACKAGE); out.extend_from_slice(&map.get(&n).unwrap_or(&0).to_be_bytes()); }
            }
        }

        let mut bytes: Vec<u8> = vec![0, 0];
        for old_idx in &final_order {
            write_entry(&cp, *old_idx, &new_index, &mut bytes);
        }
        let count: u16 = (final_order.len() as u16) + 1;
        bytes[0] = (count >> 8) as u8;
        bytes[1] = (count & 0xFF) as u8;
        bytes
    }
}


