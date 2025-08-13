// JVMS 4.7.9.1 Signatures – minimal but strict parser for class/field/method signatures

// Public API
pub fn is_valid_class_signature(s: &str) -> bool {
    let mut p = Parser::new(s);
    if p.peek() == Some('<') {
        if !p.parse_type_parameters() { return false; }
    }
    if !p.parse_class_type_signature() { return false; }
    // Safety cap: prevent pathological loops on malformed signatures
    let mut steps: usize = 0;
    while p.more() {
        if steps > crate::consts::VERIFY_MAX_SIGNATURE_ITERS { return false; }
        steps += 1;
        // superinterfaces
        if !p.parse_class_type_signature() { return false; }
    }
    p.eof()
}

pub fn is_valid_field_signature(s: &str) -> bool {
    let mut p = Parser::new(s);
    if !p.parse_field_type_signature_or_base() { return false; }
    p.eof()
}

pub fn is_valid_method_signature(s: &str) -> bool {
    let mut p = Parser::new(s);
    if p.peek() == Some('<') {
        if !p.parse_type_parameters() { return false; }
    }
    if !p.consume('(') { return false; }
    while p.peek() != Some(')') {
        if !p.parse_field_type_signature_or_base() { return false; }
    }
    if !p.consume(')') { return false; }
    // return type
    if p.peek() == Some('V') { p.next(); } else if !p.parse_field_type_signature_or_base() { return false; }
    // throws (optional)
    while p.peek() == Some('^') {
        p.next();
        if p.peek() == Some('T') {
            if !p.parse_type_variable_signature() { return false; }
        } else if !p.parse_class_type_signature() { return false; }
    }
    p.eof()
}

// Parser implementation
struct Parser<'a> { chars: std::str::Chars<'a>, look: Option<char> }

impl<'a> Parser<'a> {
    fn new(s: &'a str) -> Self { let mut it = s.chars(); let look = it.next(); Self { chars: it, look } }
    fn peek(&self) -> Option<char> { self.look }
    fn next(&mut self) -> Option<char> { let cur = self.look; self.look = self.chars.next(); cur }
    fn consume(&mut self, c: char) -> bool { if self.peek()==Some(c) { self.next(); true } else { false } }
    fn eof(&self) -> bool { self.look.is_none() }
    fn more(&self) -> bool { self.look.is_some() }

    fn parse_identifier(&mut self) -> bool {
        // JVMS identifiers exclude '/', ';', '<', '>'
        let mut seen = false;
        while let Some(c) = self.peek() {
            if c == '/' || c == ';' || c == '<' || c == '>' || c == ':' || c == '.' {
                break;
            }
            seen = true;
            self.next();
        }
        seen
    }

    fn parse_type_parameters(&mut self) -> bool {
        if !self.consume('<') { return false; }
        while let Some(c) = self.peek() {
            if c == '>' { self.next(); return true; }
            if !self.parse_identifier() { return false; }
            if !self.consume(':') { return false; }
            // zero or more ClassBound/InterfaceBound
            if self.peek() != Some(':') {
                // ClassBound: FieldTypeSignature
                if !self.parse_field_type_signature() { return false; }
            }
        // Safety cap inside bounds loop
        let mut bsteps: usize = 0;
        while self.consume(':') {
            if bsteps > crate::consts::VERIFY_MAX_SIGNATURE_ITERS { return false; }
            bsteps += 1;
                if !self.parse_field_type_signature() { return false; }
            }
        }
        false
    }

    fn parse_field_type_signature_or_base(&mut self) -> bool {
        match self.peek() {
            Some('B'|'C'|'D'|'F'|'I'|'J'|'S'|'Z') => { self.next(); true }
            _ => self.parse_field_type_signature()
        }
    }

    fn parse_field_type_signature(&mut self) -> bool {
        match self.peek() {
            Some('L') => self.parse_class_type_signature(),
            Some('T') => self.parse_type_variable_signature(),
            Some('[') => self.parse_array_type_signature(),
            _ => false,
        }
    }

    fn parse_type_variable_signature(&mut self) -> bool {
        if !self.consume('T') { return false; }
        if !self.parse_identifier() { return false; }
        self.consume(';')
    }

    fn parse_array_type_signature(&mut self) -> bool {
        if !self.consume('[') { return false; }
        self.parse_field_type_signature_or_base()
    }

    fn parse_class_type_signature(&mut self) -> bool {
        if !self.consume('L') { return false; }
        if !self.parse_package_specifier_and_simple_types() { return false; }
        if !self.consume(';') { return false; }
        true
    }

    fn parse_package_specifier_and_simple_types(&mut self) -> bool {
        // package can be empty; sequence of identifiers with '/'
        // parse first simple type
        if !self.parse_simple_class_type_signature() { return false; }
        // inner classes separated by '.'
        while self.consume('.') {
            if !self.parse_simple_class_type_signature() { return false; }
        }
        true
    }

    fn parse_simple_class_type_signature(&mut self) -> bool {
        if !self.parse_identifier() { return false; }
        // optional type arguments
        if self.peek() == Some('<') {
            if !self.parse_type_arguments() { return false; }
        }
        // allow package separators as part of the qualified name: after identifier, accept '/' then identifier segments each possibly with type args
        while self.consume('/') {
            if !self.parse_identifier() { return false; }
            if self.peek() == Some('<') { if !self.parse_type_arguments() { return false; } }
        }
        true
    }

    fn parse_type_arguments(&mut self) -> bool {
        if !self.consume('<') { return false; }
        loop {
            match self.peek() {
                Some('*') => { self.next(); }
                Some('+') | Some('-') => { self.next(); if !self.parse_field_type_signature() { return false; } }
                _ => { if !self.parse_field_type_signature() { return false; } }
            }
            if self.consume('>') { break; }
        }
        true
    }
}


