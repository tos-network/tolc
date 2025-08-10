use std::fmt;

/// Represents a location in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
    /// Byte offset from start of file
    pub offset: usize,
}

impl Location {
    /// Create a new location
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self { line, column, offset }
    }
    
    /// Create a location at the start of a file
    pub fn start() -> Self {
        Self { line: 1, column: 1, offset: 0 }
    }
    
    /// Advance the location by one character
    pub fn advance(&mut self, ch: char) {
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
    
    /// Advance the location by a string
    pub fn advance_str(&mut self, s: &str) {
        for ch in s.chars() {
            self.advance(ch);
        }
    }
    
    /// Create a span from this location to another
    pub fn to(&self, end: Location) -> Span {
        Span::new(*self, end)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Represents a span of source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Start location (inclusive)
    pub start: Location,
    /// End location (exclusive)
    pub end: Location,
}

impl Span {
    /// Create a new span
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }
    
    /// Create a span from a single location
    pub fn single(location: Location) -> Self {
        Self { start: location, end: location }
    }
    
    /// Create a span from start to end locations
    pub fn from_to(start: Location, end: Location) -> Self {
        Self { start, end }
    }
    
    /// Get the length of the span in characters
    pub fn len(&self) -> usize {
        self.end.offset.saturating_sub(self.start.offset)
    }
    
    /// Check if the span is empty
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
    
    /// Extend the span to include another span
    pub fn extend(&mut self, other: Span) {
        if other.start < self.start {
            self.start = other.start;
        }
        if other.end > self.end {
            self.end = other.end;
        }
    }
    
    /// Get the line range of the span
    pub fn line_range(&self) -> std::ops::RangeInclusive<usize> {
        self.start.line..=self.end.line
    }
    
    /// Check if a location is within this span
    pub fn contains(&self, location: Location) -> bool {
        location >= self.start && location < self.end
    }
    
    /// Get the source text for this span
    pub fn source_text<'a>(&self, source: &'a str) -> &'a str {
        if self.start.offset >= source.len() {
            return "";
        }
        let end_offset = self.end.offset.min(source.len());
        &source[self.start.offset..end_offset]
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start.line == self.end.line {
            if self.start.column == self.end.column {
                write!(f, "{}:{}", self.start.line, self.start.column)
            } else {
                write!(f, "{}:{}-{}", self.start.line, self.start.column, self.end.column)
            }
        } else {
            write!(f, "{}:{}-{}:{}", self.start.line, self.start.column, self.end.line, self.end.column)
        }
    }
}

/// Trait for types that can be converted to a span
pub trait ToSpan {
    /// Convert to a span
    fn to_span(&self) -> Span;
}

impl ToSpan for Span {
    fn to_span(&self) -> Span {
        *self
    }
}

impl ToSpan for Location {
    fn to_span(&self) -> Span {
        Span::single(*self)
    }
}

/// Trait for types that can be spanned
pub trait HasSpan {
    /// Get the span of this item
    fn span(&self) -> Span;
}

/// A wrapper that adds span information to any type
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    /// The wrapped value
    pub value: T,
    /// The span of the value
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new spanned value
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
    
    /// Create a spanned value from a single location
    pub fn at(value: T, location: Location) -> Self {
        Self { value, span: Span::single(location) }
    }
    
    /// Get a reference to the wrapped value
    pub fn as_ref(&self) -> &T {
        &self.value
    }
    
    /// Get a mutable reference to the wrapped value
    pub fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
    
    /// Convert to the inner value
    pub fn into_inner(self) -> T {
        self.value
    }
    
    /// Map the inner value
    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }
    
    /// Map the inner value with a function that can fail
    pub fn map_result<U, E, F>(self, f: F) -> Result<Spanned<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        Ok(Spanned {
            value: f(self.value)?,
            span: self.span,
        })
    }
}

impl<T> Spanned<T> {
    /// Get the span of this item
    pub fn span(&self) -> Span {
        self.span
    }
    
    /// Get the start location
    pub fn start(&self) -> Location {
        self.span.start
    }
    
    /// Get the end location
    pub fn end(&self) -> Location {
        self.span.end
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: PartialOrd> PartialOrd for Spanned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<T: Ord> Ord for Spanned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<T: std::hash::Hash> std::hash::Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

/// Create a spanned value
pub fn spanned<T>(value: T, span: Span) -> Spanned<T> {
    Spanned::new(value, span)
}

/// Create a spanned value at a specific location
pub fn at<T>(value: T, location: Location) -> Spanned<T> {
    Spanned::at(value, location)
}

/// Create a span from two locations
pub fn span(start: Location, end: Location) -> Span {
    Span::from_to(start, end)
}

/// Create a single location span
pub fn single(location: Location) -> Span {
    Span::single(location)
}
