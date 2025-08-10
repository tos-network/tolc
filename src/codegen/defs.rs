//! Generic classfile-specific definitions

/// Header of Java class file (magic number)
pub const MAGIC: u32 = 0xCAFEBABE;

/// Name of a constructor
pub const CONSTRUCTOR_METHOD_NAME: &str = "<init>";

/// Name of a static initializer
pub const STATIC_INITIALIZER_METHOD_NAME: &str = "<clinit>";

/// JVM type aliases for better type safety
pub type JvmByte = i8;
pub type JvmShort = i16;
pub type JvmInt = i32;
pub type JvmLong = i64;
pub type JvmChar = u16;
pub type JvmFloat = f32;
pub type JvmDouble = f64;

/// JVM version constants
pub mod major_versions {
    pub const JAVA_1_1: u16 = 45;
    pub const JAVA_1_2: u16 = 46;
    pub const JAVA_1_3: u16 = 47;
    pub const JAVA_1_4: u16 = 48;
    pub const JAVA_5_0: u16 = 49;
    pub const JAVA_6_0: u16 = 50;
    pub const JAVA_7: u16 = 51;
    pub const JAVA_8: u16 = 52;
    pub const JAVA_9: u16 = 53;
    pub const JAVA_10: u16 = 54;
    pub const JAVA_11: u16 = 55;
    pub const JAVA_12: u16 = 56;
    pub const JAVA_14: u16 = 58;
    pub const JAVA_15: u16 = 59;
    pub const JAVA_16: u16 = 60;
    pub const JAVA_17: u16 = 61;
    pub const JAVA_18: u16 = 62;
    pub const JAVA_19: u16 = 63;
    pub const JAVA_20: u16 = 64;
    pub const JAVA_21: u16 = 65;
}

// Legacy constants for backward compatibility
pub const JAVA_1_8: u16 = major_versions::JAVA_8;
pub const JAVA_11: u16 = major_versions::JAVA_11;
pub const JAVA_17: u16 = major_versions::JAVA_17;
pub const JAVA_21: u16 = major_versions::JAVA_21;


