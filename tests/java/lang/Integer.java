package java.lang;

public final class Integer extends Number implements Comparable<Integer> {
  public static final Class TYPE = java.base.Classes.forCanonicalName("I");

  public static final int MIN_VALUE = 0x80000000;
  public static final int MAX_VALUE = 0x7FFFFFFF;

  private final int value;

  public Integer(int value) {
    this.value = value;
  }

  public Integer(String s) {
    this.value = parseInt(s);
  }

  public static Integer valueOf(int value) {
    return new Integer(value);
  }

  public static Integer valueOf(String value) {
    return valueOf(parseInt(value));
  }

  public boolean equals(Object o) {
    return o instanceof Integer && ((Integer) o).value == value;
  }

  public int hashCode() {
    return value;
  }

  public int compareTo(Integer other) {
    return value - other.value;
  }

  public String toString() {
    return toString(value);
  }

  public static String toString(int v, int radix) {
    return Long.toString(v, radix);
  }

  public static String toString(int v) {
    return toString(v, 10);
  }

  public static String toHexString(int v) {
    return Long.toString(((long) v) & 0xFFFFFFFFL, 16);
  }

  public static String toOctalString(int v) {
    return Long.toString(((long) v) & 0xFFFFFFFFL, 8);
  }

  public static String toBinaryString(int v) {
    return Long.toString(((long) v) & 0xFFFFFFFFL, 2);
  }

  public byte byteValue() {
    return (byte) value;
  }

  public short shortValue() {
    return (short) value;
  }

  public int intValue() {
    return value;
  }

  public long longValue() {
    return value;
  }

  public float floatValue() {
    return (float) value;
  }

  public double doubleValue() {
    return (double) value;
  }

  public static int signum(int v) {
    if (v == 0)     return  0;
    else if (v > 0) return  1;
    else            return -1;
  }

  // See http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
  public static int bitCount(int v) {
    v = v - ((v >> 1) & 0x55555555);
    v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
    return ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
  }

  public static int reverseBytes(int v) {
    int byte3 =  v >>> 24;
    int byte2 = (v >>> 8) & 0xFF00;
    int byte1 = (v <<  8) & 0xFF00;
    int byte0 =  v << 24;
    return (byte0 | byte1 | byte2 | byte3);
  }

  public static int parseInt(String s) {
    return parseInt(s, 10);
  }

  public static int parseInt(String s, int radix) {
    if (s == null) {
      throw new NumberFormatException("null");
    }
    
    if (radix < Character.MIN_RADIX || radix > Character.MAX_RADIX) {
      throw new NumberFormatException("radix " + radix + " out of range");
    }
    
    boolean negative = false;
    int index = 0;
    int len = s.length();
    
    if (len == 0) {
      throw new NumberFormatException("empty string");
    }
    
    char firstChar = s.charAt(0);
    if (firstChar < '0') {
      if (firstChar == '-') {
        negative = true;
        index++;
      } else if (firstChar != '+') {
        throw new NumberFormatException("illegal character: " + firstChar);
      }
      
      if (index == len) {
        throw new NumberFormatException("no digits");
      }
    }
    
    int result = 0;
    int limit = negative ? Integer.MIN_VALUE : -Integer.MAX_VALUE;
    int multmin = limit / radix;
    
    while (index < len) {
      int digit = Character.digit(s.charAt(index++), radix);
      if (digit < 0) {
        throw new NumberFormatException("illegal character at position " + (index - 1));
      }
      if (result < multmin) {
        throw new NumberFormatException("overflow");
      }
      result *= radix;
      if (result < limit + digit) {
        throw new NumberFormatException("overflow");
      }
      result -= digit;
    }
    
    return negative ? result : -result;
  }

  public static Integer decode(String string) {
    if (string.startsWith("-")) {
      if (string.startsWith("-0") || string.startsWith("-#")) {
        return new Integer(-decode(string.substring(1)));
      }
    } else if (string.startsWith("0")) {
      char c = string.length() < 2 ? (char)-1 : string.charAt(1);
      if (c == 'x' || c == 'X') {
        return new Integer(parseInt(string.substring(2), 0x10));
      }
      return new Integer(parseInt(string, 010));
    } else if (string.startsWith("#")) {
      return new Integer(parseInt(string.substring(1), 0x10));
    }
    return new Integer(parseInt(string, 10));
  }

  public static int numberOfLeadingZeros(int i) {
    if (i == 0) return 32;
    int n = 0;
    if (i < 0) return 0;
    if (i < 0x10000) { n += 16; i <<= 16; }
    if (i < 0x1000000) { n += 8; i <<= 8; }
    if (i < 0x10000000) { n += 4; i <<= 4; }
    if (i < 0x40000000) { n += 2; i <<= 2; }
    if (i < 0x80000000) { n += 1; }
    return n;
  }

  public static int numberOfTrailingZeros(int i) {
    if (i == 0) return 32;
    int n = 0;
    if ((i & 0xFFFF) == 0) { n += 16; i >>>= 16; }
    if ((i & 0xFF) == 0) { n += 8; i >>>= 8; }
    if ((i & 0xF) == 0) { n += 4; i >>>= 4; }
    if ((i & 0x3) == 0) { n += 2; i >>>= 2; }
    if ((i & 0x1) == 0) { n += 1; }
    return n;
  }

  public static String toUnsignedString(int i, int radix) {
    return Long.toUnsignedString(((long) i) & 0xFFFFFFFFL, radix);
  }

  public static String toUnsignedString(int i) {
    return toUnsignedString(i, 10);
  }

  public static int compareUnsigned(int x, int y) {
    return Integer.compare(x + Integer.MIN_VALUE, y + Integer.MIN_VALUE);
  }

  public static int compare(int x, int y) {
    return (x < y) ? -1 : ((x == y) ? 0 : 1);
  }
}
