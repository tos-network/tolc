/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang;

public final class Long extends Number implements Comparable<Long> {
  public static final long MIN_VALUE = -9223372036854775808l;
  public static final long MAX_VALUE =  9223372036854775807l;

  public static final Class TYPE = java.base.Classes.forCanonicalName("J");

  private final long value;

  public Long(long value) {
    this.value = value;
  }

  public Long(String s) {
    this.value = parseLong(s);
  }

  public static Long valueOf(String value) {
    return new Long(value);
  }

  public static Long valueOf(long value) {
    return new Long(value);
  }

  public int compareTo(Long o) {
    return value > o.value ? 1 : (value < o.value ? -1 : 0);
  }

  public boolean equals(Object o) {
    return o instanceof Long && ((Long) o).value == value;
  }

  public int hashCode() {
    return (int) ((value >> 32) ^ (value & 0xFF));
  }

  public String toString() {
    return String.valueOf(value);
  }

  public static String toString(long v, int radix) {
    if (radix < 1 || radix > 36) {
      throw new IllegalArgumentException("radix " + radix + " not in [1,36]");
    }

    if (v == 0) {
      return "0";
    }

    boolean negative = v < 0;

    int size = (negative ? 1 : 0);
    for (long n = v; n != 0; n /= radix) ++size;

    char[] array = new char[size];

    int i = size - 1;
    for (long n = v; n != 0; n /= radix) {
      long digit = n % radix;
      if (negative) digit = -digit;

      if (digit >= 0 && digit <= 9) {
        array[i] = (char) ('0' + digit);
      } else {
        array[i] = (char) ('a' + (digit - 10));
      }
      --i;
    }

    if (negative) {
      array[i] = '-';
    }

    return new String(array, 0, size, false);
  }

  public static String toString(long v) {
    return toString(v, 10);
  }

  public static String toHexString(long v) {
    return toString(v, 16);
  }

  public static String toOctalString(long v) {
    return toString(v, 8);
  }

  public static String toBinaryString(long v) {
    return toString(v, 2);
  }

  public byte byteValue() {
    return (byte) value;
  }

  public short shortValue() {
    return (short) value;
  }

  public int intValue() {
    return (int) value;
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

  public static int signum(long v) {
    if (v == 0)     return  0;
    else if (v > 0) return  1;
    else            return -1;
  }

  private static long pow(long a, long b) {
    long c = 1;
    for (int i = 0; i < b; ++i) c *= a;
    return c;
  }

  public static long parseLong(String s) {
    return parseLong(s, 10);
  }

  public static long parseLong(String s, int radix) {
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
    
    long result = 0;
    long limit = negative ? Long.MIN_VALUE : -Long.MAX_VALUE;
    long multmin = limit / radix;
    
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

  public static int numberOfLeadingZeros(long i) {
    if (i == 0) return 64;
    int n = 0;
    if (i < 0) return 0;
    if (i < 0x100000000L) { n += 32; i <<= 32; }
    if (i < 0x1000000000000L) { n += 16; i <<= 16; }
    if (i < 0x100000000000000L) { n += 8; i <<= 8; }
    if (i < 0x1000000000000000L) { n += 4; i <<= 4; }
    if (i < 0x4000000000000000L) { n += 2; i <<= 2; }
    if (i < 0x8000000000000000L) { n += 1; }
    return n;
  }

  public static String toUnsignedString(long i, int radix) {
    if (i >= 0) {
      return toString(i, radix);
    } else {
      // Handle negative values as unsigned
      return toString(i & 0x7FFFFFFFFFFFFFFFL, radix);
    }
  }

  public static String toUnsignedString(long i) {
    return toUnsignedString(i, 10);
  }

  public static int compareUnsigned(long x, long y) {
    return Long.compare(x + Long.MIN_VALUE, y + Long.MIN_VALUE);
  }

  public static int compare(long x, long y) {
    return (x < y) ? -1 : ((x == y) ? 0 : 1);
  }
}
