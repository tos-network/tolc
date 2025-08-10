/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.math;

import java.io.Serializable;

public class BigInteger implements Serializable {

  private int   sign;
  private int[] value;

  private BigInteger(int sign, long value) {
    this.sign = sign;
    int upperBits = (int) (value >>> 32);
    if (upperBits == 0)
      // Array with one element
      this.value = new int[] { (int) value };
    else
      // Array with two elements
      this.value = new int[] { (int) value, upperBits };
  }

  public static final BigInteger ZERO = new BigInteger(0,  0);
  public static final BigInteger ONE  = new BigInteger(1,  1);
  public static final BigInteger TEN  = new BigInteger(1, 10);

  public static BigInteger valueOf(long num) {
    int signum = Long.signum(num);
    if (signum == 0)
      return BigInteger.ZERO;
    else if (signum > 0)
      return new BigInteger(signum, num);
    else
      return new BigInteger(signum, -num);   
  }

  // Constructor for string-based initialization
  public BigInteger(String val, int radix) {
    // Simplified implementation
    this.sign = 1;
    this.value = new int[] { Integer.parseInt(val, radix) };
  }

  // Constructor for byte array
  public BigInteger(int signum, byte[] magnitude) {
    this.sign = signum;
    // Simplified implementation - just use first byte
    this.value = new int[] { magnitude.length > 0 ? magnitude[0] & 0xFF : 0 };
  }

  public int signum() {
    return sign;
  }

  public String toString(int radix) {
    if (value.length == 1) {
      return Integer.toString(value[0], radix);
    }
    // Simplified implementation for multi-element arrays
    return Integer.toString(value[0], radix);
  }

  public byte[] toByteArray() {
    // Simplified implementation
    byte[] result = new byte[value.length * 4];
    for (int i = 0; i < value.length; i++) {
      result[i * 4] = (byte) (value[i] & 0xFF);
      result[i * 4 + 1] = (byte) ((value[i] >> 8) & 0xFF);
      result[i * 4 + 2] = (byte) ((value[i] >> 16) & 0xFF);
      result[i * 4 + 3] = (byte) ((value[i] >> 24) & 0xFF);
    }
    return result;
  }

  public BigInteger shiftLeft(int n) {
    // Simplified implementation
    if (n == 0) return this;
    if (n < 0) return shiftRight(-n);
    
    int[] newValue = new int[value.length + (n / 32) + 1];
    System.arraycopy(value, 0, newValue, 0, value.length);
    return new BigInteger(sign, newValue);
  }

  public BigInteger shiftRight(int n) {
    // Simplified implementation
    if (n == 0) return this;
    if (n < 0) return shiftLeft(-n);
    
    int[] newValue = new int[Math.max(1, value.length - (n / 32))];
    System.arraycopy(value, 0, newValue, 0, newValue.length);
    return new BigInteger(sign, newValue);
  }

  public BigInteger and(BigInteger val) {
    // Simplified implementation
    return new BigInteger(sign & val.sign, value[0] & val.value[0]);
  }

  public BigInteger or(BigInteger val) {
    // Simplified implementation
    return new BigInteger(sign | val.sign, value[0] | val.value[0]);
  }

  public int intValueExact() {
    if (value.length > 1) {
      throw new ArithmeticException("BigInteger out of int range");
    }
    return value[0];
  }

  public int intValue() {
    if (value.length == 0) return 0;
    return value[0];
  }

  public short shortValue() {
    return (short) intValue();
  }

  public byte byteValue() {
    return (byte) intValue();
  }

  public long longValue() {
    if (value.length == 0) return 0L;
    if (value.length == 1) return value[0];
    return ((long) value[1] << 32) | (value[0] & 0xFFFFFFFFL);
  }

  public float floatValue() {
    return (float) longValue();
  }

  public double doubleValue() {
    return (double) longValue();
  }

  public int bitLength() {
    if (value.length == 1) {
      return Integer.bitCount(value[0]);
    }
    // Simplified implementation
    return value.length * 32;
  }

  // Helper constructor for internal use
  private BigInteger(int sign, int[] value) {
    this.sign = sign;
    this.value = value;
  }
}
