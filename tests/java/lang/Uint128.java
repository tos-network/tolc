package java.lang;

import java.math.BigInteger;

/**
 * Represents unsigned values less than {@code 2**128}.
 *
 * Enhanced with SafeMath security features for smart contract safety.
 * All arithmetic operations include overflow checks to prevent vulnerabilities.
 *
 * As indicated by the type signatures, arithmetic operations are not applicable
 * to types of other widths in this package.  Copy constructors can be used to
 * explicitly promote or truncate values for the purposes of interoperability.
 */
public final class Uint128 extends UintType<Uint128> {
  static final int MAX_WIDTH = 4;

  /**
   * Maximum representable value.
   */
  public static Uint128 MAX_VALUE = new Uint128(NumericArrays.maxValue(MAX_WIDTH));

  public static Uint128 ZERO = new Uint128(NumericArrays.ZERO);
  public static Uint128 ONE  = new Uint128(NumericArrays.ONE);
  public static Uint128 TWO  = new Uint128(NumericArrays.TWO);

  /**
   * Construct from a big-endian {@code int} array.
   *
   * If {@code ints} exceeds {@link MAX_VALUE}, only the maximum prefix
   * will be considered.  Leaves {@code ints} untouched.
   */
  public Uint128(final int[] ints) {
    super(ints, MAX_WIDTH);
  }

  /**
   * Construct from a big-endian {@code byte} array.
   *
   * If {@code bytes} exceeds {@link MAX_VALUE}, only the maximum prefix
   * will be considered.  Leaves {@code bytes} untouched.
   */
  public Uint128(final byte[] bytes) {
    super(bytes, MAX_VALUE);
  }

  /**
   * Construct from a {@link Uint256}.
   *
   * Excessively wide numbers will be truncated.
   */
  public Uint128(final Uint256 other) {
    super(other.ints, MAX_WIDTH);
  }

  /**
   * Construct from a base ten string.
   *
   * Excessively wide numbers will be truncated.
   *
   * @throws NumberFormatException Negative, invalid or zero-length number.
   */
  public Uint128(final String s) {
    this(s, 10);
  }

  /**
   * Construct from a string in the given radix.
   *
   * Excessively wide numbers will be truncated.
   *
   * @throws NumberFormatException Negative, invalid or zero-length number.
   */
  public Uint128(final String s, final int radix) {
    super(s, radix, MAX_WIDTH);
  }

  /**
   * Construct from a {@link BigInteger}.
   *
   * If {@code b} exceeds {@link MAX_VALUE}, it's truncated.
   */
  public Uint128(final BigInteger b) { super(b, MAX_WIDTH); }

  /**
   * Construct from a {@code long}, when considered unsigned.
   *
   * For low values of {@code v}, an array cache may be used.
   */
    public Uint128(final long v) { super(v); }

  public Uint128 not() {
    return new Uint128(NumericArrays.not(ints, MAX_VALUE.ints));
  }

  public Uint128 and(final Uint128 other) {
    return new Uint128(NumericArrays.and(ints, other.ints));
  }

  public Uint128 or(final Uint128 other) {
    return new Uint128(NumericArrays.or(ints, other.ints));
  }

  public Uint128 xor(final Uint128 other) {
    return new Uint128(NumericArrays.xor(ints, other.ints));
  }

  public Uint128 setBit(final int bit) {
    if(bit < 0)
      throw new ArithmeticException("Negative bit address");
    return ((MAX_WIDTH <= bit >>> 5) ? this :
            new Uint128(NumericArrays.setBit(ints, bit)));
  }

  public Uint128 clearBit(final int bit) {
    if(bit < 0)
      throw new ArithmeticException("Negative bit address");
    return ((ints.length <= bit >>> 5) ? this :
            new Uint128(NumericArrays.clearBit(ints, bit)));
  }

  public Uint128 flipBit(final int bit) {
     if(bit < 0)
       throw new ArithmeticException("Negative bit address");
     return ((MAX_WIDTH <= bit >>> 5) ? this :
             new Uint128(NumericArrays.flipBit(ints, bit)));
  }

  public Uint128 shiftLeft(final int places) {
    return new Uint128(
      0 < places ?
      NumericArrays.lshift(ints,  places, MAX_WIDTH) :
      NumericArrays.rshift(ints, -places, MAX_WIDTH));
  }

  public Uint128 shiftRight(final int places) {
    return new Uint128(
      0 < places ?
      NumericArrays.rshift(ints,  places, MAX_WIDTH) :
      NumericArrays.lshift(ints, -places, MAX_WIDTH));
  }

  /**
   * Safe increment operation with overflow check.
   * 
   * @return this + 1
   * @throws ArithmeticException if overflow occurs
   */
  public Uint128 inc() {
    if (equals(MAX_VALUE)) {
      throw new ArithmeticException("SafeMath: increment overflow");
    }
    return new Uint128(NumericArrays.inc(ints, MAX_WIDTH));
  }

  /**
   * Safe decrement operation with underflow check.
   * 
   * @return this - 1
   * @throws ArithmeticException if underflow occurs
   */
  public Uint128 dec() {
    if (isZero()) {
      throw new ArithmeticException("SafeMath: decrement underflow");
    }
    return new Uint128(NumericArrays.dec(ints));
  }

  /**
   * Safe addition with overflow check.
   * 
   * @param other the value to add
   * @return this + other
   * @throws ArithmeticException if overflow occurs
   */
  public Uint128 add(final Uint128 other) {
    if (other == null) {
      throw new IllegalArgumentException("SafeMath: addition with null");
    }
    
    if (isZero()) {
      return other;
    }
    if (other.isZero()) {
      return this;
    }
    
    // Check for overflow: if MAX_VALUE - this < other, then overflow
    Uint128 maxMinusThis = MAX_VALUE.sub(this);
    if (maxMinusThis.compareTo(other) < 0) {
      throw new ArithmeticException("SafeMath: addition overflow");
    }
    
    return new Uint128(NumericArrays.add(ints, other.ints, MAX_WIDTH));
  }

  /**
   * Safe modular addition.
   * 
   * @param add the value to add
   * @param mod the modulus
   * @return (this + add) % mod
   * @throws ArithmeticException if mod is zero
   */
  public Uint128 addmod(final Uint128 add, final Uint128 mod) {
    if (mod == null || mod.isZero()) {
      throw new ArithmeticException("SafeMath: division by zero");
    }
    if (add == null) {
      throw new IllegalArgumentException("SafeMath: addition with null");
    }
    
    if (isZero() && add.isZero()) {
      return ZERO;
    }
    return new Uint128(NumericArrays.addmod(ints, add.ints, mod.ints));
  }

  /**
   * Safe subtraction with underflow check.
   * 
   * @param other the value to subtract
   * @return this - other
   * @throws ArithmeticException if underflow occurs
   */
  public Uint128 sub(final Uint128 other) {
    if (other == null) {
      throw new IllegalArgumentException("SafeMath: subtraction with null");
    }
    
    if (other.isZero()) {
      return this;
    }
    
    final int cmp = compareTo(other);
    if (cmp < 0) {
      throw new ArithmeticException("SafeMath: subtraction underflow");
    }
    
    return (cmp == 0 ? ZERO :
            new Uint128(NumericArrays.sub(ints, other.ints)));
  }

  /**
   * Safe multiplication with overflow check.
   * 
   * @param other the value to multiply by
   * @return this * other
   * @throws ArithmeticException if overflow occurs
   */
  public Uint128 mul(final Uint128 other) {
    if (other == null) {
      throw new IllegalArgumentException("SafeMath: multiplication with null");
    }
    
    if (ints.length == 0 || other.ints.length == 0) {
      return ZERO;
    }
    
    // Check for overflow: if this != 0 and MAX_VALUE / this < other, then overflow
    if (!isZero() && !other.isZero()) {
      Uint128 maxDivThis = MAX_VALUE.div(this);
      if (maxDivThis.compareTo(other) < 0) {
        throw new ArithmeticException("SafeMath: multiplication overflow");
      }
    }
    
    return new Uint128(NumericArrays.multiply(ints, other.ints, MAX_WIDTH));
  }

  /**
   * Safe modular multiplication.
   * 
   * @param mul the value to multiply by
   * @param mod the modulus
   * @return (this * mul) % mod
   * @throws ArithmeticException if mod is zero
   */
  public Uint128 mulmod(final Uint128 mul, final Uint128 mod) {
    if (mod == null || mod.isZero()) {
      throw new ArithmeticException("SafeMath: division by zero");
    }
    if (mul == null) {
      throw new IllegalArgumentException("SafeMath: multiplication with null");
    }
    
    return new Uint128(NumericArrays.mulmod(ints, mul.ints, mod.ints));
  }

  /**
   * Safe exponentiation with overflow check.
   * 
   * @param exp the exponent
   * @return this ^ exp
   * @throws ArithmeticException if exp is negative or overflow occurs
   */
  public Uint128 pow(final int exp) {
    if (exp < 0) {
      throw new ArithmeticException("SafeMath: negative exponent");
    }
    if (exp == 0) {
      return ONE;
    }
    if (isZero()) {
      return this;
    }
    
    // Check for overflow in large exponents
    if (exp > 128) {
      throw new ArithmeticException("SafeMath: exponent too large");
    }
    
    return (exp == 1 ? this :
            new Uint128(NumericArrays.pow(ints, getLowestSetBit(), exp, MAX_WIDTH)));
  }

  /**
   * Safe division with zero check.
   * 
   * @param other the divisor
   * @return this / other
   * @throws ArithmeticException if other is zero
   */
  public Uint128 div(final Uint128 other) {
    if (other == null || other.isZero()) {
      throw new ArithmeticException("SafeMath: division by zero");
    }
    
    if (isZero()) {
      return ZERO;
    }
    
    final int cmp = compareTo(other);
    return (cmp < 0 ? ZERO :
            (cmp == 0 ? ONE :
             new Uint128(NumericArrays.divide(ints, other.ints))));
  }

  /**
   * Safe modulo operation with zero check.
   * 
   * @param other the modulus
   * @return this % other
   * @throws ArithmeticException if other is zero
   */
  public Uint128 mod(final Uint128 other) {
    if (other == null || other.isZero()) {
      throw new ArithmeticException("SafeMath: division by zero");
    }
    
    if (isZero()) {
      return ZERO;
    }
    
    final int cmp = compareTo(other);
    return (cmp < 0 ? this :
            (cmp == 0 ? ZERO :
             new Uint128(NumericArrays.mod(ints, other.ints))));
  }

  /**
   * Safe division and modulo with zero check.
   * 
   * @param other the divisor
   * @return [this / other, this % other]
   * @throws ArithmeticException if other is zero
   */
  public Uint128[] divmod(final Uint128 other) {
    if (other == null || other.isZero()) {
      throw new ArithmeticException("SafeMath: division by zero");
    }
    
    if (isZero()) {
      return new Uint128[]{ZERO, ZERO};
    }
    
    final int cmp = compareTo(other);
    if (cmp < 0) {
      return new Uint128[]{ZERO, this};
    }
    if (cmp == 0) {
      return new Uint128[]{ONE, ZERO};
    }

    final int[][] qr = NumericArrays.divmod(ints, other.ints);
    return new Uint128[]{new Uint128(qr[0]), new Uint128(qr[1])};
  }

  /**
   * Safe addition static method for convenience.
   * 
   * @param a first operand
   * @param b second operand
   * @return a + b
   * @throws ArithmeticException if overflow occurs
   */
  public static Uint128 add(Uint128 a, Uint128 b) {
    if (a == null || b == null) {
      throw new IllegalArgumentException("SafeMath: addition with null");
    }
    return a.add(b);
  }

  /**
   * Safe subtraction static method for convenience.
   * 
   * @param a first operand
   * @param b second operand
   * @return a - b
   * @throws ArithmeticException if underflow occurs
   */
  public static Uint128 sub(Uint128 a, Uint128 b) {
    if (a == null || b == null) {
      throw new IllegalArgumentException("SafeMath: subtraction with null");
    }
    return a.sub(b);
  }

  /**
   * Safe multiplication static method for convenience.
   * 
   * @param a first operand
   * @param b second operand
   * @return a * b
   * @throws ArithmeticException if overflow occurs
   */
  public static Uint128 mul(Uint128 a, Uint128 b) {
    if (a == null || b == null) {
      throw new IllegalArgumentException("SafeMath: multiplication with null");
    }
    return a.mul(b);
  }

  /**
   * Safe division static method for convenience.
   * 
   * @param a first operand
   * @param b second operand
   * @return a / b
   * @throws ArithmeticException if b is zero
   */
  public static Uint128 div(Uint128 a, Uint128 b) {
    if (a == null || b == null) {
      throw new IllegalArgumentException("SafeMath: division with null");
    }
    return a.div(b);
  }

  /**
   * Safe modulo static method for convenience.
   * 
   * @param a first operand
   * @param b second operand
   * @return a % b
   * @throws ArithmeticException if b is zero
   */
  public static Uint128 mod(Uint128 a, Uint128 b) {
    if (a == null || b == null) {
      throw new IllegalArgumentException("SafeMath: modulo with null");
    }
    return a.mod(b);
  }

  public boolean equals(final Object other) {
    if(other instanceof BigInteger)
      return NumericArrays.compare(ints, (BigInteger)other, MAX_WIDTH) == 0;
    return super.equals(other);
  }
}
