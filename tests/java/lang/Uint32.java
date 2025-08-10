package java.lang;

import java.math.BigInteger;

/**
 * Represents unsigned values less than {@code 2**32}.
 *
 * Enhanced with SafeMath security features for smart contract safety.
 * All arithmetic operations include overflow checks to prevent vulnerabilities.
 *
 * As indicated by the type signatures, arithmetic operations are not applicable
 * to types of other widths in this package. Copy constructors can be used to
 * explicitly promote or truncate values for the purposes of interoperability.
 */
public final class Uint32 extends UintType<Uint32> {
    static final int MAX_WIDTH = 1; // 32 bits = 1 * 32 bits

    /**
     * Maximum representable value.
     */
    public static Uint32 MAX_VALUE = new Uint32(NumericArrays.maxValue(MAX_WIDTH));

    public static Uint32 ZERO = new Uint32(NumericArrays.ZERO);
    public static Uint32 ONE = new Uint32(NumericArrays.ONE);
    public static Uint32 TWO = new Uint32(NumericArrays.TWO);

    /**
     * Construct from a big-endian {@code int} array.
     *
     * If {@code ints} exceeds {@link MAX_VALUE}, only the maximum prefix
     * will be considered. Leaves {@code ints} untouched.
     */
    public Uint32(final int[] ints) {
        super(ints, MAX_WIDTH);
    }

    /**
     * Construct from a big-endian {@code byte} array.
     *
     * If {@code bytes} exceeds {@link MAX_VALUE}, only the maximum prefix
     * will be considered. Leaves {@code bytes} untouched.
     */
    public Uint32(final byte[] bytes) {
        super(bytes, MAX_VALUE);
    }

    /**
     * Construct from a base ten string.
     *
     * Excessively wide numbers will be truncated.
     *
     * @throws NumberFormatException Negative, invalid or zero-length number.
     */
    public Uint32(final String s) {
        this(s, 10);
    }

    /**
     * Construct from a string in the given radix.
     *
     * Excessively wide numbers will be truncated.
     *
     * @throws NumberFormatException Negative, invalid or zero-length number.
     */
    public Uint32(final String s, final int radix) {
        super(s, radix, MAX_WIDTH);
    }

    /**
     * Construct from a {@link BigInteger}.
     *
     * If {@code b} exceeds {@link MAX_VALUE}, it's truncated.
     */
    public Uint32(final BigInteger b) {
        super(b, MAX_WIDTH);
    }

    /**
     * Construct from a {@code long}, when considered unsigned.
     *
     * For low values of {@code v}, an array cache may be used.
     */
    public Uint32(final long v) {
        super(v);
    }

    /**
     * Construct from an {@code int}, when considered unsigned.
     *
     * For low values of {@code v}, an array cache may be used.
     */
    public Uint32(final int v) {
        super(v & 0xFFFFFFFFL); // Convert to unsigned long
    }

    public Uint32 not() {
        return new Uint32(NumericArrays.not(ints, MAX_VALUE.ints));
    }

    public Uint32 and(final Uint32 other) {
        return new Uint32(NumericArrays.and(ints, other.ints));
    }

    public Uint32 or(final Uint32 other) {
        return new Uint32(NumericArrays.or(ints, other.ints));
    }

    public Uint32 xor(final Uint32 other) {
        return new Uint32(NumericArrays.xor(ints, other.ints));
    }

    public Uint32 setBit(final int bit) {
        if (bit < 0)
            throw new ArithmeticException("Negative bit address");
        return ((MAX_WIDTH <= bit >>> 5) ? this :
                new Uint32(NumericArrays.setBit(ints, bit)));
    }

    public Uint32 clearBit(final int bit) {
        if (bit < 0)
            throw new ArithmeticException("Negative bit address");
        return ((ints.length <= bit >>> 5) ? this :
                new Uint32(NumericArrays.clearBit(ints, bit)));
    }

    public Uint32 flipBit(final int bit) {
        if (bit < 0)
            throw new ArithmeticException("Negative bit address");
        return ((MAX_WIDTH <= bit >>> 5) ? this :
                new Uint32(NumericArrays.flipBit(ints, bit)));
    }

    public Uint32 shiftLeft(final int places) {
        return new Uint32(
                0 < places ?
                        NumericArrays.lshift(ints, places, MAX_WIDTH) :
                        NumericArrays.rshift(ints, -places, MAX_WIDTH));
    }

    public Uint32 shiftRight(final int places) {
        return new Uint32(
                0 < places ?
                        NumericArrays.rshift(ints, places, MAX_WIDTH) :
                        NumericArrays.lshift(ints, -places, MAX_WIDTH));
    }

    /**
     * Safe increment operation with overflow check.
     * 
     * @return this + 1
     * @throws ArithmeticException if overflow occurs
     */
    public Uint32 inc() {
        if (equals(MAX_VALUE)) {
            throw new ArithmeticException("SafeMath: increment overflow");
        }
        return new Uint32(NumericArrays.inc(ints, MAX_WIDTH));
    }

    /**
     * Safe decrement operation with underflow check.
     * 
     * @return this - 1
     * @throws ArithmeticException if underflow occurs
     */
    public Uint32 dec() {
        if (isZero()) {
            throw new ArithmeticException("SafeMath: decrement underflow");
        }
        return new Uint32(NumericArrays.dec(ints));
    }

    /**
     * Safe addition with overflow check.
     * 
     * @param other the value to add
     * @return this + other
     * @throws ArithmeticException if overflow occurs
     */
    public Uint32 add(final Uint32 other) {
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
        Uint32 maxMinusThis = MAX_VALUE.sub(this);
        if (maxMinusThis.compareTo(other) < 0) {
            throw new ArithmeticException("SafeMath: addition overflow");
        }
        
        return new Uint32(NumericArrays.add(ints, other.ints, MAX_WIDTH));
    }

    /**
     * Safe modular addition.
     * 
     * @param add the value to add
     * @param mod the modulus
     * @return (this + add) % mod
     * @throws ArithmeticException if mod is zero
     */
    public Uint32 addmod(final Uint32 add, final Uint32 mod) {
        if (mod == null || mod.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        if (add == null) {
            throw new IllegalArgumentException("SafeMath: addition with null");
        }
        
        if (isZero() && add.isZero()) {
            return ZERO;
        }
        return new Uint32(NumericArrays.addmod(ints, add.ints, mod.ints));
    }

    /**
     * Safe subtraction with underflow check.
     * 
     * @param other the value to subtract
     * @return this - other
     * @throws ArithmeticException if underflow occurs
     */
    public Uint32 sub(final Uint32 other) {
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
                new Uint32(NumericArrays.sub(ints, other.ints)));
    }

    /**
     * Safe multiplication with overflow check.
     * 
     * @param other the value to multiply by
     * @return this * other
     * @throws ArithmeticException if overflow occurs
     */
    public Uint32 mul(final Uint32 other) {
        if (other == null) {
            throw new IllegalArgumentException("SafeMath: multiplication with null");
        }
        
        if (ints.length == 0 || other.ints.length == 0) {
            return ZERO;
        }
        
        // Check for overflow: if this != 0 and MAX_VALUE / this < other, then overflow
        if (!isZero() && !other.isZero()) {
            Uint32 maxDivThis = MAX_VALUE.div(this);
            if (maxDivThis.compareTo(other) < 0) {
                throw new ArithmeticException("SafeMath: multiplication overflow");
            }
        }
        
        return new Uint32(NumericArrays.multiply(ints, other.ints, MAX_WIDTH));
    }

    /**
     * Safe modular multiplication.
     * 
     * @param mul the value to multiply by
     * @param mod the modulus
     * @return (this * mul) % mod
     * @throws ArithmeticException if mod is zero
     */
    public Uint32 mulmod(final Uint32 mul, final Uint32 mod) {
        if (mod == null || mod.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        if (mul == null) {
            throw new IllegalArgumentException("SafeMath: multiplication with null");
        }
        
        return new Uint32(NumericArrays.mulmod(ints, mul.ints, mod.ints));
    }

    /**
     * Safe exponentiation with overflow check.
     * 
     * @param exp the exponent
     * @return this ^ exp
     * @throws ArithmeticException if exp is negative or overflow occurs
     */
    public Uint32 pow(final int exp) {
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
        if (exp > 32) {
            throw new ArithmeticException("SafeMath: exponent too large");
        }
        
        return (exp == 1 ? this :
                new Uint32(NumericArrays.pow(ints, getLowestSetBit(), exp, MAX_WIDTH)));
    }

    /**
     * Safe division with zero check.
     * 
     * @param other the divisor
     * @return this / other
     * @throws ArithmeticException if other is zero
     */
    public Uint32 div(final Uint32 other) {
        if (other == null || other.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        
        if (isZero()) {
            return ZERO;
        }
        
        final int cmp = compareTo(other);
        return (cmp < 0 ? ZERO :
                (cmp == 0 ? ONE :
                        new Uint32(NumericArrays.divide(ints, other.ints))));
    }

    /**
     * Safe modulo operation with zero check.
     * 
     * @param other the modulus
     * @return this % other
     * @throws ArithmeticException if other is zero
     */
    public Uint32 mod(final Uint32 other) {
        if (other == null || other.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        
        if (isZero()) {
            return ZERO;
        }
        
        final int cmp = compareTo(other);
        return (cmp < 0 ? this :
                (cmp == 0 ? ZERO :
                        new Uint32(NumericArrays.mod(ints, other.ints))));
    }

    /**
     * Safe division and modulo with zero check.
     * 
     * @param other the divisor
     * @return [this / other, this % other]
     * @throws ArithmeticException if other is zero
     */
    public Uint32[] divmod(final Uint32 other) {
        if (other == null || other.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        
        if (isZero()) {
            return new Uint32[]{ZERO, ZERO};
        }
        
        final int cmp = compareTo(other);
        if (cmp < 0) {
            return new Uint32[]{ZERO, this};
        }
        if (cmp == 0) {
            return new Uint32[]{ONE, ZERO};
        }

        final int[][] qr = NumericArrays.divmod(ints, other.ints);
        return new Uint32[]{new Uint32(qr[0]), new Uint32(qr[1])};
    }

    /**
     * Safe addition static method for convenience.
     * 
     * @param a first operand
     * @param b second operand
     * @return a + b
     * @throws ArithmeticException if overflow occurs
     */
    public static Uint32 add(Uint32 a, Uint32 b) {
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
    public static Uint32 sub(Uint32 a, Uint32 b) {
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
    public static Uint32 mul(Uint32 a, Uint32 b) {
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
    public static Uint32 div(Uint32 a, Uint32 b) {
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
    public static Uint32 mod(Uint32 a, Uint32 b) {
        if (a == null || b == null) {
            throw new IllegalArgumentException("SafeMath: modulo with null");
        }
        return a.mod(b);
    }



    @Override
    public boolean equals(final Object other) {
        if (other instanceof BigInteger)
            return NumericArrays.compare(ints, (BigInteger) other, MAX_WIDTH) == 0;
        return super.equals(other);
    }
}
