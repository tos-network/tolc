package java.lang;

import java.math.BigInteger;

/**
 * Represents unsigned values less than {@code 2**16}.
 *
 * Enhanced with SafeMath security features for smart contract safety.
 * All arithmetic operations include overflow checks to prevent vulnerabilities.
 *
 * As indicated by the type signatures, arithmetic operations are not applicable
 * to types of other widths in this package. Copy constructors can be used to
 * explicitly promote or truncate values for the purposes of interoperability.
 */
public final class Uint16 extends UintType<Uint16> {
    static final int MAX_WIDTH = 1; // 16 bits fits in 1 * 32 bits

    /**
     * Maximum representable value (65535).
     */
    public static Uint16 MAX_VALUE = new Uint16(65535);

    public static Uint16 ZERO = new Uint16(0);
    public static Uint16 ONE = new Uint16(1);
    public static Uint16 TWO = new Uint16(2);

    /**
     * Construct from a big-endian {@code int} array.
     *
     * If {@code ints} exceeds {@link MAX_VALUE}, only the maximum prefix
     * will be considered. Leaves {@code ints} untouched.
     */
    public Uint16(final int[] ints) {
        super(ints, MAX_WIDTH);
    }

    /**
     * Construct from a big-endian {@code byte} array.
     *
     * If {@code bytes} exceeds {@link MAX_VALUE}, only the maximum prefix
     * will be considered. Leaves {@code bytes} untouched.
     */
    public Uint16(final byte[] bytes) {
        super(bytes, new Uint16(65535));
    }

    /**
     * Construct from a base ten string.
     *
     * Excessively wide numbers will be truncated.
     *
     * @throws NumberFormatException Negative, invalid or zero-length number.
     */
    public Uint16(final String s) {
        this(s, 10);
    }

    /**
     * Construct from a string in the given radix.
     *
     * Excessively wide numbers will be truncated.
     *
     * @throws NumberFormatException Negative, invalid or zero-length number.
     */
    public Uint16(final String s, final int radix) {
        super(s, radix, MAX_WIDTH);
    }

    /**
     * Construct from a {@link BigInteger}.
     *
     * If {@code b} exceeds {@link MAX_VALUE}, it's truncated.
     */
    public Uint16(final BigInteger b) {
        super(b, MAX_WIDTH);
    }

    /**
     * Construct from a {@code long}, when considered unsigned.
     *
     * For low values of {@code v}, an array cache may be used.
     * Values exceeding 16-bit range will be truncated.
     */
    public Uint16(final long v) {
        super(v & 0xFFFF); // Mask to 16 bits
    }

    /**
     * Construct from an {@code int}, when considered unsigned.
     *
     * Values exceeding 16-bit range will be truncated.
     */
    public Uint16(final int v) {
        super(v & 0xFFFF); // Mask to 16 bits
    }

    /**
     * Construct from a {@code short}, when considered unsigned.
     */
    public Uint16(final short v) {
        super(v & 0xFFFF); // Convert to unsigned
    }

    public Uint16 not() {
        return new Uint16(NumericArrays.not(ints, MAX_VALUE.ints));
    }

    public Uint16 and(final Uint16 other) {
        return new Uint16(NumericArrays.and(ints, other.ints));
    }

    public Uint16 or(final Uint16 other) {
        return new Uint16(NumericArrays.or(ints, other.ints));
    }

    public Uint16 xor(final Uint16 other) {
        return new Uint16(NumericArrays.xor(ints, other.ints));
    }

    public Uint16 setBit(final int bit) {
        if (bit < 0)
            throw new ArithmeticException("Negative bit address");
        if (bit >= 16)
            throw new ArithmeticException("Bit address exceeds 16-bit range");
        return ((MAX_WIDTH <= bit >>> 5) ? this :
                new Uint16(NumericArrays.setBit(ints, bit)));
    }

    public Uint16 clearBit(final int bit) {
        if (bit < 0)
            throw new ArithmeticException("Negative bit address");
        if (bit >= 16)
            return this; // Bit is already clear outside range
        return ((ints.length <= bit >>> 5) ? this :
                new Uint16(NumericArrays.clearBit(ints, bit)));
    }

    public Uint16 flipBit(final int bit) {
        if (bit < 0)
            throw new ArithmeticException("Negative bit address");
        if (bit >= 16)
            throw new ArithmeticException("Bit address exceeds 16-bit range");
        return ((MAX_WIDTH <= bit >>> 5) ? this :
                new Uint16(NumericArrays.flipBit(ints, bit)));
    }

    public Uint16 shiftLeft(final int places) {
        if (places < 0) {
            return shiftRight(-places);
        }
        if (places >= 16) {
            return ZERO; // Shift beyond bit width
        }
        return new Uint16(
                NumericArrays.lshift(ints, places, MAX_WIDTH));
    }

    public Uint16 shiftRight(final int places) {
        if (places < 0) {
            return shiftLeft(-places);
        }
        if (places >= 16) {
            return ZERO; // Shift beyond bit width
        }
        return new Uint16(
                NumericArrays.rshift(ints, places, MAX_WIDTH));
    }

    /**
     * Safe increment operation with overflow check.
     * 
     * @return this + 1
     * @throws ArithmeticException if overflow occurs
     */
    public Uint16 inc() {
        if (equals(MAX_VALUE)) {
            throw new ArithmeticException("SafeMath: increment overflow");
        }
        return new Uint16(NumericArrays.inc(ints, MAX_WIDTH));
    }

    /**
     * Safe decrement operation with underflow check.
     * 
     * @return this - 1
     * @throws ArithmeticException if underflow occurs
     */
    public Uint16 dec() {
        if (isZero()) {
            throw new ArithmeticException("SafeMath: decrement underflow");
        }
        return new Uint16(NumericArrays.dec(ints));
    }

    /**
     * Safe addition with overflow check.
     * 
     * @param other the value to add
     * @return this + other
     * @throws ArithmeticException if overflow occurs
     */
    public Uint16 add(final Uint16 other) {
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
        Uint16 maxMinusThis = MAX_VALUE.sub(this);
        if (maxMinusThis.compareTo(other) < 0) {
            throw new ArithmeticException("SafeMath: addition overflow");
        }
        
        return new Uint16(NumericArrays.add(ints, other.ints, MAX_WIDTH));
    }

    /**
     * Safe modular addition.
     * 
     * @param add the value to add
     * @param mod the modulus
     * @return (this + add) % mod
     * @throws ArithmeticException if mod is zero
     */
    public Uint16 addmod(final Uint16 add, final Uint16 mod) {
        if (mod == null || mod.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        if (add == null) {
            throw new IllegalArgumentException("SafeMath: addition with null");
        }
        
        if (isZero() && add.isZero()) {
            return ZERO;
        }
        return new Uint16(NumericArrays.addmod(ints, add.ints, mod.ints));
    }

    /**
     * Safe subtraction with underflow check.
     * 
     * @param other the value to subtract
     * @return this - other
     * @throws ArithmeticException if underflow occurs
     */
    public Uint16 sub(final Uint16 other) {
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
                new Uint16(NumericArrays.sub(ints, other.ints)));
    }

    /**
     * Safe multiplication with overflow check.
     * 
     * @param other the value to multiply by
     * @return this * other
     * @throws ArithmeticException if overflow occurs
     */
    public Uint16 mul(final Uint16 other) {
        if (other == null) {
            throw new IllegalArgumentException("SafeMath: multiplication with null");
        }
        
        if (ints.length == 0 || other.ints.length == 0) {
            return ZERO;
        }
        
        // Check for overflow: if this != 0 and MAX_VALUE / this < other, then overflow
        if (!isZero() && !other.isZero()) {
            Uint16 maxDivThis = MAX_VALUE.div(this);
            if (maxDivThis.compareTo(other) < 0) {
                throw new ArithmeticException("SafeMath: multiplication overflow");
            }
        }
        
        return new Uint16(NumericArrays.multiply(ints, other.ints, MAX_WIDTH));
    }

    /**
     * Safe modular multiplication.
     * 
     * @param mul the value to multiply by
     * @param mod the modulus
     * @return (this * mul) % mod
     * @throws ArithmeticException if mod is zero
     */
    public Uint16 mulmod(final Uint16 mul, final Uint16 mod) {
        if (mod == null || mod.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        if (mul == null) {
            throw new IllegalArgumentException("SafeMath: multiplication with null");
        }
        
        return new Uint16(NumericArrays.mulmod(ints, mul.ints, mod.ints));
    }

    /**
     * Safe exponentiation with overflow check.
     * 
     * @param exp the exponent
     * @return this ^ exp
     * @throws ArithmeticException if exp is negative or overflow occurs
     */
    public Uint16 pow(final int exp) {
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
        if (exp > 16) {
            throw new ArithmeticException("SafeMath: exponent too large");
        }
        
        return (exp == 1 ? this :
                new Uint16(NumericArrays.pow(ints, getLowestSetBit(), exp, MAX_WIDTH)));
    }

    /**
     * Safe division with zero check.
     * 
     * @param other the divisor
     * @return this / other
     * @throws ArithmeticException if other is zero
     */
    public Uint16 div(final Uint16 other) {
        if (other == null || other.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        
        if (isZero()) {
            return ZERO;
        }
        
        final int cmp = compareTo(other);
        return (cmp < 0 ? ZERO :
                (cmp == 0 ? ONE :
                        new Uint16(NumericArrays.divide(ints, other.ints))));
    }

    /**
     * Safe modulo operation with zero check.
     * 
     * @param other the modulus
     * @return this % other
     * @throws ArithmeticException if other is zero
     */
    public Uint16 mod(final Uint16 other) {
        if (other == null || other.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        
        if (isZero()) {
            return ZERO;
        }
        
        final int cmp = compareTo(other);
        return (cmp < 0 ? this :
                (cmp == 0 ? ZERO :
                        new Uint16(NumericArrays.mod(ints, other.ints))));
    }

    /**
     * Safe division and modulo with zero check.
     * 
     * @param other the divisor
     * @return [this / other, this % other]
     * @throws ArithmeticException if other is zero
     */
    public Uint16[] divmod(final Uint16 other) {
        if (other == null || other.isZero()) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        
        if (isZero()) {
            return new Uint16[]{ZERO, ZERO};
        }
        
        final int cmp = compareTo(other);
        if (cmp < 0) {
            return new Uint16[]{ZERO, this};
        }
        if (cmp == 0) {
            return new Uint16[]{ONE, ZERO};
        }

        final int[][] qr = NumericArrays.divmod(ints, other.ints);
        return new Uint16[]{new Uint16(qr[0]), new Uint16(qr[1])};
    }

    /**
     * Safe addition static method for convenience.
     * 
     * @param a first operand
     * @param b second operand
     * @return a + b
     * @throws ArithmeticException if overflow occurs
     */
    public static Uint16 add(Uint16 a, Uint16 b) {
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
    public static Uint16 sub(Uint16 a, Uint16 b) {
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
    public static Uint16 mul(Uint16 a, Uint16 b) {
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
    public static Uint16 div(Uint16 a, Uint16 b) {
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
    public static Uint16 mod(Uint16 a, Uint16 b) {
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
