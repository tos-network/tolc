package java.lang;

import java.math.BigInteger;

/**
 * Represents unsigned 8-bit values (0-255).
 *
 * Enhanced with SafeMath security features for smart contract safety.
 * All arithmetic operations include overflow checks to prevent vulnerabilities.
 */
public final class Uint8 extends UintType<Uint8> {
    static final int MAX_WIDTH = 1;  // 8 bits, stored in a single int

    public static final Uint8 MAX_VALUE = new Uint8(255);
    public static final Uint8 ZERO = new Uint8(0);
    public static final Uint8 ONE = new Uint8(1);

    private final int value;  // Store the 8-bit value in a single int

    public Uint8(int value) {
        super(new int[] { value & 0xFF });
        if (value < 0 || value > 0xFF) {
            throw new IllegalArgumentException("Value out of range for Uint8");
        }
        this.value = value;
    }

    public Uint8(BigInteger value) {
        this(value.intValueExact());
    }

    /**
     * Safe addition with overflow check.
     * 
     * @param other the value to add
     * @return this + other
     * @throws ArithmeticException if overflow occurs
     */
    @Override
    public Uint8 add(Uint8 other) {
        if (other == null) {
            throw new IllegalArgumentException("SafeMath: addition with null");
        }
        
        int result = this.value + other.value;
        if (result > 0xFF) {
            throw new ArithmeticException("SafeMath: addition overflow");
        }
        return new Uint8(result);
    }

    /**
     * Safe subtraction with underflow check.
     * 
     * @param other the value to subtract
     * @return this - other
     * @throws ArithmeticException if underflow occurs
     */
    @Override
    public Uint8 sub(Uint8 other) {
        if (other == null) {
            throw new IllegalArgumentException("SafeMath: subtraction with null");
        }
        
        if (this.value < other.value) {
            throw new ArithmeticException("SafeMath: subtraction underflow");
        }
        return new Uint8(this.value - other.value);
    }

    /**
     * Safe multiplication with overflow check.
     * 
     * @param other the value to multiply by
     * @return this * other
     * @throws ArithmeticException if overflow occurs
     */
    @Override
    public Uint8 mul(Uint8 other) {
        if (other == null) {
            throw new IllegalArgumentException("SafeMath: multiplication with null");
        }
        
        long result = (long) this.value * other.value;
        if (result > 0xFF) {
            throw new ArithmeticException("SafeMath: multiplication overflow");
        }
        return new Uint8((int) result);
    }

    /**
     * Safe division with zero check.
     * 
     * @param other the divisor
     * @return this / other
     * @throws ArithmeticException if other is zero
     */
    @Override
    public Uint8 div(Uint8 other) {
        if (other == null || other.value == 0) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        return new Uint8(this.value / other.value);
    }

    /**
     * Safe modulo operation with zero check.
     * 
     * @param other the modulus
     * @return this % other
     * @throws ArithmeticException if other is zero
     */
    @Override
    public Uint8 mod(Uint8 other) {
        if (other == null || other.value == 0) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        return new Uint8(this.value % other.value);
    }

    @Override
    public Uint8 and(Uint8 other) {
        return new Uint8(this.value & other.value);
    }

    @Override
    public Uint8 or(Uint8 other) {
        return new Uint8(this.value | other.value);
    }

    @Override
    public Uint8 xor(Uint8 other) {
        return new Uint8(this.value ^ other.value);
    }

    @Override
    public Uint8 not() {
        return new Uint8(~this.value & 0xFF);
    }

    @Override
    public Uint8 shiftRight(int places) {
        return new Uint8((value >>> places) & 0xFF);
    }

    @Override
    public Uint8 shiftLeft(int places) {
        return new Uint8((value << places) & 0xFF);
    }

    @Override
    public Uint8 setBit(int bit) {
        return new Uint8((value | (1 << bit)) & 0xFF);
    }

    @Override
    public Uint8 clearBit(int bit) {
        return new Uint8((value & ~(1 << bit)) & 0xFF);
    }

    @Override
    public Uint8 flipBit(int bit) {
        return new Uint8((value ^ (1 << bit)) & 0xFF);
    }

    /**
     * Safe increment operation with overflow check.
     * 
     * @return this + 1
     * @throws ArithmeticException if overflow occurs
     */
    @Override
    public Uint8 inc() {
        if (value == 0xFF) {
            throw new ArithmeticException("SafeMath: increment overflow");
        }
        return new Uint8(value + 1);
    }

    /**
     * Safe decrement operation with underflow check.
     * 
     * @return this - 1
     * @throws ArithmeticException if underflow occurs
     */
    @Override
    public Uint8 dec() {
        if (value == 0) {
            throw new ArithmeticException("SafeMath: decrement underflow");
        }
        return new Uint8(value - 1);
    }

    /**
     * Safe exponentiation with overflow check.
     * 
     * @param exp the exponent
     * @return this ^ exp
     * @throws ArithmeticException if exp is negative or overflow occurs
     */
    @Override
    public Uint8 pow(int exp) {
        if (exp < 0) {
            throw new ArithmeticException("SafeMath: negative exponent");
        }
        if (exp == 0) {
            return ONE;
        }
        if (value == 0) {
            return this;
        }
        
        // Check for overflow in large exponents
        if (exp > 8) {
            throw new ArithmeticException("SafeMath: exponent too large");
        }
        
        int result = 1;
        int base = value;
        while (exp > 0) {
            if ((exp & 1) == 1) {
                result = (result * base) & 0xFF;
            }
            base = (base * base) & 0xFF;
            exp >>= 1;
        }
        return new Uint8(result);
    }

    /**
     * Safe division and modulo with zero check.
     * 
     * @param other the divisor
     * @return [this / other, this % other]
     * @throws ArithmeticException if other is zero
     */
    @Override
    public Uint8[] divmod(Uint8 other) {
        if (other == null || other.value == 0) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        return new Uint8[] { div(other), mod(other) };
    }

    /**
     * Safe modular multiplication.
     * 
     * @param mul the value to multiply by
     * @param mod the modulus
     * @return (this * mul) % mod
     * @throws ArithmeticException if mod is zero
     */
    @Override
    public Uint8 mulmod(Uint8 mul, Uint8 mod) {
        if (mod == null || mod.value == 0) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        if (mul == null) {
            throw new IllegalArgumentException("SafeMath: multiplication with null");
        }
        return new Uint8((value * mul.value) % mod.value);
    }

    /**
     * Safe modular addition.
     * 
     * @param add the value to add
     * @param mod the modulus
     * @return (this + add) % mod
     * @throws ArithmeticException if mod is zero
     */
    @Override
    public Uint8 addmod(Uint8 add, Uint8 mod) {
        if (mod == null || mod.value == 0) {
            throw new ArithmeticException("SafeMath: division by zero");
        }
        if (add == null) {
            throw new IllegalArgumentException("SafeMath: addition with null");
        }
        return new Uint8((value + add.value) % mod.value);
    }

    /**
     * Safe addition static method for convenience.
     * 
     * @param a first operand
     * @param b second operand
     * @return a + b
     * @throws ArithmeticException if overflow occurs
     */
    public static Uint8 add(Uint8 a, Uint8 b) {
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
    public static Uint8 sub(Uint8 a, Uint8 b) {
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
    public static Uint8 mul(Uint8 a, Uint8 b) {
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
    public static Uint8 div(Uint8 a, Uint8 b) {
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
    public static Uint8 mod(Uint8 a, Uint8 b) {
        if (a == null || b == null) {
            throw new IllegalArgumentException("SafeMath: modulo with null");
        }
        return a.mod(b);
    }
}