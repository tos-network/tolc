package java.lang;

import java.lang.bytes.Hex;
import java.lang.bytes.Type;
import java.math.BigInteger;

/**
 * Address type, which by default is equivalent to uint160 
 * which follows the Ethereum specification.
 */
public class Address implements Type<String> {

    public static final String TYPE_NAME = "address";
    public static final int DEFAULT_LENGTH = 160;
    public static final Address DEFAULT = new Address(BigInteger.ZERO);
    public static final Address ZERO_ADDRESS = new Address(BigInteger.ZERO);

    private final Uint160 value;

    public Address(Uint160 value) {
        this.value = value;
    }

    public Address(BigInteger value) {
        this(DEFAULT_LENGTH, value);
    }

    public Address(int bitSize, BigInteger value) {
        this(new Uint160(value));
    }

    public Address(String hexValue) {
        this(DEFAULT_LENGTH, hexValue);
    }

    public Address(int bitSize, String hexValue) {
        this(bitSize, Hex.toBigInt(hexValue));
    }

    public Uint160 toUint() {
        return value;
    }

     /**
   * {@code this == 0}
   */
    public final boolean isZeroAddress() {
        return value.isZero();
    }

    @Override
    public String getTypeAsString() {
        return TYPE_NAME;
    }

    @Override
    public String toString() {
        return Hex.toHexStringWithPrefixZeroPadded(value.toBigInteger(), DEFAULT_LENGTH >> 2);
    }

    @Override
    public String getValue() {
        return toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null) {
            return value == null || value.isZero();
        }
        if (!(o instanceof Address)) {
            return false;
        }

        Address address = (Address) o;

        return value != null ? value.equals(address.value) : address.value == null;
    }

    @Override
    public int hashCode() {
        return value != null ? value.hashCode() : 0;
    }
}
