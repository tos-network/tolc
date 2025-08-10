package java.lang;

/**
 * Fixed-size bytes type representing exactly 32 bytes.
 */
public class Bytes32 extends Bytes {
    public static final Bytes32 DEFAULT = new Bytes32(new byte[32]);

    public Bytes32(byte[] value) {
        super(32, value);
    }
}
