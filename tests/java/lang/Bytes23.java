package java.lang;

/**
 * Fixed-size bytes type representing exactly 23 bytes.
 */
public class Bytes23 extends Bytes {
    public static final Bytes23 DEFAULT = new Bytes23(new byte[23]);

    public Bytes23(byte[] value) {
        super(23, value);
    }
}
