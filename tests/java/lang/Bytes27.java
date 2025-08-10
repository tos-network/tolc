package java.lang;

/**
 * Fixed-size bytes type representing exactly 27 bytes.
 */
public class Bytes27 extends Bytes {
    public static final Bytes27 DEFAULT = new Bytes27(new byte[27]);

    public Bytes27(byte[] value) {
        super(27, value);
    }
}
