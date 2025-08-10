package java.lang;

/**
 * Fixed-size bytes type representing exactly 11 bytes.
 */
public class Bytes11 extends Bytes {
    public static final Bytes11 DEFAULT = new Bytes11(new byte[11]);

    public Bytes11(byte[] value) {
        super(11, value);
    }
}
