package java.lang;

/**
 * Fixed-size bytes type representing exactly 30 bytes.
 */
public class Bytes30 extends Bytes {
    public static final Bytes30 DEFAULT = new Bytes30(new byte[30]);

    public Bytes30(byte[] value) {
        super(30, value);
    }
}
