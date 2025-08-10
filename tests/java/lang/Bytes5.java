package java.lang;

/**
 * Fixed-size bytes type representing exactly 5 bytes.
 */
public class Bytes5 extends Bytes {
    public static final Bytes5 DEFAULT = new Bytes5(new byte[5]);

    public Bytes5(byte[] value) {
        super(5, value);
    }
}
