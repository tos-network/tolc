package java.lang;

/**
 * Fixed-size bytes type representing exactly 26 bytes.
 */
public class Bytes26 extends Bytes {
    public static final Bytes26 DEFAULT = new Bytes26(new byte[26]);

    public Bytes26(byte[] value) {
        super(26, value);
    }
}
