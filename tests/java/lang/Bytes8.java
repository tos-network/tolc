package java.lang;

/**
 * Fixed-size bytes type representing exactly 8 bytes.
 */
public class Bytes8 extends Bytes {
    public static final Bytes8 DEFAULT = new Bytes8(new byte[8]);

    public Bytes8(byte[] value) {
        super(8, value);
    }
}
