package java.lang;

/**
 * Fixed-size bytes type representing exactly 14 bytes.
 */
public class Bytes14 extends Bytes {
    public static final Bytes14 DEFAULT = new Bytes14(new byte[14]);

    public Bytes14(byte[] value) {
        super(14, value);
    }
}
