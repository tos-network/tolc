package java.lang;

/**
 * Fixed-size bytes type representing exactly 24 bytes.
 */
public class Bytes24 extends Bytes {
    public static final Bytes24 DEFAULT = new Bytes24(new byte[24]);

    public Bytes24(byte[] value) {
        super(24, value);
    }
}
