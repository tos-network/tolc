package java.lang;

/**
 * Fixed-size bytes type representing exactly 21 bytes.
 */
public class Bytes21 extends Bytes {
    public static final Bytes21 DEFAULT = new Bytes21(new byte[21]);

    public Bytes21(byte[] value) {
        super(21, value);
    }
}
