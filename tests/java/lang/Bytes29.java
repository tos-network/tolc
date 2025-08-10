package java.lang;

/**
 * Fixed-size bytes type representing exactly 29 bytes.
 */
public class Bytes29 extends Bytes {
    public static final Bytes29 DEFAULT = new Bytes29(new byte[29]);

    public Bytes29(byte[] value) {
        super(29, value);
    }
}
