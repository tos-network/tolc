package java.lang;

/**
 * Fixed-size bytes type representing exactly 19 bytes.
 */
public class Bytes19 extends Bytes {
    public static final Bytes19 DEFAULT = new Bytes19(new byte[19]);

    public Bytes19(byte[] value) {
        super(19, value);
    }
}
