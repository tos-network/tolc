package java.lang;

/**
 * Fixed-size bytes type representing exactly 18 bytes.
 */
public class Bytes18 extends Bytes {
    public static final Bytes18 DEFAULT = new Bytes18(new byte[18]);

    public Bytes18(byte[] value) {
        super(18, value);
    }
}
