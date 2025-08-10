package java.lang;

/**
 * Fixed-size bytes type representing exactly 9 bytes.
 */
public class Bytes9 extends Bytes {
    public static final Bytes9 DEFAULT = new Bytes9(new byte[9]);

    public Bytes9(byte[] value) {
        super(9, value);
    }
}
