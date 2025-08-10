package java.lang;

/**
 * Fixed-size bytes type representing exactly 20 bytes.
 */
public class Bytes20 extends Bytes {
    public static final Bytes20 DEFAULT = new Bytes20(new byte[20]);

    public Bytes20(byte[] value) {
        super(20, value);
    }
}
