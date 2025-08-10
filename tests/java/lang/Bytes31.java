package java.lang;

/**
 * Fixed-size bytes type representing exactly 31 bytes.
 */
public class Bytes31 extends Bytes {
    public static final Bytes31 DEFAULT = new Bytes31(new byte[31]);

    public Bytes31(byte[] value) {
        super(31, value);
    }
}
