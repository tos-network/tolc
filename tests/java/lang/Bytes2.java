package java.lang;

/**
 * Fixed-size bytes type representing exactly 2 bytes.
 */
public class Bytes2 extends Bytes {
    public static final Bytes2 DEFAULT = new Bytes2(new byte[2]);

    public Bytes2(byte[] value) {
        super(2, value);
    }
}
