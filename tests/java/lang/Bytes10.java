package java.lang;

/**
 * Fixed-size bytes type representing exactly 10 bytes.
 */
public class Bytes10 extends Bytes {
    public static final Bytes10 DEFAULT = new Bytes10(new byte[10]);

    public Bytes10(byte[] value) {
        super(10, value);
    }
}
