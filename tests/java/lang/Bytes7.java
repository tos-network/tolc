package java.lang;

/**
 * Fixed-size bytes type representing exactly 7 bytes.
 */
public class Bytes7 extends Bytes {
    public static final Bytes7 DEFAULT = new Bytes7(new byte[7]);

    public Bytes7(byte[] value) {
        super(7, value);
    }
}
