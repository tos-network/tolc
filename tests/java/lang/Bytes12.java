package java.lang;

/**
 * Fixed-size bytes type representing exactly 12 bytes.
 */
public class Bytes12 extends Bytes {
    public static final Bytes12 DEFAULT = new Bytes12(new byte[12]);

    public Bytes12(byte[] value) {
        super(12, value);
    }
}
