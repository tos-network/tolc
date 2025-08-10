package java.lang;

/**
 * Fixed-size bytes type representing exactly 17 bytes.
 */
public class Bytes17 extends Bytes {
    public static final Bytes17 DEFAULT = new Bytes17(new byte[17]);

    public Bytes17(byte[] value) {
        super(17, value);
    }
}
