package java.lang;

/**
 * Fixed-size bytes type representing exactly 22 bytes.
 */
public class Bytes22 extends Bytes {
    public static final Bytes22 DEFAULT = new Bytes22(new byte[22]);

    public Bytes22(byte[] value) {
        super(22, value);
    }
}
