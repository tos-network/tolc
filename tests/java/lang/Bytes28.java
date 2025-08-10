package java.lang;

/**
 * Fixed-size bytes type representing exactly 28 bytes.
 */
public class Bytes28 extends Bytes {
    public static final Bytes28 DEFAULT = new Bytes28(new byte[28]);

    public Bytes28(byte[] value) {
        super(28, value);
    }
}
