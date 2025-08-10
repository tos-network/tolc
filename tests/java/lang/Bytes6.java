package java.lang;

/**
 * Fixed-size bytes type representing exactly 6 bytes.
 */
public class Bytes6 extends Bytes {
    public static final Bytes6 DEFAULT = new Bytes6(new byte[6]);

    public Bytes6(byte[] value) {
        super(6, value);
    }
}
