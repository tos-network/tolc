package java.lang;

/**
 * Represents a message with a sender and data.
 */
public class Message {
    protected final Address sender;  // Sender of the message
    private final Uint256 value;
    protected final byte[] data;     // Data contained in the message
    protected final Uint64 asset;
    protected final byte[] sig;
    protected final Uint256 gas;


    /**
     * Constructs a new Message with the specified sender and data.
     *
     */
    public Message() {
        this.sender = new Address(new Uint160(0));    
        this.value = new Uint256(0);
        this.data = new byte[0];
        this.asset = new Uint64(0);
        this.sig = new byte[0];
        this.gas = new Uint256(0);
    }

    /**
     * Returns the sender of the message.
     *
     * @return The sender of the message.
     */
    public final Address getSender() {
        return sender;
    }

    public final Uint256 getValue() {
        return value;
    }
    
    /**
     * Returns the data contained in the message.
     *
     * @return The data contained in the message.
     */
    public byte[] getData() {
        return data;
    }

    public final Uint64 getAsset() {
        return asset;
    }

    public final byte[] getSig() {
        return sig;
    }

    public final Uint256 getGas() {
        return gas;
    }

    /**
     * Returns a string representation of the message.
     *
     * @return A string representation of the message.
     */
    @Override
    public String toString() {
        return "Message{" +
                "sender=" + sender +
                ", value=" + value +
                ", data=" + java.util.Arrays.toString(data) +
                ", asset=" + asset +
                ", sig=" + java.util.Arrays.toString(sig) +
                ", gas=" + gas +
                '}';
    }

    /**
     * Load the native library when the class is loaded.
     */
    static {
        System.loadLibrary("message"); // Load libmessage.so/message.dll
    }

}