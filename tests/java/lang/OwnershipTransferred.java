package java.lang;

/**
 * @dev Emitted when ownership of a contract is transferred.
 */
public class OwnershipTransferred extends EventLog {
    public final Address indexed_previousOwner;
    public final Address indexed_newOwner;

    public OwnershipTransferred(Address previousOwner, Address newOwner) {
        super("OwnershipTransferred event: " + previousOwner + " -> " + newOwner);
        this.indexed_previousOwner = previousOwner;
        this.indexed_newOwner = newOwner;
    }
}
