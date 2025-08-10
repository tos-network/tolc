package java.lang;

/**
 * @dev Emitted when the contract is unpaused.
 */
public class Unpaused extends EventLog {
    public final Address indexed_account;

    public Unpaused(Address account) {
        super("Unpaused event: contract unpaused by " + account);
        this.indexed_account = account;
    }
}
