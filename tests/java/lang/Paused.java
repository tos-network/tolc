package java.lang;

/**
 * @dev Emitted when the contract is paused.
 */
public class Paused extends EventLog {
    public final Address indexed_account;

    public Paused(Address account) {
        super("Paused event: contract paused by " + account);
        this.indexed_account = account;
    }
}
