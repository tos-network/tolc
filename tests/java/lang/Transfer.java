package java.lang;

/**
    * @dev Emitted when `value` tokens are moved from one account (`from`) to
    * another (`to`).
    *
    * Note that `value` may be zero.
    */
public class Transfer extends EventLog {
    public final Address indexed_from;
    public final Address indexed_to;
    public final Uint256 value;

    public Transfer(Address from, Address to, Uint256 value) {
        super("Transfer event:  " + from + " ->  " + to + " :  " + value);
        this.indexed_from = from;
        this.indexed_to = to;
        this.value = value;
    }
}