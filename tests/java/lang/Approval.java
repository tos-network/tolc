package java.lang;

/**
 * @dev Emitted when the allowance of a `spender` for an `owner` is set by
 * a call to {approve}. `value` is the new allowance.
 */
 public  class Approval extends EventLog {
    public final Address indexed_owner;
    public final Address indexed_spender;
    public final Uint256 value;

    public Approval(Address owner, Address spender, Uint256 value) {
        super("Approval event:  " + owner + " approved  " + spender + " :  " + value);
        this.indexed_owner = owner;
        this.indexed_spender = spender;
        this.value = value;
    }
}