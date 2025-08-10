package java.lang;

// SPDX-License-Identifier: MIT

/**
 * Provides information about the current execution context, including the
 * sender of the transaction and its data.
 *
 * This contract is only required for intermediate, library-like contracts.
 */
public abstract class Context {

    Address sender;

    public Context() {
        this.sender = new Address(new Uint160(0));
    }

    public Address _msgSender() {
        return new Address(new Uint160(0));
    }

    // Reverts the transaction with the given exception.
    public void revert(RuntimeException re) {
        throw new RevertException(re.getMessage()); 
    }

    // Emits an event.  
    public void emit(EventLog el) {
        System.out.println(el.getMessage());
    }
}
