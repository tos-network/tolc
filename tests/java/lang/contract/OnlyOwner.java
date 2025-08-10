package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Address;

/**
 * @dev Error thrown when a function is called by an address that is not the owner.
 */
public class OnlyOwner extends RuntimeException {
    public OnlyOwner(Address caller) {
        super("OnlyOwner: caller is not the owner. Caller: " + caller);
    }
}
