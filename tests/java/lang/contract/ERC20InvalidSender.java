package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Address;

/**
 * Thrown when the sender address is invalid (e.g., zero address).
 */
public class ERC20InvalidSender extends RuntimeException {
    public ERC20InvalidSender(Address sender) {
        super("ERC20: invalid sender. Sender: " + sender);
    }
}
