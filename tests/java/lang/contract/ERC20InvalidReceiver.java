package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Address;

/**
 * Thrown when the receiver address is invalid (e.g., zero address).
 */
public class ERC20InvalidReceiver extends RuntimeException {
    public ERC20InvalidReceiver(Address receiver) {
        super("ERC20: invalid receiver. Receiver: " + receiver);
    }
}
