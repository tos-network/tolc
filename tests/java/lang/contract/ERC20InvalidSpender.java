package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Address;

/**
 * Thrown when the spender address is invalid (e.g., zero address).
 */
public class ERC20InvalidSpender extends RuntimeException {
    public ERC20InvalidSpender(Address spender) {
        super("ERC20: invalid spender. Spender: " + spender);
    }
}
