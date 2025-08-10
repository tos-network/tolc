package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Address;

/**
 * Thrown when the approver address is invalid (e.g., zero address).
 */
public class ERC20InvalidApprover extends RuntimeException {
    public ERC20InvalidApprover(Address approver) {
        super("ERC20: invalid approver. Approver: " + approver);
    }
}
