package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Address;
import java.lang.Uint256;

/**
 * Thrown when the sender's balance is insufficient for a transfer.
 */
public class ERC20InsufficientBalance extends RuntimeException {
    public ERC20InsufficientBalance(Address sender, Uint256 balance, Uint256 needed) {
        super("ERC20: insufficient balance. Sender: " + sender + ", Balance: " + balance + ", Needed: " + needed);
    }
}
