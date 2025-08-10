package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Address;
import java.lang.Uint256;

/**
 * Thrown when the spender's allowance is insufficient for a transfer.
 */
public class ERC20InsufficientAllowance extends RuntimeException {
    public ERC20InsufficientAllowance(Address spender, Uint256 allowance, Uint256 needed) {
        super("ERC20: insufficient allowance. Spender: " + spender + ", Allowance: " + allowance + ", Needed: " + needed);
    }
}
