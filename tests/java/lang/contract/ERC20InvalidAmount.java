package java.lang.contract;

import java.lang.RuntimeException;
import java.lang.Uint256;

/**
 * Thrown when the amount is invalid (e.g., zero amount for mint/burn).
 */
public class ERC20InvalidAmount extends RuntimeException {
    public ERC20InvalidAmount(Uint256 amount) {
        super("ERC20: invalid amount. Amount: " + amount);
    }
}
