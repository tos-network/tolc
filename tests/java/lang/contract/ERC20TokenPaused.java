package java.lang.contract;

import java.lang.RuntimeException;

/**
 * Thrown when the token is paused and transfers are not allowed.
 */
public class ERC20TokenPaused extends RuntimeException {
    public ERC20TokenPaused() {
        super("ERC20: token transfer while paused");
    }
}
