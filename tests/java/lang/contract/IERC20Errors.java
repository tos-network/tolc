package java.lang.contract;

/**
 * Standard ERC20 Errors Interface.
 * This interface serves as a marker for ERC20 error types.
 * Individual error classes are now defined as separate files:
 * - ERC20InsufficientBalance
 * - ERC20InvalidSender
 * - ERC20InvalidReceiver
 * - ERC20InsufficientAllowance
 * - ERC20InvalidApprover
 * - ERC20InvalidSpender
 * - ERC20TokenPaused
 * - ERC20InvalidAmount
 */
public interface IERC20Errors {
    // This interface now serves as a marker for ERC20 error types
    // Individual error classes are defined in separate files for better organization
}