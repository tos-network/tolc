package java.lang.contract;

import java.lang.Address;
import java.lang.Bool;
import java.lang.Uint256;
import java.lang.annotation.Payable;
import java.lang.annotation.View;

// SPDX-License-Identifier: MIT

/**
 * @dev Interface of the ERC-20 standard as defined in the ERC.
 */
public interface IERC20 {

    /**
     * @dev Returns the value of tokens in existence.
     */
    @View
    Uint256 totalSupply();

    /**
     * @dev Returns the value of tokens owned by `account`.
     */
    @View
    Uint256 balanceOf(Address account);

    /**
     * @dev Moves a `value` amount of tokens from the caller's account to `to`.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    @Payable    
    Bool transfer(Address to, Uint256 value);

    /**
     * @dev Returns the remaining number of tokens that `spender` will be
     * allowed to spend on behalf of `owner` through {transferFrom}. This is
     * zero by default.
     *
     * This value changes when {approve} or {transferFrom} are called.
     */
    @View
    Uint256 allowance(Address owner, Address spender);

    /**
     * @dev Sets a `value` amount of tokens as the allowance of `spender` over the
     * caller's tokens.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * IMPORTANT: Beware that changing an allowance with this method brings the risk
     * that someone may use both the old and the new allowance by unfortunate
     * transaction ordering. One possible solution to mitigate this race
     * condition is to first reduce the spender's allowance to 0 and set the
     * desired value afterwards:
     * https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729
     *
     * Emits an {Approval} event.
     */
    @Payable
    Bool approve(Address spender, Uint256 value);

    /**
     * @dev Moves a `value` amount of tokens from `from` to `to` using the
     * allowance mechanism. `value` is then deducted from the caller's
     * allowance.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    @Payable    
    Bool transferFrom(Address from, Address to, Uint256 value);
}
