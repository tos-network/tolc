package java.lang.contract;


import java.lang.String;

import java.lang.Address;
import java.lang.Bool;
import java.lang.Mapping;
import java.lang.Uint256;
import java.lang.Uint8;
import java.lang.Transfer;
import java.lang.Approval;
import java.lang.Paused;
import java.lang.Unpaused;
import java.lang.OwnershipTransferred;
import java.lang.annotation.Payable;
import java.lang.annotation.View;

/**
 * @dev Implementation of the {IERC20} interface.
 *
 * This implementation is agnostic to the way tokens are created. This means
 * that a supply mechanism has to be added in a derived contract using {_mint}.
 *
 * The default value of {decimals} is 18. To change this, you should override
 * this function so it returns a different value.
 *
 * We have followed general OpenZeppelin Contracts guidelines: functions revert
 * instead returning `false` on failure.
 */
public abstract class ERC20 extends Contract {
    private final Mapping<Address, Uint256> balances = new Mapping<Address, Uint256>(Uint256.ZERO);
    private final Mapping<String, Uint256> allowances = new Mapping<String, Uint256>(Uint256.ZERO);
    private Uint256 totalSupply = Uint256.ZERO; 
    private final String name;
    private final String symbol;
    private Bool paused = Bool.FALSE;
    private Address owner;

    /**
     * @dev Sets the values for {name} and {symbol}.
     *
     * All two of these values are immutable: they can only be set once during
     * construction.
     */
    public ERC20(String name_, String symbol_) {
        this.name = name_;
        this.symbol = symbol_;
        this.owner = _msgSender();
    }

    @View
    public String name() {
        return name;
    }

    @View
    public String symbol() {
        return symbol;
    }

    /**
     * @dev Returns the number of decimals used to get its user representation.
     * For example, if `decimals` equals `2`, a balance of `505` tokens should
     * be displayed to a user as `5.05` (`505 / 10 ** 2`).
     *
     * Tokens usually opt for a value of 18, imitating the relationship between
     * Ether and Wei. This is the default value returned by this function, unless
     * it's overridden.
     */
    @View
    public Uint8 decimals() {
        return new Uint8(18);
    }

    /**
     * @dev Returns the address of the current owner.
     */
    @View
    public Address owner() {
        return owner;
    }

    /**
     * @dev Throws if called by any account other than the owner.
     */
    private void onlyOwner() {
        Address caller = _msgSender();
        if (!caller.equals(owner)) {
            revert(new OnlyOwner(caller));
        }
    }

    @View
    public Uint256 totalSupply() {
        return totalSupply;
    }

    @View
    public Uint256 balanceOf(Address account) {
        return balances.get(account);
    }

    @Payable
    public Bool transfer(Address to, Uint256 value) {   
        Address owner = _msgSender();
        _transfer(owner, to, value);
        return Bool.TRUE;
    }

    @View
    public Uint256 allowance(Address owner, Address spender) {  
        return allowances.get(_getAllowanceKey(owner, spender));
    }

    @Payable
    public Bool approve(Address spender, Uint256 value) {   
        Address owner = _msgSender();
        _approve(owner, spender, value);
        return Bool.TRUE;    
    }

    /**
     * @dev See {IERC20-transferFrom}.
     *
     * NOTE: Does not update the allowance if the current allowance
     * is the maximum `uint256`.
     */
    @Payable
    public Bool transferFrom(Address from, Address to, Uint256 value) {
        Address spender = _msgSender();
        _spendAllowance(from, spender, value);
        _transfer(from, to, value);
        return Bool.TRUE;
    }

    /**
     * @dev Generates a composite key for allowances mapping.
     */
    private String _getAllowanceKey(Address owner, Address spender) {
        return owner.toString() + ":" + spender.toString();
    }

    /**
     * @dev Moves a `value` amount of tokens from `from` to `to`.
     *
     * This internal function is equivalent to {transfer}, and can be used to
     * e.g. implement automatic token fees, slashing mechanisms, etc.
     *
     * Emits a {Transfer} event.
     */
    private void _transfer(Address from, Address to, Uint256 value) {
        if (paused.equals(Bool.TRUE)) {
            revert(new ERC20TokenPaused());
        }
        if (Address.ZERO_ADDRESS.equals(from)) {
            revert(new ERC20InvalidSender(from));
        }
        if (Address.ZERO_ADDRESS.equals(to)) {
            revert(new ERC20InvalidReceiver(to));
        }
        if (from.equals(to)) {
            return; // No need to transfer to self
        }
        if (value.isZero()) {
            return; // No need to process zero-value transfers
        }
        _update(from, to, value);
    }

    /**
     * @dev Transfers a `value` amount of tokens from `from` to `to`, or alternatively mints (or burns) if `from`
     * (or `to`) is the zero address. All customizations to transfers, mints, and burns should be done by overriding
     * this function.
     *
     * Emits a {Transfer} event.
     */
    protected void _update(Address from, Address to, Uint256 value) {   
        if (Address.ZERO_ADDRESS.equals(from)) {
            // Minting tokens
            totalSupply = totalSupply.add(value);
        } else {    
            Uint256 fromBalance = balanceOf(from);
            if (fromBalance.compareTo(value) < 0) {
                revert(new ERC20InsufficientBalance(from, fromBalance, value));
            }
            balances.set(from, fromBalance.sub(value));
        }

        if (Address.ZERO_ADDRESS.equals(to)) {
            // Burning tokens
            totalSupply = totalSupply.sub(value);
        } else {
            Uint256 toBalance = balanceOf(to);
            balances.set(to, toBalance.add(value));
        }

        emit(new Transfer(from, to, value));
    }

    /**
     * @dev Creates a `value` amount of tokens and assigns them to `account`.
     *
     * Emits a {Transfer} event with `from` set to the zero address.
     */
    protected void _mint(Address account, Uint256 value) {  
        if (Address.ZERO_ADDRESS.equals(account)) {
            revert(new ERC20InvalidReceiver(account));
        }
        if (value.isZero()) {
            revert(new ERC20InvalidAmount(value));
        }
        _update(Address.ZERO_ADDRESS, account, value);
    }

    /**
     * @dev Creates a `value` amount of tokens and assigns them to `account`.
     * Can only be called by the owner.
     *
     * Emits a {Transfer} event with `from` set to the zero address.
     */
    @Payable
    public void mint(Address account, Uint256 value) {
        onlyOwner();
        _mint(account, value);
    }

    /**
     * @dev Destroys a `value` amount of tokens from `account`.
     *
     * Emits a {Transfer} event with `to` set to the zero address.
     */
    protected void _burn(Address account, Uint256 value) {
        if (Address.ZERO_ADDRESS.equals(account)) {
            revert(new ERC20InvalidSender(account));
        }
        if (value.isZero()) {
            revert(new ERC20InvalidAmount(value));
        }
        _update(account, Address.ZERO_ADDRESS, value);
    }

    /**
     * @dev Destroys a `value` amount of tokens from `account`.
     * Can only be called by the owner.
     *
     * Emits a {Transfer} event with `to` set to the zero address.
     */
    @Payable
    public void burn(Address account, Uint256 value) {
        onlyOwner();
        _burn(account, value);
    }

    protected void _approve(Address owner, Address spender, Uint256 value) {
        _approve(owner, spender, value, true);
    }

    /**
     * @dev Variant of {_approve} with an optional flag to enable or disable the {Approval} event.
     */
    protected void _approve(Address owner, Address spender, Uint256 value, boolean emitEvent) {
        if (Address.ZERO_ADDRESS.equals(owner)) {
            revert(new ERC20InvalidApprover(owner));
        }
        if (Address.ZERO_ADDRESS.equals(spender)) {
            revert(new ERC20InvalidSpender(spender));
        }

        allowances.set(_getAllowanceKey(owner, spender), value);

        if (emitEvent) {
            emit(new Approval(owner, spender, value));
        }
    }

    /**
     * @dev Updates `owner`'s allowance for `spender` based on spent `value`.
     *
     * Does not update the allowance value in case of infinite allowance.
     * Revert if not enough allowance is available.
     */
    protected void _spendAllowance(Address owner, Address spender, Uint256 value) {
        Uint256 currentAllowance = allowance(owner, spender);
        if (currentAllowance.compareTo(Uint256.MAX_VALUE) < 0) {
            if (currentAllowance.compareTo(value) < 0) {
                revert(new ERC20InsufficientAllowance(spender, currentAllowance, value));
            }
            _approve(owner, spender, currentAllowance.sub(value), false);
        }
    }

    /**
     * @dev Atomically increases the allowance granted to `spender` by the caller.
     *
     * This is an alternative to {approve} that can be used as a mitigation for
     * problems described in {IERC20-approve}.
     *
     * Emits an {Approval} event indicating the updated allowance.
     */
    @Payable
    public Bool increaseAllowance(Address spender, Uint256 addedValue) {
        Address owner = _msgSender();
        Uint256 currentAllowance = allowance(owner, spender);
        _approve(owner, spender, currentAllowance.add(addedValue));
        return Bool.TRUE;
    }

    /**
     * @dev Atomically decreases the allowance granted to `spender` by the caller.
     *
     * This is an alternative to {approve} that can be used as a mitigation for
     * problems described in {IERC20-approve}.
     *
     * Emits an {Approval} event indicating the updated allowance.
     */
    @Payable
    public Bool decreaseAllowance(Address spender, Uint256 subtractedValue) {
        Address owner = _msgSender();
        Uint256 currentAllowance = allowance(owner, spender);
        if (currentAllowance.compareTo(subtractedValue) < 0) {
            revert(new ERC20InsufficientAllowance(spender, currentAllowance, subtractedValue));
        }
        _approve(owner, spender, currentAllowance.sub(subtractedValue));
        return Bool.TRUE;
    }

    /**
     * @dev Returns true if the contract is paused, and false otherwise.
     */
    @View
    public Bool paused() {
        return paused;
    }

    /**
     * @dev Triggers stopped state.
     *
     * Requirements:
     *
     * - The contract must not be paused.
     * - Only the owner can call this function.
     */
    @Payable
    public void pause() {
        onlyOwner();
        paused = Bool.TRUE;
        emit(new Paused(_msgSender()));
    }

    /**
     * @dev Returns to normal state.
     *
     * Requirements:
     *
     * - The contract must be paused.
     * - Only the owner can call this function.
     */
    @Payable
    public void unpause() {
        onlyOwner();
        paused = Bool.FALSE;
        emit(new Unpaused(_msgSender()));
    }

    /**
     * @dev Transfers ownership of the contract to a new account (`newOwner`).
     * Can only be called by the current owner.
     */
    @Payable
    public void transferOwnership(Address newOwner) {
        onlyOwner();
        if (Address.ZERO_ADDRESS.equals(newOwner)) {
            revert(new ERC20InvalidReceiver(newOwner));
        }
        Address oldOwner = owner;
        owner = newOwner;
        emit(new OwnershipTransferred(oldOwner, newOwner));
    }

    /**
     * @dev Leaves the contract without owner. It will not be possible to call
     * `onlyOwner` functions anymore. Can only be called by the current owner.
     *
     * NOTE: Renouncing ownership will leave the contract without an owner,
     * thereby removing any functionality that is only available to the owner.
     */
    @Payable
    public void renounceOwnership() {
        onlyOwner();
        Address oldOwner = owner;
        owner = Address.ZERO_ADDRESS;
        emit(new OwnershipTransferred(oldOwner, Address.ZERO_ADDRESS));
    }
}
