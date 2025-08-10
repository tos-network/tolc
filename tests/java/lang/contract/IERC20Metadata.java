package java.lang.contract;

import java.lang.String;
import java.lang.Uint8;

// SPDX-License-Identifier: MIT

public interface IERC20Metadata extends IERC20 {

    /**
     * @dev Returns the name of the token.
     */
    String name();

    /**
     * @dev Returns the symbol of the token.
     */
    String symbol();

    /**
     * @dev Returns the decimal places of the token.
     */
    Uint8 decimals();
}
