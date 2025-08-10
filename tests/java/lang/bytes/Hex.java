package java.lang.bytes;

import java.math.BigInteger;
import java.util.Arrays;

/**
 * Message codec functions.
 *
 * <p>Implementation as per https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding
 */
public final class Hex {

    private static final String HEX_PREFIX = "0x";
    private static final char[] HEX_CHAR_MAP = "0123456789abcdef".toCharArray();

    private Hex() {} 

    public static String encodeQuantity(BigInteger value) {
        if (value.signum() != -1) {
            return HEX_PREFIX + value.toString(16);
        } else {
            return HEX_PREFIX;
        }
    }

    public static BigInteger decodeQuantity(String value) {
        if (isLongValue(value)) {
            return BigInteger.valueOf(Long.parseLong(value));
        }

        if (!isValidHexQuantity(value)) {
            return BigInteger.ZERO;
        }

        try {
            return parsePaddedNumberHex(value);
        } catch (NumberFormatException e) {
            return BigInteger.ZERO;
        }
    }

    public static BigInteger parsePaddedNumberHex(String value) {
        // Simplified implementation without regex - remove leading zeros manually
        String cleanValue = cleanHexPrefix(value);
        int start = 0;
        while (start < cleanValue.length() - 1 && cleanValue.charAt(start) == '0') {
            start++;
        }
        String numWithoutLeadingZeros = cleanValue.substring(start);
        return new BigInteger(numWithoutLeadingZeros, 16);
    }

    private static boolean isLongValue(String value) {
        try {
            Long.parseLong(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    protected static boolean isValidHexQuantity(String value) {
        if (value == null) {
            return false;
        }

        if (value.length() < 3) {
            return false;
        }

        if (!value.startsWith(HEX_PREFIX)) {
            return false;
        }

        // Simplified implementation without regex - check hex format manually
        String hexPart = value.substring(2); // Remove "0x" prefix
        for (int i = 0; i < hexPart.length(); i++) {
            char c = hexPart.charAt(i);
            if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
                return false;
            }
        }
        return true;
    }

    public static String cleanHexPrefix(String input) {
        if (containsHexPrefix(input)) {
            return input.substring(2);
        } else {
            return input;
        }
    }

    public static String prependHexPrefix(String input) {
        if (!containsHexPrefix(input)) {
            return HEX_PREFIX + input;
        } else {
            return input;
        }
    }

    public static boolean containsHexPrefix(String input) {
        return !isEmpty(input)
                && input.length() > 1
                && input.charAt(0) == '0'
                && input.charAt(1) == 'x';
    }

    public static BigInteger toBigInt(byte[] value, int offset, int length) {
        return toBigInt((Arrays.copyOfRange(value, offset, offset + length)));
    }

    public static BigInteger toBigInt(byte[] value) {
        return new BigInteger(1, value);
    }

    public static BigInteger toBigInt(String hexValue) {
        String cleanValue = cleanHexPrefix(hexValue);
        return toBigIntNoPrefix(cleanValue);
    }

    public static BigInteger toBigIntNoPrefix(String hexValue) {
        return new BigInteger(hexValue, 16);
    }

    public static String toHexStringWithPrefix(BigInteger value) {
        return HEX_PREFIX + value.toString(16);
    }

    public static String toHexStringNoPrefix(BigInteger value) {
        return value.toString(16);
    }

    public static String toHexStringNoPrefix(byte[] input) {
        return toHexString(input, 0, input.length, false);
    }

    public static String toHexStringWithPrefixZeroPadded(BigInteger value, int size) {
        return toHexStringZeroPadded(value, size, true);
    }

    /**
     * @deprecated use {@link #toHexStringNoPrefix(BigInteger value)} instead, more details <a
     *     href="https://github.com/web3j/web3j/pull/1679">here</a>
     */
    @Deprecated
    public static String toHexStringWithPrefixSafe(BigInteger value) {
        String result = toHexStringNoPrefix(value);
        if (result.length() < 2) {
            result = zeros(1) + result;
        }
        return HEX_PREFIX + result;
    }

    public static String toHexStringNoPrefixZeroPadded(BigInteger value, int size) {
        return toHexStringZeroPadded(value, size, false);
    }

    private static String toHexStringZeroPadded(BigInteger value, int size, boolean withPrefix) {
        String result = toHexStringNoPrefix(value);

        int length = result.length();
        if (length > size) {
            throw new UnsupportedOperationException(
                    "Value " + result + "is larger then length " + size);
        } else if (value.signum() < 0) {
            throw new UnsupportedOperationException("Value cannot be negative");
        }

        if (length < size) {
            result = zeros(size - length) + result;
        }

        if (withPrefix) {
            return HEX_PREFIX + result;
        } else {
            return result;
        }
    }

    public static byte[] toBytesPadded(BigInteger value, int length) {
        byte[] result = new byte[length];
        byte[] bytes = value.toByteArray();

        int bytesLength;
        int srcOffset;
        if (bytes[0] == 0) {
            bytesLength = bytes.length - 1;
            srcOffset = 1;
        } else {
            bytesLength = bytes.length;
            srcOffset = 0;
        }

        if (bytesLength > length) {
            throw new RuntimeException("Input is too large to put in byte array of size " + length);
        }

        int destOffset = length - bytesLength;
        System.arraycopy(bytes, srcOffset, result, destOffset, bytesLength);
        return result;
    }

    public static byte[] hexStringToByteArray(String input) {
        String cleanInput = cleanHexPrefix(input);

        int len = cleanInput.length();

        if (len == 0) {
            return new byte[] {};
        }

        byte[] data;
        int startIdx;
        if (len % 2 != 0) {
            data = new byte[(len / 2) + 1];
            data[0] = (byte) Character.digit(cleanInput.charAt(0), 16);
            startIdx = 1;
        } else {
            data = new byte[len / 2];
            startIdx = 0;
        }

        for (int i = startIdx; i < len; i += 2) {
            data[(i + 1) / 2] =
                    (byte)
                            ((Character.digit(cleanInput.charAt(i), 16) << 4)
                                    + Character.digit(cleanInput.charAt(i + 1), 16));
        }
        return data;
    }

    public static String toHexString(byte[] input, int offset, int length, boolean withPrefix) {
        final String output = new String(toHexCharArray(input, offset, length));
        return withPrefix ? new StringBuilder(HEX_PREFIX).append(output).toString() : output;
    }

    private static char[] toHexCharArray(byte[] input, int offset, int length) {
        final char[] output = new char[length << 1];
        for (int i = offset, j = 0; i < length + offset; i++, j++) {
            final int v = input[i] & 0xFF;
            output[j++] = HEX_CHAR_MAP[v >>> 4];
            output[j] = HEX_CHAR_MAP[v & 0x0F];
        }
        return output;
    }

    public static String toHexString(byte[] input) {
        return toHexString(input, 0, input.length, true);
    }

    public static byte asByte(int m, int n) {
        return (byte) ((m << 4) | n);
    }

    // The following method is disabled because BigDecimal is not available in our rt.jar
    /*
    public static boolean isIntegerValue(BigDecimal value) {
        // Implementation would go here
        return false;
    }
    */

    public static String removeDoubleQuotes(String string) {
        return string != null ? string.replace("\"", "") : null;
    }

        /**
     * Returns true if the string is empty, otherwise false.
     *
     * @param s String value
     * @return is given string is Empty or not
     */
    private static boolean isEmpty(String s) {
        return s == null || s.isEmpty();
    }

    private static String zeros(int n) {
        return repeat('0', n);
    }
    private static String repeat(char value, int n) {
        return new String(new char[n]).replace("\0", String.valueOf(value));
    }

}
