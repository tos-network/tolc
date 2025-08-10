package java.lang;

import java.util.Arrays;
import java.lang.bytes.Type;

/** Binary sequence of bytes. */
public abstract class BytesType implements Type<byte[]> {

    private byte[] value;
    private String type;

    public BytesType(byte[] src, String type) {
        this.value = src;
        this.type = type;
    }

    @Override
    public int bytes32PaddedLength() {
        if (value.length < MAX_BYTE_LENGTH) {
            return MAX_BYTE_LENGTH;
        } else if (value.length % MAX_BYTE_LENGTH == 0) {
            return value.length;
        }
        return (value.length / MAX_BYTE_LENGTH + 1) * MAX_BYTE_LENGTH;
    }

    @Override
    public byte[] getValue() {
        return value;
    }

    @Override
    public String getTypeAsString() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        BytesType bytesType = (BytesType) o;

        if (!Arrays.equals(value, bytesType.value)) {
            return false;
        }
        return type.equals(bytesType.type);
    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(value);
        result = 31 * result + type.hashCode();
        return result;
    }
}
