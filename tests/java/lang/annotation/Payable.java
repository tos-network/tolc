package java.lang.annotation;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Payable {
    // Methods marked with @Payable can receive Ether
    // Similar to Solidity's payable
}