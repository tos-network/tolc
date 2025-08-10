package java.lang.annotation;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Pure {
    // Methods marked with @Pure don't read or modify blockchain state
    // Similar to Solidity's pure
}