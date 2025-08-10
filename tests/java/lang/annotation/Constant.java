package java.lang.annotation;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface Constant {
    // Fields marked with @Constant are compile-time constants
    // Similar to Solidity's constant modifier
}
