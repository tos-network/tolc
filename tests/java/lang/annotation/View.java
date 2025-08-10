package java.lang.annotation;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface View {
    // Methods marked with @View only read the blockchain state
    // Similar to Solidity's view
}