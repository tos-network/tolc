package java.lang.annotation;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface External {
    // Methods marked with @External can be called from outside the contract
    // Similar to Solidity's external visibility modifier
}
