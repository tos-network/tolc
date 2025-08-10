package java.lang.annotation;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Internal {
    // Methods marked with @Internal can only be called from within the contract and its derived contracts
    // Similar to Solidity's internal visibility modifier
}
