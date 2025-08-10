package java.lang.annotation;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Virtual {
    // Methods marked with @Virtual can be overridden in derived contracts
    // Similar to Solidity's virtual modifier
}
