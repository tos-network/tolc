package java.lang;

/**
 * A task that returns a result and may throw an exception.
 * Implementors define a single method with no arguments.
 *
 * @param <V> the result type of method {@code call}
 */
public interface Callable<V> {
    /**
     * Computes a result, or throws an exception if unable to do so.
     *
     * @return computed result
     * @throws Exception if unable to compute a result
     */
    V call() throws Exception;
}
