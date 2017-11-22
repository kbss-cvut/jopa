package cz.cvut.kbss.jopa.utils;

/**
 * Represents an operation that accepts a single input argument and returns no result. Unlike most other functional
 * interfaces, {@code ThrowingConsumer} is expected to operate via side-effects.
 * <p>
 * This is a functional interface whose functional method is {@link #accept(Object)}.
 * <p>
 * This version allows the accept method to throw checked exceptions.
 *
 * @param <T>     the type of the input to the operation
 * @param <E></E> The type of the checked exception thrown by the consumer code
 */
@FunctionalInterface
public interface ThrowingConsumer<T, E extends Exception> {

    void accept(T t) throws E;
}
