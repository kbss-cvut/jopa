package cz.cvut.kbss.ontodriver.rdf4j.util;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

import java.util.function.Function;

/**
 * A {@link Function} that may throw a checked exception.
 *
 * @param <T> Argument type
 * @param <R> Result type
 */
public interface ThrowingFunction<T, R> {

    /**
     * Apply the function to the specified argument.
     *
     * @param t Argument
     * @return Result
     * @throws Rdf4jDriverException If function evaluation throws an exception
     */
    R apply(T t) throws Rdf4jDriverException;
}
