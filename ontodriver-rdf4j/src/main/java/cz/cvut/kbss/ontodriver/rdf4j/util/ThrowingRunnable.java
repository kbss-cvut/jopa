package cz.cvut.kbss.ontodriver.rdf4j.util;

/**
 * A {@link Runnable} that throws a checked exception.
 *
 * @param <E> the type of exception
 */
public interface ThrowingRunnable<E extends Exception> {

    void run() throws E;
}
