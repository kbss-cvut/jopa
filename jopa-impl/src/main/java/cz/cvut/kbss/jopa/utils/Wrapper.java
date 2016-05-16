package cz.cvut.kbss.jopa.utils;

public interface Wrapper {

    /**
     * Unwraps implementation of the specified class.
     *
     * @param cls The class of the object to be returned
     * @return An instance of the specified class
     * @throws cz.cvut.kbss.jopa.exceptions.OWLPersistenceException If the provider does not support the class
     */
    <T> T unwrap(Class<T> cls);
}
