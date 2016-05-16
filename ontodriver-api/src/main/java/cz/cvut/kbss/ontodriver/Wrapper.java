package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Interface for retrieving concrete implementations of the OntoDriver API and/or classes the implementations use (e.g.
 * classes of the underlying storage framework).
 */
public interface Wrapper {

    /**
     * Returns an object that implements the given interface to allow access to non-standard methods, or standard
     * methods not exposed by the proxy. If the receiver implements the interface then the result is the receiver or a
     * proxy for the receiver. If the receiver is a wrapper and the wrapped object implements the interface then the
     * result is the wrapped object or a proxy for the wrapped object. Otherwise return the the result of calling unwrap
     * recursively on the wrapped object or a proxy for that result. If the receiver is not a wrapper and does not
     * implement the interface, then an {@link OntoDriverException} is thrown.
     *
     * @param cls The type of the required result
     * @param <T> The type of the class modelled by this Class object
     * @return An object implementing the interface
     * @throws OntoDriverException If no matching object is found
     */
    <T> T unwrap(Class<T> cls) throws OntoDriverException;
}
