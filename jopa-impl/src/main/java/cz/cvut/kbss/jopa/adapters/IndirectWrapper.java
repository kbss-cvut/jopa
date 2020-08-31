package cz.cvut.kbss.jopa.adapters;

/**
 * Wraps a target object in an indirect proxy which is able to intercept method calls and perform additional processing
 * (usually persistence context notification).
 */
public interface IndirectWrapper {

    /**
     * Retrieves the wrapped object.
     *
     * @return The wrapped object
     */
    Object unwrap();
}
