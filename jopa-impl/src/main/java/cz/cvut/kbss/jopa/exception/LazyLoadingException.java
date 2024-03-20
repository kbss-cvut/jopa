package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates an issue with lazy loading.
 * <p>
 * Typically, this would be an attempt to trigger lazy loading on a proxy that is not attached to any active persistence
 * context.
 */
public class LazyLoadingException extends OWLPersistenceException {

    public LazyLoadingException(String message) {
        super(message);
    }
}
