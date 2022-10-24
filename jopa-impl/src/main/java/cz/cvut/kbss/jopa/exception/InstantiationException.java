package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Thrown when an instance of class cannot be created.
 */
public class InstantiationException extends OWLPersistenceException {

    public InstantiationException(Throwable cause) {
        super(cause);
    }
}
