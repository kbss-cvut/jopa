package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Thrown when an entity is missing an identifier and it cannot be generated.
 */
public class IdentifierNotSetException extends OWLPersistenceException {

    public IdentifierNotSetException(String message) {
        super(message);
    }
}
