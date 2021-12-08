package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates that an unexpected datatype has been provided for a value.
 */
public class IncompatibleDatatypeException extends OWLPersistenceException {

    public IncompatibleDatatypeException(String message) {
        super(message);
    }
}
