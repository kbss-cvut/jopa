package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Thrown when it is not possible to execute a type transformation.
 */
public class UnsupportedTypeTransformation extends OWLPersistenceException {

    public UnsupportedTypeTransformation(String message) {
        super(message);
    }
}
