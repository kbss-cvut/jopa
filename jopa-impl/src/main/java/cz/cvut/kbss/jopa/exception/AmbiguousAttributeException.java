package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

public class AmbiguousAttributeException extends OWLPersistenceException {
    public AmbiguousAttributeException(String message, Throwable cause) {
        super(message, cause);
    }

    public AmbiguousAttributeException(String message) {
        super(message);
    }

    public AmbiguousAttributeException(Throwable cause) {
        super(cause);
    }
}
