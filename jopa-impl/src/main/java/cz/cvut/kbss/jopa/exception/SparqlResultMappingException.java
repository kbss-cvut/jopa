package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Represents a problem with a {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} declaration or its runtime usage to map query result to target value(s).
 */
public class SparqlResultMappingException extends OWLPersistenceException {

    public SparqlResultMappingException(Throwable cause) {
        super(cause);
    }

    public SparqlResultMappingException(String message, Throwable cause) {
        super(message, cause);
    }
}
