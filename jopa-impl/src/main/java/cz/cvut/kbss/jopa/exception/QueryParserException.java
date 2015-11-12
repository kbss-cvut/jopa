package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Exception thrown when an error occurs when parsing a query.
 */
public class QueryParserException extends OWLPersistenceException {

    public QueryParserException() {
    }

    public QueryParserException(String message, Throwable cause) {
        super(message, cause);
    }

    public QueryParserException(String message) {
        super(message);
    }

    public QueryParserException(Throwable cause) {
        super(cause);
    }
}
