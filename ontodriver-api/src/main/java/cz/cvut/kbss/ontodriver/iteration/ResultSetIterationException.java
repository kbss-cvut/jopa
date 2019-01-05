package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.exception.OntoDriverRuntimeException;

/**
 * Indicates that an error occurred during iteration over a query result set.
 */
public class ResultSetIterationException extends OntoDriverRuntimeException {

    public ResultSetIterationException(Throwable cause) {
        super(cause);
    }
}
