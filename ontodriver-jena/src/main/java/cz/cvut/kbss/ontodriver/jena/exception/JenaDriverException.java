package cz.cvut.kbss.ontodriver.jena.exception;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Generic exception for the Jena driver.
 */
public class JenaDriverException extends OntoDriverException {

    public JenaDriverException(String message) {
        super(message);
    }

    public JenaDriverException(Throwable cause) {
        super(cause);
    }

    public JenaDriverException(String message, Throwable cause) {
        super(message, cause);
    }
}
