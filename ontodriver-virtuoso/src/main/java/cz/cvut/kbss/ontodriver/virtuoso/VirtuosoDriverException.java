package cz.cvut.kbss.ontodriver.virtuoso;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

public class VirtuosoDriverException extends OntoDriverException {

    public VirtuosoDriverException(String message) {
        super(message);
    }

    public VirtuosoDriverException(String message, Throwable cause) {
        super(message, cause);
    }
}
