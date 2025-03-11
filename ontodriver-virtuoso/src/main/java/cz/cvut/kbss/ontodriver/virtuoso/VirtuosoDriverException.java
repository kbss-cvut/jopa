package cz.cvut.kbss.ontodriver.virtuoso;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

public class VirtuosoDriverException extends OntoDriverException {

    public VirtuosoDriverException(Throwable cause) {
        super(cause);
    }

    public VirtuosoDriverException(String message) {
        super(message);
    }
}
