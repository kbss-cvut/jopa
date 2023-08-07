package cz.cvut.kbss.ontodriver.stardog.exception;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

public class StardogDriverException extends Rdf4jDriverException {

    public StardogDriverException(String message) {
        super(message);
    }

    public StardogDriverException(Throwable cause) {
        super(cause);
    }
}
