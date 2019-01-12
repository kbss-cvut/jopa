package cz.cvut.kbss.ontodriver.exception;

/**
 * Parent exception for OntoDriver-specific unchecked exceptions.
 */
public class OntoDriverRuntimeException extends RuntimeException {

    public OntoDriverRuntimeException() {
    }

    public OntoDriverRuntimeException(String message) {
        super(message);
    }

    public OntoDriverRuntimeException(String message, Throwable cause) {
        super(message, cause);
    }

    public OntoDriverRuntimeException(Throwable cause) {
        super(cause);
    }
}
