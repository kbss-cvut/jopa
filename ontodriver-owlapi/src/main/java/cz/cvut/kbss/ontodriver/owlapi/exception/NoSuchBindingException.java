package cz.cvut.kbss.ontodriver.owlapi.exception;

/**
 * Thrown when trying to get a query result binding that does not exist.
 */
public class NoSuchBindingException extends RuntimeException {

    public NoSuchBindingException() {
    }

    public NoSuchBindingException(String message) {
        super(message);
    }

    public NoSuchBindingException(String message, Throwable cause) {
        super(message, cause);
    }

    public NoSuchBindingException(Throwable cause) {
        super(cause);
    }
}
