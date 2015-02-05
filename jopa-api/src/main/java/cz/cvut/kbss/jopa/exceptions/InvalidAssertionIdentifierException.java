package cz.cvut.kbss.jopa.exceptions;

/**
 * This exception is thrown when assertion identifier is not a valid URI.
 *
 * @author ledvima1
 */
public class InvalidAssertionIdentifierException extends RuntimeException {

    public InvalidAssertionIdentifierException() {
    }

    public InvalidAssertionIdentifierException(String message) {
        super(message);
    }

    public InvalidAssertionIdentifierException(String message, Throwable cause) {
        super(message, cause);
    }

    public InvalidAssertionIdentifierException(Throwable cause) {
        super(cause);
    }
}
