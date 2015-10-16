package cz.cvut.kbss.ontodriver.owlapi.exception;

/**
 * Signals that the driver requested services of a reasoner but it was not available.
 */
public class ReasonerNotAvailableException extends RuntimeException {

    public ReasonerNotAvailableException() {
    }

    public ReasonerNotAvailableException(String message) {
        super(message);
    }

    public ReasonerNotAvailableException(String message, Throwable cause) {
        super(message, cause);
    }

    public ReasonerNotAvailableException(Throwable cause) {
        super(cause);
    }
}
