package cz.cvut.kbss.ontodriver.jena.exception;

/**
 * Thrown when Jena reasoner cannot be initialized.
 */
public class ReasonerInitializationException extends RuntimeException {

    public ReasonerInitializationException(String message) {
        super(message);
    }

    public ReasonerInitializationException(String message, Throwable cause) {
        super(message, cause);
    }
}
