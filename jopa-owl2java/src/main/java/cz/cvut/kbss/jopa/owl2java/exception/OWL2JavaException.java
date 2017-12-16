package cz.cvut.kbss.jopa.owl2java.exception;

/**
 * Generic exception for a failed transformation/vocabulary generation.
 */
public class OWL2JavaException extends RuntimeException {

    public OWL2JavaException(String message) {
        super(message);
    }

    public OWL2JavaException(String message, Throwable cause) {
        super(message, cause);
    }
}
