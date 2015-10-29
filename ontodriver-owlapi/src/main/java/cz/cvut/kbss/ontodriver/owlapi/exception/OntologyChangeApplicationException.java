package cz.cvut.kbss.ontodriver.owlapi.exception;

/**
 * Thrown when the driver is unable to apply an ontology change.
 */
public class OntologyChangeApplicationException extends RuntimeException {

    public OntologyChangeApplicationException() {
    }

    public OntologyChangeApplicationException(String message) {
        super(message);
    }

    public OntologyChangeApplicationException(String message, Throwable cause) {
        super(message, cause);
    }

    public OntologyChangeApplicationException(Throwable cause) {
        super(cause);
    }
}
