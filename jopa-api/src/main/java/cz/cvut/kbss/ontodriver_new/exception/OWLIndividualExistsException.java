package cz.cvut.kbss.ontodriver_new.exception;

/**
 * Thrown when trying to insert an individual into the ontology, but it already exists there.
 */
public class OWLIndividualExistsException extends RuntimeException {

    public OWLIndividualExistsException() {
    }

    public OWLIndividualExistsException(String message) {
        super(message);
    }

    public OWLIndividualExistsException(String message, Throwable cause) {
        super(message, cause);
    }

    public OWLIndividualExistsException(Throwable cause) {
        super(cause);
    }
}
