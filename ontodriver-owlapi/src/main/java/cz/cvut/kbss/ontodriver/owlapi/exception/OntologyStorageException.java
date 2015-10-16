package cz.cvut.kbss.ontodriver.owlapi.exception;

/**
 * Thrown when an error occurs during storage of an ontology.
 */
public class OntologyStorageException extends OwlapiDriverException {

    public OntologyStorageException() {
    }

    public OntologyStorageException(String message) {
        super(message);
    }

    public OntologyStorageException(Throwable cause) {
        super(cause);
    }

    public OntologyStorageException(String message, Throwable cause) {
        super(message, cause);
    }
}
