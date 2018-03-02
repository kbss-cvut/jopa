package cz.cvut.kbss.ontodriver.jena.exception;

/**
 * Thrown when an error occurs during processing of an OWL list.
 */
public class ListProcessingException extends RuntimeException {

    public ListProcessingException(String message) {
        super(message);
    }
}
