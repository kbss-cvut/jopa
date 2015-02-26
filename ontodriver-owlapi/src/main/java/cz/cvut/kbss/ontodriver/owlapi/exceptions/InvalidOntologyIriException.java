package cz.cvut.kbss.ontodriver.owlapi.exceptions;

/**
 * Thrown when IRI of ontology does not match the expected one.
 */
public class InvalidOntologyIriException extends RuntimeException {

    public InvalidOntologyIriException() {
    }

    public InvalidOntologyIriException(String message) {
        super(message);
    }
}
