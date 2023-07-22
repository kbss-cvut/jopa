package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates an error during parsing and translation of SOQL to SPARQL.
 */
public class SoqlException extends OWLPersistenceException {

    public SoqlException(String message) {
        super(message);
    }
}
