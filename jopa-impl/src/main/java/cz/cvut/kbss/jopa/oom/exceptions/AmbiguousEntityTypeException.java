package cz.cvut.kbss.jopa.oom.exceptions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Thrown when a polymorphic entity cannot be loaded because its types are ambiguous, i.e. there are multiple entity
 * types which match types of the individual.
 */
public class AmbiguousEntityTypeException extends OWLPersistenceException {

    public AmbiguousEntityTypeException(String message) {
        super(message);
    }
}
