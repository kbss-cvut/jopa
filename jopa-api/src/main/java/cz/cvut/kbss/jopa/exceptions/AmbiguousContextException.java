package cz.cvut.kbss.jopa.exceptions;

import cz.cvut.kbss.jopa.model.descriptors.AbstractDescriptor;

/**
 * Exception thrown when multiple contexts defined in a {@link AbstractDescriptor} are used
 * for saving an assertion.
 */
public class AmbiguousContextException extends OWLPersistenceException {

    public AmbiguousContextException(String message) {
        super(message);
    }
}
