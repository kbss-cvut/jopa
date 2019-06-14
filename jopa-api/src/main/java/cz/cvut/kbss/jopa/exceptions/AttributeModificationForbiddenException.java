package cz.cvut.kbss.jopa.exceptions;

/**
 * Indicates that a modification to an attribute could not be performed.
 *
 * This exception is raised when an attribute is read only for some reason and the application attempts to change its value.
 *
 * Concrete subclasses will provide more context as to why this exception was thrown.
 */
public class AttributeModificationForbiddenException extends OWLPersistenceException {

    public AttributeModificationForbiddenException(String message) {
        super(message);
    }
}
