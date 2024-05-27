package cz.cvut.kbss.jopa.exception;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Indicates that an attempt has been made to instantiate a class that is abstract in terms of JOPA.
 * <p>
 * This could be, for example, a {@link cz.cvut.kbss.jopa.model.annotations.MappedSuperclass} type, an {@link
 * cz.cvut.kbss.jopa.model.metamodel.EntityType} representing an interface or an abstract entity class that needs to be
 * inherited for instantiation.
 */
public class AbstractTypeException extends OWLPersistenceException {

    public AbstractTypeException(String message) {
        super(message);
    }
}
