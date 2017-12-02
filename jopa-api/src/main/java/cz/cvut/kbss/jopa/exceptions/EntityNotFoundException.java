package cz.cvut.kbss.jopa.exceptions;

import cz.cvut.kbss.jopa.model.EntityManager;

/**
 * Thrown when {@link EntityManager#refresh(Object)}  is called and the object no longer exists in the database.
 * <p>
 * The current transaction, if one is active and the persistence context has been joined to it, will be marked for rollback.
 */
public class EntityNotFoundException extends OWLPersistenceException {

    public EntityNotFoundException(String message) {
        super(message);
    }
}
