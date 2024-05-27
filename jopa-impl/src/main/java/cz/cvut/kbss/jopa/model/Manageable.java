package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * Interface dynamically assigned to entity classes so that their instances may be attached to a persistence context.
 */
public interface Manageable {

    /**
     * Sets persistence context to this instance.
     * <p>
     * Done when an object enters the persistence context
     *
     * @param uow Persistence context
     */
    void setPersistenceContext(UnitOfWork uow);

    /**
     * Gets the persistence context associated with this entity instance.
     *
     * @return Persistence context instance
     */
    UnitOfWork getPersistenceContext();
}
