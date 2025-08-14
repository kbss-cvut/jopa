package cz.cvut.kbss.jopa.plugin;

import cz.cvut.kbss.jopa.model.EntityManager;

/**
 * Interface to be implemented by JOPA lifecycle plugins.
 * <p>
 * The plugins may implement only selected methods, as they all by default do nothing.
 * <p>
 * Implementations must provide a public no-args constructor.
 */
public interface PersistenceUnitLifecyclePlugin {

    /**
     * Invoked after the persistence unit has been created.
     * <p>
     * This method is invoked when the persistence unit is fully initialized.
     *
     * @param em EntityManager from the created persistence unit
     */
    default void afterPersistenceUnitCreated(EntityManager em) {
        // Do nothing by default
    }

    /**
     * Invoked before the persistence unit is destroyed (typically on application shutdown).
     * <p>
     * At the time of this call, all the entity managers from this persistence unit are already closed, but a connection
     * to the underlying repository driver is still open.
     *
     * @param em Entity manager from the persistence unit that is shutting down
     */
    default void beforePersistenceUnitDestroyed(EntityManager em) {
        // Do nothing by default
    }
}
