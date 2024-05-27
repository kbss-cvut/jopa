package cz.cvut.kbss.jopa.model;

/**
 * Represents the state of an entity with respect to a persistence context.
 */
public enum EntityState {
    MANAGED, MANAGED_NEW, NOT_MANAGED, REMOVED
}
