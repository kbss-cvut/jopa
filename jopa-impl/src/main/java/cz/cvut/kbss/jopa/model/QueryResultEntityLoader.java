package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.Optional;

/**
 * Loads entities from a query result row.
 *
 * @param <T> Type of the result entity
 */
interface QueryResultEntityLoader<T> {

    /**
     * Loads an entity instance from the given result row.
     *
     * @param resultRow Result row to load entity from
     * @return Loaded entity, if present
     */
    Optional<T> loadEntityInstance(ResultRow resultRow);
}
