package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.Optional;

/**
 * Loads result from a query result row.
 *
 * @param <T> Type of the result
 */
public interface QueryResultLoader<T> {

    /**
     * Loads the result from the given result row.
     *
     * @param resultRow Result row to load from
     * @return Loaded value, if present
     */
    Optional<T> loadResult(ResultRow resultRow);

    /**
     * If the loader performs any aggregation, use this to method to get the last pending result.
     *
     * @return Last pending value
     */
    default Optional<T> loadLastPending() {
        return Optional.empty();
    }
}
