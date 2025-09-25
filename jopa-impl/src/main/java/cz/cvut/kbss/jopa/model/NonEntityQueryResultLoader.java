package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.Optional;

/**
 * Query result loader for a non-entity result type.
 * <p>
 * The loader takes the result row and attempts to load the first binding as an instance of the specified class.
 *
 * @param <T> Result type
 */
class NonEntityQueryResultLoader<T> implements QueryResultLoader<T> {

    private final Class<T> resultType;

    NonEntityQueryResultLoader(Class<T> resultType) {
        this.resultType = resultType;
    }

    @Override
    public Optional<T> loadEntityInstance(ResultRow resultRow) {
        try {
            return Optional.of(resultRow.getObject(0, resultType));
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException("Unable to map the query result to class " + resultType, e);
        }
    }
}
