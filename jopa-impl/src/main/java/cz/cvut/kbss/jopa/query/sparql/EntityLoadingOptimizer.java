package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.BaseEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.JOPAExperimentalProperties;
import cz.cvut.kbss.jopa.model.NonEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.util.Optional;

/**
 * Allows optimizing queries that load entity instances.
 */
public class EntityLoadingOptimizer {

    private final UnitOfWork uow;

    public EntityLoadingOptimizer(UnitOfWork uow) {
        this.uow = uow;
    }

    /**
     * Optionally gets a SPARQL assembly modifier that adjusts the query for optimized entity instance loading.
     * <p>
     * If the query cannot be optimized, an empty {@link Optional} is returned.
     *
     * @param query       Query holder
     * @param resultClass Query result class
     * @return Optional SPARQL assembly modifier
     */
    public Optional<SparqlAssemblyModifier> getSparqlAssemblyModifier(QueryHolder query, Class<?> resultClass) {
        return canOptimize(query, resultClass) ? Optional.of(new EntityLoadingSparqlAssemblyModifier()) : Optional.empty();
    }

    private boolean canOptimize(QueryHolder query, Class<?> resultClass) {
        return query.getQueryType() == QueryType.SELECT && uow.isEntityType(resultClass)
                && query.getProjectedQueryParameters().size() == 1
                && uow.getConfiguration().is(JOPAExperimentalProperties.QUERY_ENABLE_ENTITY_LOADING_OPTIMIZER);
    }

    /**
     * Gets loader of query results for the specified query.
     * <p>
     * If possible, a version supporting optimized entity loading is returned.
     *
     * @param query       Query to get loader for
     * @param resultClass Result class
     * @param descriptor  Descriptor specified for results
     * @param <T>         Result type
     * @return Query result loader
     */
    public <T> QueryResultLoader<T> getQueryResultLoader(QueryHolder query, Class<T> resultClass,
                                                         Descriptor descriptor) {
        if (uow.isEntityType(resultClass)) {
            if (canOptimize(query, resultClass)) {
                return new RowsToAxiomsEntityQueryResultLoader<>(uow, resultClass, descriptor);
            }
            return new BaseEntityQueryResultLoader<>(uow, resultClass, descriptor);
        }
        return new NonEntityQueryResultLoader<>(resultClass);
    }

    /**
     * Gets loader of query results for the specified query.
     * <p>
     * In contrast to {@link #getQueryResultLoader(QueryHolder, Class, Descriptor)} always returns the default result
     * loader implementations.
     *
     * @param resultClass Result class
     * @param descriptor  Descriptor specified for results
     * @param <T>         Result type
     * @return Query result loader
     */
    public <T> QueryResultLoader<T> getUnoptimizedQueryResultLoader(Class<T> resultClass, Descriptor descriptor) {
        if (uow.isEntityType(resultClass)) {
            return new BaseEntityQueryResultLoader<>(uow, resultClass, descriptor);
        }
        return new NonEntityQueryResultLoader<>(resultClass);
    }
}
