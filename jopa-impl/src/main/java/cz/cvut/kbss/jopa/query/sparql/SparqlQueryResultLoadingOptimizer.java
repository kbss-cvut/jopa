package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.BaseEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.JOPAExperimentalProperties;
import cz.cvut.kbss.jopa.model.NonEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * Allows optimizing queries that load entity instances.
 */
public class SparqlQueryResultLoadingOptimizer extends QueryResultLoadingOptimizer<TokenStreamSparqlQueryHolder> {

    private final UnitOfWork uow;

    public SparqlQueryResultLoadingOptimizer(TokenStreamSparqlQueryHolder queryHolder, UnitOfWork uow) {
        super(queryHolder);
        this.uow = uow;
    }

    @Override
    public void optimizeQueryAssembly(Class<?> resultClass) {
        this.enableOptimization();
        if (canOptimize(resultClass)) {
            queryHolder.setAssemblyModifier(new EntityLoadingSparqlAssemblyModifier());
        }
    }

    private boolean canOptimize(Class<?> resultClass) {
        return optimizationEnabled && queryHolder.getQueryType() == QueryType.SELECT
                && projectsEntity(resultClass) && !limitOffsetSet()
                && uow.getConfiguration().is(JOPAExperimentalProperties.QUERY_ENABLE_ENTITY_LOADING_OPTIMIZER);
    }

    private boolean projectsEntity(Class<?> resultClass) {
        return uow.isEntityType(resultClass) && queryHolder.getProjectedQueryParameters().size() == 1;
    }

    private boolean limitOffsetSet() {
        return queryHolder.getFirstResult() != 0 || queryHolder.getMaxResults() != Integer.MAX_VALUE;
    }

    /**
     * Gets loader of query results for the specified query.
     * <p>
     * If possible, a version supporting optimized entity loading is returned.
     *
     * @param resultClass Result class
     * @param descriptor  Descriptor specified for results
     * @param <T>         Result type
     * @return Query result loader
     */
    @Override
    public <T> QueryResultLoader<T> getQueryResultLoader(Class<T> resultClass,
                                                         Descriptor descriptor) {
        if (uow.isEntityType(resultClass)) {
            if (canOptimize(resultClass)) {
                return new RowsToAxiomsEntityQueryResultLoader<>(uow, resultClass, descriptor);
            }
            return new BaseEntityQueryResultLoader<>(uow, resultClass, descriptor);
        }
        return new NonEntityQueryResultLoader<>(resultClass);
    }
}
