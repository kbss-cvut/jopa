package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.BaseEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.NonEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Allows optimizing queries that load entity instances.
 */
public class SparqlQueryResultLoadingOptimizer extends QueryResultLoadingOptimizer<TokenStreamSparqlQueryHolder> {

    private static final Logger LOG = LoggerFactory.getLogger(SparqlQueryResultLoadingOptimizer.class);

    private final UnitOfWork uow;

    public SparqlQueryResultLoadingOptimizer(TokenStreamSparqlQueryHolder queryHolder, UnitOfWork uow) {
        super(queryHolder);
        this.uow = uow;
    }

    @Override
    public void optimizeQueryAssembly(Class<?> resultClass) {
        if (canOptimize(resultClass)) {
            LOG.trace("Processing query results with optimized entity loading.");
            queryHolder.setAssemblyModifier(new UnboundPredicateObjectSparqlAssemblyModifier());
        }
    }

    private boolean canOptimize(Class<?> resultClass) {
        return optimizationEnabled && queryHolder.getQueryType() == QueryType.SELECT
                && projectsEntity(resultClass) && limitOrOffsetNotSet() && queryDoesNotContainGraphOrServiceClause();
    }

    private boolean projectsEntity(Class<?> resultClass) {
        return uow.isEntityType(resultClass) && queryHolder.getProjectedQueryParameters().size() == 1;
    }

    private boolean limitOrOffsetNotSet() {
        return !queryHolder.hasOffset() && !queryHolder.hasLimit();
    }

    private boolean queryDoesNotContainGraphOrServiceClause() {
        // Do not optimize a query containing GRAPH or SERVICE. The optimization pattern uses the default context, and it
        // could lead to incorrect results
        return !queryHolder.getQueryAttributes().hasGraphOrService();
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
    public <T> QueryResultLoader<T> getQueryResultLoader(Class<T> resultClass, Descriptor descriptor) {
        if (uow.isEntityType(resultClass)) {
            if (canOptimize(resultClass)) {
                return new TripleBasedRowsToAxiomsQueryResultLoader<>(uow, resultClass, descriptor);
            }
            return new BaseEntityQueryResultLoader<>(uow, resultClass, descriptor);
        }
        return new NonEntityQueryResultLoader<>(resultClass);
    }
}
