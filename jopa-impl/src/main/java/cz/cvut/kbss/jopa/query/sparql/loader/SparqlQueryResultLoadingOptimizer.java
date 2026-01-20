package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.BaseEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.NonEntityQueryResultLoader;
import cz.cvut.kbss.jopa.model.QueryResultLoader;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Allows optimizing queries that load entity instances.
 */
public class SparqlQueryResultLoadingOptimizer extends QueryResultLoadingOptimizer<TokenStreamSparqlQueryHolder> {

    private static final Logger LOG = LoggerFactory.getLogger(SparqlQueryResultLoadingOptimizer.class);

    private final UnitOfWork uow;

    private final ConnectionWrapper connection;

    public SparqlQueryResultLoadingOptimizer(TokenStreamSparqlQueryHolder queryHolder, UnitOfWork uow,
                                             ConnectionWrapper connection) {
        super(queryHolder);
        this.uow = uow;
        this.connection = connection;
    }

    @Override
    public void optimizeQueryAssembly(Class<?> resultClass, Descriptor descriptor) {
        switch (resolveOptimizerType(resultClass, descriptor)) {
            case TRIPLE_BASED:
                LOG.trace("Processing query results with triple-based optimized entity loading.");
                queryHolder.setAssemblyModifier(new UnboundPredicateObjectSparqlAssemblyModifier());
                break;
            case ATTRIBUTE_BASED:
                LOG.trace("Processing query results with attribute enumeration-based optimized attribute loading.");
                queryHolder.setAssemblyModifier(new AttributeEnumeratingSparqlAssemblyModifier(uow.getMetamodel()
                                                                                                  .entity(resultClass), descriptor, connection));
                break;
            default:
                // Do nothing
                break;
        }
    }

    private OptimizerType resolveOptimizerType(Class<?> resultClass, Descriptor descriptor) {
        if (!canOptimize(resultClass, descriptor)) {
            return OptimizerType.NONE;
        }
        final IdentifiableEntityType<?> et = uow.getMetamodel().entity(resultClass);
        if (et.getProperties() == null && !et.hasSubtypes()) {
            return OptimizerType.ATTRIBUTE_BASED;
        }
        if (!queryContainsGraphOrServiceClause()) {
            return OptimizerType.TRIPLE_BASED;
        }
        return OptimizerType.NONE;
    }

    private boolean canOptimize(Class<?> resultClass, Descriptor descriptor) {
        return optimizationEnabled && queryHolder.getQueryType() == QueryType.SELECT
                && projectsEntity(resultClass) && limitOrOffsetNotSet() && descriptorSpecifiesAtMostOneContext(descriptor);
    }

    private boolean projectsEntity(Class<?> resultClass) {
        return uow.isEntityType(resultClass) && queryHolder.getProjectedQueryParameters().size() == 1;
    }

    private boolean limitOrOffsetNotSet() {
        return !queryHolder.hasOffset() && !queryHolder.hasLimit();
    }

    /**
     * Descriptor specifies at most one repository context for the root entity and all its attributes.
     *
     * @param descriptor Descriptor to examine
     * @return {@code true} if at most one context is used, {@code false} otherwise
     */
    private boolean descriptorSpecifiesAtMostOneContext(Descriptor descriptor) {
        return descriptor.getContexts().size() <= 1 && descriptor.getAttributeDescriptors().stream()
                                                                 .allMatch(d -> d.getContexts().size() <= 1);
    }

    private boolean queryContainsGraphOrServiceClause() {
        // Do not optimize a query containing GRAPH or SERVICE with unbound predicate.
        // The optimization pattern (?x ?p ?v) has no control over individual property/attribute contexts, and it could lead to incorrect results
        return queryHolder.getQueryAttributes().hasGraphOrService();
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
        final OptimizerType optimizerType = resolveOptimizerType(resultClass, descriptor);
        return switch (optimizerType) {
            case TRIPLE_BASED -> new TripleBasedRowsToAxiomsQueryResultLoader<>(uow, resultClass, descriptor);
            case ATTRIBUTE_BASED -> new AttributeBasedRowsToAxiomsQueryResultLoader<>(uow, resultClass, descriptor);
            default ->
                    uow.isEntityType(resultClass) ? new BaseEntityQueryResultLoader<>(uow, resultClass, descriptor) : new NonEntityQueryResultLoader<>(resultClass);
        };
    }

    private enum OptimizerType {
        TRIPLE_BASED,
        ATTRIBUTE_BASED,
        NONE
    }
}
