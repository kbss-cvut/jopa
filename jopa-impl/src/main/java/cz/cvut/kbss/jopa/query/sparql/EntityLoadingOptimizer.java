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

public class EntityLoadingOptimizer {

    private final UnitOfWork uow;

    public EntityLoadingOptimizer(UnitOfWork uow) {
        this.uow = uow;
    }

    public boolean canOptimize(QueryHolder query, Class<?> resultClass) {
        return query.getQueryType() == QueryType.SELECT && uow.isEntityType(resultClass)
                && query.getProjectedQueryParameters().size() == 1
                && uow.getConfiguration().is(JOPAExperimentalProperties.QUERY_ENABLE_ENTITY_LOADING_OPTIMIZER);
    }

    public Optional<SparqlAssemblyModifier> getSparqlAssemblyModifier(QueryHolder query, Class<?> resultClass) {
        return canOptimize(query, resultClass) ? Optional.of(new EntityLoadingSparqlAssemblyModifier()) : Optional.empty();
    }

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

    public <T> QueryResultLoader<T> getUnoptimizedQueryResultLoader(Class<T> resultClass, Descriptor descriptor) {
        if (uow.isEntityType(resultClass)) {
            return new BaseEntityQueryResultLoader<>(uow, resultClass, descriptor);
        }
        return new NonEntityQueryResultLoader<>(resultClass);
    }
}
