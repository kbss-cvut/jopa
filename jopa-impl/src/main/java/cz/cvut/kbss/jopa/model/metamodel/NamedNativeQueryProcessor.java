package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.NamedNativeQueries;
import cz.cvut.kbss.jopa.model.annotations.NamedNativeQuery;
import cz.cvut.kbss.jopa.query.NamedQueryManager;

import java.util.List;

class NamedNativeQueryProcessor {

    private final NamedQueryManager queryManager;

    NamedNativeQueryProcessor(NamedQueryManager queryManager) {
        this.queryManager = queryManager;
    }

    /**
     * Discovers named native queries in the specified class.
     * <p>
     * The queries (if found) are added to the {@code NamedQueryManager}, which was passed to this class in constructor.
     *
     * @param cls The class to process
     */
    <T> void processClass(Class<T> cls) {
        final List<Class<? super T>> hierarchy = EntityClassProcessor.getEntityHierarchy(cls);
        for (Class<? super T> c : hierarchy) {
            final NamedNativeQueries queries = c.getAnnotation(NamedNativeQueries.class);
            if (queries != null) {
                for (NamedNativeQuery q : queries.value()) {
                    processQuery(q);
                }
            }
            final NamedNativeQuery nq = c.getAnnotation(NamedNativeQuery.class);
            if (nq != null) {
                processQuery(nq);
            }
        }
    }

    private void processQuery(NamedNativeQuery query) {
        queryManager.addNamedQuery(query.name(), query.query());
    }
}
