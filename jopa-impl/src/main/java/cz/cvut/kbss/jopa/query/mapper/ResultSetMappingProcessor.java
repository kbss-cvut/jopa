package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;

/**
 * Builds mappers for {@link SparqlResultSetMapping} instances discovered on classpath.
 */
public class ResultSetMappingProcessor {

    private final ResultSetMappingManager manager = new ResultSetMappingManager();

    /**
     * Builds result set mapper for the specified mapping.
     *
     * @param mapping Mapping configuration
     */
    public void buildMapper(SparqlResultSetMapping mapping) {

    }

    public ResultSetMappingManager getManager() {
        return manager;
    }
}
