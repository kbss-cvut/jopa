package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;

import java.util.Objects;

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
        Objects.requireNonNull(mapping);
        final ResultRowMapper mapper = new ResultRowMapper(mapping.name());
        buildVariableMappers(mapping, mapper);
        manager.addMapper(mapper.getName(), mapper);
    }

    private void buildVariableMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
        for (VariableResult vr : mapping.variables()) {
            parent.addMapper(new VariableResultMapper(vr));
        }
    }

    public ResultSetMappingManager getManager() {
        return manager;
    }
}
