package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.ConstructorResult;
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
        final ResultRowMapper rowMapper = new ResultRowMapper(mapping.name());
        buildVariableMappers(mapping, rowMapper);
        buildConstructorMappers(mapping, rowMapper);
        manager.addMapper(rowMapper.getName(), rowMapper);
    }

    private void buildVariableMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
        for (VariableResult vr : mapping.variables()) {
            parent.addMapper(new VariableResultMapper(vr));
        }
    }

    private void buildConstructorMappers(SparqlResultSetMapping mapping, ResultRowMapper parent) {
        for (ConstructorResult cr : mapping.classes()) {
            final ConstructorResultMapper mapper = new ConstructorResultMapper(cr.targetClass());
            for (VariableResult vr : cr.variables()) {
                mapper.addParameterMapper(new VariableResultMapper(vr));
            }
            parent.addMapper(mapper);
        }
    }

    public ResultSetMappingManager getManager() {
        return manager;
    }
}
