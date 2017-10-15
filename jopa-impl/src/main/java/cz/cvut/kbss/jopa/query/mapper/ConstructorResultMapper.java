package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.ontodriver.ResultSet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Maps SPARQL query result to target value using a constructor configured via a {@link cz.cvut.kbss.jopa.model.annotations.ConstructorResult} configuration.
 */
public class ConstructorResultMapper implements SparqlResultMapper {

    private final Class<?> targetType;

    private final List<VariableResultMapper> paramMappers = new ArrayList<>();

    ConstructorResultMapper(Class<?> targetType) {
        this.targetType = targetType;
    }

    Class<?> getTargetType() {
        return targetType;
    }

    List<VariableResultMapper> getParamMappers() {
        return Collections.unmodifiableList(paramMappers);
    }

    void addParameterMapper(VariableResultMapper mapper) {
        paramMappers.add(mapper);
    }

    @Override
    public Object map(ResultSet resultSet) {
        return null;
    }
}
