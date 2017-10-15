package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.ontodriver.ResultSet;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
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
        final Object[] values = new Object[paramMappers.size()];
        final Class<?>[] types = new Class[paramMappers.size()];
        for (int i = 0; i < paramMappers.size(); i++) {
            values[i] = paramMappers.get(i).map(resultSet);
            types[i] = values[i].getClass();
        }
        try {
            final Constructor<?> ctor = targetType.getDeclaredConstructor(types);
            if (!ctor.isAccessible()) {
                ctor.setAccessible(true);
            }
            return ctor.newInstance(values);
        } catch (NoSuchMethodException e) {
            throw new SparqlResultMappingException(
                    String.format("No matching constructor for values %s found in type %s.", Arrays.toString(values),
                            targetType), e);
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
            throw new SparqlResultMappingException(
                    String.format("Unable to map values %s to type %s.", Arrays.toString(values), targetType), e);
        }
    }
}
