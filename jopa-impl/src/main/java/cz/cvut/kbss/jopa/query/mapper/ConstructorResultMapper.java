/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.LangString;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Maps SPARQL query result to target value using a constructor configured via a {@link
 * cz.cvut.kbss.jopa.model.annotations.ConstructorResult} configuration.
 */
class ConstructorResultMapper implements SparqlResultMapper {

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
    public Object map(ResultRow resultRow, UnitOfWorkImpl uow) {
        final Object[] values = new Object[paramMappers.size()];
        final Class<?>[] types = new Class[paramMappers.size()];
        for (int i = 0; i < paramMappers.size(); i++) {
            values[i] = paramMappers.get(i).map(resultRow, uow);
            types[i] = values[i] != null ? values[i].getClass() : paramMappers.get(i).getTargetType();
        }
        return buildInstance(values, types);
    }

    private Object buildInstance(Object[] values, Class<?>[] types) {
        try {
            final Constructor<?> ctor = resolveConstructor(types);
            if (!ctor.canAccess(null)) {
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

    private Constructor<?> resolveConstructor(Class<?>[] types) throws NoSuchMethodException {
        try {
            return targetType.getDeclaredConstructor(types);
        } catch (NoSuchMethodException e) {
            boolean replaced = false;
            // Try replacing LangString with String and finding a constructor then
            for (int i = 0; i < types.length; i++) {
                if (types[i].equals(LangString.class)) {
                    replaced = true;
                    types[i] = String.class;
                }
            }
            if (replaced) {
                return targetType.getDeclaredConstructor(types);
            }
            throw e;
        }
    }
}
