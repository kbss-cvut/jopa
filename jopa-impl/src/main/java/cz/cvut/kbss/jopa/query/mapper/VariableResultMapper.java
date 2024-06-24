/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.datatype.DatatypeTransformer;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.model.metamodel.Converters;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

/**
 * Maps SPARQL query result to target value based on a {@link VariableResult} configuration.
 */
class VariableResultMapper implements SparqlResultMapper {

    private final String name;
    private final Class<?> targetType;

    VariableResultMapper(VariableResult mapping) {
        this.name = mapping.name();
        this.targetType = mapping.type();
    }

    String getName() {
        return name;
    }

    Class<?> getTargetType() {
        return targetType;
    }

    /**
     * Maps value from the current line of the specified result set according to the {@link VariableResult}
     * configuration represented by this instance.
     *
     * @param resultRow Query result set to read
     * @param uow       UnitOfWork instance
     * @return The mapped value
     */
    @Override
    public Object map(ResultRow resultRow, UnitOfWork uow) {
        try {
            if (!resultRow.isBound(name)) {
                return null;
            }
            final Object value = resultRow.getObject(name);
            if (!void.class.equals(targetType)) {
                if (Converters.getDefaultConverters().containsKey(targetType)) {
                    return ((ConverterWrapper) Converters.getDefaultConverters()
                            .get(targetType)).convertToAttribute(value);
                }
                return DatatypeTransformer.transform(value, targetType);
            }
            return value;
        } catch (OntoDriverException e) {
            throw new SparqlResultMappingException(e);
        }
    }
}
