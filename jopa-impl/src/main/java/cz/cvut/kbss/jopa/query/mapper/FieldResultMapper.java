/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Optional;

class FieldResultMapper {

    private final String variableName;

    private final FieldSpecification<?, ?> fieldSpec;

    FieldResultMapper(FieldResult fieldResult, FieldSpecification<?, ?> fieldSpec) {
        this.variableName = fieldResult.variable();
        this.fieldSpec = fieldSpec;
    }

    /**
     * Creates mappers for which no explicit {@link FieldResult} configuration exists.
     * <p>
     * Variable name is taken to be the same as the name of the field.
     */
    FieldResultMapper(FieldSpecification<?, ?> fieldSpec) {
        this.fieldSpec = fieldSpec;
        this.variableName = fieldSpec.getName();
    }

    String getVariableName() {
        return variableName;
    }

    FieldSpecification<?, ?> getFieldSpecification() {
        return fieldSpec;
    }

    /**
     * Maps value from the specified result set to the specified target object's field based on the mapping represented
     * by this instance.
     *
     * @param resultSet Result set with value to map
     * @param target    Target object on which the field will be set
     */
    void map(ResultSet resultSet, Object target, UnitOfWork uow) {
        final Optional<Object> value = getVariableValue(resultSet);
        value.ifPresent(val -> {
            verifyValueRange(val);
            EntityPropertiesUtils.setFieldValue(fieldSpec.getJavaField(), target, val);
        });
    }

    Optional<Object> getVariableValue(ResultSet resultSet) {
        try {
            if (!resultSet.isBound(variableName)) {
                return Optional.empty();
            }
            return Optional.of(resultSet.getObject(variableName));
        } catch (OntoDriverException e) {
            throw new SparqlResultMappingException(e);
        }
    }

    private void verifyValueRange(Object value) {
        if (!fieldSpec.getJavaType().isAssignableFrom(value.getClass())) {
            throw new SparqlResultMappingException(
                    String.format("Value %s cannot be assigned to field of type %s.", value, fieldSpec.getJavaType()));
        }
    }
}
