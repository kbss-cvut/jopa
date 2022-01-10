/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.datatype.DatatypeTransformer;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.datatype.exception.UnsupportedTypeTransformationException;
import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

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
     * @param resultRow Result set with value to map
     * @param target    Target object on which the field will be set
     */
    void map(ResultRow resultRow, Object target, UnitOfWork uow) {
        final Optional<Object> value = getVariableValue(resultRow);
        value.ifPresent(
                val -> EntityPropertiesUtils.setFieldValue(fieldSpec.getJavaField(), target, resolveValue(val)));
    }

    Optional<Object> getVariableValue(ResultRow resultRow) {
        try {
            if (!resultRow.isBound(variableName)) {
                return Optional.empty();
            }
            return Optional.of(resultRow.getObject(variableName));
        } catch (OntoDriverException e) {
            throw new SparqlResultMappingException(e);
        }
    }

    private Object resolveValue(Object queryValue) {
        if (fieldSpec.getJavaType().isAssignableFrom(queryValue.getClass())) {
            return queryValue;
        }
        try {
            return DatatypeTransformer.transform(queryValue, fieldSpec.getJavaType());
        } catch (UnsupportedTypeTransformationException e) {
            throw new SparqlResultMappingException(
                    String.format("Value %s cannot be assigned (or transformed) to field of type %s.", queryValue,
                            fieldSpec.getJavaType()));
        }
    }
}
