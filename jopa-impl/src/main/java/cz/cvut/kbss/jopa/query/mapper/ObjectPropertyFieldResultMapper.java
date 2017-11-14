/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.ResultSet;

import java.util.Optional;

/**
 * Mapping SPARQL SELECT results to object property fields.
 * <p>
 * This means that the referenced instance has to be loaded (unless the field is plain identifier).
 */
class ObjectPropertyFieldResultMapper extends FieldResultMapper {

    ObjectPropertyFieldResultMapper(FieldResult fieldResult, FieldSpecification<?, ?> fieldSpec) {
        super(fieldResult, fieldSpec);
    }

    ObjectPropertyFieldResultMapper(FieldSpecification<?, ?> fieldSpec) {
        super(fieldSpec);
    }

    @Override
    void map(ResultSet resultSet, Object target, UnitOfWork uow) {
        final Optional<Object> id = getVariableValue(resultSet);
        id.ifPresent(idValue -> {
            final Object value = resolveValue(uow, idValue);
            EntityPropertiesUtils.setFieldValue(getFieldSpecification().getJavaField(), target, value);
        });
    }

    private Object resolveValue(UnitOfWork uow, Object id) {
        if (IdentifierTransformer.isValidIdentifierType(getFieldSpecification().getJavaType())) {
            return IdentifierTransformer.transformToIdentifier(id, getFieldSpecification().getJavaType());
        }
        return uow.readObject(getFieldSpecification().getJavaType(), id, new EntityDescriptor());
    }
}
