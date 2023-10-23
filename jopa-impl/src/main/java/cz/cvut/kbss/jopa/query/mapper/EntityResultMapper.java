/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.lifecycle.PostLoadInvoker;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Maps result of a SPARQL SELECT query to an entity instance.
 */
class EntityResultMapper<T> implements SparqlResultMapper {

    private final IdentifiableEntityType<T> et;

    private final List<FieldResultMapper> fieldMappers = new ArrayList<>();

    EntityResultMapper(IdentifiableEntityType<T> et) {
        this.et = et;
    }

    void addFieldMapper(FieldResultMapper mapper) {
        fieldMappers.add(mapper);
    }

    List<FieldResultMapper> getFieldMappers() {
        return Collections.unmodifiableList(fieldMappers);
    }

    EntityType<T> getEntityType() {
        return et;
    }

    @Override
    public T map(ResultRow resultRow, UnitOfWorkImpl uow) {
        try {
            final T instance = ReflectionUtils.instantiateUsingDefaultConstructor(et.getJavaType());
            fieldMappers.forEach(m -> m.map(resultRow, instance, uow));
            return et.getJavaType().cast(uow.registerExistingObject(instance, new EntityDescriptor(),
                    Collections.singletonList(new PostLoadInvoker(uow.getMetamodel()))));
        } catch (cz.cvut.kbss.jopa.exception.InstantiationException e) {
            // This is not expected, since an entity class must have a public no-arg constructor
            throw new SparqlResultMappingException(e);
        }
    }
}
