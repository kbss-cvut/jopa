/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.lifecycle.PostLoadInvoker;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
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
    public T map(ResultRow resultRow, UnitOfWork uow) {
        try {
            final T instance = ReflectionUtils.instantiateUsingDefaultConstructor(et.getJavaType());
            final LoadStateDescriptor<T> loadStateDescriptor = LoadStateDescriptorFactory.createAllUnknown(instance, et);
            fieldMappers.forEach(m -> {
                m.map(resultRow, instance, uow);
                loadStateDescriptor.setLoaded((FieldSpecification<? super T, ?>) m.getFieldSpecification(), LoadState.LOADED);
            });
            uow.getLoadStateRegistry().put(instance, loadStateDescriptor);
            return et.getJavaType()
                     .cast(uow.registerExistingObject(instance, new CloneRegistrationDescriptor(new EntityDescriptor()).postCloneHandlers(List.of(new PostLoadInvoker(uow.getMetamodel())))));
        } catch (cz.cvut.kbss.jopa.exception.InstantiationException e) {
            // This is not expected, since an entity class must have a public no-arg constructor
            throw new SparqlResultMappingException(e);
        }
    }
}
