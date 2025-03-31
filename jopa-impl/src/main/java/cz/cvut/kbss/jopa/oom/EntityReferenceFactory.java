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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.proxy.reference.EntityReferenceProxy;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.InvocationTargetException;

class EntityReferenceFactory {

    private final MetamodelImpl metamodel;

    private final UnitOfWork uow;

    EntityReferenceFactory(MetamodelImpl metamodel, UnitOfWork uow) {
        this.metamodel = metamodel;
        this.uow = uow;
    }

    /**
     * Creates entity reference proxy with initialized identifier and associated persistence context.
     *
     * @param loadingParameters Parameters for creating the reference
     * @param <T>               Type of the expected instance
     * @return Entity reference proxy
     */
    <T> T createReferenceProxy(LoadingParameters<T> loadingParameters) {
        assert loadingParameters != null;

        final Class<? extends T> referenceProxyClass = metamodel.getEntityReferenceProxy(loadingParameters.getEntityClass());
        try {
            final T reference = referenceProxyClass.getDeclaredConstructor().newInstance();
            assert reference instanceof EntityReferenceProxy<?>;
            final EntityReferenceProxy<?> referenceProxy = (EntityReferenceProxy<?>) reference;
            referenceProxy.setIdentifier(loadingParameters.getIdentifier());
            referenceProxy.setType((Class) loadingParameters.getEntityClass());
            referenceProxy.setPersistenceContext(uow);
            referenceProxy.setDescriptor(loadingParameters.getDescriptor());
            EntityPropertiesUtils.setIdentifier(loadingParameters.getIdentifier(), reference, metamodel.entity(loadingParameters.getEntityClass()));
            return reference;
        } catch (InstantiationException | IllegalAccessException | NoSuchMethodException |
                 InvocationTargetException e) {
            // We do not expect this to happen, as we generated the proxy class
            throw new OWLPersistenceException("Unable to instantiate entity reference proxy class " + referenceProxyClass, e);
        }
    }
}
