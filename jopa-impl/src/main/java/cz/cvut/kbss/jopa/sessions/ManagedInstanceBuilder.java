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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;

import java.lang.reflect.Field;

/**
 * Builds instances of entity types.
 * <p>
 * This builder expects the original's class has a public no-arg constructor. Furthermore, if the configuration
 * specifies that the result will be registered in a persistence context, the instance built is not the base Java type
 * of the original, but rather the {@link IdentifiableEntityType#getInstantiableJavaType()} result, which is a generated
 * subclass whose instances can be attached to the persistence context.
 */
public class ManagedInstanceBuilder extends DefaultInstanceBuilder {

    ManagedInstanceBuilder(CloneBuilder builder, UnitOfWork uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration config) {
        assert uow.isEntityType(original.getClass());
        final IdentifiableEntityType<?> et = uow.getMetamodel().entity(original.getClass());
        assert et != null;
        final Class<?> cls = config.isForPersistenceContext() ? et.getInstantiableJavaType() : et.getJavaType();
        return ReflectionUtils.instantiateUsingDefaultConstructor(cls);
    }
}
