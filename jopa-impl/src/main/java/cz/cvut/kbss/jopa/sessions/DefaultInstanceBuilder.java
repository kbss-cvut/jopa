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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * This class has responsibility for creating new instances of various kinds of objects. It handles security
 * restrictions as well.
 */
class DefaultInstanceBuilder extends AbstractInstanceBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultInstanceBuilder.class);

    DefaultInstanceBuilder(CloneBuilder builder, UnitOfWork uow) {
        super(builder, uow);
    }

    /**
     * Builds a new instance of the specified class.
     *
     * @return New object of the given class.
     */
    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration config) {
        if (CloneBuilder.isImmutable(original)) {
            return original;
        }
        final Class<?> javaClass = original.getClass();
        Object newInstance = buildNewInstanceUsingDefaultConstructor(javaClass);
        if (newInstance == null) {
            final Field[] fields = javaClass.getDeclaredFields();
            List<Class<?>> fieldClasses = new ArrayList<>();
            Constructor<?> c;
            try {
                for (Field f : fields) {
                    if (EntityPropertiesUtils.isFieldTransient(f)) {
                        continue;
                    }
                    Class<?>[] args = {f.getType()};
                    c = getDeclaredConstructorFor(javaClass, args);
                    if (c == null) {
                        fieldClasses.add(f.getType());
                    } else {
                        try {
                            Object[] params = new Object[1];
                            params[0] = original.getClass().getDeclaredField(f.getName());
                            newInstance = c.newInstance(params);
                            return newInstance;
                        } catch (SecurityException e) {
                            logConstructorAccessException(c, e);
                            // Do nothing
                        } catch (NoSuchFieldException e) {
                            throw new OWLPersistenceException(e);
                        }
                    }
                }
                Class<?>[] args = new Class<?>[fieldClasses.size()];
                args = fieldClasses.toArray(args);
                c = getDeclaredConstructorFor(javaClass, args);
                if (c != null) {
                    Object[] params = new Object[args.length];
                    try {
                        newInstance = c.newInstance(params);
                    } catch (SecurityException e) {
                        logConstructorAccessException(c, e);
                        throw new OWLPersistenceException(e);
                    }
                }
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                throw new OWLPersistenceException(e);
            }
        }
        if (newInstance == null) {
            throw new OWLPersistenceException(
                    "Unable to create a new object or to find a suitable constructor for class "
                            + javaClass.getName());
        }
        return newInstance;
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
        if (originalValue == null) {
            Object clOrig = builder.getOriginal(cloneValue);
            if (clOrig == null) {
                clOrig = cloneValue;
            }
            EntityPropertiesUtils.setFieldValue(field, target, clOrig);
            return;
        }
        Class<?> cls = originalValue.getClass();
        if (builder.isTypeManaged(cls) && builder.getOriginal(cloneValue) != null) {
            EntityPropertiesUtils.setFieldValue(field, target, builder.getOriginal(cloneValue));
        } else {
            mergeFieldChanges(originalValue, cloneValue, cls);
        }
    }

    private void mergeFieldChanges(Object originalValue, Object cloneValue, Class<?> cls) {
        List<Field> fields = EntityPropertiesUtils.getAllFields(cls);
        for (Field f : fields) {
            Object clVal = EntityPropertiesUtils.getFieldValue(f, cloneValue);
            Object origVal = EntityPropertiesUtils.getFieldValue(f, originalValue);
            if (!(clVal instanceof Collection) && !builder.isOriginalInUoW(origVal)) {
                EntityPropertiesUtils.setFieldValue(f, originalValue, clVal);
            } else {
                builder.getInstanceBuilder(origVal).mergeChanges(f, originalValue, origVal, clVal);
            }
        }
    }

    /**
     * Builds a new instance of the specified class, using its no-argument constructor.
     *
     * @return New object of the given class, or null if the class has no no-argument constructor.
     */
    private static Object buildNewInstanceUsingDefaultConstructor(final Class<?> javaClass) {
        final Constructor<?> c = getDeclaredConstructorFor(javaClass, null);
        Object newInstance = null;
        if (c != null) {
            try {
                try {
                    newInstance = c.newInstance((Object[]) null);
                } catch (SecurityException e) {
                    logConstructorAccessException(c, e);
                    // Do nothing
                }
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                LOG.trace("Class {} does not have a suitable no-arg constructor.", javaClass);
                // Do nothing
            }
        }
        return newInstance;
    }
}
