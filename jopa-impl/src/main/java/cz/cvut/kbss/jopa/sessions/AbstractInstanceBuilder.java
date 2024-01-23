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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.security.PrivilegedActionException;

abstract class AbstractInstanceBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractInstanceBuilder.class);

    protected final CloneBuilder builder;
    protected final UnitOfWorkImpl uow;

    AbstractInstanceBuilder(CloneBuilder builder, UnitOfWorkImpl uow) {
        this.builder = builder;
        this.uow = uow;
    }

    /**
     * Returns true if this builder instances automatically populates the created instance's attribute.
     *
     * @return boolean
     */
    boolean populatesAttributes() {
        return false;
    }

    /**
     * Builds new instance from the original. </p>
     * <p>
     * For some implementations this may mean creating an empty object, others might choose to initialize it using the
     * original data.
     *
     * @param cloneOwner         Instance owning the clone which will be created
     * @param field              Field which will contain the clone
     * @param original           The original object
     * @param cloneConfiguration Configuration for the cloning process
     * @return The clone
     */
    abstract Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration cloneConfiguration);

    /**
     * Merges changes from clone to the original.
     *
     * @param field         The field we are merging
     * @param target        target object on which the values are merged
     * @param originalValue The original value
     * @param cloneValue    The clone value
     */
    abstract void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue);

    /**
     * Return the declared constructor for the specified class. If the constructor is not accessible, it is set
     * accessible. If there is no constructor corresponding to the specified argument list, null is returned.
     *
     * @param javaClass The class of the constructor.
     * @param args      An Array of classes, which should take the constructor as parameters.
     * @return Constructor
     * @throws SecurityException If the security check denies access to the constructor.
     */
    protected static Constructor<?> getDeclaredConstructorFor(final Class<?> javaClass, Class<?>[] args) {
        Constructor<?> c;
        try {
            c = javaClass.getDeclaredConstructor(args);
            if (!c.canAccess(null)) {
                c.setAccessible(true);
            }
        } catch (NoSuchMethodException e) {
            // No constructor matching the argument types
            return null;
        } catch (RuntimeException e) {
            // Constructor cannot be resolved for some other reason
            LOG.warn("Unable to get constructor for arguments of type {}. Got runtime exception {}.", args, e);
            return null;
        }
        return c;
    }

    protected static void logConstructorAccessException(Constructor<?> constructor, Exception e) {
        LOG.warn("Exception caught when invoking constructor " + constructor + ". Exception: " + e);
    }

    protected static void logPrivilegedConstructorAccessException(Constructor<?> constructor,
                                                                  PrivilegedActionException e) {
        LOG.warn("Exception caught on privileged invocation of constructor " + constructor + ". Exception: " + e);
    }
}
