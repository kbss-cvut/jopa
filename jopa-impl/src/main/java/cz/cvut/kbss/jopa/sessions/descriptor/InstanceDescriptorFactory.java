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
package cz.cvut.kbss.jopa.sessions.descriptor;

import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

/**
 * Builds {@link InstanceDescriptor}s on various occasions.
 */
public class InstanceDescriptorFactory {

    private InstanceDescriptorFactory() {
        throw new AssertionError();
    }

    /**
     * Creates an instance descriptor which marks all attributes except the identifier as not loaded.
     *
     * @param instance Instance to create descriptor for
     * @param et       Entity type of the instance
     * @param <T>      Instance type
     * @return Fresh instance descriptor
     */
    public static <T> InstanceDescriptor<T> createNotLoaded(T instance, EntityType<T> et) {
        return new InstanceDescriptor<>(instance, et);
    }

    /**
     * Creates an instance descriptor which marks all attributes as loaded.
     *
     * @param instance Instance to create descriptor for
     * @param et       Entity type of the instance
     * @param <T>      Instance type
     * @return Fresh instance descriptor with all loaded
     */
    public static <T> InstanceDescriptor<T> createAllLoaded(T instance, EntityType<T> et) {
        final InstanceDescriptor<T> descriptor = createNotLoaded(instance, et);
        et.getFieldSpecifications().forEach(fs -> descriptor.setLoaded(fs, LoadState.LOADED));
        return descriptor;
    }

    /**
     * Creates an instance descriptor which sets load status of attributes based on their value in the specified
     * instance as follows:
     * <p>
     * If the attribute value is not {@code null}, its status is set to {@link LoadState#LOADED}. If the value is {@code
     * null} and the attribute fetch type is {@link FetchType#EAGER}, the status is also set to {@code LOADED}.
     * Otherwise, the status is set to {@link LoadState#UNKNOWN}.
     *
     * @param instance Instance to create descriptor for
     * @param et       Entity type of the instance
     * @param <T>      Instance type
     * @return Fresh instance descriptor
     */
    public static <T> InstanceDescriptor<T> create(T instance, EntityType<T> et) {
        final InstanceDescriptor<T> descriptor = createNotLoaded(instance, et);
        et.getFieldSpecifications()
          .forEach(fs -> descriptor.setLoaded(fs, fs.getFetchType() == FetchType.EAGER ? LoadState.LOADED :
                                                  EntityPropertiesUtils.getAttributeValue(fs, instance) != null ?
                                                  LoadState.LOADED :
                                                  LoadState.UNKNOWN));
        return descriptor;
    }

    /**
     * Copies the load states from the specified original descriptor into a new descriptor for the specified instance.
     *
     * @param instance Instance to create descriptor for
     * @param original Load state to copy
     * @param <T>      Instance type
     * @return Fresh instance descriptor with state copied from the specified one
     */
    public static <T> InstanceDescriptor<T> createCopy(T instance, InstanceDescriptor<T> original) {
        return new InstanceDescriptor<>(instance, original);
    }
}
