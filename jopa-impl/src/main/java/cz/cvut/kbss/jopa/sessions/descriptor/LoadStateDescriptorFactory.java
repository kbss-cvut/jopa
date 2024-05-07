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
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

/**
 * Builds {@link LoadStateDescriptor}s on various occasions.
 */
public class LoadStateDescriptorFactory {

    private LoadStateDescriptorFactory() {
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
    public static <T> LoadStateDescriptor<T> createNotLoaded(T instance, EntityType<T> et) {
        return new LoadStateDescriptor<>(instance, et, LoadState.NOT_LOADED);
    }

    /**
     * Creates an instance descriptor which marks all attributes as loaded.
     *
     * @param instance Instance to create descriptor for
     * @param et       Entity type of the instance
     * @param <T>      Instance type
     * @return Fresh instance descriptor with all loaded
     */
    public static <T> LoadStateDescriptor<T> createAllLoaded(T instance, EntityType<T> et) {
        return new LoadStateDescriptor<>(instance, et, LoadState.LOADED);
    }

    /**
     * Creates an instance descriptor which marks all attributes except the identifier as having an unknown load state.
     *
     * @param instance Instance to create descriptor for
     * @param et       Entity type of the instance
     * @param <T>      Instance type
     * @return Fresh instance descriptor
     */
    public static <T> LoadStateDescriptor<T> createAllUnknown(T instance, EntityType<T> et) {
        return new LoadStateDescriptor<>(instance, et, LoadState.UNKNOWN);
    }

    /**
     * Creates an instance descriptor which sets load status of attributes based on their value in the specified
     * instance as follows:
     * <p>
     * If the attribute value is not {@code null}, its status is set to {@link LoadState#LOADED}. If the value is
     * {@code null} and the attribute fetch type is {@link FetchType#EAGER}, the status is also set to {@code LOADED}.
     * Otherwise, the status is set to {@link LoadState#UNKNOWN}.
     *
     * @param instance Instance to create descriptor for
     * @param et       Entity type of the instance
     * @param <T>      Instance type
     * @return Fresh instance descriptor
     */
    public static <T> LoadStateDescriptor<T> create(T instance, EntityType<T> et) {
        final LoadStateDescriptor<T> descriptor = createNotLoaded(instance, et);
        et.getFieldSpecifications()
          .forEach(fs -> descriptor.setLoaded(fs, getAttributeLoadState(instance, fs)));
        return descriptor;
    }

    private static <T> LoadState getAttributeLoadState(T instance, FieldSpecification<? super T, ?> fs) {
        if (fs.getFetchType() == FetchType.EAGER) {
            return LoadState.LOADED;
        }
        final Object attValue = EntityPropertiesUtils.getAttributeValue(fs, instance);
        if (attValue instanceof LazyLoadingProxy<?>) {
            return LoadState.NOT_LOADED;
        }
        return attValue != null ? LoadState.LOADED : LoadState.UNKNOWN;
    }

    /**
     * Copies the load states from the specified original descriptor into a new descriptor for the specified instance.
     *
     * @param instance Instance to create descriptor for
     * @param original Load state to copy
     * @param <T>      Instance type
     * @return Fresh instance descriptor with state copied from the specified one
     */
    public static <T> LoadStateDescriptor<T> createCopy(T instance, LoadStateDescriptor<T> original) {
        return new LoadStateDescriptor<>(instance, original);
    }
}
