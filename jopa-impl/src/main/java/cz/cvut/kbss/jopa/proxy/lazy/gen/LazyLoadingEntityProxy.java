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
package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;

/**
 * Implemented by generated lazy loading entity proxy classes.
 * <p>
 * It defines the default method for triggering lazy loading.
 *
 * @param <T> Type of the lazily-loaded value
 */
public interface LazyLoadingEntityProxy<T> extends LazyLoadingProxyPropertyAccessor<T>, LazyLoadingProxy<T> {

    @Override
    default T triggerLazyLoading() {
        if (isLoaded()) {
            return getLoadedValue();
        }
        if (getPersistenceContext() == null || !getPersistenceContext().isActive()) {
            throw new LazyLoadingException("No active persistence context is available in lazy loading proxy for attribute "
                    + getFieldSpec() + " of entity " + getOwner());
        }
        setValue((T) getPersistenceContext().loadEntityField(getOwner(), (FieldSpecification<? super Object, ?>) getFieldSpec()));
        return getLoadedValue();
    }

    /**
     * Gets the entity class for which this is a lazy loading proxy.
     *
     * @return Proxied entity class
     */
    default Class<T> getProxiedClass() {
        assert getClass().getSuperclass() != null;
        return (Class<T>) getClass().getSuperclass();
    }

    /**
     * Common implementation of {@link Object#toString()}.
     *
     * @return String representation of this proxy
     */
    default String stringify() {
        return getClass().getSimpleName() + "[" + getOwner().getClass()
                                                            .getSimpleName() + "." + getFieldSpec().getName() + "]";
    }
}
