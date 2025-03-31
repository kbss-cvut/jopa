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
package cz.cvut.kbss.jopa.proxy.reference;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

public interface EntityReferenceProxy<T> extends EntityReferenceProxyPropertyAccessor<T> {

    default T triggerLoading() {
        if (isLoaded()) {
            return getValue();
        }
        if (getPersistenceContext() == null || !getPersistenceContext().isActive()) {
            throw new LazyLoadingException("No active persistence context is available to load reference of type " + getType() + " with identifier + " + getIdentifier());
        }
        final T loaded = getPersistenceContext().readObject(getType(), getIdentifier(), getDescriptor());
        if (loaded == null) {
            throw new EntityNotFoundException("Entity '" + getType().getSimpleName() + "' with id " + IdentifierTransformer.stringifyIri(getIdentifier()) + " not found in the repository.");
        }
        setValue(loaded);
        return loaded;
    }

    default boolean isLoaded() {
        return getValue() != null;
    }
}
