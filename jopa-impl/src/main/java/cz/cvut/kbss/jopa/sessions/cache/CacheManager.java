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
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.Cache;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;

import java.util.Set;

/**
 * This interface defines basic methods for accessing the shared live object cache.
 */
public interface CacheManager extends Cache {

    /**
     * Adds the specified object into the shared session cache.
     * <p>
     * If the cache already contains an object with the specified identifier (and it is in the same repository context),
     * it is replaced with the one passed as argument.
     *
     * @param identifier  Identifier of the specified object
     * @param entity      The object to be added into the cache
     * @param descriptors Instance descriptors
     */
    void add(Object identifier, Object entity, Descriptors descriptors);

    /**
     * Gets entity with the specified identifier from the cache.
     * <p>
     * The entity is searched for in the context specified by {@code descriptor}. Thus all three conditions - class,
     * identifier and descriptor must match to return a result.
     *
     * @param cls        Class of the entity
     * @param identifier Primary key of the entity
     * @param descriptor Instance descriptor, contains info about repository context(s) and language tags
     * @return Entity with the specified primary key or {@code null}
     */
    <T> T get(Class<T> cls, Object identifier, Descriptor descriptor);

    /**
     * Gets {@link LoadStateDescriptor} that is associated with the specified cached instance.
     *
     * @param instance Instance whose load state descriptor to retrieve
     * @return Load state descriptor, {@code null} if this cache does not contain the specified instance
     */
    LoadStateDescriptor<?> getLoadStateDescriptor(Object instance);

    /**
     * Removes objects with (possibly) inferred attributes from the cache.
     * <p>
     * This should be called when changes in the ontology may influence inference results.
     */
    void evictInferredObjects();

    /**
     * Set the inferred classes for this cache manager.
     * <p>
     * Entities from inferred classes are special in that when anything in the ontology changes, they have to be evicted
     * from the cache, since they are reasoned and their attributes may change.
     *
     * @param inferredClasses Set of inferred classes
     */
    void setInferredClasses(Set<Class<?>> inferredClasses);

    /**
     * Closes the cache.
     */
    void close();
}
