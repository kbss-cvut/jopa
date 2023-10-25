/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.Set;

/**
 * This interface defines basic methods for accessing the shared live object cache.
 */
public interface CacheManager extends Cache {

    /**
     * Adds the specified object into the shared session cache.
     * <p>
     * If the cache already contains object with the specified identifier (and it is in the same repository context),
     * it is replaced with the one passed as argument.
     *
     * @param identifier Identifier of the specified object
     * @param entity     The object to be added into the cache
     * @param descriptor    Instance descriptor, contains info about repository context(s) and language tags
     */
    void add(Object identifier, Object entity, Descriptor descriptor);

    /**
     * Gets entity with the specified identifier from the cache.
     * <p>
     * The entity is searched for in the context specified by {@code descriptor}. Thus all three conditions - class,
     * identifier and descriptor must match to return a result.
     *
     * @param cls        Class of the entity
     * @param identifier Primary key of the entity
     * @param descriptor    Instance descriptor, contains info about repository context(s) and language tags
     * @return Entity with the specified primary key or {@code null}
     */
    <T> T get(Class<T> cls, Object identifier, Descriptor descriptor);

    /**
     * Removes objects with (possibly) inferred attributes from the cache.
     *
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
