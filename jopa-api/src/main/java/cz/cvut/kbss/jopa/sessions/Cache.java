/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.net.URI;

/**
 * Interface used to interact with the second-level cache.
 * <p>
 * If a cache is not in use, the methods of this interface have no effect, except for contains, which
 * returns false.
 * <p>
 * Taken from JPA 2.
 */
public interface Cache {

    /**
     * Checks whether the cache contains data for the given entity.
     *
     * @param cls        Entity class
     * @param identifier Instance identifier
     * @param descriptor Specifies instance context and additional possible information, e.g. language tags
     * @return {@code boolean} indicating whether the entity is in the cache
     */
    boolean contains(Class<?> cls, Object identifier, Descriptor descriptor);

    /**
     * Removes the data for the given entity from the cache.
     *
     * @param cls        Entity class
     * @param identifier Instance identifier
     * @param context    Context URI, possibly {@code null}, meaning the default context
     */
    void evict(Class<?> cls, Object identifier, URI context);

    /**
     * Removes the data for entities of the specified class (and its subclasses) from the cache.
     * <p>
     * This method removes the entities from all available contexts.
     *
     * @param cls entity class
     */
    void evict(Class<?> cls);

    /**
     * Removes the data for entities of the specified repository context from the cache.
     *
     * @param context URI of the context to evict, possibly {@code null}, meaning the default context
     */
    void evict(URI context);

    /**
     * Clears the cache.
     */
    void evictAll();
}
