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

import java.net.URI;

/**
 * Interface used to interact with the second-level cache. If a cache is not in
 * use, the methods of this interface have no effect, except for contains, which
 * returns false.
 *
 * Taken from JPA 2.
 *
 * @author kidney
 *
 */
public interface Cache {

    /**
     * Checks whether the cache contains data for the given entity. </p>
     *
     * This method searches all the available contexts and returns true on
     * finding the first occurrence of matching entity.
     *
     * @param cls
     *            entity class
     * @param primaryKey
     *            primary key
     * @return {@code boolean} indicating whether the entity is in the cache
     * @see #contains(Class, Object, URI)
     */
    boolean contains(Class<?> cls, Object primaryKey);

    /**
     * Checks whether the cache contains data for the given entity (in the given
     * context only).
     *
     * @param cls
     *            Entity class
     * @param primaryKey
     *            Primary key
     * @param context
     *            Context URI
     * @return {@code boolean} indicating whether the entity is in the cache
     */
    boolean contains(Class<?> cls, Object primaryKey, URI context);

    /**
     * Removes the data for the given entity from the cache.
     *
     * @param cls
     *            Entity class
     * @param primaryKey
     *            Primary key
     * @param context
     *            Context URI
     */
    void evict(Class<?> cls, Object primaryKey, URI context);

    /**
     * Removes the data for entities of the specified class (and its subclasses)
     * from the cache. </p>
     *
     * This method removes the entities from all available contexts.
     *
     * @param cls
     *            entity class
     */
    void evict(Class<?> cls);

    /**
     * Removes the data for entities of the specified repository context from
     * the cache.
     *
     * @param context
     *            URI of {@code Context}
     */
    void evict(URI context);

    /**
     * Clears the cache.
     */
    void evictAll();
}
