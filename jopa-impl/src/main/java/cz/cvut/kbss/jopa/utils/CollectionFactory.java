/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectMap;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

import java.lang.reflect.Field;
import java.util.*;

public final class CollectionFactory {

    private final UnitOfWorkImpl uow;

    public CollectionFactory(UnitOfWorkImpl uow) {
        this.uow = uow;
    }

    /**
     * Creates an indirect collection which wraps the specified collection and propagates changes to the current
     * persistence context.
     *
     * @param collection The collection to be proxied
     * @param owner      Collection owner instance
     * @param field      Field whose value the collection is
     * @return Indirect collection wrapping the collection
     */
    public IndirectCollection<?> createIndirectCollection(Object collection, Object owner, Field field) {
        final IndirectCollection<?> res;
        if (collection instanceof List) {
            res = new IndirectList<>(owner, field, uow, (List<?>) collection);
        } else if (collection instanceof Set) {
            res = new IndirectSet<>(owner, field, uow, (Set<?>) collection);
        } else if (collection instanceof Map) {
            res = new IndirectMap<>(owner, field, uow, (Map<?, ?>) collection);
        } else {
            throw new UnsupportedOperationException("Unsupported collection type " + collection.getClass());
        }
        return res;
    }

    /**
     * Creates an instance of a {@link Collection} implementation best matching the specified instance.
     * <p>
     * E.g. for any kind of {@link List}, an {@link ArrayList} is returned.
     *
     * @param collection Create matching instance for this collection
     * @return Best matching collection instance
     */
    public static Collection<?> createInstance(Collection<?> collection) {
        Objects.requireNonNull(collection);

        if (collection instanceof List) {
            return new ArrayList<>(collection.size());
        } else if (collection instanceof Set) {
            return new HashSet<>(collection.size());
        }
        throw new IllegalArgumentException("Unsupported collection type: " + collection);
    }

    /**
     * Creates default collection for the specified collection type.
     *
     * @param collectionType Type of the collection to create
     * @return Collection implementation instance
     */
    public static Collection<Object> createDefaultCollection(PluralAttribute.CollectionType collectionType) {
        switch (collectionType) {
            case LIST:
                return new ArrayList<>();
            case SET:
            case COLLECTION:    // Intentional fall-through
                return new HashSet<>();
            default:
                throw new IllegalArgumentException("Collection type " + collectionType + " is not supported.");
        }
    }

    /**
     * Creates default instance of {@link Map}.
     *
     * @return Default Map implementation instance
     */
    public static Map<Object, Object> createDefaultMap() {
        return new HashMap<>();
    }
}
