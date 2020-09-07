/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;

import java.util.*;

public final class CollectionFactory {

    private CollectionFactory() {
        throw new AssertionError();
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
    public static <T> Collection<T> createDefaultCollection(PluralAttribute.CollectionType collectionType) {
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
