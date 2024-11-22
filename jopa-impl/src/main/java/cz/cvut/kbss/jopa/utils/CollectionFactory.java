/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.metamodel.CollectionType;

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
    public static <T> Collection<T> createDefaultCollection(CollectionType collectionType) {
        return switch (collectionType) {
            case LIST -> new ArrayList<>();
            case SET, COLLECTION -> new HashSet<>();
            default -> throw new IllegalArgumentException("Collection type " + collectionType + " is not supported.");
        };
    }

    /**
     * Creates a default collection for the specified collection type in the context of Queries.
     * This differs from {@link CollectionFactory#createDefaultCollection}, because SPARQL results are treated as lists,
     * so Collections should be handled as Lists, not as Sets
     *
     * @param collectionType Type of the collection to create
     * @return Collection implementation instance
     */
    public static <T> Collection<T> createDefaultQueryCollection(CollectionType collectionType) {
        if(collectionType.equals(CollectionType.COLLECTION)) {
            collectionType = CollectionType.LIST;
        }
        return createDefaultCollection(collectionType);
    }

    /**
     * Creates default instance of {@link Map}.
     *
     * @return Default Map implementation instance
     */
    public static Map<Object, Object> createDefaultMap() {
        return new HashMap<>();
    }

    /**
     * Resolves {@link CollectionType} from the specified Java class.
     *
     * @param javaType Java type whose collection type to resolve
     * @return {@code CollectionType} value
     * @throws IllegalArgumentException When unsupported java type is provided
     */
    public static CollectionType resolveCollectionType(Class<?> javaType) {
        if (List.class.isAssignableFrom(javaType)) {
            return CollectionType.LIST;
        } else if (Set.class.isAssignableFrom(javaType)) {
            return CollectionType.SET;
        } else if (Collection.class.isAssignableFrom(javaType)) {
            return CollectionType.COLLECTION;
        } else if (Map.class.isAssignableFrom(javaType)) {
            return CollectionType.MAP;
        } else {
            throw new IllegalArgumentException();
        }
    }
}
