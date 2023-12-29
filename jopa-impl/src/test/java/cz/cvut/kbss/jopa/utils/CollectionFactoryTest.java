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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class CollectionFactoryTest {


    @ParameterizedTest
    @MethodSource("collectionTypeMapping")
    void createDefaultCollectionReturnsCorrectTypeInstance(CollectionType collectionType, Class<?> expected) {
        assertInstanceOf(expected, CollectionFactory.createDefaultCollection(collectionType));
    }

    static Stream<Arguments> collectionTypeMapping() {
        return Stream.of(
                Arguments.of(CollectionType.LIST, List.class),
                Arguments.of(CollectionType.SET, Set.class),
                Arguments.of(CollectionType.COLLECTION, Set.class)
        );
    }

    @Test
    void createDefaultCollectionThrowsIllegalArgumentForMapType() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> CollectionFactory.createDefaultCollection(CollectionType.MAP));
        assertEquals("Collection type " + CollectionType.MAP + " is not supported.", ex.getMessage());
    }

    @ParameterizedTest
    @MethodSource("resolvedCollectionTypes")
    void resolveCollectionTypeReturnsCorrectCollectionType(CollectionType expected, Class<?> cls) {
        assertEquals(expected, CollectionFactory.resolveCollectionType(cls));
    }

    static Stream<Arguments> resolvedCollectionTypes() {
        return Stream.of(
                Arguments.of(CollectionType.LIST, List.class),
                Arguments.of(CollectionType.SET, Set.class),
                Arguments.of(CollectionType.COLLECTION, Collection.class)
        );
    }

    @Test
    void resolveCollectionTypeReturnsMapTypeForMapClass() {
        assertEquals(CollectionType.MAP, CollectionFactory.resolveCollectionType(Map.class));
    }

    @Test
    void resolveCollectionTypeThrowsIllegalArgumentExceptionForUnsupportedType() {
        assertThrows(IllegalArgumentException.class, () -> CollectionFactory.resolveCollectionType(String.class));
    }
}
