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
package cz.cvut.kbss.jopa.model.metamodel;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.*;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CollectionTypeTest {

    @ParameterizedTest
    @MethodSource("fromClassTestValues")
    void fromClassReturnsMatchingEnumConstant(CollectionType expected, Class<?> cls) {
        assertEquals(expected, CollectionType.fromClass(cls));
    }

    static Stream<Arguments> fromClassTestValues() {
        return Stream.of(
                Arguments.of(CollectionType.LIST, List.class),
                Arguments.of(CollectionType.SET, Set.class),
                Arguments.of(CollectionType.MAP, Map.class),
                Arguments.of(CollectionType.COLLECTION, Collection.class),
                // Test also some implementation classes
                Arguments.of(CollectionType.LIST, ArrayList.class),
                Arguments.of(CollectionType.SET, HashSet.class),
                Arguments.of(CollectionType.MAP, HashMap.class)
        );
    }

    @Test
    void fromClassThrowsIllegalArgumentExceptionForUnsupportedClass() {
        assertThrows(IllegalArgumentException.class, () -> CollectionType.fromClass(String.class));
    }
}
