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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Set;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class TypeReferenceMapTest {

    private final TypeReferenceMap sut = new TypeReferenceMap();

    @Test
    void getReferringTypesReturnsEmptySetForUnknownClass() {
        final Set<Class<?>> result = sut.getReferringTypes(String.class);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void addReferenceWorksFirstTimeForType() {
        sut.addReference(OWLClassA.class, OWLClassD.class);
        final Set<Class<?>> result = sut.getReferringTypes(OWLClassA.class);
        assertEquals(Collections.singleton(OWLClassD.class), result);
    }

    @Test
    void addReferenceWorksMultipleTimesForType() {
        sut.addReference(OWLClassA.class, OWLClassD.class);
        sut.addReference(OWLClassA.class, OWLClassC.class);
        final Set<Class<?>> result = sut.getReferringTypes(OWLClassA.class);
        assertEquals(2, result.size());
        assertThat(result, hasItems(OWLClassC.class, OWLClassD.class));
    }
}