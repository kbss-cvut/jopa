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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class MapInstanceBuilderTest {

    @Mock
    private AbstractUnitOfWork uow;

    private Descriptor descriptor;

    private MapInstanceBuilder sut;

    @BeforeEach
    public void setUp() {
        CloneBuilder cloneBuilder = new CloneBuilder(uow);
        this.sut = new MapInstanceBuilder(cloneBuilder, uow);
        this.descriptor = new EntityDescriptor();
    }

    @Test
    void buildCloneOfSingletonMapReturnsSingletonMap() throws Exception {
        Map<String, String> map = Collections.singletonMap("key", "value");
        Map<String, String> clone = (Map<String, String>) sut.buildClone(new OWLClassB(), OWLClassB.getPropertiesField(), map, new CloneConfiguration(descriptor, false));
        assertEquals(map, clone);
        assertNotSame(map, clone);
    }

    @Test
    void buildCloneOfMapOfOneReturnsSingletonMap() throws Exception {
        Map<String, String> map = Map.of("key", "value");
        Map<String, String> clone = (Map<String, String>) sut.buildClone(new OWLClassB(), OWLClassB.getPropertiesField(), map, new CloneConfiguration(descriptor, false));
        assertEquals(map, clone);
        assertNotSame(map, clone);
    }

    @Test
    void buildCloneOfEmptyMapReturnsEmptyMap() throws Exception {
        Map<String, String> map = Map.of();
        Map<String, String> clone = (Map<String, String>) sut.buildClone(new OWLClassB(), OWLClassB.getPropertiesField(), map, new CloneConfiguration(descriptor, false));
        assertEquals(map, clone);
        assertNotSame(map, clone);
    }

    @Test
    void buildCloneOfMapOfNReturnsDefaultMap() throws Exception {
        final Map<String, String> map = Map.of("key1", "value1", "key2", "value2");
        final Map<String, String> clone = (Map<String, String>) sut.buildClone(new OWLClassB(), OWLClassB.getPropertiesField(), map, new CloneConfiguration(descriptor, false));
        assertEquals(map, clone);
        assertNotSame(map, clone);
    }
}
