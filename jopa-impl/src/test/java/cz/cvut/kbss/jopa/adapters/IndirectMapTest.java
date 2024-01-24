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
package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class IndirectMapTest {

    private static OWLClassB owner;
    private static Field ownerField;
    private static Map<String, Set<String>> backupMap;

    @Mock
    private UnitOfWork uow;

    private Map<String, Set<String>> map;
    private IndirectMap<String, Set<String>> sut;

    @BeforeAll
    static void setUpBeforeClass() throws Exception {
        owner = new OWLClassB();
        owner.setUri(Generators.createIndividualIdentifier());
        ownerField = OWLClassB.getPropertiesField();
        backupMap = Generators.generateStringProperties(15, 15);
    }

    @BeforeEach
    void setUp() {
        this.map = new HashMap<>();
        map.putAll(backupMap);
        this.sut = new IndirectMap<>(owner, ownerField, uow, map);

    }

    @Test
    void testConstructorNullUoW() {
        assertThrows(NullPointerException.class, () -> new IndirectMap<>(owner, ownerField, null, map));
    }

    @Test
    void testConstructorNullMap() {
        assertThrows(NullPointerException.class, () -> new IndirectMap<>(owner, ownerField, uow, null));
    }

    @Test
    void testPut() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        final String key = Generators.createPropertyIdentifier().toString();
        final String value = "someDataPropertyValue";
        sut.put(key, Collections.singleton(value));
        verify(uow).attributeChanged(owner, ownerField);
        assertTrue(map.containsKey(key));
    }

    @Test
    void testRemove() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        final String key = map.keySet().iterator().next();
        sut.remove(key);
        verify(uow).attributeChanged(owner, ownerField);
        assertFalse(map.containsKey(key));
    }

    @Test
    void testPutAll() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        final Map<String, Set<String>> newMap = Generators.generateStringProperties();
        sut.putAll(newMap);
        verify(uow).attributeChanged(owner, ownerField);
        for (String key : newMap.keySet()) {
            assertTrue(map.containsKey(key));
        }
    }

    @Test
    void testClear() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        sut.clear();
        verify(uow).attributeChanged(owner, ownerField);
        assertTrue(map.isEmpty());
    }

    @Test
    void equalsWorksForTwoIndirectMaps() {
        final IndirectMap<String, Set<String>> other = new IndirectMap<>(owner, ownerField, uow, map);
        assertEquals(sut, other);
    }

    @Test
    void equalsWorksForIndirectMapAndRegularMap() {
        assertEquals(backupMap, sut);
    }

    @Test
    void equalsReturnsFalseForInEqualMaps() {
        sut.put("added", Collections.singleton("b"));
        assertNotEquals(backupMap, sut);
    }

    @Test
    void hashCodeReturnsHashCodeOfInternalMap() {
        assertEquals(backupMap.hashCode(), sut.hashCode());
    }

    @Test
    void keySetReturnsInternalMapKeySet() {
        assertEquals(backupMap.keySet(), sut.keySet());
    }

    @Test
    void valuesReturnsInternalMapValues() {
        assertEquals(backupMap.values().size(), sut.values().size());
        assertTrue(backupMap.values().containsAll(sut.values()));
    }

    @Test
    void toStringInvokesToStringOfReferencedInstance() {
        assertEquals(map.toString(), sut.toString());
    }
}
