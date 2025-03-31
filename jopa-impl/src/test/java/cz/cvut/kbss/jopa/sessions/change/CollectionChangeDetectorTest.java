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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class CollectionChangeDetectorTest {

    @Mock
    private MetamodelProvider providerMock;

    @Mock
    private MetamodelImpl metamodel;

    private CollectionChangeDetector changeDetector;

    @BeforeEach
    public void setUp() throws Exception {
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodel);
        when(providerMock.getMetamodel()).thenReturn(metamodel);
        this.changeDetector = new CollectionChangeDetector(new ChangeDetectors(providerMock), providerMock);
    }

    @Test
    public void hasChangesReturnsFalseForTwoSetsOfManagedClassInstances() {
        final Set<OWLClassA> original = initSet();
        final Set<OWLClassA> clone = initSet();
        when(providerMock.isEntityType(OWLClassA.class)).thenReturn(true);
        assertFalse(changeDetector.hasChanges(clone, original));
    }

    private Set<OWLClassA> initSet() {
        final List<OWLClassA> original = new ArrayList<>();
        final OWLClassA a1 = new OWLClassA(
                URI.create("http://onto.fel.cvut.cz/ontologies/documentation/question#instance-1523798300"));
        a1.setStringAttribute("one");
        original.add(a1);
        final OWLClassA a2 = new OWLClassA(
                URI.create("http://onto.fel.cvut.cz/ontologies/documentation/question#instance1483024927"));
        a2.setStringAttribute("two");
        original.add(a2);
        final OWLClassA a3 = new OWLClassA(
                URI.create("http://onto.fel.cvut.cz/ontologies/documentation/question#instance-2120907524"));
        a3.setStringAttribute("three");
        original.add(a3);
        Collections.shuffle(original);
        return new HashSet<>(original);
    }

    @Test
    public void hasChangesReturnsFalseWhenCollectionOfManagedTypesHasAttributeValueUpdate() {
        final Set<OWLClassA> original = initSet();
        final Set<OWLClassA> clone = initSet();
        when(providerMock.isEntityType(OWLClassA.class)).thenReturn(true);
        clone.iterator().next().setStringAttribute("updated");
        assertFalse(changeDetector.hasChanges(clone, original));
    }

    @Test
    public void hasChangesReturnsTrueWhenItemIsReplacedInCollectionOfManagedTypes() {
        final Set<OWLClassA> original = initSet();
        final Set<OWLClassA> clone = initSet();
        when(providerMock.isEntityType(OWLClassA.class)).thenReturn(true);
        final OWLClassA newItem = new OWLClassA(Generators.createIndividualIdentifier());
        removeItemFromCollection(clone);
        clone.add(newItem);
        assertTrue(changeDetector.hasChanges(clone, original));
    }

    private void removeItemFromCollection(Set<OWLClassA> clone) {
        final Iterator<OWLClassA> it = clone.iterator();
        it.next();
        it.remove();
    }

    @Test
    public void hasChangesReturnsTrueWhenItemIsReplacedByNewObjectInCollectionOfManagedTypes() {
        final Set<OWLClassA> original = initSet();
        final Set<OWLClassA> clone = initSet();
        when(providerMock.isEntityType(OWLClassA.class)).thenReturn(true);
        final OWLClassA newItem = new OWLClassA();
        removeItemFromCollection(clone);
        clone.add(newItem);
        assertTrue(changeDetector.hasChanges(clone, original));
    }

    @Test
    public void hasChangesReturnsFalseForTwoEmptyCollections() {
        final Set<String> original = new HashSet<>();
        final Set<String> clone = new HashSet<>();
        assertFalse(changeDetector.hasChanges(clone, original));
    }

    @Test
    public void hasChangesReturnsFalseForTwoIdenticalCollectionsOfNonManagedTypes() {
        final List<Integer> lst = Arrays
                .asList(Generators.randomInt(100), Generators.randomInt(200), Generators.randomInt(500),
                        Generators.randomInt(400), Generators.randomInt(300));
        final Set<Integer> original = new HashSet<>(lst);
        final Set<Integer> clone = new HashSet<>(lst);
        assertFalse(changeDetector.hasChanges(clone, original));
    }

    @Test
    public void hasChangesReturnsTrueWhenCollectionSizeChanges() {
        final Set<OWLClassA> original = initSet();
        final Set<OWLClassA> clone = initSet();
        removeItemFromCollection(clone);
        assertTrue(changeDetector.hasChanges(clone, original));
    }

    @Test
    public void hasChangesReturnsTrueWhenOrderedCollectionOfNonManagedInstancesChanges() {
        final List<String> original = new ArrayList<>();
        final List<String> clone = new ArrayList<>();
        final int cnt = Generators.randomPositiveInt(117);
        for (int i = 0; i < cnt; i++) {
            original.add(Generators.createIndividualIdentifier().toString());
            clone.add(Generators.createIndividualIdentifier().toString());
        }
        assertTrue(changeDetector.hasChanges(clone, original));
    }
}
