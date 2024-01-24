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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.adapters.IndirectList;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class RefreshInstanceMergerTest {

    @Mock
    private UnitOfWork uowMock;

    private RefreshInstanceMerger sut;

    @BeforeEach
    public void setUp() {
        this.sut = new RefreshInstanceMerger(new IndirectWrapperHelper(uowMock));
    }

    @Test
    public void mergeChangesOverwritesSingularAttributeChangesWithSourceValues() throws Exception {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA clone = new OWLClassA(original.getUri());
        clone.setStringAttribute("changedString");
        clone.setTypes(new HashSet<>(original.getTypes()));
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(original, clone, new EntityDescriptor());
        final FieldSpecification<?, ?> fieldSpec = mock(FieldSpecification.class);
        when(fieldSpec.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        changeSet.addChangeRecord(new ChangeRecord(fieldSpec, clone.getStringAttribute()));

        sut.mergeChanges(changeSet);
        assertEquals(original.getStringAttribute(), clone.getStringAttribute());
    }

    @Test
    public void mergeChangesReplacesCollectionWithNewOneWithSourceValues() throws Exception {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        original.setTypes(new IndirectSet<>(original, OWLClassA.getTypesField(), uowMock, original.getTypes()));
        final OWLClassA clone = new OWLClassA(original.getUri());
        clone.setTypes(new HashSet<>(original.getTypes()));
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(original, clone, new EntityDescriptor());
        final TypesSpecification<?, ?> fieldSpec = mock(TypesSpecification.class);
        when(fieldSpec.getJavaField()).thenReturn(OWLClassA.getTypesField());
        changeSet.addChangeRecord(new ChangeRecord(fieldSpec, clone.getTypes()));

        sut.mergeChanges(changeSet);
        assertTrue(clone.getTypes() instanceof IndirectSet);
        assertEquals(original.getTypes(), clone.getTypes());
        final Field ownerField = IndirectCollection.class.getDeclaredField("owner");
        ownerField.setAccessible(true);
        assertEquals(clone, ownerField.get(clone.getTypes()));
    }

    @Test
    public void mergeChangesReplacesObjectPropertyCollectionWithSourceValues() throws Exception {
        final OWLClassC original = new OWLClassC(Generators.createIndividualIdentifier());
        final OWLClassC clone = new OWLClassC(original.getUri());
        final List<OWLClassA> refList = IntStream.range(0, 5).mapToObj(i -> Generators.generateOwlClassAInstance())
                .collect(Collectors.toList());
        final List<OWLClassA> refListClone = new ArrayList<>(refList);
        original.setReferencedList(new IndirectList<>(original, OWLClassC.getRefListField(), uowMock, refList));
        clone.setReferencedList(new IndirectList<>(clone, OWLClassC.getRefListField(), uowMock, refListClone));
        clone.getReferencedList().add(Generators.generateOwlClassAInstance());
        final Attribute<?, ?> att = mock(Attribute.class);
        when(att.getJavaField()).thenReturn(OWLClassC.getRefListField());
        final ObjectChangeSet changeSet = ChangeSetFactory.createObjectChangeSet(original, clone, new EntityDescriptor());
        changeSet.addChangeRecord(new ChangeRecord(att, refListClone));

        sut.mergeChanges(changeSet);
        assertEquals(refList.size(), clone.getReferencedList().size());
        assertTrue(clone.getReferencedList() instanceof IndirectList);
        final Field ownerField = IndirectCollection.class.getDeclaredField("owner");
        ownerField.setAccessible(true);
        assertEquals(clone, ownerField.get(clone.getReferencedList()));
    }
}
