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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DetachedInstanceMergerTest {

    private final Descriptor descriptor = new EntityDescriptor();

    @Mock
    private UnitOfWork uow;

    private MetamodelMocks metamodelMocks;

    private DetachedInstanceMerger sut;

    @BeforeEach
    public void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        metamodelMocks.setMocks(metamodel);
        when(uow.getMetamodel()).thenReturn(metamodel);

        this.sut = new DetachedInstanceMerger(uow);
    }

    @Test
    public void mergeFromDetachedAssignsValuesOfNonManagedTypes() {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA detached = new OWLClassA(original.getUri());
        detached.setTypes(original.getTypes());
        final String updatedString = "updatedString";
        detached.setStringAttribute(updatedString);
        final ObjectChangeSet changeSet = createChangeSet(original, detached);
        changeSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassA().stringAttribute(), updatedString));

        final OWLClassA result = (OWLClassA) sut.mergeChangesFromDetachedToManagedInstance(changeSet, descriptor);
        assertEquals(updatedString, result.getStringAttribute());
    }

    @Test
    public void mergeFromDetachedSetsValueToNullWhenChangeValueIsNull() {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA detached = new OWLClassA(original.getUri());
        detached.setTypes(original.getTypes());
        final ObjectChangeSet changeSet = createChangeSet(original, detached);
        changeSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassA().stringAttribute(), null));

        final OWLClassA result = (OWLClassA) sut.mergeChangesFromDetachedToManagedInstance(changeSet, descriptor);
        assertNull(result.getStringAttribute());
    }

    @Test
    public void mergeFromDetachedLoadsExistingInstanceCorrespondingToNewValue() {
        final OWLClassD orig = new OWLClassD(Generators.createIndividualIdentifier());
        final OWLClassA origRef = Generators.generateOwlClassAInstance();
        orig.setOwlClassA(origRef);
        final OWLClassD clone = new OWLClassD(orig.getUri());
        final OWLClassA newRef = Generators.generateOwlClassAInstance();
        final OWLClassA newRefOrig = new OWLClassA(newRef.getUri());
        newRefOrig.setStringAttribute(newRef.getStringAttribute());
        newRefOrig.setTypes(new HashSet<>(newRef.getTypes()));
        final ObjectChangeSet chSet = createChangeSet(orig, clone);
        chSet.addChangeRecord(new ChangeRecord(metamodelMocks.forOwlClassD().owlClassAAtt(), newRef));
        when(uow.readObject(OWLClassA.class, newRef.getUri(), descriptor)).thenReturn(newRefOrig);
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);

        final OWLClassD result = (OWLClassD) sut.mergeChangesFromDetachedToManagedInstance(chSet, descriptor);
        verify(uow).readObject(OWLClassA.class, newRef.getUri(), descriptor);
        assertNotNull(result);
        assertSame(newRefOrig, result.getOwlClassA());
    }

    private ObjectChangeSet createChangeSet(Object original, Object clone) {
        return ChangeSetFactory.createObjectChangeSet(original, clone, new EntityDescriptor());
    }
}
