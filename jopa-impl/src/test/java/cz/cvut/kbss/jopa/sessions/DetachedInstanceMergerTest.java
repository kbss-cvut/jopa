/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSetImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.HashSet;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DetachedInstanceMergerTest {

    private Descriptor descriptor;

    @Mock
    private UnitOfWorkImpl uow;

    @Mock
    private MetamodelImpl metamodel;

    private MetamodelMocks metamodelMocks;

    private DetachedInstanceMerger merger;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        when(uow.getMetamodel()).thenReturn(metamodel);
        this.descriptor = new EntityDescriptor();

        this.merger = new DetachedInstanceMerger(uow);
    }

    @Test
    public void mergeFromDetachedAssignsValuesOfNonManagedTypes() throws Exception {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA detached = new OWLClassA(original.getUri());
        detached.setTypes(original.getTypes());
        final String updatedString = "updatedString";
        detached.setStringAttribute(updatedString);
        final ObjectChangeSet changeSet = createChangeSet(original, detached);
        changeSet.addChangeRecord(new ChangeRecordImpl(metamodelMocks.forOwlClassA().stringAttribute(), updatedString));

        final OWLClassA result = (OWLClassA) merger.mergeChangesFromDetachedToManagedInstance(changeSet, descriptor);
        assertEquals(updatedString, result.getStringAttribute());
    }

    @Test
    public void mergeFromDetachedSetsValueToNullWhenChangeValueIsNull() throws Exception {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA detached = new OWLClassA(original.getUri());
        detached.setTypes(original.getTypes());
        final ObjectChangeSet changeSet = createChangeSet(original, detached);
        changeSet.addChangeRecord(new ChangeRecordImpl(metamodelMocks.forOwlClassA().stringAttribute(), null));

        final OWLClassA result = (OWLClassA) merger.mergeChangesFromDetachedToManagedInstance(changeSet, descriptor);
        assertNull(result.getStringAttribute());
    }

    @Test
    public void mergeFromDetachedLoadsExistingInstanceCorrespondingToNewValue() throws Exception {
        final OWLClassD orig = new OWLClassD(Generators.createIndividualIdentifier());
        final OWLClassA origRef = Generators.generateOwlClassAInstance();
        orig.setOwlClassA(origRef);
        final OWLClassD clone = new OWLClassD(orig.getUri());
        final OWLClassA newRef = Generators.generateOwlClassAInstance();
        final OWLClassA newRefOrig = new OWLClassA(newRef.getUri());
        newRefOrig.setStringAttribute(newRef.getStringAttribute());
        newRefOrig.setTypes(new HashSet<>(newRef.getTypes()));
        final ObjectChangeSet chSet = createChangeSet(orig, clone);
        chSet.addChangeRecord(new ChangeRecordImpl(metamodelMocks.forOwlClassD().owlClassAAtt(), newRef));
        when(uow.readObject(OWLClassA.class, newRef.getUri(), descriptor)).thenReturn(newRefOrig);
        when(uow.isEntityType(OWLClassA.class)).thenReturn(true);

        final OWLClassD result = (OWLClassD) merger.mergeChangesFromDetachedToManagedInstance(chSet, descriptor);
        verify(uow).readObject(OWLClassA.class, newRef.getUri(), descriptor);
        assertNotNull(result);
        assertSame(newRefOrig, result.getOwlClassA());
    }

    private ObjectChangeSet createChangeSet(Object original, Object clone) {
        return new ObjectChangeSetImpl(original, clone, new EntityDescriptor());
    }
}