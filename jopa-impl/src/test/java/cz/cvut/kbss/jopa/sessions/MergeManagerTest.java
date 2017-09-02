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

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.UnitOfWorkChangeSetImpl;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class MergeManagerTest {

    private static final URI DEFAULT_URI = URI.create("http://defaultContext");

    private static Descriptor defaultDescriptor;

    @Mock
    private UnitOfWorkImpl uow;

    @Mock
    private CloneBuilderImpl cloneBuilder;

    @Mock
    private MetamodelImpl metamodel;

    private MetamodelMocks metamodelMocks;

    private UnitOfWorkChangeSet uowChangeSet;

    private MergeManagerImpl mm;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        defaultDescriptor = new EntityDescriptor(DEFAULT_URI);
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.uowChangeSet = new UnitOfWorkChangeSetImpl();
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.getCloneBuilder()).thenReturn(cloneBuilder);
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        this.mm = new MergeManagerImpl(uow);
    }

    @After
    public void tearDown() throws Exception {
        uow.release();
    }

    @Test
    public void mergeChangesOnObjectCallsCloneBuilderWithChangeSetToMerge() throws Exception {
        final OWLClassB orig = new OWLClassB(Generators.createIndividualIdentifier());
        orig.setStringAttribute("ANiceAttribute");
        final OWLClassB clone = new OWLClassB(orig.getUri());
        final ObjectChangeSet chs = createChangeSet(orig, clone);
        clone.setStringAttribute("AnotherStringAttribute");
        chs.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassB().stringAttribute(), clone.getStringAttribute()));
        mm.mergeChangesOnObject(chs);
        verify(cloneBuilder).mergeChanges(chs);
    }

    @Test
    public void testMergeChangesFromChangeSet() throws Exception {
        final OWLClassB objOne = new OWLClassB(Generators.createIndividualIdentifier());
        final OWLClassB objTwo = new OWLClassB(Generators.createIndividualIdentifier());
        OWLClassB cloneOne = new OWLClassB(objOne.getUri());
        OWLClassB cloneTwo = new OWLClassB(objTwo.getUri());
        cloneOne.setStringAttribute("testAtt");
        uowChangeSet.addDeletedObjectChangeSet(createChangeSet(objTwo, cloneTwo));
        final ObjectChangeSet changeSet = createChangeSet(objOne, cloneOne);
        changeSet.addChangeRecord(
                new ChangeRecordImpl(metamodelMocks.forOwlClassB().stringAttribute(), cloneOne.getStringAttribute()));
        uowChangeSet.addObjectChangeSet(changeSet);
        mm.mergeChangesFromChangeSet(uowChangeSet);
        verify(uow).removeObjectFromCache(objTwo, defaultDescriptor.getContext());
        verify(cloneBuilder).mergeChanges(changeSet);
    }

    @Test
    public void mergeChangesFromChangeSetWithNewObjectPutsOriginalIntoCache() {
        final OWLClassB objOne = new OWLClassB(Generators.createIndividualIdentifier());
        objOne.setStringAttribute("ABeautifulAttribute");
        final OWLClassB clone = new OWLClassB(objOne.getUri());
        final ObjectChangeSet changeSet = createChangeSet(objOne, clone);
        uowChangeSet.addNewObjectChangeSet(changeSet);
        mm.mergeChangesFromChangeSet(uowChangeSet);
        verify(uow).putObjectIntoCache(objOne.getUri(), objOne, defaultDescriptor);
    }

    @Test
    public void mergeNewObjectPutsObjectIntoCache() {
        final OWLClassB newOne = new OWLClassB(Generators.createIndividualIdentifier());
        final OWLClassB clone = new OWLClassB(newOne.getUri());
        final ObjectChangeSet ochs = createChangeSet(newOne, clone);
        mm.mergeNewObject(ochs);
        verify(uow).putObjectIntoCache(newOne.getUri(), newOne, defaultDescriptor);
    }

    private static ObjectChangeSet createChangeSet(Object orig, Object clone) {
        return ChangeSetFactory.createObjectChangeSet(orig, clone, defaultDescriptor);
    }
}
