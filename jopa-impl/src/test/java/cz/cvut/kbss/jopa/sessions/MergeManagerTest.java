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

import cz.cvut.kbss.jopa.api.ObjectChangeSet;
import cz.cvut.kbss.jopa.api.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.UnitOfWorkChangeSetImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.mockito.Mockito.*;

class MergeManagerTest {

    private static final URI DEFAULT_URI = URI.create("http://defaultContext");

    private static Descriptor defaultDescriptor;

    @Mock
    private UnitOfWorkImpl uow;

    @Mock
    private CloneBuilder cloneBuilder;

    @Mock
    private MetamodelImpl metamodel;

    private MetamodelMocks metamodelMocks;

    private UnitOfWorkChangeSet uowChangeSet;

    private MergeManager mm;

    @BeforeAll
    static void setUpBeforeClass() {
        defaultDescriptor = new EntityDescriptor(DEFAULT_URI);
    }

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        this.uowChangeSet = new UnitOfWorkChangeSetImpl();
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.getCloneBuilder()).thenReturn(cloneBuilder);
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        this.mm = new MergeManager(uow);
    }

    @AfterEach
    void tearDown() {
        uow.release();
    }

    @Test
    void mergeChangesOnObjectCallsCloneBuilderWithChangeSetToMerge() {
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
    void testMergeChangesFromChangeSet() {
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
        verify(uow).removeObjectFromCache(objTwo, DEFAULT_URI);
        verify(cloneBuilder).mergeChanges(changeSet);
    }

    @Test
    void mergeChangesFromChangeSetWithNewObjectPutsOriginalIntoCache() {
        final OWLClassB objOne = new OWLClassB(Generators.createIndividualIdentifier());
        objOne.setStringAttribute("ABeautifulAttribute");
        final OWLClassB clone = new OWLClassB(objOne.getUri());
        final ObjectChangeSet changeSet = createChangeSet(objOne, clone);
        uowChangeSet.addNewObjectChangeSet(changeSet);
        mm.mergeChangesFromChangeSet(uowChangeSet);
        verify(uow).putObjectIntoCache(objOne.getUri(), objOne, defaultDescriptor);
    }

    @Test
    void mergeNewObjectPutsObjectIntoCache() {
        final OWLClassB newOne = new OWLClassB(Generators.createIndividualIdentifier());
        final OWLClassB clone = new OWLClassB(newOne.getUri());
        final ObjectChangeSet changeSet = createChangeSet(newOne, clone);
        changeSet.setNew(true);
        mm.mergeNewObject(changeSet);
        verify(uow).putObjectIntoCache(newOne.getUri(), newOne, defaultDescriptor);
    }

    private static ObjectChangeSet createChangeSet(Object orig, Object clone) {
        return ChangeSetFactory.createObjectChangeSet(orig, clone, defaultDescriptor);
    }

    @Test
    void mergeChangesOnObjectPreventsCachingWhenChangeRecordPreventingCachingExists() {
        final OWLClassD original = new OWLClassD(Generators.createIndividualIdentifier());
        final OWLClassD clone = new OWLClassD(original.getUri());
        clone.setOwlClassA(new OWLClassA(Generators.createIndividualIdentifier()));
        final ObjectChangeSet changeSet = createChangeSet(original, clone);
        final ChangeRecordImpl record =
                new ChangeRecordImpl(metamodelMocks.forOwlClassD().owlClassAAtt(), clone.getOwlClassA());
        record.preventCaching();
        changeSet.addChangeRecord(record);
        mm.mergeChangesOnObject(changeSet);
        verify(uow, never()).putObjectIntoCache(original.getUri(), original, defaultDescriptor);
        verify(uow).removeObjectFromCache(original, DEFAULT_URI);
    }
}
