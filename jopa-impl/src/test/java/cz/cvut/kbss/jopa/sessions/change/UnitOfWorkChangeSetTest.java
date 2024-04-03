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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashSet;

import static org.junit.jupiter.api.Assertions.*;

public class UnitOfWorkChangeSetTest {

    private ObjectChangeSet changeSet;
    private String testObject;

    private UnitOfWorkChangeSet uowChangeSet;

    @BeforeEach
    public void setUp() {
        this.testObject = "TEST";
        final String testClone = "TEST";
        this.changeSet = ChangeSetFactory.createObjectChangeSet(testObject, testClone, new EntityDescriptor());
        uowChangeSet = new UnitOfWorkChangeSet();
    }

    @Test
    public void testAddObjectChangeSet() {
        uowChangeSet.addObjectChangeSet(changeSet);
        assertEquals(1, uowChangeSet.getExistingObjectsChanges().size());
        Change res = uowChangeSet.getExistingObjectsChanges().iterator().next();
        assertSame(changeSet, res);
        assertTrue(uowChangeSet.hasChanges());
    }

    @Test
    public void testAddDeletedObject() {
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA clone = new OWLClassA(original.getUri());
        clone.setStringAttribute(original.getStringAttribute());
        clone.setTypes(new HashSet<>(original.getTypes()));
        final DeleteObjectChange deleteChange = ChangeSetFactory.createDeleteObjectChange(clone, original, new EntityDescriptor());
        uowChangeSet.addDeletedObjectChangeSet(deleteChange);
        assertEquals(1, uowChangeSet.getDeletedObjects().size());
        DeleteObjectChange res = uowChangeSet.getDeletedObjects().iterator().next();
        Object result = res.getOriginal();
        assertEquals(original, result);
        assertTrue(uowChangeSet.hasDeleted());
        assertTrue(uowChangeSet.hasChanges());
    }

    @Test
    public void testAddNewObjectChangeSet() {
        final NewObjectChange change = ChangeSetFactory.createNewObjectChange(testObject, new EntityDescriptor());
        uowChangeSet.addNewObjectChangeSet(change);
        assertTrue(uowChangeSet.hasChanges());
        assertEquals(1, uowChangeSet.getNewObjects().size());
        Change res = uowChangeSet.getNewObjects().iterator().next();
        assertSame(change, res);
        assertTrue(uowChangeSet.hasNew());
    }

    @Test
    public void getExistingObjectChangesReturnsChangeSetForTheSpecifiedOriginal() {
        uowChangeSet.addObjectChangeSet(changeSet);
        final Change result = uowChangeSet.getExistingObjectChanges(testObject);
        assertNotNull(result);
        assertSame(changeSet, result);
    }

    @Test
    public void cancelObjectChangesRemovesObjectChangeSet() {
        uowChangeSet.addObjectChangeSet(changeSet);
        assertNotNull(uowChangeSet.getExistingObjectChanges(testObject));
        uowChangeSet.cancelObjectChanges(testObject);
        assertNull(uowChangeSet.getExistingObjectChanges(testObject));
    }
}
