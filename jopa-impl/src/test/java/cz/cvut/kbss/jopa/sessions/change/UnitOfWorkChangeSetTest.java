/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class UnitOfWorkChangeSetTest {

    private ObjectChangeSet changeSet;
    private String testObject;

    private UnitOfWorkChangeSet uowChangeSet;

    @Before
    public void setUp() {
        this.testObject = "TEST";
        final String testClone = "TEST";
        this.changeSet = ChangeSetFactory.createObjectChangeSet(testObject, testClone, new EntityDescriptor());
        uowChangeSet = new UnitOfWorkChangeSetImpl();
    }

    @Test
    public void testAddObjectChangeSet() {
        uowChangeSet.addObjectChangeSet(changeSet);
        assertEquals(1, uowChangeSet.getExistingObjectsChanges().size());
        ObjectChangeSet res = uowChangeSet.getExistingObjectsChanges().iterator().next();
        assertSame(changeSet, res);
        assertTrue(uowChangeSet.hasChanges());
    }

    /**
     * This tests the fact that if we pass object change set for a new object, the UoWChangeSet should forward the call
     * to the addNewObjectChangeSet
     */
    @Test
    public void testAddObjectChangeSetWithNew() {
        changeSet.setNew(true);
        uowChangeSet.addObjectChangeSet(changeSet);
        assertEquals(1, uowChangeSet.getNewObjects().size());
        ObjectChangeSet res = uowChangeSet.getNewObjects().iterator().next();
        assertSame(changeSet, res);
        assertTrue(uowChangeSet.hasNew());
    }

    @Test
    public void testAddDeletedObject() {
        uowChangeSet.addDeletedObjectChangeSet(changeSet);
        assertEquals(1, uowChangeSet.getDeletedObjects().size());
        ObjectChangeSet res = uowChangeSet.getDeletedObjects().iterator().next();
        Object result = res.getChangedObject();
        assertEquals(testObject, result);
        assertTrue(uowChangeSet.hasDeleted());
        assertTrue(uowChangeSet.hasChanges());
    }

    @Test
    public void testAddNewObjectChangeSet() {
        changeSet.setNew(true);
        uowChangeSet.addNewObjectChangeSet(changeSet);
        assertTrue(uowChangeSet.hasChanges());
        assertEquals(1, uowChangeSet.getNewObjects().size());
        ObjectChangeSet res = uowChangeSet.getNewObjects().iterator().next();
        assertSame(changeSet, res);
        assertTrue(uowChangeSet.hasNew());
    }

    @Test
    public void getExistingObjectChangesReturnsChangeSetForTheSpecifiedOriginal() {
        uowChangeSet.addObjectChangeSet(changeSet);
        final ObjectChangeSet result = uowChangeSet.getExistingObjectChanges(testObject);
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
