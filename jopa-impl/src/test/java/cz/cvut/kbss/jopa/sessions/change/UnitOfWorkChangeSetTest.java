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

    private UnitOfWorkChangeSet chs;

    @Before
    public void setUp() throws Exception {
        this.testObject = "TEST";
        final String testClone = "TEST";
        this.changeSet = ChangeSetFactory.createObjectChangeSet(testObject, testClone, new EntityDescriptor());
        chs = new UnitOfWorkChangeSetImpl();
    }

    @Test
    public void testAddObjectChangeSet() {
        chs.addObjectChangeSet(changeSet);
        assertEquals(1, chs.getExistingObjectsChanges().size());
        ObjectChangeSet res = chs.getExistingObjectsChanges().iterator().next();
        assertSame(changeSet, res);
        assertTrue(chs.hasChanges());
    }

    /**
     * This tests the fact that if we pass object change set for a new object, the UoWChangeSet should forward the call
     * to the addNewObjectChangeSet
     */
    @Test
    public void testAddObjectChangeSetWithNew() {
        changeSet.setNew(true);
        chs.addObjectChangeSet(changeSet);
        assertEquals(1, chs.getNewObjects().size());
        ObjectChangeSet res = chs.getNewObjects().iterator().next();
        assertSame(changeSet, res);
        assertTrue(chs.hasNew());
    }

    @Test
    public void testAddDeletedObject() {
        chs.addDeletedObjectChangeSet(changeSet);
        assertEquals(1, chs.getDeletedObjects().size());
        ObjectChangeSet res = chs.getDeletedObjects().iterator().next();
        Object result = res.getChangedObject();
        assertEquals(testObject, result);
        assertTrue(chs.hasDeleted());
        assertTrue(chs.hasChanges());
    }

    @Test
    public void testAddNewObjectChangeSet() {
        changeSet.setNew(true);
        chs.addNewObjectChangeSet(changeSet);
        assertTrue(chs.hasChanges());
        assertEquals(1, chs.getNewObjects().size());
        ObjectChangeSet res = chs.getNewObjects().iterator().next();
        assertSame(changeSet, res);
        assertTrue(chs.hasNew());
    }
}
