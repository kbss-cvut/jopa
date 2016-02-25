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

import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;

import static org.junit.Assert.*;

public class ObjectChangeSetTest {

    private static final URI CONTEXT = URI.create("http://example.org/uri");

    private String testObject;
    private String testClone;

    @Before
    public void setUp() throws Exception {
        this.testObject = "TEST";
        this.testClone = "TEST";
    }

    @Test
    public void testObjectChangeSetImpl() {
        ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
        assertEquals(this.testObject.getClass(), chs.getObjectClass());
    }

    @Test
    public void testAddChangeRecord() {
        final String attName = "testAtt";
        ChangeRecord record = new ChangeRecordImpl(attName, testObject);
        ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
        chs.addChangeRecord(record);
        assertNotNull(chs.getChanges().get(attName));
        Object res = chs.getChanges().get(attName).getNewValue();
        assertEquals(testObject, res);
    }

    @Test
    public void testGetAttributesToChange() {
        ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
        assertNotNull(chs.getChanges());
    }

    @Test
    public void testGetObjectClass() {
        ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
        assertEquals(testObject.getClass(), chs.getObjectClass());
    }

    @Test
    public void testGetChangedObject() {
        ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
        assertEquals(testObject, chs.getChangedObject());
    }

    @Test
    public void testGetCloneObject() {
        ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
        assertEquals(testClone, chs.getCloneObject());
    }

    @Test
    public void testSetCloneObject() {
        ObjectChangeSetImpl chs = new ObjectChangeSetImpl(testObject, testClone, CONTEXT);
        String newClone = "newClone";
        chs.setCloneObject(newClone);
        assertNotSame(testClone, chs.getCloneObject());
    }
}
