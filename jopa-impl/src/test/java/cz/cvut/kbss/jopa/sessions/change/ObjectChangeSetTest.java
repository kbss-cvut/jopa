/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import org.junit.Before;
import org.junit.Test;

import java.util.Optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ObjectChangeSetTest {

    private String testObject;
    private String testClone;

    @Before
    public void setUp() throws Exception {
        this.testObject = "TEST";
        this.testClone = "TEST";
    }

    @Test
    public void testAddChangeRecord() {
        final String attName = "testAtt";
        final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
        when(fs.getName()).thenReturn(attName);
        ChangeRecord record = new ChangeRecordImpl(fs, testObject);
        ObjectChangeSet chs = new ObjectChangeSetImpl(testObject, testClone, new EntityDescriptor());
        chs.addChangeRecord(record);
        final Optional<ChangeRecord> result = chs.getChanges().stream().filter(ch -> ch.getAttribute().equals(fs))
                                                 .findAny();
        assertTrue(result.isPresent());
        assertEquals(testObject, result.get().getNewValue());
    }
}
