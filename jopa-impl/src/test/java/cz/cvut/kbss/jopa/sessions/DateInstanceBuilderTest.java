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

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.Date;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;

/**
 * @author ledvima1
 */
public class DateInstanceBuilderTest {

    private OWLClassM entityM;
    private Field dateField;
    private Descriptor descriptor;

    private DateInstanceBuilder builder = new DateInstanceBuilder(mock(CloneBuilderImpl.class), mock(UnitOfWork.class));

    @Before
    public void setUp() throws Exception {
        entityM = new OWLClassM();
        this.dateField = OWLClassM.getDateAttributeField();
        this.descriptor = new EntityDescriptor();
    }

    @Test
    public void testBuildClone() throws Exception {
        final Date original = new Date();
        final Object res = builder.buildClone(entityM, dateField, original, new CloneConfiguration(descriptor));
        assertTrue(res instanceof Date);
        assertNotSame(original, res);
        assertEquals(original, res);
    }

    @Test
    public void testBuildCloneOfNull() throws Exception {
        final Object res = builder.buildClone(entityM, dateField, null, new CloneConfiguration(descriptor));
        assertNull(res);
    }

    @Test
    public void testMergeChanges() throws Exception {
        final Date orig = new Date();
        final Date clone = new Date(System.currentTimeMillis() - 100000);
        entityM.setDateAttribute(orig);
        builder.mergeChanges(dateField, entityM, orig, clone);
        assertEquals(clone, entityM.getDateAttribute());
    }
}