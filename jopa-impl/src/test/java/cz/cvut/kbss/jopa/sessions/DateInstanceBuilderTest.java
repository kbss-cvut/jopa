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

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

class DateInstanceBuilderTest {

    private OWLClassM entityM;
    private Field dateField;
    private Descriptor descriptor;

    private final DateInstanceBuilder builder = new DateInstanceBuilder(mock(CloneBuilder.class),
            mock(UnitOfWorkImpl.class));

    @BeforeEach
    void setUp() throws Exception {
        entityM = new OWLClassM();
        this.dateField = OWLClassM.getDateAttributeField();
        this.descriptor = new EntityDescriptor();
    }

    @Test
    void testBuildClone() {
        final Date original = new Date();
        final Object res = builder.buildClone(entityM, dateField, original, new CloneConfiguration(descriptor, false));
        assertTrue(res instanceof Date);
        assertNotSame(original, res);
        assertEquals(original, res);
    }

    @Test
    void testBuildCloneOfNull() {
        final Object res = builder.buildClone(entityM, dateField, null, new CloneConfiguration(descriptor, false));
        assertNull(res);
    }

    @Test
    void testMergeChanges() {
        final Date orig = new Date();
        final Date clone = new Date(System.currentTimeMillis() - 100000);
        entityM.setDateAttribute(orig);
        builder.mergeChanges(dateField, entityM, orig, clone);
        assertEquals(clone, entityM.getDateAttribute());
    }
}
