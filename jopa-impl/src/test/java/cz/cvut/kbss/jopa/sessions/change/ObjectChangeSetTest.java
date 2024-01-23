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

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.api.ChangeRecord;
import cz.cvut.kbss.jopa.api.ObjectChangeSet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ObjectChangeSetTest {

    private String testObject;
    private String testClone;

    @BeforeEach
    public void setUp() {
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
