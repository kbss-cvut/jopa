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
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ObjectChangeSetImplTest {

    private static final OWLClassA original = new OWLClassA(URI.create(Vocabulary.c_OwlClassA));

    private static final OWLClassA clone = new OWLClassA(URI.create(Vocabulary.c_OwlClassA));

    private final ObjectChangeSetImpl sut = new ObjectChangeSetImpl(original, clone, new EntityDescriptor());

    @Test
    public void addChangeRecordAddsChangeRecordToChanges() {
        final String attName = "stringAttribute";
        final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
        when(fs.getName()).thenReturn(attName);
        ChangeRecord record = new ChangeRecord(fs, "newValue");
        sut.addChangeRecord(record);
        final Optional<ChangeRecord> result = sut.getChanges().stream().filter(ch -> ch.getAttribute().equals(fs))
                                                 .findAny();

        assertTrue(result.isPresent());
        assertEquals(record.getNewValue(), result.get().getNewValue());
    }

    @Test
    void addChangeRecordOverwritesPreviousChangeRecordOfAttribute() {
        final String attName = "stringAttribute";
        final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
        when(fs.getName()).thenReturn(attName);
        ChangeRecord firstRecord = new ChangeRecord(fs, "firstValue");
        sut.addChangeRecord(firstRecord);
        final ChangeRecord secondRecord = new ChangeRecord(fs, "secondValue");
        sut.addChangeRecord(secondRecord);

        final Optional<ChangeRecord> result = sut.getChanges().stream().filter(ch -> ch.getAttribute().equals(fs))
                                                 .findAny();
        assertTrue(result.isPresent());
        assertEquals(secondRecord.getNewValue(), result.get().getNewValue());
    }
}
