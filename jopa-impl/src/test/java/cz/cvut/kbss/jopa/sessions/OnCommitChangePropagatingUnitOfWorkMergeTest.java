/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.NoopInstantiableTypeGenerator;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class OnCommitChangePropagatingUnitOfWorkMergeTest extends UnitOfWorkMergeTestRunner {

    @BeforeAll
    static void setUpBeforeAll() {
        MetamodelFactory.setInstantiableTypeGenerator(NoopInstantiableTypeGenerator.INSTANCE);
    }

    @BeforeEach
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new OnCommitChangePropagatingUnitOfWork(serverSessionStub, new Configuration());
    }

    @AfterAll
    static void tearDownAfterAll() {
        MetamodelFactory.reset();
    }

    @Test
    void testMergeDetachedExisting() {
        final OWLClassA result = mergeDetached();
        assertNotNull(result);
        assertEquals(entityA.getUri(), result.getUri());
        assertEquals(entityA.getStringAttribute(), result.getStringAttribute());
        final ObjectChangeSet changeSet = uow.uowChangeSet.getExistingObjectChanges(uow.getOriginal(result));
        assertNotNull(changeSet);
        assertTrue(changeSet.getChanges().stream()
                            .anyMatch(r -> r.getAttribute().equals(metamodelMocks.forOwlClassA().stringAttribute())));
    }
}
