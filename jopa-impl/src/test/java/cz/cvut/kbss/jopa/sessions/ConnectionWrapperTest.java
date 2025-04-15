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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.oom.ObjectOntologyMapper;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

class ConnectionWrapperTest {

    @Mock
    private Connection connectionMock;
    @Mock
    private ObjectOntologyMapper oomMock;

    private ConnectionWrapper connectionWrapper;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        this.connectionWrapper = new ConnectionWrapper(connectionMock);
        final Field oomField = ConnectionWrapper.class.getDeclaredField("mapper");
        oomField.setAccessible(true);
        oomField.set(connectionWrapper, oomMock);
    }

    @Test
    void containsReturnsFalseForNullIdentifier() {
        final boolean res = connectionWrapper.contains(null, OWLClassA.class, new EntityDescriptor());
        assertFalse(res);
        verify(oomMock, never()).containsEntity(any(), any(), any());
    }

    @Test
    void unwrapCallIsForwardedToConnection() throws Exception {
        connectionWrapper.unwrap(Object.class);
        verify(connectionMock).unwrap(Object.class);
    }

    @Test
    void throwsPersistenceExceptionWhenUnwrapCallFailsOnConnection() throws Exception {
        when(connectionMock.unwrap(Object.class)).thenThrow(new OntoDriverException());
        assertThrows(OWLPersistenceException.class, () -> connectionWrapper.unwrap(Object.class));
    }
}
