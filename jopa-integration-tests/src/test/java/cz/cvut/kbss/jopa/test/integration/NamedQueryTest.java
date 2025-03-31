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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Iterator;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class NamedQueryTest extends IntegrationTestBase {

    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;
    @Mock
    private Iterator<ResultRow> iteratorMock;

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Test
    void testCreateTypedNamedNativeQuery() {
        final TypedQuery<OWLClassA> query = em.createNamedQuery("OWLClassA.findAll", OWLClassA.class);
        assertNotNull(query);
    }

    @Test
    void testNamedNativeQueryWithParameterReplace() throws Exception {
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(resultSetMock.iterator()).thenReturn(iteratorMock);
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        final Query query = em.createNamedQuery("OWLClassA.findByString");
        assertNotNull(query);
        query.setParameter("str", "Test", "en").getResultList();
        verify(statementMock)
                .executeQuery("SELECT ?x WHERE { ?x <" + Vocabulary.P_A_STRING_ATTRIBUTE + "> \"Test\"@en . }");
    }

    @Test
    void createUnknownNamedNativeQueryThrowsIllegalArgument() {
        assertThrows(IllegalArgumentException.class, () -> em.createNamedQuery("findAll"));
    }
}
