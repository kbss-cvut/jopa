/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Iterator;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

public class NamedQueryTest extends IntegrationTestBase {

    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;

    @BeforeEach
    protected void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        super.setUp();
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(resultSetMock.iterator()).thenReturn(mock(Iterator.class));
    }

    @Test
    void testCreateTypedNamedNativeQuery() throws Exception {
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        final TypedQuery<OWLClassA> query = em.createNamedQuery("OWLClassA.findAll", OWLClassA.class);
        assertNotNull(query);
    }

    @Test
    void testNamedNativeQueryWithParameterReplace() throws Exception {
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
