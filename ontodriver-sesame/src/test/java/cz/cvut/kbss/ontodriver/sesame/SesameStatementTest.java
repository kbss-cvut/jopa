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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.query.AskResultSet;
import cz.cvut.kbss.ontodriver.sesame.query.SelectResultSet;
import cz.cvut.kbss.ontodriver.sesame.query.SesameStatement;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class SesameStatementTest {

    private static final String SELECT_ENTITY_QUERY =
            "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";
    private static final String ASK_BOOLEAN_QUERY =
            "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";

    @Mock
    private StatementExecutor executorMock;

    private SesameStatement statement;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.statement = new SesameStatement(executorMock);
    }

    @Test
    public void executeSelectQueryReturnsSelectResultSet() throws Exception {
        when(executorMock.executeSelectQuery(SELECT_ENTITY_QUERY)).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rs = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertNotNull(rs);
        assertTrue(rs instanceof SelectResultSet);
        verify(executorMock).executeSelectQuery(SELECT_ENTITY_QUERY);
    }

    @Test
    public void executeAskQueryReturnsAskResultSet() throws Exception {
        when(executorMock.executeBooleanQuery(ASK_BOOLEAN_QUERY)).thenReturn(true);
        final ResultSet rs = statement.executeQuery(ASK_BOOLEAN_QUERY);
        assertNotNull(rs);
        assertTrue(rs instanceof AskResultSet);
        verify(executorMock).executeBooleanQuery(ASK_BOOLEAN_QUERY);
    }

    @Test
    public void closeClosesCurrentResultSet() throws Exception {
        when(executorMock.executeSelectQuery(SELECT_ENTITY_QUERY)).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rs = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertTrue(rs.isOpen());
        statement.close();
        assertFalse(rs.isOpen());
        assertFalse(statement.isOpen());
    }

    @Test
    public void executeQueryClosesCurrentResultSet() throws Exception {
        when(executorMock.executeSelectQuery(SELECT_ENTITY_QUERY)).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rsOne = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertTrue(rsOne.isOpen());
        final ResultSet rsTwo = statement.executeQuery(ASK_BOOLEAN_QUERY);
        assertTrue(rsTwo.isOpen());
        assertFalse(rsOne.isOpen());
        assertNotSame(rsOne, rsTwo);
    }

    @Test
    public void executeUpdateClosesCurrentResultSet() throws Exception {
        when(executorMock.executeSelectQuery(SELECT_ENTITY_QUERY)).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rsOne = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertTrue(rsOne.isOpen());
        statement.executeUpdate("INSERT DATA { ?x ?y ?z .}");
        assertFalse(rsOne.isOpen());
    }
}