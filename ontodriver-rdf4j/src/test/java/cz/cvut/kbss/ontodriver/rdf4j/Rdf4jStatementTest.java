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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.rdf4j.query.AskResultSet;
import cz.cvut.kbss.ontodriver.rdf4j.query.QuerySpecification;
import cz.cvut.kbss.ontodriver.rdf4j.query.Rdf4jStatement;
import cz.cvut.kbss.ontodriver.rdf4j.query.SelectResultSet;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class Rdf4jStatementTest {

    private static final String SELECT_ENTITY_QUERY =
            "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";
    private static final String ASK_BOOLEAN_QUERY =
            "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";

    @Mock
    private StatementExecutor executorMock;

    private Rdf4jStatement statement;

    @BeforeEach
    public void setUp() {
        this.statement = new Rdf4jStatement(executorMock);
    }

    @Test
    public void executeSelectQueryReturnsSelectResultSet() throws Exception {
        when(executorMock.executeSelectQuery(any())).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rs = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertNotNull(rs);
        assertInstanceOf(SelectResultSet.class, rs);
        verify(executorMock).executeSelectQuery(QuerySpecification.query(SELECT_ENTITY_QUERY));
    }

    @Test
    public void executeAskQueryReturnsAskResultSet() throws Exception {
        when(executorMock.executeBooleanQuery(any())).thenReturn(true);
        final ResultSet rs = statement.executeQuery(ASK_BOOLEAN_QUERY);
        assertNotNull(rs);
        assertInstanceOf(AskResultSet.class, rs);
        verify(executorMock).executeBooleanQuery(QuerySpecification.query(ASK_BOOLEAN_QUERY));
    }

    @Test
    public void closeClosesCurrentResultSet() throws Exception {
        when(executorMock.executeSelectQuery(any())).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rs = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertTrue(rs.isOpen());
        statement.close();
        assertFalse(rs.isOpen());
        assertFalse(statement.isOpen());
    }

    @Test
    public void executeQueryClosesCurrentResultSet() throws Exception {
        when(executorMock.executeSelectQuery(any())).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rsOne = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertTrue(rsOne.isOpen());
        final ResultSet rsTwo = statement.executeQuery(ASK_BOOLEAN_QUERY);
        assertTrue(rsTwo.isOpen());
        assertFalse(rsOne.isOpen());
        assertNotSame(rsOne, rsTwo);
    }

    @Test
    public void executeUpdateClosesCurrentResultSet() throws Exception {
        when(executorMock.executeSelectQuery(any())).thenReturn(mock(TupleQueryResult.class));
        final ResultSet rsOne = statement.executeQuery(SELECT_ENTITY_QUERY);
        assertTrue(rsOne.isOpen());
        statement.executeUpdate("INSERT DATA { ?x ?y ?z .}");
        assertFalse(rsOne.isOpen());
    }

    /**
     * Bug #89
     */
    @Test
    void executeQueryReturnsAskResultForAskQueryWithPrefix() throws Exception {
        final String askWithPrefix = "PREFIX jopa: <https://onto.fel.cvut.cz/ontologies/jopa/entities#>\n" +
                "ASK { ?x a jopa:OWLClassA }";
        when(executorMock.executeBooleanQuery(any())).thenReturn(true);
        final ResultSet rs = statement.executeQuery(askWithPrefix);
        assertNotNull(rs);
        assertInstanceOf(AskResultSet.class, rs);
        verify(executorMock).executeBooleanQuery(QuerySpecification.query(askWithPrefix));
    }
}
