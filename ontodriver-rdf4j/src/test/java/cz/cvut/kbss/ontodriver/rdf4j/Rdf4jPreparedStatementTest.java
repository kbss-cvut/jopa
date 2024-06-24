/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.rdf4j.query.QuerySpecification;
import cz.cvut.kbss.ontodriver.rdf4j.query.Rdf4jPreparedStatement;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class Rdf4jPreparedStatementTest {

    @Mock
    private StatementExecutor executorMock;
    @Mock
    private TupleQueryResult resultMock;

    private PreparedStatement statement;

    @Test
    public void testConstructorEmptyStatement() {
        assertThrows(IllegalArgumentException.class, () -> initStatement(""));
    }

    private void initStatement(final String query) {
        this.statement = new Rdf4jPreparedStatement(executorMock, query);
    }

    @Test
    public void testExecuteQuery() throws Exception {
        when(executorMock.executeSelectQuery(any(QuerySpecification.class))).thenReturn(resultMock);
        final String query = "SELECT ?y WHERE { ?x <http://property> ?y . }";
        final String expected = "SELECT ?y WHERE { <http://subject> <http://property> ?y . }";
        initStatement(query);
        statement.setObject("x", "<http://subject>");
        statement.executeQuery();
        verify(executorMock).executeSelectQuery(QuerySpecification.query(expected));
    }

    @Test
    public void testExecuteUpdate() throws Exception {
        final String query = "WITH <urn:sparql:tests:update:insert:delete:with>"
                + "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> ?name }"
                + "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' }" + "WHERE"
                + "{" + "?person <http://xmlns.com/foaf/0.1/givenName> ?name }";
        final String expected = "WITH <urn:sparql:tests:update:insert:delete:with>"
                + "DELETE { ?person <http://xmlns.com/foaf/0.1/givenName> 'Bill' }"
                + "INSERT { ?person <http://xmlns.com/foaf/0.1/givenName> 'William' }" + "WHERE"
                + "{" + "?person <http://xmlns.com/foaf/0.1/givenName> 'Bill' }";
        initStatement(query);
        statement.setObject("name", "'Bill'");
        statement.executeUpdate();
        verify(executorMock).executeUpdate(QuerySpecification.query(expected));
    }

    @Test
    public void executeQueryClosesCurrentResultSet() throws Exception {
        when(executorMock.executeSelectQuery(any(QuerySpecification.class))).thenReturn(resultMock);
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }";
        initStatement(query);
        final ResultSet rsOne = statement.executeQuery();
        assertTrue(rsOne.isOpen());
        final ResultSet rsTwo = statement.executeQuery();
        assertTrue(rsTwo.isOpen());
        assertFalse(rsOne.isOpen());
        assertNotSame(rsOne, rsTwo);
    }
}
