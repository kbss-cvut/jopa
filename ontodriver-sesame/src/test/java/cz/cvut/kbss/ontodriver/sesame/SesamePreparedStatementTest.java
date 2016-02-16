/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.query.SesamePreparedStatement;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.query.TupleQueryResult;

import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class SesamePreparedStatementTest {

    @Mock
    private StatementExecutor executorMock;
    @Mock
    private TupleQueryResult resultMock;

    private PreparedStatement statement;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(executorMock.executeSelectQuery(any(String.class))).thenReturn(resultMock);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConstructorEmptyStatement() throws Exception {
        initStatement("");
        fail("This line should not have been reached.");
    }

    @Test
    public void testExecuteQuery() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x <http://property> ?y . }";
        final String expected = "SELECT _:subject ?y WHERE { _:subject <http://property> ?y . }";
        initStatement(query);
        statement.setObject("x", "_:subject");
        statement.executeQuery();
        verify(executorMock).executeSelectQuery(expected);
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
        verify(executorMock).executeUpdate(expected);
    }

    private void initStatement(final String query) throws OntoDriverException {
        this.statement = new SesamePreparedStatement(executorMock, query);
    }
}
