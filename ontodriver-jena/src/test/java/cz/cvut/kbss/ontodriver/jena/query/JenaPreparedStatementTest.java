/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.jena.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.util.StatementHolder;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.query.QueryFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class JenaPreparedStatementTest {

    private static final String QUERY = "SELECT * WHERE { ?x ?y ?z . }";

    @Mock
    private StatementExecutor executor;

    @Mock
    private AbstractResultSet resultSet;

    private JenaPreparedStatement statement;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(executor.executeSelectQuery(any(), any())).thenReturn(resultSet);
        when(executor.executeAskQuery(any(), any())).thenReturn(resultSet);
    }

    @Test
    public void constructorAnalyzesQueryPassedAsParameter() throws Exception {
        this.statement = new JenaPreparedStatement(executor, QUERY);
        final StatementHolder holder = getStatementHolder();
        assertNotNull(holder);
        assertEquals(QUERY, holder.getStatement());
    }

    private StatementHolder getStatementHolder() throws Exception {
        final Field holderField = JenaPreparedStatement.class.getDeclaredField("holder");
        holderField.setAccessible(true);
        return (StatementHolder) holderField.get(statement);
    }

    @Test
    public void constructorThrowsIllegalArgumentForEmptyQuery() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> this.statement = new JenaPreparedStatement(executor, ""));
        assertThat(ex.getMessage(), containsString("Statement cannot be empty"));

    }

    @Test
    public void setParameterSetsParameterOnQuery() throws Exception {
        this.statement = new JenaPreparedStatement(executor, QUERY);
        final String value = "<" + Vocabulary.RDF_TYPE + ">";
        statement.setObject("y", value);
        final StatementHolder holder = getStatementHolder();
        assertThat(holder.assembleStatement(), containsString(value));
    }

    @Test
    public void setParameterThrowsIllegalStateOnClosedStatement() throws Exception {
        this.statement = new JenaPreparedStatement(executor, QUERY);
        statement.close();
        final IllegalStateException ex = assertThrows(IllegalStateException.class,
                () -> statement.setObject("y", "rdf:type"));
        assertThat(ex.getMessage(), containsString("Statement is closed"));
    }

    @Test
    public void executeQueryExecutesStatementWithParameter() throws Exception {
        this.statement = new JenaPreparedStatement(executor, QUERY);
        final String value = "<" + Vocabulary.RDF_TYPE + ">";
        statement.setObject("y", value);
        final String expected = QUERY.replace("?y", value);
        statement.executeQuery();
        verify(executor)
                .executeSelectQuery(eq(QueryFactory.create(expected)), eq(Statement.StatementOntology.CENTRAL));
    }

    @Test
    public void executeQueryThrowsIllegalStateForClosedStatement() throws Exception {
        this.statement = new JenaPreparedStatement(executor, QUERY);
        statement.close();
        final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> statement.executeQuery());
        assertThat(ex.getMessage(), containsString("Statement is closed"));
    }

    @Test
    public void executeUpdateExecutesUpdateStatement() throws Exception {
        final String update = "INSERT DATA { _:a1 a ?type . }";
        this.statement = new JenaPreparedStatement(executor, update);
        final String value = "<" + Generator.generateUri() + ">";
        statement.setObject("type", value);
        final String expected = update.replace("?type", value);
        statement.executeUpdate();
        verify(executor).executeUpdate(eq(expected), eq(Statement.StatementOntology.CENTRAL));
    }

    @Test
    public void executeUpdateThrowsIllegalStateForClosedStatement() throws Exception {
        final String update = "INSERT DATA { _:a1 a ?type . }";
        this.statement = new JenaPreparedStatement(executor, update);
        statement.close();
        final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> statement.executeUpdate());
        assertThat(ex.getMessage(), containsString("Statement is closed"));
    }

    @Test
    public void clearParametersClearsAlreadySetParameters() throws Exception {
        this.statement = new JenaPreparedStatement(executor, QUERY);
        final String value = "<" + Vocabulary.RDF_TYPE + ">";
        statement.setObject("y", value);
        statement.clearParameters();
        statement.executeQuery();
        verify(executor)
                .executeSelectQuery(eq(QueryFactory.create(QUERY)), eq(Statement.StatementOntology.CENTRAL));
    }

    @Test
    public void clearParametersThrowsIllegalStateForClosedStatement() throws Exception {
        this.statement = new JenaPreparedStatement(executor, QUERY);
        final String value = "<" + Vocabulary.RDF_TYPE + ">";
        statement.setObject("y", value);
        statement.close();
        final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> statement.clearParameters());
        assertThat(ex.getMessage(), containsString("Statement is closed"));
    }
}
