/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.function.Consumer;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class LiveOntologyStatementExecutorTest {

    private static final String QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }";
    private static final String UPDATE = "INSERT DATA { a rdf:type b . }";

    @Mock
    private Connector connectorMock;
    @Mock
    private Statement statementMock;

    private LiveOntologyStatementExecutor executor;

    @BeforeEach
    public void setUp() {
        this.executor = new LiveOntologyStatementExecutor(connectorMock);
    }

    @Test
    public void executeQueryPassesFunctionToConnectorForExecutionInReadOnly() throws Exception {
        final SelectResultSet resultSet = mock(SelectResultSet.class);
        when(connectorMock.executeRead(any(Function.class))).thenReturn(resultSet);
        final ResultSet res = executor.executeQuery(QuerySpecification.query(QUERY).statement(statementMock));
        assertNotNull(res);
        assertEquals(resultSet, res);
        verify(connectorMock).executeRead(any(Function.class));
    }

    @Test
    public void executeQueryReturningNullThrowsDriverException() {
        assertThrows(OntoDriverException.class, () -> executor.executeQuery(QuerySpecification.query(QUERY).statement(statementMock)));
    }

    @Test
    public void executeUpdatePassesConsumerFunctionToConnectorForExecutionInWriteMode() {
        executor.executeUpdate(QuerySpecification.query(UPDATE).statement(statementMock));
        verify(connectorMock).executeWrite(any(Consumer.class));
    }
}
