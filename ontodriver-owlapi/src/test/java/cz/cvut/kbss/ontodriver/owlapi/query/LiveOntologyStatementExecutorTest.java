package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver_new.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.function.Consumer;
import java.util.function.Function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class LiveOntologyStatementExecutorTest {

    private static final String QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }";
    private static final String UPDATE = "INSERT DATA { a rdf:type b . }";

    @Mock
    private Connector connectorMock;
    @Mock
    private Statement statementMock;

    private LiveOntologyStatementExecutor executor;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.executor = new LiveOntologyStatementExecutor(connectorMock);
    }

    @Test
    public void executeQueryPassesFunctionToConnectorForExecutionInReadOnly() throws Exception {
        final OwlapiResultSet resultSet = mock(OwlapiResultSet.class);
        when(connectorMock.executeRead(any(Function.class))).thenReturn(resultSet);
        final OwlapiResultSet res = executor.executeQuery(QUERY, statementMock);
        assertNotNull(res);
        assertEquals(resultSet, res);
        verify(connectorMock).executeRead(any(Function.class));
    }

    @Test(expected = OntoDriverException.class)
    public void executeQueryReturningNullThrowsDriverException() throws Exception {
        executor.executeQuery(QUERY, statementMock);
    }

    @Test
    public void executeUpdatePassesConsumerFunctionToConnectorForExecutionInWriteMode() throws Exception {
        executor.executeUpdate(UPDATE);
        verify(connectorMock).executeWrite(any(Consumer.class));
    }
}