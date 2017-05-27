package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class OwlapiPreparedStatementTest {

    private static final String QUERY = "SELECT ?x WHERE { ?x ?y ?z . }";

    @Mock
    private TransactionalStatementExecutor executorMock;
    @Mock
    private StatementExecutorFactory executorFactoryMock;
    @Mock
    private OwlapiConnection connectionMock;
    @Mock
    private ResultSet resultSetMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(executorFactoryMock.getStatementExecutor(any(Statement.StatementOntology.class))).thenReturn(executorMock);
        when(executorMock.executeQuery(anyString(), any())).thenReturn(resultSetMock);
    }

    @Test
    public void executeQueryClosesCurrentResultSet() throws Exception {
        final OwlapiPreparedStatement statement = new OwlapiPreparedStatement(executorFactoryMock, connectionMock,
                QUERY);
        statement.executeQuery();
        statement.setObject("x", "value");
        statement.executeQuery();
        verify(resultSetMock).close();
    }
}