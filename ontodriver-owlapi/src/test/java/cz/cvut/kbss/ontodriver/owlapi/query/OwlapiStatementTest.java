package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import cz.cvut.kbss.ontodriver_new.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class OwlapiStatementTest {

    private static final String QUERY = "SELECT ?x WHERE { ?x ?y ?z . }";
    private static final String UPDATE = "INSERT DATA { a b c . }";

    @Mock
    private TransactionalStatementExecutor executorMock;
    @Mock
    private StatementExecutorFactory executorFactoryMock;
    @Mock
    private OwlapiConnection connectionMock;

    private OwlapiStatement statement;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(executorFactoryMock.getStatementExecutor(any(Statement.StatementOntology.class))).thenReturn(executorMock);
        this.statement = new OwlapiStatement(executorFactoryMock, connectionMock);
    }

    @Test
    public void executeStatementOnTransactionalUsesTransactionalSnapshot() throws Exception {
        statement.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        statement.executeQuery(QUERY);
        verify(executorFactoryMock).getStatementExecutor(Statement.StatementOntology.TRANSACTIONAL);
        verify(executorMock).executeQuery(QUERY, statement);
    }

    @Test
    public void executeUpdateOnCentralUsesTheLiveOntology() throws Exception {
        statement.useOntology(Statement.StatementOntology.CENTRAL);
        statement.executeUpdate(UPDATE);
        verify(executorFactoryMock).getStatementExecutor(Statement.StatementOntology.CENTRAL);
        verify(executorMock).executeUpdate(UPDATE);
    }

    @Test
    public void executeUpdateCommitsConnectionIfSetToAutoCommit() throws Exception {
        statement.executeUpdate(UPDATE);
        verify(connectionMock).commitIfAuto();
    }

    @Test(expected = IllegalStateException.class)
    public void executeQueryOnClosedThrowsException() throws Exception {
        statement.close();
        statement.executeQuery(QUERY);
    }

    @Test(expected = IllegalStateException.class)
    public void executeUpdateOnClosedThrowsException() throws Exception {
        statement.close();
        statement.executeUpdate(UPDATE);
    }
}