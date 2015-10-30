package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver_new.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

public class StatementExecutorFactoryTest {

    @Mock
    private Connector connectorMock;
    @Mock
    private OntologySnapshot transactionalSnapshotMock;

    private StatementExecutorFactory factory;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectorMock.getLiveOntology()).thenReturn(mock(OntologySnapshot.class));
        this.factory = new StatementExecutorFactory(transactionalSnapshotMock, connectorMock);
    }

    @Test
    public void getExecutorReturnsTransactionalSnapshotForTransactionalOntologySetting() throws Exception {
        final StatementExecutor executor = factory.getStatementExecutor(Statement.StatementOntology.TRANSACTIONAL);
        assertNotNull(executor);
        verify(connectorMock, never()).getLiveOntology();
    }

    @Test
    public void getExecutorReturnsLiveOntologySnapshotForCentralOntologySetting() throws Exception {
        final StatementExecutor executor = factory.getStatementExecutor(Statement.StatementOntology.CENTRAL);
        assertNotNull(executor);
        verify(connectorMock).getLiveOntology();
    }
}