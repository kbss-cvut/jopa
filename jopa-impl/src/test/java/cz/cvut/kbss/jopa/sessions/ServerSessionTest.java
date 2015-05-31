package cz.cvut.kbss.jopa.sessions;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.Collections;

import cz.cvut.kbss.jopa.accessors.DataSourceStub;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.AbstractEntityManager;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

public class ServerSessionTest {

    @Mock
    private Metamodel metamodelMock;

    private ServerSession session;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        OntologyStorageProperties storageProperties = OntologyStorageProperties.ontologyUri(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa")).physicalUri(
                URI.create("file://tmp/jopa")).connectorType(OntologyConnectorType.OWLAPI)
                                                                               .driver(DataSourceStub.class
                                                                                       .getCanonicalName()).build();
        when(metamodelMock.getEntities()).thenReturn(Collections.<EntityType<?>>emptySet());
        this.session = new ServerSession(storageProperties,
                Collections.singletonMap("storage", "new"), metamodelMock);
    }

    @Test
    public void testClose() {
        final EntityTransaction et = mock(EntityTransaction.class);
        when(et.isActive()).thenReturn(Boolean.TRUE);
        when(et.isRollbackOnly()).thenReturn(Boolean.FALSE);
        final AbstractEntityManager em = mock(AbstractEntityManager.class);
        session.transactionStarted(et, em);

        session.close();
        verify(et).setRollbackOnly();
    }
}
