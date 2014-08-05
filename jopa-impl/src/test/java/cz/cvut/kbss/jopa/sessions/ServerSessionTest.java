package cz.cvut.kbss.jopa.sessions;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

public class ServerSessionTest {

	private OntologyStorageProperties storageProperties;

	@Mock
	private Metamodel metamodelMock;

	private ServerSession session;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		this.storageProperties = new OntologyStorageProperties(
				URI.create("http://krizik.felk.cvut.cz/ontologies/jopa"),
				URI.create("file://tmp/jopa"), OntologyConnectorType.OWLAPI);
		when(metamodelMock.getEntities()).thenReturn(Collections.<EntityType<?>> emptySet());
		this.session = new ServerSession(storageProperties,
				Collections.<String, String> emptyMap(), metamodelMock);
	}

	@Test
	public void testClose() {
		final EntityTransaction et = mock(EntityTransaction.class);
		when(et.isActive()).thenReturn(Boolean.TRUE);
		when(et.isRollbackOnly()).thenReturn(Boolean.FALSE);
		final EntityManager em = mock(EntityManager.class);
		session.transactionStarted(et, em);

		session.close();
		verify(et).setRollbackOnly();
	}
}
