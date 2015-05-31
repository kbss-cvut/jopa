package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.query.Query;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class QueryFactoryImplTest {

	private static final String QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z. }";
	private static final Class<OWLClassA> CLS = OWLClassA.class;

	@Mock
	private UnitOfWork uowMock;

	@Mock
	private ConnectionWrapper connectionMock;

	private QueryFactoryImpl factory;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(uowMock.useBackupOntologyForQueryProcessing()).thenReturn(Boolean.FALSE);
		when(uowMock.useTransactionalOntologyForQueryProcessing()).thenReturn(Boolean.TRUE);
		this.factory = new QueryFactoryImpl(uowMock, connectionMock);
	}

	@Test
	public void testCreateNativeQuery() {
		final Query<List<String>> q = factory.createNativeQuery(QUERY);
		assertNotNull(q);
		verify(uowMock).useBackupOntologyForQueryProcessing();
	}

	@Test(expected = NullPointerException.class)
	public void testCreateNativeQueryNull() {
		final Query<List<String>> q = factory.createNativeQuery(null);
		assert q == null;
	}

	@Test
	public void testCreateNativeQueryTyped() {
		final Query<OWLClassA> q = factory.createNativeQuery(QUERY, CLS);
		assertNotNull(q);
		verify(uowMock).useBackupOntologyForQueryProcessing();
	}

	@Test(expected = NullPointerException.class)
	public void testCreateNativeQueryTypedNullQuery() {
		final Query<OWLClassA> q = factory.createNativeQuery(null, CLS);
		assert q == null;
	}

	@Test(expected = NullPointerException.class)
	public void testCreateNativeQueryTypedNullType() {
		final Query<OWLClassA> q = factory.createNativeQuery(QUERY, null);
		assert q == null;
	}

	@Test
	public void testCreateQuery() {
		final Query q = factory.createQuery(QUERY);
		assertNotNull(q);
		verify(uowMock).useBackupOntologyForQueryProcessing();
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNull() {
		final Query q = factory.createQuery(null);
		assert q == null;
	}

	@Test
	public void testCreateQueryTyped() {
		final Query<OWLClassA> q = factory.createQuery(QUERY, CLS);
		assertNotNull(q);
		verify(uowMock).useBackupOntologyForQueryProcessing();
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryTypedNullQuery() {
		final Query<OWLClassA> q = factory.createQuery(null, CLS);
		assert q == null;
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryTypedNullType() {
		final Query<OWLClassA> q = factory.createQuery(QUERY, null);
		assert q == null;
	}
}
