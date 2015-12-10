package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import cz.cvut.kbss.ontodriver.sesame.query.SesamePreparedStatement;
import cz.cvut.kbss.ontodriver.sesame.query.SesameStatement;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;

public class SesameConnectionTest {

	@Mock
	private SesameAdapter adapterMock;

	private Connection connection;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		this.connection = new SesameConnection(adapterMock);
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(connection.isOpen());
		connection.close();
		assertFalse(connection.isOpen());
		connection.close();
		assertFalse(connection.isOpen());
		verify(adapterMock).close();
	}

	@Test
	public void testCommit() throws Exception {
		connection.commit();
		verify(adapterMock).commit();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitClosed() throws Exception {
		connection.close();
		assertFalse(connection.isOpen());
		try {
			connection.commit();
		} finally {
			verify(adapterMock, never()).commit();
		}
	}

	@Test
	public void testRollback() throws Exception {
		connection.rollback();
		verify(adapterMock).rollback();
	}

	@Test
	public void testRollbackAutoCommit() throws Exception {
		connection.setAutoCommit(true);
		connection.rollback();
		verify(adapterMock, never()).rollback();
	}

	@Test
	public void testSetAutoCommit() {
		connection.setAutoCommit(true);
		assertTrue(connection.isAutoCommit());
		connection.setAutoCommit(false);
		assertFalse(connection.isAutoCommit());
	}

	@Test
	public void testCreateStatement() throws Exception {
		final Statement res = connection.createStatement();
		assertNotNull(res);
		assertTrue(res instanceof SesameStatement);
	}

	@Test(expected = IllegalStateException.class)
	public void testCreateStatementOnClosed() throws Exception {
		connection.close();
		final Statement res = connection.createStatement();
		fail("This line should not have been reached");
		assert res == null;
	}

	@Test
	public void testPrepareStatement() throws Exception {
		final PreparedStatement res = connection
				.prepareStatement("SELECT ?x ? y ?z WHERE { ?x ?y ?z . }");
		assertNotNull(res);
		assertTrue(res instanceof SesamePreparedStatement);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPrepareStatementEmpty() throws Exception {
		final PreparedStatement res = connection.prepareStatement("");
		fail("This line should not have been reached.");
		assert res == null;
	}

	@Test
	public void testIsConsistent() throws Exception {
		final URI ctx = URI.create("http://contextUri");
		when(adapterMock.isConsistent(ctx)).thenReturn(Boolean.TRUE);
		boolean res = connection.isConsistent(ctx);
		assertTrue(res);
		verify(adapterMock).isConsistent(ctx);
	}

	@Test
	public void testGetContexts() throws Exception {
		final List<URI> contexts = new ArrayList<>();
		contexts.add(URI.create("http://contextOne"));
		contexts.add(URI.create("http://contextTwo"));
		when(adapterMock.getContexts()).thenReturn(contexts);
		final List<URI> res = connection.getContexts();
		assertEquals(contexts, res);
		verify(adapterMock).getContexts();
	}

	@Test(expected = IllegalStateException.class)
	public void testGetContextsOnClosed() throws Exception {
		connection.close();
		try {
			final List<URI> res = connection.getContexts();
			fail("This line should not have been reached.");
			assert res == null;
		} finally {
			verify(adapterMock, never()).getContexts();
		}
	}

	@Test
	public void testGenerateIdentifier() throws Exception {
		final URI classUri = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/owlClassA");
		final URI identifier = URI
				.create("http://krizik.felk.cvut.cz/ontologies/jopa/owlClassA_instance_1");
		when(adapterMock.generateIdentifier(classUri)).thenReturn(identifier);
		final URI res = connection.generateIdentifier(classUri);
		assertEquals(identifier, res);
		verify(adapterMock).generateIdentifier(classUri);
	}

	@Test
	public void testContainsAxiom() throws Exception {
		final Axiom<?> ax = mock(Axiom.class);
		when(adapterMock.contains(ax, null)).thenReturn(Boolean.TRUE);
		final boolean res = connection.contains(ax, null);
		assertTrue(res);
		verify(adapterMock).contains(ax, null);
	}

	@Test
	public void testContainsAxiomInContext() throws Exception {
		final Axiom<?> ax = mock(Axiom.class);
		final URI context = URI.create("http://context.org");
		when(adapterMock.contains(ax, context)).thenReturn(Boolean.TRUE);
		final boolean res = connection.contains(ax, context);
		assertTrue(res);
		verify(adapterMock).contains(ax, context);
	}

	@Test
	public void testFind() throws Exception {
		final AxiomDescriptor axDesc = mock(AxiomDescriptor.class);
		final List<Axiom<?>> axioms = new ArrayList<>();
		axioms.add(mock(Axiom.class));
		axioms.add(mock(Axiom.class));
		when(adapterMock.find(axDesc)).thenReturn(axioms);
		final Collection<Axiom<?>> res = connection.find(axDesc);
		assertEquals(axioms, res);
	}

	@Test(expected = NullPointerException.class)
	public void testFindNull() throws Exception {
		try {
			final Collection<Axiom<?>> res = connection.find(null);
			fail("This line should not have been reached.");
			assert res == null;
		} finally {
			verify(adapterMock, never()).find(any(AxiomDescriptor.class));
		}
	}

	@Test
	public void testPersist() throws Exception {
		final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
		connection.persist(axDesc);
		verify(adapterMock).persist(axDesc);
	}

	@Test
	public void testPersistAutoCommit() throws Exception {
		final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
		connection.setAutoCommit(true);
		connection.persist(axDesc);
		verify(adapterMock).persist(axDesc);
		verify(adapterMock).commit();
	}

	@Test(expected = IllegalStateException.class)
	public void testPersistOnClosed() throws Exception {
		connection.close();
		final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
		try {
			connection.persist(axDesc);
		} finally {
			verify(adapterMock, never()).persist(axDesc);
		}
	}

	@Test
	public void testUpdate() throws Exception {
		final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
		connection.update(axDesc);
		verify(adapterMock).update(axDesc);
	}

	@Test
	public void testUpdateAutoCommit() throws Exception {
		final AxiomValueDescriptor axDesc = mock(AxiomValueDescriptor.class);
		connection.setAutoCommit(true);
		connection.update(axDesc);
		verify(adapterMock).update(axDesc);
		verify(adapterMock).commit();
	}

	@Test
	public void testRemove() throws Exception {
		final AxiomDescriptor axDesc = mock(AxiomDescriptor.class);
		connection.remove(axDesc);
		verify(adapterMock).remove(axDesc);
	}
}
