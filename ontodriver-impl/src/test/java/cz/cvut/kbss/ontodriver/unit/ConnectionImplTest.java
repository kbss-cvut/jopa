package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.impl.ConnectionImpl;
import cz.cvut.kbss.ontodriver.utils.OWLClassA;

public class ConnectionImplTest {

	private static final URI CONTEXT = URI.create("http://defaultUri");

	private static Field hasChangesField;
	private static Descriptor descriptor;

	private ConnectionImpl connection;

	@Mock
	private Metamodel metamodelMock;

	@Mock
	private PersistenceProviderFacade providerMock;

	@Mock
	private StorageModule storageModuleMock;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		hasChangesField = ConnectionImpl.class.getDeclaredField("hasChanges");
		hasChangesField.setAccessible(true);
		descriptor = new EntityDescriptor();
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(providerMock.getMetamodel()).thenReturn(metamodelMock);

		this.connection = new ConnectionImpl(storageModuleMock);
		assertTrue(connection.isOpen());
	}

	@Test
	public void testConnectionImpl() throws Exception {
		final StorageModule mngrMock = mock(StorageModule.class);
		final Connection c = new ConnectionImpl(mngrMock);
		assertNotNull(c);
		assertTrue(c.isOpen());
	}

	@Test(expected = NullPointerException.class)
	public void testConnectionImplNull() throws Exception {
		final Connection c = new ConnectionImpl(null);
	}

	@Test
	public void testClose() throws Exception {
		connection.close();

		assertFalse(connection.isOpen());
		verify(storageModuleMock).close();
	}

	@Test
	public void testCloseTwice() throws Exception {
		connection.close();
		connection.close();

		assertFalse(connection.isOpen());
		// Exactly once
		verify(storageModuleMock).close();
	}

	@Test
	public void testCommit() throws Exception {
		// Make sure there are changes
		hasChangesField.set(connection, Boolean.TRUE);
		connection.commit();

		verify(storageModuleMock).commit();
		assertFalse(hasChangesField.getBoolean(connection));
	}

	@Test
	public void testCommitNoChanges() throws Exception {
		connection.commit();

		verify(storageModuleMock, never()).commit();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitClosed() throws Exception {
		connection.close();
		try {
			connection.commit();
		} finally {
			verify(storageModuleMock, never()).commit();
		}
	}

	@Test
	public void testSetAutoCommit() throws Exception {
		connection.setAutoCommit(true);
		assertTrue(connection.getAutoCommit());

		connection.setAutoCommit(false);
		assertFalse(connection.getAutoCommit());
	}

	@Test(expected = IllegalStateException.class)
	public void testSetAutoCommitOnClosed() throws Exception {
		connection.close();
		connection.setAutoCommit(true);
	}

	@Test
	public void testRollback() throws Exception {
		// Make sure there are changes
		hasChangesField.set(connection, Boolean.TRUE);
		connection.rollback();

		verify(storageModuleMock).rollback();
		assertFalse(hasChangesField.getBoolean(connection));
	}

	@Test
	public void testRollbackNoChanges() throws Exception {
		connection.commit();

		verify(storageModuleMock, never()).rollback();
	}

	@Test
	public void testContains() throws Exception {
		final URI pk = URI.create("http://pk");
		when(storageModuleMock.contains(pk, CONTEXT)).thenReturn(true);

		final boolean res = connection.contains(pk, CONTEXT);
		assertTrue(res);
		verify(storageModuleMock).contains(pk, CONTEXT);
	}

	@Test
	public void testContainsNoContext() throws Exception {
		final URI pk = URI.create("http://pk");
		when(storageModuleMock.contains(pk, null)).thenReturn(true);

		final boolean res = connection.contains(pk, null);
		assertTrue(res);
		verify(storageModuleMock).contains(pk, null);
	}

	@Test(expected = NullPointerException.class)
	public void testContainsNull() throws Exception {
		final boolean res = connection.contains(null, null);
		// Shouldn't be called
		assertFalse(res);
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testFind() throws Exception {
		final Descriptor descriptor = new EntityDescriptor(CONTEXT);
		final URI pk = URI.create("http://pk");
		final OWLClassA a = new OWLClassA();
		when(
				storageModuleMock.find(any(Class.class), any(Object.class),
						any(EntityDescriptor.class))).thenReturn(null);
		when(storageModuleMock.find(OWLClassA.class, pk, descriptor)).thenReturn(a);

		final OWLClassA res = connection.find(OWLClassA.class, pk, descriptor);
		assertNotNull(res);
		assertSame(a, res);
		verify(storageModuleMock).find(OWLClassA.class, pk, descriptor);

		final URI pkTwo = URI.create("http://pkTwo");
		final OWLClassA resTwo = connection.find(OWLClassA.class, pkTwo, descriptor);
		assertNull(resTwo);
		verify(storageModuleMock).find(OWLClassA.class, pkTwo, descriptor);
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testFindNull() throws Exception {
		try {
			final OWLClassA res = connection.find(OWLClassA.class, null, null);
			assertNull(res);
		} finally {
			verify(storageModuleMock, never()).find(any(Class.class), any(Object.class),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testIsConsistent() throws Exception {
		when(storageModuleMock.isConsistent(any(URI.class))).thenReturn(Boolean.TRUE);

		final boolean res = connection.isConsistent(CONTEXT);
		assertTrue(res);
		verify(storageModuleMock).isConsistent(CONTEXT);
	}

	@Test
	public void testLoadFieldValue() throws Exception {
		doAnswer(new Answer<Object>() {

			@Override
			public Object answer(InvocationOnMock invocation) throws Throwable {
				for (Object o : invocation.getArguments()) {
					// Just check that the arguments were passed to storage
					// manager correctly
					if (o == null) {
						throw new NullPointerException();
					}
				}
				return null;
			}
		}).when(storageModuleMock).loadFieldValue(any(), any(Field.class),
				any(EntityDescriptor.class));
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();

		connection.loadFieldValue(a, f, descriptor);
		verify(storageModuleMock).loadFieldValue(a, f, descriptor);
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNull() throws Exception {
		final Field f = OWLClassA.getStrAttField();

		try {
			connection.loadFieldValue(null, f, descriptor);
		} finally {
			verify(storageModuleMock, never()).loadFieldValue(any(), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testMerge() throws Exception {
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();
		connection.setAutoCommit(false);

		connection.merge(a, f, descriptor);
		assertTrue(hasChangesField.getBoolean(connection));
		verify(storageModuleMock).merge(a, f, descriptor);
		verify(storageModuleMock, never()).commit();
	}

	@Test
	public void testMergeAutoCommit() throws Exception {
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();
		connection.setAutoCommit(true);

		connection.merge(a, f, descriptor);
		// The changes were committed, so there should be none now
		assertFalse(hasChangesField.getBoolean(connection));
		verify(storageModuleMock).merge(a, f, descriptor);
		verify(storageModuleMock).commit();
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNull() throws Exception {
		final Field f = OWLClassA.getStrAttField();
		try {
			connection.merge(null, f, descriptor);
		} finally {
			assertFalse(hasChangesField.getBoolean(connection));
			verify(storageModuleMock, never()).merge(any(), any(Field.class),
					any(EntityDescriptor.class));
			verify(storageModuleMock, never()).commit();
		}
	}

	@Test
	public void testPersist() throws Exception {
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://pk");
		final EntityDescriptor descriptor = new EntityDescriptor();
		connection.setAutoCommit(false);

		connection.persist(pk, a, descriptor);
		assertTrue(hasChangesField.getBoolean(connection));
		verify(storageModuleMock).persist(pk, a, descriptor);
		verify(storageModuleMock, never()).commit();
	}

	@Test
	public void testPersistAutoCommit() throws Exception {
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://pk");
		connection.setAutoCommit(true);

		connection.persist(pk, a, descriptor);
		assertFalse(hasChangesField.getBoolean(connection));
		verify(storageModuleMock).persist(pk, a, descriptor);
		verify(storageModuleMock).commit();
	}

	@Test
	public void testRemove() throws Exception {
		final URI pk = URI.create("http://pk");
		connection.setAutoCommit(false);

		connection.remove(pk, descriptor);
		assertTrue(hasChangesField.getBoolean(connection));
		verify(storageModuleMock).remove(pk, descriptor);
		verify(storageModuleMock, never()).commit();
	}

	@Test
	public void testRemoveAutoCommit() throws Exception {
		final URI pk = URI.create("http://pk");
		connection.setAutoCommit(true);

		connection.remove(pk, descriptor);
		assertFalse(hasChangesField.getBoolean(connection));
		verify(storageModuleMock).remove(pk, descriptor);
		verify(storageModuleMock).commit();
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveNull() throws Exception {
		try {
			connection.remove(null, descriptor);
		} finally {
			verify(storageModuleMock, never()).remove(any(), any(EntityDescriptor.class));
		}
	}

	@Test
	public void createStatement() throws Exception {
		final Statement stmt = connection.createStatement();
		assertNotNull(stmt);
	}

	@Test
	public void testGetContexts() throws Exception {
		final List<URI> ctxs = new ArrayList<>();
		when(storageModuleMock.getContexts()).thenReturn(ctxs);
		final List<URI> res = connection.getContexts();
		assertSame(ctxs, res);
		verify(storageModuleMock).getContexts();
	}
}
