package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertEquals;
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

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.RepositoryNotFoundException;
import cz.cvut.kbss.ontodriver.impl.ConnectionImpl;
import cz.cvut.kbss.ontodriver.utils.OWLClassA;

public class ConnectionImplTest {

	private static List<Repository> repos;
	private static Field hasChangesField;

	private ConnectionImpl connection;

	@Mock
	private Metamodel metamodelMock;

	@Mock
	private PersistenceProviderFacade providerMock;

	@Mock
	private StorageManager storageMngrMock;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		repos = new ArrayList<>(4);
		for (int i = 0; i < 4; i++) {
			repos.add(new Repository(URI.create("http://krizik.felk.cvut.cz/jopa/connectiontest"
					+ i)));
		}
		hasChangesField = ConnectionImpl.class.getDeclaredField("hasChanges");
		hasChangesField.setAccessible(true);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(providerMock.getMetamodel()).thenReturn(metamodelMock);
		when(storageMngrMock.getRepositories()).thenReturn(repos);

		this.connection = new ConnectionImpl(storageMngrMock);
		assertTrue(connection.isOpen());
	}

	@Test
	public void testConnectionImpl() throws Exception {
		final StorageManager mngrMock = mock(StorageManager.class);
		final Connection c = new ConnectionImpl(mngrMock);
		assertNotNull(c);
		assertTrue(c.isOpen());
		verify(mngrMock).getRepositories();
	}

	@Test(expected = NullPointerException.class)
	public void testConnectionImplNull() throws Exception {
		final Connection c = new ConnectionImpl(null);
		// This won't be reached, it's here just to suppress the unused
		// warning
		assert c == null;
	}

	@Test
	public void testClose() throws Exception {
		connection.close();

		assertFalse(connection.isOpen());
		verify(storageMngrMock).close();
	}

	@Test
	public void testCloseTwice() throws Exception {
		connection.close();
		connection.close();

		assertFalse(connection.isOpen());
		// Exactly once
		verify(storageMngrMock).close();
	}

	@Test
	public void testCommit() throws Exception {
		// Make sure there are changes
		hasChangesField.set(connection, Boolean.TRUE);
		connection.commit();

		verify(storageMngrMock).commit();
		assertFalse(hasChangesField.getBoolean(connection));
	}

	@Test
	public void testCommitNoChanges() throws Exception {
		connection.commit();

		verify(storageMngrMock, never()).commit();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitClosed() throws Exception {
		connection.close();
		try {
			connection.commit();
		} finally {
			verify(storageMngrMock, never()).commit();
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

		verify(storageMngrMock).rollback();
		assertFalse(hasChangesField.getBoolean(connection));
	}

	@Test
	public void testRollbackNoChanges() throws Exception {
		connection.commit();

		verify(storageMngrMock, never()).rollback();
	}

	@Test
	public void testContains() throws Exception {
		final EntityDescriptor id = repos.get(2).createRepositoryID(false);
		final URI pk = URI.create("http://pk");
		when(storageMngrMock.contains(pk, id)).thenReturn(true);

		final boolean res = connection.contains(pk, id);
		assertTrue(res);
		verify(storageMngrMock).contains(pk, id);
	}

	@Test(expected = NullPointerException.class)
	public void testContainsNull() throws Exception {
		final URI pk = URI.create("http://pk");

		final boolean res = connection.contains(pk, null);
		// Shouldn't be called
		assertFalse(res);
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testFind() throws Exception {
		final EntityDescriptor id = repos.get(2).createRepositoryID(false);
		final URI pk = URI.create("http://pk");
		final OWLClassA a = new OWLClassA();
		when(storageMngrMock.find(any(Class.class), any(Object.class), any(EntityDescriptor.class)))
				.thenReturn(null);
		when(storageMngrMock.find(OWLClassA.class, pk, id)).thenReturn(a);

		final OWLClassA res = connection.find(OWLClassA.class, pk, id);
		assertNotNull(res);
		assertSame(a, res);
		verify(storageMngrMock).find(OWLClassA.class, pk, id);

		final URI pkTwo = URI.create("http://pkTwo");
		final OWLClassA resTwo = connection.find(OWLClassA.class, pkTwo, id);
		assertNull(resTwo);
		verify(storageMngrMock).find(OWLClassA.class, pkTwo, id);
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testFindNull() throws Exception {
		try {
			final OWLClassA res = connection.find(OWLClassA.class, null, null);
			assertNull(res);
		} finally {
			verify(storageMngrMock, never()).find(any(Class.class), any(Object.class),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testIsConsistent() throws Exception {
		when(storageMngrMock.isConsistent(any(EntityDescriptor.class))).thenReturn(Boolean.TRUE);
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);

		final boolean res = connection.isConsistent(id);
		assertTrue(res);
		verify(storageMngrMock).isConsistent(id);
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
		}).when(storageMngrMock).loadFieldValue(any(), any(Field.class), any(EntityDescriptor.class));
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);

		connection.loadFieldValue(a, f, id);
		verify(storageMngrMock).loadFieldValue(a, f, id);
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNull() throws Exception {
		final Field f = OWLClassA.getStrAttField();
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);

		try {
			connection.loadFieldValue(null, f, id);
		} finally {
			verify(storageMngrMock, never()).loadFieldValue(any(), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testMerge() throws Exception {
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://pk");
		final Field f = OWLClassA.getStrAttField();
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);
		connection.setAutoCommit(false);

		connection.merge(pk, a, f, id);
		assertTrue(hasChangesField.getBoolean(connection));
		verify(storageMngrMock).merge(pk, a, f, id);
		verify(storageMngrMock, never()).commit();
	}

	@Test
	public void testMergeAutoCommit() throws Exception {
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://pk");
		final Field f = OWLClassA.getStrAttField();
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);
		connection.setAutoCommit(true);

		connection.merge(pk, a, f, id);
		// The changes were committed, so there should be none now
		assertFalse(hasChangesField.getBoolean(connection));
		verify(storageMngrMock).merge(pk, a, f, id);
		verify(storageMngrMock).commit();
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNull() throws Exception {
		final URI pk = URI.create("http://pk");
		final Field f = OWLClassA.getStrAttField();
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);
		try {
			connection.merge(pk, null, f, id);
		} finally {
			assertFalse(hasChangesField.getBoolean(connection));
			verify(storageMngrMock, never()).merge(any(), any(), any(Field.class),
					any(EntityDescriptor.class));
			verify(storageMngrMock, never()).commit();
		}
	}

	@Test
	public void testPersist() throws Exception {
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://pk");
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);
		connection.setAutoCommit(false);

		connection.persist(pk, a, id);
		assertTrue(hasChangesField.getBoolean(connection));
		verify(storageMngrMock).persist(pk, a, id);
		verify(storageMngrMock, never()).commit();
	}

	@Test
	public void testPersistAutoCommit() throws Exception {
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://pk");
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);
		connection.setAutoCommit(true);

		connection.persist(pk, a, id);
		assertFalse(hasChangesField.getBoolean(connection));
		verify(storageMngrMock).persist(pk, a, id);
		verify(storageMngrMock).commit();
	}

	@Test
	public void testRemove() throws Exception {
		final URI pk = URI.create("http://pk");
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);
		connection.setAutoCommit(false);

		connection.remove(pk, id);
		assertTrue(hasChangesField.getBoolean(connection));
		verify(storageMngrMock).remove(pk, id);
		verify(storageMngrMock, never()).commit();
	}

	@Test
	public void testRemoveAutoCommit() throws Exception {
		final URI pk = URI.create("http://pk");
		final EntityDescriptor id = repos.get(1).createRepositoryID(false);
		connection.setAutoCommit(true);

		connection.remove(pk, id);
		assertFalse(hasChangesField.getBoolean(connection));
		verify(storageMngrMock).remove(pk, id);
		verify(storageMngrMock).commit();
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveNull() throws Exception {
		final EntityDescriptor id = repos.get(0).createRepositoryID(false);
		try {
			connection.remove(null, id);
		} finally {
			verify(storageMngrMock, never()).remove(any(), any(EntityDescriptor.class));
		}
	}

	@Test
	public void createStatement() throws Exception {
		final Statement stmt = connection.createStatement();
		assertNotNull(stmt);
	}

	@Test
	public void testGetRepositories() throws Exception {
		final List<Repository> res = connection.getRepositories();
		assertEquals(repos, res);
	}

	@Test
	public void testGetRepository() throws Exception {
		final Repository r = repos.get(3);
		final Repository res = connection.getRepository(r.getId());
		assertNotNull(res);
		assertEquals(r, res);
	}

	@Test(expected = RepositoryNotFoundException.class)
	public void testGetRepositoryTooSmall() throws Exception {
		final Integer val = -1;
		final Repository res = connection.getRepository(val);
		assert res == null;
	}

	@Test(expected = RepositoryNotFoundException.class)
	public void testGetRepositoryTooBig() throws Exception {
		final Integer val = Integer.MAX_VALUE;
		final Repository res = connection.getRepository(val);
		assert res == null;
	}
}
