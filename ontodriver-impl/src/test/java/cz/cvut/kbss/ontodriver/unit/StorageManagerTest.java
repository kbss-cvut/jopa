package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.impl.OntoDriverImpl;
import cz.cvut.kbss.ontodriver.impl.StorageManagerImpl;
import cz.cvut.kbss.ontodriver.utils.OWLClassA;

public class StorageManagerTest {

	private static List<Repository> repositories;

	@Mock
	private PersistenceProviderFacade facadeMock;
	@Mock
	private Metamodel metamodelMock;

	@Mock
	private OntoDriverImpl driverMock;

	@Mock
	private DriverFactory factoryMock;

	@Mock
	private StorageModule moduleMock;

	private StorageManager manager;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		repositories = new ArrayList<>(4);
		for (int i = 0; i < 4; i++) {
			repositories.add(new Repository(URI.create("http://testing-repository.org/v/" + i)));
		}
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(driverMock.getFactory(any(Repository.class))).thenReturn(factoryMock);
		when(factoryMock.createStorageModule(any(Repository.class), eq(facadeMock), anyBoolean()))
				.thenReturn(moduleMock);
		when(moduleMock.contains(any(), any(RepositoryID.class))).thenReturn(Boolean.FALSE);
		when(facadeMock.getMetamodel()).thenReturn(metamodelMock);

		this.manager = new StorageManagerImpl(facadeMock, repositories, driverMock);
	}

	@Test(expected = NullPointerException.class)
	public void testStorageManagerImplNull() {
		final StorageManager res = new StorageManagerImpl(null, repositories, null);
		// This shouldn't be reached
		assert res == null;
	}

	@Test(expected = IllegalArgumentException.class)
	public void testStorageManagerImplEmptyRepositories() {
		final StorageManager res = new StorageManagerImpl(facadeMock,
				Collections.<Repository> emptyList(), driverMock);
		// This shouldn't be reached
		assert res == null;
	}

	@Test
	public void testClose() throws Exception {
		final RepositoryID rid = repositories.get(0).createIdentifier();
		final URI pk = URI.create("http://blabla");
		// Just to trigger module creation
		manager.contains(pk, rid);

		assertTrue(manager.isOpen());
		manager.close();
		assertFalse(manager.isOpen());
		verify(factoryMock).releaseStorageModule(moduleMock);
	}

	@Test
	public void testContains() throws Exception {
		final RepositoryID rid = repositories.get(0).createIdentifier();
		final URI pk = URI.create("http://do-you-contain-me");
		when(moduleMock.contains(pk, rid)).thenReturn(Boolean.TRUE);

		final boolean res = manager.contains(pk, rid);
		assertTrue(res);
		verify(factoryMock).createStorageModule(eq(repositories.get(0)), eq(facadeMock),
				anyBoolean());
		verify(moduleMock).contains(pk, rid);
	}

	@Test(expected = IllegalStateException.class)
	public void testContainsOnClosed() throws Exception {
		final RepositoryID rid = repositories.get(0).createIdentifier();
		final URI pk = URI.create("http://do-you-contain-me");
		when(moduleMock.contains(pk, rid)).thenReturn(Boolean.TRUE);

		manager.close();
		manager.contains(pk, rid);
	}

	@Test
	public void testFind() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		final OWLClassA a = new OWLClassA();
		when(moduleMock.find(OWLClassA.class, pk, descriptor)).thenReturn(a);

		final OWLClassA res = manager.find(OWLClassA.class, pk, descriptor);
		assertNotNull(res);
		verify(factoryMock).createStorageModule(eq(repositories.get(0)), eq(facadeMock),
				anyBoolean());
		verify(moduleMock).find(OWLClassA.class, pk, descriptor);
	}

	@Test
	public void testFindTwice() throws Exception {
		// Calls find twice and verifies that the module is created only once
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		final OWLClassA a = new OWLClassA();
		when(moduleMock.find(OWLClassA.class, pk, descriptor)).thenReturn(a);

		final OWLClassA res = manager.find(OWLClassA.class, pk, descriptor);
		assertNotNull(res);
		final OWLClassA resTwo = manager.find(OWLClassA.class, pk, descriptor);
		assertNotNull(resTwo);
		assertSame(res, resTwo);
		verify(factoryMock, atMost(1)).createStorageModule(eq(repositories.get(0)), eq(facadeMock),
				anyBoolean());
		verify(moduleMock, times(2)).find(OWLClassA.class, pk, descriptor);
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testFindNull() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		when(moduleMock.find(any(Class.class), any(), any(EntityDescriptor.class))).thenThrow(
				UnsupportedOperationException.class);
		try {
			final OWLClassA res = manager.find(OWLClassA.class, null, descriptor);
			// This shouldn't be reached
			assert res == null;
		} finally {
			verify(moduleMock, never()).find(any(Class.class), any(), any(EntityDescriptor.class));
		}
	}

	@Test
	public void testFindUnknown() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		when(moduleMock.find(OWLClassA.class, pk, descriptor)).thenReturn(null);

		final OWLClassA res = manager.find(OWLClassA.class, pk, descriptor);
		assertNull(res);
		verify(factoryMock).createStorageModule(eq(repositories.get(0)), eq(facadeMock),
				anyBoolean());
		verify(moduleMock).find(OWLClassA.class, pk, descriptor);
	}

	@Test
	public void testIsConsistent() throws Exception {
		final RepositoryID rid = repositories.get(1).createIdentifier();
		when(moduleMock.isConsistent(rid)).thenReturn(Boolean.TRUE);

		final boolean res = manager.isConsistent(rid);
		assertTrue(res);
		verify(moduleMock).isConsistent(rid);
	}

	@Test
	public void testGetRepositories() {
		final List<Repository> res = manager.getRepositories();
		assertEquals(repositories, res);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testGetRepositoriesModify() {
		final List<Repository> res = manager.getRepositories();
		assertFalse(res.isEmpty());
		res.remove(0);
	}

	@Test
	public void testLoadFieldValue() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();

		manager.loadFieldValue(a, f, descriptor);
		verify(moduleMock).loadFieldValue(a, f, descriptor);
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNull() throws Exception {
		final EntityDescriptor rid = repositories.get(0).createDescriptor();
		final Field f = OWLClassA.getStrAttField();

		try {
			manager.loadFieldValue(null, f, rid);
		} finally {
			verify(moduleMock, never()).loadFieldValue(any(), eq(f), eq(rid));
		}
	}

	@Test
	public void testMerge() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.merge(a, f, descriptor);
		verify(moduleMock).merge(a, f, descriptor);
		assertFalse(changes.isEmpty());
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNull() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final Field f = OWLClassA.getStrAttField();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());
		try {
			manager.merge(null, f, descriptor);
		} finally {
			assertTrue(changes.isEmpty());
			verify(moduleMock, never()).merge(any(), eq(f), eq(descriptor));
		}
	}

	@Test
	public void testPersist() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		final OWLClassA a = new OWLClassA();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.persist(pk, a, descriptor);
		assertFalse(changes.isEmpty());

		verify(moduleMock).persist(pk, a, descriptor);
	}

	@Test
	public void testPersistNullPk() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final OWLClassA a = new OWLClassA();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.persist(null, a, descriptor);
		assertFalse(changes.isEmpty());

		verify(moduleMock).persist(any(), eq(a), eq(descriptor));
	}

	@Test(expected = IllegalStateException.class)
	public void testPersistOnClosed() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		final OWLClassA a = new OWLClassA();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		try {
			manager.close();
			manager.persist(pk, a, descriptor);
		} finally {
			assertTrue(changes.isEmpty());
			verify(factoryMock, never()).createStorageModule(any(Repository.class), eq(facadeMock),
					anyBoolean());
			verify(moduleMock, never()).persist(any(), any(), any(EntityDescriptor.class));
		}
	}

	@Test
	public void testRemove() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.remove(pk, descriptor);
		assertFalse(changes.isEmpty());
		assertTrue(changes.containsValue(moduleMock));
		verify(moduleMock).remove(pk, descriptor);
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveNullPk() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		try {
			manager.remove(null, descriptor);
		} finally {
			assertTrue(changes.isEmpty());
			verify(moduleMock, never()).remove(any(), any(EntityDescriptor.class));
		}
	}

	@Test
	public void testCommitNoChanges() throws Exception {
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.commit();

		verify(moduleMock, never()).commit();
	}

	@Test
	public void testCommit() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.remove(pk, descriptor);
		assertFalse(changes.isEmpty());
		manager.commit();
		assertTrue(changes.isEmpty());
		verify(moduleMock).commit();
	}

	@Test
	public void testRollback() throws Exception {
		final EntityDescriptor descriptor = repositories.get(0).createDescriptor();
		final URI pk = URI.create("http://do-you-contain-me");
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<EntityDescriptor, StorageModule> changes = (Map<EntityDescriptor, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.remove(pk, descriptor);
		assertFalse(changes.isEmpty());
		manager.rollback();
		assertTrue(changes.isEmpty());
		verify(moduleMock).remove(pk, descriptor);
		verify(moduleMock).rollback();
	}

	@Test
	public void testExecuteStatement() throws Exception {
		final RepositoryID rid = repositories.get(0).createIdentifier();
		final JopaStatement stmtMock = mock(JopaStatement.class);
		final ResultSet rsMock = mock(ResultSet.class);
		when(stmtMock.getRepositoryId()).thenReturn(rid);
		when(moduleMock.executeStatement(stmtMock)).thenReturn(rsMock);

		final ResultSet rs = manager.executeStatement(stmtMock);
		assertNotNull(rs);
		assertSame(rsMock, rs);
		verify(moduleMock).executeStatement(stmtMock);
	}

	@Test(expected = NullPointerException.class)
	public void testExecuteStatementNullRepoId() throws Exception {
		final JopaStatement stmtMock = mock(JopaStatement.class);
		final ResultSet rsMock = mock(ResultSet.class);
		when(stmtMock.getRepositoryId()).thenReturn(null);
		when(moduleMock.executeStatement(stmtMock)).thenReturn(rsMock);
		try {
			final ResultSet rs = manager.executeStatement(stmtMock);
			// This shouldn't be reached
			assert rs == null;
		} finally {
			verify(moduleMock, never()).executeStatement(any(JopaStatement.class));
		}
	}
}
