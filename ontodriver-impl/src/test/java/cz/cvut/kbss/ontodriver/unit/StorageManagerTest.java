package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.never;
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

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
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
	private DriverFactory factoryMoc;

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
		when(driverMock.getFactory(any(RepositoryID.class))).thenReturn(factoryMoc);
		when(factoryMoc.createStorageModule(any(RepositoryID.class), eq(facadeMock), anyBoolean()))
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
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final URI pk = URI.create("http://blabla");
		// Just to trigger module creation
		manager.contains(pk, rid);

		assertTrue(manager.isOpen());
		manager.close();
		assertFalse(manager.isOpen());
		verify(factoryMoc).releaseStorageModule(moduleMock);
	}

	@Test
	public void testContains() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final URI pk = URI.create("http://do-you-contain-me");
		when(moduleMock.contains(pk, rid)).thenReturn(Boolean.TRUE);

		final boolean res = manager.contains(pk, rid);
		assertTrue(res);
		verify(factoryMoc).createStorageModule(eq(rid), eq(facadeMock), anyBoolean());
		verify(moduleMock).contains(pk, rid);
	}

	@Test(expected = IllegalStateException.class)
	public void testContainsOnClosed() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final URI pk = URI.create("http://do-you-contain-me");
		when(moduleMock.contains(pk, rid)).thenReturn(Boolean.TRUE);

		manager.close();
		manager.contains(pk, rid);
	}

	@Test
	public void testFind() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final URI pk = URI.create("http://do-you-contain-me");
		final OWLClassA a = new OWLClassA();
		when(moduleMock.find(OWLClassA.class, pk, rid)).thenReturn(a);

		final OWLClassA res = manager.find(OWLClassA.class, pk, rid);
		assertNotNull(res);
		verify(factoryMoc).createStorageModule(eq(rid), eq(facadeMock), anyBoolean());
		verify(moduleMock).find(OWLClassA.class, pk, rid);
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testFindNull() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		when(moduleMock.find(any(Class.class), any(), any(RepositoryID.class))).thenThrow(
				UnsupportedOperationException.class);
		try {
			final OWLClassA res = manager.find(OWLClassA.class, null, rid);
			// This shouldn't be reached
			assert res == null;
		} finally {
			verify(moduleMock, never()).find(any(Class.class), any(), any(RepositoryID.class));
		}
	}

	@Test
	public void testFindUnknown() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final URI pk = URI.create("http://do-you-contain-me");
		when(moduleMock.find(OWLClassA.class, pk, rid)).thenReturn(null);

		final OWLClassA res = manager.find(OWLClassA.class, pk, rid);
		assertNull(res);
		verify(factoryMoc).createStorageModule(eq(rid), eq(facadeMock), anyBoolean());
		verify(moduleMock).find(OWLClassA.class, pk, rid);
	}

	@Test
	public void testIsConsistent() throws Exception {
		final RepositoryID rid = repositories.get(1).createRepositoryID(false);
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
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();

		manager.loadFieldValue(a, f, rid);
		verify(moduleMock).loadFieldValue(a, f, rid);
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNull() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final Field f = OWLClassA.getStrAttField();

		try {
			manager.loadFieldValue(null, f, rid);
		} finally {
			verify(moduleMock, never()).loadFieldValue(any(), eq(f), eq(rid));
		}
	}

	@Test
	public void testMerge() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final URI pk = URI.create("http://do-you-contain-me");
		final OWLClassA a = new OWLClassA();
		final Field f = OWLClassA.getStrAttField();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<RepositoryID, StorageModule> changes = (Map<RepositoryID, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());

		manager.merge(pk, a, f, rid);
		verify(moduleMock).merge(pk, a, f, rid);
		assertFalse(changes.isEmpty());
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNull() throws Exception {
		final RepositoryID rid = repositories.get(0).createRepositoryID(false);
		final URI pk = URI.create("http://do-you-contain-me");
		final Field f = OWLClassA.getStrAttField();
		final Field changesField = StorageManagerImpl.class.getDeclaredField("modulesWithChanges");
		changesField.setAccessible(true);
		@SuppressWarnings("unchecked")
		final Map<RepositoryID, StorageModule> changes = (Map<RepositoryID, StorageModule>) changesField
				.get(manager);
		assertTrue(changes.isEmpty());
		try {
			manager.merge(pk, null, f, rid);
		} finally {
			assertTrue(changes.isEmpty());
			verify(moduleMock, never()).merge(eq(pk), any(), eq(f), eq(rid));
		}
	}
}
