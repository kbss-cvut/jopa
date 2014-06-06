package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.ontodriver.Connection;

public class UnitOfWorkTest {

	private static final URI CONTEXT_URI = URI.create("http://testContext");

	private static Set<Class<?>> managedTypes;
	private static EntityDescriptor descriptor;

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;

	private ServerSessionStub serverSessionMock;

	@Mock
	private Metamodel metamodelMock;

	@Mock
	private CacheManager cacheManagerMock;
	@Mock
	private Connection connectionMock;
	@Mock
	private EntityType<OWLClassA> typeA;
	@Mock
	private EntityType<OWLClassB> typeB;
	@Mock
	private EntityType<OWLClassD> typeD;
	@Mock
	private Identifier idA;
	@Mock
	private Identifier idB;
	@Mock
	private Identifier idD;

	UnitOfWorkImpl uow;
	EntityManagerImpl em;

	@BeforeClass
	public static void setUpBeforeClass() {
		final URI pkOne = URI.create("http://testOne");
		entityA = new OWLClassA();
		entityA.setUri(pkOne);
		entityA.setStringAttribute("attribute");
		entityA.setTypes(new HashSet<String>());
		final URI pkTwo = URI.create("http://testTwo");
		entityB = new OWLClassB();
		entityB.setUri(pkTwo);
		final URI pkThree = URI.create("http://testThree");
		entityD = new OWLClassD();
		entityD.setUri(pkThree);
		entityD.setOwlClassA(entityA);
		descriptor = EntityDescriptor.createWithEntityContext(CONTEXT_URI);
		managedTypes = new HashSet<>();
		managedTypes.add(OWLClassA.class);
		managedTypes.add(OWLClassB.class);
		managedTypes.add(OWLClassD.class);
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		final ServerSessionStub ssStub = new ServerSessionStub(connectionMock);
		this.serverSessionMock = spy(ssStub);
		when(serverSessionMock.getMetamodel()).thenReturn(metamodelMock);
		when(serverSessionMock.getManagedTypes()).thenReturn(managedTypes);
		when(serverSessionMock.getLiveObjectCache()).thenReturn(cacheManagerMock);
		when(cacheManagerMock.acquireReadLock()).thenReturn(Boolean.TRUE);
		when(cacheManagerMock.acquireWriteLock()).thenReturn(Boolean.TRUE);
		when(metamodelMock.entity(OWLClassA.class)).thenReturn(typeA);
		when(metamodelMock.entity(OWLClassB.class)).thenReturn(typeB);
		when(metamodelMock.entity(OWLClassD.class)).thenReturn(typeD);
		when(typeA.getIdentifier()).thenReturn(idA);
		when(typeB.getIdentifier()).thenReturn(idB);
		when(typeD.getIdentifier()).thenReturn(idD);
		when(idA.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
		when(idB.getJavaField()).thenReturn(OWLClassB.class.getDeclaredField("uri"));
		when(idD.getJavaField()).thenReturn(OWLClassD.class.getDeclaredField("uri"));
		uow = new UnitOfWorkImpl(serverSessionMock);
		final Field connectionField = UnitOfWorkImpl.class.getDeclaredField("storageConnection");
		connectionField.setAccessible(true);
		connectionField.set(uow, connectionMock);
	}

	@Test
	public void testReadObjectFromCache() {
		when(cacheManagerMock.get(OWLClassA.class, entityA.getUri(), CONTEXT_URI)).thenReturn(
				entityA);
		OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
		assertNotNull(res);
		assertEquals(res.getStringAttribute(), entityA.getStringAttribute());
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testReadObectNullPrimaryKey() {
		try {
			uow.readObject(entityA.getClass(), null, descriptor);
		} finally {
			verify(cacheManagerMock, never()).get(any(Class.class), any(), any(URI.class));
		}
		fail("This line should not have been reached.");
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testReadObjectNullClass() {
		try {
			uow.readObject(null, entityB.getUri(), descriptor);
		} finally {
			verify(cacheManagerMock, never()).get(any(Class.class), any(), any(URI.class));
		}
		fail("This line should not have been reached.");
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testReadObjectNullContext() {
		try {
			uow.readObject(entityA.getClass(), entityA.getUri(), null);
		} finally {
			verify(cacheManagerMock, never()).get(any(Class.class), any(), any(URI.class));
		}
		fail("This line should not have been reached.");
	}

	@Test
	public void testReadObjectFromOntology() throws Exception {
		when(connectionMock.find(OWLClassA.class, entityA.getUri(), descriptor))
				.thenReturn(entityA);
		OWLClassA res = this.uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		verify(cacheManagerMock).get(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
	}

	@Test
	public void testCalculateChanges() {
		fail("Not yet implemented");
	}

	@Test
	public void testCalculateNewObjects() {
		fail("Not yet implemented");
	}

	@Test
	public void testContains() throws Exception {
		OWLClassA res = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		assertNotNull(res);
		assertTrue(uow.contains(res));
	}

	@Test
	public void testGetState() throws Exception {
		assertEquals(State.NOT_MANAGED, uow.getState(entityA));
		OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		assertEquals(State.MANAGED, uow.getState(toRemove));
		uow.removeObject(toRemove);
		assertEquals(State.REMOVED, uow.getState(toRemove));
		final OWLClassA stateTest = new OWLClassA();
		final URI pk = URI.create("http://stateTest");
		stateTest.setUri(pk);
		uow.registerNewObject(stateTest, descriptor);
		assertEquals(State.MANAGED_NEW, uow.getState(stateTest));
	}

	@Test
	public void testGetStateWithDescriptor() throws Exception {
		assertEquals(State.NOT_MANAGED, uow.getState(entityA, descriptor));
		OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		assertEquals(State.MANAGED, uow.getState(toRemove, descriptor));
		uow.removeObject(toRemove);
		assertEquals(State.REMOVED, uow.getState(toRemove, descriptor));
		final OWLClassA stateTest = new OWLClassA();
		final URI pk = URI.create("http://stateTest");
		stateTest.setUri(pk);
		uow.registerNewObject(stateTest, descriptor);
		assertEquals(State.MANAGED_NEW, uow.getState(stateTest, descriptor));
	}

	@Test
	public void testGetOriginal() throws Exception {
		when(connectionMock.find(OWLClassA.class, entityA.getUri(), descriptor))
				.thenReturn(entityA);
		OWLClassA tO = this.uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
		assertNotNull(tO);
		OWLClassA origOne = (OWLClassA) uow.getOriginal(tO);
		assertSame(entityA, origOne);
		OWLClassA origTwo = (OWLClassA) uow.getOriginal(tO);
		assertSame(origOne, origTwo);
	}

	@Test
	public void testGetOriginalNull() {
		assertNull(uow.getOriginal(null));
	}

	@Test
	public void testIsObjectNew() throws Exception {
		final OWLClassA testNew = new OWLClassA();
		final URI pk = URI.create("http://testNewOne");
		testNew.setUri(pk);
		uow.registerNewObject(testNew, descriptor);
		assertTrue(uow.isObjectNew(testNew));
		verify(connectionMock).persist(IRI.create(pk), testNew, descriptor);
	}

	@Test
	public void testIsObjectNewWithNullAndManaged() {
		assertFalse(uow.isObjectNew(null));
		OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		assertFalse(uow.isObjectNew(managed));
	}

	@Test
	public void testIsObjectManaged() {
		OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		assertTrue(uow.isObjectManaged(managed));
	}

	@Test(expected = NullPointerException.class)
	public void testIsObjectManagerNull() {
		uow.isObjectManaged(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testMergeChangesIntoParent() {
		fail("Not yet implemented");
	}

	@Test
	public void testRegisterExistingObject() {
		OWLClassB clone = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
		assertNotNull(clone);
		assertEquals(entityB.getUri(), clone.getUri());
		assertTrue(uow.contains(clone));
		assertSame(entityB, uow.getOriginal(clone));
	}

	/**
	 * This method tests the situation when the Unit of Work has no clone to
	 * originals mapping - it was cleared. This tests the second branch of the
	 * register method.
	 */
	@Test
	public void testRegisterExistingObjectTwice() {
		OWLClassB clone = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
		assertNotNull(clone);
		assertEquals(entityB.getUri(), clone.getUri());
		final OWLClassB cloneTwo = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
		assertSame(clone, cloneTwo);
	}

	@Test
	public void testRemoveObjectFromCache() {
		uow.removeObjectFromCache(entityB, descriptor.getEntityContext());
		verify(cacheManagerMock).evict(OWLClassB.class, IRI.create(entityB.getUri()),
				descriptor.getEntityContext());
	}

	@Test
	public void testRegisterNewObject() throws Exception {
		final OWLClassA newOne = new OWLClassA();
		final URI pk = URI.create("http://newEntity");
		newOne.setUri(pk);
		newOne.setStringAttribute("stringAttributeOne");
		uow.registerNewObject(newOne, descriptor);
		assertTrue(uow.getNewObjectsCloneToOriginal().containsKey(newOne));
		verify(connectionMock).persist(IRI.create(pk), newOne, descriptor);
	}

	@Test(expected = NullPointerException.class)
	public void testRegisterNewObjectNull() {
		uow.registerNewObject(null, descriptor);
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testRegisterNewObjectNullDescriptor() {
		uow.registerNewObject(entityA, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testReleaseUnitOfWork() throws Exception {
		assertTrue(uow.isActive());
		uow.release();
		assertFalse(uow.isActive());
		verify(connectionMock).close();
	}

	@Test
	public void testRemoveObject() throws Exception {
		final OWLClassB toRemove = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
		uow.removeObject(toRemove);
		assertTrue(uow.getDeletedObjects().containsKey(toRemove));
		assertFalse(uow.contains(toRemove));
		verify(connectionMock).remove(IRI.create(entityB.getUri()), descriptor);
	}

	@Test
	public void testRemoveNewObject() {
		final OWLClassB newOne = new OWLClassB();
		final URI pk = URI.create("http://testObject");
		newOne.setUri(pk);
		newOne.setStringAttribute("strAtt");
		this.uow.registerNewObject(newOne, descriptor);
		assertTrue(uow.contains(newOne));
		// Now try to remove it
		uow.removeObject(newOne);
		assertFalse(uow.contains(newOne));
	}

	@Test
	public void testUnregisterObject() {
		final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		assertTrue(uow.contains(managed));
		this.uow.unregisterObject(managed);
		assertFalse(uow.contains(managed));
	}

	@Test
	public void testRevertObject() {
		final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		assertNotNull(managed);
		final String changedAtt = "changedAtt";
		managed.setStringAttribute(changedAtt);
		this.uow.revertObject(managed);
		assertEquals(entityA.getStringAttribute(), managed.getStringAttribute());
	}

	@Test
	public void revertObjectReference() {
		OWLClassD clone = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
		OWLClassA changedRef = new OWLClassA();
		final URI pk = URI.create("http://changedOne");
		changedRef.setStringAttribute("changedAtt");
		changedRef.setUri(pk);
		clone.setOwlClassA(changedRef);
		this.uow.revertObject(clone);
		assertEquals(entityA.getUri(), clone.getOwlClassA().getUri());
		assertEquals(entityA.getStringAttribute(), clone.getOwlClassA().getStringAttribute());
		 assertNotNull(clone.getOwlClassA().getTypes());
	}

	@Test
	public void testUseTransactionalOntologyForQueryProcessing() {
		assertTrue(uow.useTransactionalOntologyForQueryProcessing());
		assertFalse(uow.useBackupOntologyForQueryProcessing());
	}

	@Test
	public void testSwitchQueryProcessingOntology() {
		uow.setUseBackupOntologyForQueryProcessing();
		assertTrue(uow.useBackupOntologyForQueryProcessing());
		assertFalse(uow.useTransactionalOntologyForQueryProcessing());
		uow.setUseTransactionalOntologyForQueryProcessing();
		assertTrue(uow.useTransactionalOntologyForQueryProcessing());
		assertFalse(uow.useBackupOntologyForQueryProcessing());
	}

	private static class ServerSessionStub extends ServerSession {

		private Connection connection;

		private ServerSessionStub(Connection conn) {
			this.connection = conn;
		}

		protected Connection acquireConnection() {
			return connection;
		}

		@Override
		public Metamodel getMetamodel() {
			// Just exporting API as public so that we can stub it with Mockito
			return null;
		}

		@Override
		protected synchronized void registerEntityWithPersistenceContext(Object entity,
				UnitOfWorkImpl uow) {
			// do nothing
		}

	}
}
