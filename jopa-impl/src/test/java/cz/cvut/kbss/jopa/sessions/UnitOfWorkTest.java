package cz.cvut.kbss.jopa.sessions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;

public class UnitOfWorkTest {

	private static final URI CONTEXT_URI = URI.create("http://testContext");

	private static Set<Class<?>> managedTypes;
	private static Descriptor descriptor;

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
	@Mock
	private Attribute strAttMock;
	@Mock
	private TypesSpecification typesMock;
	@Mock
	private Attribute classAAttMock;
	@Mock
	private PropertiesSpecification propertiesMock;
	@Mock
	private Attribute strAttBMock;
	@Mock
	EntityManagerImpl emMock;
	@Mock
	EntityTransaction transactionMock;

	UnitOfWorkImpl uow;

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
		descriptor = new EntityDescriptor(CONTEXT_URI);
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
		when(metamodelMock.entity(OWLClassA.class)).thenReturn(typeA);
		when(metamodelMock.entity(OWLClassB.class)).thenReturn(typeB);
		when(metamodelMock.entity(OWLClassD.class)).thenReturn(typeD);
		when(typeA.getIdentifier()).thenReturn(idA);
		when(typeB.getIdentifier()).thenReturn(idB);
		when(typeD.getIdentifier()).thenReturn(idD);
		when(idA.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));
		when(idB.getJavaField()).thenReturn(OWLClassB.class.getDeclaredField("uri"));
		when(idD.getJavaField()).thenReturn(OWLClassD.class.getDeclaredField("uri"));
		when(typeA.getFieldSpecification(OWLClassA.getTypesField().getName()))
				.thenReturn(typesMock);
		when(typeD.getFieldSpecification(OWLClassD.getOwlClassAField().getName())).thenReturn(
				classAAttMock);
		when(typeB.getFieldSpecification(OWLClassB.getPropertiesField().getName())).thenReturn(
				propertiesMock);
		when(typeB.getFieldSpecification(OWLClassB.getStrAttField().getName())).thenReturn(
				strAttBMock);
		when(strAttMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
		when(typesMock.getJavaField()).thenReturn(OWLClassA.getTypesField());
		when(classAAttMock.getJavaField()).thenReturn(OWLClassD.getOwlClassAField());
		when(classAAttMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
		when(strAttBMock.getJavaField()).thenReturn(OWLClassB.getStrAttField());
		when(propertiesMock.getJavaField()).thenReturn(OWLClassB.getPropertiesField());
		when(emMock.getTransaction()).thenReturn(transactionMock);
		uow = new UnitOfWorkImpl(serverSessionMock);
		uow.setEntityManager(emMock);
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
	public void testReadObjectJustPersisted() throws Exception {
		uow.registerNewObject(entityA, descriptor);
		assertTrue(uow.contains(entityA));
		final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
		assertNotNull(res);
		assertSame(entityA, res);
	}

	@Test
	public void testCalculateNewObjects() throws Exception {
		uow.registerNewObject(entityA, descriptor);
		uow.registerNewObject(entityB, descriptor);
		uow.registerNewObject(entityD, descriptor);
		uow.commit();

		ArgumentCaptor<Object> pks = ArgumentCaptor.forClass(Object.class);
		verify(cacheManagerMock, times(3)).add(pks.capture(), any(Object.class), eq(CONTEXT_URI));
		final Set<URI> uris = new HashSet<>();
		for (Object pk : pks.getAllValues()) {
			uris.add(URI.create(pk.toString()));
		}
		assertTrue(uris.contains(entityA.getUri()));
		assertTrue(uris.contains(entityB.getUri()));
		assertTrue(uris.contains(entityD.getUri()));
	}

	@Test
	public void testCalculateDeletedObjects() throws Exception {
		final Object toRemove = uow.registerExistingObject(entityA, descriptor);
		uow.registerExistingObject(entityB, descriptor);
		uow.removeObject(toRemove);
		uow.commit();

		verify(cacheManagerMock).evict(OWLClassA.class, IRI.create(entityA.getUri()), CONTEXT_URI);
		verify(connectionMock).remove(IRI.create(entityA.getUri()), descriptor);
	}

	@Test
	public void testCalculateModificationsObjectProperty() throws Exception {
		final Attribute attMock = mock(Attribute.class);
		when(typeD.getFieldSpecification(OWLClassD.getOwlClassAField().getName())).thenReturn(
				attMock);
		when(attMock.getJavaField()).thenReturn(OWLClassD.getOwlClassAField());
		final OWLClassD d = new OWLClassD();
		d.setUri(URI.create("http://tempD"));
		final OWLClassA a = new OWLClassA();
		a.setUri(URI.create("http://oldA"));
		d.setOwlClassA(a);
		final OWLClassD clone = (OWLClassD) uow.registerExistingObject(d, descriptor);
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://newA"));
		newA.setStringAttribute("somestring");
		clone.setOwlClassA(newA);
		uow.registerNewObject(newA, descriptor);
		uow.commit();

		assertEquals(d.getOwlClassA().getUri(), newA.getUri());
		verify(cacheManagerMock).add(eq(IRI.create(newA.getUri())), any(Object.class),
				eq(CONTEXT_URI));
	}

	@Test
	public void testCalculateModificationsDataProperty() throws Exception {
		when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://newA"));
		newA.setStringAttribute("somestring");
		final OWLClassA clone = (OWLClassA) uow.registerExistingObject(newA, descriptor);
		// Trigger change, otherwise we would have to stub
		// OWLAPIPersistenceProvider's emfs and server session
		uow.setHasChanges();
		final String newStr = "newStr";
		clone.setStringAttribute(newStr);
		uow.commit();

		assertEquals(newStr, newA.getStringAttribute());
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
		uow.removeObjectFromCache(entityB, descriptor.getContext());
		verify(cacheManagerMock).evict(OWLClassB.class, IRI.create(entityB.getUri()),
				descriptor.getContext());
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

	@Test(expected = PrimaryKeyNotSetException.class)
	public void testRegisterNewObjectNullPkNotGenerated() throws Exception {
		final OWLClassB b = new OWLClassB();
		try {
			uow.registerNewObject(b, descriptor);
		} finally {
			verify(connectionMock, never()).persist(any(Object.class), any(Object.class),
					eq(descriptor));
		}
		fail("This line should not have been reached.");
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testRegisterNewObjectAlreadyExists() {
		uow.registerNewObject(entityB, descriptor);
		uow.registerNewObject(entityB, descriptor);
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

	@Test(expected = IllegalArgumentException.class)
	public void testRemoveObjectNotRegistered() {
		uow.removeObject(entityA);
		fail("This line should not have been reached.");
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
	public void revertObjectReference() throws Exception {
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

	@Test(expected = IllegalStateException.class)
	public void testCommitInactive() throws Exception {
		uow.release();
		uow.commit();
		fail("This line should not have been reached.");
	}

	@Test
	public void testRollback() throws Exception {
		uow.registerNewObject(entityA, descriptor);
		final Object clone = uow.registerExistingObject(entityB, descriptor);
		verify(connectionMock).persist(IRI.create(entityA.getUri()), entityA, descriptor);
		assertTrue(uow.contains(entityA));
		assertTrue(uow.contains(clone));

		uow.rollback();
		verify(connectionMock).rollback();
		assertFalse(uow.contains(entityA));
		assertFalse(uow.contains(clone));
	}

	@Test(expected = IllegalStateException.class)
	public void testRollbackInactive() throws Exception {
		uow.release();
		uow.rollback();
		fail("This line should not have been reached.");
	}

	@Test(expected = OWLPersistenceException.class)
	public void testCommitFailed() throws Exception {
		doThrow(IllegalStateException.class).when(connectionMock).commit();
		try {
			uow.commit();
		} finally {
			verify(emMock).removeCurrentPersistenceContext();
		}
	}

	@Test
	public void testClearCacheAfterCommit() throws Exception {
		uow.registerNewObject(entityA, descriptor);
		final Object clone = uow.registerExistingObject(entityB, descriptor);
		verify(connectionMock).persist(IRI.create(entityA.getUri()), entityA, descriptor);
		assertTrue(uow.contains(entityA));
		assertTrue(uow.contains(clone));
		uow.setShouldClearAfterCommit(true);
		uow.commit();

		verify(cacheManagerMock).evictAll();
	}

	@Test
	public void testLoadFieldValue() throws Exception {
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://bUri"));
		final String stringAtt = "string";
		final OWLClassB clone = (OWLClassB) uow.registerExistingObject(b, descriptor);
		assertNull(clone.getStringAttribute());
		final Field strField = OWLClassB.getStrAttField();
		strField.setAccessible(true);
		doAnswer(new Answer<Void>() {

			@Override
			public Void answer(InvocationOnMock invocation) throws Throwable {
				final Field f = (Field) invocation.getArguments()[1];
				f.set(invocation.getArguments()[0], stringAtt);
				return null;
			}

		}).when(connectionMock).loadFieldValue(clone, strField, descriptor);

		uow.loadEntityField(clone, strField);
		assertNotNull(clone.getStringAttribute());
		verify(connectionMock).loadFieldValue(clone, strField, descriptor);
	}

	@Test
	public void testLoadFieldValueManagedType() throws Exception {
		final OWLClassD d = new OWLClassD();
		d.setUri(URI.create("http://dUri"));
		final OWLClassD clone = (OWLClassD) uow.registerExistingObject(d, descriptor);
		assertNull(clone.getOwlClassA());
		final Field toLoad = OWLClassD.getOwlClassAField();
		doAnswer(new Answer<Void>() {

			@Override
			public Void answer(InvocationOnMock invocation) throws Throwable {
				final Field f = (Field) invocation.getArguments()[1];
				f.set(invocation.getArguments()[0], entityA);
				return null;
			}

		}).when(connectionMock).loadFieldValue(clone, toLoad, descriptor);

		uow.loadEntityField(clone, toLoad);
		assertNotNull(clone.getOwlClassA());
		// Verify that the loaded value was cloned
		assertNotSame(entityA, clone.getOwlClassA());
		assertTrue(uow.contains(clone.getOwlClassA()));
		verify(connectionMock).loadFieldValue(clone, toLoad, descriptor);
	}

	@Test(expected = OWLPersistenceException.class)
	public void testLoadFieldValueNotRegistered() throws Exception {
		try {
			uow.loadEntityField(entityB, OWLClassB.getStrAttField());
		} finally {
			verify(connectionMock, never()).loadFieldValue(any(Object.class),
					eq(OWLClassB.getStrAttField()), eq(descriptor));
		}
	}

	@Test
	public void testAttributeChanged() throws Exception {
		when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
		final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
		final Field strField = OWLClassA.getStrAttField();

		uow.attributeChanged(clone, strField);
		verify(connectionMock).merge(clone, strField, descriptor);
	}

	@Test(expected = OWLPersistenceException.class)
	public void testAttributeChangedNotRegistered() throws Exception {
		when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
		final Field strField = OWLClassA.getStrAttField();
		try {
			uow.attributeChanged(entityA, strField);
		} finally {
			verify(connectionMock, never()).merge(any(Object.class), eq(strField), eq(descriptor));
		}
		fail("This line should not have been reached.");
	}

	@Test(expected = IllegalStateException.class)
	public void testAttributeChangedOutsideTransaction() throws Exception {
		final Field strField = OWLClassA.getStrAttField();
		try {
			uow.attributeChanged(entityA, strField);
		} finally {
			verify(connectionMock, never()).merge(any(Object.class), eq(strField), eq(descriptor));
		}
		fail("This line should not have been reached.");
	}

	@Test
	public void testMergeDetachedExisting() throws Exception {
		mergeDetachedTest();
	}

	@SuppressWarnings("unchecked")
	private void mergeDetachedTest() throws Exception {
		when(connectionMock.contains(IRI.create(entityA.getUri()), CONTEXT_URI)).thenReturn(
				Boolean.TRUE);
		final Attribute<? super OWLClassA, ?> strAtt = mock(Attribute.class);
		when(strAtt.getJavaField()).thenReturn(OWLClassA.getStrAttField());
		@SuppressWarnings("rawtypes")
		final TypesSpecification typesAtt = mock(TypesSpecification.class);
		when(typesAtt.getJavaField()).thenReturn(OWLClassA.getTypesField());
		final Set<Attribute<? super OWLClassA, ?>> atts = new HashSet<>();
		atts.add(strAtt);
		when(typeA.getAttributes()).thenReturn(atts);
		when(typeA.getTypes()).thenReturn(typesAtt);
		final OWLClassA res = uow.mergeDetached(entityA, descriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		final ArgumentCaptor<Field> ac = ArgumentCaptor.forClass(Field.class);
		verify(connectionMock, atLeastOnce())
				.merge(any(Object.class), ac.capture(), eq(descriptor));
		final List<Field> mergedFields = ac.getAllValues();
		assertTrue(mergedFields.contains(OWLClassA.getStrAttField()));
		assertTrue(mergedFields.contains(OWLClassA.getTypesField()));
	}

	@Test
	public void testMergeDetachedEvictFromCache() throws Exception {
		when(cacheManagerMock.contains(OWLClassA.class, IRI.create(entityA.getUri()), CONTEXT_URI))
				.thenReturn(Boolean.TRUE);
		mergeDetachedTest();
		verify(cacheManagerMock).evict(OWLClassA.class, IRI.create(entityA.getUri()), CONTEXT_URI);
	}

	@Test
	public void testMergeDetachedNew() throws Exception {
		when(connectionMock.contains(IRI.create(entityA.getUri()), CONTEXT_URI)).thenReturn(
				Boolean.FALSE);
		final OWLClassA res = uow.mergeDetached(entityA, descriptor);
		assertNotNull(res);
		assertSame(entityA, res);
		verify(connectionMock).persist(IRI.create(entityA.getUri()), entityA, descriptor);
	}

	@Test
	public void testIsConsistent() throws Exception {
		when(connectionMock.isConsistent(CONTEXT_URI)).thenReturn(Boolean.TRUE);
		final boolean res = uow.isConsistent(CONTEXT_URI);
		assertTrue(res);
		verify(connectionMock).isConsistent(CONTEXT_URI);
	}

	@Test
	public void testGetContexts() throws Exception {
		final List<URI> contexts = new ArrayList<>(1);
		contexts.add(CONTEXT_URI);
		when(connectionMock.getContexts()).thenReturn(contexts);
		final List<URI> res = uow.getContexts();
		assertSame(contexts, res);
		assertEquals(contexts, res);
		verify(connectionMock).getContexts();
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
