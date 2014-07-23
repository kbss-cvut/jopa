package cz.cvut.kbss.ontodriver.unit.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
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
import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStatement;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageConnector;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageModule;
import cz.cvut.kbss.ontodriver.utils.DriverFactoryStub;
import cz.cvut.kbss.ontodriver.utils.OWLClassA;

public class OwlapiStorageModuleTest {

	private static int INITIAL_PK_COUNTER = 1;

	private static DriverFactoryStub factory;
	private static OWLClassA entity;
	private static EntityDescriptor descriptor;

	@Mock
	private ModuleInternal<OWLOntologyChange, OwlapiStatement> internalMock;

	@Mock
	private OwlapiStorageConnector connectorMock;

	@Mock
	private OwlapiStatement statementMock;

	@Mock
	private PersistenceProviderFacade provider;

	private DriverFactoryStub factorySpy;

	private OwlapiStorageModule module;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		factory = new DriverFactoryStub();
		entity = new OWLClassA();
		entity.setUri(URI.create("http://entityA"));
		entity.setStringAttribute("someString");
		descriptor = new EntityDescriptor();
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		factory.setStorageConnector(connectorMock);
		factory.setStatement(statementMock);
		factorySpy = spy(factory);
		when(connectorMock.getClassAssertionsCount()).thenReturn(INITIAL_PK_COUNTER);
		when(connectorMock.getOntologyData()).thenReturn(
				OwlapiConnectorDataHolder.language("en").build());
		module = new OwlapiStorageModule(provider, factorySpy);
		final Field internalField = module.getClass().getSuperclass().getDeclaredField("internal");
		internalField.setAccessible(true);
		internalField.set(module, internalMock);
	}

	@Test
	public void testOwlapiStorageModule() throws Exception {
		final DriverFactoryStub fact = spy(factory);
		final OwlapiStorageModule m = new OwlapiStorageModule(provider, fact);
		assertNotNull(m);
		verify(fact).createStorageConnector();
		// Twice, once in the setUp method
		verify(connectorMock, times(2)).getClassAssertionsCount();
		verify(connectorMock, times(2)).getOntologyData();
	}

	@Test(expected = NullPointerException.class)
	public void testOwlapiStorageModuleNullProvider() throws Exception {
		final OwlapiStorageModule m = new OwlapiStorageModule(null, factory);
		// shouldn't be reached
		assert m == null;
	}

	@Test(expected = NullPointerException.class)
	public void testOwlapiStorageModuleNullFactory() throws Exception {
		final OwlapiStorageModule m = new OwlapiStorageModule(provider, null);
		// shouldn't be reached
		assert m == null;
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(module.isOpen());
		module.close();
		verify(factorySpy).releaseStorageConnector(connectorMock);
		assertFalse(module.isOpen());
	}

	@Test
	public void testCommit() throws Exception {
		final List<OWLOntologyChange> changes = new ArrayList<>();
		when(internalMock.commitAndRetrieveChanges()).thenReturn(changes);
		// Call persist to initiate a transaction
		module.persist(entity.getUri(), entity, descriptor);
		module.commit();
		verify(internalMock).commitAndRetrieveChanges();
		verify(connectorMock).applyChanges(changes);
		verify(connectorMock).saveWorkingOntology();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitOutsideTransaction() throws Exception {
		try {
			module.commit();
		} finally {
			verify(internalMock, never()).commitAndRetrieveChanges();
			verify(connectorMock, never()).saveWorkingOntology();
		}
	}

	@Test
	public void testGetOntologyData() throws Exception {
		module.getOntologyData();
		// Twice, first in initialization
		verify(connectorMock, times(2)).getOntologyData();
	}

	@Test
	public void testContains() throws Exception {
		when(internalMock.containsEntity(eq(entity.getUri()), any(URI.class))).thenReturn(
				Boolean.TRUE);
		final boolean res = module.contains(entity.getUri(), null);
		assertTrue(res);
		verify(internalMock).containsEntity(eq(entity.getUri()), any(URI.class));
	}

	@Test(expected = NullPointerException.class)
	public void testContainsNullPk() throws Exception {
		try {
			module.contains(null, null);
		} finally {
			verify(internalMock, never()).containsEntity(any(), any(URI.class));
		}
	}

	@Test
	public void testFind() throws Exception {
		when(internalMock.findEntity(OWLClassA.class, entity.getUri(), descriptor)).thenReturn(
				entity);
		final OWLClassA res = module.find(OWLClassA.class, entity.getUri(), descriptor);
		assertNotNull(res);
		assertSame(entity, res);
		verify(internalMock).findEntity(OWLClassA.class, entity.getUri(), descriptor);
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testFindNullClass() throws Exception {
		try {
			module.find(null, entity.getUri(), descriptor);
		} finally {
			verify(internalMock, never()).findEntity(any(Class.class), any(),
					any(EntityDescriptor.class));
		}
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testFindNullPk() throws Exception {
		try {
			module.find(OWLClassA.class, null, descriptor);
		} finally {
			verify(internalMock, never()).findEntity(any(Class.class), any(),
					any(EntityDescriptor.class));
		}
	}

	@SuppressWarnings("unchecked")
	@Test(expected = NullPointerException.class)
	public void testFindNullDescriptor() throws Exception {
		try {
			module.find(OWLClassA.class, entity.getUri(), null);
		} finally {
			verify(internalMock, never()).findEntity(any(Class.class), any(),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testGetContexts() {
		// OWLAPI modules return an empty list because they don't use contexts
		final List<URI> res = module.getContexts();
		assertNotNull(res);
		assertTrue(res.isEmpty());
	}

	@Test
	public void testIsConsistent() throws Exception {
		when(internalMock.isConsistent(any(URI.class))).thenReturn(Boolean.TRUE);
		final boolean res = module.isConsistent(null);
		assertTrue(res);
		final boolean res2 = module.isConsistent(URI.create("http://contextUri"));
		assertTrue(res2);
		verify(internalMock, times(2)).isConsistent(any(URI.class));
	}

	@Test
	public void testLoadFieldValue() throws Exception {
		final Field field = OWLClassA.getStrAttField();
		module.loadFieldValue(entity, field, descriptor);
		verify(internalMock).loadFieldValue(entity, field, descriptor);
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNullEntity() throws Exception {
		final Field field = OWLClassA.getStrAttField();
		try {
			module.loadFieldValue(null, field, descriptor);
		} finally {
			verify(internalMock, never()).loadFieldValue(any(OWLClassA.class), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNullField() throws Exception {
		try {
			module.loadFieldValue(entity, null, descriptor);
		} finally {
			verify(internalMock, never()).loadFieldValue(any(OWLClassA.class), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNullDescriptor() throws Exception {
		final Field field = OWLClassA.getStrAttField();
		try {
			module.loadFieldValue(entity, field, null);
		} finally {
			verify(internalMock, never()).loadFieldValue(any(OWLClassA.class), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testMerge() throws Exception {
		final Field field = OWLClassA.getStrAttField();
		module.merge(entity, field, descriptor);
		verify(internalMock).mergeEntity(entity, field, descriptor);
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNullEntity() throws Exception {
		final Field field = OWLClassA.getStrAttField();
		try {
			module.merge(null, field, descriptor);
		} finally {
			verify(internalMock, never()).mergeEntity(any(OWLClassA.class), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNullField() throws Exception {
		try {
			module.merge(entity, null, descriptor);
		} finally {
			verify(internalMock, never()).mergeEntity(any(OWLClassA.class), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNullDescriptor() throws Exception {
		final Field field = OWLClassA.getStrAttField();
		try {
			module.merge(entity, field, null);
		} finally {
			verify(internalMock, never()).mergeEntity(any(OWLClassA.class), any(Field.class),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testPersist() throws Exception {
		module.persist(entity.getUri(), entity, descriptor);
		verify(internalMock).persistEntity(entity.getUri(), entity, descriptor);

		module.persist(null, entity, descriptor);
		verify(internalMock).persistEntity(null, entity, descriptor);
	}

	@Test(expected = IllegalStateException.class)
	public void testPersistOnClosed() throws Exception {
		try {
			module.close();
			module.persist(entity.getUri(), entity, descriptor);
		} finally {
			verify(internalMock, never()).persistEntity(any(), any(OWLClassA.class),
					any(EntityDescriptor.class));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNullEntity() throws Exception {
		try {
			module.persist(entity.getUri(), null, descriptor);
		} finally {
			verify(internalMock, never()).persistEntity(any(), any(OWLClassA.class),
					any(EntityDescriptor.class));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNullDescriptor() throws Exception {
		try {
			module.persist(entity.getUri(), entity, null);
		} finally {
			verify(internalMock, never()).persistEntity(any(), any(OWLClassA.class),
					any(EntityDescriptor.class));
		}
	}

	@Test
	public void testRemove() throws Exception {
		module.remove(entity.getUri(), descriptor);
		verify(internalMock).removeEntity(entity.getUri(), descriptor);
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveNullPk() throws Exception {
		try {
			module.remove(null, descriptor);
		} finally {
			verify(internalMock, never()).removeEntity(any(URI.class), any(EntityDescriptor.class));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveNullDescriptor() throws Exception {
		try {
			module.remove(entity.getUri(), null);
		} finally {
			verify(internalMock, never()).removeEntity(any(URI.class), any(EntityDescriptor.class));
		}
	}

	@Test
	public void testExecuteStatement() throws Exception {
		final ResultSet rs = mock(ResultSet.class);
		final JopaStatement jopaStmt = mock(JopaStatement.class);
		when(internalMock.executeStatement(statementMock)).thenReturn(rs);
		final ResultSet res = module.executeStatement(jopaStmt);
		assertNotNull(res);
		assertSame(rs, res);
		verify(internalMock).executeStatement(statementMock);
	}

	@Test(expected = NullPointerException.class)
	public void testExecuteStatementNull() throws Exception {
		try {
			module.executeStatement(null);
		} finally {
			verify(internalMock, never()).executeStatement(any(OwlapiStatement.class));
		}
	}

	@Test
	public void testRollback() throws Exception {
		// Call persist to initiate a transaction
		module.persist(entity.getUri(), entity, descriptor);
		module.rollback();
		verify(internalMock).rollback();
	}

	@Test
	public void testGetEntityFromCache() {
		when(
				provider.getEntityFromLiveObjectCache(eq(OWLClassA.class), eq(entity.getUri()),
						any(URI.class))).thenReturn(entity);
		final OWLClassA res = module.getEntityFromCache(OWLClassA.class, entity.getUri());
		assertSame(entity, res);
		verify(provider).getEntityFromLiveObjectCache(eq(OWLClassA.class), eq(entity.getUri()),
				any(URI.class));
	}

	@Test
	public void testGetNewPrimaryKey() {
		final int res = module.getNewPrimaryKey();
		assertEquals(INITIAL_PK_COUNTER + 1, res);
	}

	@Test
	public void testIncrementPrimaryKeyCounter() {
		module.incrementPrimaryKeyCounter();
		final int res = module.getNewPrimaryKey();
		// First increment and then icrement and get
		assertEquals(INITIAL_PK_COUNTER + 2, res);
	}

}
