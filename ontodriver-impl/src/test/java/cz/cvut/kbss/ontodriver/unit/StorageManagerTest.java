package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.StorageManagerImpl;
import cz.cvut.kbss.ontodriver.utils.DriverFactoryStub;
import cz.cvut.kbss.ontodriver.utils.OWLClassA;
import cz.cvut.kbss.ontodriver.utils.OWLClassB;
import cz.cvut.kbss.ontodriver.utils.OntoDriverMock;
import cz.cvut.kbss.ontodriver.utils.PersistenceProviderMock;
import cz.cvut.kbss.ontodriver.utils.StorageModuleMock;

public class StorageManagerTest {

	private static final Logger LOG = Logger.getLogger(StorageManagerTest.class.getName());

	private static OntoDriverMock driver;
	private static DriverFactoryStub factory;

	private static StorageManager manager;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		driver = new OntoDriverMock();
		factory = driver.getFactoryMock();
		manager = new StorageManagerImpl(new PersistenceProviderMock(), factory.getContexts(),
				driver);
	}

	@After
	public void tearDown() throws Exception {
		manager = new StorageManagerImpl(new PersistenceProviderMock(), factory.getContexts(),
				driver);
		factory.reset();
	}

	@Test
	public void testClose() throws Exception {
		LOG.config("Test: close the manager.");
		final List<Context> ctxs = factory.getContexts();
		final URI u = URI.create("http://testUri");
		// Make sure all modules are open
		for (Context c : ctxs) {
			manager.contains(u, c);
		}
		manager.close();
		assertFalse(manager.isOpen());
		for (StorageModuleMock m : DriverFactoryStub.getDefaultModules().values()) {
			assertFalse(m.isOpen());
		}
	}

	@Test
	public void testFind() throws Exception {
		LOG.config("Test: find.");
		final StorageModuleMock e = DriverFactoryStub.getDefaultModules().get(
				factory.getContexts().get(0));
		final Context ctx = e.getContext();
		final Object pk = e.getUris().get(0);
		final Object entity = e.getEntities().get(pk);

		final Object res = manager.find(entity.getClass(), pk, ctx,
				Collections.<String, Context> emptyMap());
		assertNotNull(res);
		assertEquals(entity, res);
	}

	@Test(expected = NullPointerException.class)
	public void testFindNull() throws Exception {
		LOG.config("Test: find entity, null primary key passed.");
		@SuppressWarnings("unused")
		final Object res = manager.find(OWLClassA.class, null, factory.getContexts().get(0),
				Collections.<String, Context> emptyMap());
		fail("This line should not have been reached.");
	}

	@Test(expected = OntoDriverException.class)
	public void testFindUnknownContext() throws Exception {
		LOG.config("Test: find entity. Unknown context.");
		final StorageModuleMock e = DriverFactoryStub.getDefaultModules().get(
				factory.getContexts().get(0));
		final Object pk = e.getUris().get(0);
		final Object entity = e.getEntities().get(pk);
		final Context unknown = new Context(URI.create("http://unknown.context"),
				OntologyConnectorType.JENA);

		manager.find(entity.getClass(), pk, unknown, Collections.<String, Context> emptyMap());
		fail("This line should not have been reached.");
	}

	@Test
	public void testIsConsistent() throws Exception {
		LOG.config("Test: consistency check.");
		final boolean res = manager.isConsistent(factory.getContexts().get(0));
		assertTrue(res);
	}

	@Test(expected = NullPointerException.class)
	public void testIsConsistentNull() throws Exception {
		LOG.config("Test: consistency check. Null passed as URI.");
		@SuppressWarnings("unused")
		final boolean res = manager.isConsistent(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testGetAvailableContexts() {
		LOG.config("Test: get contexts.");
		final List<Context> contexts = factory.getContexts();
		final List<Context> res = manager.getAvailableContexts();
		assertNotNull(res);
		assertEquals(contexts.size(), res.size());
		assertTrue(res.containsAll(contexts));
	}

	@Test
	public void testGetContextsByUris() {
		LOG.config("Test: get contexts by URIs.");
		final Map<URI, Context> res = manager.getContextsByUris();
		assertNotNull(res);
		final List<Context> ctxs = factory.getContexts();
		assertEquals(ctxs.size(), res.size());
		for (Context ctx : ctxs) {
			assertTrue(res.containsKey(ctx.getUri()));
			assertEquals(ctx, res.get(ctx.getUri()));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testLoadFieldValueNull() throws Exception {
		LOG.config("Test: load field value. Null entity.");
		manager.loadFieldValue(null, OWLClassA.class.getDeclaredFields()[0], factory.getContexts()
				.get(0));
		fail("This line should not have been reached.");
	}

	@Test
	public void testMerge() throws Exception {
		LOG.config("Test: merge entity.");
		final StorageModuleMock e = DriverFactoryStub.getDefaultModules().get(
				factory.getContexts().get(1));
		final Context ctx = e.getContext();
		final OWLClassB merged = new OWLClassB();
		merged.setUri(URI.create("http://mergedB"));
		merged.setStringAttribute("mergedBStringAttribute");
		final Field f = OWLClassB.getStrAttField();
		// Simulate merging by passing new entity
		manager.merge(merged.getUri(), merged, f, ctx, Collections.<String, Context> emptyMap());
		final OWLClassB res = (OWLClassB) e.getEntities().get(merged.getUri());
		assertNotNull(res);
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNull() throws Exception {
		LOG.config("Test: merge. Null passed.");
		final Field f = OWLClassB.getStrAttField();
		manager.merge(null, null, f, factory.getContexts().get(0),
				Collections.<String, Context> emptyMap());
		fail("This line should not have been reached.");
	}

	@Test
	public void testPersist() throws Exception {
		LOG.config("Test: persist.");
		final StorageModuleMock e = DriverFactoryStub.getDefaultModules().get(
				factory.getContexts().get(1));
		final Context ctx = e.getContext();
		final OWLClassB toPersist = new OWLClassB();
		toPersist.setUri(URI.create("http://toPersistB"));
		toPersist.setStringAttribute("toPersistStringAttribute");
		assertFalse(manager.contains(toPersist.getUri(), ctx));
		manager.persist(toPersist.getUri(), toPersist, ctx,
				Collections.<String, Context> emptyMap());
		assertTrue(manager.contains(toPersist.getUri(), ctx));
		final Object res = e.getEntities().get(toPersist.getUri());
		assertNotNull(res);
	}

	@Test(expected = OntoDriverException.class)
	public void testPersistUnknownContext() throws Exception {
		LOG.config("Test: persist. Unknown context.");
		final OWLClassB toPersist = new OWLClassB();
		toPersist.setUri(URI.create("http://toPersistB"));
		toPersist.setStringAttribute("toPersistStringAttribute");
		final Context unknown = new Context(URI.create("http://unknownContext"),
				OntologyConnectorType.OWLAPI);
		manager.persist(toPersist.getUri(), toPersist, unknown,
				Collections.<String, Context> emptyMap());
		fail("This line should not have been reached.");
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove.");
		final StorageModuleMock e = DriverFactoryStub.getDefaultModules().get(
				factory.getContexts().get(0));
		final Context ctx = e.getContext();
		final Object pk = e.getUris().get(0);
		assertTrue(manager.contains(pk, ctx));
		manager.remove(pk, ctx);
		assertFalse(manager.contains(pk, ctx));
		final Object res = e.getEntities().get(pk);
		assertNull(res);
	}

	@Test(expected = NullPointerException.class)
	public void testStorageManagerImpl() {
		LOG.config("Test: constructor. Null passed.");
		final StorageManager m = new StorageManagerImpl(null, factory.getContexts(), null);
		fail("This line should not have been reached.");
		m.isOpen();
	}

	// TODO Should modules be reloaded on commit?
	// @Test
	// public void testCommit() {
	// fail("Not yet implemented");
	// }

	@Test
	public void testRollback() throws Exception {
		LOG.config("Test: rollback.");
		final StorageModuleMock e = DriverFactoryStub.getDefaultModules().get(
				factory.getContexts().get(1));
		final Context ctx = e.getContext();
		final OWLClassB toPersist = new OWLClassB();
		toPersist.setUri(URI.create("http://toPersistB"));
		toPersist.setStringAttribute("toPersistStringAttribute");
		assertFalse(manager.contains(toPersist.getUri(), ctx));
		manager.persist(toPersist.getUri(), toPersist, ctx,
				Collections.<String, Context> emptyMap());
		assertTrue(manager.contains(toPersist.getUri(), ctx));
		manager.rollback();
		assertFalse(manager.contains(toPersist.getUri(), ctx));
	}

}
