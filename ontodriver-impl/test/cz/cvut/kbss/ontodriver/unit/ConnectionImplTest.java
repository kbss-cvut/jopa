package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.exceptions.EntityNotRegisteredException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.ConnectionImpl;
import cz.cvut.kbss.ontodriver.utils.OWLClassA;
import cz.cvut.kbss.ontodriver.utils.OWLClassB;
import cz.cvut.kbss.ontodriver.utils.PersistenceProviderMock;
import cz.cvut.kbss.ontodriver.utils.StorageManagerMock;

public class ConnectionImplTest {

	private static final Logger LOG = Logger.getLogger(ConnectionImplTest.class.getName());

	private static PersistenceProviderMock pp;

	private static ConnectionImpl connection;
	private static StorageManagerMock storageMngr;
	private static Map<Context, Map<Object, Object>> knownEntities;
	private static List<Context> knownContexts;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		pp = new PersistenceProviderMock();
	}

	@Before
	public void setUp() throws Exception {
		storageMngr = new StorageManagerMock(pp);
		connection = new ConnectionImpl(storageMngr);
		knownEntities = storageMngr.getEntities();
		knownContexts = storageMngr.getAvailableContexts();
	}

	@Test(expected = NullPointerException.class)
	public void testConnectionImplNull() throws Exception {
		LOG.config("Test: connection constructor. Null passed.");
		@SuppressWarnings("unused")
		final Connection c = new ConnectionImpl(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testClose() throws Exception {
		LOG.config("Test: close connection.");
		connection.close();
		assertFalse(connection.isOpen());
		try {
			connection.close();
		} catch (Exception e) {
			fail("Exception caught. Test failed.");
		}
	}

	@Test
	public void testCommit() throws Exception {
		LOG.config("Test: commit connection.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://connectionTestB"));
		b.setStringAttribute("string");
		// Make a change so there's something to commit
		connection.setAutoCommit(false);
		connection.persist(b.getUri(), b);
		connection.commit();
		assertTrue(storageMngr.isCommitted());
		assertFalse(storageMngr.isRolledBack());
	}

	@Test
	public void testContainsByPK() throws Exception {
		LOG.config("Test: contains by primary key.");
		final Map<Object, Object> someContext = knownEntities.get(knownContexts.get(0));
		final Object pk = someContext.keySet().iterator().next();
		assertTrue(connection.contains(pk));
	}

	@Test(expected = NullPointerException.class)
	public void testContainsByPKNull() throws Exception {
		LOG.config("Test: contains by primary key. Null passed.");
		connection.contains(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testContainsInContext() throws Exception {
		LOG.config("Test: contains by context and primary key.");
		URI context = StorageManagerMock.CONTEXT_ONE_URI;
		final Map<Object, Object> someContext = knownEntities.get(storageMngr.getContextsByUris()
				.get(context));
		final Object pk = someContext.keySet().iterator().next();
		assertTrue(connection.contains(pk, context));
	}

	@Test
	public void testContainsInContextUnknownPK() throws Exception {
		LOG.config("Test: contains by context and primary key. Unknown primary key.");
		URI context = StorageManagerMock.CONTEXT_ONE_URI;
		assertFalse(connection.contains(URI.create("http://unknown"), context));
	}

	@Test(expected = OntoDriverException.class)
	public void testContainsInContextUnknownContext() throws Exception {
		LOG.config("Test: contains by context and primary key. Unknown context.");
		final URI pk = URI.create("http://unknown");
		connection.contains(pk, pk);
	}

	@Test(expected = NullPointerException.class)
	public void testContainsInContextNull() throws Exception {
		LOG.config("Test: contains by context and primary key. Null passed.");
		connection.contains(null, null);
	}

	@Test
	public void testFind() throws Exception {
		LOG.config("Test: find by primary key.");
		final Map<Object, Object> someContext = knownEntities.get(knownContexts.get(0));
		final Object pk = someContext.keySet().iterator().next();
		final Object entity = someContext.get(pk);
		final Object res = connection.find(entity.getClass(), pk);
		assertNotNull(res);
		assertEquals(entity, res);
	}

	@Test
	public void testFindInContext() throws Exception {
		LOG.config("Test: find in context.");
		final URI context = StorageManagerMock.CONTEXT_THREE_URI;
		final Map<Object, Object> someContext = knownEntities.get(storageMngr.getContextsByUris()
				.get(context));
		final Object pk = someContext.keySet().iterator().next();
		final Object entity = someContext.get(pk);
		final Object res = connection.find(entity.getClass(), pk, context);
		assertNotNull(res);
		assertEquals(entity, res);
	}

	@Test(expected = NullPointerException.class)
	public void testFindInContextNull() throws Exception {
		LOG.config("Test: find in context. Null passed as primary key.");
		final URI context = StorageManagerMock.CONTEXT_TWO_URI;
		@SuppressWarnings("unused")
		final Object res = connection.find(OWLClassA.class, null, context);
	}

	@Test
	public void testGetContext() throws Exception {
		LOG.config("Test: get context by URI.");
		final Context ctx = knownContexts.get(0);
		final Context res = connection.getContext(ctx.getUri());
		assertNotNull(res);
		assertEquals(ctx, res);
	}

	@Test(expected = NullPointerException.class)
	public void testGetContextByNull() throws Exception {
		LOG.config("Test: get context by URI. Null passed.");
		@SuppressWarnings("unused")
		final Context res = connection.getContext(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testGetCurrentContext() throws Exception {
		LOG.config("Test: get current connection context.");
		final Context ctx = connection.getCurrentContext();
		assertNotNull(ctx);
		assertTrue(knownContexts.contains(ctx));
	}

	@Test
	public void testGetContexts() throws Exception {
		LOG.config("Test: get all contexts.");
		final List<Context> res = connection.getContexts();
		assertNotNull(res);
		assertEquals(knownContexts.size(), res.size());
		assertTrue(res.containsAll(knownContexts));
	}

	@Test
	public void testGetSaveContextForExisting() throws Exception {
		LOG.config("Test: get save context for entity.");
		final URI context = StorageManagerMock.CONTEXT_THREE_URI;
		final Map<Object, Object> someContext = knownEntities.get(storageMngr.getContextsByUris()
				.get(context));
		final Object pk = someContext.keySet().iterator().next();
		// Find the entity so that it is registered in the connection
		final Object entity = connection.find(Object.class, pk, context);
		final Context res = connection.getSaveContextFor(entity);
		assertNotNull(res);
		assertEquals(storageMngr.getContextsByUris().get(context), res);
	}

	@Test
	public void testGetSaveContextForNew() throws Exception {
		LOG.config("Test: get save context for a new entity.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://newB"));
		// Default is the first one
		final Context defaultContext = storageMngr.getAvailableContexts().get(0);
		final Context res = connection.getSaveContextFor(b);
		assertNotNull(res);
		assertEquals(defaultContext, res);
	}

	@Test(expected = NullPointerException.class)
	public void testGetSaveContextForNull() throws Exception {
		LOG.config("Test: get save context for entity. Null passed.");
		@SuppressWarnings("unused")
		final Context res = connection.getSaveContextFor(null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testIsOpen() throws Exception {
		LOG.config("Test: is open.");
		assertTrue(connection.isOpen());
		connection.close();
		assertFalse(connection.isOpen());
	}

	@Test
	public void testMerge() throws Exception {
		LOG.config("Test: merge entity.");
		final Map<Object, Object> someContext = knownEntities.get(knownContexts.get(0));
		OWLClassA a = null;
		for (Object e : someContext.values()) {
			if (e instanceof OWLClassA) {
				a = (OWLClassA) e;
				break;
			}
		}
		assertNotNull(a);
		final OWLClassA res = connection.find(OWLClassA.class, a.getUri());
		assertNotNull(res);
		someContext.remove(res.getUri());
		res.setStringAttribute("ModifiedString");
		connection.merge(res.getUri(), res);
		assertTrue(someContext.containsKey(res.getUri()));
		assertEquals(res, someContext.get(res.getUri()));
	}

	@Test(expected = EntityNotRegisteredException.class)
	public void testMergeNotPersistent() throws Exception {
		LOG.config("Test: merge unknown entity.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://connectionTestB"));
		b.setStringAttribute("string");
		connection.merge(b.getUri(), b);
	}

	@Test
	public void testPersist() throws Exception {
		LOG.config("Test: persist entity into current context.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://connectionTestB"));
		b.setStringAttribute("string");
		final Context ctx = knownContexts.get(0);
		connection.persist(b.getUri(), b);
		assertTrue(knownEntities.get(ctx).containsKey(b.getUri()));
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() throws Exception {
		LOG.config("Test: persist null.");
		connection.persist(null, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testPersistObjectToContext() throws Exception {
		LOG.config("Test: persist entity into context.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://connectionTestB"));
		b.setStringAttribute("string");
		final Context ctx = knownContexts.get(0);
		connection.persist(b.getUri(), b, ctx.getUri());
		assertTrue(knownEntities.get(ctx).containsKey(b.getUri()));
	}

	@Test(expected = OntoDriverException.class)
	public void testPersistObjectToUnknownContext() throws Exception {
		LOG.config("Test: persist entity into unknown context.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://connectionTestB"));
		b.setStringAttribute("string");
		final URI unknownCtx = URI.create("http://unknown-context");
		connection.persist(b.getUri(), b, unknownCtx);
		fail("This line should not have been reached.");
	}

	@Test
	public void testRegisterWithContext() throws Exception {
		LOG.config("Test: register entity with context.");
		final Context ctx = knownContexts.get(0);
		final Map<Object, Object> someContext = knownEntities.get(ctx);
		final Object pk = someContext.keySet().iterator().next();
		final Object entity = someContext.get(pk);
		connection.registerWithContext(entity, ctx.getUri());
		final Context res = connection.getSaveContextFor(entity);
		assertNotNull(res);
		assertEquals(ctx, res);
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove entity.");
		final Map<Object, Object> someContext = knownEntities.get(knownContexts.get(0));
		final Object pk = someContext.keySet().iterator().next();
		final Object entity = someContext.get(pk);
		final Object res = connection.find(entity.getClass(), pk);
		connection.remove(pk, res);
		assertFalse(someContext.containsKey(pk));
	}

	@Test
	public void testRemoveFromContext() throws Exception {
		LOG.config("Test: remove entity from context.");
		final Context ctx = knownContexts.get(0);
		final Map<Object, Object> someContext = knownEntities.get(ctx);
		final Object pk = someContext.keySet().iterator().next();
		final Object entity = someContext.get(pk);
		final Object res = connection.find(entity.getClass(), pk);
		connection.remove(pk, res, ctx.getUri());
		assertFalse(someContext.containsKey(pk));
	}

	@Test(expected = OntoDriverException.class)
	public void testRemoveFromDifferentContext() throws Exception {
		LOG.config("Test: remove entity from incorrect context.");
		final Context ctx = knownContexts.get(0);
		final Context other = knownContexts.get(1);
		final Map<Object, Object> someContext = knownEntities.get(ctx);
		final Object pk = someContext.keySet().iterator().next();
		final Object entity = someContext.get(pk);
		final Object res = connection.find(entity.getClass(), pk);
		connection.remove(pk, res, other.getUri());
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testRemoveNull() throws Exception {
		LOG.config("Test: remove entity. Null passed.");
		connection.remove(null, new OWLClassA());
		fail("This line should not have been reached.");
	}

	@Test
	public void testRollback() throws Exception {
		LOG.config("Test: rollback.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://connectionTestB"));
		b.setStringAttribute("string");
		// Make a change so there's something to commit
		connection.setAutoCommit(false);
		connection.persist(b.getUri(), b);
		connection.rollback();
		assertTrue(storageMngr.isRolledBack());
	}

	@Test
	public void testSetAutoCommit() throws Exception {
		LOG.config("Test: set auto commit.");
		connection.setAutoCommit(false);
		assertFalse(connection.getAutoCommit());
	}

	@Test
	public void testSetConnectionContext() throws Exception {
		LOG.config("Test: set connection context.");
		final Context ctx = knownContexts.get(knownContexts.size() - 1);
		assertFalse(ctx.equals(connection.getCurrentContext()));
		connection.setConnectionContext(ctx.getUri());
		assertEquals(ctx, connection.getCurrentContext());
	}

	@Test
	public void testSetSaveContextFor() throws Exception {
		LOG.config("Test: set save context for entity.");
		final OWLClassB b = new OWLClassB();
		b.setUri(URI.create("http://connectionTestB"));
		b.setStringAttribute("string");
		final URI ctx = StorageManagerMock.CONTEXT_THREE_URI;
		connection.setSaveContextFor(b, ctx);
		final Context c = connection.getSaveContextFor(b);
		assertEquals(ctx, c.getUri());
	}

	@Test(expected = NullPointerException.class)
	public void testSetSaveContextForNull() throws Exception {
		LOG.config("Test: set save context for entity. Null passed as entity.");
		connection.setSaveContextFor(null, StorageManagerMock.CONTEXT_ONE_URI);
		fail("This line should not have been reached.");
	}
}
