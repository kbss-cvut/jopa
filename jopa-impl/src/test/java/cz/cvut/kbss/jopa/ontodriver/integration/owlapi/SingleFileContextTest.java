package cz.cvut.kbss.jopa.ontodriver.integration.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.ontodriver.TestEnv;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.OWLClassI;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.EntityNotRegisteredException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class SingleFileContextTest {

	private static final Logger LOG = Logger.getLogger(SingleFileContextTest.class.getName());

	private static final List<StorageInfo> storage = Collections.singletonList(new StorageInfo(
			OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
	private static final String OWLCLASS_A_REFERENCE_FIELD = "owlClassA";

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;

	private static DataSource ds;
	private static PersistenceProviderFacade facade;
	private static Connection c;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		entityA.setTypes(Collections.singleton("http://JustOneType"));
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://entityI"));
		entityI.setOwlClassA(entityA);
		facade = TestEnv.getProviderFacade();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		entityE.setUri(null);
	}

	@Test
	public void testSimplePersist() throws Exception {
		LOG.config("Test: simple persist.");
		acquireConnection("SingleFileContextSimplePersist");
		c.setAutoCommit(true);
		c.persist(entityB.getUri(), entityB);
		final OWLClassB res = c.find(entityB.getClass(), entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testPersistRelationship() throws Exception {
		LOG.config("Test: persist relationship, referenced first.");
		acquireConnection("SingleFileContextPersistRelationship");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();
		final OWLClassA resA = c.find(entityA.getClass(), entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		final OWLClassD resD = c.find(entityD.getClass(), entityD.getUri());
		assertNotNull(resD);
		assertEquals(entityD.getUri(), resD.getUri());
	}

	@Test
	public void testPersistRelationshipInverse() throws Exception {
		LOG.config("Test: persist relationship, owner first.");
		acquireConnection("SingleFileContextPersistRelationshipInverse");
		c.setAutoCommit(false);
		c.persist(entityD.getUri(), entityD);
		c.persist(entityA.getUri(), entityA);
		c.commit();
		final OWLClassA resA = c.find(entityA.getClass(), entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		final OWLClassD resD = c.find(entityD.getClass(), entityD.getUri());
		assertNotNull(resD);
		assertEquals(entityD.getUri(), resD.getUri());
	}

	@Test
	public void testPersistGeneratePk() throws Exception {
		LOG.config("Test: persist, generate primary key.");
		acquireConnection("SingleFileContextPersistGeneratePk");
		c.setAutoCommit(true);
		assertNull(entityE.getUri());
		c.persist(null, entityE);
		assertNotNull(entityE.getUri());
		final OWLClassE res = c.find(entityE.getClass(), entityE.getUri());
		assertNotNull(res);
		assertEquals(entityE.getUri(), res.getUri());
		assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() throws Exception {
		LOG.config("Test: persist, null passed as entity.");
		acquireConnection("SingleFileContextPersistNull");
		c.persist(null, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testRollback() throws Exception {
		LOG.config("Test: rollback connection.");
		acquireConnection("SingleFileContextRollback");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.rollback();
		assertFalse(c.contains(entityA.getUri()));
	}

	@Test
	public void testContains() throws Exception {
		LOG.config("Test: contains.");
		acquireConnection("SingleFileContextContains");
		c.setAutoCommit(true);
		c.persist(entityB.getUri(), entityB);
		assertTrue(c.contains(entityB.getUri()));
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove entity.");
		acquireConnection("SingleFileContextRemove");
		c.setAutoCommit(true);
		final URI pk = entityA.getUri();
		c.persist(pk, entityA);
		final OWLClassA res = c.find(entityA.getClass(), pk);
		assertNotNull(res);
		c.remove(pk, res);
		assertNull(c.find(entityA.getClass(), pk));
		assertFalse(c.contains(pk));
	}

	@Test(expected = EntityNotRegisteredException.class)
	public void testRemoveNotExisting() throws Exception {
		LOG.config("Test: remove entity which doesn't exist.");
		acquireConnection("SingleFileContextRemoveNotExisting");
		c.setAutoCommit(true);
		c.remove(entityB.getUri(), entityB);
	}

	@Test
	public void testMergeSimple() throws Exception {
		LOG.config("Test: merge entity changes.");
		acquireConnection("SingleFileContextMergeSimple");
		c.setAutoCommit(true);
		final URI pk = entityB.getUri();
		c.persist(pk, entityB);
		final OWLClassB changed = c.find(entityB.getClass(), pk);
		assertNotNull(changed);
		final String newString = "newStringAttributeForEntityB";
		changed.setStringAttribute(newString);
		c.merge(pk, changed);
		final OWLClassB res = c.find(entityB.getClass(), pk);
		assertNotNull(res);
		assertEquals(newString, res.getStringAttribute());
	}

	@Test
	public void testMergeTypesChange() throws Exception {
		LOG.config("Test: merge change in types collection.");
		acquireConnection("SingleFileContextMergeTypesChange");
		c.setAutoCommit(true);
		final URI pk = entityA.getUri();
		c.persist(pk, entityA);
		final OWLClassA toChange = c.find(entityA.getClass(), pk);
		assertNotNull(toChange);
		final String typeOne = "http://AddedTypeOne";
		final String typeTwo = "http://AddedTypeTwo";
		toChange.getTypes().add(typeOne);
		toChange.getTypes().add(typeTwo);
		final int size = toChange.getTypes().size();
		c.merge(pk, toChange);
		final OWLClassA res = c.find(entityA.getClass(), pk);
		assertNotNull(res);
		assertEquals(size, res.getTypes().size());
		assertTrue(res.getTypes().contains(typeOne));
		assertTrue(res.getTypes().contains(typeTwo));
	}

	@Test
	public void testLoadField() throws Exception {
		LOG.config("Test: load lazily loaded field value.");
		acquireConnection("SingleFileContextLoadField");
		c.setAutoCommit(false);
		final URI pk = entityI.getUri();
		c.persist(pk, entityI);
		c.persist(entityA.getUri(), entityA);
		c.commit();
		final OWLClassA a = c.find(entityA.getClass(), entityA.getUri());
		assertNotNull(a);
		final OWLClassI i = c.find(entityI.getClass(), pk);
		assertNotNull(i);
		assertNull(i.getOwlClassA());
		c.loadFieldValue(i, i.getClass().getDeclaredField(OWLCLASS_A_REFERENCE_FIELD));
		assertNotNull(i.getOwlClassA());
		assertEquals(a.getUri(), i.getOwlClassA().getUri());
		assertEquals(a.getStringAttribute(), i.getOwlClassA().getStringAttribute());
	}

	@Test(expected = EntityNotRegisteredException.class)
	public void testLoadFieldUnregistered() throws Exception {
		LOG.config("Test: load lazily loaded field for an unregistered entity.");
		acquireConnection("SingleFileContextLoadFieldUnregistered");
		c.setAutoCommit(false);
		final URI pk = entityI.getUri();
		c.persist(pk, entityI);
		c.persist(entityA.getUri(), entityA);
		c.commit();
		c.loadFieldValue(entityI, entityI.getClass().getDeclaredField(OWLCLASS_A_REFERENCE_FIELD));
		fail("This line should not have been reached.");
	}

	@Test
	public void testRegisterEntityWithContext() throws Exception {
		LOG.config("Test: register existing entity with context.");
		acquireConnection("SingleFileContextRegisterEntityWithContext");
		final List<Context> ctxs = c.getContexts();
		assertEquals(1, ctxs.size());
		final Context ctx = ctxs.get(0);
		c.registerWithContext(entityB, ctx.getUri());
		final Context res = c.getSaveContextFor(entityB);
		assertNotNull(res);
		assertEquals(ctx, res);
	}

	@Test(expected = NullPointerException.class)
	public void testRegisterEntityWithContextNull() throws Exception {
		LOG.config("Test: register entity with context. Null passed as entity.");
		acquireConnection("SingleFileContextRegisterNull");
		final List<Context> ctxs = c.getContexts();
		assertEquals(1, ctxs.size());
		final Context ctx = ctxs.get(0);
		c.registerWithContext(null, ctx.getUri());
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storage, false);
		c = ds.getConnection(facade);
	}
}
