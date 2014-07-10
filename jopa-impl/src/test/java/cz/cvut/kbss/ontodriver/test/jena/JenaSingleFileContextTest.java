package cz.cvut.kbss.ontodriver.test.jena;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.utils.JenaStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.EntityNotRegisteredException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class JenaSingleFileContextTest {

	private static final Logger LOG = Logger.getLogger(JenaSingleFileContextTest.class.getName());

	private static final List<StorageConfig> storage = Collections
			.<StorageConfig> singletonList(new JenaStorageConfig());

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	private static OWLClassE entityE;
	private static OWLClassI entityI;

	private static DataSource ds;
	private static Connection c;
	private static PersistenceProviderFacade facade;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		entityA.setTypes(Collections.singleton("JustOneType"));
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityI"));
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
	public void testConnector() throws Exception {
		LOG.config("Test: initialize connector to the Jena storage.");
		acquireConnection("JenaSingleFileContextConnector");
		assertNotNull(c);
		final List<Context> contexts = c.getContexts();
		assertNotNull(contexts);
		assertEquals(1, contexts.size());
		// Just make it connect
		assertFalse(c.contains(entityA.getUri()));
	}

	@Test
	public void testPersistSimple() throws Exception {
		LOG.config("Test: persist a simple entity.");
		acquireConnection("JenaSingleFileContextPersistSimple");
		c.setAutoCommit(true);
		c.persist(entityB.getUri(), entityB);
		assertTrue(c.contains(entityB.getUri()));
		final OWLClassB res = c.find(entityB.getClass(), entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getUri(), res.getUri());
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testPersistRelated() throws Exception {
		LOG.config("Test: persist two related entities.");
		acquireConnection("JenaSingleFileContextPersistRelated");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();
		assertTrue(c.contains(entityA.getUri()));
		assertTrue(c.contains(entityD.getUri()));
	}

	@Test
	public void testPersistGeneratePk() throws Exception {
		LOG.config("Test: persist entity with primary key generation.");
		acquireConnection("JenaSingleFileContextPersistGenerated");
		c.setAutoCommit(true);
		assertNull(entityE.getUri());
		c.persist(null, entityE);
		assertNotNull(entityE.getUri());
		c.close();
		c = ds.getConnection(facade);
		assertTrue(c.contains(entityE.getUri()));
	}

	@Test
	public void testRetrieve() throws Exception {
		LOG.config("Test: persist two related entities, then close the connection and retrieve them in another connection.");
		final String base = "JenaSingleFileContextRetrieve";
		acquireConnection(base);
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();
		c.close();
		c = ds.getConnection(facade);
		assertTrue(c.contains(entityA.getUri()));
		assertTrue(c.contains(entityD.getUri()));
		final OWLClassA resA = c.find(entityA.getClass(), entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		final OWLClassD resD = c.find(entityD.getClass(), entityD.getUri());
		assertNotNull(resD);
		assertEquals(entityA.getUri(), resD.getOwlClassA().getUri());
	}

	@Test
	public void testMerge() throws Exception {
		LOG.config("Test: merge changes on object.");
		acquireConnection("JenaSingleFileContextMerge");
		c.setAutoCommit(false);
		final URI uriA = entityA.getUri();
		c.persist(uriA, entityA);
		final URI uriD = entityD.getUri();
		c.persist(uriD, entityD);
		final URI uriB = entityB.getUri();
		c.persist(uriB, entityB);
		c.commit();

		final String newString = "newStringAttributeForB";
		final OWLClassB b = c.find(entityB.getClass(), uriB);
		assertNotNull(b);
		b.setStringAttribute(newString);
		final Field strField = OWLClassB.getStrAttField();
		c.merge(uriB, b, strField);
		final OWLClassA a = c.find(entityA.getClass(), uriA);
		assertNotNull(a);
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/TOne");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/TTwo");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/TThree");
		a.setTypes(types);
		final Field typesField = OWLClassA.getTypesField();
		c.merge(uriA, a, typesField);
		final OWLClassD d = c.find(entityD.getClass(), uriD);
		assertNotNull(d);
		d.setOwlClassA(null);
		final Field aField = OWLClassD.getOwlClassAField();
		c.merge(uriD, d, aField);
		c.commit();

		assertTrue(c.contains(uriB));
		assertTrue(c.contains(uriA));
		assertTrue(c.contains(uriD));
		final OWLClassB resB = c.find(entityB.getClass(), uriB);
		assertNotNull(resB);
		assertEquals(newString, resB.getStringAttribute());
		final OWLClassA resA = c.find(entityA.getClass(), uriA);
		assertNotNull(resA);
		assertEquals(types.size(), resA.getTypes().size());
		assertTrue(types.containsAll(resA.getTypes()));
		final OWLClassD resD = c.find(entityD.getClass(), uriD);
		assertNotNull(resD);
		assertNull(resD.getOwlClassA());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDuplicate() throws Exception {
		LOG.config("Test: persist entity twice violating IC.");
		acquireConnection("JenaSingleFileContextPersistDuplicate");
		c.setAutoCommit(true);
		c.persist(entityB.getUri(), entityB);
		assertTrue(c.contains(entityB.getUri()));
		c.persist(entityB.getUri(), entityB);
		fail("This line should not have been reached.");
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove entity.");
		acquireConnection("JenaSingleFileContextRemove");
		c.setAutoCommit(false);
		final URI uriD = entityD.getUri();
		final URI uriA = entityA.getUri();
		c.persist(uriD, entityD);
		c.persist(uriA, entityA);
		c.commit();
		final OWLClassD toRemove = c.find(entityD.getClass(), uriD);
		assertNotNull(toRemove);
		assertTrue(c.contains(uriA));
		c.remove(uriD, toRemove);
		c.commit();
		assertFalse(c.contains(uriD));
		assertTrue(c.contains(uriA));
		assertNull(c.find(entityD.getClass(), uriD));
		assertNotNull(c.find(entityA.getClass(), uriA));
	}

	@Test(expected = EntityNotRegisteredException.class)
	public void testRemoveTwice() throws Exception {
		LOG.config("Test: remove entity twice during one transaction.");
		acquireConnection("JenaSingleFileContextRemoveTwice");
		c.setAutoCommit(false);
		final URI uriB = entityB.getUri();
		c.persist(uriB, entityB);
		c.commit();
		final OWLClassB toRemove = c.find(entityB.getClass(), uriB);
		assertNotNull(toRemove);
		c.remove(uriB, toRemove);
		assertFalse(c.contains(uriB));
		c.remove(uriB, toRemove);
		fail("This line should not have been reached.");
	}

	@Test
	public void testRollbackRemove() throws Exception {
		LOG.config("Test: rollback a remove operation.");
		acquireConnection("JenaSingleFileContextRollbackRemove");
		c.setAutoCommit(false);
		final URI uriB = entityB.getUri();
		final URI uriA = entityA.getUri();
		c.persist(uriB, entityB);
		c.persist(uriA, entityA);
		c.commit();
		assertTrue(c.contains(uriB));
		final OWLClassB b = c.find(entityB.getClass(), uriB);
		assertNotNull(b);
		c.remove(uriB, b);
		assertFalse(c.contains(uriB));
		c.rollback();
		assertTrue(c.contains(uriA));
		assertTrue(c.contains(uriB));
		assertNotNull(c.find(entityA.getClass(), uriA));
		assertNotNull(c.find(entityB.getClass(), uriB));
	}

	@Test
	public void testLoadFieldValue() throws Exception {
		LOG.config("Test: load a lazily loaded field value.");
		acquireConnection("JenaSingleFileContextLoadFiled");
		c.setAutoCommit(false);
		final URI uriI = entityI.getUri();
		c.persist(entityA.getUri(), entityA);
		c.persist(uriI, entityI);
		c.commit();
		c.close();
		c = ds.getConnection(facade);
		final OWLClassI res = c.find(entityI.getClass(), uriI);
		assertNull(res.getOwlClassA());
		final Field aField = OWLClassI.getOwlClassAField();
		c.loadFieldValue(res, aField);
		assertNotNull(res.getOwlClassA());
		assertEquals(entityA.getUri(), res.getOwlClassA().getUri());
		assertEquals(entityA.getStringAttribute(), res.getOwlClassA().getStringAttribute());
		assertEquals(entityA.getTypes().size(), res.getOwlClassA().getTypes().size());
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storage);
		c = ds.getConnection(facade);
	}
}
