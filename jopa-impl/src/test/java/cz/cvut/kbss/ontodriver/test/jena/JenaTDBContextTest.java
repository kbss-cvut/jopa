package cz.cvut.kbss.ontodriver.test.jena;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.utils.JenaTDBStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;
import cz.cvut.kbss.ontodriver.test.TestEnv;

/**
 * The slash in every connection acquisition tells the OntoDriver that a Jena
 * TDB connector should be used.
 * 
 * @author kidney
 * 
 */
public class JenaTDBContextTest {

	private static final Logger LOG = Logger.getLogger(JenaTDBContextTest.class.getName());

	private static final List<StorageConfig> storage = Collections
			.<StorageConfig> singletonList(new JenaTDBStorageConfig());
	private static final Map<String, String> properties = initProperties();

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
		entityA.setTypes(Collections
				.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/OWLClassA"));
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
		LOG.config("Test: connect to the storage.");
		acquireConnection("JenaTDBContextConnector");
		assertNotNull(c);
		final List<Context> contexts = c.getContexts();
		assertNotNull(contexts);
		assertFalse(contexts.isEmpty());
		assertEquals(1, contexts.size());
		// Force connector initialization
		assertNull(c.find(OWLClassA.class, entityA.getUri()));
	}

	@Test
	public void testPersistMultiple() throws Exception {
		LOG.config("Test: persist multiple entities.");
		acquireConnection("JenaTDBContextPersist");
		assertNotNull(c);
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityB.getUri(), entityB);
		assertNull(entityE.getUri());
		c.persist(entityE.getUri(), entityE);
		assertNotNull(entityE.getUri());
		c.persist(entityD.getUri(), entityD);
		c.commit();

		final OWLClassD resD = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		assertEquals(entityA.getUri(), resD.getOwlClassA().getUri());
		final OWLClassE resE = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
		final OWLClassB resB = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		assertEquals(entityB.getStringAttribute(), resB.getStringAttribute());
		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
	}

	@Test
	public void testUpdate() throws Exception {
		LOG.config("Test: simple update.");
		acquireConnection("JenaTDBContextUpdate");
		// Let it commit every operation
		c.persist(entityB.getUri(), entityB);
		c.persist(entityA.getUri(), entityA);

		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		final String newType = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/OWLClassANew";
		a.getTypes().add(newType);
		final Field typesField = OWLClassA.getTypesField();
		c.merge(a.getUri(), a, typesField);
		final OWLClassB b = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(b);
		final String newStr = "newStringAttribute";
		b.setStringAttribute(newStr);
		final Field strField = OWLClassB.getStrAttField();
		c.merge(b.getUri(), b, strField);

		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getTypes().size() + 1, resA.getTypes().size());
		assertTrue(resA.getTypes().contains(newType));
		final OWLClassB resB = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		assertEquals(newStr, resB.getStringAttribute());
	}

	@Test
	public void testUpdateRelationship() throws Exception {
		LOG.config("Test: update relationship.");
		acquireConnection("JenaTBContextUpdateRelationship");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityI.getUri(), entityI);
		c.commit();

		final OWLClassI i = c.find(OWLClassI.class, entityI.getUri());
		assertNotNull(i);
		assertNull(i.getOwlClassA());
		final Field f = OWLClassI.getOwlClassAField();
		c.loadFieldValue(i, f);
		assertNotNull(i.getOwlClassA());
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newA"));
		newA.setStringAttribute("someString");
		i.setOwlClassA(newA);
		c.persist(newA.getUri(), newA);
		final Field aField = OWLClassI.getOwlClassAField();
		c.merge(i.getUri(), i, aField);
		c.commit();

		final OWLClassI resI = c.find(OWLClassI.class, entityI.getUri());
		assertNotNull(resI);
		c.loadFieldValue(resI, f);
		assertNotNull(resI.getOwlClassA());
		assertEquals(newA.getUri(), resI.getOwlClassA().getUri());
		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove an entity.");
		acquireConnection("JenaTDBContextRemove");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.persist(null, entityE);
		c.commit();

		final OWLClassD d = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d);
		c.remove(d.getUri(), d);
		final OWLClassE e = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(e);
		c.remove(e.getUri(), e);
		c.commit();

		final Connection cTwo = ds.getConnection(facade);
		try {
			assertNotNull(cTwo);
			assertFalse(cTwo.contains(entityE.getUri()));
			assertNull(cTwo.find(OWLClassE.class, entityE.getUri()));
			assertFalse(cTwo.contains(entityD.getUri()));
			assertNull(cTwo.find(OWLClassD.class, entityD.getUri()));
			assertTrue(cTwo.contains(entityA.getUri()));
			final OWLClassA res = c.find(OWLClassA.class, entityA.getUri());
			assertNotNull(res);
			assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
		} finally {
			cTwo.close();
		}
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storage, properties);
		c = ds.getConnection(facade);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<String, String>();
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, DriverCachingJenaFactory.class.getName());
		return m;
	}
}
