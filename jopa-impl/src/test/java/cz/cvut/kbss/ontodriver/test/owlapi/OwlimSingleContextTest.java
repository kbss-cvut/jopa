package cz.cvut.kbss.ontodriver.test.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.utils.OwlimStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class OwlimSingleContextTest {

	private static final Logger LOG = Logger.getLogger(JenaSingleFileContextTest.class.getName());

	private static final List<StorageConfig> storage = Collections
			.<StorageConfig> singletonList(new OwlimStorageConfig());

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
		entityI.setOwlClassA(entityA);
	}

	@Test
	public void testAcquireConnector() throws Exception {
		LOG.config("Test: initialize connector to the OWLIM storage.");
		acquireConnection("OwlimSingleContextConnect");
		assertNotNull(c);
		final List<Context> contexts = c.getContexts();
		assertNotNull(contexts);
		assertEquals(1, contexts.size());
		// Just make it connect
		assertFalse(c.contains(entityA.getUri()));
	}

	@Test
	public void testDoPersist() throws Exception {
		LOG.config("Test: persist a simple entity.");
		acquireConnection("OwlimSingleContextSimplePersist");
		c.persist(entityB.getUri(), entityB);
		assertTrue(c.contains(entityB.getUri()));
		final OWLClassB res = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getUri(), res.getUri());
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testPersistMultiple() throws Exception {
		LOG.config("Test: persist multiple entities. Two in relationship and one with generated id.");
		acquireConnection("OwlimSingleContextPersistMultiple");
		c.setAutoCommit(false);
		c.persist(null, entityE);
		assertNotNull(entityE.getUri());
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();

		final OWLClassE resE = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
		final OWLClassD resD = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		assertEquals(entityA.getUri(), resD.getOwlClassA().getUri());
		assertEquals(entityA.getStringAttribute(), resD.getOwlClassA().getStringAttribute());
	}

	@Test
	public void testPersistReadInDifferent() throws Exception {
		LOG.config("Test: persist entities in one connection and read them then in another.");
		// This test will force Owlim->Jena->OWLAPI transformation with
		// non-empty ontology
		acquireConnection("OwlimSingleContextPersistReadInDifferent");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB);
		c.persist(null, entityE);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();

		final Connection cTwo = ds.getConnection(facade);
		final OWLClassB resB = cTwo.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		assertEquals(entityB.getStringAttribute(), resB.getStringAttribute());
		final OWLClassE resE = cTwo.find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
		final OWLClassD resD = cTwo.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		final OWLClassA resA = cTwo.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(resA.getUri(), resD.getOwlClassA().getUri());
	}

	@Test
	public void testUpdate() throws Exception {
		LOG.config("Test: persist some entities and modify them later.");
		acquireConnection("OwlimSingleContextUpdate");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB);
		c.persist(null, entityE);
		c.commit();

		final OWLClassB b = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(b);
		b.setStringAttribute("changedStringAttribute");
		final Field bStrField = OWLClassB.getStrAttField();
		c.merge(b.getUri(), b, bStrField);
		final OWLClassE e = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(e);
		e.setStringAttribute("anotherChangedStringAttribute");
		final Field eStrField = OWLClassE.getStrAttField();
		c.merge(e.getUri(), e, eStrField);
		c.commit();

		final OWLClassB resB = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		assertEquals(b.getStringAttribute(), resB.getStringAttribute());
		final OWLClassE resE = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		assertEquals(e.getStringAttribute(), resE.getStringAttribute());
	}

	@Test
	public void testUpdateRelationship() throws Exception {
		LOG.config("Test: persist some entities with relationships and update these relationship. Plus do some lazy loading.");
		acquireConnection("OwlimSingleContextUpdateRelationship");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();

		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		final OWLClassD d = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d);
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newA"));
		c.persist(newA.getUri(), newA);
		d.setOwlClassA(newA);
		final Field aField = OWLClassD.getOwlClassAField();
		c.merge(entityD.getUri(), d, aField);
		entityI.setOwlClassA(a);
		c.persist(entityI.getUri(), entityI);
		c.commit();

		final OWLClassD resD = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		assertEquals(newA.getUri(), resD.getOwlClassA().getUri());
		final OWLClassA resAOne = c.find(OWLClassA.class, newA.getUri());
		assertNotNull(resAOne);
		assertEquals(resD.getOwlClassA().getStringAttribute(), resAOne.getStringAttribute());
		final OWLClassA resATwo = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resATwo);
		assertEquals(entityA.getStringAttribute(), resATwo.getStringAttribute());
		final OWLClassI resI = c.find(OWLClassI.class, entityI.getUri());
		assertNotNull(resI);
		assertNull(resI.getOwlClassA());
		final Field f = OWLClassI.getOwlClassAField();
		c.loadFieldValue(resI, f);
		assertNotNull(resI.getOwlClassA());
		assertEquals(resATwo.getUri(), resI.getOwlClassA().getUri());
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove an entity.");
		acquireConnection("OwlimSingleContextRemove");
		c.setAutoCommit(false);
		c.persist(entityB.getUri(), entityB);
		c.persist(null, entityE);
		assertNotNull(entityE.getUri());
		c.commit();

		final OWLClassE e = c.find(OWLClassE.class, entityE.getUri());
		c.remove(e.getUri(), e);
		c.commit();

		final OWLClassB resB = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		assertNull(c.find(OWLClassE.class, entityE.getUri()));
		final Connection cTwo = ds.getConnection(facade);
		final OWLClassB resBTwo = cTwo.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resBTwo);
		assertEquals(resB.getStringAttribute(), resBTwo.getStringAttribute());
		assertNull(cTwo.find(OWLClassE.class, entityE.getUri()));
	}

	@Test
	public void testRemoveRelationship() throws Exception {
		LOG.config("Test: remove a relationship owner.");
		acquireConnection("OwlimSingleContextRemoveRelationship");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();

		final OWLClassD d = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d);
		c.remove(d.getUri(), d);
		c.commit();

		assertFalse(c.contains(entityD.getUri()));
		assertNull(c.find(OWLClassD.class, entityD.getUri()));
		assertTrue(c.contains(entityA.getUri()));
		assertNotNull(c.find(OWLClassA.class, entityA.getUri()));
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storage);
		c = ds.getConnection(facade);
	}
}
