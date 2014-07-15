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
import java.util.Set;

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
import cz.cvut.kbss.ontodriver.test.BaseSingleContextOntoDriverTest;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class JenaSingleFileContextTest extends BaseSingleContextOntoDriverTest {

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	private static OWLClassE entityE;
	private static OWLClassI entityI;

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
		storageConfig = new JenaStorageConfig();
	}

	@After
	public void tearDown() throws Exception {
		after();
		entityE.setUri(null);
	}

	@Test
	public void testConnector() throws Exception {
		LOG.config("Test: initialize connector to the Jena storage.");
		acquireConnection("JenaSingleFileContextConnector");
		assertNotNull(c);
		// Just make it connect
		assertFalse(contains(entityA.getUri()));
	}

	@Test
	public void testPersistSimple() throws Exception {
		LOG.config("Test: persist a simple entity.");
		acquireConnection("JenaSingleFileContextPersistSimple");
		c.setAutoCommit(true);
		persist(entityB.getUri(), entityB);
		assertTrue(contains(entityB.getUri()));
		final OWLClassB res = find(entityB.getClass(), entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getUri(), res.getUri());
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testPersistRelated() throws Exception {
		LOG.config("Test: persist two related entities.");
		acquireConnection("JenaSingleFileContextPersistRelated");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityD.getUri(), entityD);
		c.commit();
		assertTrue(contains(entityA.getUri()));
		assertTrue(contains(entityD.getUri()));
	}

	@Test
	public void testPersistGeneratePk() throws Exception {
		LOG.config("Test: persist entity with primary key generation.");
		acquireConnection("JenaSingleFileContextPersistGenerated");
		c.setAutoCommit(true);
		assertNull(entityE.getUri());
		persist(null, entityE);
		assertNotNull(entityE.getUri());
		c.close();
		c = ds.getConnection(facade);
		assertTrue(contains(entityE.getUri()));
	}

	@Test
	public void testRetrieve() throws Exception {
		LOG.config("Test: persist two related entities, then close the connection and retrieve them in another connection.");
		final String base = "JenaSingleFileContextRetrieve";
		acquireConnection(base);
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityD.getUri(), entityD);
		c.commit();
		c.close();
		c = ds.getConnection(facade);
		assertTrue(contains(entityA.getUri()));
		assertTrue(contains(entityD.getUri()));
		final OWLClassA resA = find(entityA.getClass(), entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		final OWLClassD resD = find(entityD.getClass(), entityD.getUri());
		assertNotNull(resD);
		assertEquals(entityA.getUri(), resD.getOwlClassA().getUri());
	}

	@Test
	public void testMerge() throws Exception {
		LOG.config("Test: merge changes on object.");
		acquireConnection("JenaSingleFileContextMerge");
		c.setAutoCommit(false);
		final URI uriA = entityA.getUri();
		persist(uriA, entityA);
		final URI uriD = entityD.getUri();
		persist(uriD, entityD);
		final URI uriB = entityB.getUri();
		persist(uriB, entityB);
		c.commit();

		final String newString = "newStringAttributeForB";
		final OWLClassB b = find(entityB.getClass(), uriB);
		assertNotNull(b);
		b.setStringAttribute(newString);
		final Field strField = OWLClassB.getStrAttField();
		merge(b, strField);
		final OWLClassA a = find(entityA.getClass(), uriA);
		assertNotNull(a);
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/TOne");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/TTwo");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/TThree");
		a.setTypes(types);
		final Field typesField = OWLClassA.getTypesField();
		merge(a, typesField);
		final OWLClassD d = find(entityD.getClass(), uriD);
		assertNotNull(d);
		d.setOwlClassA(null);
		final Field aField = OWLClassD.getOwlClassAField();
		merge(d, aField);
		c.commit();

		assertTrue(contains(uriB));
		assertTrue(contains(uriA));
		assertTrue(contains(uriD));
		final OWLClassB resB = find(entityB.getClass(), uriB);
		assertNotNull(resB);
		assertEquals(newString, resB.getStringAttribute());
		final OWLClassA resA = find(entityA.getClass(), uriA);
		assertNotNull(resA);
		assertEquals(types.size(), resA.getTypes().size());
		assertTrue(types.containsAll(resA.getTypes()));
		final OWLClassD resD = find(entityD.getClass(), uriD);
		assertNotNull(resD);
		assertNull(resD.getOwlClassA());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDuplicate() throws Exception {
		LOG.config("Test: persist entity twice violating IC.");
		acquireConnection("JenaSingleFileContextPersistDuplicate");
		c.setAutoCommit(true);
		persist(entityB.getUri(), entityB);
		assertTrue(contains(entityB.getUri()));
		persist(entityB.getUri(), entityB);
		fail("This line should not have been reached.");
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove entity.");
		acquireConnection("JenaSingleFileContextRemove");
		c.setAutoCommit(false);
		final URI uriD = entityD.getUri();
		final URI uriA = entityA.getUri();
		persist(uriD, entityD);
		persist(uriA, entityA);
		c.commit();
		final OWLClassD toRemove = find(entityD.getClass(), uriD);
		assertNotNull(toRemove);
		assertTrue(contains(uriA));
		remove(uriD);
		c.commit();
		assertFalse(contains(uriD));
		assertTrue(contains(uriA));
		assertNull(find(entityD.getClass(), uriD));
		assertNotNull(find(entityA.getClass(), uriA));
	}

	@Test
	public void testRemoveTwice() throws Exception {
		LOG.config("Test: remove entity twice during one transaction. Shouldn't throw any exceptions.");
		acquireConnection("JenaSingleFileContextRemoveTwice");
		c.setAutoCommit(false);
		final URI uriB = entityB.getUri();
		persist(uriB, entityB);
		c.commit();
		final OWLClassB toRemove = find(entityB.getClass(), uriB);
		assertNotNull(toRemove);
		remove(uriB);
		assertFalse(contains(uriB));
		remove(uriB);
		assertFalse(contains(uriB));
	}

	@Test
	public void testRollbackRemove() throws Exception {
		LOG.config("Test: rollback a remove operation.");
		acquireConnection("JenaSingleFileContextRollbackRemove");
		c.setAutoCommit(false);
		final URI uriB = entityB.getUri();
		final URI uriA = entityA.getUri();
		persist(uriB, entityB);
		persist(uriA, entityA);
		c.commit();
		assertTrue(contains(uriB));
		final OWLClassB b = find(entityB.getClass(), uriB);
		assertNotNull(b);
		remove(uriB);
		assertFalse(contains(uriB));
		c.rollback();
		assertTrue(contains(uriA));
		assertTrue(contains(uriB));
		assertNotNull(find(entityA.getClass(), uriA));
		assertNotNull(find(entityB.getClass(), uriB));
	}

	@Test
	public void testLoadFieldValue() throws Exception {
		LOG.config("Test: load a lazily loaded field value.");
		acquireConnection("JenaSingleFileContextLoadFiled");
		c.setAutoCommit(false);
		final URI uriI = entityI.getUri();
		persist(entityA.getUri(), entityA);
		persist(uriI, entityI);
		c.commit();
		c.close();
		c = ds.getConnection(facade);
		final OWLClassI res = find(entityI.getClass(), uriI);
		assertNull(res.getOwlClassA());
		final Field aField = OWLClassI.getOwlClassAField();
		loadFieldValue(res, aField);
		assertNotNull(res.getOwlClassA());
		assertEquals(entityA.getUri(), res.getOwlClassA().getUri());
		assertEquals(entityA.getStringAttribute(), res.getOwlClassA().getStringAttribute());
		assertEquals(entityA.getTypes().size(), res.getOwlClassA().getTypes().size());
	}
}
