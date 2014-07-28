package cz.cvut.kbss.ontodriver.test.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.ontodriver.test.BaseSingleContextOntoDriverTest;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class SingleFileContextTest extends BaseSingleContextOntoDriverTest {

	/**
	 * We don't really care about descriptors in case of OWLAPI connectors.
	 */
	private static final Descriptor DEFAULT_DESCRIPTOR = new EntityDescriptor();

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;

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
		storageConfig = new OwlapiStorageConfig();
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
		persist(entityB.getUri(), entityB);
		final OWLClassB res = find(entityB.getClass(), entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testPersistRelationship() throws Exception {
		LOG.config("Test: persist relationship, referenced first.");
		acquireConnection("SingleFileContextPersistRelationship");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityD.getUri(), entityD);
		c.commit();
		final OWLClassA resA = find(entityA.getClass(), entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		final OWLClassD resD = find(entityD.getClass(), entityD.getUri());
		assertNotNull(resD);
		assertEquals(entityD.getUri(), resD.getUri());
	}

	@Test
	public void testPersistRelationshipInverse() throws Exception {
		LOG.config("Test: persist relationship, owner first.");
		acquireConnection("SingleFileContextPersistRelationshipInverse");
		c.setAutoCommit(false);
		persist(entityD.getUri(), entityD);
		persist(entityA.getUri(), entityA);
		c.commit();
		final OWLClassA resA = c.find(entityA.getClass(), entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		final OWLClassD resD = c.find(entityD.getClass(), entityD.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(resD);
		assertEquals(entityD.getUri(), resD.getUri());
	}

	@Test
	public void testPersistGeneratePk() throws Exception {
		LOG.config("Test: persist, generate primary key.");
		acquireConnection("SingleFileContextPersistGeneratePk");
		c.setAutoCommit(true);
		assertNull(entityE.getUri());
		persist(null, entityE);
		assertNotNull(entityE.getUri());
		final OWLClassE res = find(entityE.getClass(), entityE.getUri());
		assertNotNull(res);
		assertEquals(entityE.getUri(), res.getUri());
		assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() throws Exception {
		LOG.config("Test: persist, null passed as entity.");
		acquireConnection("SingleFileContextPersistNull");
		persist(null, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testRollback() throws Exception {
		LOG.config("Test: rollback connection.");
		acquireConnection("SingleFileContextRollback");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		c.rollback();
		assertFalse(contains(entityA.getUri()));
	}

	@Test
	public void testContains() throws Exception {
		LOG.config("Test: contains.");
		acquireConnection("SingleFileContextContains");
		c.setAutoCommit(true);
		persist(entityB.getUri(), entityB);
		assertTrue(contains(entityB.getUri()));
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove entity.");
		acquireConnection("SingleFileContextRemove");
		c.setAutoCommit(true);
		final URI pk = entityA.getUri();
		persist(pk, entityA);
		final OWLClassA res = find(entityA.getClass(), pk);
		assertNotNull(res);
		remove(pk);
		assertNull(find(entityA.getClass(), pk));
		assertFalse(contains(pk));
	}

	@Test
	public void testMergeSimple() throws Exception {
		LOG.config("Test: merge entity changes.");
		acquireConnection("SingleFileContextMergeSimple");
		c.setAutoCommit(true);
		final URI pk = entityB.getUri();
		persist(pk, entityB);
		final OWLClassB changed = find(entityB.getClass(), pk);
		assertNotNull(changed);
		final String newString = "newStringAttributeForEntityB";
		changed.setStringAttribute(newString);
		final Field strField = OWLClassB.getStrAttField();
		merge(changed, strField);
		final OWLClassB res = find(entityB.getClass(), pk);
		assertNotNull(res);
		assertEquals(newString, res.getStringAttribute());
	}

	@Test
	public void testMergeTypesChange() throws Exception {
		LOG.config("Test: merge change in types collection.");
		acquireConnection("SingleFileContextMergeTypesChange");
		c.setAutoCommit(true);
		final URI pk = entityA.getUri();
		persist(pk, entityA);
		final OWLClassA toChange = find(entityA.getClass(), pk);
		assertNotNull(toChange);
		final String typeOne = "http://AddedTypeOne";
		final String typeTwo = "http://AddedTypeTwo";
		toChange.getTypes().add(typeOne);
		toChange.getTypes().add(typeTwo);
		final int size = toChange.getTypes().size();
		final Field typesField = OWLClassA.getTypesField();
		merge(toChange, typesField);
		final OWLClassA res = find(entityA.getClass(), pk);
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
		persist(pk, entityI);
		persist(entityA.getUri(), entityA);
		c.commit();
		final OWLClassA a = find(entityA.getClass(), entityA.getUri());
		assertNotNull(a);
		final OWLClassI i = find(entityI.getClass(), pk);
		assertNotNull(i);
		assertNull(i.getOwlClassA());
		loadFieldValue(i, OWLClassI.getOwlClassAField());
		assertNotNull(i.getOwlClassA());
		assertEquals(a.getUri(), i.getOwlClassA().getUri());
		assertEquals(a.getStringAttribute(), i.getOwlClassA().getStringAttribute());
	}

	@Test
	public void testIsConsistent() throws Exception {
		LOG.config("Test: check consistency of the storage.");
		acquireConnection("SingleFileContextIsConsistent");
		c.setAutoCommit(false);
		assertFalse(c.getAutoCommit());
		persist(entityA.getUri(), entityA);
		persist(entityB.getUri(), entityB);
		persist(entityD.getUri(), entityD);
		persist(null, entityE);
		persist(entityI.getUri(), entityI);
		c.commit();
		// No need for context URI in OWLAPI
		assertTrue(c.isConsistent(null));
	}
}
