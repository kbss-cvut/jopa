package cz.cvut.kbss.jopa.test.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.TestEnvironment;

public class TestEntityTransactionsWithoutCache {

	private static final Logger LOG = Logger.getLogger(TestEntityTransactionsWithoutCache.class
			.getName());

	private static final Map<String, String> properties = new HashMap<String, String>();

	private OWLClassA testEntity;
	private OWLClassD composedEntity;

	public TestEntityTransactionsWithoutCache() {
		this.testEntity = new OWLClassA();
		final URI pkOne = URI.create("http://testEntity");
		final String strAtt = "TestAttribute";
		this.testEntity.setUri(pkOne);
		this.testEntity.setStringAttribute(strAtt);
		this.composedEntity = new OWLClassD();
		final URI pkTwo = URI.create("http://composedTestEntity");
		this.composedEntity.setUri(pkTwo);
		this.composedEntity.setOwlClassA(testEntity);
	}

	@BeforeClass
	public static void setup() {
		properties.put("cache", "off");
	}

	@After
	public void tearDown() {
		testEntity.setTypes(null);
	}

	@Test
	public void testPersist() {
		LOG.config("TestPersist");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testPersistWithoutCache", false);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.getTransaction().commit();
		OWLClassA result = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(result);
		assertEquals(testEntity.getStringAttribute(), result.getStringAttribute());
	}

	@Test
	public void testPersistComposed() {
		LOG.config("TestPersistComposed");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testPersistComposedWithoutCache", false);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.persist(composedEntity);
		pc.getTransaction().commit();
		OWLClassD result = pc.find(OWLClassD.class, composedEntity.getUri());
		assertNotNull(result);
		assertNotNull(result.getOwlClassA());
		assertEquals(testEntity.getUri(), result.getOwlClassA().getUri());
	}

	@Test
	public void testRemove() {
		LOG.config("TestRemove");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testRemoveWithoutCache", false);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.getTransaction().commit();
		pc.getTransaction().begin();
		OWLClassA toDelete = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(toDelete);
		pc.remove(toDelete);
		pc.getTransaction().commit();
		toDelete = pc.find(OWLClassA.class, testEntity.getUri());
		assertNull(toDelete);
	}

	@Test
	public void testRemoveWithoutCascade() {
		LOG.config("TestRemoveComposedWithoutCascade");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testRemoveNotCascadeWithoutCache", false);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.persist(composedEntity);
		pc.getTransaction().commit();
		final OWLClassD toDelete = pc.find(OWLClassD.class, composedEntity.getUri());
		assertNotNull(toDelete);
		pc.getTransaction().begin();
		pc.remove(toDelete);
		pc.getTransaction().commit();
		final OWLClassD shouldBeNull = pc.find(OWLClassD.class, composedEntity.getUri());
		assertNull(shouldBeNull);
		final OWLClassA shouldNotBeNull = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(shouldNotBeNull);
	}

	@Test
	public void testPersistSimpleChange() {
		LOG.config("TestPersistSimpleChange");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testPersistSimpleChangeWithoutCache", false);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.getTransaction().commit();
		pc.getTransaction().begin();
		final OWLClassA toChange = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(toChange);
		final String newString = "NewStringAttribute";
		toChange.setStringAttribute(newString);
		pc.getTransaction().commit();
		final OWLClassA changed = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(changed);
		assertEquals(newString, changed.getStringAttribute());
	}

	@Test
	public void testPersistReferenceChange() {
		LOG.config("TestPersistReferenceChange");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testPersistReferenceChangeWithoutCache", false);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.persist(composedEntity);
		pc.getTransaction().commit();
		pc.getTransaction().begin();
		final OWLClassD toChange = pc.find(OWLClassD.class, composedEntity.getUri());
		final OWLClassA newReference = new OWLClassA();
		final URI pk = URI.create("http://newReferenceToA");
		newReference.setUri(pk);
		assertNull(pc.find(OWLClassA.class, pk));
		newReference.setStringAttribute("AnotherStringAttribute");
		assertNull(pc.find(OWLClassA.class, pk));
		toChange.setOwlClassA(newReference);
		pc.persist(newReference);
		pc.getTransaction().commit();
		final OWLClassD changed = pc.find(OWLClassD.class, composedEntity.getUri());
		assertNotNull(changed);
		assertEquals(newReference.getUri(), changed.getOwlClassA().getUri());
		assertEquals(newReference.getStringAttribute(), changed.getOwlClassA().getStringAttribute());
		assertNotNull(pc.find(OWLClassA.class, pk));
		assertNotNull(pc.find(OWLClassA.class, testEntity.getUri()));
	}

	@Test
	public void testPersistCollectionChange() {
		LOG.config("TestPersistCollectionChange - empty collection changed to non-empty");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testPersistCollectionChangeWithoutCache", false);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.getTransaction().commit();
		pc.getTransaction().begin();
		final OWLClassA toChange = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(toChange);
		Set<String> col = new HashSet<String>();
		col.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#NumberOne");
		col.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#NumberTwo");
		col.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#NumberThree");
		toChange.setTypes(col);
		pc.getTransaction().commit();
		final OWLClassA changed = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(changed);
		assertNotNull(changed.getTypes());
		assertTrue(changed.getTypes().contains(
				"http://krizik.felk.cvut.cz/ontologies/jopa/entities#NumberTwo"));
	}

	@Test
	public void testAddObjectToCollection() {
		LOG.config("TestAddObjectToCollection - added element into collection managed by entity.");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testAddObjectToCollectionWithoutCache", false);
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeOne");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeTwo");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeThree");
		pc.getTransaction().begin();
		testEntity.setTypes(new HashSet<String>());
		for (String t : types) {
			testEntity.getTypes().add(t);
		}
		pc.persist(testEntity);
		pc.getTransaction().commit();
		final OWLClassA changed = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(changed);
		assertNotNull(changed.getTypes());
		assertEquals(types.size(), changed.getTypes().size());
		for (String t : types) {
			assertTrue(changed.getTypes().contains(t));
		}
	}

	@Test
	public void testPersistCollectionChangeII() {
		LOG.config("TestPersistCollectionChange - changed content of the collection");
		EntityManager pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testPersistCollectionChangeWithoutCacheII", false);
		pc.getTransaction().begin();
		final Set<String> orSet = new HashSet<String>();
		orSet.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#One");
		orSet.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Two");
		orSet.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Three");
		testEntity.setTypes(orSet);
		pc.persist(testEntity);
		pc.getTransaction().commit();
		final OWLClassA toChange = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(toChange);
		pc.getTransaction().begin();
		toChange.getTypes().remove("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Two");
		toChange.getTypes().add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Four");
		toChange.getTypes().add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Five");
		pc.getTransaction().commit();
		final OWLClassA changed = pc.find(OWLClassA.class, testEntity.getUri());
		assertNotNull(changed);
		assertTrue(changed.getTypes().contains(
				"http://krizik.felk.cvut.cz/ontologies/jopa/entities#Four"));
		assertTrue(changed.getTypes().contains(
				"http://krizik.felk.cvut.cz/ontologies/jopa/entities#Five"));
		assertFalse(changed.getTypes().contains(
				"http://krizik.felk.cvut.cz/ontologies/jopa/entities#Two"));
	}
}
