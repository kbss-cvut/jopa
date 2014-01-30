package cz.cvut.kbss.jopa.test.sesame.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.Generators;
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestCreateOperations {

	private static final Logger LOG = Logger.getLogger(TestCreateOperations.class.getName());

	private static final List<StorageConfig> storages = initStorages();
	private static final Map<String, String> properties = initProperties();

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassC entityC;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;
	// Two relationships
	private static OWLClassG entityG;
	private static OWLClassH entityH;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		entityA.setTypes(types);
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityC = new OWLClassC();
		entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityI"));
		entityI.setOwlClassA(entityA);
		entityH = new OWLClassH();
		entityH.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityH"));
		entityH.setOwlClassA(entityA);
		entityG = new OWLClassG();
		entityG.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
		entityG.setOwlClassH(entityH);
	}

	@After
	public void tearDown() throws Exception {
		if (em.isOpen()) {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}
			em.close();
			em.getEntityManagerFactory().close();
		}
		entityE.setUri(null);
		entityC.setSimpleList(null);
		entityC.setReferencedList(null);
	}

	@Test
	public void testPersistWithGenerated() {
		LOG.config("Test: persist into all contexts, also with generated id.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistWithGenerated", storages, false,
				properties);
		final List<Context> contexts = em.getAvailableContexts();
		final Context ctx1 = contexts.get(0);
		final Context ctx2 = contexts.get(1);
		em.getTransaction().begin();
		em.persist(entityA, ctx1.getUri());
		em.persist(entityA, ctx2.getUri());
		em.persist(entityE, ctx1.getUri());
		em.persist(entityB, ctx2.getUri());
		em.getTransaction().commit();

		final OWLClassA resA1 = em.find(OWLClassA.class, entityA.getUri(), ctx1.getUri());
		assertNotNull(resA1);
		assertEquals(entityA.getStringAttribute(), resA1.getStringAttribute());
		assertEquals(entityA.getTypes().size(), resA1.getTypes().size());
		assertTrue(entityA.getTypes().containsAll(resA1.getTypes()));
		final OWLClassA resA2 = em.find(OWLClassA.class, entityA.getUri(), ctx2.getUri());
		assertNotNull(resA2);
		assertEquals(entityA.getStringAttribute(), resA2.getStringAttribute());
		assertEquals(entityA.getTypes().size(), resA2.getTypes().size());
		assertTrue(entityA.getTypes().containsAll(resA2.getTypes()));

		assertNotNull(entityE.getUri());
		final OWLClassE resE = em.find(OWLClassE.class, entityE.getUri(), ctx1.getUri());
		assertNotNull(resE);
		assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());

		final OWLClassB resB = em.find(OWLClassB.class, entityB.getUri(), ctx2.getUri());
		assertNotNull(resB);
		assertEquals(entityB.getUri(), resB.getUri());
		assertEquals(entityB.getStringAttribute(), resB.getStringAttribute());
		assertTrue(em.contains(resB));
	}

	@Test
	public void testPersistCascade() {
		LOG.config("Test: persist with cascade over two relationships.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistWithCascade", storages, false,
				properties);
		final List<Context> contexts = em.getAvailableContexts();
		final Context ctx1 = contexts.get(0);
		final Context ctx2 = contexts.get(1);
		em.getTransaction().begin();
		em.persist(entityA, ctx1.getUri());
		em.persist(entityD, ctx1.getUri());
		em.persist(entityG, ctx2.getUri());
		em.getTransaction().commit();

		final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), ctx1.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		assertEquals(resD.getOwlClassA(), resA);

		final OWLClassA resA2 = em.find(OWLClassA.class, entityA.getUri(), ctx2.getUri());
		assertNotNull(resA2);
		final OWLClassH resH = em.find(OWLClassH.class, entityH.getUri(), ctx2.getUri());
		assertNotNull(resH);
		assertEquals(resH.getOwlClassA(), resA2);
		final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri(), ctx2.getUri());
		assertNotNull(resG);
		assertEquals(resG.getOwlClassH(), resH);
		assertEquals(resG.getOwlClassH().getOwlClassA(), resA2);
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceInOne() {
		LOG.config("Test: persist twice into one context.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistTwice", storages, false,
				properties);
		em.getTransaction().begin();
		em.persist(entityB);
		em.persist(entityB);
		em.getTransaction().commit();
		fail("This line should not have been reached.");
	}

	@Test(expected = RollbackException.class)
	public void testPersistWithoutCascade() {
		LOG.config("Test: try persisting relationship not marked as cascade.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistWithoutCascade", storages,
				false, properties);
		em.getTransaction().begin();
		em.persist(entityD);
		em.getTransaction().commit();
		fail("This line should not have been reached.");
	}

	@Test
	public void testPersistSimpleList() {
		LOG.config("Test: persist entity with simple list.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistSimpleList", storages, false,
				properties);
		URI ctx = em.getAvailableContexts().get(em.getAvailableContexts().size() - 1).getUri();
		entityC.setSimpleList(Generators.createSimpleList(5));
		em.getTransaction().begin();
		em.persist(entityC, ctx);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, ctx);
		}
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityC.getSimpleList().get(1).getUri(), ctx);
		assertNotNull(a);
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertNotNull(c);
		assertNotNull(c.getSimpleList());
		assertFalse(c.getSimpleList().isEmpty());
		assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
		assertTrue(c.getSimpleList().contains(a));
	}

	@Test(expected = RollbackException.class)
	public void testPersistSimpleListNoCascade() {
		LOG.config("Test: persist entity with simple list, but don't persist the referenced entities.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistSimpleListNoCascade", storages,
				false, properties);
		entityC.setSimpleList(Generators.createSimpleList(10));
		em.getTransaction().begin();
		em.persist(entityC);
		em.getTransaction().commit();
		fail("This line should not have been reached.");
	}

	@Test
	public void testPersistReferencedList() {
		LOG.config("Test: persist entity with referenced list.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistReferencedList", storages,
				false, properties);
		entityC.setReferencedList(Generators.createReferencedList(5));
		em.getTransaction().begin();
		em.persist(entityC);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a);
		}
		assertTrue(em.contains(entityC));
		assertTrue(em.contains(entityC.getReferencedList().get(0)));
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(c);
		assertNotNull(c.getReferencedList());
		assertFalse(c.getReferencedList().isEmpty());
		assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
		for (OWLClassA a : entityC.getReferencedList()) {
			final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
			assertNotNull(resA);
			assertEquals(a.getStringAttribute(), resA.getStringAttribute());
			assertTrue(c.getReferencedList().contains(resA));
		}
	}

	@Test(expected = RollbackException.class)
	public void testPersistReferencedListNoCascade() {
		LOG.config("Test: persist entity with referenced list. Don't persist the referenced entities.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistReferencedListNoCascade",
				storages, false, properties);
		entityC.setReferencedList(Generators.createReferencedList(5));
		em.getTransaction().begin();
		em.persist(entityC);
		em.getTransaction().commit();
		fail("This line should not have been reached.");
	}

	@Test
	public void estPersistSimpleAndReferencedList() {
		LOG.config("Test: persist entity with both simple and referenced list.");
		em = TestEnvironment.getPersistenceConnector("SesamePersistSimpleAndReferencedList",
				storages, false, properties);
		entityC.setReferencedList(Generators.createReferencedList(5));
		entityC.setSimpleList(Generators.createSimpleList(5));
		em.getTransaction().begin();
		em.persist(entityC);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a);
		}
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(c);
		assertNotNull(c.getSimpleList());
		assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
		assertNotNull(c.getReferencedList());
		assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
		for (OWLClassA a : entityC.getSimpleList()) {
			final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
			assertNotNull(resA);
			assertTrue(c.getSimpleList().contains(resA));
		}
		for (OWLClassA a : entityC.getReferencedList()) {
			final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
			assertNotNull(resA);
			assertTrue(c.getReferencedList().contains(resA));
		}
	}

	@Test
	public void testPersistProperties() {
		LOG.config("Test: persist entity with properties.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistWithProperties",
				storages, false, properties);
		final Map<String, Set<String>> props = new HashMap<>(3);
		props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyOne", Collections
				.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/Individial10"));
		props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyTwo", Collections
				.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/SomeEntity"));
		props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyThree",
				Collections.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
		entityB.setProperties(props);
		em.getTransaction().begin();
		em.persist(entityB);
		em.getTransaction().commit();
		em.clear();

		final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
		assertNotNull(res.getProperties());
		assertFalse(res.getProperties().isEmpty());
		assertEquals(props.size(), res.getProperties().size());
		for (Entry<String, Set<String>> e : props.entrySet()) {
			assertTrue(res.getProperties().containsKey(e.getKey()));
			final Set<String> s = e.getValue();
			final Set<String> resS = res.getProperties().get(e.getKey());
			assertNotNull(resS);
			assertEquals(1, resS.size());
			assertEquals(s.iterator().next(), resS.iterator().next());
		}
	}

	@Test
	public void testPersistPropertiesEmpty() {
		LOG.config("Test: persist entity with properties. The properties will be an empty map.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistWithPropertiesEmpty",
				storages, false, properties);
		entityB.setProperties(Collections.<String, Set<String>> emptyMap());
		final Context ctx = em.getAvailableContexts().get(em.getAvailableContexts().size() - 1);
		em.getTransaction().begin();
		em.persist(entityB, ctx.getUri());
		assertTrue(em.contains(entityB));
		em.getTransaction().commit();
		em.clear();

		final OWLClassB b = em.find(OWLClassB.class, entityB.getUri(), ctx.getUri());
		assertNotNull(b);
		assertEquals(entityB.getUri(), b.getUri());
		assertEquals(entityB.getStringAttribute(), b.getStringAttribute());
		assertNull(b.getProperties());
	}

	private static List<StorageConfig> initStorages() {
		final List<StorageConfig> lst = new ArrayList<>(2);
		lst.add(new SesameNativeStorageConfig());
		lst.add(new SesameMemoryStorageConfig());
		return lst;
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
