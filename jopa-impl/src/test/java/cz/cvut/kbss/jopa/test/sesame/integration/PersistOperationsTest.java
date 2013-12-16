package cz.cvut.kbss.jopa.test.sesame.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class PersistOperationsTest {

	private static final Logger LOG = Logger.getLogger(PersistOperationsTest.class.getName());

	private static final List<StorageConfig> storages = initStorages();
	private static final Map<String, String> properties = initProperties();

	private static OWLClassA entityA;
	private static OWLClassB entityB;
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

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
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

	private static List<StorageConfig> initStorages() {
		final List<StorageConfig> lst = new ArrayList<>(2);
		lst.add(new SesameNativeStorageConfig());
		lst.add(new SesameMemoryStorageConfig());
		return lst;
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, "true");
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, "false");
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
