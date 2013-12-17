package cz.cvut.kbss.jopa.test.sesame.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

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

public class TestRetrieveOperations {

	private static final Logger LOG = Logger.getLogger(TestRetrieveOperations.class.getName());

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
	public void testRetrieveLazy() throws Exception {
		LOG.config("Test: retrieve entity with lazy loaded attribute.");
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveLazy", storages, false,
				properties);
		final List<Context> contexts = em.getAvailableContexts();
		assertEquals(2, contexts.size());
		final URI ctx1 = contexts.get(0).getUri();
		final URI ctx2 = contexts.get(1).getUri();
		em.getTransaction().begin();
		em.persist(entityI, ctx1);
		em.persist(entityG, ctx2);
		em.getTransaction().commit();

		final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri(), ctx1);
		assertNotNull(resI);
		final Field f = OWLClassI.class.getDeclaredField("owlClassA");
		f.setAccessible(true);
		Object value = f.get(resI);
		assertNull(value);
		assertNotNull(resI.getOwlClassA());
		value = f.get(resI);
		assertNotNull(value);
		assertEquals(entityA.getUri(), resI.getOwlClassA().getUri());
		assertTrue(em.contains(resI.getOwlClassA()));

		final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri(), ctx2);
		assertNotNull(resG);
		assertNotNull(resG.getOwlClassH());
		assertNotNull(resG.getOwlClassH().getOwlClassA());
		assertEquals(resI.getOwlClassA().getUri(), resG.getOwlClassH().getOwlClassA().getUri());
	}

	@Test
	public void testRetrieveGenerated() throws Exception {
		LOG.config("Test: persist and retrieve several entities with generated identifiers.");
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveGenerated", storages, false,
				properties);
		em.getTransaction().begin();
		final List<OWLClassE> lst = new ArrayList<>(10);
		for (int i = 0; i < 10; i++) {
			final OWLClassE e = new OWLClassE();
			e.setStringAttribute("blablabla" + i);
			final URI ctx = em.getAvailableContexts().get(i % 2).getUri();
			assertNull(e.getUri());
			em.persist(e, ctx);
			assertNotNull(e.getUri());
			lst.add(e);
		}
		em.getTransaction().commit();

		em.clear();
		int i = 0;
		for (OWLClassE e : lst) {
			final URI ctx = em.getAvailableContexts().get((i++) % 2).getUri();
			final OWLClassE res = em.find(OWLClassE.class, e.getUri(), ctx);
			assertNotNull(res);
			assertEquals(e.getStringAttribute(), res.getStringAttribute());
		}
	}

	@Test
	public void testRetrieveNotExisting() {
		LOG.config("Test: retrieve entity which does not exist in the specified context.");
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveNotExisting", storages, false,
				properties);
		em.getTransaction().begin();
		em.persist(entityB, em.getAvailableContexts().get(1).getUri());
		em.getTransaction().commit();

		final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), em.getAvailableContexts()
				.get(0).getUri());
		assertNull(res);
	}

	@Test
	public void testRefresh() {
		LOG.config("Test: refresh entity.");
		em = TestEnvironment.getPersistenceConnector("SesameRefresh", storages, false, properties);
		em.getTransaction().begin();
		em.persist(entityD);
		em.persist(entityA);
		em.getTransaction().commit();

		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		newA.setStringAttribute("newA");
		final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
		assertEquals(d.getOwlClassA(), a);
		d.setOwlClassA(newA);
		em.refresh(d);
		assertEquals(a.getUri(), d.getOwlClassA().getUri());
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
