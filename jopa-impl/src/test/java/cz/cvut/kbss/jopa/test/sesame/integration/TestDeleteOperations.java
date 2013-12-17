package cz.cvut.kbss.jopa.test.sesame.integration;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestDeleteOperations {

	private static final Logger LOG = Logger.getLogger(TestDeleteOperations.class.getName());

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
	public void testRemoveReference() {
		LOG.config("Test: remove entity referenced by another entity.");
		em = TestEnvironment.getPersistenceConnector("SesameRemoveReference", storages, true,
				properties);
		em.getTransaction().begin();
		em.persist(entityD);
		em.persist(entityA);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		em.getTransaction().begin();
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassD res = em.find(OWLClassD.class, entityD.getUri());
		assertNotNull(res);
		// TODO When a is removed, the reference to it should be removed from
		// all entities in cache
		// assertNull(res.getOwlClassA());
		assertNull(em.find(OWLClassA.class, entityA.getUri()));
	}

	@Test
	public void testRemoveCascade() {
		LOG.config("Test: remove cascade.");
		em = TestEnvironment.getPersistenceConnector("SesameRemoveCascade", storages, true,
				properties);
		em.getTransaction().begin();
		em.persist(entityG);
		assertTrue(em.contains(entityG));
		assertTrue(em.contains(entityH));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassG g = em.find(OWLClassG.class, entityG.getUri());
		final OWLClassH h = em.find(OWLClassH.class, entityH.getUri());
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
		assertTrue(em.contains(g));
		assertTrue(em.contains(h));
		assertTrue(em.contains(a));
		assertNotNull(g);
		em.remove(g);
		assertFalse(em.contains(g));
		assertFalse(em.contains(h));
		assertFalse(em.contains(a));
		em.getTransaction().commit();

		assertNull(em.find(OWLClassG.class, entityG.getUri()));
		assertNull(em.find(OWLClassH.class, entityH.getUri()));
		assertNull(em.find(OWLClassA.class, entityA.getUri()));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRemoveDetached() {
		LOG.config("Test: try removing detached entity.");
		em = TestEnvironment.getPersistenceConnector("SesameRemoveDetached", storages, true,
				properties);
		final URI ctx = em.getAvailableContexts().get(1).getUri();
		em.getTransaction().begin();
		assertNull(entityE.getUri());
		em.persist(entityE, ctx);
		em.getTransaction().commit();
		assertNotNull(entityE.getUri());

		em.getTransaction().begin();
		final OWLClassE e = em.find(OWLClassE.class, entityE.getUri(), ctx);
		assertNotNull(e);
		assertTrue(em.contains(e));
		em.detach(e);
		assertFalse(em.contains(e));
		em.remove(e);
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
