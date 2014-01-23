package cz.cvut.kbss.jopa.test.sesame.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

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
import cz.cvut.kbss.jopa.test.OWLClassC;
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

public class TestUpdateOperations {

	private static final Logger LOG = Logger.getLogger(TestUpdateOperations.class.getName());

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
	public void testUpdateReference() {
		LOG.config("Test: update reference to entity.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReference", storages, true,
				properties);
		final URI ctx1 = em.getAvailableContexts().get(0).getUri();
		final URI ctx2 = em.getAvailableContexts().get(1).getUri();
		em.getTransaction().begin();
		em.persist(entityD, ctx1);
		em.persist(entityA, ctx1);
		em.persist(entityI, ctx2);
		em.getTransaction().commit();

		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newEntityA"));
		newA.setStringAttribute("newA");
		em.getTransaction().begin();
		final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d.getOwlClassA());
		d.setOwlClassA(newA);
		em.persist(newA, ctx1);
		final OWLClassI i = em.find(OWLClassI.class, entityI.getUri(), ctx2);
		assertNotNull(i.getOwlClassA());
		i.setOwlClassA(newA);
		em.persist(newA, ctx2);
		em.getTransaction().commit();

		final OWLClassA resA1 = em.find(OWLClassA.class, newA.getUri(), ctx1);
		assertNotNull(resA1);
		final OWLClassD resD = em.find(OWLClassD.class, d.getUri());
		assertEquals(resD.getOwlClassA(), resA1);
		assertNotNull(em.find(OWLClassA.class, entityA.getUri(), ctx1));
		final OWLClassI resI = em.find(OWLClassI.class, i.getUri());
		assertEquals(newA.getUri(), resI.getOwlClassA().getUri());
		assertNotNull(em.find(OWLClassA.class, entityA.getUri(), ctx2));
	}

	@Test
	public void testMergeDetachedWithChanges() {
		LOG.config("Test: merge detached entity with changes.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDetached", storages, true,
				properties);
		em.getTransaction().begin();
		em.persist(entityA);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
		assertTrue(em.contains(a));
		em.detach(a);
		assertFalse(em.contains(a));
		final String newType = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#AddedType";
		a.getTypes().add(newType);
		em.getTransaction().begin();
		final OWLClassA merged = em.merge(a);
		assertTrue(merged.getTypes().contains(newType));
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, a.getUri());
		assertEquals(a.getTypes().size(), res.getTypes().size());
		assertTrue(res.getTypes().containsAll(a.getTypes()));
	}

	@Test
	public void testMergeDetachedCascade() {
		LOG.config("Test: merge detached with cascade.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateCascade", storages, true,
				properties);
		final URI ctx = em.getAvailableContexts().get(1).getUri();
		em.getTransaction().begin();
		em.persist(entityH, ctx);
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		final OWLClassH h = em.find(OWLClassH.class, entityH.getUri(), ctx);
		assertNotNull(h.getOwlClassA());
		em.detach(h);
		assertFalse(em.contains(h));
		assertFalse(em.contains(h.getOwlClassA()));
		final String newStr = "newStringAttribute";
		h.getOwlClassA().setStringAttribute(newStr);
		em.getTransaction().begin();
		final OWLClassH merged = em.merge(h, ctx);
		assertEquals(newStr, merged.getOwlClassA().getStringAttribute());
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), ctx);
		assertNotNull(res);
		assertEquals(newStr, res.getStringAttribute());
	}

	@Test
	public void testRemoveFromSimpleList() {
		LOG.config("Test: remove entity from simple list. (But keep it in the ontology.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromSimpleList", storages,
				true, properties);
		entityC.setSimpleList(createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(c);
		em.getTransaction().begin();
		final OWLClassA a = c.getSimpleList().get(1);
		c.getSimpleList().remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
		assertNotNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, c.getUri());
		assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
		assertEquals(entityC.getSimpleList().size() - 1, resC.getSimpleList().size());
		for (OWLClassA aa : resC.getSimpleList()) {
			assertFalse(resA.getUri().equals(aa.getUri()));
		}
	}

	@Test
	public void testAddToSimpleList() {
		LOG.config("Test: add entity to simple list.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToSimpleList", storages, true,
				properties);
		final URI ctx = em.getAvailableContexts().get(em.getAvailableContexts().size() - 1)
				.getUri();
		entityC.setSimpleList(createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC, ctx);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, ctx);
		}
		em.persist(entityA, ctx);
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertNotNull(c);
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), ctx);
		assertNotNull(a);
		assertFalse(c.getSimpleList().contains(a));
		c.getSimpleList().add(a);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertEquals(c.getSimpleList().size(), resC.getSimpleList().size());
		assertEquals(entityC.getSimpleList().size() + 1, resC.getSimpleList().size());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), ctx);
		assertNotNull(resA);
		assertTrue(resC.getSimpleList().contains(resA));
	}

	@Test
	public void testClearSimpleList() {
		LOG.config("Test: clear a simple list (but keep the entities in ontology).");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearSimpleList", storages, true,
				properties);
		entityC.setSimpleList(createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(c);
		assertFalse(c.getSimpleList().isEmpty());
		em.getTransaction().begin();
		c.getSimpleList().clear();
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(resC);
		assertTrue(resC.getSimpleList().isEmpty());
		for (OWLClassA a : entityC.getSimpleList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri()));
		}
	}

	@Test
	public void testReplaceSimpleList() {
		LOG.config("Test: replace simple list with a new one.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceSimpleList", storages,
				true, properties);
		final URI ctx = em.getAvailableContexts().get(em.getAvailableContexts().size() - 1)
				.getUri();
		entityC.setSimpleList(createSimpleList());
		em.getTransaction().begin();
		em.persist(entityC, ctx);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, ctx);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx);
		final List<OWLClassA> newList = new ArrayList<>(1);
		newList.add(entityA);
		em.getTransaction().begin();
		em.persist(entityA, ctx);
		c.setSimpleList(newList);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertNotNull(resC);
		assertEquals(newList.size(), resC.getSimpleList().size());
		boolean found = false;
		for (OWLClassA a : newList) {
			found = false;
			for (OWLClassA aa : resC.getSimpleList()) {
				if (a.getUri().equals(aa.getUri())) {
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
		for (OWLClassA a : entityC.getSimpleList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri(), ctx));
		}
	}

	@Test
	public void testRemoveFromReferencedList() {
		LOG.config("Test: remove entity from referenced list. (But keep it in the ontology.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromReferencedList",
				storages, true, properties);
		entityC.setReferencedList(createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(c);
		em.getTransaction().begin();
		final OWLClassA a = c.getReferencedList().get(3);
		c.getReferencedList().remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
		assertNotNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, c.getUri());
		assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
		assertEquals(entityC.getReferencedList().size() - 1, resC.getReferencedList().size());
		for (OWLClassA aa : resC.getReferencedList()) {
			assertFalse(resA.getUri().equals(aa.getUri()));
		}
	}

	@Test
	public void testAddToReferencedList() {
		LOG.config("Test: add entity to Referenced list.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToReferencedList", storages,
				true, properties);
		final URI ctx = em.getAvailableContexts().get(em.getAvailableContexts().size() - 1)
				.getUri();
		entityC.setReferencedList(createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC, ctx);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, ctx);
		}
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertNotNull(c);
		em.persist(entityA, ctx);
		c.getReferencedList().add(entityA);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertEquals(c.getReferencedList().size(), resC.getReferencedList().size());
		assertEquals(entityC.getReferencedList().size() + 1, resC.getReferencedList().size());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), ctx);
		assertNotNull(resA);
		assertTrue(resC.getReferencedList().contains(resA));
	}

	@Test
	public void testClearReferencedList() {
		LOG.config("Test: clear referenced list (but keep the entities in ontology).");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearReferencedList", storages,
				true, properties);
		entityC.setReferencedList(createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(c);
		assertFalse(c.getReferencedList().isEmpty());
		em.getTransaction().begin();
		c.setReferencedList(null);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(resC);
		assertNull(resC.getReferencedList());
		for (OWLClassA a : entityC.getReferencedList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri()));
		}
	}

	@Test
	public void testReplaceReferencedList() {
		LOG.config("Test: replace referenced list with a new one.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceReferencedList", storages,
				true, properties);
		entityC.setReferencedList(createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
		final List<OWLClassA> newList = new ArrayList<>(1);
		newList.add(entityA);
		em.getTransaction().begin();
		em.persist(entityA);
		c.setReferencedList(newList);
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
		assertNotNull(resC);
		assertEquals(newList.size(), resC.getReferencedList().size());
		boolean found = false;
		for (OWLClassA a : newList) {
			found = false;
			for (OWLClassA aa : resC.getReferencedList()) {
				if (a.getUri().equals(aa.getUri())) {
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
		for (OWLClassA a : entityC.getReferencedList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri()));
		}
	}

	private static List<OWLClassA> createSimpleList() {
		final List<OWLClassA> lst = new ArrayList<>(5);
		int counter = 110;
		for (int i = 0; i < 5; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityASimple"
					+ counter));
			a.setStringAttribute("stringAttributeeee" + counter++);
			lst.add(a);
		}
		return lst;
	}

	private static List<OWLClassA> createReferencedList() {
		final List<OWLClassA> lst = new ArrayList<>(5);
		int counter = 101;
		for (int i = 0; i < 5; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI
					.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityAReferenced"
							+ counter));
			a.setStringAttribute("stringAttributeeee" + counter++);
			lst.add(a);
		}
		return lst;
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
