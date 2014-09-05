package cz.cvut.kbss.jopa.test.performance;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class LegacyVsNewStorageAccess {

	private static final int SIZE = 10000;
	private static final boolean legacy = true;
	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private static List<OWLClassA> data;

	private EntityManager em;

	public static void main(String[] args) {
		initData();
		new LegacyVsNewStorageAccess().run();
	}

	private static void initData() {
		data = new ArrayList<>(SIZE);
		final Set<String> types = new HashSet<>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassV");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassW");
		for (int i = 0; i < SIZE; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA_" + i));
			a.setTypes(types);
			a.setStringAttribute("StringAttribute_" + i);
			data.add(a);
		}
	}

	private static StorageConfig initStorage() {
		return new SesameMemoryStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		if (!legacy) {
			System.out.println("Using the new storage.");
			map.put("storage", "new");
		} else {
			System.out.println("Using the legacy storage.");
		}
		return map;
	}

	private void run() {
		this.em = TestEnvironment.getPersistenceConnector(
				"SesameLegacyVsNewStorageAccessPerformance", storage, false, properties);
		final long start = System.currentTimeMillis();
		try {
			runImpl();
		} finally {
			em.close();
			em.getEntityManagerFactory().close();
		}
		final long end = System.currentTimeMillis();
		System.out.println("Execution took " + (end - start) + " ms.");
	}

	private void runImpl() {
		em.getTransaction().begin();
		for (OWLClassA a : data) {
			em.persist(a);
		}
		em.getTransaction().commit();
		em.getEntityManagerFactory().getCache().evictAll();
		for (OWLClassA a : data) {
			final OWLClassA res = em.find(OWLClassA.class, a.getUri());
			Assert.assertNotNull(res);
			Assert.assertFalse(res.getTypes().isEmpty());
			Assert.assertEquals(3, res.getTypes().size());
		}
	}
}
