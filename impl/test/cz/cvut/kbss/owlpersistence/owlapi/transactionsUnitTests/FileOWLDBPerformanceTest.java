package cz.cvut.kbss.owlpersistence.owlapi.transactionsUnitTests;

import static org.junit.Assert.assertNotNull;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassA;
import cz.cvut.kbss.owlpersistence.owlapi.TestEnvironment;

public class FileOWLDBPerformanceTest {

	private static final Logger LOG = Logger
			.getLogger(FileOWLDBPerformanceTest.class.getName());
	private static final int COUNT = 1000;
	private static final int FIND_CNT = COUNT / 10;
	private static final String IRI_PREFIX = "http://classA";

	private static List<OWLClassA> entitiesA;
	private static List<URI> ids;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entitiesA = new ArrayList<OWLClassA>(COUNT);
		for (int i = 0; i < COUNT; i++) {
			final OWLClassA a = new OWLClassA();
			final URI pk = URI.create(IRI_PREFIX + Integer.toString(i));
			final String strAtt = "StringAttribute";
			a.setUri(pk);
			a.setStringAttribute(strAtt);
			final Set<String> types = new HashSet<String>();
			types.add("TypeOne");
			types.add("TypeTwo");
			types.add("TypeThree");
			a.setTypes(types);
			entitiesA.add(a);
		}
		final Random rand = new Random();
		ids = new ArrayList<URI>();
		for (int i = 0; i < FIND_CNT; i++) {
			final Integer id = rand.nextInt(COUNT);
			ids.add(URI.create(IRI_PREFIX + id));
		}
	}

	@Test
	public void testFileOntologyPerformancePersist() {
		LOG.config("Testing file ontology access performance. Persisting "
				+ COUNT + " entities.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"FileOntologyPerformanceTest-Persist", false, true);

		persistEntities(em);
	}

	@Test
	public void testOWLDBOntologyPerformancePersist() {
		LOG.config("Testing OWLDB ontology access performance. Persisting "
				+ COUNT + " entities.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"owldb", true, true);

		persistEntities(em);
		em.getEntityManagerFactory().close();
	}

	/**
	 * Persist predefined entities using the specified entity manager.
	 * 
	 * @param em
	 *            EntityManager
	 */
	private void persistEntities(final EntityManager em) {
		em.getTransaction().begin();
		for (OWLClassA a : entitiesA) {
			em.persist(a);
		}
		em.getTransaction().commit();
	}

	@Test
	public void testFileOntologyPerformanceRead() {
		LOG.config("Search for several randomly chosen entities and measure file ontology performance.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"FileOntologyPerformanceTest-Find", false, false);
		persistEntities(em);

		findEntities(em);
	}

	@Test
	public void testOWLDBOntologyPerformanceRead() {
		LOG.config("Search for several randomly chosen entities and measure OWLDB ontology performance.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"owldb", true, false);
		persistEntities(em);

		findEntities(em);
	}

	private void findEntities(final EntityManager em) {
		em.clear();
		for (URI id : ids) {
			final OWLClassA a = em.find(OWLClassA.class, id);
			assertNotNull(a);
		}
	}

}
