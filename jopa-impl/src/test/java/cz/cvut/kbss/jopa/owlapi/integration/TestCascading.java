package cz.cvut.kbss.jopa.owlapi.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassG;
import cz.cvut.kbss.jopa.owlapi.OWLClassH;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;

public class TestCascading {

	private static final Logger LOG = Logger.getLogger(TestCascading.class.getName());

	private static OWLClassH testH;
	private static OWLClassG testG;
	private static OWLClassA testA;

	private static EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		LOG.setLevel(Level.ALL);
		testH = new OWLClassH();
		final URI pkH = URI.create("http://testH");
		testH.setUri(pkH);
		testG = new OWLClassG();
		final URI pkG = URI.create("http://testG");
		testG.setUri(pkG);
		testA = new OWLClassA();
		final URI pkA = URI.create("http://testA");
		testA.setUri(pkA);
		testA.setStringAttribute("someStringAttribute");
	}

	@After
	public void tearDown() {
		if (em.isOpen()) {
			em.close();
		}
	}

	@Test
	public void testPersistSimpleCascade() {
		LOG.config("Test: persist, cascade over one relationship.");
		em = TestEnvironment.getPersistenceConnector("TestCascading-testPersistSimpleCascade");
		testH.setOwlClassA(testA);
		em.getTransaction().begin();
		em.persist(testH);
		assertTrue(em.contains(testH));
		assertTrue(em.contains(testA));
		em.getTransaction().commit();
		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();
		final OWLClassH resH = em.find(OWLClassH.class, testH.getUri());
		assertNotNull(resH);
		final OWLClassA resA = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(resA);
//		assertEquals(resA, resH.getOwlClassA());
		assertEquals(testA.getStringAttribute(), resA.getStringAttribute());
	}

	@Test
	public void testPersistDoubleCascade() {
		LOG.config("Test: persist, cascade over two relationships.");
		em = TestEnvironment.getPersistenceConnector("TestCascading-testPersistDoubleCascade");
		testH.setOwlClassA(testA);
		testG.setOwlClassH(testH);
		em.getTransaction().begin();
		em.persist(testG);
		assertTrue(em.contains(testG));
		assertTrue(em.contains(testH));
		assertTrue(em.contains(testA));
		em.getTransaction().commit();
		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();
		final OWLClassG resG = em.find(OWLClassG.class, testG.getUri());
		assertNotNull(resG);
		final OWLClassH resH = em.find(OWLClassH.class, testH.getUri());
		assertNotNull(resH);
		final OWLClassA resA = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(resA);
	}

	@Test
	public void testRemoveSimpleCascade() {
		LOG.config("Test: remove, cascade over a single relationship.");
		em = TestEnvironment.getPersistenceConnector("TestCascading-testRemoveSimpleCascade");
		testH.setOwlClassA(testA);
		em.getTransaction().begin();
		em.persist(testH);
		em.getTransaction().commit();
		final OWLClassH resH = em.find(OWLClassH.class, testH.getUri());
		assertNotNull(resH);
		em.getTransaction().begin();
		em.remove(resH);
		em.getTransaction().commit();
		final OWLClassH remH = em.find(OWLClassH.class, testH.getUri());
		assertNull(remH);
		final OWLClassA remA = em.find(OWLClassA.class, testA.getUri());
		assertNull(remA);
	}
}
