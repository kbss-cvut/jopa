package cz.cvut.kbss.jopa.test.owldb.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.Collections;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.OwldbStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;

public class BasicOperationsTest {

	private static EntityManager em;

	@BeforeClass
	public static void setupBeforeClass() throws Exception {
		TestEnvironment.clearDatabase();
		TestEnvironment.resetOwldbHibernateProvider();
		em = TestEnvironment.getPersistenceConnector("OWLDBTestBasicOperations",
				Collections.<StorageConfig> singletonList(new OwldbStorageConfig()), true);
	}

	@Before
	public void setup() throws Exception {
		em.getEntityManagerFactory().getCache().evictAll();
		em.clear();
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		if (em != null) {
			em.getEntityManagerFactory().close();
		}
	}

	@Test
	public void testFetchSimpleData() {
		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://new#A");
		a.setUri(uri);

		a.setStringAttribute("new-value");

		em.getTransaction().begin();
		em.persist(a);
		assertTrue(em.contains(a));
		em.getTransaction().commit();

		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();

		assertFalse(em.contains(a));

		final OWLClassA aX = em.find(OWLClassA.class, uri);

		assertNotNull(aX);

		assertEquals(aX.getStringAttribute(), "new-value");
	}

	@Test
	public void testPersistEntity() {
		final OWLClassA a = new OWLClassA();
		final URI uri = URI.create("http://persistA");
		a.setUri(uri);
		final String str = "PersistTestString";
		a.setStringAttribute(str);
		em.getTransaction().begin();
		em.persist(a);
		em.getTransaction().commit();

		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();

		final OWLClassA aX = em.find(OWLClassA.class, uri);
		assertNotNull(aX);
		assertEquals(str, aX.getStringAttribute());
	}

	@Test
	public void testPersistRelationship() {
		final OWLClassA a = new OWLClassA();
		final URI uri = URI.create("http://persistARelationship");
		a.setUri(uri);
		final String str = "PersistTestString";
		a.setStringAttribute(str);
		final OWLClassD d = new OWLClassD();
		d.setOwlClassA(a);
		final URI dUri = URI.create("persistD");
		d.setUri(dUri);
		em.getTransaction().begin();
		em.persist(a);
		em.persist(d);
		em.getTransaction().commit();
		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();

		final OWLClassD dX = em.find(OWLClassD.class, dUri);
		assertNotNull(dX);
		assertNotNull(dX.getOwlClassA());
		assertEquals(uri, dX.getOwlClassA().getUri());
	}

	@Test
	public void testRemoveEntity() {
		final OWLClassA a = new OWLClassA();
		final URI uri = URI.create("http://aToRemove");
		a.setUri(uri);
		em.getTransaction().begin();
		em.persist(a);
		em.getTransaction().commit();

		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();
		assertFalse(em.contains(a));

		final OWLClassA aX = em.find(OWLClassA.class, uri);
		assertNotNull(aX);
		em.getTransaction().begin();
		em.remove(aX);
		em.getTransaction().commit();

		em.clear();
		final OWLClassA res = em.find(OWLClassA.class, uri);
		assertNull(res);
	}

}
