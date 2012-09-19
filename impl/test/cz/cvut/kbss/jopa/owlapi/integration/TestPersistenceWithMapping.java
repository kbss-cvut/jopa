package cz.cvut.kbss.jopa.owlapi.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.net.URI;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment.Storage;

public class TestPersistenceWithMapping {

	private static EntityManager em;

	private static OWLClassA testA;

	@BeforeClass
	public static void setUp() throws Exception {
		testA = new OWLClassA();
		final URI pk = URI.create("http://testA");
		testA.setUri(pk);
		testA.setStringAttribute("someStringAttribute");
	}

	@After
	public void tearDown() throws Exception {
		em.close();
	}

	@Test
	public void testPersistEntity() {
		em = TestEnvironment.getPersistenceConnectorWithMappingFile(
				"TestPersistenceWithMapping-testPersistEntity", Storage.FILE,
				true);
		em.getTransaction().begin();
		em.persist(testA);
		em.getTransaction().commit();
		em.clear();
		final OWLClassA res = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(res);
		assertEquals(testA.getStringAttribute(), res.getStringAttribute());
	}

}
