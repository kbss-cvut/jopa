package cz.cvut.kbss.jopa.test.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.TestEnvironment;

public class TestRetrieveOperations {

	private Logger log = TestEnvironment.getLogger();

	private static OWLClassC testC;
	private static OWLClassF testF;
	private static List<OWLClassA> classes;
	private static List<OWLClassA> refList;

	private EntityManager pc;

	@BeforeClass
	public static void beforeClass() throws Exception {
		testC = new OWLClassC();
		final URI pkC = URI.create("http://testC");
		testC.setUri(pkC);
		classes = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI.create("http://classA" + Integer.toString(i));
			a.setUri(pkA);
			a.setStringAttribute("StringAttribute" + Integer.toString(i + 1));
			classes.add(a);
		}
		refList = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkRA = URI
					.create("http://refA" + Integer.toString(i + 1));
			a.setUri(pkRA);
			a.setStringAttribute("strAttForRefA_" + Integer.toString(i + 1));
			refList.add(a);
		}
		testC.setSimpleList(classes);
		testF = new OWLClassF();
		final URI pkF = URI.create("http://testF");
		testF.setUri(pkF);
	}

	@After
	public void tearDown() throws Exception {
		if (pc.isOpen()) {
			pc.close();
		}
	}

	@Test
	public void testFindEntity() {
		log.info("Test: Find entity");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testFindEntity");
		pc.clear();
		final OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://textA");
		entity.setUri(pk);
		try {
			pc.getTransaction().begin();
			pc.persist(entity);
			pc.getTransaction().commit();
			pc.clear();
			OWLClassA result = pc.find(OWLClassA.class, pk);
			assertNotNull(result);
			assertEquals(pk, result.getUri());
		} catch (OWLPersistenceException e) {
			log.info("Loading entity failed.");
			fail();
		}
	}

	@Test(expected = NullPointerException.class)
	public void testFindNull() {
		log.info("Test: Find null entity");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testFindNull");
		pc.clear();
		final URI uri = null;
		@SuppressWarnings("unused")
		final OWLClassA res = pc.find(OWLClassA.class, uri);
		fail("This line should not have been reached.");
	}

	@Test
	public void testContainsMember() {
		log.info("Test: does the EntityManager contain object which is a member of another object");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testContainsMember");
		final OWLClassA a = new OWLClassA();
		final URI pkOne = URI.create("http://testA");
		a.setUri(pkOne);
		a.setStringAttribute("someStringAttribute");
		final OWLClassD d = new OWLClassD();
		final URI pkTwo = URI.create("http://testD");
		d.setUri(pkTwo);
		d.setOwlClassA(a);
		pc.getTransaction().begin();
		pc.persist(a);
		pc.persist(d);
		pc.getTransaction().commit();
		OWLClassD resD = pc.find(OWLClassD.class, pkTwo);
		assertNotNull(resD);
		assertTrue(pc.contains(resD.getOwlClassA()));
	}

	@Test
	public void testRefreshEntity() {
		log.info("Test: refresh the entity state.");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testRefreshEntity");
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://testA");
		a.setUri(pk);
		a.setStringAttribute("stringAttribute");
		pc.getTransaction().begin();
		pc.persist(a);
		pc.getTransaction().commit();
		OWLClassA toChange = pc.find(OWLClassA.class, pk);
		assertNotNull(toChange);
		toChange.setStringAttribute("newString");
		pc.refresh(toChange);
		assertEquals(a.getStringAttribute(), toChange.getStringAttribute());
	}
}
