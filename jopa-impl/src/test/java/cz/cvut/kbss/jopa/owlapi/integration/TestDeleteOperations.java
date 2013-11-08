package cz.cvut.kbss.jopa.owlapi.integration;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.OWLClassF;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;

public class TestDeleteOperations {

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
			pc.getEntityManagerFactory().close();
		}
	}

	@Test
	public void testRemoveEntity() {
		log.info("Test: Remove entity");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testRemoveEntity");
		pc.clear();
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://textA");
		entity.setUri(pk);
		try {
			pc.getTransaction().begin();
			pc.persist(entity);
			pc.getTransaction().commit();
			OWLClassA fnd = pc.find(OWLClassA.class, pk);
			assertNotNull(fnd);

			pc.getTransaction().begin();
			pc.remove(fnd);
			pc.getTransaction().commit();
			fnd = pc.find(OWLClassA.class, pk);
			assertNull(fnd);
		} catch (OWLPersistenceException e) {
			log.info("Remove failed with exception.");
			e.printStackTrace();
			fail();
		}
	}
}
