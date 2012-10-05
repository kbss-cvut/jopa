package cz.cvut.kbss.jopa.owlapi.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
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

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.OWLClassF;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;

public class TestUpdateOperations {

	private static final Logger log = TestEnvironment.getLogger();

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
			pc.getEntityManagerFactory().close();
		}
	}

	@Test
	public void testMergeDetachedEntity() {
		log.info("Test: Merge detached (detached during transaction)");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testMergeDetachedEntity");
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://TestA");
		entity.setStringAttribute("OriginalStringAttribute");
		entity.setUri(pk);
		pc.getTransaction().begin();
		pc.persist(entity);
		pc.getTransaction().commit();
		pc.getTransaction().begin();
		OWLClassA det = pc.find(OWLClassA.class, pk);
		pc.detach(det);
		assertFalse(pc.contains(det));
		det.setStringAttribute("NewStringAttribute");
		pc.merge(det);
		assertTrue(pc.contains(det));
		pc.getTransaction().commit();
		det = pc.find(OWLClassA.class, pk);
		assertNotNull(det);
		assertEquals("NewStringAttribute", det.getStringAttribute());
	}

	@Test
	public void testMergeDetachedOutsideTransaction() {
		log.info("Test: Merge detached (detached outside transaction)");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testMergeDetachedOutsideTransaction");
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://TestA");
		entity.setStringAttribute("OriginalStringAttribute");
		entity.setUri(pk);
		pc.getTransaction().begin();
		pc.persist(entity);
		pc.getTransaction().commit();
		OWLClassA det = pc.find(OWLClassA.class, pk);
		pc.detach(det);
		det.setStringAttribute("NewStringAttribute");
		pc.getTransaction().begin();
		pc.merge(det);
		pc.getTransaction().commit();
		det = pc.find(OWLClassA.class, pk);
		assertNotNull(det);
		assertEquals("NewStringAttribute", det.getStringAttribute());
	}

	@Test
	public void testChangeReferenceInList() {
		log.info("Test: change a reference in referenced list of an entity and persist this change");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testChangeReferenceInList");
		testC.setReferencedList(refList);
		pc.getTransaction().begin();
		for (OWLClassA ref : testC.getReferencedList()) {
			pc.persist(ref);
		}
		for (OWLClassA simple : testC.getSimpleList()) {
			pc.persist(simple);
		}
		pc.persist(testC);
		pc.getTransaction().commit();
		OWLClassC c = pc.find(OWLClassC.class, testC.getUri());
		assertNotNull(c);
		pc.getTransaction().begin();
		OWLClassA a = c.getReferencedList().get(3);
		final String nStr = "newString";
		a.setStringAttribute(nStr);
		pc.getTransaction().commit();
		pc.getEntityManagerFactory().getCache().evictAll();
		c = pc.find(OWLClassC.class, testC.getUri());
		boolean found = false;
		for (OWLClassA aa : c.getReferencedList()) {
			if (nStr.equals(aa.getStringAttribute())) {
				found = true;
			}
		}
		assertTrue(found);
	}

	@Test(expected = OWLPersistenceException.class)
	public void testChangeInferredAttribute() {
		log.info("Test: change inferred attribute.");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testChangeInferredAttribute");
		pc.getTransaction().begin();
		pc.persist(testF);
		pc.getTransaction().commit();
		pc.clear();
		final OWLClassF res = pc.find(OWLClassF.class, testF.getUri());
		res.setSecondStringAttribute("ChangedSecondString");
		fail("This line should not have been reached.");
	}
}
