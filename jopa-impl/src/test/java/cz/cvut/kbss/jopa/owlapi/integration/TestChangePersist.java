package cz.cvut.kbss.jopa.owlapi.integration;

import java.net.URI;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import junit.framework.TestCase;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;

public class TestChangePersist extends TestCase {

	private static EntityManager pc;

	private static String strAttr = "TestAttribute";
	private static Set<String> strTypes;

	private OWLClassA testEntityOne;
	private OWLClassD testEntityTwo;

	public TestChangePersist() {
		this.testEntityOne = new OWLClassA();
		final URI uri = URI.create("http://testOne");
		this.testEntityOne.setUri(uri);
		this.testEntityOne.setStringAttribute(strAttr);
		this.testEntityTwo = new OWLClassD();
		final URI dUri = URI.create("http://testOWLClassD");
		this.testEntityTwo.setUri(dUri);
		this.testEntityTwo.setOwlClassA(testEntityOne);
	}

	@BeforeClass
	public static void setStatics() {
		strTypes = new HashSet<String>();
		strTypes.add("typeOne");
		strTypes.add("typeTwo");
		strTypes.add("typeThree");
	}

	@Before
	public void setUp() {
		this.testEntityOne.setStringAttribute(strAttr);
		this.testEntityOne.setTypes(strTypes);
		this.testEntityTwo.setOwlClassA(testEntityOne);
	}

	@After
	public void tearDown() {
		if (pc.isOpen()) {
			pc.getEntityManagerFactory().close();
		}
	}

	/**
	 * Test saving changes of Strings or built-in types (or simple immutable
	 * types in general).
	 */
	@Test
	public void testCommitSimpleChange() {
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testCommitSimpleChange");
		pc.clear();
		pc.getTransaction().begin();
		pc.persist(testEntityOne);
		pc.getTransaction().commit();

		pc.getTransaction().begin();
		final OWLClassA toChange = pc.find(OWLClassA.class, testEntityOne.getUri());
		assertNotNull(toChange);
		String newAtt = "NewOne";
		toChange.setStringAttribute(newAtt);
		pc.getTransaction().commit();
		final OWLClassA changed = pc.find(OWLClassA.class, testEntityOne.getUri());
		assertEquals(newAtt, changed.getStringAttribute());
	}

	@Test
	public void testCommitCollectionChange() {
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testCommitCollectionChange");
		pc.clear();
		pc.getTransaction().begin();
		pc.persist(testEntityOne);
		pc.getTransaction().commit();

		pc.getTransaction().begin();
		final OWLClassA toChange = pc.find(OWLClassA.class, testEntityOne.getUri());
		assertNotNull(toChange);
		Set<String> newTypes = new HashSet<String>();
		newTypes.add("NewTypeOne");
		newTypes.add("NewTypeTwo");
		toChange.setTypes(newTypes);
		pc.getTransaction().commit();
		final OWLClassA changed = pc.find(OWLClassA.class, testEntityOne.getUri());
		assertEquals(newTypes.size(), changed.getTypes().size());
		Iterator<String> it = changed.getTypes().iterator();
		Iterator<String> it2 = newTypes.iterator();
		while (it.hasNext()) {
			assertEquals(it2.next(), it.next());
		}
	}

	@Test
	public void testCommitReferenceChange() {
		final OWLClassA newRef = new OWLClassA();
		final URI newUri = URI.create("http://newURIforA");
		newRef.setUri(newUri);
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testCommitReferenceChange");
		pc.clear();
		pc.getTransaction().begin();
		pc.persist(testEntityOne);
		pc.persist(testEntityTwo);
		pc.persist(newRef);
		pc.getTransaction().commit();

		pc.getTransaction().begin();
		final OWLClassD toChange = pc.find(OWLClassD.class, testEntityTwo.getUri());
		assertNotNull(toChange);
		toChange.setOwlClassA(newRef);
		pc.getTransaction().commit();

		final OWLClassD changed = pc.find(OWLClassD.class, testEntityTwo.getUri());
		assertNotNull(changed.getOwlClassA());
		assertEquals(newUri, changed.getOwlClassA().getUri());
	}

	@Test
	public void testComitChangeInReference() {
		final String changedAttribute = "changedString";
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testCommitChangeInReference");
		pc.clear();
		pc.getTransaction().begin();
		pc.persist(testEntityOne);
		pc.persist(testEntityTwo);
		pc.getTransaction().commit();

		pc.getTransaction().begin();
		final OWLClassD toChange = pc.find(OWLClassD.class, testEntityTwo.getUri());
		assertNotNull(toChange);
		toChange.getOwlClassA().setStringAttribute(changedAttribute);
		pc.getTransaction().commit();

		final OWLClassA res = pc.find(OWLClassA.class, testEntityOne.getUri());
		assertNotNull(res);
		assertEquals(changedAttribute, res.getStringAttribute());
	}

}
