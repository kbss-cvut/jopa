package cz.cvut.kbss.jopa.test.sesame.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

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
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestSharedConnector {

	private static final Logger LOG = Logger.getLogger(TestSharedConnector.class.getName());

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

	private EntityManager emOne;
	private EntityManager emTwo;

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
		if (emOne.isOpen()) {
			if (emTwo.isOpen()) {
				if (emTwo.getTransaction().isActive()) {
					emTwo.getTransaction().rollback();
				}
				emTwo.close();
			}
			if (emOne.getTransaction().isActive()) {
				emOne.getTransaction().rollback();
			}
			emOne.close();
			emOne.getEntityManagerFactory().close();
		}
		entityE.setUri(null);
		entityC.setSimpleList(null);
		entityC.setReferencedList(null);
	}

	@Test
	public void testPersist() {
		LOG.config("Test: persist in one persistence context and check if it is correctly interpretted in the other.");
		emOne = TestEnvironment.getPersistenceConnector("SesameSharedConnectorPersist", storages,
				false, properties);
		emTwo = emOne.getEntityManagerFactory().createEntityManager();
		emOne.getTransaction().begin();
		// TODO This is probably not entirely correct, the transaction on emTwo
		// shouldn't be required here
		emTwo.getTransaction().begin();
		emOne.persist(entityA);
		emOne.persist(entityB);
		assertNull(emTwo.find(OWLClassA.class, entityA.getUri()));
		assertNull(emTwo.find(OWLClassB.class, entityB.getUri()));
		emOne.getTransaction().commit();
		emTwo.getTransaction().rollback();

		emTwo.clear();
		emOne.clear();
		final OWLClassA aOne = emOne.find(OWLClassA.class, entityA.getUri());
		assertNotNull(aOne);
		final OWLClassA aTwo = emTwo.find(OWLClassA.class, entityA.getUri());
		assertNotNull(aTwo);
		assertEquals(aOne.getStringAttribute(), aTwo.getStringAttribute());
		assertEquals(aOne.getTypes(), aTwo.getTypes());
		final OWLClassB bOne = emOne.find(OWLClassB.class, entityB.getUri());
		assertNotNull(bOne);
		final OWLClassB bTwo = emTwo.find(OWLClassB.class, entityB.getUri());
		assertNotNull(bTwo);
		assertEquals(bOne.getStringAttribute(), bTwo.getStringAttribute());
	}

	@Test
	public void testPersistGeneratePk() {
		LOG.config("Test: persist in two persistence contexts, each with id generation.");
		emOne = TestEnvironment.getPersistenceConnector("SesameSharedConnectorPersistGeneratePk",
				storages, false, properties);
		emTwo = emOne.getEntityManagerFactory().createEntityManager();
		final OWLClassE ee = new OWLClassE();
		ee.setStringAttribute("blablablablalb");
		emOne.getTransaction().begin();
		emOne.persist(entityE);
		emTwo.getTransaction().begin();
		emTwo.persist(ee);
		assertFalse(entityE.getUri().equals(ee.getUri()));
		emTwo.getTransaction().commit();
		emOne.getTransaction().commit();
		final OWLClassE eOne = emOne.find(OWLClassE.class, entityE.getUri());
		assertNotNull(eOne);
		final OWLClassE eeOne = emOne.find(OWLClassE.class, ee.getUri());
		assertNotNull(eeOne);
		final OWLClassE eTwo = emTwo.find(OWLClassE.class, entityE.getUri());
		assertNotNull(eTwo);
		final OWLClassE eeTwo = emTwo.find(OWLClassE.class, ee.getUri());
		assertNotNull(eeTwo);
		assertEquals(eOne.getUri(), eTwo.getUri());
		assertEquals(eOne.getStringAttribute(), eTwo.getStringAttribute());
		assertEquals(eeOne.getUri(), eeTwo.getUri());
		assertEquals(eeOne.getStringAttribute(), eeTwo.getStringAttribute());
	}

	@Test
	public void testUpdateReference() {
		LOG.config("Test: persist with a reference and then change the reference.");
		emOne = TestEnvironment.getPersistenceConnector("SesameSharedConnectorUpdateReference",
				storages, false, properties);
		emTwo = emOne.getEntityManagerFactory().createEntityManager();
		emOne.getTransaction().begin();
		emOne.persist(entityD);
		emOne.persist(entityA);
		emOne.getTransaction().commit();

		emTwo.getTransaction().begin();
		final OWLClassD d = emTwo.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d);
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newEntityA"));
		newA.setStringAttribute("blablablabla");
		d.setOwlClassA(newA);
		emTwo.persist(newA);
		emTwo.getTransaction().commit();

		final OWLClassD dOne = emOne.find(OWLClassD.class, d.getUri());
		assertNotNull(dOne);
		assertEquals(newA.getUri(), dOne.getOwlClassA().getUri());
		final OWLClassA aOne = emOne.find(OWLClassA.class, entityA.getUri());
		assertNotNull(aOne);

		final OWLClassD dTwo = emTwo.find(OWLClassD.class, d.getUri());
		assertNotNull(dTwo);
		assertEquals(newA.getUri(), dTwo.getOwlClassA().getUri());
		final OWLClassA aTwo = emTwo.find(OWLClassA.class, entityA.getUri());
		assertNotNull(aTwo);
	}

	@Test
	public void testRemove() {
		LOG.config("Test: remove entity in one persistence context.");
		emOne = TestEnvironment.getPersistenceConnector("SesameSharedConnectorRemove", storages,
				false, properties);
		emTwo = emOne.getEntityManagerFactory().createEntityManager();
		emOne.getTransaction().begin();
		emOne.persist(entityD);
		emOne.persist(entityA);
		emOne.getTransaction().commit();

		emTwo.getTransaction().begin();
		final OWLClassD d = emTwo.find(OWLClassD.class, entityD.getUri());
		assertNotNull(d);
		final OWLClassA a = emTwo.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		assertEquals(d.getOwlClassA(), a);
		emTwo.remove(d);
		emTwo.getTransaction().commit();

		final OWLClassD dOne = emOne.find(OWLClassD.class, d.getUri());
		assertNull(dOne);
		final OWLClassA aOne = emOne.find(OWLClassA.class, a.getUri());
		assertNotNull(aOne);
		final OWLClassD dTwo = emTwo.find(OWLClassD.class, d.getUri());
		assertNull(dTwo);
		final OWLClassA aTwo = emTwo.find(OWLClassA.class, a.getUri());
		assertNotNull(aTwo);
		assertEquals(aOne.getUri(), aTwo.getUri());
		assertEquals(aOne.getStringAttribute(), aTwo.getStringAttribute());
	}

	private static List<StorageConfig> initStorages() {
		final List<StorageConfig> lst = new ArrayList<>(1);
		lst.add(new SesameNativeStorageConfig());
		return lst;
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
