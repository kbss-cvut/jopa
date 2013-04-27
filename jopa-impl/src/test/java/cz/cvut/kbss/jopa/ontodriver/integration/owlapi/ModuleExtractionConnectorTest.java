package cz.cvut.kbss.jopa.ontodriver.integration.owlapi;

import static org.junit.Assert.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.ontodriver.TestEnv;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.OWLClassI;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverModularizingOwlapiFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class ModuleExtractionConnectorTest {

	private static final Logger LOG = Logger.getLogger(SingleFileContextTest.class.getName());

	private static final List<StorageInfo> storage = Collections.singletonList(new StorageInfo(
			OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
	private static final String OWLCLASS_A_REFERENCE_FIELD = "owlClassA";
	private static final Map<String, String> properties = initProperties();

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;

	private static DataSource ds;
	private static PersistenceProviderFacade facade;
	private static Connection c;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		entityA.setTypes(Collections.singleton("JustOneType"));
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://entityI"));
		entityI.setOwlClassA(entityA);
		facade = TestEnv.getProviderFacade();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		entityE.setUri(null);
	}

	@Test
	public void testPersistSingle() throws OntoDriverException {
		LOG.config("Test: persist single entity.");
		acquireConnection("ModuleExtractionPersistSingle");
		c.setAutoCommit(true);
		c.persist(entityA.getUri(), entityA);

		final OWLClassA res = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
		assertNotNull(res.getTypes());
		assertTrue(entityA.getTypes().containsAll(res.getTypes()));
	}

	@Test
	public void testPersistRelationship() throws OntoDriverException {
		LOG.config("Test: persist two related entities.");
		acquireConnection("ModuleExtractionPersistRelationship");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		assertNotNull(contexts);
		assertEquals(storage.size(), contexts.size());
		final Context ctx = contexts.get(0);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.commit();

		final OWLClassD resD = c.find(OWLClassD.class, entityD.getUri(), ctx.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri(), ctx.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
	}

	@Test
	public void testPersistMultiple() throws Exception {
		LOG.config("Test: persist multiple entities.");
		acquireConnection("ModuleExtractionPersistMultiple");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityD.getUri(), entityD);
		c.persist(entityI.getUri(), entityI);
		c.commit();
		c.persist(entityB.getUri(), entityB);
		assertNull(entityE.getUri());
		c.persist(entityE.getUri(), entityE);
		c.commit();
		assertNotNull(entityE.getUri());

		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		final OWLClassB resB = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		final OWLClassD resD = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		final OWLClassE resE = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		final OWLClassI resI = c.find(OWLClassI.class, entityI.getUri());
		final Field f = OWLClassI.class.getDeclaredField(OWLCLASS_A_REFERENCE_FIELD);
		f.setAccessible(true);
		assertNull(f.get(resI));
		c.loadFieldValue(resI, f);
		assertNotNull(resI.getOwlClassA());
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove an entity.");
		acquireConnection("ModuleExtractionRemoveEntity");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityB.getUri(), entityB);
		c.persist(entityD.getUri(), entityD);
		c.persist(entityE.getUri(), entityE);
		c.commit();

		assertNotNull(c.find(OWLClassA.class, entityA.getUri()));
		assertNotNull(c.find(OWLClassB.class, entityB.getUri()));
		assertNotNull(c.find(OWLClassE.class, entityE.getUri()));
		final OWLClassD toRemove = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(toRemove);
		c.remove(toRemove.getUri(), toRemove);
		c.commit();

		assertNotNull(c.find(OWLClassA.class, entityA.getUri()));
		assertNotNull(c.find(OWLClassB.class, entityB.getUri()));
		assertNotNull(c.find(OWLClassE.class, entityE.getUri()));
		assertNull(c.find(OWLClassD.class, entityD.getUri()));
	}

	@Test
	public void testRollback() throws Exception {
		LOG.config("Test: rollback.");
		acquireConnection("ModuleExtractionRollback");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityB.getUri(), entityB);
		c.persist(entityD.getUri(), entityD);
		c.persist(entityE.getUri(), entityE);
		c.commit();

		assertNotNull(c.find(OWLClassA.class, entityA.getUri()));
		assertNotNull(c.find(OWLClassB.class, entityB.getUri()));
		final OWLClassE toRemoveE = c.find(OWLClassE.class, entityE.getUri());
		assertNotNull(toRemoveE);
		c.remove(toRemoveE.getUri(), toRemoveE);
		final OWLClassD toRemove = c.find(OWLClassD.class, entityD.getUri());
		assertNotNull(toRemove);
		c.remove(toRemove.getUri(), toRemove);
		c.rollback();

		assertNotNull(c.find(OWLClassA.class, entityA.getUri()));
		assertNotNull(c.find(OWLClassB.class, entityB.getUri()));
		assertNotNull(c.find(OWLClassE.class, entityE.getUri()));
		assertNotNull(c.find(OWLClassD.class, entityD.getUri()));
	}

	@Test
	public void testMerge() throws Exception {
		LOG.config("Test: merge entity change.");
		acquireConnection("ModuleExtractionMerge");
		c.setAutoCommit(false);
		c.persist(entityA.getUri(), entityA);
		c.persist(entityB.getUri(), entityB);
		c.commit();

		final OWLClassA a = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		final String newType = entityB.getClass().getName();
		assertFalse(a.getTypes().isEmpty());
		a.getTypes().add(newType);
		final OWLClassB b = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(b);
		final String newString = "NewStringAttribute";
		b.setStringAttribute(newString);
		c.merge(a.getUri(), a);
		c.merge(b.getUri(), b);
		c.commit();

		final OWLClassA resA = c.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		assertEquals(entityA.getTypes().size() + 1, resA.getTypes().size());
		assertTrue(resA.getTypes().contains(newType));
		final OWLClassB resB = c.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		assertEquals(newString, resB.getStringAttribute());
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storage, properties, false);
		c = ds.getConnection(facade);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<String, String>();
		m.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverModularizingOwlapiFactory.class.getName());
		return m;
	}
}
