package cz.cvut.kbss.ontodriver.test.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverModularizingOwlapiFactory;
import cz.cvut.kbss.ontodriver.test.BaseSingleContextOntoDriverTest;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class ModuleExtractionConnectorTest extends BaseSingleContextOntoDriverTest {

	private static final Map<String, String> properties = initProperties();

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		entityA.setTypes(Collections
				.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/entities#SomeTestType"));
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
		for (String t : entityA.getTypes()) {
			facade.getMetamodel().addUriToModuleExtractionSignature(URI.create(t));
		}
		storageConfig = new OwlapiStorageConfig();
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
		persist(entityA.getUri(), entityA);

		final OWLClassA res = find(OWLClassA.class, entityA.getUri());
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
		persist(entityA.getUri(), entityA);
		persist(entityD.getUri(), entityD);
		c.commit();

		final OWLClassD resD = find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		final OWLClassA resA = find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
	}

	@Test
	public void testPersistMultiple() throws Exception {
		LOG.config("Test: persist multiple entities.");
		acquireConnection("ModuleExtractionPersistMultiple");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityD.getUri(), entityD);
		persist(entityI.getUri(), entityI);
		c.commit();
		persist(entityB.getUri(), entityB);
		assertNull(entityE.getUri());
		persist(entityE.getUri(), entityE);
		c.commit();
		assertNotNull(entityE.getUri());

		final OWLClassA resA = find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		final OWLClassB resB = find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
		final OWLClassD resD = find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		final OWLClassE resE = find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		final OWLClassI resI = find(OWLClassI.class, entityI.getUri());
		final Field f = OWLClassI.getOwlClassAField();
		f.setAccessible(true);
		assertNull(f.get(resI));
		loadFieldValue(resI, f);
		assertNotNull(resI.getOwlClassA());
	}

	@Test
	public void testRemove() throws Exception {
		LOG.config("Test: remove an entity.");
		acquireConnection("ModuleExtractionRemoveEntity");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityB.getUri(), entityB);
		persist(entityD.getUri(), entityD);
		persist(entityE.getUri(), entityE);
		c.commit();

		assertNotNull(find(OWLClassA.class, entityA.getUri()));
		assertNotNull(find(OWLClassB.class, entityB.getUri()));
		assertNotNull(find(OWLClassE.class, entityE.getUri()));
		final OWLClassD toRemove = find(OWLClassD.class, entityD.getUri());
		assertNotNull(toRemove);
		remove(toRemove.getUri());
		c.commit();

		assertNotNull(find(OWLClassA.class, entityA.getUri()));
		assertNotNull(find(OWLClassB.class, entityB.getUri()));
		assertNotNull(find(OWLClassE.class, entityE.getUri()));
		assertNull(find(OWLClassD.class, entityD.getUri()));
	}

	@Test
	public void testRollback() throws Exception {
		LOG.config("Test: rollback.");
		acquireConnection("ModuleExtractionRollback");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityB.getUri(), entityB);
		persist(entityD.getUri(), entityD);
		persist(entityE.getUri(), entityE);
		c.commit();

		assertNotNull(find(OWLClassA.class, entityA.getUri()));
		assertNotNull(find(OWLClassB.class, entityB.getUri()));
		final OWLClassE toRemoveE = find(OWLClassE.class, entityE.getUri());
		assertNotNull(toRemoveE);
		remove(toRemoveE.getUri());
		final OWLClassD toRemove = find(OWLClassD.class, entityD.getUri());
		assertNotNull(toRemove);
		remove(toRemove.getUri());
		c.rollback();

		assertNotNull(find(OWLClassA.class, entityA.getUri()));
		assertNotNull(find(OWLClassB.class, entityB.getUri()));
		assertNotNull(find(OWLClassE.class, entityE.getUri()));
		assertNotNull(find(OWLClassD.class, entityD.getUri()));
	}

	@Test
	public void testMerge() throws Exception {
		LOG.config("Test: merge entity change.");
		acquireConnection("ModuleExtractionMerge");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityB.getUri(), entityB);
		c.commit();

		final OWLClassA a = find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		final String newType = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB";
		assertFalse(a.getTypes().isEmpty());
		a.getTypes().add(newType);
		facade.getMetamodel().addUriToModuleExtractionSignature(URI.create(newType));
		final OWLClassB b = find(OWLClassB.class, entityB.getUri());
		assertNotNull(b);
		final String newString = "NewStringAttribute";
		b.setStringAttribute(newString);
		final Field typesField = OWLClassA.getTypesField();
		final Field strField = OWLClassB.getStrAttField();
		merge(a, typesField);
		merge(b, strField);
		c.commit();

		// Make sure the extended signature is used
		final Connection cTwo = ds.getConnection(facade);
		try {
			final OWLClassA resA = cTwo.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
			assertNotNull(resA);
			assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
			assertEquals(entityA.getTypes().size() + 1, resA.getTypes().size());
			assertTrue(resA.getTypes().contains(newType));
			final OWLClassB resB = cTwo.find(OWLClassB.class, entityB.getUri(), DEFAULT_DESCRIPTOR);
			assertNotNull(resB);
			assertEquals(newString, resB.getStringAttribute());
		} finally {
			cTwo.close();
		}
	}

	@Override
	protected void acquireConnection(String baseName) throws OntoDriverException {
		this.ds = TestEnv.createDataSource(baseName, storageConfig, properties);
		this.c = ds.getConnection(facade);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<String, String>();
		m.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverModularizingOwlapiFactory.class.getName());
		return m;
	}
}
