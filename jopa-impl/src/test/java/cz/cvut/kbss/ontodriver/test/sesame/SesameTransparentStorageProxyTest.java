package cz.cvut.kbss.ontodriver.test.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.test.BaseSingleContextOntoDriverTest;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class SesameTransparentStorageProxyTest extends BaseSingleContextOntoDriverTest {

	private static final Map<String, String> properties = initProperties();

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// ID generated
	private static OWLClassE entityE;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLTypeForA");
		entityA.setTypes(types);
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		facade = TestEnv.getProviderFacade();
		storageConfig = new SesameNativeStorageConfig();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		ds.close();
		entityE.setUri(null);
	}

	@Test
	public void testSetDataProperty() throws Exception {
		LOG.config("Test: update data property value during a transaction.");
		acquireConnection("SesameTransparentProxySetDataProperty");
		persist(entityA.getUri(), entityA);

		c.setAutoCommit(false);
		final OWLClassA a = find(OWLClassA.class, entityA.getUri());
		a.setStringAttribute("newString");
		final Field strField = OWLClassA.getStrAttField();
		merge(a, strField);
		final OWLClassA other = find(OWLClassA.class, entityA.getUri());
		assertEquals(entityA.getStringAttribute(), other.getStringAttribute());
	}

	@Test
	public void testSetObjectProperty() throws Exception {
		LOG.config("Test: update object property value during a transaction.");
		acquireConnection("SesameTransparentProxySetObjectProperty");
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityANew"));
		newA.setStringAttribute("newA'sString");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityD.getUri(), entityD);
		persist(newA.getUri(), newA);
		c.commit();

		final OWLClassD d = find(OWLClassD.class, entityD.getUri());
		final OWLClassA a = find(OWLClassA.class, newA.getUri());
		d.setOwlClassA(a);
		final Field aField = OWLClassD.getOwlClassAField();
		merge(d, aField);
		final OWLClassD other = find(OWLClassD.class, entityD.getUri());
		assertEquals(d.getUri(), other.getUri());
		loadFieldValue(d, OWLClassD.class.getDeclaredField("owlClassA"));
		assertEquals(entityA.getUri(), d.getOwlClassA().getUri());
	}

	@Test
	public void testPersistAndSparql() throws Exception {
		LOG.config("Test: persist an entity and ask for it using SPARQL.");
		acquireConnection("SesameTransparentProxyPersist");
		c.setAutoCommit(false);
		persist(entityB.getUri(), entityB);
		final String sparql = " SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB> . }";
		final ResultSet rs = c.createStatement().executeQuery(sparql);
		assertFalse(rs.hasNext());
		rs.close();
		c.commit();
		final OWLClassB b = find(OWLClassB.class, entityB.getUri());
		assertNotNull(b);
		final ResultSet rs2 = c.createStatement().executeQuery(sparql);
		assertTrue(rs2.hasNext());
		rs2.next();
		final URI u = rs2.getObject(0, URI.class);
		rs2.close();
		assertEquals(entityB.getUri(), u);
	}

	@Override
	protected void acquireConnection(String ontoName) throws OntoDriverException {
		this.ds = TestEnv.createDataSource(ontoName, storageConfig, properties);
		this.c = ds.getConnection(facade);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<>();
		m.put(OWLAPIPersistenceProperties.LANG, "en");
		m.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		m.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.FALSE.toString());
		return m;
	}
}
