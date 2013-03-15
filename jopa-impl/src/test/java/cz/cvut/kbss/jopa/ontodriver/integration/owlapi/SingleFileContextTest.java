package cz.cvut.kbss.jopa.ontodriver.integration.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.ontodriver.TestEnv;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class SingleFileContextTest {

	private static final Logger LOG = Logger.getLogger(SingleFileContextTest.class.getName());

	private static final Map<OntologyConnectorType, OwlapiStorageType> storage = Collections
			.singletonMap(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE);

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;

	private static DataSource ds;
	private static PersistenceProviderFacade facade;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		entityA.setTypes(Collections.singleton("justOneType"));
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://entityD"));
		entityD.setOwlClassA(entityA);
		facade = TestEnv.getProviderFacade();
	}

	@Test
	public void testSimplePersist() throws Exception {
		LOG.config("Test: simple persist.");
		ds = TestEnv.createDataSource("ontoDriverSimplePersist", storage);
		final Connection c = ds.getConnection(facade);
		c.setAutoCommit(true);
		c.persist(entityB.getUri(), entityB);
		final OWLClassB res = c.find(entityB.getClass(), entityB.getUri());
		assertNotNull(res);
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
	}

}
