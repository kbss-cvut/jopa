package cz.cvut.kbss.jopa.query.sparql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.jopa.query.env.QueryTestEnvironment;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverCachingOwlapiFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

/**
 * These tests use the same data in multiple ontology contexts and compare query
 * results, which should be the same for all the contexts.
 * 
 * @author ledvima1
 * 
 */
public class MultiContextQueryTest {

	private static final Logger LOG = Logger.getLogger(MultiContextQueryTest.class.getName());

	private static final Map<String, String> properties = initProperties();
	private static final List<StorageInfo> storages = initStorages();

	private static EntityManagerFactory emf;
	private static List<Context> contexts;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		TestEnvironment.clearDatabase();
		TestEnvironment.resetOwldbHibernateProvider();
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"SPARQLMultiContextQueryTests", storages, true, properties);
		QueryTestEnvironment.generateTestData(em);
		emf = em.getEntityManagerFactory();
		contexts = em.getAvailableContexts();
		em.close();
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		emf.close();
		TestEnvironment.clearDatabase();
	}

	@Before
	public void setUp() throws Exception {
		emf.getCache().evictAll();
		this.em = emf.createEntityManager();
	}

	@After
	public void tearDown() throws Exception {
		em.close();
	}

	// --------------------
	//
	// This part uses Query
	//
	// --------------------

	@Test
	public void testSelectFromSingleContext() {
		LOG.config("Test: select subjects by type from a single context.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA> . }";
		final Query<List<String>> q = em.createNativeQuery(query, contexts.get(contexts.size() - 1)
				.getUri());
		assertNotNull(q);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		final List<OWLClassA> as = QueryTestEnvironment.getData(OWLClassA.class);
		assertEquals(as.size(), res.size());
		boolean found = false;
		for (OWLClassA a : as) {
			found = false;
			for (List<String> lst : res) {
				assertEquals(1, lst.size());
				if (a.getUri().toString().equals(lst.get(0))) {
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	@Test
	public void testSelectByDataProperty() {
		LOG.config("Test: select data property values from all contexts.");
		final String query = "SELECT ?y WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> ?y . }";
		final List<List<List<String>>> res = new ArrayList<>(contexts.size());
		for (Context ctx : contexts) {
			final Query<List<String>> q = em.createNativeQuery(query, ctx.getUri());
			final List<List<String>> subRes = q.getResultList();
			assertNotNull(subRes);
			assertFalse(subRes.isEmpty());
			res.add(subRes);
		}
		final Set<String> exp = new HashSet<>();
		for (OWLClassB e : QueryTestEnvironment.getData(OWLClassB.class)) {
			exp.add(e.getStringAttribute());
		}
		for (List<List<String>> lst : res) {
			assertEquals(exp.size(), lst.size());
			for (List<String> lst2 : lst) {
				assertEquals(1, lst2.size());
				// False means we got the expected value
				assertFalse(exp.add(lst2.get(0)));
			}
		}
	}

	@Test
	public void testSelectByObjectProperty() {
		LOG.config("Test: select subjects having the same object property value.");
		final OWLClassD d = QueryTestEnvironment.getData(OWLClassD.class).get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> <"
				+ d.getOwlClassA().getUri().toString() + "> . }";
		for (Context ctx : contexts) {
			final Query<List<String>> q = em.createNativeQuery(query, ctx.getUri());
			final List<List<String>> res = q.getResultList();
			assertEquals(1, res.size());
			final List<String> subRes = res.get(0);
			assertEquals(1, subRes.size());
			assertEquals(d.getUri().toString(), subRes.get(0));
		}
	}

	@Test
	public void testSelectTypes() {
		LOG.config("Test: select types of a subject.");
		final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
		final Set<String> types = a.getTypes();
		types.add(a.getClass().getAnnotation(OWLClass.class).iri());
		types.add(QueryTestEnvironment.OWL_THING);
		final String query = "SELECT ?x WHERE { <" + a.getUri().toString() + "> a ?x . }";
		for (Context ctx : contexts) {
			final Query<List<String>> q = em.createNativeQuery(query, ctx.getUri());
			final List<List<String>> res = q.getResultList();
			assertEquals(types.size(), res.size());
			for (List<String> lst : res) {
				assertEquals(1, lst.size());
				assertTrue(types.contains(lst.get(0)));
			}
		}
	}

	// -------------------------
	//
	// This part uses TypedQuery
	//
	// -------------------------

	@Test
	public void testSelectInstancesOfClass_Typed() {
		LOG.config("Test: select all instances of a single class. Typed query.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassD> .}";
		final List<OWLClassD> ds = QueryTestEnvironment.getData(OWLClassD.class);
		for (Context ctx : contexts) {
			final TypedQuery<OWLClassD> q = em.createNativeQuery(query, OWLClassD.class,
					ctx.getUri());
			final List<OWLClassD> res = q.getResultList();
			assertNotNull(res);
			assertFalse(res.isEmpty());
			assertEquals(ds.size(), res.size());
			boolean found = false;
			for (OWLClassD d : ds) {
				found = false;
				for (OWLClassD dd : res) {
					if (d.getUri().equals(dd.getUri())) {
						found = true;
						assertNotNull(dd.getOwlClassA());
						assertEquals(d.getOwlClassA().getUri(), dd.getOwlClassA().getUri());
						break;
					}
				}
				assertTrue(found);
			}
		}
	}

	@Test
	public void testSelectByTypeAndDataPropertyValue_Typed() {
		LOG.config("Test: select an instance by class and data property value. Typed query.");
		final OWLClassB b = QueryTestEnvironment.getData(OWLClassB.class).get(5);
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB> ; <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> \""
				+ b.getStringAttribute() + "\" . }";
		for (Context ctx : contexts) {
			final TypedQuery<OWLClassB> q = em.createNativeQuery(query, OWLClassB.class,
					ctx.getUri());
			final OWLClassB res = q.getSingleResult();
			assertNotNull(res);
			assertEquals(b.getUri(), res.getUri());
			assertEquals(b.getStringAttribute(), res.getStringAttribute());
		}
	}

	@Test
	public void testSelectByObjectProperty_Typed() {
		LOG.config("Test: select instances by object property. Using setMaxResults. Typed query.");
		final List<OWLClassD> ds = QueryTestEnvironment.getData(OWLClassD.class);
		final int cnt = ds.size() / 2;
		assertTrue(cnt > 1);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> ?y . }";
		for (Context ctx : contexts) {
			final TypedQuery<OWLClassD> q = em.createNativeQuery(query, OWLClassD.class,
					ctx.getUri());
			final List<OWLClassD> res = q.setMaxResults(cnt).getResultList();
			assertEquals(cnt, res.size());
		}
	}

	private static List<StorageInfo> initStorages() {
		final List<StorageInfo> lst = new ArrayList<StorageInfo>(3);
		lst.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
		lst.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.OWLDB));
		lst.add(new StorageInfo(OntologyConnectorType.JENA, OwlapiStorageType.FILE));
		return lst;
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> props = new HashMap<>();
		props.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverCachingOwlapiFactory.class.getName());
		props.put(OntoDriverProperties.JENA_DRIVER_FACTORY,
				DriverCachingJenaFactory.class.getName());
		return props;
	}
}
