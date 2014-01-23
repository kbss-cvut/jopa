package cz.cvut.kbss.jopa.test.query.sparql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.query.env.QueryTestEnvironment;
import cz.cvut.kbss.ontodriver.Context;

public class QueryExecutor {

	public void testSelectFromSingleContext(EntityManager em) {
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA> . }";
		final Query<List<String>> q = em.createNativeQuery(query, em.getAvailableContexts().get(0)
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

	public void testSelectByDataProperty(EntityManager em) {
		final String query = "SELECT ?y WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> ?y . }";
		final List<List<List<String>>> res = new ArrayList<>(em.getAvailableContexts().size());
		for (Context ctx : em.getAvailableContexts()) {
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

	public void testSelectByObjectProperty(EntityManager em) {
		final OWLClassD d = QueryTestEnvironment.getData(OWLClassD.class).get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> <"
				+ d.getOwlClassA().getUri().toString() + "> . }";
		for (Context ctx : em.getAvailableContexts()) {
			final Query<List<String>> q = em.createNativeQuery(query, ctx.getUri());
			final List<List<String>> res = q.getResultList();
			assertEquals(1, res.size());
			final List<String> subRes = res.get(0);
			assertEquals(1, subRes.size());
			assertEquals(d.getUri().toString(), subRes.get(0));
		}
	}

	public void testSelectTypes(EntityManager em) {
		final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
		final Set<String> types = a.getTypes();
		types.add(a.getClass().getAnnotation(OWLClass.class).iri());
		final String query = "SELECT ?x WHERE { <" + a.getUri().toString() + "> a ?x . }";
		for (Context ctx : em.getAvailableContexts()) {
			final Query<List<String>> q = em.createNativeQuery(query, ctx.getUri());
			final List<List<String>> res = q.getResultList();
			// The result can contain more types (inference)
			assertTrue(res.size() >= types.size());
			boolean found = false;
			for (String type : types) {
				found = false;
				for (List<String> lst : res) {
					assertEquals(1, lst.size());
					if (type.equals(lst.get(0))) {
						found = true;
						break;
					}
				}
				assertTrue(found);
			}
		}
	}

	// -------------------------
	//
	// This part uses TypedQuery
	//
	// -------------------------

	public void testSelectInstancesOfClass_Typed(EntityManager em) {
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassD> .}";
		final List<OWLClassD> ds = QueryTestEnvironment.getData(OWLClassD.class);
		for (Context ctx : em.getAvailableContexts()) {
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

	public void testSelectByTypeAndDataPropertyValue_Typed(EntityManager em) {
		final OWLClassB b = QueryTestEnvironment.getData(OWLClassB.class).get(5);
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB> ; <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> \""
				+ b.getStringAttribute() + "\" . }";
		for (Context ctx : em.getAvailableContexts()) {
			final TypedQuery<OWLClassB> q = em.createNativeQuery(query, OWLClassB.class,
					ctx.getUri());
			final OWLClassB res = q.getSingleResult();
			assertNotNull(res);
			assertEquals(b.getUri(), res.getUri());
			assertEquals(b.getStringAttribute(), res.getStringAttribute());
		}
	}

	public void testSelectByObjectProperty_Typed(EntityManager em) {
		final List<OWLClassD> ds = QueryTestEnvironment.getData(OWLClassD.class);
		final int cnt = ds.size() / 2;
		assertTrue(cnt > 1);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> ?y . }";
		for (Context ctx : em.getAvailableContexts()) {
			final TypedQuery<OWLClassD> q = em.createNativeQuery(query, OWLClassD.class,
					ctx.getUri());
			final List<OWLClassD> res = q.setMaxResults(cnt).getResultList();
			assertEquals(cnt, res.size());
		}
	}

}
