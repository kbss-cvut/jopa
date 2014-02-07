package cz.cvut.kbss.jopa.test.integration.runners;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.OWLClassI;

public class RetrieveOperationsRunner {

	private OWLClassA entityA;
	private OWLClassB entityB;
	private OWLClassD entityD;
	// Generated IRI
	private OWLClassE entityE;
	// Lazy reference to OWLClassA
	private OWLClassI entityI;
	// Two relationships
	private OWLClassG entityG;
	private OWLClassH entityH;

	public RetrieveOperationsRunner() {
		init();
	}

	private void init() {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		entityA.setTypes(types);
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
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

	public void initBeforeTest() {
		entityE.setUri(null);
	}

	public void retrieveLazily(EntityManager em, URI ctx) throws Exception {
		em.getTransaction().begin();
		em.persist(entityI, ctx);
		em.getTransaction().commit();

		final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri(), ctx);
		assertNotNull(resI);
		final Field f = OWLClassI.class.getDeclaredField("owlClassA");
		f.setAccessible(true);
		Object value = f.get(resI);
		assertNull(value);
		assertNotNull(resI.getOwlClassA());
		value = f.get(resI);
		assertNotNull(value);
		assertEquals(entityA.getUri(), resI.getOwlClassA().getUri());
		assertTrue(em.contains(resI.getOwlClassA()));
	}

	public void retrieveGenerated(EntityManager em, URI ctx) {
		em.getTransaction().begin();
		final List<OWLClassE> lst = new ArrayList<>(10);
		for (int i = 0; i < 10; i++) {
			final OWLClassE e = new OWLClassE();
			e.setStringAttribute("blablabla" + i);
			assertNull(e.getUri());
			em.persist(e, ctx);
			assertNotNull(e.getUri());
			lst.add(e);
		}
		em.getTransaction().commit();

		em.clear();
		for (OWLClassE e : lst) {
			final OWLClassE res = em.find(OWLClassE.class, e.getUri(), ctx);
			assertNotNull(res);
			assertEquals(e.getStringAttribute(), res.getStringAttribute());
		}
	}

	public void retrieveNotExisting(EntityManager em, URI ctx) {
		final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), ctx);
		assertNull(res);
	}

	public void refresh(EntityManager em, URI ctx) {
		em.getTransaction().begin();
		em.persist(entityD, ctx);
		em.persist(entityA, ctx);
		em.getTransaction().commit();

		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		newA.setStringAttribute("newA");
		final OWLClassD d = em.find(OWLClassD.class, entityD.getUri(), ctx);
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), ctx);
		assertEquals(d.getOwlClassA(), a);
		d.setOwlClassA(newA);
		em.refresh(d);
		assertEquals(a.getUri(), d.getOwlClassA().getUri());
	}
}
