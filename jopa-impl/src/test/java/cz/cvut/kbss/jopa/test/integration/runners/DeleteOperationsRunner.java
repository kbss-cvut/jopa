package cz.cvut.kbss.jopa.test.integration.runners;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.utils.Generators;

public class DeleteOperationsRunner {

	private OWLClassA entityA;
	private OWLClassB entityB;
	private OWLClassC entityC;
	private OWLClassD entityD;
	// Generated IRI
	private OWLClassE entityE;
	// Lazy reference to OWLClassA
	private OWLClassI entityI;
	// Two relationships
	private OWLClassG entityG;
	private OWLClassH entityH;

	public DeleteOperationsRunner() {
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

	public void initBeforeTest() {
		entityE.setUri(null);
		entityC.setReferencedList(null);
		entityC.setSimpleList(null);
	}

	public void removeReference(EntityManager em, URI ctx) {
		em.getTransaction().begin();
		em.persist(entityD, ctx);
		em.persist(entityA, ctx);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), ctx);
		assertNotNull(a);
		em.getTransaction().begin();
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassD res = em.find(OWLClassD.class, entityD.getUri(), ctx);
		assertNotNull(res);
		// TODO When a is removed, the reference to it should be removed from
		// all entities in cache
		// assertNull(res.getOwlClassA());
		assertNull(em.find(OWLClassA.class, entityA.getUri(), ctx));
	}

	public void removeCascade(EntityManager em, URI ctx) {
		em.getTransaction().begin();
		em.persist(entityG, ctx);
		assertTrue(em.contains(entityG));
		assertTrue(em.contains(entityH));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassG g = em.find(OWLClassG.class, entityG.getUri(), ctx);
		final OWLClassH h = em.find(OWLClassH.class, entityH.getUri(), ctx);
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), ctx);
		assertTrue(em.contains(g));
		assertTrue(em.contains(h));
		assertTrue(em.contains(a));
		assertNotNull(g);
		em.remove(g);
		assertFalse(em.contains(g));
		assertFalse(em.contains(h));
		assertFalse(em.contains(a));
		em.getTransaction().commit();

		assertNull(em.find(OWLClassG.class, entityG.getUri(), ctx));
		assertNull(em.find(OWLClassH.class, entityH.getUri(), ctx));
		assertNull(em.find(OWLClassA.class, entityA.getUri(), ctx));
	}

	public void removeDetached(EntityManager em, URI ctx) {
		em.getTransaction().begin();
		assertNull(entityE.getUri());
		em.persist(entityE, ctx);
		em.getTransaction().commit();
		assertNotNull(entityE.getUri());

		em.getTransaction().begin();
		final OWLClassE e = em.find(OWLClassE.class, entityE.getUri(), ctx);
		assertNotNull(e);
		assertTrue(em.contains(e));
		em.detach(e);
		assertFalse(em.contains(e));
		em.remove(e);
		fail("This line should not have been reached.");
	}

	public void removeFromSimpleList(EntityManager em, URI ctx) {
		entityC.setSimpleList(Generators.createSimpleList(5));
		em.getTransaction().begin();
		em.persist(entityC, ctx);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, ctx);
		}
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityC.getSimpleList().get(2).getUri(), ctx);
		assertNotNull(a);
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertNotNull(c);
		em.getTransaction().begin();
		// We have to remove A from the simple list as well because otherwise we
		// would break the chain in instances
		assertTrue(c.getSimpleList().remove(a));
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), ctx);
		assertNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), ctx);
		boolean found = false;
		for (OWLClassA aa : resC.getSimpleList()) {
			if (aa.getUri().equals(a.getUri())) {
				found = true;
				break;
			}
		}
		assertFalse(found);
	}

	public void removeFromReferencedList(EntityManager em, URI ctx) {
		entityC.setReferencedList(Generators.createReferencedList(10));
		em.getTransaction().begin();
		em.persist(entityC, ctx);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, ctx);
		}
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityC.getReferencedList().get(1).getUri(),
				ctx);
		assertNotNull(a);
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertNotNull(c);
		em.getTransaction().begin();
		// We have to remove A from the referenced list as well because
		// otherwise we would break the chain in instances
		assertTrue(c.getReferencedList().remove(a));
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), ctx);
		assertNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), ctx);
		boolean found = false;
		for (OWLClassA aa : resC.getReferencedList()) {
			if (aa.getUri().equals(a.getUri())) {
				found = true;
				break;
			}
		}
		assertFalse(found);
	}

	public void removeListOwner(EntityManager em, URI ctx) {
		entityC.setSimpleList(Generators.createSimpleList());
		entityC.setReferencedList(Generators.createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC, ctx);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, ctx);
		}
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, ctx);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx);
		assertNotNull(c);
		em.getTransaction().begin();
		em.remove(c);
		em.getTransaction().commit();

		em.getEntityManagerFactory().getCache().evictAll();
		for (OWLClassA a : entityC.getSimpleList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri(), ctx));
		}
		for (OWLClassA a : entityC.getReferencedList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri(), ctx));
		}
	}
}
