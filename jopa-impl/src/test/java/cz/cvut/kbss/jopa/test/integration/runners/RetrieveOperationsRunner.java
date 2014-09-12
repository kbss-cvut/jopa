package cz.cvut.kbss.jopa.test.integration.runners;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;

public class RetrieveOperationsRunner extends BaseRunner {

	public RetrieveOperationsRunner(Logger logger) {
		super(logger);
	}

	public void retrieveSimple(EntityManager em, URI ctx) {
		logger.config("Test: retrieve a simple entity.");
		final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		em.getEntityManagerFactory().getCache().evictAll();
		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
		assertTrue(entityA.getTypes().containsAll(res.getTypes()));
		assertTrue(em.contains(res));
	}

	public void retrieveNull(EntityManager em, URI ctx) {
		logger.config("Test: retrieve null.");
		em.find(OWLClassA.class, null, new EntityDescriptor(ctx));
		fail("This line should not have been reached.");
	}

	public void retrieveLazily(EntityManager em, URI ctx) throws Exception {
		logger.config("Test: retrieve entity with lazy loaded attribute.");
		final EntityDescriptor iDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityI, iDescriptor);
		em.getTransaction().commit();

		final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri(), iDescriptor);
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
		logger.config("Test: persist and retrieve several entities with generated identifiers.");
		final EntityDescriptor eDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		final int size = 10;
		final List<OWLClassE> lst = new ArrayList<>(size);
		for (int i = 0; i < size; i++) {
			final OWLClassE e = new OWLClassE();
			e.setStringAttribute("blablabla" + i);
			assertNull(e.getUri());
			em.persist(e, eDescriptor);
			assertNotNull(e.getUri());
			lst.add(e);
		}
		em.getTransaction().commit();

		em.clear();
		for (OWLClassE e : lst) {
			final OWLClassE res = em.find(OWLClassE.class, e.getUri(), eDescriptor);
			assertNotNull(res);
			assertEquals(e.getStringAttribute(), res.getStringAttribute());
		}
	}

	public void retrieveNotExisting(EntityManager em, URI ctx) {
		logger.config("Test: retrieve entity which does not exist in the specified context.");
		final EntityDescriptor bDescriptor = new EntityDescriptor(ctx);
		final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
		assertNull(res);
	}

	public void refresh(EntityManager em, URI ctx) {
		logger.config("Test: refresh entity.");
		final EntityDescriptor dDescriptor = new EntityDescriptor(ctx);
		final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityD, dDescriptor);
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		newA.setStringAttribute("newA");
		final OWLClassD d = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertEquals(d.getOwlClassA(), a);
		d.setOwlClassA(newA);
		em.refresh(d);
		assertEquals(a.getUri(), d.getOwlClassA().getUri());
	}

	public void refreshNotManaged(EntityManager em, URI ctx) {
		logger.config("Test: refresh entity which is not managed.");
		final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(a);
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		newA.setStringAttribute("newA");
		em.refresh(newA);
		fail("This line should not have been reached.");
	}

	public void retrieveDifferentType(EntityManager em, URI ctx) {
		logger.config("Test: persist entity but try to retrieve it as a different type.");
		final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassB res = em.find(OWLClassB.class, entityA.getUri(), aDescriptor);
		assertNull(res);
	}
}
