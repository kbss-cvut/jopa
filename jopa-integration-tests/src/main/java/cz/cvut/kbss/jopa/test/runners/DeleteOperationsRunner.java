package cz.cvut.kbss.jopa.test.runners;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;

import java.net.URI;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public class DeleteOperationsRunner extends BaseRunner {

	public DeleteOperationsRunner(Logger logger) {
		super(logger);
	}

	public void removeSimple(EntityManager em, URI ctx) {
		logger.config("Test: simple entity removal.");
		final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(a);
		em.getTransaction().begin();
		em.remove(a);
		em.getTransaction().commit();

		assertNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
	}

	public void removeReference(EntityManager em, URI ctx) {
		logger.config("Test: remove entity referenced by another entity.");
		final EntityDescriptor dDescriptor = new EntityDescriptor(ctx);
		final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityD, dDescriptor);
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(a);
		em.getTransaction().begin();
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassD res = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
		assertNotNull(res);
		assertNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
	}

	public void removeCascade(EntityManager em, URI ctx) {
		logger.config("Test: remove cascade.");
		final EntityDescriptor gDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityG, gDescriptor);
		assertTrue(em.contains(entityG));
		assertTrue(em.contains(entityH));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassG g = em.find(OWLClassG.class, entityG.getUri(), gDescriptor);
		final OWLClassH h = em.find(OWLClassH.class, entityH.getUri(), gDescriptor);
		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), gDescriptor);
		assertNotNull(g);
		assertNotNull(h);
		assertNotNull(a);
		assertTrue(em.contains(g));
		assertTrue(em.contains(h));
		assertTrue(em.contains(a));
		assertNotNull(g);
		em.remove(g);
		assertFalse(em.contains(g));
		assertFalse(em.contains(h));
		assertFalse(em.contains(a));
		em.getTransaction().commit();

		assertNull(em.find(OWLClassG.class, entityG.getUri(), gDescriptor));
		assertNull(em.find(OWLClassH.class, entityH.getUri(), gDescriptor));
		assertNull(em.find(OWLClassA.class, entityA.getUri(), gDescriptor));
	}

	public void removeDetached(EntityManager em, URI ctx) {
		logger.config("Test: try removing detached entity.");
		final EntityDescriptor eDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		assertNull(entityE.getUri());
		em.persist(entityE, eDescriptor);
		em.getTransaction().commit();
		assertNotNull(entityE.getUri());

		em.getTransaction().begin();
		final OWLClassE e = em.find(OWLClassE.class, entityE.getUri(), eDescriptor);
		assertNotNull(e);
		assertTrue(em.contains(e));
		em.detach(e);
		assertFalse(em.contains(e));
		em.remove(e);
		fail("This line should not have been reached.");
	}

	public void removeFromSimpleList(EntityManager em, URI ctx) {
		logger.config("Test: remove entity from simple list.");
		final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
		final int size = 5;
		entityC.setSimpleList(Generators.createSimpleList(size));
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final int randIndex = TestEnvironmentUtils.randomInt(size);
		final OWLClassA a = em.find(OWLClassA.class, entityC.getSimpleList().get(randIndex)
				.getUri(), cDescriptor);
		assertNotNull(a);
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		em.getTransaction().begin();
		// We have to remove A from the simple list as well because otherwise we
		// would break the chain in instances
		assertTrue(c.getSimpleList().remove(a));
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), cDescriptor);
		assertNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
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
		logger.config("Test: remove entity from referenced list.");
		final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
		final int size = 10;
		entityC.setReferencedList(Generators.createReferencedList(size));
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final int randIndex = TestEnvironmentUtils.randomInt(size);
		final OWLClassA a = em.find(OWLClassA.class, entityC.getReferencedList().get(randIndex)
				.getUri(), cDescriptor);
		assertNotNull(a);
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		em.getTransaction().begin();
		// We have to remove A from the referenced list as well because
		// otherwise we would break the chain in instances
		assertTrue(c.getReferencedList().remove(a));
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), cDescriptor);
		assertNull(resA);
		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
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
		logger.config("Test: remove owner of simple and referenced list.");
		final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
		entityC.setSimpleList(Generators.createSimpleList());
		entityC.setReferencedList(Generators.createReferencedList());
		em.getTransaction().begin();
		em.persist(entityC, cDescriptor);
		for (OWLClassA a : entityC.getSimpleList()) {
			em.persist(a, cDescriptor);
		}
		for (OWLClassA a : entityC.getReferencedList()) {
			em.persist(a, cDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
		assertNotNull(c);
		em.getTransaction().begin();
		em.remove(c);
		em.getTransaction().commit();

		em.getEntityManagerFactory().getCache().evictAll();
		for (OWLClassA a : entityC.getSimpleList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri(), cDescriptor));
		}
		for (OWLClassA a : entityC.getReferencedList()) {
			assertNotNull(em.find(OWLClassA.class, a.getUri(), cDescriptor));
		}
	}

	public void removeNotYetCommitted(EntityManager em, URI ctx) {
		logger.config("Test: persist entity, but remove it before committing the transaction.");
		final EntityDescriptor eDescriptor = new EntityDescriptor(ctx);
		em.getTransaction().begin();
		em.persist(entityE, eDescriptor);
		assertTrue(em.contains(entityE));
		em.remove(entityE);
		assertFalse(em.contains(entityE));
		em.getTransaction().commit();

		final OWLClassE res = em.find(OWLClassE.class, entityE.getUri(), eDescriptor);
		assertNull(res);
	}
}
