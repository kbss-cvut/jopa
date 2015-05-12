package cz.cvut.kbss.jopa.test.runners;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;

import java.util.logging.Logger;

import static org.junit.Assert.*;

public class DeleteOperationsMultiContextRunner extends BaseRunner {

	public DeleteOperationsMultiContextRunner(Logger logger) {
		super(logger);
	}

	public void removeFromContext(EntityManager em) {
		logger.config("Test: remove entity from a context.");
		final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(a);
		em.getTransaction().begin();
		em.remove(a);
		assertFalse(em.contains(a));
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNull(res);
	}

	public void removeFromOneKeepInTheOther(EntityManager em) {
		logger.config("Test: persist an entity into two contexts and then remove it from one of them.");
		final Descriptor aDescriptorOne = new EntityDescriptor(CONTEXT_ONE);
		final Descriptor aDescriptorTwo = new EntityDescriptor(CONTEXT_TWO);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptorOne);
		em.persist(entityA, aDescriptorTwo);
		em.getTransaction().commit();

		final OWLClassA aOne = em.find(OWLClassA.class, entityA.getUri(), aDescriptorOne);
		assertNotNull(aOne);
		final OWLClassA aTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
		assertNotNull(aTwo);
		em.getTransaction().begin();
		em.remove(aTwo);
		em.getTransaction().commit();

		final OWLClassA resOne = em.find(OWLClassA.class, entityA.getUri(), aDescriptorOne);
		assertNotNull(resOne);
		final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
		assertNull(resTwo);
	}

	public void removeObjectPropertyFromContext(EntityManager em) throws Exception {
		logger.config("Test: remove object property value from a context.");
		final Descriptor dDescriptor = new EntityDescriptor(CONTEXT_ONE);
		final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
		dDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), aDescriptor);
		em.getTransaction().begin();
		em.persist(entityD, dDescriptor);
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassD d = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
		assertNotNull(d);
		final OWLClassA a = d.getOwlClassA();
		assertNotNull(a);
		d.setOwlClassA(null);
		em.getTransaction().begin();
		em.remove(a);
		em.getTransaction().commit();

		final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
		assertNotNull(resD);
		assertNull(resD.getOwlClassA());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNull(resA);
	}

	public void removeCascadeOverContexts(EntityManager em) throws Exception {
		logger.config("Test: remove entities through cascade, each in a different context.");
		final Descriptor gDescriptor = new EntityDescriptor();
		final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE);
		final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
		hDescriptor.addAttributeDescriptor(OWLClassH.getOwlClassAField(), aDescriptor);
		gDescriptor.addAttributeDescriptor(OWLClassG.getOwlClassHField(), hDescriptor);
		em.getTransaction().begin();
		em.persist(entityG, gDescriptor);
		em.getTransaction().commit();

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(a);
		final OWLClassH h = em.find(OWLClassH.class, entityH.getUri(), hDescriptor);
		assertNotNull(h);
		assertSame(a, h.getOwlClassA());
		final OWLClassG g = em.find(OWLClassG.class, entityG.getUri(), gDescriptor);
		assertNotNull(g);
		assertSame(h, g.getOwlClassH());
		em.getTransaction().begin();
		em.remove(g);
		em.getTransaction().commit();

		assertNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
		assertNull(em.find(OWLClassH.class, entityH.getUri(), hDescriptor));
		assertNull(em.find(OWLClassG.class, entityG.getUri(), gDescriptor));
	}
}
