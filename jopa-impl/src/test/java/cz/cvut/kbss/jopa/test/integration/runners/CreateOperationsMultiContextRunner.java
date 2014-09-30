package cz.cvut.kbss.jopa.test.integration.runners;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassF;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.OWLClassK;
import cz.cvut.kbss.jopa.test.utils.Generators;

public class CreateOperationsMultiContextRunner extends BaseRunner {

	private OWLClassF entityF;
	private OWLClassK entityK;

	public CreateOperationsMultiContextRunner(Logger logger) {
		super(logger);
		initialize();
	}

	private void initialize() {
		this.entityF = new OWLClassF();
		entityF.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityF"));
		this.entityK = new OWLClassK();
		entityK.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityK"));
	}

	public void persistDataPropertyIntoContext(EntityManager em) throws Exception {
		logger.config("Test: persist an entity into the default context and its data property into a different one.");
		final Descriptor aDescriptor = new EntityDescriptor();
		aDescriptor.addAttributeContext(OWLClassA.getStrAttField(), CONTEXT_ONE);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
		assertEquals(entityA.getTypes().size(), res.getTypes().size());
		assertTrue(entityA.getTypes().containsAll(res.getTypes()));
	}

	public void persistObjectPropertyIntoContext(EntityManager em) throws Exception {
		logger.config("Test: persist entity into one context and its object property into another, along with its own attributes.");
		final Descriptor dDescriptor = new EntityDescriptor();
		final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
		dDescriptor.addAttributeDescriptor(OWLClassD.getOwlClassAField(), aDescriptor);
		em.getTransaction().begin();
		em.persist(entityD, dDescriptor);
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri(), dDescriptor);
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(resA);
		assertEquals(resD.getOwlClassA().getUri(), resA.getUri());
		assertEquals(resD.getOwlClassA().getStringAttribute(), resA.getStringAttribute());
		assertTrue(resD.getOwlClassA().getTypes().containsAll(resA.getTypes()));
	}

	public void persistWithGeneratedIntoContext(EntityManager em) {
		logger.config("Test: persist entity with generated ID into a context.");
		final Descriptor eDescriptor = new EntityDescriptor(CONTEXT_ONE);
		em.getTransaction().begin();
		em.persist(entityE, eDescriptor);
		em.getTransaction().commit();

		final OWLClassE res = em.find(OWLClassE.class, entityE.getUri(), eDescriptor);
		assertNotNull(res);
		assertEquals(entityE.getUri(), res.getUri());
		assertEquals(entityE.getStringAttribute(), res.getStringAttribute());
	}

	public void persistTwiceIntoOneContext(EntityManager em) {
		logger.config("Test: persist an entity twice into the same context.");
		final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_ONE);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();

		assertNotNull(em.find(OWLClassA.class, entityA.getUri(), aDescriptor));
		em.getTransaction().begin();
		em.persist(entityA, aDescriptor);
		em.getTransaction().commit();
		fail("This line should not have been reached.");
	}

	public void persistTwiceIntoDifferentContexts(EntityManager em) {
		logger.config("Test: persist an entity into two different contexts.");
		final Descriptor aDescriptorOne = new EntityDescriptor(CONTEXT_ONE);
		final Descriptor aDescriptorTwo = new EntityDescriptor(CONTEXT_TWO);
		em.getTransaction().begin();
		em.persist(entityA, aDescriptorOne);
		em.persist(entityA, aDescriptorTwo);
		em.getTransaction().commit();

		final OWLClassA resOne = em.find(OWLClassA.class, entityA.getUri(), aDescriptorOne);
		assertNotNull(resOne);
		final OWLClassA resTwo = em.find(OWLClassA.class, entityA.getUri(), aDescriptorTwo);
		assertNotNull(resTwo);
		assertNotSame(resOne, resTwo);
		assertEquals(resOne.getUri(), resTwo.getUri());
		assertEquals(resOne.getStringAttribute(), resTwo.getStringAttribute());
	}

	public void persistPropertiesIntoDifferent(EntityManager em) throws Exception {
		logger.config("Test: persist an entity and persist its properties into a different context.");
		final Descriptor bDescriptor = new EntityDescriptor();
		entityB.setProperties(Generators.createProperties(10));
		bDescriptor.addAttributeContext(OWLClassB.getPropertiesField(), CONTEXT_ONE);
		em.getTransaction().begin();
		em.persist(entityB, bDescriptor);
		em.getTransaction().commit();

		final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
		assertNotNull(res);
		assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
		assertEquals(entityB.getProperties().size(), res.getProperties().size());
		for (Entry<String, Set<String>> e : res.getProperties().entrySet()) {
			assertTrue(entityB.getProperties().containsKey(e.getKey()));
			assertEquals(e.getValue(), entityB.getProperties().get(e.getKey()));
		}
	}

	public void persistCascadeIntoThreeContexts(EntityManager em) throws Exception {
		logger.config("Test: persist three entities in cascaded relationship, each into a different context.");
		final Descriptor gDescriptor = new EntityDescriptor();
		final Descriptor hDescriptor = new EntityDescriptor(CONTEXT_ONE);
		final Descriptor aDescriptor = new EntityDescriptor(CONTEXT_TWO);
		hDescriptor.addAttributeDescriptor(OWLClassH.getOwlClassAField(), aDescriptor);
		gDescriptor.addAttributeDescriptor(OWLClassG.getOwlClassHField(), hDescriptor);
		em.getTransaction().begin();
		em.persist(entityG, gDescriptor);
		assertTrue(em.contains(entityG));
		assertTrue(em.contains(entityH));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
		assertNotNull(resA);
		final OWLClassH resH = em.find(OWLClassH.class, entityH.getUri(), hDescriptor);
		assertNotNull(resH);
		assertSame(resA, resH.getOwlClassA());
		final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri(), gDescriptor);
		assertNotNull(resG);
		assertSame(resH, resG.getOwlClassH());
	}

	public void persistSetWithAttributeContexts(EntityManager em) throws Exception {
		logger.config("Test: persist entity with simple set, the set will be in a different context and attributes of its element in another.");
		entityF.setSimpleSet(Generators.createSimpleSet(20));
		final Descriptor fDescriptor = new EntityDescriptor();
		final Descriptor setDescriptor = new ObjectPropertyCollectionDescriptor(CONTEXT_ONE,
				OWLClassF.getSimpleSetField());
		fDescriptor.addAttributeDescriptor(OWLClassF.getSimpleSetField(), setDescriptor);
		setDescriptor.addAttributeContext(OWLClassA.getStrAttField(), CONTEXT_TWO);
		setDescriptor.addAttributeContext(OWLClassA.getTypesField(), CONTEXT_TWO);
		em.getTransaction().begin();
		em.persist(entityF, fDescriptor);
		for (OWLClassA a : entityF.getSimpleSet()) {
			em.persist(a, setDescriptor);
		}
		em.getTransaction().commit();

		final OWLClassF resF = em.find(OWLClassF.class, entityF.getUri(), fDescriptor);
		assertNotNull(resF);
		assertEquals(entityF.getSimpleSet().size(), resF.getSimpleSet().size());
		for (OWLClassA a : resF.getSimpleSet()) {
			final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), setDescriptor);
			assertNotNull(resA);
			assertEquals(a.getStringAttribute(), resA.getStringAttribute());
			assertEquals(a.getTypes(), resA.getTypes());
		}
	}

	public void persistEntityWithObjectPropertyWithGeneratedIdentifierAndPutThePropertyIntoDifferentContext(
			EntityManager em) throws Exception {
		logger.config("Test: persist entity with reference to an entity with generated identifier. "
				+ "The identifier should be generated automatically before the referenced entity itself is persisted.");
		entityK.setOwlClassE(entityE);
		assertNull(entityE.getUri());
		final Descriptor eDescriptor = new EntityDescriptor(CONTEXT_TWO);
		final Descriptor kDescriptor = new EntityDescriptor(CONTEXT_ONE);
		kDescriptor.addAttributeDescriptor(OWLClassK.getOwlClassEField(), eDescriptor);
		em.getTransaction().begin();
		em.persist(entityK, kDescriptor);
		assertNotNull(entityE.getUri());
		em.persist(entityE, eDescriptor);
		em.getTransaction().commit();

		final OWLClassE resE = em.find(OWLClassE.class, entityE.getUri(), eDescriptor);
		assertNotNull(resE);
		assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
		final OWLClassK resK = em.find(OWLClassK.class, entityK.getUri(), kDescriptor);
		assertNotNull(resK);
		assertEquals(resE, resK.getOwlClassE());
	}
}
