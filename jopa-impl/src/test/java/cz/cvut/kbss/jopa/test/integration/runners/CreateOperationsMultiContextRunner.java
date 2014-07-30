package cz.cvut.kbss.jopa.test.integration.runners;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;

public class CreateOperationsMultiContextRunner {

	private static final URI CONTEXT_ONE = URI
			.create("http://krizik.felk.cvut.cz/jopa/contexts#One");

	private final Logger logger;

	private OWLClassA entityA;
	private OWLClassD entityD;
	private OWLClassE entityE;

	public CreateOperationsMultiContextRunner(Logger logger) {
		assert logger != null;
		this.logger = logger;
		init();
	}

	private void init() {
		this.entityA = new OWLClassA();
		this.entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		this.entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		this.entityA.setTypes(types);
		this.entityD = new OWLClassD();
		this.entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		this.entityD.setOwlClassA(entityA);
		this.entityE = new OWLClassE();
		this.entityE.setStringAttribute("entityEStringAttribute");
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
		// TODO Temporarily commented out
		// assertSame(resD.getOwlClassA(), resA);
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
}
