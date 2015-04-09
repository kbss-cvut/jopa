package cz.cvut.kbss.jopa.test.integration.runners;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public class CreateOperationsRunner extends BaseRunner {

    public CreateOperationsRunner(Logger logger) {
        super(logger);
    }

    public void persistWithGenerated(EntityManager em) {
        persistWithGenerated(em, null);
    }

    public void persistWithGenerated(EntityManager em, URI ctx) {
        logger.config("Test: persist into all contexts, also with generated id.");
        em.getTransaction().begin();
        final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
        final EntityDescriptor eDescriptor = new EntityDescriptor(ctx);
        em.persist(entityA, aDescriptor);
        em.persist(entityE, eDescriptor);
        em.getTransaction().commit();

        final OWLClassA resA1 = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(resA1);
        assertEquals(entityA.getStringAttribute(), resA1.getStringAttribute());
        assertEquals(entityA.getTypes().size(), resA1.getTypes().size());
        assertTrue(entityA.getTypes().containsAll(resA1.getTypes()));

        assertNotNull(entityE.getUri());
        final OWLClassE resE = em.find(OWLClassE.class, entityE.getUri(), eDescriptor);
        assertNotNull(resE);
        assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
    }

    public void persistNull(EntityManager em, URI ctx) {
        logger.config("Test: persist null.");
        final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.persist(null, aDescriptor);
        fail("This line should not have been reached.");
    }

    public void persistWithoutId(EntityManager em, URI ctx) {
        logger.config("Test: persist without id. No ID generation specified.");
        final EntityDescriptor bDescriptor = new EntityDescriptor(ctx);
        final OWLClassB b = new OWLClassB();
        b.setStringAttribute("someValue");
        em.getTransaction().begin();
        em.persist(b, bDescriptor);
        em.getTransaction().commit();
        fail("This line should not have been reached.");
    }

    public void persistRollback(EntityManager em, URI ctx) {
        logger.config("Test: persist and then rollback the transaction.");
        final EntityDescriptor eDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.persist(entityE, eDescriptor);
        assertTrue(em.contains(entityE));
        em.getTransaction().rollback();

        assertFalse(em.contains(entityE));
        assertNull(em.find(entityE.getClass(), entityE.getUri(), eDescriptor));
    }

    public void persistRollbackOnly(EntityManager em, URI ctx) {
        logger.config("Test: set transaction as rollback only and the try persisting an entity.");
        final EntityDescriptor eDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.getTransaction().setRollbackOnly();
        em.persist(entityE, eDescriptor);
        em.getTransaction().commit();
        fail("This line should not have been reached.");
    }

    public void persistCascade(EntityManager em, URI ctx) {
        logger.config("Test: persist with cascade over two relationships.");
        final EntityDescriptor gDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.persist(entityG, gDescriptor);
        em.getTransaction().commit();

        final OWLClassA resA2 = em.find(OWLClassA.class, entityA.getUri(),
                new EntityDescriptor(ctx));
        assertNotNull(resA2);
        final OWLClassH resH = em
                .find(OWLClassH.class, entityH.getUri(), new EntityDescriptor(ctx));
        assertNotNull(resH);
        assertEquals(resH.getOwlClassA(), resA2);
        final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri(), gDescriptor);
        assertNotNull(resG);
        assertEquals(resG.getOwlClassH(), resH);
        assertEquals(resG.getOwlClassH().getOwlClassA(), resA2);
    }

    public void persistTwice(EntityManager em, URI ctx) {
        logger.config("Test: persist twice into one context.");
        final EntityDescriptor bDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.persist(entityB, bDescriptor);
        em.persist(entityB, bDescriptor);
        em.getTransaction().commit();
        fail("This line should not have been reached.");
    }

    public void persistWithoutCascade(EntityManager em, URI ctx) {
        logger.config("Test: try persisting relationship not marked as cascade.");
        final EntityDescriptor dDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.persist(entityD, dDescriptor);
        em.getTransaction().commit();
        fail("This line should not have been reached.");
    }

    public void persistDetachedEntity(EntityManager em, URI ctx) {
        logger.config("Test: persist detached entity. Should throw entity exists exception.");
        final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.persist(entityA, aDescriptor);
        em.getTransaction().commit();
        final OWLClassA det = em.find(OWLClassA.class, entityA.getUri(), aDescriptor);
        assertNotNull(det);
        em.detach(det);
        em.persist(det);
        fail("This line should not have been reached.");
    }

    public void persistSimpleList(EntityManager em, URI ctx) {
        logger.config("Test: persist entity with simple list.");
        final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
        entityC.setSimpleList(Generators.createSimpleList(10));
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getSimpleList()) {
            em.persist(a, cDescriptor);
        }
        em.getTransaction().commit();

        final OWLClassA a = em.find(OWLClassA.class, entityC.getSimpleList().get(1).getUri(),
                cDescriptor);
        assertNotNull(a);
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(c);
        assertNotNull(c.getSimpleList());
        assertFalse(c.getSimpleList().isEmpty());
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        assertTrue(c.getSimpleList().contains(a));
    }

    public void persistSimpleListNoCascade(EntityManager em, URI ctx) {
        logger.config("Test: persist entity with simple list, but don't persist the referenced entities.");
        final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
        entityC.setSimpleList(Generators.createSimpleList(10));
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        em.getTransaction().commit();
        fail("This line should not have been reached.");
    }

    public void persistReferencedList(EntityManager em, URI ctx) {
        logger.config("Test: persist entity with referenced list.");
        final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
        entityC.setReferencedList(Generators.createReferencedList(5));
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        for (OWLClassA a : entityC.getReferencedList()) {
            em.persist(a, cDescriptor);
        }
        assertTrue(em.contains(entityC));
        assertTrue(em.contains(entityC.getReferencedList().get(0)));
        em.getTransaction().commit();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), cDescriptor);
        assertNotNull(c);
        assertNotNull(c.getReferencedList());
        assertFalse(c.getReferencedList().isEmpty());
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), cDescriptor);
            assertNotNull(resA);
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
            assertTrue(c.getReferencedList().contains(resA));
        }
    }

    public void persistReferencedListNoCascade(EntityManager em, URI ctx) {
        logger.config("Test: persist entity with referenced list. Don't persist the referenced entities.");
        final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
        entityC.setReferencedList(Generators.createReferencedList(5));
        em.getTransaction().begin();
        em.persist(entityC, cDescriptor);
        em.getTransaction().commit();
        fail("This line should not have been reached.");
    }

    public void persistSimpleAndReferencedList(EntityManager em, URI ctx) {
        logger.config("Test: persist entity with both simple and referenced list.");
        final EntityDescriptor cDescriptor = new EntityDescriptor(ctx);
        entityC.setReferencedList(Generators.createReferencedList(5));
        entityC.setSimpleList(Generators.createSimpleList(5));
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
        assertNotNull(c.getSimpleList());
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        assertNotNull(c.getReferencedList());
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        for (OWLClassA a : entityC.getSimpleList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), cDescriptor);
            assertNotNull(resA);
            assertTrue(c.getSimpleList().contains(resA));
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri(), cDescriptor);
            assertNotNull(resA);
            assertTrue(c.getReferencedList().contains(resA));
        }
    }

    public void persistProperties(EntityManager em, URI ctx) {
        logger.config("Test: persist entity with properties.");
        final EntityDescriptor bDescriptor = new EntityDescriptor(ctx);
        final Map<String, Set<String>> props = new HashMap<>(3);
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyOne", Collections
                .singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/Individial10"));
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyTwo", Collections
                .singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/SomeEntity"));
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyThree",
                Collections.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
        final Map<String, Set<String>> expected = new HashMap<>(4);
        expected.putAll(props);
        entityB.setProperties(props);
        em.getTransaction().begin();
        em.persist(entityB, bDescriptor);
        em.getTransaction().commit();
        em.clear();

        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertNotNull(res);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertNotNull(res.getProperties());
        assertFalse(res.getProperties().isEmpty());
        assertEquals(expected.size(), res.getProperties().size());
        for (Entry<String, Set<String>> e : expected.entrySet()) {
            assertTrue(res.getProperties().containsKey(e.getKey()));
            final Set<String> s = e.getValue();
            final Set<String> resS = res.getProperties().get(e.getKey());
            assertNotNull(resS);
            assertEquals(1, resS.size());
            assertEquals(s.iterator().next(), resS.iterator().next());
        }
    }

    public void persistPropertiesEmpty(EntityManager em, URI ctx) {
        logger.config("Test: persist entity with properties. The properties will be an empty map.");
        final EntityDescriptor bDescriptor = new EntityDescriptor(ctx);
        entityB.setProperties(Collections.<String, Set<String>>emptyMap());
        em.getTransaction().begin();
        em.persist(entityB, bDescriptor);
        assertTrue(em.contains(entityB));
        em.getTransaction().commit();
        em.clear();

        final OWLClassB b = em.find(OWLClassB.class, entityB.getUri(), bDescriptor);
        assertNotNull(b);
        assertEquals(entityB.getUri(), b.getUri());
        assertEquals(entityB.getStringAttribute(), b.getStringAttribute());
        assertNull(b.getProperties());
    }

    public void persistURITwiceInDifferentClasses(EntityManager em, URI ctx) {
        logger.config("Test: persist two different entities (of different types) with the same URI.");
        final URI pk = URI.create("http://krizik.felk.cvut.cz/jopa/onto/sameEntity");
        final OWLClassA a = new OWLClassA();
        a.setUri(pk);
        final OWLClassB b = new OWLClassB();
        b.setUri(pk);
        final EntityDescriptor aDescriptor = new EntityDescriptor(ctx);
        final EntityDescriptor bDescriptor = new EntityDescriptor(ctx);
        em.getTransaction().begin();
        em.persist(a, aDescriptor);
        em.persist(b, bDescriptor);
        em.getTransaction().commit();
        fail("This line should not have been reached.");
    }
}
