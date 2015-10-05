package cz.cvut.kbss.jopa.test.runners;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;
import org.junit.Test;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public abstract class CreateOperationsRunner extends BaseRunner {

    protected CreateOperationsRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testPersistWithGenerated() {
        logger.config("Test: persist into all contexts, also with generated id.");
        this.em = getEntityManager("PersistWithGenerated", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.persist(entityE);
        em.getTransaction().commit();

        final OWLClassA resA1 = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(resA1);
        assertEquals(entityA.getStringAttribute(), resA1.getStringAttribute());
        assertEquals(entityA.getTypes().size(), resA1.getTypes().size());
        assertTrue(entityA.getTypes().containsAll(resA1.getTypes()));

        assertNotNull(entityE.getUri());
        final OWLClassE resE = em.find(OWLClassE.class, entityE.getUri());
        assertNotNull(resE);
        assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
    }

    @Test(expected = PrimaryKeyNotSetException.class)
    public void testPersistWithoutId() {
        logger.config("Test: persist without id. No ID generation specified.");
        this.em = getEntityManager("PersistWithoutId", false);
        final OWLClassB b = new OWLClassB();
        b.setStringAttribute("someValue");
        em.getTransaction().begin();
        em.persist(b);
        em.getTransaction().commit();
    }

    @Test(expected = NullPointerException.class)
    public void testPersistNull() {
        logger.config("Test: persist null.");
        this.em = getEntityManager("PersistNull", false);
        em.getTransaction().begin();
        em.persist(null);
    }

    @Test
    public void testPersistRollback() {
        logger.config("Test: persist and then rollback the transaction.");
        this.em = getEntityManager("PersistRollback", false);
        em.getTransaction().begin();
        em.persist(entityE);
        assertTrue(em.contains(entityE));
        em.getTransaction().rollback();

        assertFalse(em.contains(entityE));
        assertNull(em.find(entityE.getClass(), entityE.getUri()));
    }

    @Test(expected = RollbackException.class)
    public void testPersistRollbackOnly() {
        logger.config("Test: set transaction as rollback only and the try persisting an entity.");
        this.em = getEntityManager("PersistRollbackOnly", false);
        em.getTransaction().begin();
        em.getTransaction().setRollbackOnly();
        em.persist(entityE);
        em.getTransaction().commit();
    }

    @Test
    public void testPersistCascade() {
        logger.config("Test: persist with cascade over two relationships.");
        this.em = getEntityManager("PersistWithCascade", false);
        em.getTransaction().begin();
        em.persist(entityG);
        em.getTransaction().commit();

        final OWLClassA resA2 = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(resA2);
        final OWLClassH resH = em.find(OWLClassH.class, entityH.getUri());
        assertNotNull(resH);
        assertEquals(resH.getOwlClassA(), resA2);
        final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri());
        assertNotNull(resG);
        assertEquals(resG.getOwlClassH(), resH);
        assertEquals(resG.getOwlClassH().getOwlClassA(), resA2);
    }

    @Test(expected = OWLEntityExistsException.class)
    public void testPersistTwiceInOne() {
        logger.config("Test: persist twice into one context.");
        this.em = getEntityManager("PersistTwice", false);
        em.getTransaction().begin();
        em.persist(entityB);
        em.persist(entityB);
        em.getTransaction().commit();
    }

    @Test(expected = RollbackException.class)
    public void testPersistWithoutCascade() {
        logger.config("Test: try persisting relationship not marked as cascade.");
        this.em = getEntityManager("PersistWithoutCascade", false);
        em.getTransaction().begin();
        em.persist(entityD);
        em.getTransaction().commit();
    }

    @Test(expected = OWLEntityExistsException.class)
    public void testPersistDetached() {
        logger.config("Test: persist detached entity. Should throw entity exists exception.");
        this.em = getEntityManager("PersistDetached", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();
        final OWLClassA det = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(det);
        em.detach(det);
        em.persist(det);
    }

    @Test
    public void testPersistSimpleList() {
        logger.config("Test: persist entity with simple list.");
        this.em = getEntityManager("PersistSimpleList", false);
        entityC.setSimpleList(Generators.createSimpleList(10));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

        final OWLClassA a = em.find(OWLClassA.class, entityC.getSimpleList().get(1).getUri());
        assertNotNull(a);
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        assertNotNull(c.getSimpleList());
        assertFalse(c.getSimpleList().isEmpty());
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        assertTrue(c.getSimpleList().contains(a));
    }

    @Test(expected = RollbackException.class)
    public void testPersistSimpleListNoCascade() {
        logger.config("Test: persist entity with simple list, but don't persist the referenced entities.");
        this.em = getEntityManager("PersistSimpleListNoCascade", false);
        entityC.setSimpleList(Generators.createSimpleList(10));
        em.getTransaction().begin();
        em.persist(entityC);
        em.getTransaction().commit();
    }

    @Test
    public void testPersistReferencedList() {
        logger.config("Test: persist entity with referenced list.");
        this.em = getEntityManager("PersistReferencedList", false);
        entityC.setReferencedList(Generators.createReferencedList(5));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        assertTrue(em.contains(entityC));
        assertTrue(em.contains(entityC.getReferencedList().get(0)));
        em.getTransaction().commit();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        assertNotNull(c.getReferencedList());
        assertFalse(c.getReferencedList().isEmpty());
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
            assertNotNull(resA);
            assertEquals(a.getStringAttribute(), resA.getStringAttribute());
            assertTrue(c.getReferencedList().contains(resA));
        }
    }

    @Test(expected = RollbackException.class)
    public void testPersistReferencedListNoCascade() {
        logger.config("Test: persist entity with referenced list. Don't persist the referenced entities.");
        this.em = getEntityManager("PersistReferencedListNoCascade", false);
        entityC.setReferencedList(Generators.createReferencedList(5));
        em.getTransaction().begin();
        em.persist(entityC);
        em.getTransaction().commit();
    }

    @Test
    public void testPersistSimpleAndReferencedList() {
        logger.config("Test: persist entity with both simple and referenced list.");
        this.em = getEntityManager("PersistSimpleAndReferencedList", false);
        entityC.setReferencedList(Generators.createReferencedList(5));
        entityC.setSimpleList(Generators.createSimpleList(5));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        assertNotNull(c.getSimpleList());
        assertEquals(entityC.getSimpleList().size(), c.getSimpleList().size());
        assertNotNull(c.getReferencedList());
        assertEquals(entityC.getReferencedList().size(), c.getReferencedList().size());
        for (OWLClassA a : entityC.getSimpleList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
            assertNotNull(resA);
            assertTrue(c.getSimpleList().contains(resA));
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
            assertNotNull(resA);
            assertTrue(c.getReferencedList().contains(resA));
        }
    }

    @Test
    public void testPersistProperties() {
        logger.config("Test: persist entity with properties.");
        this.em = getEntityManager("PersistWithProperties", false);
        final Map<String, Set<String>> props = new HashMap<>(3);
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyOne", Collections
                .singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/Individual10"));
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyTwo", Collections
                .singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/SomeEntity"));
        props.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#propertyThree",
                Collections.singleton("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
        final Map<String, Set<String>> expected = new HashMap<>(4);
        expected.putAll(props);
        entityB.setProperties(props);
        em.getTransaction().begin();
        em.persist(entityB);
        em.getTransaction().commit();
        em.clear();

        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(res);
        assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
        assertNotNull(res.getProperties());
        assertFalse(res.getProperties().isEmpty());
        assertEquals(expected.size(), res.getProperties().size());
        for (Map.Entry<String, Set<String>> e : expected.entrySet()) {
            assertTrue(res.getProperties().containsKey(e.getKey()));
            final Set<String> s = e.getValue();
            final Set<String> resS = res.getProperties().get(e.getKey());
            assertNotNull(resS);
            assertEquals(1, resS.size());
            assertEquals(s.iterator().next(), resS.iterator().next());
        }
    }

    @Test
    public void testPersistPropertiesEmpty() {
        logger.config("Test: persist entity with properties. The properties will be an empty map.");
        this.em = getEntityManager("PersistWithPropertiesEmpty", false);
        entityB.setProperties(Collections.emptyMap());
        em.getTransaction().begin();
        em.persist(entityB);
        assertTrue(em.contains(entityB));
        em.getTransaction().commit();
        em.clear();

        final OWLClassB b = em.find(OWLClassB.class, entityB.getUri());
        assertNotNull(b);
        assertEquals(entityB.getUri(), b.getUri());
        assertEquals(entityB.getStringAttribute(), b.getStringAttribute());
        assertNull(b.getProperties());
    }

    @Test(expected = OWLEntityExistsException.class)
    public void persistURITwiceInDifferentClasses() {
        logger.config("Test: persist two different entities (of different types) with the same URI.");
        this.em = getEntityManager("PersistURITwiceInDifferentClasses", false);
        final URI pk = URI.create("http://krizik.felk.cvut.cz/jopa/onto/sameEntity");
        final OWLClassA a = new OWLClassA();
        a.setUri(pk);
        final OWLClassB b = new OWLClassB();
        b.setUri(pk);
        em.getTransaction().begin();
        em.persist(a);
        em.persist(b);
        em.getTransaction().commit();
    }

    @Test
    public void testPersistEntityWithBasicTypeAttributes() {
        logger.config("Test: persist entity with attributes of basic types (Integer, Boolean etc.).");
        this.em = getEntityManager("PersistEntityWithBasicTypeAttributes", false);
        em.getTransaction().begin();
        em.persist(entityM);
        em.getTransaction().commit();
        em.clear();

        final OWLClassM res = em.find(OWLClassM.class, entityM.getKey());
        assertNotNull(res);
        assertEquals(entityM.getKey(), res.getKey());
        assertEquals(entityM.getBooleanAttribute(), res.getBooleanAttribute());
        assertEquals(entityM.getIntAttribute(), res.getIntAttribute());
        assertEquals(entityM.getLongAttribute(), res.getLongAttribute());
        assertEquals(entityM.getDoubleAttribute(), res.getDoubleAttribute());
        assertEquals(entityM.getDateAttribute(), res.getDateAttribute());
    }

    @Test
    public void testPersistAndUpdateAttributeBeforeCommit() {
        logger.config("Test: persist entity, set attribute value and then commit.");
        this.em = getEntityManager("PersistAndUpdateBeforeCommit", false);
        final String updatedValue = "updatedStringAttributeValue";
        em.getTransaction().begin();
        em.persist(entityA);
        entityA.setStringAttribute(updatedValue);
        em.getTransaction().commit();
        em.clear();

        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(res);
        assertEquals(updatedValue, res.getStringAttribute());
    }

    @Test
    public void testPersistEntityWithEnumAttribute() {
        logger.config("Test: persist entity with enum attribute.");
        this.em = getEntityManager("PersistEntityWithEnum", false);
        em.getTransaction().begin();
        em.persist(entityM);
        em.getTransaction().commit();

        final OWLClassM res = em.find(OWLClassM.class, entityM.getKey());
        assertNotNull(res);
        assertEquals(entityM.getEnumAttribute(), res.getEnumAttribute());
    }
}
