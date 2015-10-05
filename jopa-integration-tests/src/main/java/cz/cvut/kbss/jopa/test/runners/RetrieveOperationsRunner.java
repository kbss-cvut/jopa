package cz.cvut.kbss.jopa.test.runners;

import cz.cvut.kbss.jopa.test.*;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public abstract class RetrieveOperationsRunner extends BaseRunner {

    public RetrieveOperationsRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRetrieveSimple() {
        logger.config("Test: retrieve a simple entity.");
        this.em = getEntityManager("RetrieveSimple", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();

        em.getEntityManagerFactory().getCache().evictAll();
        final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
        assertTrue(entityA.getTypes().containsAll(res.getTypes()));
        assertTrue(em.contains(res));
    }

    @Test(expected = NullPointerException.class)
    public void testRetrieveNull() {
        logger.config("Test: retrieve null.");
        this.em = getEntityManager("RetrieveNull", false);
        em.find(OWLClassA.class, null);
    }

    @Test
    public void testRetrieveLazy() throws Exception {
        logger.config("Test: retrieve entity with lazy loaded attribute.");
        this.em = getEntityManager("RetrieveLazy", false);
        em.getTransaction().begin();
        em.persist(entityI);
        em.getTransaction().commit();

        final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri());
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

    @Test
    public void testRetrieveGenerated() throws Exception {
        logger.config("Test: persist and retrieve several entities with generated identifiers.");
        this.em = getEntityManager("RetrieveGenerated", false);
        em.getTransaction().begin();
        final int size = 10;
        final List<OWLClassE> lst = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            final OWLClassE e = new OWLClassE();
            e.setStringAttribute("blablabla" + i);
            assertNull(e.getUri());
            em.persist(e);
            assertNotNull(e.getUri());
            lst.add(e);
        }
        em.getTransaction().commit();

        em.clear();
        for (OWLClassE e : lst) {
            final OWLClassE res = em.find(OWLClassE.class, e.getUri());
            assertNotNull(res);
            assertEquals(e.getStringAttribute(), res.getStringAttribute());
        }
    }

    @Test
    public void testRetrieveNotExisting() {
        logger.config("Test: retrieve entity which does not exist in the specified context.");
        this.em = getEntityManager("RetrieveNotExisting", false);
        final OWLClassB res = em.find(OWLClassB.class, entityB.getUri());
        assertNull(res);
    }

    @Test
    public void testRefresh() {
        logger.config("Test: refresh entity.");
        this.em = getEntityManager("Refresh", false);
        em.getTransaction().begin();
        em.persist(entityD);
        em.persist(entityA);
        em.getTransaction().commit();

        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        final OWLClassD d = em.find(OWLClassD.class, entityD.getUri());
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertEquals(d.getOwlClassA(), a);
        d.setOwlClassA(newA);
        em.refresh(d);
        assertEquals(a.getUri(), d.getOwlClassA().getUri());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRefreshNotManaged() {
        logger.config("Test: refresh entity which is not managed.");
        this.em = getEntityManager("RefreshNotManaged", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
        newA.setStringAttribute("newA");
        em.refresh(newA);
    }

    @Test
    public void testRetrieveDifferentType() {
        logger.config("Test: persist entity but try to retrieve it as a different type.");
        this.em = getEntityManager("RetrieveDifferentType", false);
        em.getTransaction().begin();
        em.persist(entityA);
        em.getTransaction().commit();

        final OWLClassB res = em.find(OWLClassB.class, entityA.getUri());
        assertNull(res);
    }
}
