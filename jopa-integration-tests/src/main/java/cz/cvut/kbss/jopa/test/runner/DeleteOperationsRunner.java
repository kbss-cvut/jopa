package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironmentUtils;
import org.junit.Ignore;
import org.junit.Test;

import java.util.logging.Logger;

import static org.junit.Assert.*;

public abstract class DeleteOperationsRunner extends BaseRunner {

    public DeleteOperationsRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRemoveSimple() {
        logger.config("Test: simple entity removal.");
        this.em = getEntityManager("SimpleRemove", false);
        persist(entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        em.getTransaction().begin();
        em.remove(a);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    // TODO First we need to resolve referential integrity
    @Ignore
    @Test
    public void testRemoveReference() {
        logger.config("Test: remove entity referenced by another entity.");
        this.em = getEntityManager("RemoveReference", true);
        persist(entityD, entityA);

        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
        assertNotNull(a);
        em.getTransaction().begin();
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassD res = em.find(OWLClassD.class, entityD.getUri());
        assertNotNull(res);
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test
    public void testRemoveCascade() {
        logger.config("Test: remove cascade.");
        this.em = getEntityManager("RemoveCascade", true);
        em.getTransaction().begin();
        em.persist(entityG);
        assertTrue(em.contains(entityG));
        assertTrue(em.contains(entityH));
        assertTrue(em.contains(entityA));
        em.getTransaction().commit();

        em.getTransaction().begin();
        final OWLClassG g = em.find(OWLClassG.class, entityG.getUri());
        final OWLClassH h = em.find(OWLClassH.class, entityH.getUri());
        final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
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

        assertNull(em.find(OWLClassG.class, entityG.getUri()));
        assertNull(em.find(OWLClassH.class, entityH.getUri()));
        assertNull(em.find(OWLClassA.class, entityA.getUri()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRemoveDetached() {
        logger.config("Test: try removing detached entity.");
        this.em = getEntityManager("RemoveDetached", true);
        assertNull(entityE.getUri());
        persist(entityE);
        assertNotNull(entityE.getUri());

        em.getTransaction().begin();
        final OWLClassE e = em.find(OWLClassE.class, entityE.getUri());
        assertNotNull(e);
        assertTrue(em.contains(e));
        em.detach(e);
        assertFalse(em.contains(e));
        em.remove(e);
    }

    @Test
    public void testRemoveFromSimpleList() {
        logger.config("Test: remove entity from simple list.");
        this.em = getEntityManager("RemoveFromSimpleList", true);
        final int size = 5;
        entityC.setSimpleList(Generators.createSimpleList(size));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        em.getTransaction().commit();

        final int randIndex = TestEnvironmentUtils.randomInt(size);
        final OWLClassA a = em.find(OWLClassA.class, entityC.getSimpleList().get(randIndex).getUri());
        assertNotNull(a);
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        // We have to remove A from the simple list as well because otherwise we would break the chain in instances
        assertTrue(c.getSimpleList().remove(a));
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNull(resA);
        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        boolean found = false;
        for (OWLClassA aa : resC.getSimpleList()) {
            if (aa.getUri().equals(a.getUri())) {
                found = true;
                break;
            }
        }
        assertFalse(found);
    }

    @Test
    public void testRemoveFromReferencedList() {
        logger.config("Test: remove entity from referenced list.");
        this.em = getEntityManager("RemoveFromReferencedList", true);
        final int size = 10;
        entityC.setReferencedList(Generators.createReferencedList(size));
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

        final int randIndex = TestEnvironmentUtils.randomInt(size);
        final OWLClassA a = em.find(OWLClassA.class, entityC.getReferencedList().get(randIndex).getUri());
        assertNotNull(a);
        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        // We have to remove A from the referenced list as well because otherwise we would break the chain in instances
        assertTrue(c.getReferencedList().remove(a));
        em.remove(a);
        em.getTransaction().commit();

        final OWLClassA resA = em.find(OWLClassA.class, a.getUri());
        assertNull(resA);
        final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri());
        boolean found = false;
        for (OWLClassA aa : resC.getReferencedList()) {
            if (aa.getUri().equals(a.getUri())) {
                found = true;
                break;
            }
        }
        assertFalse(found);
    }

    @Test
    public void testRemoveListOwner() {
        logger.config("Test: remove owner of simple and referenced list.");
        this.em = getEntityManager("RemoveListOwner", true);
        entityC.setSimpleList(Generators.createSimpleList());
        entityC.setReferencedList(Generators.createReferencedList());
        em.getTransaction().begin();
        em.persist(entityC);
        entityC.getSimpleList().forEach(em::persist);
        entityC.getReferencedList().forEach(em::persist);
        em.getTransaction().commit();

        final OWLClassC c = em.find(OWLClassC.class, entityC.getUri());
        assertNotNull(c);
        em.getTransaction().begin();
        em.remove(c);
        em.getTransaction().commit();

        em.getEntityManagerFactory().getCache().evictAll();
        for (OWLClassA a : entityC.getSimpleList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
        for (OWLClassA a : entityC.getReferencedList()) {
            assertNotNull(em.find(OWLClassA.class, a.getUri()));
        }
    }

    @Test
    public void testRemoveNotYetCommitted() {
        logger.config("Test: persist entity, but remove it before committing the transaction.");
        this.em = getEntityManager("RemoveNotYetCommitted", true);
        em.getTransaction().begin();
        em.persist(entityE);
        assertTrue(em.contains(entityE));
        em.remove(entityE);
        assertFalse(em.contains(entityE));
        em.getTransaction().commit();

        final OWLClassE res = em.find(OWLClassE.class, entityE.getUri());
        assertNull(res);
    }
}
