package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassQ;
import cz.cvut.kbss.jopa.test.OWLClassT;
import org.junit.Test;
import org.slf4j.Logger;

import static org.junit.Assert.*;

public abstract class DeleteOperationsWithInheritanceRunner extends BaseInheritanceRunner {

    public DeleteOperationsWithInheritanceRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testRemoveEntityWithMappedSuperclass() {
        this.em = getEntityManager("RemoveEntityWithMappedSuperclass", false);
        persist(entityQ, entityA);

        em.getTransaction().begin();
        final OWLClassQ toRemove = em.find(OWLClassQ.class, entityQ.getUri());
        assertNotNull(toRemove);
        em.remove(toRemove);
        em.getTransaction().commit();

        assertNull(em.find(OWLClassQ.class, entityQ.getUri()));
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
        verifyIndividualWasRemoved(entityQ.getUri());
    }

    @Test
    public void testRemoveEntityWithEntitySuperclass() {
        this.em = getEntityManager("RemoveEntityWithEntitySuperclass", false);
        persist(entityT, entityA);

        em.getTransaction().begin();
        final OWLClassT toRemove = em.find(OWLClassT.class, entityT.getUri());
        assertNotNull(toRemove);
        em.remove(toRemove);
        assertFalse(em.contains(toRemove));
        em.getTransaction().commit();

        assertNull(em.find(OWLClassT.class, entityT.getUri()));
        assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
        verifyIndividualWasRemoved(entityT.getUri());
    }
}
