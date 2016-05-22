package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public abstract class PersistenceUnitTestRunner extends BaseRunner {

    public PersistenceUnitTestRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void multiplePersistenceUnitsSaveDataIndependently() {
        final List<EntityManager> ems = initPersistenceUnits();
        try {
            for (EntityManager em : ems) {
                em.getTransaction().begin();
                em.persist(entityA);
                em.getTransaction().commit();
                em.clear();
            }

            for (EntityManager em : ems) {
                assertNotNull(em.find(OWLClassA.class, entityA.getUri()));
                // Cannot use count query, because OWL2Query does not support it
                final List<?> res = em.createNativeQuery("SELECT ?x WHERE {?x a ?type .}")
                        .setParameter("type", URI.create(OWLClassA.getClassIri())).getResultList();
                assertEquals(1, res.size());
            }
        } finally {
            ems.forEach(em -> {
                em.close();
                em.getEntityManagerFactory().close();
            });
        }
    }

    private List<EntityManager> initPersistenceUnits() {
        final List<EntityManager> ems = new ArrayList<>();
        for (int i = 0; i < Generators.randomPositiveInt(5); i++) {
            ems.add(getEntityManager("MultiplePUsTest" + i, false));
        }
        return ems;
    }
}
